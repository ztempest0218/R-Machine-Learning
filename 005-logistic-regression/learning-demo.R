# OLSとLogistic回帰が、同じ購入データから係数を学ぶ説明用デモ。
# 図を描くための補助コードであり、受講者が勾配の式を覚える必要はない。
# 両者を固定学習率の全バッチ勾配降下法で更新して、学習の過程を見せる。
# OLSは反復せず最小二乗解を求められ、lm()は通常QR分解を使う。
# glm(..., family = binomial())は通常、反復再重み付け最小二乗法(IRLS)を使う。
# このデモの更新方法は、lm()・glm()の内部処理を再現したものではない。
# MSEとLog Lossは別の損失なので、二つの値を直接比べて優劣を判断しない。

make_learning_demo <- function() {
  # incomeの単位は万円（200は年間所得200万円）。各グループは100人。
  # 一人ずつの正解は0/1であり、20%、50%、80%は集計した購入割合である。
  group_summary <- data.frame(
    group = c("A", "B", "C"),
    income = c(200, 400, 600),
    purchased = c(20L, 50L, 80L),
    total = rep(100L, 3)
  )
  group_summary$purchase_rate <- group_summary$purchased / group_summary$total
  income <- rep(group_summary$income, each = 100)
  y <- unlist(lapply(group_summary$purchased,
                     function(n) c(rep(1, n), rep(0, 100 - n))))
  data <- data.frame(
    group = factor(rep(group_summary$group, each = 100), levels = c("A", "B", "C")),
    income = income,
    y = y
  )

  # 数値計算の内部だけで中心化し、100万円を1単位に換算する。
  # これにより学習率と更新過程を保ち、history・stepsは万円単位の係数へ戻す。
  center <- mean(income)
  internal_scale <- 100
  centered_x <- (income - center) / internal_scale
  n_steps <- 100L
  # 両方の損失が安定して減る固定値。更新回数は計算速度の比較ではない。
  learning_rate <- 0.3
  ols_learning_rate <- 0.15
  coefficient <- ols_coefficient <- c(0, 0)
  history <- data.frame(
    iteration = 0:n_steps,
    beta0 = numeric(n_steps + 1L),
    beta1 = numeric(n_steps + 1L),
    loss = numeric(n_steps + 1L)
  )
  ols_history <- history

  # 数値的に安定した、300人分の平均binary cross-entropy。
  binary_loss <- function(z, y) {
    mean(pmax(z, 0) - y * z + log1p(exp(-abs(z))))
  }

  for (iteration in 0:n_steps) {
    z <- coefficient[1] + coefficient[2] * centered_x
    ols_prediction <- ols_coefficient[1] + ols_coefficient[2] * centered_x
    history[iteration + 1L, c("beta0", "beta1", "loss")] <- c(
      coefficient[1] - coefficient[2] * center / internal_scale,
      coefficient[2] / internal_scale,
      binary_loss(z, y)
    )
    ols_history[iteration + 1L, c("beta0", "beta1", "loss")] <- c(
      ols_coefficient[1] - ols_coefficient[2] * center / internal_scale,
      ols_coefficient[2] / internal_scale,
      mean((ols_prediction - y)^2)
    )

    if (iteration < n_steps) {
      probability <- plogis(z)
      gradient <- c(
        mean(probability - y),
        mean((probability - y) * centered_x)
      )
      ols_gradient <- 2 * c(
        mean(ols_prediction - y),
        mean((ols_prediction - y) * centered_x)
      )
      coefficient <- coefficient - learning_rate * gradient
      ols_coefficient <- ols_coefficient - ols_learning_rate * ols_gradient
    }
  }

  # 両者とも係数0から始める。
  # ただし最初の出力は、OLSでは0、Logisticではplogis(0) = 0.5となる。
  reference_fit <- glm(y ~ income, data = data, family = binomial())
  reference_loss <- binary_loss(predict(reference_fit, type = "link"), y)
  ols_reference_fit <- lm(y ~ income, data = data)
  ols_reference_loss <- mean(residuals(ols_reference_fit)^2)
  stopifnot(
    nrow(data) == 300L,
    identical(as.integer(tapply(data$y, data$group, sum)), c(20L, 50L, 80L)),
    history$beta0[1] == 0, history$beta1[1] == 0,
    ols_history$beta0[1] == 0, ols_history$beta1[1] == 0,
    abs(history$loss[1] - log(2)) < 1e-12,
    abs(ols_history$loss[1] - 0.5) < 1e-12,
    all(diff(history$loss) <= 1e-12),
    all(diff(ols_history$loss) <= 1e-12),
    abs(tail(history$loss, 1) - reference_loss) < 1e-9,
    abs(tail(ols_history$loss, 1) - ols_reference_loss) < 1e-12,
    max(abs(unlist(history[n_steps + 1L, c("beta0", "beta1")]) -
              unname(coef(reference_fit)))) < 1e-5,
    max(abs(unlist(ols_history[n_steps + 1L, c("beta0", "beta1")]) -
              unname(coef(ols_reference_fit)))) < 1e-10
  )

  # 表・対話的な図でも、丸める前の実計算結果を利用できるようにする。
  step_numbers <- c(0L, 1L, 5L, 100L)
  selected_logistic <- history[match(step_numbers, history$iteration), ]
  selected_ols <- ols_history[match(step_numbers, ols_history$iteration), ]
  make_steps <- function(selected, model, metric) {
    predictions <- sapply(group_summary$income, function(x) selected$beta0 + selected$beta1 * x)
    if (model == "Logistic") predictions <- plogis(predictions)
    data.frame(
      model = model,
      iteration = selected$iteration,
      beta0 = selected$beta0,
      beta1 = selected$beta1,
      prediction_A = predictions[, 1],
      prediction_B = predictions[, 2],
      prediction_C = predictions[, 3],
      loss = selected$loss,
      metric = metric,
      row.names = NULL
    )
  }
  steps <- rbind(make_steps(selected_ols, "OLS", "MSE"),
                 make_steps(selected_logistic, "Logistic", "Log Loss"))

  list(
    data = data,
    group_summary = group_summary,
    history = history,
    ols_history = ols_history,
    steps = steps,
    learning_rate = learning_rate,
    ols_learning_rate = ols_learning_rate,
    reference_loss = reference_loss,
    reference_coefficients = coef(reference_fit),
    ols_reference_loss = ols_reference_loss,
    ols_reference_coefficients = coef(ols_reference_fit)
  )
}

fig_learning_comparison <- function(demo) {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  font_family <- if (Sys.info()[["sysname"]] == "Darwin") "Hiragino Sans" else "sans"
  par(mfrow = c(2, 3), family = font_family, cex = 0.86,
      mar = c(5.0, 4.0, 4.1, 0.7), oma = c(3.2, 0, 0, 0), las = 1)

  selected_iterations <- c(0L, 5L, 100L)
  stage_names <- c("初期（0回）", "途中（5回）", "最終（100回）")
  histories <- list(demo$ols_history, demo$history)
  model_names <- c("OLS", "Logistic")
  metrics <- c("MSE", "Log Loss")
  model_colors <- c("#21548a", "#c06424")
  groups <- demo$group_summary
  grid_x <- seq(0, 800, length.out = 400)

  for (model in 1:2) {
    history <- histories[[model]]
    for (stage in seq_along(selected_iterations)) {
      row <- match(selected_iterations[stage], history$iteration)
      stopifnot(!is.na(row))
      beta0 <- history$beta0[row]
      beta1 <- history$beta1[row]
      predicted <- beta0 + beta1 * grid_x
      if (model == 2) predicted <- plogis(predicted)

      plot(NA, xlim = c(0, 800), ylim = c(-0.25, 1.25), axes = FALSE,
           xlab = "", ylab = if (model == 1) "予測値" else "購入確率",
           main = paste0(model_names[model], "：", stage_names[stage]),
           font.main = 1, cex.main = 1.02, col.main = model_colors[model])
      rect(0, -0.25, 800, 0, col = "#f8efea", border = NA)
      rect(0, 1, 800, 1.25, col = "#f8efea", border = NA)
      abline(h = c(0, 0.5, 1), col = "#e0e6eb", lty = c(1, 3, 1))
      axis(1, at = seq(0, 800, 200))
      axis(2, at = c(0, 0.5, 1), labels = c("0", "0.5", "1"))
      box(bty = "l")
      lines(grid_x, predicted, col = model_colors[model], lwd = 2.5)
      points(groups$income, groups$purchase_rate, pch = 21,
             col = "#343d46", bg = "white", lwd = 1.5, cex = 1.4)
      label_y <- groups$purchase_rate + 0.12
      if (model == 2 && stage == 2) label_y[1] <- groups$purchase_rate[1] - 0.12
      text(groups$income, label_y,
           groups$group, col = "#4a5763", cex = 0.8)
      mtext(sprintf("beta0 = %.3f     beta1 = %.5f", beta0, beta1),
            side = 3, line = 0.50, cex = 0.77, col = "#465868")
      mtext("年間所得（万円）", side = 1, line = 2.1, cex = 0.79)
      mtext(sprintf("%s = %.4f", metrics[model], history$loss[row]),
            side = 1, line = 3.65, cex = 0.83, col = model_colors[model])
    }
  }
  mtext("○：各グループ100人の購入率（A：20%、B：50%、C：80%）。",
        side = 1, outer = TRUE, line = 0.35, cex = 0.80, col = "#465868")
  mtext("損失は300人の0/1データで計算。MSEとLog Lossの数値を直接比較して、モデルの優劣は判断しない。",
        side = 1, outer = TRUE, line = 1.8, cex = 0.76, col = "#566573")
}

fig_training_losses <- function(demo) {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  font_family <- if (Sys.info()[["sysname"]] == "Darwin") "Hiragino Sans" else "sans"
  par(mfrow = c(1, 2), family = font_family,
      mar = c(4.5, 4.5, 3.4, 1.0), oma = c(3.1, 0, 0, 0), las = 1)
  histories <- list(demo$ols_history, demo$history)
  model_names <- c("OLS", "Logistic")
  metrics <- c("MSE", "Log Loss")
  model_colors <- c("#21548a", "#c06424")
  y_max <- c(0.55, 0.75)

  for (model in 1:2) {
    history <- histories[[model]]
    plot(history$iteration, history$loss, type = "n",
         xlim = range(history$iteration), ylim = c(0, y_max[model]),
         xlab = "係数を更新した回数", ylab = metrics[model],
         main = paste0(model_names[model], "：", metrics[model], "を小さくする"),
         font.main = 1, cex.main = 0.98, col.main = model_colors[model],
         axes = FALSE)
    abline(h = seq(0, y_max[model], 0.1), col = "#e6ecf1")
    axis(1, at = seq(0, 100, 20))
    axis(2, at = seq(0, y_max[model], 0.1))
    box(bty = "l")
    lines(history$iteration, history$loss, col = model_colors[model], lwd = 2.5)
    selected_rows <- match(c(0L, 5L, 100L), history$iteration)
    points(history$iteration[selected_rows], history$loss[selected_rows],
           pch = c(16, 17, 16), col = model_colors[model], cex = 1.1)
    text(3, history$loss[1] + 0.025,
         sprintf("開始 %.3f", history$loss[1]),
         adj = c(0, 0.5), col = model_colors[model], cex = 0.87)
    text(99, tail(history$loss, 1) + 0.045,
         sprintf("最終 %.3f", tail(history$loss, 1)),
         adj = c(1, 0.5), col = model_colors[model], cex = 0.87)
  }
  mtext("それぞれのモデルの中で、更新前から損失が下がる様子を確認する。",
        side = 1, outer = TRUE, line = 0.3, cex = 0.82)
  mtext("MSEとLog Lossは計算方法が異なるため、左右の数値でモデルの優劣を判断しない。",
        side = 1, outer = TRUE, line = 1.7, cex = 0.79, col = "#566573")
}

# 旧名で呼ぶコードにも対応する。推奨サイズは11×6.6インチ。
fig_learning_progress <- function(demo) {
  fig_learning_comparison(demo)
}
