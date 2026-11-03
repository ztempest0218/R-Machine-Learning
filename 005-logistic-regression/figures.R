# 第5回「Logistic回帰」の概念図。
# すべて説明用のデータ・係数を使い、実証分析の結果とは区別する。
# base Rだけで描画できる。各関数は描画設定を終了時に戻す。

# 表示上は各グループを最大20人ずつ複数行に並べる。
# 上下のずれは見やすくするためのもので、実際のyは0または1。
purchase_scatter_positions <- function(income, outcome) {
  incomes <- sort(unique(income))
  half_width <- if (length(incomes) > 1) min(diff(incomes)) * 0.38 else 60
  displayed_x <- income
  displayed_y <- outcome
  for (x in incomes) {
    for (result in 0:1) {
      indices <- which(income == x & outcome == result)
      n_rows <- ceiling(length(indices) / 20)
      if (!length(indices)) next
      for (row in seq_len(n_rows)) {
        row_indices <- indices[seq.int((row - 1) * 20 + 1, min(row * 20, length(indices)))]
        displayed_x[row_indices] <- x + if (length(row_indices) == 1) 0 else {
          seq(-half_width, half_width, length.out = length(row_indices))
        }
        displayed_y[row_indices] <- result + (row - (n_rows + 1) / 2) * 0.018
      }
    }
  }
  list(x = displayed_x, y = displayed_y)
}

# まず観測された所得の範囲でOLSを読み、次に同じ直線を外側へ延ばす。
fig_lpm <- function(purchase, extrapolate = FALSE, stage = "fit") {
  stage <- match.arg(stage, c("points", "intro", "means", "fit"))
  # 外挿の図は、従来どおり平均とOLSの予測をすべて表示する。
  if (extrapolate) stage <- "fit"
  show_means <- stage %in% c("intro", "means", "fit")
  show_fit <- stage %in% c("intro", "means", "fit")
  show_footer <- stage != "intro" && !extrapolate
  stage_title <- c(points = "① 一人ひとりの購入結果は、0か1",
                   intro = "所得と購入確率",
                   means = "② 直線と、所得ごとの購入割合を見比べる",
                   fit = "③ OLSの傾きで、購入確率の変化を読む")
  stage_axis <- c(points = "個人の購入結果 y（0か1）",
                  intro = "購入結果 y・OLSの予測値",
                  means = "購入結果 y・平均・予測値",
                  fit = "購入結果 y・予測確率")
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  font_family <- if (Sys.info()[["sysname"]] == "Darwin") "Hiragino Sans" else "sans"
  par(mfrow = c(1, 1), family = font_family,
      mar = c(4.2, 6.1, if (extrapolate) 0.8 else 2.6, 1.2),
      oma = c(if (show_footer) 3.4 else 0, 0, 0, 0), las = 1,
      col.axis = "#555555", col.lab = "#333333", col.main = "#333333",
      fg = "#9a9a9a", mgp = c(2.7, 0.7, 0), tcl = -0.22,
      cex.axis = 0.9, cex.lab = 0.95)
  outcome <- if ("bought" %in% names(purchase)) "bought" else "y"
  stopifnot("income" %in% names(purchase), outcome %in% names(purchase))
  demo <- data.frame(income = purchase$income, y = purchase[[outcome]])
  stopifnot(is.numeric(demo$income), all(is.finite(demo$income)),
            all(demo$y %in% c(0, 1)), nrow(demo) > 0)
  incomes <- sort(unique(demo$income))
  stopifnot(length(incomes) >= 2)
  observed_range <- range(incomes)
  rates <- vapply(incomes, function(x) mean(demo$y[demo$income == x]), numeric(1))
  counts <- table(factor(demo$income, levels = incomes), factor(demo$y, levels = 0:1))
  fit <- lm(y ~ income, data = demo)
  x_limits <- if (extrapolate) range(c(0, 800, incomes)) else observed_range + c(-95, 95)
  y_limits <- if (extrapolate) c(-0.24, 1.25) else c(-0.12, 1.15)
  plot(NA, xlim = x_limits, ylim = y_limits,
       xlab = "年間所得 x（万円）", ylab = "",
       axes = FALSE, main = if (extrapolate) "" else {
         stage_title[[stage]]
       }, font.main = 1, cex.main = 1.0)
  abline(h = c(0, 1), col = "#d8d8d8", lwd = 0.8)
  abline(h = 0.5, col = "#eeeeee", lwd = 0.6)
  x_ticks <- if (extrapolate) sort(unique(c(0, incomes, 800))) else incomes
  x_labels <- as.character(x_ticks)
  x_labels[match(incomes, x_ticks)] <- paste0(incomes, " (", LETTERS[seq_along(incomes)], ")")
  axis(1, at = x_ticks, labels = x_labels)
  axis(2, at = c(0, 0.2, 0.5, 0.8, 1),
       labels = c("0（未購入）", "0.2", "0.5", "0.8", "1（購入）"), cex.axis = 0.9)
  mtext(stage_axis[[stage]], side = 2, line = 4.8, las = 0, cex = 0.95, col = "#333333")

  displayed <- purchase_scatter_positions(demo$income, demo$y)
  points(displayed$x, displayed$y, pch = 16, cex = 0.8,
         col = adjustcolor("#8b949a", alpha.f = 0.55))
  text(incomes, 1.065, paste0("購入 ", counts[, 2], "人"), cex = 0.77, col = "#7a7a7a")
  text(incomes, -0.065, paste0("未購入 ", counts[, 1], "人"), cex = 0.77, col = "#7a7a7a")
  if (show_fit) {
    observed_grid <- data.frame(income = seq(observed_range[1], observed_range[2], length.out = 200))
    lines(observed_grid$income, predict(fit, observed_grid), col = "#2e526b", lwd = 2.5)
  }
  if (extrapolate) {
    for (span in list(c(x_limits[1], observed_range[1]), c(observed_range[2], x_limits[2]))) {
      grid <- data.frame(income = seq(span[1], span[2], length.out = 100))
      lines(grid$income, predict(fit, grid), col = "#2e526b", lwd = 2.2, lty = "44")
    }
    edge <- data.frame(income = x_limits)
    edge_prediction <- predict(fit, edge)
    points(edge$income, edge_prediction, pch = 21, bg = "white", col = "#a4493c", lwd = 1.5, cex = 1.05)
    text(edge$income[1], edge_prediction[1] - 0.075,
         sprintf("予測 %.2f", edge_prediction[1]), adj = 0, cex = 0.88, col = "#a4493c")
    text(edge$income[2], edge_prediction[2] + 0.075,
         sprintf("予測 %.2f", edge_prediction[2]), adj = 1, cex = 0.88, col = "#a4493c")
  }
  if (show_means) {
    points(incomes, rates, pch = 23, bg = "#b16b43", col = "white", cex = 1.5, lwd = 1.0)
    text(incomes, rates + if (stage == "intro") 0.095 else 0.08,
         if (stage == "intro") sprintf("平均 %d%%", round(100 * rates)) else if (stage == "means") {
           sprintf("平均 %.2f（%d%%）", rates, round(100 * rates))
         } else {
           sprintf("%.2f", rates)
         }, cex = 0.87, col = "#9d5e3a")
  }
  if (!show_footer) return(invisible(NULL))
  legend_positions <- switch(stage, points = 0.5, intro = c(0.32, NA, 0.72),
                              means = c(0.2, 0.51, 0.82), fit = c(0.2, 0.51, 0.82))
  mtext("○ 個人の結果（0か1）", side = 1, outer = TRUE, line = 0.15,
        at = legend_positions[1], cex = 0.83, col = "#657584")
  if (show_means) {
    mtext("◆ グループの平均（購入割合）", side = 1, outer = TRUE, line = 0.15,
          at = legend_positions[2], cex = 0.83, col = "#a44e15")
  }
  if (show_fit) {
    mtext("━ OLSの予測値", side = 1, outer = TRUE, line = 0.15,
          at = legend_positions[3], cex = 0.83, col = "#21548a")
  }
  mtext(if (extrapolate) "実線：所得200〜600万円の範囲。破線：外挿。個人の点は横方向だけずらして表示。" else {
    sprintf("説明用の%d人。灰色の点は横方向だけずらして表示。各人の縦の値はすべて0か1。", nrow(demo))
  }, side = 1, outer = TRUE, line = 1.45, cex = 0.82, col = "#566573")
}

fig_binary_probability <- function(data = NULL) {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  font_family <- if (Sys.info()[["sysname"]] == "Darwin") "Hiragino Sans" else "sans"
  par(mfrow = c(1, 2), family = font_family,
      mar = c(4.3, 5.5, 2.5, 1.1), oma = c(0, 0, 0, 0), las = 1,
      col.axis = "#555555", col.lab = "#333333", col.main = "#333333",
      fg = "#9a9a9a", mgp = c(2.7, 0.7, 0), tcl = -0.22,
      cex.lab = 0.9)
  if (is.null(data)) {
    demo <- data.frame(
      income = rep(c(200, 400, 600), each = 100),
      y = unlist(lapply(c(20, 50, 80), function(n) c(rep(1, n), rep(0, 100 - n))))
    )
  } else {
    outcome <- if ("y" %in% names(data)) "y" else "bought"
    stopifnot("income" %in% names(data), outcome %in% names(data))
    demo <- data.frame(income = data$income, y = data[[outcome]])
  }
  stopifnot(is.numeric(demo$income), all(is.finite(demo$income)),
            is.numeric(demo$y) || is.logical(demo$y),
            all(demo$y %in% c(0, 1)), nrow(demo) > 0)
  incomes <- sort(unique(demo$income))
  rates <- vapply(incomes, function(x) mean(demo$y[demo$income == x]), numeric(1))
  counts <- table(factor(demo$income, levels = incomes), factor(demo$y, levels = 0:1))
  linear_fit <- lm(y ~ income, data = demo)
  logistic_fit <- glm(y ~ income, data = demo, family = binomial())
  x_limits <- range(c(0, 800, demo$income))
  grid <- data.frame(income = seq(x_limits[1], x_limits[2], length.out = 400))
  displayed <- purchase_scatter_positions(demo$income, demo$y)
  for (j in 1:2) {
    plot(NA, xlim = x_limits, ylim = c(-0.25, 1.27),
         xlab = "年間所得 x（万円）", ylab = "",
         axes = FALSE,
         main = c("OLS", "Logistic")[j],
         font.main = 1, cex.main = 1.05)
    abline(h = c(0, 1), col = "#d8d8d8", lwd = 0.8)
    abline(h = 0.5, col = "#eeeeee", lwd = 0.6)
    x_ticks <- sort(unique(c(pretty(x_limits, n = 4), incomes)))
    x_labels <- as.character(x_ticks)
    if (length(incomes) == 3) {
      x_labels[match(incomes, x_ticks)] <- paste0(incomes, " (", LETTERS[1:3], ")")
    }
    axis(1, at = x_ticks, labels = x_labels, cex.axis = 0.85)
    axis(2, at = c(0, 0.5, 1), labels = c("0（未購入）", "0.5", "1（購入）"), cex.axis = 0.8)
    mtext("購入結果 y・予測確率", side = 2, line = 4.6, las = 0, cex = 0.85, col = "#333333")
    points(displayed$x, displayed$y, pch = 16, cex = 0.8,
           col = adjustcolor("#8b949a", alpha.f = 0.55))
    text(incomes, 1.08, paste0(counts[, 2], "人"), cex = 0.72, col = "#7a7a7a")
    text(incomes, -0.08, paste0(counts[, 1], "人"), cex = 0.72, col = "#7a7a7a")
    curve_y <- if (j == 1) predict(linear_fit, grid) else {
      predict(logistic_fit, grid, type = "response")
    }
    observed <- grid$income >= min(incomes) & grid$income <= max(incomes)
    lines(grid$income, curve_y, col = "#2e526b", lwd = 2.2, lty = "44")
    lines(grid$income[observed], curve_y[observed], col = "#2e526b", lwd = 2.5)
    points(incomes, rates, pch = 23, bg = "#b16b43", col = "white", cex = 1.35, lwd = 1.0)
    text(incomes, rates + 0.085, sprintf("%.2f", rates),
         cex = 0.82, col = "#9d5e3a")
    if (j == 1) {
      edge_y <- predict(linear_fit, data.frame(income = x_limits))
      for (k in 1:2) {
        if (edge_y[k] < 0 || edge_y[k] > 1) {
          points(x_limits[k], edge_y[k], pch = 21, bg = "white",
                 col = "#a4493c", lwd = 1.5, cex = 1.0)
          text(x_limits[k], if (edge_y[k] < 0) -0.215 else 1.215,
               sprintf("%.2f（%s）", edge_y[k], if (edge_y[k] < 0) "0未満" else "1超"),
               adj = if (k == 1) 0 else 1, col = "#a4493c", cex = 0.78)
        }
      }
    } else {
      text(mean(x_limits), 1.22, "予測確率は0と1の間", col = "#2e526b", cex = 0.78)
    }
  }

}

# Cグループの80対20を、4対1の模式例で読む。数値はすべてここで計算する。
fig_likelihood_bridge <- function() {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  font_family <- if (Sys.info()[["sysname"]] == "Darwin") "Hiragino Sans" else "sans"
  par(mfrow = c(1, 1), family = font_family,
      mar = c(0.3, 0.2, 2.3, 0.2), oma = c(3.5, 0, 0, 0))
  actual <- c(1, 1, 1, 1, 0)
  candidates <- c(0.50, 0.80, 0.99)
  assigned <- t(vapply(candidates, function(p) ifelse(actual == 1, p, 1 - p), numeric(5)))
  likelihood <- apply(assigned, 1, prod)
  log_loss <- -rowMeans(log(assigned))
  stopifnot(which.max(likelihood) == which.min(log_loss))

  plot.new()
  plot.window(xlim = c(0, 10.5), ylim = c(0.15, 4.65), xaxs = "i", yaxs = "i")
  title(main = "Cグループの80対20を、購入4人・未購入1人で表す", font.main = 1, cex.main = 1.05)
  cell_x <- c(1.72, 2.67, 3.62, 4.57, 5.72)
  row_y <- c(2.90, 1.95, 1.00)
  text(0.66, 4.27, "購入確率の候補", cex = 0.84)
  text(mean(cell_x[1:4]), 4.27, "購入した4人", col = "#21548a", cex = 0.92)
  text(cell_x[5], 4.27, "未購入の1人", col = "#a44e15", cex = 0.86)
  segments(1.29, 4.05, 4.99, 4.05, col = "#9bb4cb", lwd = 1.2)
  text(cell_x, 3.82, c(rep("y = 1", 4), "y = 0"), cex = 0.83, col = "#485661")
  text(cell_x, 3.47, c(rep("p", 4), "1 − p"), cex = 0.90,
       col = c(rep("#21548a", 4), "#a44e15"))
  text(7.24, 4.27, "5人分の確率の積", cex = 0.86)
  text(7.24, 3.82, "L", cex = 1.00)
  text(9.23, 4.27, "1人あたりの損失", cex = 0.86)
  text(9.23, 3.82, "Log Loss", cex = 0.95)
  text(7.24, 3.47, "大きいほどよい", cex = 0.75, col = "#566573")
  text(9.23, 3.47, "小さいほどよい", cex = 0.75, col = "#566573")

  for (j in seq_along(candidates)) {
    yy <- row_y[j]
    if (j == which.max(likelihood)) {
      rect(0.06, yy - 0.40, 10.42, yy + 0.40,
           col = "#eff5fa", border = "#a9c1d6", lwd = 1)
    }
    text(0.66, yy, sprintf("p = %.2f", candidates[j]), cex = 0.98,
         col = if (j == 2) "#21548a" else "#343d46")
    for (i in seq_along(actual)) {
      cell_fill <- if (i == 5) "#fbefe7" else "#e6eff7"
      cell_border <- if (i == 5) "#dabba2" else "#b7cbdc"
      cell_color <- if (i == 5) "#a44e15" else "#21548a"
      if (j == 3 && i == 5) {
        cell_fill <- "#f8ddce"
        cell_border <- "#b95620"
      }
      rect(cell_x[i] - 0.38, yy - 0.28, cell_x[i] + 0.38, yy + 0.28,
           col = cell_fill, border = cell_border, lwd = 1)
      text(cell_x[i], yy, sprintf("%.2f", assigned[j, i]),
           cex = 1.02, col = cell_color)
    }
    text((cell_x[-5] + cell_x[-1]) / 2, rep(yy, 4), "×",
         cex = 0.78, col = "#627180")
    arrows(6.22, yy, 6.50, yy, length = 0.06, angle = 22,
           col = "#627180", lwd = 1)
    text(7.24, yy, sprintf("%.8f", likelihood[j]), cex = 0.94,
         col = if (j == 2) "#21548a" else "#343d46")
    text(9.23, yy, sprintf("%.4f", log_loss[j]), cex = 1.04,
         col = if (j == 3) "#a44e15" else if (j == 2) "#21548a" else "#343d46")
  }
  mtext("購入確率を99%にすると、未購入者の結果に割り当てる確率は1%しかない。",
        side = 1, outer = TRUE, line = 0.25, cex = 0.83, col = "#a44e15")
  mtext("この5人は比率を読むための模式例。本番の学習では300人全員の結果を使う。",
        side = 1, outer = TRUE, line = 1.75, cex = 0.78, col = "#566573")
  invisible(data.frame(p = candidates, likelihood = likelihood, LogLoss = log_loss))
}

# 同じ購入者に対する二つの採点法。左右で縦軸の尺度が異なる。
fig_loss_comparison <- function() {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  font_family <- if (Sys.info()[["sysname"]] == "Darwin") "Hiragino Sans" else "sans"
  par(mfrow = c(1, 2), family = font_family,
      mar = c(4.2, 4.2, 3.4, 0.8), oma = c(3.0, 0, 0.6, 0), las = 1)
  r <- seq(0.01, 0.99, length.out = 500)
  examples <- c(0.2, 0.5, 0.8)
  scores <- list((1 - r)^2, -log(r))
  example_scores <- list((1 - examples)^2, -log(examples))
  titles <- c("二乗誤差で採点する", "Log Lossで採点する")
  formulas <- c("y = 1:  (1 − r)²", "y = 1:  −log(r)")
  upper_limits <- c(1.03, 4.95)
  colors <- c("#21548a", "#c06424")
  for (j in 1:2) {
    plot(r, scores[[j]], type = "n", xlim = c(0, 1), ylim = c(0, upper_limits[j]),
         xlab = "予測値・予測確率 r", ylab = "この方式での損失",
         main = titles[j], font.main = 1, cex.main = 1.05,
         axes = FALSE, xaxs = "i", yaxs = "i")
    abline(h = pretty(c(0, upper_limits[j])), col = "#e8edf2")
    axis(1, at = c(0, 0.2, 0.5, 0.8, 1), cex.axis = 0.85)
    axis(2, at = if (j == 1) seq(0, 1, 0.2) else 0:4, cex.axis = 0.85)
    box(bty = "l")
    lines(r, scores[[j]], col = colors[j], lwd = 2.6)
    segments(examples, 0, examples, example_scores[[j]], col = "#a7b4c0", lty = 3)
    points(examples, example_scores[[j]], pch = 21, bg = colors[j],
           col = "white", cex = 1.3, lwd = 1)
    text(examples, example_scores[[j]] + 0.10 * upper_limits[j],
         sprintf("%.3f", example_scores[[j]]), cex = 0.86, col = colors[j])
    mtext(formulas[j], side = 3, line = 0.35, cex = 0.88, col = colors[j])
  }
  mtext("どちらも、同じ「購入した人」について予測を採点している。",
        side = 1, outer = TRUE, line = 0.20, cex = 0.82)
  mtext("縦軸は各方式の採点値。左右の損失の数値を直接比べて、優劣を判断しない。",
        side = 1, outer = TRUE, line = 1.55, cex = 0.78, col = "#566573")
  invisible(data.frame(r = examples, SquaredError = example_scores[[1]],
                       LogLoss = example_scores[[2]]))
}

fig_probability_odds <- function() {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  font_family <- if (Sys.info()[["sysname"]] == "Darwin") "Hiragino Sans" else "sans"
  par(mfrow = c(1, 3), family = font_family, cex = 0.95,
      mar = c(4.2, 3.3, 3.2, 0.7), oma = c(3.1, 0, 0, 0), las = 1)
  p <- c(0.1, 0.2, 0.5, 0.8, 0.9)
  odds <- p / (1 - p)
  logit <- qlogis(p)
  rows <- 5:1
  cols <- c("#21548a", "#21548a", "#5c6873", "#c06424", "#c06424")
  values <- list(p, odds, logit)
  limits <- list(c(0, 1.17), c(0, 11.4), c(-2.65, 3.25))
  ticks <- list(c(0, 0.5, 1), c(0, 1, 3, 6, 9), c(-2, 0, 2))
  labels <- list(sprintf("%.2f", p), c("1/9", "1/4", "1", "4", "9"),
                 sprintf("%+.2f", logit))
  references <- c(0.5, 1, 0)
  titles <- c("① 確率 p", "② オッズ p / (1 − p)", "③ 対数オッズ logit(p)")
  xlabels <- c("0から1の間", "0より大きい", "負の値も正の値もとる")
  for (j in 1:3) {
    plot(NA, xlim = limits[[j]], ylim = c(0.4, 5.7), axes = FALSE,
         xlab = xlabels[j], ylab = "", main = titles[j],
         font.main = 1, cex.main = 0.91)
    abline(h = rows, col = "#edf0f3")
    abline(v = references[j], col = "#778897", lty = 2)
    segments(rep(0, 5), rows, values[[j]], rows, col = "#d3dce4", lwd = 3)
    points(values[[j]], rows, pch = 21, bg = cols, col = cols, cex = 1.3)
    text(values[[j]], rows, labels = labels[[j]], pos = 4,
         offset = 0.55, cex = 0.88, col = cols)
    axis(1, at = ticks[[j]], cex.axis = 0.88)
    if (j == 1) axis(2, at = rows, labels = paste0(100 * p, "%"),
                     tick = FALSE, cex.axis = 0.9)
    box(bty = "l")
  }
  mtext("同じ横の行は、同じ確率を別の数値で表している。", side = 1,
        outer = TRUE, line = 0.05, cex = 0.85)
  mtext("p = 0.5  →  odds = 1  →  logit(p) = 0", side = 1,
        outer = TRUE, line = 1.25, cex = 0.85, col = "#21548a")
}

# 同じ購入者数を、全体または未購入者数で割って読む。
fig_purchase_odds <- function(purchase) {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  font_family <- if (Sys.info()[["sysname"]] == "Darwin") "Hiragino Sans" else "sans"
  outcome <- if ("bought" %in% names(purchase)) "bought" else "y"
  stopifnot("income" %in% names(purchase), outcome %in% names(purchase),
            all(purchase[[outcome]] %in% c(0, 1)))
  incomes <- sort(unique(purchase$income))
  stopifnot(length(incomes) == 3)
  total <- vapply(incomes, function(x) sum(purchase$income == x), numeric(1))
  bought <- vapply(incomes, function(x) sum(purchase[[outcome]][purchase$income == x]), numeric(1))
  not_bought <- total - bought
  stopifnot(all(not_bought > 0))
  probability <- bought / total
  odds <- bought / not_bought
  par(mfrow = c(1, 3), family = font_family, cex = 1.1,
      mar = c(0.4, 0.7, 2.0, 0.7), oma = c(3.1, 0, 2.4, 0))
  for (j in seq_along(incomes)) {
    plot.new()
    plot.window(xlim = c(0, 100), ylim = c(0, 10), xaxs = "i", yaxs = "i")
    title(main = sprintf("%s：所得%d万円", LETTERS[j], incomes[j]),
          font.main = 1, cex.main = 1.05, line = 0.2)
    text(50, 9.45, sprintf("全体 %d人", total[j]), cex = 1.0, col = "#485661")
    rect(4, 8.1, 4 + 92 * probability[j], 8.95, col = "#c06424", border = NA)
    rect(4 + 92 * probability[j], 8.1, 96, 8.95, col = "#c8cfd5", border = NA)
    text(4 + 46 * probability[j], 8.52, sprintf("%d人", bought[j]), col = "white", cex = 0.9)
    text(4 + 92 * probability[j] + 46 * (1 - probability[j]), 8.52,
         sprintf("%d人", not_bought[j]), col = "#34404a", cex = 0.9)

    text(50, 6.95, "購入割合", cex = 1.05, col = "#21548a")
    text(50, 6.35, sprintf("購入 ÷ 全体%d人", total[j]), cex = 0.9, col = "#485661")
    text(50, 5.55, sprintf("%d / %d = %.2f", bought[j], total[j], probability[j]),
         cex = 1.2, col = "#21548a")
    text(50, 4.9, sprintf("全体の%d%%が購入", round(100 * probability[j])), cex = 0.90, col = "#21548a")
    segments(8, 4.25, 92, 4.25, col = "#dbe2e8")
    text(50, 3.6, "オッズ", cex = 1.05, col = "#a44e15")
    text(50, 3.0, sprintf("購入 ÷ 未購入%d人", not_bought[j]), cex = 0.9, col = "#485661")
    text(50, 2.15, sprintf("%d / %d = %s", bought[j], not_bought[j], format(odds[j], trim = TRUE)),
         cex = 1.2, col = "#a44e15")
    ratio_label <- if (abs(odds[j] - 0.25) < 1e-8) "購入は未購入の1/4" else {
      if (abs(odds[j] - 1) < 1e-8) "購入と未購入は同じ人数" else sprintf("購入は未購入の%s倍", format(odds[j], trim = TRUE))
    }
    text(50, 1.35, ratio_label, cex = 0.88, col = "#a44e15")
  }
  mtext("分子は購入者のまま。分母を変える。", side = 3, outer = TRUE,
        line = 0.4, cex = 1.1)
  mtext("■ 購入した", side = 1, outer = TRUE, line = 0.3, at = 0.38, col = "#c06424", cex = 0.88)
  mtext("■ 購入しなかった", side = 1, outer = TRUE, line = 0.3, at = 0.63, col = "#657584", cex = 0.88)
  mtext("オッズ4は「4倍」という人数比。購入確率400%ではない。", side = 1,
        outer = TRUE, line = 1.5, cex = 0.84, col = "#566573")
  invisible(data.frame(income = incomes, total = total, bought = bought, probability = probability, odds = odds))
}

fig_coefficient_effect <- function(purchase_fit = NULL) {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  font_family <- if (Sys.info()[["sysname"]] == "Darwin") "Hiragino Sans" else "sans"
  if (is.null(purchase_fit)) {
    purchase <- data.frame(
      income = rep(c(200, 400, 600), each = 100),
      bought = unlist(lapply(c(20, 50, 80), function(n) c(rep(1, n), rep(0, 100 - n))))
    )
    purchase_fit <- glm(bought ~ income, data = purchase, family = binomial())
  }
  original <- data.frame(income = c(200, 400, 600))
  higher <- data.frame(income = original$income + 100)
  before <- predict(purchase_fit, newdata = original, type = "response")
  after <- predict(purchase_fit, newdata = higher, type = "response")
  beta <- unname(coef(purchase_fit)["income"])
  par(family = font_family, mar = c(4.5, 10.0, 4.5, 1.2),
      oma = c(2.8, 0, 0, 0), las = 1)
  rows <- 3:1
  plot(NA, xlim = c(0, 1), ylim = c(0.4, 3.7), axes = FALSE,
       xlab = "購入する予測確率", ylab = "",
       main = "所得が100万円高いと、予測確率は？", font.main = 1, cex.main = 1.05)
  mtext(sprintf("どの行もスコア ＋%.3f、オッズ ×%.1f", beta * 100, exp(beta * 100)),
        side = 3, line = 0.65, cex = 0.9, col = "#526171")
  abline(v = seq(0, 1, 0.1), col = "#eef1f4")
  axis(1, at = seq(0, 1, 0.2), labels = paste0(seq(0, 100, 20), "%"))
  axis(2, at = rows, labels = sprintf("%s：%d → %d万円", LETTERS[1:3], original$income, higher$income),
       tick = FALSE, cex.axis = 0.91)
  box(bty = "l")
  arrows(before, rows, after, rows, length = 0.10, angle = 25,
         col = "#748596", lwd = 2)
  points(before, rows, pch = 21, bg = "white", col = "#21548a", cex = 1.6, lwd = 2)
  points(after, rows, pch = 15, col = "#c06424", cex = 1.3)
  for (j in 1:3) {
    label_x <- mean(c(before[j], after[j]))
    text(label_x, rows[j] + 0.30,
         sprintf("%.1f%% → %.1f%%", 100 * before[j], 100 * after[j]),
         cex = 0.93)
    text(label_x, rows[j] - 0.29,
         sprintf("＋%.1fポイント", 100 * (after[j] - before[j])),
         cex = 0.88, col = "#c06424")
  }
  mtext("○ もとの所得での予測　　■ 100万円高い所得での予測", side = 1,
        outer = TRUE, line = 0.1, cex = 0.85)
  mtext("同じモデルで所得だけを変えた比較。所得を増やすことの因果効果ではない。", side = 1,
        outer = TRUE, line = 1.5, cex = 0.81, col = "#566573")
  invisible(data.frame(income = original$income, before = before, after = after,
                       change_points = 100 * (after - before)))
}

fig_likelihood <- function() {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  font_family <- if (Sys.info()[["sysname"]] == "Darwin") "Hiragino Sans" else "sans"
  par(mfrow = c(1, 2), family = font_family,
      mar = c(4.5, 4.2, 3.5, 0.8), oma = c(3.5, 0, 0, 0), las = 1)
  y <- c(1, 0, 1, 0)
  p_a <- c(0.8, 0.2, 0.7, 0.3)
  p_b <- c(0.6, 0.4, 0.55, 0.45)
  q_a <- ifelse(y == 1, p_a, 1 - p_a)
  q_b <- ifelse(y == 1, p_b, 1 - p_b)
  cols <- c("#21548a", "#c06424")
  probability <- rbind(q_a, q_b)
  loss <- -log(probability)
  for (j in 1:2) {
    values <- if (j == 1) probability else loss
    centers <- barplot(values, beside = TRUE, col = cols, border = NA,
                       names.arg = paste0(1:4, "人目\ny = ", y),
                       ylim = if (j == 1) c(0, 1.15) else c(0, 0.85),
                       ylab = if (j == 1) "正解に割り当てた確率" else "−log P(observed outcome)",
                       main = c("確率が高いほど、正解を支持する", "対数を取ると、積が和になる")[j],
                       font.main = 1, cex.main = 0.97, cex.names = 0.85)
    text(centers, values + if (j == 1) 0.045 else 0.035,
         labels = sprintf("%.2f", values), cex = 0.8)
    legend("topright", legend = c("モデルA", "モデルB"), fill = cols,
           border = NA, bty = "n", horiz = TRUE, cex = 0.83)
    box(bty = "l")
  }
  mtext(sprintf("尤度（4人の確率の積）　A：%.4f　　B：%.4f", prod(q_a), prod(q_b)),
        side = 1, outer = TRUE, line = 0.4, cex = 0.88)
  mtext(sprintf("Log Loss（損失の平均）　A：%.3f　　B：%.3f", mean(-log(q_a)), mean(-log(q_b))),
        side = 1, outer = TRUE, line = 1.85, cex = 0.88)
}

fig_log_loss <- function() {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  font_family <- if (Sys.info()[["sysname"]] == "Darwin") "Hiragino Sans" else "sans"
  par(family = font_family, mar = c(4.5, 4.5, 3.2, 1),
      oma = c(2.6, 0, 0, 0), las = 1)
  p <- seq(0.01, 0.99, length.out = 500)
  plot(p, -log(p), type = "n", xlim = c(0, 1), ylim = c(0, 5.2),
       xlab = "y = 1と予測する確率 p", ylab = "1人分の損失",
       xaxt = "n", main = "確信して間違うほど、大きな損失になる",
       font.main = 1, cex.main = 1.05)
  abline(h = 0:5, col = "#edf0f3")
  axis(1, at = c(0.01, 0.25, 0.5, 0.75, 0.99))
  lines(p, -log(p), col = "#21548a", lwd = 2.5)
  lines(p, -log(1 - p), col = "#c06424", lwd = 2.5, lty = 2)
  legend("top", legend = c("y = 1: −log(p)", "y = 0: −log(1 − p)"),
         col = c("#21548a", "#c06424"), lty = c(1, 2), lwd = 2.5,
         bty = "n", cex = 0.9)
  points(c(0.01, 0.5), -log(c(0.01, 0.5)), pch = 16, col = "#21548a", cex = 1.1)
  points(0.99, -log(0.01), pch = 17, col = "#c06424", cex = 1.1)
  text(0.03, 4.85, "4.605", adj = 0, col = "#21548a", cex = 0.85)
  text(0.97, 4.85, "4.605", adj = 1, col = "#c06424", cex = 0.85)
  text(0.5, 1.13, "−log(0.5) ≈ 0.693", cex = 0.9)
  mtext("表示範囲はp = 0.01〜0.99。", side = 1, outer = TRUE, line = 0.05, cex = 0.82)
  mtext("y = 1でp → 0、またはy = 0でp → 1のとき、損失は限りなく大きくなる。",
        side = 1, outer = TRUE, line = 1.25, cex = 0.82, col = "#566573")
}

fig_linear_boundary <- function() {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  font_family <- if (Sys.info()[["sysname"]] == "Darwin") "Hiragino Sans" else "sans"
  layout(matrix(c(1, 2), nrow = 1), widths = c(5.7, 1.5))
  par(family = font_family, mar = c(4.5, 4.5, 4.5, 0.8),
      oma = c(2.0, 0, 0, 0), las = 1)
  x1 <- x2 <- seq(0, 6, length.out = 241)
  probabilities <- outer(x1, x2, function(a, b) plogis(-4 + 0.8 * a + b))
  palette <- colorRampPalette(c("#f6f9fc", "#bfd2e3", "#21548a"))(160)
  image(x1, x2, probabilities, zlim = c(0, 1), col = palette, useRaster = TRUE,
        xlab = expression(x[1]), ylab = expression(x[2]),
        main = "同じ確率をもつ点は直線上に並ぶ", font.main = 1,
        cex.main = 1.0, xaxs = "i", yaxs = "i")
  mtext(expression(z == -4 + 0.8 * x[1] + x[2]), side = 3, line = 0.6, cex = 0.95)
  for (threshold in c(0.3, 0.5, 0.7)) {
    intercept <- qlogis(threshold) + 4
    line_col <- if (threshold == 0.5) "#a84b12" else "#485e70"
    abline(a = intercept, b = -0.8, col = line_col,
           lwd = if (threshold == 0.5) 2.8 else 1.6,
           lty = if (threshold == 0.5) 1 else 2)
    text(0.2, intercept - 0.16 + 0.15, paste0("p = ", threshold),
         adj = c(0, 0), cex = 0.83, col = line_col)
  }
  text(3.9, 4.65, "p >= 0.5\ny = 1と分類", col = "white", cex = 0.95)
  text(1.2, 0.65, "p < 0.5\ny = 0と分類", col = "#21548a", cex = 0.95)
  box()
  par(mar = c(4.5, 0.8, 4.5, 3.2))
  color_y <- seq(0, 1, length.out = 161)
  image(c(0, 1), color_y, matrix(rep(color_y, each = 2), nrow = 2),
        col = palette, zlim = c(0, 1), axes = FALSE, xlab = "", ylab = "", useRaster = TRUE)
  axis(4, at = c(0, 0.25, 0.5, 0.75, 1), labels = c("0", "0.25", "0.5", "0.75", "1"))
  mtext("確率 p", side = 3, line = 0.6, cex = 0.9)
  box()
  mtext("確率の曲線はS字でも、元の特徴量に線形なlogitの分類境界は直線になる。",
        side = 1, outer = TRUE, line = 0.3, cex = 0.84)
}

# 購入割合を、100人の小さな点の集まりとして見る。
fig_purchase_groups <- function() {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  font_family <- if (Sys.info()[["sysname"]] == "Darwin") "Hiragino Sans" else "sans"
  par(mfrow = c(1, 3), family = font_family, cex = 0.95,
      mar = c(3.7, 0.5, 2.5, 0.5), oma = c(3.1, 0, 0, 0), las = 1)
  purchased <- c(20, 50, 80)
  group_colors <- c("#21548a", "#5c6873", "#c06424")
  people <- expand.grid(column = 1:10, row = 10:1)
  for (j in seq_along(purchased)) {
    n <- purchased[j]
    plot(NA, xlim = c(0.4, 10.6), ylim = c(0.4, 10.6),
         axes = FALSE, xlab = "", ylab = "", asp = 1,
         main = paste0("グループ ", LETTERS[j], "（100人）"),
         font.main = 1, cex.main = 1.0, col.main = group_colors[j])
    points(people$column, people$row, pch = 16, cex = 1.45,
           col = c(rep("#c06424", n), rep("#c8cfd5", 100 - n)))
    mtext(sprintf("購入 %d人・未購入 %d人", n, 100 - n), side = 1,
          line = 0.7, cex = 0.88)
    # 日本語の説明と、数値による式は行を分ける。
    mtext(sprintf("%d / 100 = %d%%", n, n), side = 1,
          line = 2.2, cex = 0.91, col = group_colors[j])
  }
  mtext("● 購入した", side = 1, outer = TRUE, line = 0.3,
        at = 0.37, col = "#c06424", cex = 0.85)
  mtext("● 購入しなかった", side = 1, outer = TRUE, line = 0.3,
        at = 0.60, col = "#657584", cex = 0.85)
  mtext("点1つが1人を表す。3グループとも全体は100人。", side = 1,
        outer = TRUE, line = 1.35, cex = 0.79, col = "#566573")
}

# オッズの倍率が、対数オッズの加法になることを場面別のカードで示す。
# カードの位置は、オッズを一定間隔の数軸上に置いたものではない。
fig_odds_to_logit <- function() {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  font_family <- if (Sys.info()[["sysname"]] == "Darwin") "Hiragino Sans" else "sans"
  par(family = font_family, mar = c(0.3, 0.3, 3.2, 0.3),
      oma = c(2.6, 0, 0, 0), las = 1)
  plot.new()
  plot.window(xlim = c(0, 12), ylim = c(0, 8.2), xaxs = "i", yaxs = "i")
  title(main = "オッズの「4倍」を、logitの「足し算」に変える",
        font.main = 1, cex.main = 1.05, line = 1.0)

  centers <- c(2, 6, 10)
  colors <- c("#21548a", "#5c6873", "#c06424")
  fills <- c("#edf4fa", "#f0f2f4", "#fbf1e9")
  odds_labels <- c("1/4", "1", "4")
  logit_labels <- c("-1.39", "0", "+1.39")
  for (j in seq_along(centers)) {
    x <- centers[j]
    text(x, 7.9, paste0("グループ ", LETTERS[j]), col = colors[j], cex = 0.95)
    rect(x - 1.15, 5.9, x + 1.15, 7.35,
         col = fills[j], border = colors[j], lwd = if (j == 2) 2.1 else 1.2)
    rect(x - 1.15, 1.7, x + 1.15, 3.15,
         col = fills[j], border = colors[j], lwd = if (j == 2) 2.1 else 1.2)
    text(x, 7.00, "odds", col = colors[j], cex = 0.87)
    text(x, 6.36, odds_labels[j], col = colors[j], cex = 1.55)
    text(x, 2.80, "logit", col = colors[j], cex = 0.87)
    text(x, 2.17, logit_labels[j], col = colors[j],
         cex = if (j == 2) 1.65 else 1.45, font = if (j == 2) 2 else 1)
    arrows(x, 5.65, x, 3.4, length = 0.12, angle = 25,
           col = colors[j], lwd = 1.6)
    text(x + 0.18, 4.5, "log", adj = 0, col = colors[j], cex = 1.05)
  }
  for (j in 1:2) {
    from <- centers[j] + 1.34
    to <- centers[j + 1] - 1.34
    middle <- mean(c(from, to))
    arrows(from, 6.47, to, 6.47, length = 0.10, angle = 25,
           col = "#657584", lwd = 1.5)
    text(middle, 7.04, "× 4", cex = 1.05, col = "#465868")
    arrows(from, 2.27, to, 2.27, length = 0.10, angle = 25,
           col = "#657584", lwd = 1.5)
    text(middle, 2.86, "+ 1.39", cex = 0.98, col = "#465868")
  }
  text(6, 0.80, "log(4) = 1.386...", cex = 0.88, col = "#566573")
  mtext("logitは小数第2位に丸めて表示。", side = 1,
        outer = TRUE, line = 0.2, cex = 0.80, col = "#566573")
  mtext("カードの並びは3つの場面を表す。図の間隔は数値の距離を表さない。", side = 1,
        outer = TRUE, line = 1.45, cex = 0.80, col = "#566573")
}

# 確率とオッズの関係を示す。
fig_probability_odds_curve <- function() {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  font_family <- if (Sys.info()[["sysname"]] == "Darwin") "Hiragino Sans" else "sans"
  par(family = font_family, mar = c(4.4, 4.8, 2.7, 1.4), las = 1,
      col.axis = "#555555", col.lab = "#333333", fg = "#999999")
  plot(NA, xlim = c(0, 1), ylim = c(0, 10), xlab = "確率 p",
       ylab = "オッズ p / (1 − p)", axes = FALSE,
       main = "確率とオッズの関係", font.main = 1)
  axis(1, at = c(0, 0.2, 0.5, 0.8, 1))
  axis(2, at = c(0, 1, 2, 4, 6, 8, 10))
  box(bty = "l")
  abline(v = 1, col = "#bbbbbb", lty = 2)
  p <- seq(0, 10 / 11, length.out = 1000)
  lines(p, p / (1 - p), col = "#2e526b", lwd = 3)
  examples <- c(0.2, 0.5, 0.8)
  values <- examples / (1 - examples)
  points(examples, values, pch = 21, bg = "#b16b43", col = "white", cex = 1.4)
  text(examples, values, labels = c("A: 0.25", "B: 1", "C: 4"),
       pos = c(3, 3, 2), offset = 1.4, cex = 0.85, col = "#a45f3c")
  text(0.72, 9.2, "p → 1：オッズ → +∞", adj = 1, cex = 0.8, col = "#555555")
}

# 確率からLogitへの変換と両端の発散を示す。
fig_logit_range <- function() {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  font_family <- if (Sys.info()[["sysname"]] == "Darwin") "Hiragino Sans" else "sans"
  par(family = font_family, mar = c(4.4, 4.8, 2.7, 1.4), las = 1,
      col.axis = "#555555", col.lab = "#333333", fg = "#999999")
  plot(NA, xlim = c(0, 1), ylim = c(-5.5, 5.5), xlab = "確率 p",
       ylab = "Logit（対数オッズ）", axes = FALSE,
       main = "確率とLogitの関係", font.main = 1)
  axis(1, at = c(0, 0.2, 0.5, 0.8, 1))
  axis(2, at = c(-4, -2, 0, 2, 4))
  box(bty = "l")
  abline(h = 0, col = "#dddddd")
  abline(v = c(0, 1), col = "#bbbbbb", lty = 2)
  p <- seq(0.0041, 0.9959, length.out = 1000)
  lines(p, qlogis(p), col = "#2e526b", lwd = 3)
  examples <- c(0.2, 0.5, 0.8)
  points(examples, qlogis(examples), pch = 21, bg = "#b16b43", col = "white", cex = 1.4)
  text(examples, qlogis(examples), labels = c("A: −1.386", "B: 0", "C: 1.386"),
       pos = 3, offset = 1.4, cex = 0.85, col = "#a45f3c")
  text(0.06, -4.8, "p → 0：Logit → −∞", adj = 0, cex = 0.8, col = "#555555")
  text(0.94, 4.8, "p → 1：Logit → +∞", adj = 1, cex = 0.8, col = "#555555")
}

# 学習した3グループのスコアを、逆リンク関数で確率に戻す。
fig_logistic_inverse <- function(purchase) {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  font_family <- if (Sys.info()[["sysname"]] == "Darwin") "Hiragino Sans" else "sans"
  stopifnot(all(c("income", "bought") %in% names(purchase)),
            is.numeric(purchase$income), all(is.finite(purchase$income)),
            all(purchase$bought %in% c(0, 1)))
  groups <- data.frame(income = sort(unique(purchase$income)))
  stopifnot(nrow(groups) == 3)
  purchase_fit <- glm(bought ~ income, data = purchase, family = binomial())
  groups$score <- unname(predict(purchase_fit, newdata = groups, type = "link"))
  groups$probability <- unname(predict(purchase_fit, newdata = groups, type = "response"))

  par(family = font_family, mar = c(4.2, 4.5, 2.8, 1.0),
      oma = c(0, 0, 0, 0), las = 1)
  z <- seq(-5, 5, length.out = 501)
  plot(z, plogis(z), type = "n", xlim = c(-5, 5), ylim = c(0, 1),
       xaxs = "i", yaxs = "i", axes = FALSE,
       xlab = "Logit（対数オッズ）", ylab = "購入する確率 p",
       main = "スコアから確率へ", font.main = 1, cex.main = 1.05)
  abline(h = seq(0, 1, 0.2), col = "#edf0f3")
  abline(v = 0, col = "#d8dfe5", lty = 2)
  axis(1, at = -5:5)
  axis(2, at = seq(0, 1, 0.2), labels = sprintf("%.1f", seq(0, 1, 0.2)))
  box(bty = "l")
  lines(z, plogis(z), col = "#21548a", lwd = 3)
  text(-4.5, 0.84, expression(sigma(z) == frac(1, 1 + exp(-z))),
       adj = 0, cex = 1.05, col = "#21548a")
  points(groups$score, groups$probability, pch = 23,
         bg = "#c06424", col = "white", cex = 1.7, lwd = 1.1)
  text(groups$score - 0.22, groups$probability + 0.05,
       sprintf("%s %.0f%%", LETTERS[seq_len(nrow(groups))], 100 * groups$probability),
       adj = c(1, 0), col = "#a44e15", cex = 0.93)
  invisible(groups)
}

# 実際の0・1に対応するLog Lossの曲線。
fig_binary_log_loss <- function() {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  font_family <- if (Sys.info()[["sysname"]] == "Darwin") "Hiragino Sans" else "sans"
  par(family = font_family, mar = c(4.5, 4.5, 2.6, 1.2), las = 1,
      col.axis = "#555555", col.lab = "#333333", fg = "#999999")
  plot(NA, xlim = c(0, 1), ylim = c(0, 4.7), xlab = "予測する購入確率 p",
       ylab = "1人のLog Loss", main = "実際の結果ごとの損失", font.main = 1)
  abline(h = 0, col = "#dddddd")
  p <- seq(0.01, 0.99, length.out = 500)
  lines(p, -log(p), col = "#245780", lwd = 2.5)
  lines(p, -log1p(-p), col = "#b76531", lwd = 2.5)
  points(0.5, log(2), pch = 21, bg = "white", col = "#555555", cex = 1.1, lwd = 1.5)
  legend("top", legend = c("実際に購入（y = 1）：−log(p)", "実際は未購入（y = 0）：−log(1 − p)"),
         col = c("#245780", "#b76531"), text.col = "#333333", lwd = 2.5, bty = "n", cex = 0.95)
}
