# Run from the course repository root. Render real fonts and math as vector paths,
# plus a 300 dpi PNG for download. No generated lettering is used.
draw_logit_flow <- function() {
  par(mar = c(0, 0, 0, 0), family = "Hiragino Sans", xaxs = "i", yaxs = "i")
  plot.new()
  plot.window(xlim = c(0, 1000), ylim = c(0, 415))
  ink <- "#222222"
  line <- "#555555"
  # Draw boxes first; arrow tips have a visible gap from their borders.
  rect(c(35, 390, 785), 145, c(215, 610, 965), 245,
       col = c("white", "white", "#eeeeee"), border = "#888888", lwd = 1.1)
  curve_arrow <- function(points) {
    t <- seq(0, 1, length.out = 200)
    xy <- outer((1-t)^3, points[1, ]) + outer(3*(1-t)^2*t, points[2, ]) +
      outer(3*(1-t)*t^2, points[3, ]) + outer(t^3, points[4, ])
    lines(xy, col = line, lwd = 1.3)
    arrows(xy[190, 1], xy[190, 2], xy[200, 1], xy[200, 2],
           length = 0.10, angle = 28, col = line, lwd = 1.3)
  }
  curve_arrow(rbind(c(140, 245), c(320, 322), c(680, 322), c(860, 254)))
  curve_arrow(rbind(c(860, 145), c(680, 76), c(320, 76), c(140, 136)))
  arrows(c(222, 617), 195, c(382, 777), 195, length = 0.10,
         angle = 28, col = line, lwd = 1.3)
  text(c(125, 500, 875), 220, c("確率", "オッズ", "Logit"), col = ink, cex = 1)
  text(125, 175, expression(italic(p)), col = ink, cex = 1.1)
  text(500, 178, expression(frac(p, 1-p)), col = ink, cex = 1)
  text(875, 175, expression(italic(z)), col = ink, cex = 1.1)
  text(302, 234, expression(frac(p, 1-p)), col = ink, cex = 0.95)
  text(698, 230, "自然対数 log", col = ink, cex = 0.95)
  # Plotmath's extensible parentheses have visible joins. Draw each parenthesis
  # as one continuous Bezier curve around the typeset fraction instead.
  prefix <- expression(paste("Logit関数： ", plain(logit)(p) == plain(log)))
  fraction <- expression(frac(p, 1-p))
  prefix_width <- strwidth(prefix, units = "user", cex = 1)
  fraction_width <- strwidth(fraction, units = "user", cex = 1)
  left <- 500 - (prefix_width + fraction_width + 30) / 2
  text(left, 365, prefix, adj = c(0, 0.5), col = ink, cex = 1)
  fraction_x <- left + prefix_width + 15 + fraction_width / 2
  text(fraction_x, 365, fraction, col = ink, cex = 1)
  t <- seq(0, 1, length.out = 200)
  smooth_parenthesis <- function(x, direction) {
    points <- cbind(x + direction * c(5, -6, -6, 5),
                    365 + c(25, 12, -12, -25))
    xy <- outer((1-t)^3, points[1, ]) + outer(3*(1-t)^2*t, points[2, ]) +
      outer(3*(1-t)*t^2, points[3, ]) + outer(t^3, points[4, ])
    lines(xy, col = ink, lwd = 1.2)
  }
  smooth_parenthesis(fraction_x - fraction_width / 2 - 7, 1)
  smooth_parenthesis(fraction_x + fraction_width / 2 + 7, -1)
  text(500, 43,
       expression(paste("Logistic関数： ", p == frac(1, 1+e^{-z}))),
       col = ink, cex = 1)
}
svg("005-logistic-regression/logit-flow-clear.svg", width = 10, height = 4.15,
    pointsize = 16, family = "Hiragino Sans", bg = "white")
draw_logit_flow()
dev.off()
png("005-logistic-regression/logit-flow-clear.png", width = 3000, height = 1245,
    res = 300, pointsize = 16, type = "quartz", bg = "white")
draw_logit_flow()
dev.off()
