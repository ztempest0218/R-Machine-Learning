# Run from the course repository root. All labels and arrows share one PNG canvas.
png("005-logistic-regression/logit-flow.png", width = 3000, height = 1080,
    res = 300, type = "quartz", bg = "white")
par(mar = c(0, 0, 0, 0), family = "Hiragino Sans", xaxs = "i", yaxs = "i")
plot.new()
plot.window(xlim = c(0, 1000), ylim = c(0, 360))
ink <- "#333333"
line <- "#777777"
# Smooth outer arrows, kept clear of the formulas and intermediate labels.
curve_arrow <- function(points) {
  t <- seq(0, 1, length.out = 150)
  xy <- outer((1-t)^3, points[1, ]) + outer(3*(1-t)^2*t, points[2, ]) +
    outer(3*(1-t)*t^2, points[3, ]) + outer(t^3, points[4, ])
  lines(xy, col = line, lwd = 1.1)
  arrows(xy[147, 1], xy[147, 2], xy[150, 1], xy[150, 2],
         length = 0.08, angle = 25, col = line, lwd = 1.1)
}
curve_arrow(rbind(c(140, 225), c(320, 285), c(680, 285), c(860, 225)))
curve_arrow(rbind(c(860, 135), c(680, 75), c(320, 75), c(140, 135)))
arrows(c(215, 610), 180, c(390, 785), 180, length = 0.09,
       angle = 25, col = line, lwd = 1.1)
rect(c(35, 390, 785), 135, c(215, 610, 965), 225,
     col = c("white", "white", "#e8e8e8"), border = "#999999", lwd = 1)
text(c(125, 500, 875), 202, c("確率", "オッズ", "Logit"), col = ink, cex = 1.05)
text(125, 163, expression(italic(p)), col = ink, cex = 1.15)
text(500, 164, expression(frac(p, 1-p)), col = ink, cex = 1.1)
text(875, 163, expression(italic(z)), col = ink, cex = 1.15)
text(302, 215, expression(frac(p, 1-p)), col = ink, cex = 1.05)
text(698, 215, expression(paste("自然対数 ", log)), col = ink, cex = 1)
text(500, 320, expression(paste("Logit関数： ", plain(logit)(p) == plain(log) * bgroup("(", frac(p, 1-p), ")"))),
     col = ink, cex = 1.1)
text(500, 43, expression(paste("Logistic関数： ", p == frac(1, 1+e^{-z}))),
     col = ink, cex = 1.1)
dev.off()
