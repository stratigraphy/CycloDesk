# make_icon.R: CycloDesk icon, a bold R with a Morlet wavelet in front ####
draw_icon <- function(size) {
  png(sprintf("icon_%d.png", size), width = size, height = size, bg = "transparent", family = "sans")
  op <- par(mar = c(0, 0, 0, 0))
  plot.new()
  plot.window(c(0, 1), c(0, 1), xaxs = "i", yaxs = "i")
  r <- 0.2
  th <- seq(0, pi / 2, length.out = 30)
  cx <- c(1 - r, r, r, 1 - r)
  cy <- c(1 - r, 1 - r, r, r)
  px <- numeric()
  py <- numeric()
  st <- c(0, pi / 2, pi, 3 * pi / 2)
  for (k in 1:4) {
    a <- st[k] + th
    px <- c(px, cx[k] + r * cos(a))
    py <- c(py, cy[k] + r * sin(a))
  }
  # vertical gradient from deep to lighter blue, clipped to the rounded square
  gx <- px * 0.96 + 0.02
  gy <- py * 0.96 + 0.02
  pal <- grDevices::colorRampPalette(c("#173b66", "#3a72b8"))(40)
  for (k in 1:40) {
    y0 <- (k - 1) / 40
    y1 <- k / 40
    yy <- pmin(pmax(gy, y0), y1)
    polygon(gx, yy, col = pal[k], border = NA)
  }
  cexr <- 1
  h <- strheight("R", cex = 1, font = 2)
  cexr <- 0.5 / h
  text(0.5, 0.6, "R", font = 2, cex = cexr, col = "white")
  t <- seq(-1, 1, length.out = 400)
  x <- 0.08 + 0.84 * (t + 1) / 2
  nc <- if (size <= 32) { 2 } else { 3 }
  y <- 0.3 + 0.17 * exp(-t^2 / (2 * 0.33^2)) * cos(2 * pi * nc * t)
  lw <- size / 256 * if (size <= 32) { 1.8 } else { 1 }
  lines(x, y, col = "#1d4677", lwd = 22 * lw, lend = 1)
  lines(x, y, col = "#ffb02e", lwd = 12 * lw, lend = 1)
  par(op)
  dev.off()
}
sizes <- c(256, 128, 64, 48, 32, 16)
for (s in sizes) {
  draw_icon(s)
}
library(magick)
im <- image_read(sprintf("icon_%d.png", sizes))
image_write(im, "cyclodesk.ico", format = "ico")
image_write(image_read("icon_256.png"), "cyclodesk.png", format = "png")
cat(file.size("cyclodesk.ico"), "\n")
