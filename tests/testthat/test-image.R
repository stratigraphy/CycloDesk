test_that("cd_image_profile recovers the bed thickness of a synthetic outcrop image", {
  skip_if_not_installed("png")
  # beds of 40 px, tilted by 20 degrees, with noise; 100 px per metre, so 0.4 m beds
  W <- 600
  H <- 300
  ang <- 20 * pi / 180
  xx <- matrix(rep(seq_len(W), each = H), H, W)
  yy <- matrix(rep(seq_len(H), W), H, W)
  u <- xx * cos(ang) + yy * sin(ang)
  set.seed(1)
  g <- 0.5 + 0.3 * sin(2 * pi * u / 40) + matrix(stats::rnorm(W * H, 0, 0.15), H, W)
  g <- pmin(pmax(g, 0), 1)
  img <- array(0, c(H, W, 3))
  img[, , 1] <- g
  img[, , 2] <- 0.9 * g
  img[, , 3] <- 0.7 * g
  f <- tempfile(fileext = ".png")
  png::writePNG(img, f)
  # a route perpendicular to the beds
  route <- data.frame(x = c(100, 100 + 400 * cos(ang)), y = c(80, 80 + 400 * sin(ang)))
  prof <- cd_image_profile(f, route, width = 21, step = 1, scale = 100)
  expect_equal(nrow(prof), 401)
  expect_equal(max(prof$depth), 4, tolerance = 1e-6)
  expect_true(all(c("grey", "L", "a", "b", "R", "G", "B", "hue", "saturation", "value") %in% names(prof)))
  # dominant period of the grey profile: 0.4 m
  y <- prof$grey - mean(prof$grey)
  n <- length(y)
  sp <- Mod(stats::fft(c(y, numeric(7 * n))))^2
  fr <- (seq_along(sp) - 1) / (length(sp) * 0.01)
  keep <- fr > 0.5 & fr < 20
  expect_equal(1 / fr[keep][which.max(sp[keep])], 0.4, tolerance = 0.03)
  # the swath averages out the noise: a 21-px swath is much smoother than a 1-px line
  p1 <- cd_image_profile(f, route, width = 1, scale = 100)
  expect_lt(stats::sd(diff(prof$grey)), stats::sd(diff(p1$grey)) / 2)
  # median, direction, start, and a grey-scale array
  pm <- cd_image_profile(img[, , 1], route, width = 11, stat = "median", scale = 100, start = 10, direction = -1)
  expect_equal(range(pm$depth), c(6, 10), tolerance = 1e-6)
  expect_equal(pm$R, pm$G)
  expect_error(cd_image_profile(img, data.frame(x = 1, y = 1)), "two points")
  # the figures
  grDevices::pdf(NULL)
  cd_plot_image_route(cd_image_display(img), route = route, width = 21, scale_pts = data.frame(x = c(10, 110), y = c(290, 290)))
  cd_plot_image_profile(prof, "L", "Depth (m)")
  grDevices::dev.off()
  # a JPEG is read as well
  skip_if_not_installed("jpeg")
  fj <- tempfile(fileext = ".jpg")
  jpeg::writeJPEG(img, fj, quality = 0.95)
  expect_equal(dim(cd_image_read(fj)), c(H, W, 3L))
})
