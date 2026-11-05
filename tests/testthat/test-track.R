test_that("snapped track, tuning check and age plot work", {
  d <- cd_even(as.data.frame(WaverideR::grey))
  wt <- WaverideR::analyse_wavelet(as.matrix(d), dj = 1 / 20, lowerPeriod = 0.02, upperPeriod = 5, verbose = FALSE)
  gt <- as.data.frame(WaverideR::grey_track)[, 1:2]
  tr <- cd_snap_track(wt, gt[seq(1, nrow(gt), length.out = 8), ], tol = 0.3)
  expect_equal(nrow(tr), length(wt$axis.1))
  r <- cd_track_check(wt, tr, 110, 21, 110, mod_down = 0.75, mod_up = 1.3, env_down = 0.85, env_up = 1.15)
  expect_true(is.finite(r$r) && is.finite(r$r_raw))
  expect_equal(r$mod_band, c(0.75, 1.3))
  expect_error(cd_track_check(wt, tr, 110, 21, 110, mod_down = 1.3, mod_up = 0.75), "lower factor")
  expect_error(cd_track_check(wt, tr, 110, 21, 5000), "too short")
  am <- as.data.frame(WaverideR::curve2time(tracked_cycle_curve = tr, tracked_cycle_period = 110))
  tu <- as.data.frame(astrochron::tune(d, controlPts = am, genplot = FALSE, verbose = FALSE))
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  cd_plot_track_check(r)
  cd_plot_track_age(list(x = range(d[, 1])), d, am, tu, 110, bounds = 160)
  grDevices::dev.off()
  expect_true(file.exists(f))
})

test_that("cd_extract gives the same result as WaverideR::extract_signal", {
  d <- cd_even(as.data.frame(WaverideR::grey))
  wt <- WaverideR::analyse_wavelet(as.matrix(d), dj = 1 / 20, lowerPeriod = 0.05, upperPeriod = 5, verbose = FALSE)
  tr <- as.data.frame(WaverideR::grey_track)[, 1:2]
  a <- WaverideR::extract_signal(tracked_cycle_curve = tr, wavelet = wt, period_up = 1.3, period_down = 0.75,
      add_mean = TRUE, tracked_cycle_period = 110, extract_cycle = 405)
  b <- cd_extract(cd_extract_prep(wt), tr, 110, 405, up = 1.3, down = 0.75, add_mean = TRUE)
  expect_equal(b[, 2], a[, 2], tolerance = 1e-10)
  expect_equal(cd_peak_frac(0), 0)
  expect_equal(cd_peak_frac(50), 0.01)
})
