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

test_that("evolutive, eTimeOpt and eCOCO maps can be tracked", {
  skip_on_cran()
  g <- WaverideR::grey
  g <- g[g[, 1] < 166, ]
  run <- function(id, mod = list(), d = g) {
    p <- cd_defaults(cd_get_tool(id), d)
    for (k in names(mod)) {
      p[[k]] <- mod[[k]]
    }
    cd_run_tool(id, d = d, p = p, file = "g.txt")
  }
  maps <- list(eha = run("evolutive", list(method = "eha")), log2 = run("evolutive", list(method = "eha_log2")),
      mesa = run("evolutive", list(method = "mesa")), fft = run("evolutive", list(method = "winfft")),
      eto = run("etimeopt", list(win = 6, step = 1, numsed = 12, sedmin = 1, sedmax = 3.5), d = WaverideR::grey),
      eco = run("ecoco", list(window = 4, srstep = 0.25, srmin = 0.5, srmax = 5)))
  kinds <- c(eha = "map", log2 = "map", mesa = "map", fft = "map", eto = "sedrate", eco = "sedrate")
  for (nm in names(maps)) {
    r <- maps[[nm]]
    expect_identical(cd_track_kind(r), kinds[[nm]], info = nm)
    m <- cd_track_map(r)
    expect_true(all(dim(m$Power) > 1) && all(diff(m$Period) > 0) && all(m$Power >= 0), info = nm)
    guide <- if (kinds[[nm]] == "map") { data.frame(c(155, 165), c(0.5, 0.5)) } else { data.frame(c(156, 164), c(2, 2)) }
    tr <- cd_snap_track(m, guide, tol = 0.4)
    expect_equal(nrow(tr), length(m$axis.1), info = nm)
    expect_true(all(is.finite(tr$period)), info = nm)
    h <- WaverideR::track_ridge_hmm(m, guide = guide, guide_tolerance = 0.4, fallback_weight = 0)
    expect_true(all(is.finite(h$period)), info = nm)
  }
  # a sedimentation-rate track gives the age model directly (rate in cm/kyr, depth in m)
  m <- cd_track_map(maps$eco)
  tr <- cd_snap_track(m, data.frame(c(156, 164), c(2, 2)), tol = 0.4)
  per <- data.frame(depth = tr$depth, period = tr$period / 100 * 21)
  am <- as.data.frame(WaverideR::curve2time(tracked_cycle_curve = per, tracked_cycle_period = 21))
  expect_equal(diff(range(am[, 2])), sum(diff(tr$depth) / (tr$period[-1] / 100)), tolerance = 0.05)
  # the rho1/wavelet results are not maps for the Track view
  expect_null(cd_track_kind(list(tool = "sampling", result = list())))
})

test_that("helper lines: cycles, fixed periods and file columns with a multiplier", {
  x <- seq(100, 200, by = 1)
  trk <- data.frame(depth = x, period = rep(20, length(x)))
  s1 <- list(type = "cyc_track", values = c(405, 100), colour = "white", lty = 2L, label = "")
  h1 <- cd_helper_lines(s1, x, track = trk, track_cycle = 405)
  expect_length(h1, 2)
  expect_equal(h1[[1]]$period, rep(20, length(x)))
  expect_equal(h1[[2]]$period, rep(20 * 100 / 405, length(x)))
  am <- data.frame(depth = c(100, 150, 200), age = c(0, 500, 1500))
  s2 <- list(type = "cyc_file", values = 405, colour = "red", lty = 1L, label = "C")
  h2 <- cd_helper_lines(s2, x, data = am)
  expect_equal(h2[[1]]$period[1], 405 * 50 / 500)
  expect_equal(h2[[1]]$period[length(x)], 405 * 50 / 1000)
  expect_equal(h2[[1]]$label, "C 405 kyr")
  expect_equal(h2[[1]]$colour, "red")
  s3 <- list(type = "fixed", values = c(25, 32.5), colour = "cyan", lty = 3L, label = "")
  h3 <- cd_helper_lines(s3, x)
  expect_length(h3, 2)
  expect_equal(h3[[2]]$period, c(32.5, 32.5))
  expect_equal(h3[[2]]$depth, c(100, 200))
  d <- data.frame(depth = c(50, 120, 160, 250), rate = c(5, 6, 8, 9))
  s4 <- list(type = "column", xcol = 1L, ycol = 2L, mult = 4.05, colour = "white", lty = 2L, label = "")
  h4 <- cd_helper_lines(s4, x, data = d)
  expect_equal(h4[[1]]$depth, c(120, 160))
  expect_equal(h4[[1]]$period, c(6, 8) * 4.05)
  expect_equal(h4[[1]]$label, "rate x 4.05")
  expect_match(cd_helper_describe(c(s4, list(file = "f.csv", colname = "rate"))), "rate \\(f.csv\\) x 4.05")
  expect_equal(cd_fmt_depths(c(1453.25, 2160)), "1453.25, 2160")
})

test_that("helper status reports lines outside the period range", {
  h <- list(list(depth = c(1, 2), period = c(5, 6), label = "a"), list(depth = c(1, 2), period = c(500, 900), label = "b"))
  s <- cd_helper_status(h, c(1, 100), 2L)
  expect_match(s, "2 line")
  expect_match(s, "'b' \\(500 to 900\\) lies outside")
  expect_false(grepl("'a'", s))
})

test_that("on a sedimentation-rate map only fixed and column helper lines are drawn", {
  h <- list(list(type = "cyc_track"), list(type = "fixed"), list(type = "column"), list(type = "cyc_file"))
  expect_length(cd_helper_for_kind(h, "cwt"), 4)
  k <- cd_helper_for_kind(h, "sedrate")
  expect_length(k, 2)
  expect_equal(k[[1]]$type, "fixed")
})
