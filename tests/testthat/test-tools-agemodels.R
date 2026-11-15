# Tools of the Age models menu: every tool runs with its defaults (small iteration counts for the Monte Carlo and
# Bayesian tools), writes non-empty outputs, gives R code that parses and draws its figure

am_run <- function(id, d = NULL, d2 = NULL, over = list()) {
  t <- CycloDesk:::cd_get_tool(id)
  p <- CycloDesk:::cd_defaults(t, d)
  for (n in names(over)) {
    p[n] <- list(over[[n]])
  }
  r <- CycloDesk:::cd_run_tool(id, d = d, p = p, d2 = d2, file = "x.txt", file2 = "y.txt")
  expect_true(is.list(r), info = id)
  expect_true(length(r$outputs) > 0, info = id)
  for (o in r$outputs) {
    expect_true(nrow(as.data.frame(o$data)) > 0, info = paste(id, o$name))
  }
  expect_no_error(parse(text = r$code), message = id)
  grDevices::pdf(NULL, width = 11, height = 8)
  expect_no_error(t$plot(r$result, r$params), message = id)
  grDevices::dev.off()
  r
}

test_that("age-model tools are registered with unique ids in the Age models menu", {
  tl <- CycloDesk:::cd_tools_agemodels()
  ids <- character()
  for (t in tl) {
    ids <- c(ids, t$id)
    expect_identical(t$menu, "Age models")
    expect_true(startsWith(t$id, "am_"))
    expect_true(nzchar(t$info))
  }
  expect_false(anyDuplicated(ids) > 0)
  all_ids <- character()
  for (t in CycloDesk:::cd_tools()) {
    all_ids <- c(all_ids, t$id)
  }
  expect_false(anyDuplicated(all_ids) > 0)
})

test_that("helpers: curve, dates table and the minimal-tuning workaround", {
  tr <- WaverideR::grey_track
  cv <- CycloDesk:::cd_am_curve(tr[, 1:2], "period", 3)
  expect_equal(ncol(cv), 3)
  expect_equal(nrow(cv), 501)
  expect_true(all(cv[, 3] > 0))
  d <- data.frame(depth = c(1, 2, 3), age = c(10, 20, 30), sd2 = c(2, 2, 4))
  dt <- CycloDesk:::cd_am_dates(d)
  expect_equal(dt$sd, c(1, 1, 2))
  expect_equal(dt$thickness, c(0, 0, 0))
  d0 <- data.frame(x = c(1, 2, 3), y = c(10, 20, 30), y2 = c(1, 1, 1))
  expect_equal(CycloDesk:::cd_am_dates(d0)$age, c(10, 20, 30))
  s <- data.frame(x = seq(154.37, 160, by = 0.0137), y = 0)
  s$y <- cos(2 * pi * s$x / 0.5)
  expect_no_error(WaverideR::minimal_tuning(data = CycloDesk:::cd_am_nudge(s), pts = 5, cycle = 21))
})

test_that("age-model tools run and draw their figures", {
  skip_on_cran()
  oldop <- options(waverider.dev_new = FALSE)
  on.exit(options(oldop), add = TRUE)
  g <- WaverideR::grey
  tr <- WaverideR::grey_track
  am <- WaverideR::age_model_zeeden
  f <- astrochron::taner(g, flow = 1 / 0.6, fhigh = 1 / 0.35, genplot = FALSE, verbose = FALSE)

  # minimal tuning, all tie-point options; the minima must give another age model than the maxima
  r_max <- am_run("am_mintune", f, over = list(cycle = 21))
  r_min <- am_run("am_mintune", f, over = list(cycle = 21, tune_opt = "min"))
  am_run("am_mintune", f, over = list(cycle = 21, tune_opt = "minmax"))
  expect_false(isTRUE(all.equal(r_max$outputs[[1]]$data[, 2], r_min$outputs[[1]]$data[, 2])))

  # depth to time with a tracked curve, a sedimentation-rate curve and tie points
  am_run("am_convert", g, tr[, 1:2], over = list(cycle = 21))
  r <- am_run("am_convert", g, tr[, c(1, 3)], over = list(method = "sedrate", interp = TRUE))
  expect_equal(nrow(r$outputs[[2]]$data), nrow(g))
  am_run("am_convert", g, am, over = list(method = "anchor"))

  # sedimentation-rate ramp of a time series
  ts <- data.frame(time = 0:1500, y = cos(2 * pi * (0:1500) / 100) + 0.5 * cos(2 * pi * (0:1500) / 21))
  am_run("am_sedramp", ts)

  # Monte Carlo re-tracking on two proxies, then age models with uncertainty from it and from a period curve
  px <- data.frame(depth = g[, 1], grey = g[, 2], noisy = g[, 2] + stats::rnorm(nrow(g), 0, 15))
  rt <- am_run("am_retrack", px, tr[, 1:2], over = list(n_sim = 4, smoothing = "none"))
  retrack <- rt$outputs[[1]]$data
  expect_equal(ncol(retrack), 3)
  am_run("am_unc", tr[, 1:2], over = list(cycle = 21, n_sim = 30))
  am_run("am_unc", retrack, over = list(type = "retrack", cycle = 21, cycle_unc = 0.5, n_sim = 20))
  r <- am_run("am_unc", tr[, 1:2], over = list(cycle = 21, n_sim = 5, anchor = "anchor", anc_depth = c(158, 168),
      anc_age = c(5160, 5630), anc_sd = 5, gap_depth = 165, gap_dur = 10, gap_sd = 3))
  s <- r$outputs[[1]]$data
  expect_true(all(s$q97.5 >= s$q2.5))

  # gap duration
  am_run("am_gaps", tr[, 1:2], g, over = list(cycle = 21, missing = 21, n_sim = 2))

  # age-model check (Morlet wavelet only, to keep the test short)
  am_run("am_check", g, tr[, 1:2], over = list(cycle = 21, age_ma = 5, tf_eha = FALSE))

  # Bayesian age-depth models: dates alone, dates with the astrochronology (also with a hiatus and the modBchron
  # comparison, and from single-zircon dates)
  z <- c(155.5, 160, 165, 170, 173.5)
  set.seed(3)
  dates <- data.frame(depth = z, age = stats::approx(am[, 1], am[, 2], z)$y + stats::rnorm(5, 0, 5), sd = 5)
  r <- am_run("am_bchron", dates, over = list(MC = 400, burn = 100, n_pred = 40, predict = c(158, 163)))
  expect_equal(nrow(r$outputs[[3]]$data), 2)
  am_run("am_astrobchron", dates, tr[, 1:2], over = list(age_unit = "ka", target_period = 21, n_iter = 2000, thin = 10,
      n_tracks = 40, predict = 160))
  r <- am_run("am_astrobchron", dates, tr[, 1:2], over = list(age_unit = "ka", target_period = 21, n_iter = 2000, thin = 10,
      n_tracks = 40, hiatus = 166, hiatus_mean = 20, hiatus_sd = 10, bchron = TRUE, bchron_mc = 300))
  expect_true(NCOL(r$result$hiatus) == 1)
  set.seed(4)
  gr <- data.frame(depth = rep(c(156, 162, 171), each = 4), age = rep(stats::approx(am[, 1], am[, 2], c(156, 162, 171))$y,
      each = 4) + stats::rnorm(12, 0, 4), sd = 4)
  r <- am_run("am_astrobchron", gr, tr[, 1:2], over = list(date_type = "grains", D_max = 20, age_unit = "ka",
      target_period = 21, n_iter = 2000, thin = 10, n_tracks = 40))
  expect_equal(nrow(r$outputs[[1]]$data), 3)
})
