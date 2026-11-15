# astrochron tools of the Math ("More (astrochron)"), Time Series ("Pre-processing (astrochron)") and Basic Series
# ("Models and insolation (astrochron)") menus: every tool and method runs, writes non-empty outputs, gives R code that
# parses and draws its figure

ab_run <- function(id, d = NULL, d2 = NULL, over = list(), plot = TRUE) {
  t <- CycloDesk:::cd_get_tool(id)
  p <- CycloDesk:::cd_defaults(t, d)
  for (nm in names(over)) {
    p[nm] <- list(over[[nm]])
  }
  r <- CycloDesk:::cd_run_tool(id, d = d, p = p, d2 = d2, file = "x.txt", file2 = "y.txt")
  lab <- paste(id, paste(names(over), over, sep = "=", collapse = " "))
  expect_true(is.list(r), info = lab)
  expect_true(length(r$outputs) > 0, info = lab)
  for (o in r$outputs) {
    expect_true(nrow(as.data.frame(o$data)) > 0, info = paste(lab, o$name))
  }
  code <- paste(r$code, collapse = "\n")
  expect_true(nzchar(code), info = lab)
  expect_false(grepl(":::", code, fixed = TRUE), info = lab)
  expect_no_error(parse(text = code), message = lab)
  if (plot) {
    grDevices::pdf(NULL, width = 11, height = 8)
    expect_no_error(t$plot(r$result, r$params), message = lab)
    grDevices::dev.off()
  }
  invisible(r)
}

test_that("astrochron_basic tools are registered with unique ids, info and uses", {
  tl <- CycloDesk:::cd_tools_astrochron_basic()
  expect_true(length(tl) >= 20)
  for (t in tl) {
    expect_true(startsWith(t$id, "ab_"), info = t$id)
    expect_true(t$menu %in% c("Math", "Time Series", "Basic Series"), info = t$id)
    expect_true(nzchar(t$info), info = t$id)
    expect_true(nzchar(t$uses), info = t$id)
    expect_true(nzchar(t$group), info = t$id)
  }
  ids <- character()
  for (t in CycloDesk:::cd_tools()) {
    ids <- c(ids, t$id)
  }
  expect_equal(anyDuplicated(ids), 0L)
})

test_that("helpers: quiet evaluation keeps the device, tables are sorted and averaged", {
  grDevices::pdf(NULL)
  dv <- grDevices::dev.cur()
  x <- CycloDesk:::cd_ab_quiet({
    graphics::plot(1:3)
    cat("noise\n")
    5
  }, device = TRUE)
  expect_equal(x, 5)
  expect_equal(grDevices::dev.cur(), dv)
  grDevices::dev.off()
  d <- data.frame(x = c(3, 1, 2, 2), a = c(1, 2, 3, 5), b = c(1, 1, 1, 3))
  s <- CycloDesk:::cd_ab_table(d, 1:3)
  expect_equal(s$x, c(1, 2, 3))
  expect_equal(s$a, c(2, 4, 1))
  expect_true(CycloDesk:::cd_ab_exact(0:10))
})

test_that("Math tools (astrochron) run with default parameters", {
  skip_on_cran()
  g <- WaverideR::grey
  set.seed(2)
  gu <- g[sort(sample(nrow(g), 500)), ]
  xrf <- WaverideR::Bisciaro_XRF
  tab <- xrf[, c(1, 2, 21, 40)]
  tab[c(5, 9), 3] <- NA
  tab[c(3, 4), 2] <- -tab[c(3, 4), 2]

  for (m in c("rmNA", "repl0", "replEps", "trimAT", "iso")) {
    r <- ab_run("ab_clean", tab, over = list(method = m))
  }
  r <- ab_run("ab_clean", tab, over = list(method = "rmNA"))
  expect_equal(nrow(r$outputs[[1]]$data), nrow(tab) - 2)
  r <- ab_run("ab_clean", tab, over = list(method = "repl0"))
  expect_true(all(r$outputs[[1]]$data[, 2] >= 0))

  for (m in c("divTrend", "arcsinT", "flip", "clipIt", "clipdiv", "pad", "constantSedrate")) {
    ab_run("ab_transform", g, over = list(method = m))
  }
  r <- ab_run("ab_transform", g, over = list(method = "pad"))
  expect_equal(nrow(r$outputs[[1]]$data), 2 * nrow(g))

  for (m in c("cos", "dpss", "gaus", "hann")) {
    ab_run("ab_taper", g, over = list(taper = m, write_win = TRUE))
  }

  r <- ab_run("ab_resample", g, gu)
  expect_equal(length(r$outputs), 2)
  expect_equal(ncol(r$outputs[[2]]$data), 3)

  r <- ab_run("ab_lh13", gu)
  expect_true(nrow(r$outputs[[1]]$data) > 10)
  ab_run("ab_lh13", gu, over = list(dtmode = "given", dt = 0.05))
  expect_error(ab_run("ab_lh13", g), "evenly spaced")

  for (m in c("peak", "trough", "both")) {
    ab_run("ab_peaks", g, over = list(which = m))
  }
  ab_run("ab_peaks", g, over = list(which = "both", use_level = TRUE))

  for (m in c("mwStats", "grid", "minmax")) {
    ab_run("ab_mwstats", g, over = list(method = m, windows = TRUE))
  }
  ab_run("ab_mwstats", g, over = list(method = "mwStats", conv = "2", CI = 0))
  ab_run("ab_mwstats", gu, over = list(method = "minmax", conv = "3"))

  r <- ab_run("ab_mwcor", tab)
  expect_true(all(abs(r$outputs[[1]]$data[, 2]) <= 1, na.rm = TRUE))
  ab_run("ab_mwcor", tab, over = list(cormethod = "2", conv = "2"))

  ab_run("ab_strats", gu)
  ab_run("ab_strats", g, over = list(plot_dt = "2"))

  ab_run("ab_xplot", tab)

  r <- ab_run("ab_pca", xrf)
  expect_equal(ncol(r$outputs[[2]]$data), ncol(r$outputs[[1]]$data))
  ab_run("ab_pca", tab, over = list(rot = "1", nPC = 2))
  ab_run("ab_pca", tab, over = list(rot = "2", nPC = 2, standardise = FALSE))

  wt <- data.frame(depth = g[, 1], caco3 = 50 + (g[, 2] - mean(g[, 2])) / 10)
  r <- ab_run("ab_accum", wt)
  expect_equal(ncol(r$outputs[[2]]$data), 5)

  set.seed(3)
  beds <- data.frame(thickness = stats::runif(30, 0.05, 0.5), rank = sample(1:4, 30, replace = TRUE))
  r <- ab_run("ab_rank", beds)
  expect_true(abs(max(r$outputs[[1]]$data[, 1]) - sum(beds$thickness)) <= r$params$dt)
})

test_that("Time Series pre-processing (astrochron) runs with default parameters", {
  skip_on_cran()
  g <- WaverideR::grey
  for (m in c("lowpass", "highpass", "kernel")) {
    ab_run("ab_lowpass", g, over = list(method = m, write_trend = TRUE))
  }
  ab_run("ab_lowpass", g, over = list(method = "lowpass", win = "1"))
  ab_run("ab_lowpass", g, over = list(method = "highpass", win = "2", detrend = TRUE))
  expect_error(ab_run("ab_lowpass", g, over = list(fcut = 1000)), "Nyquist")
})

test_that("Basic Series models and insolation (astrochron) run with default parameters", {
  skip_on_cran()
  for (m in c("cycles", "arma", "ar1etp")) {
    ab_run("ab_synth", NULL, over = list(method = m))
  }
  r <- ab_run("ab_synth", NULL, over = list(method = "ar1etp", start = 100, end = 600, dt = 2))
  expect_equal(r$outputs[[1]]$data[1, 1], 100)
  ab_run("ab_synth", NULL, over = list(method = "arma", rhos = c(0.5, 0.2), thetas = 0.3))

  r <- ab_run("ab_insoseries", NULL, over = list(tmax = 200))
  expect_equal(nrow(r$outputs[[1]]$data), 201)
  ab_run("ab_insoseries", NULL, over = list(opt = "2", tmax = 20))
  ab_run("ab_insoseries", NULL, over = list(opt = "3", tmax = 50))
  ab_run("ab_insoseries", NULL, over = list(opt = "4", tmax = 20, lat = -65))

  r <- ab_run("ab_insomap", NULL)
  expect_equal(ncol(r$outputs[[1]]$data), 74)
  ab_run("ab_insomap", NULL, over = list(method = "diff", long_pick = "180"))

  r <- ab_run("ab_imbrie", NULL, over = list(tmax = 600))
  expect_true(all(r$outputs[[1]]$data[, 2] >= 0 & r$outputs[[1]]$data[, 2] <= 1))
  ab_run("ab_imbrie", NULL, over = list(tmax = 600, Tm = c(15, 5), b = c(0.6, 0.3), times = c(300, 600), standardize = FALSE))
  expect_error(ab_run("ab_imbrie", NULL, over = list(Tm = c(15, 5))), "start times")

  ts <- astrochron::cycles(start = 0, end = 600, dt = 1, genplot = FALSE, verbose = FALSE)
  ts01 <- data.frame(time = seq(0, 300, by = 0.1), y = sin(2 * pi * seq(0, 300, by = 0.1) / 21))
  r <- ab_run("ab_sedmodel", ts)
  expect_equal(r$outputs[[1]]$data[nrow(ts), 1], sum((1:nrow(ts) - 1) / (nrow(ts) - 1) * 0.04 + 0.01) - 0.01,
      tolerance = 1e-6)
  r2 <- ab_run("ab_sedmodel", ts01)
  expect_equal(nrow(r2$outputs[[1]]$data), nrow(ts01))
  ab_run("ab_sedmodel", ts, over = list(method = "diff"))
  ab_run("ab_sedmodel", ts, over = list(method = "diff", dir = "-1"))

  r <- ab_run("ab_synthstrat", ts)
  expect_true(all(r$outputs[[1]]$data[, 2] %in% 1:4))
  ab_run("ab_synthstrat", ts, over = list(clip = FALSE, flip = TRUE, nfacies = 6))

  r <- ab_run("ab_bioturb", ts)
  mixed <- r$outputs[[1]]$data
  expect_true(stats::sd(mixed[, 2]) < stats::sd(ts[, 2]))
  ab_run("ab_bioturb", mixed, over = list(method = "unbioturb"))
  ab_run("ab_bioturb", mixed, over = list(method = "unbioturb", wiener = FALSE, fhigh = 0.1))
})
