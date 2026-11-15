# Tools from astrochron (spectral testing, astronomical testing, anchoring, geochronology, astronomical periods):
# every tool runs (small settings for the slow ones), writes non-empty outputs, gives valid R code and draws its figure

# check_par: the figure leaves par() as it was (cd_plot_series keeps the layout of its last panel on purpose)
at_test_run <- function(id, d = NULL, p_over = list(), d2 = NULL, shows = NULL, show_param = "show", check_par = TRUE) {
  t <- CycloDesk:::cd_get_tool(id)
  p <- CycloDesk:::cd_defaults(t, d)
  for (nm in names(p_over)) {
    p[nm] <- list(p_over[[nm]])
  }
  r <- CycloDesk:::cd_run_tool(id, d = d, p = p, d2 = d2, file = "x.txt", file2 = "y.txt")
  expect_true(is.list(r), info = id)
  expect_true(length(r$code) > 0, info = id)
  expect_no_error(parse(text = r$code), message = id)
  expect_true(length(r$outputs) > 0, info = id)
  for (o in r$outputs) {
    expect_true(nrow(as.data.frame(o$data)) > 0, info = paste(id, o$name))
  }
  if (is.null(shows)) {
    shows <- if (is.null(p[[show_param]])) { "x" } else { p[[show_param]] }
  }
  for (sh in shows) {
    p[show_param] <- list(sh)
    grDevices::pdf(NULL, width = 11, height = 8)
    oldpar <- graphics::par(no.readonly = TRUE)
    expect_no_error(t$plot(r$result, p), message = paste(id, sh))
    newpar <- graphics::par(no.readonly = TRUE)
    if (check_par) {
      expect_identical(newpar$mfrow, oldpar$mfrow, info = paste(id, sh))
      expect_identical(newpar$mar, oldpar$mar, info = paste(id, sh))
    }
    grDevices::dev.off()
  }
  r
}

at_grey <- function(by = 1) {
  g <- as.data.frame(WaverideR::grey)
  g[seq(1, nrow(g), by = by), ]
}

test_that("astrochron testing tools are registered with unique ids in existing menus", {
  tl <- CycloDesk:::cd_tools_astrochron_testing()
  expect_true(length(tl) >= 20)
  for (t in tl) {
    expect_true(grepl("^at_", t$id))
    expect_true(t$menu %in% c("Time Series", "Sed. rate", "Age models", "Astronomy"))
    expect_true(grepl("astrochron", t$group))
    expect_true(nzchar(t$info))
    expect_true(grepl("astrochron::", t$uses))
  }
  all_ids <- character()
  for (t in CycloDesk:::cd_tools()) {
    all_ids <- c(all_ids, t$id)
  }
  expect_false(anyDuplicated(all_ids) > 0)
})

test_that("helpers parse astrochron output and p-values", {
  expect_equal(CycloDesk:::cd_at_grab(c("a", " * Data correlation coefficient = 0.93 "), "Data correlation coefficient ="),
      0.93)
  expect_true(is.na(CycloDesk:::cd_at_grab("nothing", "r=")))
  expect_equal(CycloDesk:::cd_at_line_nums(c("x", "u:\t2.1  ,\t2.2  , 0.1 , 1.9 - 2.3"), "u:"), c(2.1, 2.2, 0.1, 1.9, 2.3))
  expect_equal(CycloDesk:::cd_at_pval(0, 100)$p, 0.1)
  expect_equal(CycloDesk:::cd_at_pval(50, 100)$p, 0.5)
  b <- CycloDesk:::cd_at_bands("1.6 2.4; 5 4")
  expect_equal(b$flow, c(1.6, 4))
  expect_equal(b$fhigh, c(2.4, 5))
  expect_error(CycloDesk:::cd_at_bands("1 2 3"))
  q <- CycloDesk:::cd_at_quiet(astrochron::periodogram(at_grey(4), output = 1, genplot = TRUE, verbose = TRUE))
  expect_true(is.data.frame(q$value))
  expect_true(length(q$log) > 0)
  pg <- CycloDesk:::cd_at_pages(astrochron::bergerPeriods(age = 100, genplot = TRUE))
  expect_length(pg$pages, 1)
})

test_that("Time Series tools (spectral testing, surrogates, correlation) run", {
  skip_on_cran()
  g <- at_grey()
  at_test_run("at_spectest", g)
  at_test_run("at_spectest", g, list(method = "multiTest", bands = "1.6 2.4; 4 5"))
  at_test_run("at_spectest", g, list(method = "mtmAR", order = 2))
  r <- at_test_run("at_spectest", g, list(method = "pwrLawFit"))
  expect_true(any(grepl("beta", r$text)))
  r <- at_test_run("at_testbg", g, list(iter = 20))
  expect_equal(ncol(r$outputs[[1]]$data), 4)
  expect_true(all(diff(r$outputs[[1]]$data$frequency) > 0))
  at_test_run("at_testbg", g, list(iter = 10, noise = "pwrLaw", bmethod = "periodogramPL", multi = FALSE))
  at_test_run("at_bandpower", g)
  at_test_run("at_bandpower", g, list(method = "extract", quantity = "3"))
  at_test_run("at_bicoherence", g, shows = c("bic", "bis", "pwr"))
  r <- at_test_run("at_surrogates", g, list(nsim = 2, seed = 1), check_par = FALSE)
  expect_equal(nrow(r$outputs[[1]]$data), nrow(g))
  r <- at_test_run("at_surrogates", g, list(method = "makeNoise", model = "ar1", seed = 1), check_par = FALSE)
  expect_equal(ncol(r$outputs[[1]]$data), 2)
  set.seed(3)
  g2 <- g
  g2[, 2] <- g2[, 2] + stats::rnorm(nrow(g), 0, 20)
  g2 <- g2[seq(1, nrow(g2), 3), ]
  r <- at_test_run("at_correlate", g, list(nsim = 50), d2 = g2)
  expect_true(r$outputs[[1]]$data$r > 0.8)
  r <- at_test_run("at_correlate", g[1:900, ], list(method = "slideCor"), d2 = g2[100:200, ])
  expect_true(max(abs(r$outputs[[1]]$data$r)) > 0.8)
  at_test_run("at_impulse")
})

test_that("TimeOpt significance, eTimeOpt tracking, ASM and freq2sedrate run", {
  skip_on_cran()
  skip_if(parallel::detectCores() < 2, "timeOptSim needs two cores")
  gs <- at_grey(4)
  r <- at_test_run("at_timeopt_sim", gs, list(numsim = 11, numsed = 20))
  expect_equal(nrow(r$outputs[[2]]$data), 11)
  at_test_run("at_timeopt_sim", gs, list(numsim = 11, numsed = 15, noise = "pl", fit = "2", sedmin = 1, sedmax = 4))
  r <- at_test_run("at_etimeopt_track", gs, list(numsed = 15, win = 8, step = 2))
  expect_true(all(r$outputs[[2]]$data[, 2] > 0))
  g <- at_grey()
  at_test_run("at_asm", g, list(iter = 1000, numsed = 20))
  r <- at_test_run("at_asm", g, list(method = "easm", iter = 1000, numsed = 20, threshold = 50, win = 6, step = 1))
  expect_equal(ncol(r$outputs[[2]]$data), 2)
  r <- at_test_run("at_freq2sedrate", as.data.frame(WaverideR::grey_track)[, 1:2], list(cycle = 22))
  sr <- r$outputs[[1]]$data
  expect_true(all(sr[, 2] > 1.5 & sr[, 2] < 3))
})

test_that("TimeOpt with a sed.-rate template runs", {
  skip_on_cran()
  gs <- at_grey(8)
  r <- at_test_run("at_timeopt_template", gs, list(template = "linear", numsed = 3, sedmin = 2, sedmax = 2.5, ncores = 1))
  expect_equal(ncol(r$outputs[[3]]$data), 2)
  at_test_run("at_timeopt_template", gs, list(template = "step", depths = 165, numsed = 3, sedmin = 2, sedmax = 2.5,
      ncores = 1, sim = TRUE, numsim = 2))
})

test_that("Bayesian TimeOpt tools run", {
  skip_on_cran()
  gs <- at_grey(8)
  r <- at_test_run("at_timeoptb", gs, list(age = 400, sedmin = 1.5, sedmax = 3, numsed = 10, numk = 10),
      shows = c("1", "2", "3", "4", "5"))
  expect_equal(nrow(r$outputs[[1]]$data), 2)
  expect_true(all(is.finite(r$outputs[[1]]$data$MAP)))
  gt <- gs[1:120, ]
  r <- at_test_run("at_timeopt_mcmc", gt, list(age = 400, sedmin = 1.5, sedmax = 3, nsamples = 100, seed = 1),
      shows = c(1, 2, 6), show_param = "page")
  expect_true(ncol(r$outputs[[2]]$data) > 5)
  at_test_run("at_timeopt_mcmc", gt, list(method = "mcmc", age = 400, sedmin = 1.5, sedmax = 3, nsamples = 100, seed = 1),
      shows = c(1, 4), show_param = "page")
})

test_that("amplitude-modulation tests of a tuned record run", {
  skip_on_cran()
  la <- CycloDesk::cd_laskar("la04")
  ex <- astrochron::etp(tmin = 0, tmax = 2000, dt = 2, solution = la, genplot = FALSE, verbose = FALSE)
  set.seed(1)
  ex[, 2] <- ex[, 2] + stats::rnorm(nrow(ex), 0, 0.5 * stats::sd(ex[, 2]))
  r <- at_test_run("at_am_test", ex, list(nsim = 20))
  expect_true(any(grepl("r = 0.[6-9]", r$text)))
  at_test_run("at_am_test", ex, list(nsim = 20, method = "tilt"))
})

test_that("Age-model and Astronomy tools run", {
  skip_on_cran()
  ex <- data.frame(t = 0:200 * 2, y = sin(0:200 / 5))
  r <- at_test_run("at_anchor", ex, list(age = 66000))
  expect_equal(max(r$outputs[[1]]$data[, 1]), 66000)
  age <- c(93.66, 94.75, 94.6, 94.22, 86.87, 94.64, 94.34, 94.03, 93.56, 93.85, 88.55, 93.45)
  sd2 <- c(5.83, 4.10, 8.78, 2.5, 8.86, 3.37, 4.63, 3.18, 8.35, 5.73, 4.23, 2.56)
  r <- at_test_run("at_wtmean", data.frame(age = age, sd = sd2 / 2), list(del = c(5, 11)))
  expect_true(abs(r$outputs[[1]]$data[1, 1] - 94) < 1)
  st <- data.frame(perAr39 = c(4.96, 27.58, 19.68, 39.9, 6.25, 1.02, 0.42, 0.19),
      age = c(90.08, 89.77, 89.92, 89.95, 89.89, 89.55, 87.71, 86.13), sd = c(0.18, 0.11, 0.08, 0.06, 0.14, 0.64, 1.5, 3.22),
      KCa = c(113, 138, 101, 195, 307, 27, 17, 24), perAr40 = c(93.42, 99.42, 99.64, 99.79, 99.61, 97.99, 94.64, 90.35),
      Fval = c(2.148234, 2.140643, 2.144197, 2.145006, 2.143627, 2.135163, 2.090196, 2.051682),
      Fsd = c(0.00439, 0.00270, 0.00192, 0.00149, 0.00331, 0.01557, 0.03664, 0.07846))
  at_test_run("at_wtmean", st, list(method = "stepHeat", del = c(7, 8)))
  expect_error(CycloDesk:::cd_run_tool("at_wtmean", d = st[, 1:3], p = list(method = "stepHeat")))
  r <- at_test_run("at_periods")
  expect_equal(r$outputs[[1]]$data$period_kyr, c(38.7, 31.6, 19.7, 16.7))
  r <- at_test_run("at_periods", p_over = list(method = "calc"))
  expect_true(any(abs(r$outputs[[1]]$data$period_kyr - 405) < 1))
  r <- at_test_run("at_priors")
  expect_equal(nrow(r$outputs[[1]]$data), 10)
  r <- at_test_run("at_alod")
  expect_true(r$outputs[[1]]$data$a_earth_radii > 50)
  r <- at_test_run("at_tones", p_over = list(target = 100))
  expect_equal(nrow(r$outputs[[1]]$data), 30)
})
