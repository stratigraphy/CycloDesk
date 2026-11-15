# Astronomy menu: every tool runs with its default parameters (small Monte Carlo runs) and draws its figure

cd_test_tuned_record <- function(k = 21, len = 3000) {
  set.seed(1)
  fr <- c(-18.85, -26.348, 4.257452, 7.452, 17.916)
  amp <- c(1, 0.3, 1, 0.87, 0.7)
  tt <- 0:len
  per <- 1296 / (1296 / k + fr)
  y <- 0.8 * cos(2 * pi * tt / 405) + 1.5 * as.numeric(stats::arima.sim(list(ar = 0.7), length(tt)))
  for (i in 1:5) {
    y <- y + amp[i] * cos(2 * pi * tt / per[i] + stats::runif(1, 0, 2 * pi))
  }
  data.frame(time = tt, value = y)
}

cd_test_run_astro <- function(id, over = list(), d = NULL, d2 = NULL) {
  t <- CycloDesk:::cd_get_tool(id)
  p <- CycloDesk:::cd_defaults(t, d)
  for (nm in names(over)) {
    p[nm] <- list(over[[nm]])
  }
  r <- CycloDesk:::cd_run_tool(id, d = d, p = p, d2 = d2, file = "x.txt", file2 = "y.txt")
  info <- paste(id, paste(names(over), collapse = ","))
  expect_true(is.list(r), info = info)
  expect_true(length(r$outputs) > 0, info = info)
  for (o in r$outputs) {
    expect_true(nrow(as.data.frame(o$data)) > 0, info = paste(info, o$name))
  }
  expect_no_error(parse(text = r$code), message = info)
  grDevices::pdf(NULL, width = 11, height = 8)
  op <- graphics::par(no.readonly = TRUE)
  expect_no_error(t$plot(r$result, r$params), message = info)
  op2 <- graphics::par(no.readonly = TRUE)
  keep <- setdiff(names(op), c("usr", "xaxp", "yaxp", "fig", "plt", "mfg", "mfrow", "mfcol"))
  expect_identical(op[keep], op2[keep], info = info)
  grDevices::dev.off()
  invisible(r)
}

test_that("all astronomy tools run with default parameters", {
  skip_on_cran()
  oldop <- options(waverider.dev_new = FALSE)
  on.exit(options(oldop), add = TRUE)
  tuned <- cd_test_tuned_record()
  set.seed(2)
  s1 <- data.frame(sample = 1:400, k_yr = stats::rnorm(400, 20700, 300))
  s2 <- data.frame(sample = 1:400, k_yr = stats::rnorm(400, 21000, 400))
  small <- list(as_ctest = list(nsim = 200), as_ctest_tuned = list(nsim = 200), as_k = list(nsim = 1000),
      as_test_tuned = list(n_surr = 5), as_test_check = list(k_true = 21, len = 3000))
  ids <- character()
  for (t in CycloDesk:::cd_tools()) {
    if (t$menu != "Astronomy" || !startsWith(t$id, "as_")) {
      next
    }
    ids <- c(ids, t$id)
    expect_true(nzchar(t$info) && !is.null(t$group) && !is.null(t$icon), info = t$id)
    d <- if (t$inputs >= 1) { tuned } else { NULL }
    d2 <- NULL
    if (t$id == "as_k_combine") {
      d <- s1
      d2 <- s2
    }
    over <- if (is.null(small[[t$id]])) { list() } else { small[[t$id]] }
    cd_test_run_astro(t$id, over, d = d, d2 = d2)
  }
  expect_equal(length(ids), 15)
  expect_false(anyDuplicated(ids) > 0)
})

test_that("astronomy tools: every method runs", {
  skip_on_cran()
  oldop <- options(waverider.dev_new = FALSE)
  on.exit(options(oldop), add = TRUE)
  cases <- list(
    list("as_periods", list(set = "all")),
    list("as_periods", list(set = "tracking", ksrc = "own", k = 21)),
    list("as_periods", list(set = "orbital")),
    list("as_periods", list(set = "nine", ksrc = "own")),
    list("as_periods_time", list(src = "la04")),
    list("as_periods_time", list(src = "acycle", step = 100)),
    list("as_chaos", list(method = "sensitivity")),
    list("as_chaos", list(method = "resonance")),
    list("as_ctest", list(method = "ctest_loop", nsim = 100)),
    list("as_ctest", list(method = "mc_depth", nsim = 100)),
    list("as_ctest", list(method = "ctest", obl = numeric(), nsim = 100)),
    list("as_ctest_tuned", list(method = "zeeden", nsim = 100)),
    list("as_ctest_tuned", list(method = "invert", nsim = 200)),
    list("as_ctest_tuned", list(method = "mc", nsim = 100, extra = "g4-g3 = 2000, 2800")),
    list("as_k", list(method = "ratio", nsim = 1000)),
    list("as_k", list(method = "bundling", nsim = 1000)),
    list("as_k", list(method = "cycles", nsim = 0)),
    list("as_k", list(method = "hypotheses")),
    list("as_k", list(method = "quick")),
    list("as_k", list(method = "scan")),
    list("as_compare", list(method = "zeeden")),
    list("as_earthmoon", list(known = "p", method = "astrogeo22")),
    list("as_earthmoon", list(known = "a_km")),
    list("as_earthmoon", list(known = "lod", oblsrc = "today")),
    list("as_earthmoon", list(known = "months", months_kind = "synodic")),
    list("as_earthmoon", list(known = "cycle", method = "astrogeo22")),
    list("as_earthmoon", list(known = "age")),
    list("as_earthmoon", list(known = "a_RE", oblsrc = "own", obl = 22)),
    list("as_em_relations", list(xvar = "lod")),
    list("as_em_relations", list(xvar = "months")),
    list("as_em_relations", list(xvar = "k")),
    list("as_gts", list(mode = "range", tmin = 0, tmax = 66)),
    list("as_gts", list(mode = "age", age = 435)),
    list("as_gts", list(mode = "unit", name = "Rhuddanian"))
  )
  for (cs in cases) {
    cd_test_run_astro(cs[[1]], cs[[2]])
  }
})

test_that("astronomy tools give sensible numbers", {
  skip_on_cran()
  r <- CycloDesk:::cd_run_tool("as_periods", p = CycloDesk:::cd_defaults(CycloDesk:::cd_get_tool("as_periods")))
  expect_equal(r$result$table$period_kyr[1], 405.69, tolerance = 1e-3)
  # the depth-domain Monte Carlo returns the sedimentation rate in m/kyr (about 2 cm/kyr for the default ranges)
  t <- CycloDesk:::cd_get_tool("as_ctest")
  p <- CycloDesk:::cd_defaults(t)
  p$method <- "mc_depth"
  p$nsim <- 100
  r <- CycloDesk:::cd_run_tool("as_ctest", p = p)
  expect_equal(r$result$summary$mean[1], 0.02, tolerance = 0.1)
  expect_equal(mean(r$result$post$k_kyr), 20.7, tolerance = 0.05)
  # Earth-Moon calculator: present-day length of day gives the present-day distance
  t <- CycloDesk:::cd_get_tool("as_earthmoon")
  p <- CycloDesk:::cd_defaults(t)
  p$known <- "lod"
  p$v_lod <- 24
  r <- CycloDesk:::cd_run_tool("as_earthmoon", p = p)
  expect_equal(r$result$state$a_RE, 60.14, tolerance = 1e-3)
})
