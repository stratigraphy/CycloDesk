# Tools of the Wavelets menu and the Math extras (group "More (WaverideR)") run with their defaults and plot

cd_wv_test_tool <- function(id, d = NULL, d2 = NULL, over = list(), plot = TRUE) {
  t <- CycloDesk:::cd_get_tool(id)
  p <- CycloDesk:::cd_defaults(t, d)
  for (nm in names(over)) {
    p[nm] <- list(over[[nm]])
  }
  r <- CycloDesk:::cd_run_tool(id, d = d, p = p, d2 = d2, file = "x.txt", file2 = "y.txt")
  expect_true(is.list(r), info = id)
  expect_true(length(r$code) > 0, info = id)
  code <- paste(r$code, collapse = "\n")
  expect_false(grepl(":::", code, fixed = TRUE), info = id)
  expect_no_error(parse(text = code), message = id)
  for (o in r$outputs) {
    expect_true(nrow(as.data.frame(o$data)) > 0, info = paste(id, o$name))
  }
  if (plot && !is.null(t$plot)) {
    grDevices::pdf(NULL, width = 11, height = 8)
    expect_no_error(t$plot(r$result, r$params), message = id)
    grDevices::dev.off()
  }
  invisible(r)
}

test_that("all Wavelets and Math extra tools are registered with unique ids", {
  tl <- CycloDesk:::cd_tools()
  ids <- character()
  for (t in tl) {
    ids <- c(ids, t$id)
  }
  expect_equal(anyDuplicated(ids), 0L)
  mine <- c("wv_transform", "wv_xsuperlet", "wv_chirp_eha", "wv_chirp_wavelet", "wv_dechirped", "wv_significance",
      "wv_sst_significance", "wv_band_significance", "wv_surrogates", "wv_extract", "wv_extract_track", "wv_dynamic_filter",
      "wv_transients", "wv_uncertainty", "wv_resolution", "wv_spectra", "mx_extremes", "mx_lag1", "mx_lithlog",
      "mx_bcp_r2", "mx_noise")
  expect_true(all(mine %in% ids))
  for (t in tl) {
    if (t$id %in% mine) {
      expect_true(nzchar(t$info), info = t$id)
      expect_true(nzchar(t$uses), info = t$id)
    }
  }
})

test_that("Wavelets tools run with default parameters", {
  skip_on_cran()
  oldop <- options(waverider.dev_new = FALSE)
  on.exit(options(oldop), add = TRUE)
  g <- WaverideR::grey
  gs <- g[g[, 1] < 162, ]
  trk <- WaverideR::grey_track[, 1:2]
  set.seed(1)
  g2 <- data.frame(g[, 1], g[, 2] + stats::rnorm(nrow(g), sd = stats::sd(g[, 2])))

  for (m in c("awavelet", "superlet_sst", "chirplet_sst", "conceft", "multitaper")) {
    over <- list(method = m, n_comb = 3)
    if (m == "chirplet_sst") {
      over$chirp_max <- 1
      over$chirp_step <- 0.5
    }
    cd_wv_test_tool("wv_transform", gs, over = over)
  }
  cd_wv_test_tool("wv_transform", gs, over = list(method = "chirplet_sst", chirp_max = 1, chirp_step = 0.5,
      show_chirp = TRUE))
  cd_wv_test_tool("wv_xsuperlet", gs, g2, over = list(Nf = 50))
  cd_wv_test_tool("wv_chirp_eha", g, over = list(n_chirp = 4))
  cd_wv_test_tool("wv_chirp_eha", g, over = list(n_chirp = 2, n_sim = 2, criterion = "template",
      astro = c(41, 23.7, 22.4, 19)))
  cd_wv_test_tool("wv_chirp_wavelet", g, over = list(n_chirp = 2, astro = c(41, 23.7, 22.4, 19)))
  cd_wv_test_tool("wv_dechirped", g, trk, over = list(cycle = 22))
  cd_wv_test_tool("wv_dechirped", g, trk, over = list(cycle = 22, transform = "eha", n_sim = 2, style = "outline"))

  cd_wv_test_tool("wv_significance", g)
  # the AR(1) fit of the surrogates (stats::arima) may warn about optim convergence on these short tests
  suppressWarnings(cd_wv_test_tool("wv_significance", g, over = list(test = "surrogate", n_sim = 5, candidates = c(0.52, 2))))
  cd_wv_test_tool("wv_significance", g, over = list(test = "rednoise", n_sim = 5))
  suppressWarnings(cd_wv_test_tool("wv_significance", gs, over = list(transform = "superlet", n_sim = 2, Nf = 40)))
  expect_error(cd_wv_test_tool("wv_significance", g, over = list(background = "powerlaw")), "analytic")
  cd_wv_test_tool("wv_sst_significance", g, over = list(n_sim = 5))
  cd_wv_test_tool("wv_band_significance", g)
  cd_wv_test_tool("wv_surrogates", g)
  cd_wv_test_tool("wv_surrogates", g, over = list(type = "ft", n_sim = 1))

  for (w in c("signal", "signal_v2", "power", "amplitude", "envelope")) {
    cd_wv_test_tool("wv_extract", g, over = list(what = w))
  }
  for (w in c("signal", "power", "amplitude")) {
    cd_wv_test_tool("wv_extract_track", g, trk, over = list(what = w, tracked = 22, extract = 22))
  }
  r <- cd_wv_test_tool("wv_dynamic_filter", g, trk)
  expect_true(max(abs(r$outputs[[1]]$data$y)) < 10 * stats::sd(g[, 2]))

  cd_wv_test_tool("wv_transients", g)
  for (m in c("wavelet", "practical", "gabor")) {
    r <- cd_wv_test_tool("wv_uncertainty", g, trk, over = list(method = m, cycle = 22))
    expect_true(all(is.finite(r$outputs[[2]]$data$y)), info = m)
  }
  cd_wv_test_tool("wv_resolution", g)
  for (m in c("bayes", "swa", "mesa")) {
    cd_wv_test_tool("wv_spectra", g, over = list(method = m))
  }
})

test_that("Math extra tools run with default parameters", {
  skip_on_cran()
  oldop <- options(waverider.dev_new = FALSE)
  on.exit(options(oldop), add = TRUE)
  g <- WaverideR::grey
  f <- astrochron::taner(g, flow = 1 / 0.6, fhigh = 1 / 0.35, genplot = FALSE, verbose = FALSE)
  for (w in c("max", "min", "both")) {
    cd_wv_test_tool("mx_extremes", f, over = list(which = w))
  }
  cd_wv_test_tool("mx_lag1", g[g[, 1] < 160, ], over = list(n_sim = 2))
  cd_wv_test_tool("mx_lithlog", as.data.frame(WaverideR::depth_rank_example))
  cd_wv_test_tool("mx_lithlog", as.data.frame(WaverideR::depth_rank_example), over = list(lin_interp = TRUE))
  r <- cd_wv_test_tool("mx_bcp_r2", g, over = list(k_max = 3, n_sim = 50, max_n = 300))
  expect_equal(nrow(r$outputs[[1]]$data), 3L)
  r <- cd_wv_test_tool("mx_noise")
  expect_equal(nrow(r$outputs[[1]]$data), 1000L)
})

test_that("Wavelets plot functions leave par() unchanged", {
  skip_on_cran()
  oldop <- options(waverider.dev_new = FALSE)
  on.exit(options(oldop), add = TRUE)
  g <- WaverideR::grey
  for (id in c("wv_band_significance", "wv_transients", "wv_spectra")) {
    t <- CycloDesk:::cd_get_tool(id)
    p <- CycloDesk:::cd_defaults(t, g)
    r <- CycloDesk:::cd_run_tool(id, d = g, p = p, file = "x.txt")
    grDevices::pdf(NULL)
    before <- graphics::par(no.readonly = TRUE)
    t$plot(r$result, r$params)
    after <- graphics::par(no.readonly = TRUE)
    grDevices::dev.off()
    expect_equal(after$mar, before$mar, info = id)
    expect_equal(after$mfrow, before$mfrow, info = id)
  }
})
