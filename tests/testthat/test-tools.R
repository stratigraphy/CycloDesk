# Every tool runs with its default parameters and draws its figure
test_that("all tools run with default parameters", {
  skip_on_cran()
  oldop <- options(waverider.dev_new = FALSE)
  on.exit(options(oldop), add = TRUE)
  g <- WaverideR::grey
  g <- g[g[, 1] < 165, ]
  f <- astrochron::taner(g, flow = 1 / 0.6, fhigh = 1 / 0.35, genplot = FALSE, verbose = FALSE)
  am <- cd_cycle_agemodel(f, cycle = 21, pts = 3)
  tuned <- astrochron::linterp(astrochron::tune(g, am, genplot = FALSE, verbose = FALSE), dt = 1, genplot = FALSE,
      verbose = FALSE)
  xrf <- CycloDesk:::cd_example_data("Bisciaro_XRF")
  srd <- data.frame(d = g[, 1], sr = 2 + sin(g[, 1]))
  skip_net <- c("astro_solution", "insolation", "etp")
  for (t in CycloDesk:::cd_tools()) {
    if (t$id %in% skip_net || t$id %in% c("etimeopt", "coherence", "xwavelet", "ecoco", "dynot", "rho1", "changepoint")) {
      next
    }
    d <- switch(t$id, agemodel = f, amplitude = f, sedrate2age = srd, column = xrf, g)
    if (t$inputs == 0) {
      d <- NULL
    }
    d2 <- switch(t$id, tune = am, NULL)
    p <- CycloDesk:::cd_defaults(t, d)
    if (t$id == "coco") {
      p$nsim <- 0
    }
    r <- CycloDesk:::cd_run_tool(t$id, d = d, p = p, d2 = d2, file = "x.txt", file2 = "y.txt")
    expect_true(is.list(r), info = t$id)
    if (!is.null(t$plot)) {
      grDevices::pdf(NULL, width = 11, height = 8)
      expect_no_error(t$plot(r$result, r$params), message = t$id)
      grDevices::dev.off()
    }
    for (o in if (t$id == "remove") { list() } else { r$outputs }) {
      expect_true(nrow(as.data.frame(o$data)) > 0, info = paste(t$id, o$name))
    }
  }
})
