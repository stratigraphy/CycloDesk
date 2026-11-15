# Tools of DecomposeR (empirical mode decomposition), lomb, dtw, recurrence plots and cross-spectral analysis

dc_test_run <- function(id, d = NULL, over = list(), d2 = NULL, shows = NULL) {
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
  expect_true(length(r$outputs) > 0, info = id)
  for (o in r$outputs) {
    expect_true(nrow(as.data.frame(o$data)) > 0, info = paste(id, o$name))
  }
  if (is.null(shows)) {
    shows <- if (is.null(p$show)) { "x" } else { p$show }
  }
  for (sh in shows) {
    p["show"] <- list(sh)
    grDevices::pdf(NULL, width = 11, height = 8)
    expect_no_error(t$plot(r$result, p), message = paste(id, sh))
    grDevices::dev.off()
  }
  invisible(r)
}

test_that("decompose tools are registered with unique ids and the dc_ prefix", {
  tl <- CycloDesk:::cd_tools_decompose()
  expect_true(length(tl) >= 9)
  for (t in tl) {
    expect_true(grepl("^dc_", t$id))
    expect_true(t$menu %in% c("Time Series", "Age models"))
    expect_true(nzchar(t$info), info = t$id)
    expect_true(nzchar(t$uses), info = t$id)
  }
  ids <- character()
  for (t in CycloDesk:::cd_tools()) {
    ids <- c(ids, t$id)
  }
  expect_equal(anyDuplicated(ids), 0L)
})

test_that("helpers parse mode lists and run base-R SSA", {
  expect_equal(CycloDesk:::cd_dc_ints("3, 5-7"), c(3L, 5L, 6L, 7L))
  expect_equal(CycloDesk:::cd_dc_ints(""), integer())
  g <- CycloDesk:::cd_dc_groups("1, 2; 4:5")
  expect_length(g, 2)
  expect_equal(g[[2]], 4:5)
  tt <- seq(0, 99.5, by = 0.5)
  y <- sin(2 * pi * tt / 10) + 0.5 * sin(2 * pi * tt / 3)
  s <- CycloDesk:::cd_dc_ssa(y, 40, 6)
  expect_equal(dim(s$rec), c(length(y), 4))
  expect_true(max(abs(rowSums(s$rec[, 1:4]) - y)) < 1e-6)
  R <- diag(5) > 0
  R[1, 2] <- TRUE
  R[2, 1] <- TRUE
  q <- CycloDesk:::cd_dc_rqa(R)
  expect_true(q$RR > 0)
})

test_that("EMD, SSA, instantaneous frequency and ratio tools run", {
  skip_on_cran()
  skip_if_not_installed("DecomposeR")
  oldop <- options(waverider.dev_new = FALSE)
  on.exit(options(oldop), add = TRUE)
  g <- as.data.frame(WaverideR::grey)
  r <- dc_test_run("dc_emd", g, list(comb = 6, series = "4", sum = "3-5", merge = "1, 2"), shows = c("modes", "plot_emd"))
  tab <- r$outputs[[1]]$data
  expect_true(all(c("depth", "signal", "residue", "trend") %in% names(tab)))
  expect_true(max(abs(rowSums(tab[, -(1:2)]) - tab$signal)) < 1e-6)
  dc_test_run("dc_emd", g[seq(1, nrow(g), by = 2), ], list(method = "emd", interp = "gcrd", checks = FALSE, bind = TRUE,
      show = "plot_emd", fig_modes = "3, 4"))
  dc_test_run("dc_ssa", g, list(groups = "2, 3; 4, 5", series = "1"), shows = c("comps", "norms"))
  r <- dc_test_run("dc_pulse", tab, list(lines = "0.47"), shows = c("periods", "mode", "dist", "pop", "imf"))
  per <- r$result$per
  expect_equal(ncol(per), sum(!grepl("^(residue|trend)", names(tab)[-(1:2)])))
  for (m in c("NHT", "DQ", "GZC", "chain")) {
    r <- dc_test_run("dc_pulse", tab, list(method = m, modes = "2, 3", departure = TRUE, smooth = 11, focus = 3),
        shows = c("periods", "mode", "imf"))
    nms <- character()
    for (o in r$outputs) {
      nms <- c(nms, o$name)
    }
    expect_true(any(grepl("^x-period_", nms)))
    expect_true(stats::median(r$result$per[, 2], na.rm = TRUE) > 0.3)
    expect_true(stats::median(r$result$per[, 2], na.rm = TRUE) < 0.7)
  }
  dc_test_run("dc_pulse", data.frame(depth = tab$depth, y = tab$imf_4), shows = "periods")
  r <- dc_test_run("dc_ratio", tab, list(modes = "3-5"), shows = c("depth", "pop"))
  expect_equal(nrow(r$outputs[[2]]$data), 3)
  expect_error(CycloDesk:::cd_run_tool("dc_ratio", d = g, p = CycloDesk:::cd_defaults(CycloDesk:::cd_get_tool("dc_ratio"), g)),
      "decomposition table")
  dc_test_run("dc_precision", NULL)
  dc_test_run("dc_precision", NULL, list(uneven = TRUE, method = "chain"))
})

test_that("Lomb-Scargle periodogram works on uneven data without interpolation", {
  skip_on_cran()
  skip_if_not_installed("lomb")
  set.seed(2)
  tt <- sort(stats::runif(300, 0, 100))
  d <- data.frame(t = tt, y = sin(2 * pi * tt / 7) + stats::rnorm(300, sd = 0.5))
  r <- dc_test_run("dc_lomb", d)
  sp <- r$outputs[[1]]$data
  expect_equal(1 / sp$frequency[which.max(sp$power)], 7, tolerance = 0.05)
  dc_test_run("dc_lomb", d, list(randomise = TRUE, repeats = 10, normalize = "press", xaxis = "period", logy = TRUE))
})

test_that("cross-spectral analysis gives the lag of series 2", {
  skip_on_cran()
  tt <- seq(0, 200, by = 0.5)
  set.seed(3)
  d1 <- data.frame(t = tt, y = sin(2 * pi * tt / 20) + stats::rnorm(length(tt), sd = 0.3))
  d2 <- data.frame(t = tt, y = sin(2 * pi * (tt - 2) / 20) + stats::rnorm(length(tt), sd = 0.3))
  r <- dc_test_run("dc_xspec", d1, d2 = d2)
  tb <- r$outputs[[1]]$data
  k <- which.min(abs(tb$frequency - 1 / 20))
  expect_equal(tb$lag[k], 2, tolerance = 0.1)
  expect_true(tb$coherence[k] > tb$coherence_level[k])
  dc_test_run("dc_xspec", d1, list(third = "lag", xaxis = "period"), d2 = d2)
})

test_that("recurrence plot and dynamic time warping run", {
  skip_on_cran()
  g <- as.data.frame(WaverideR::grey)
  r <- dc_test_run("dc_recurrence", g[1:700, ], shows = c("rp", "dist"))
  expect_true(r$outputs[[1]]$data$RR > 0)
  r <- dc_test_run("dc_recurrence", g, list(thr = "rr", maxn = 400), shows = "rp")
  expect_equal(r$outputs[[1]]$data$RR, 0.05, tolerance = 0.01)
  skip_if_not_installed("dtw")
  gs <- g[g[, 1] > 158 & g[, 1] < 170, ]
  w <- data.frame(depth = 10 + (gs[, 1] - 158)^1.1, v = gs[, 2])
  r <- dc_test_run("dc_dtw", g, d2 = w, list(npts = 400, open_end = TRUE, open_begin = TRUE, step = "asymmetric"),
      shows = c("align", "overlay", "tie"))
  tie <- r$outputs[[1]]$data
  expect_equal(min(tie[, 2]), 158, tolerance = 0.005)
  expect_equal(max(tie[, 2]), 170, tolerance = 0.005)
  dc_test_run("dc_dtw", g, d2 = w, list(npts = 300, window = "sakoechiba", wsize = 0.3), shows = "align")
  expect_error(dc_test_run("dc_dtw", g, d2 = w, list(npts = 300, open_begin = TRUE)), "open end")
})
