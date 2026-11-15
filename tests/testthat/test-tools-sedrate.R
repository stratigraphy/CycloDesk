# Tools of the Sed. rate menu: every tool runs (small settings for the slow ones) and draws its figure

sr_test_run <- function(id, d, p_over = list(), d2 = NULL, shows = NULL, show_param = "show") {
  t <- CycloDesk:::cd_get_tool(id)
  p <- CycloDesk:::cd_defaults(t, d)
  for (nm in names(p_over)) {
    p[nm] <- list(p_over[[nm]])
  }
  r <- CycloDesk:::cd_run_tool(id, d = d, p = p, d2 = d2, file = "x.txt", file2 = "y.txt")
  expect_true(is.list(r), info = id)
  expect_true(length(r$code) > 0, info = id)
  expect_no_error(parse(text = r$code), message = id)
  for (o in r$outputs) {
    expect_true(nrow(as.data.frame(o$data)) > 0, info = paste(id, o$name))
  }
  if (is.null(shows)) {
    shows <- p[[show_param]]
  }
  for (sh in shows) {
    p[show_param] <- list(sh)
    grDevices::pdf(NULL, width = 11, height = 8)
    expect_no_error(t$plot(r$result, p), message = paste(id, sh))
    grDevices::dev.off()
  }
  r
}

test_that("Sed. rate tools are registered with unique ids", {
  tl <- CycloDesk:::cd_tools_sedrate()
  ids <- character()
  for (t in tl) {
    ids <- c(ids, t$id)
    expect_identical(t$menu, "Sed. rate")
    expect_true(grepl("^sr_", t$id))
  }
  expect_false(anyDuplicated(ids) > 0)
  all_ids <- character()
  for (t in CycloDesk:::cd_tools()) {
    all_ids <- c(all_ids, t$id)
  }
  expect_false(anyDuplicated(all_ids) > 0)
})

test_that("helpers convert units and periods", {
  expect_equal(CycloDesk:::cd_sr_age(c(0, 1, 2), c(2, 2, 2), 100), c(0, 50, 100))
  o9 <- CycloDesk:::cd_sr_orbit9(400)
  expect_length(o9, 9)
  expect_true(all(diff(o9) <= 0))
  pts <- CycloDesk:::cd_sr_points("156 2.4; 165 2.2")
  expect_equal(nrow(pts), 2)
  expect_error(CycloDesk:::cd_sr_points("156"))
})

test_that("scalogram tracking tools run and write sed. rate curves", {
  skip_on_cran()
  oldop <- options(waverider.dev_new = FALSE)
  on.exit(options(oldop), add = TRUE)
  g <- as.data.frame(WaverideR::grey)
  r <- sr_test_run("sr_hmm", g, list(n_samples = 30), shows = c("track", "evidence", "posterior", "curve"))
  sr <- r$outputs[[2]]$data
  expect_equal(ncol(sr), 2)
  expect_true(all(sr[, 2] > 0.5 & sr[, 2] < 5))
  expect_equal(ncol(r$outputs[[3]]$data), 2)
  sr_test_run("sr_hmm", g[g[, 1] < 166, ], list(method = "iterative", scalogram_it = "cwt", n_samples = 20,
      periods = "list"), shows = c("track", "posterior"))
  sr_test_run("sr_chirp", g, list(n_samples = 20), shows = "x")
  sr_test_run("sr_chirp", g[g[, 1] < 166, ], list(method = "rategram"), shows = "x")
  r <- sr_test_run("sr_mesa", g, shows = "x")
  expect_true(all(r$outputs[[2]]$data[, 2] > 0))
  sr_test_run("sr_flmw", g, shows = "x")
  sr_test_run("sr_flmw", g[g[, 1] < 162, ], list(method = "sumpower", n_sim = 2, srstep = 0.2), shows = "x")
})

test_that("eCOCO and COCO tools run", {
  skip_on_cran()
  oldop <- options(waverider.dev_new = FALSE)
  on.exit(options(oldop), add = TRUE)
  g <- as.data.frame(WaverideR::grey)
  gs <- g[seq(1, nrow(g), by = 2), ]
  r <- sr_test_run("sr_ecoco", gs, list(nsim = 10, srstep = 0.25), shows = "x")
  expect_true(nrow(r$outputs[[1]]$data) > 0)
  sr_test_run("sr_ecoco", gs, list(method = "adaptive", nsim = 10, srstep = 0.25, tracker = "points",
      points = "157 2.4; 165 2.2; 172 2.5", orbit = "age", age_ma = 100), shows = "x")
  sr_test_run("sr_ecoco", gs, list(nsim = 10, srstep = 0.25, tracker = "slices", p_max = 1), shows = "x")
  sr_test_run("sr_coco", gs, list(nsim = 20, srstep = 0.1), shows = c("result", "spectra", "pcoco", "audit", "target"),
      show_param = "page")
  sr_test_run("sr_coco", gs, list(method = "adaptive", nsim = 20, srstep = 0.1), shows = c("result", "target"),
      show_param = "page")
  tt <- 0:3000
  astro <- data.frame(t = tt, v = sin(2 * pi * tt / 405) + 0.5 * sin(2 * pi * tt / 100) + 0.7 * sin(2 * pi * tt / 41) +
      0.6 * sin(2 * pi * tt / 21))
  sr_test_run("sr_coco_series", gs, list(nsim = 0), d2 = astro, shows = "x")
  r <- sr_test_run("sr_coco_series", gs, list(method = "ecoco", nsim = 0), d2 = astro, shows = "x")
  expect_equal(ncol(r$outputs[[2]]$data), 2)
})

test_that("astronomical testing tools run", {
  skip_on_cran()
  oldop <- options(waverider.dev_new = FALSE)
  on.exit(options(oldop), add = TRUE)
  g <- as.data.frame(WaverideR::grey)
  gs <- g[seq(1, nrow(g), by = 2), ]
  sr_test_run("sr_wto_multi", gs, list(n_null = 3, numsed = 8, step = 100), shows = "x")
  r <- sr_test_run("sr_astro_test", gs, list(n_null = 5, numsed = 10), shows = "x")
  expect_equal(length(r$outputs), 4)
  sr_test_run("sr_astro_test", gs, list(method = "tune", n_sur = 4, n_paths = 10, numsed = 10, periods = "age",
      age_ma = 100), shows = "x")
  set.seed(1)
  g2 <- data.frame(gs[, 1], gs[, 2] + stats::rnorm(nrow(gs), sd = stats::sd(gs[, 2])))
  sr_test_run("sr_astro_multi", gs, list(n_null = 4, numsed = 8), d2 = g2, shows = "x")
  sr_test_run("sr_pda_legacy", g, shows = "x")
})
