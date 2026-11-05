test_that("read_series detects separators and headers", {
  d <- data.frame(depth = seq(0, 1, by = 0.1), value = sin(1:11))
  f1 <- tempfile(fileext = ".txt")
  write_series(d, f1)
  r1 <- read_series(f1)
  expect_equal(unname(as.matrix(r1)), unname(as.matrix(d)), tolerance = 1e-8)
  f2 <- tempfile(fileext = ".csv")
  utils::write.csv(d, f2, row.names = FALSE)
  r2 <- read_series(f2)
  expect_equal(names(r2), c("depth", "value"))
  expect_equal(nrow(r2), 11)
})

test_that("cd_clean sorts and averages duplicates", {
  d <- data.frame(x = c(2, 1, 1, 3), y = c(5, 1, 3, 7))
  r <- CycloDesk:::cd_clean(d)
  expect_equal(r$x, c(1, 2, 3))
  expect_equal(r$y, c(2, 5, 7))
})

test_that("insolation has the right size and range", {
  s <- data.frame(Time_ka = 0:10, prec_LA04 = rep(1.796, 11), tilt_LA04 = rep(0.4091, 11), ecc_LA04 = rep(0.0167, 11))
  q <- cd_insolation(tmin = 0, tmax = 10, lat = 65, lon = 90, solution = s)
  expect_equal(nrow(q), 11)
  expect_true(all(q$insolation > 450 & q$insolation < 520))
})

test_that("cycle age model gives one period per peak", {
  x <- seq(0, 10, by = 0.01)
  am <- cd_cycle_agemodel(data.frame(x, sin(2 * pi * x)), cycle = 20, pts = 5)
  expect_equal(nrow(am), 10)
  expect_equal(diff(am$age_kyr), rep(20, 9))
  expect_equal(diff(am$depth), rep(1, 9), tolerance = 0.02)
})

test_that("LAS files are read with NULL values as NA, wrapped or not", {
  f <- tempfile(fileext = ".las")
  writeLines(c("~VERSION INFORMATION", " VERS.   2.0 : CWLS LOG ASCII STANDARD", " WRAP.   NO  : ONE LINE PER DEPTH STEP",
      "~WELL INFORMATION", " NULL.   -999.2500 : NULL VALUE", "~CURVE INFORMATION", " DEPT.M   : DEPTH", " GR  .GAPI : GAMMA RAY",
      "~A", "100.0  50.5", "100.5  -999.25", "101.0  61.0"), f)
  d <- read_series(f)
  expect_equal(names(d), c("DEPT (M)", "GR (GAPI)"))
  expect_true(is.na(d[2, 2]))
  w <- tempfile(fileext = ".las")
  writeLines(c("~V", " WRAP. YES : wrapped", "~W", " NULL. -999.25 :", "~C", " DEPT.M :", " A.U :", " B.U :", "~A", "1.0",
      "2 3", "1.5", "4 5"), w)
  dw <- read_series(w)
  expect_equal(dim(dw), c(2L, 3L))
  expect_equal(dw[2, 3], 5)
})

test_that("Excel sheets are read with and without header", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  f <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(A = data.frame(depth = 1:5, Ca = (1:5) / 10), B = data.frame(depth = 1:3, Fe = 7:9)), f)
  a <- read_series(f)
  expect_equal(names(a), c("depth", "Ca"))
  b <- read_series(f, sheet = "B")
  expect_equal(b$Fe, 7:9)
})

test_that("semicolon csv with decimal commas is read", {
  f <- tempfile(fileext = ".csv")
  writeLines(c("Depth;Grey", "1,5;300,2", "2,0;310,8"), f)
  d <- read_series(f)
  expect_equal(d$Grey, c(300.2, 310.8))
})

test_that("files are moved and copied without overwriting", {
  src <- file.path(tempdir(), "cd_src")
  dst <- file.path(tempdir(), "cd_dst")
  dir.create(src, showWarnings = FALSE)
  dir.create(dst, showWarnings = FALSE)
  writeLines("1 2", file.path(src, "a.txt"))
  writeLines("3 4", file.path(src, "b.txt"))
  writeLines("old", file.path(dst, "b.txt"))
  r <- CycloDesk:::cd_move_files(file.path(src, c("a.txt", "b.txt")), dst)
  expect_equal(r$done, "a.txt")
  expect_equal(r$skipped, "b.txt")
  expect_false(file.exists(file.path(src, "a.txt")))
  expect_equal(readLines(file.path(dst, "b.txt")), "old")
  r2 <- CycloDesk:::cd_move_files(file.path(src, "b.txt"), file.path(tempdir()), copy = TRUE)
  expect_true(file.exists(file.path(src, "b.txt")))
})

test_that("anchored age model passes through the ties and keeps the floating rate outside", {
  base <- function(z) {
    (z - 100) * 50
  }
  ties <- data.frame(depth = c(102, 106), time = c(120, 300))
  a <- CycloDesk:::cd_anchor_model(c(100, 102, 104, 106, 108), base, ties)
  expect_equal(a[c(2, 4)], c(120, 300))
  expect_equal(a[3], 210)
  expect_equal(a[1], 120 - 100)
  expect_equal(a[5], 300 + 100)
  one <- CycloDesk:::cd_anchor_model(c(100, 104), base, ties[1, ])
  expect_equal(one, c(0, 200) + 20)
})
