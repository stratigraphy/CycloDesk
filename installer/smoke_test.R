# smoke_test.R: checks an installed CycloDesk; run it with the installed R ####
#   <install folder>/bin/Rscript installer/smoke_test.R            (Linux, macOS)
#   <install folder>\Lib\R\bin\x64\Rscript.exe installer\smoke_test.R   (Windows)
# Loads the packages, runs a few tools (astrochron with compiled code, DecomposeR, lomb, a background process with
# callr) and starts the app and requests its page. Stops with an error when something fails.

say <- function(...) {
  cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), paste0(...)))
}
say("R ", R.version.string, " in ", R.home())
for (pk in c("CycloDesk", "WaverideR", "astrochron", "shiny", "callr", "DecomposeR", "lomb", "dtw", "readxl", "png",
    "jpeg", "ragg", "remotes")) {
  suppressPackageStartupMessages(library(pk, character.only = TRUE))
  say(pk, " ", as.character(utils::packageVersion(pk)))
}
options(waverider.dev_new = FALSE)

# Windows: no package DLL may contain 32-bit mingw pseudo relocations. They crash R ("Mingw-w64 runtime failure:
# 32 bit pseudo relocation ... out of range") whenever Windows happens to load two DLLs more than 2 GB apart,
# so a test run can pass while users' computers crash.
pseudo_reloc32 <- function(f) {
  b <- readBin(f, "raw", file.size(f))
  u32 <- function(o) {
    sum(as.numeric(as.integer(b[o + 1:4])) * 256^(0:3))
  }
  u16 <- function(o) {
    as.integer(b[o + 1]) + 256L * as.integer(b[o + 2])
  }
  pe <- u32(0x3c)
  nsec <- u16(pe + 6)
  sh <- pe + 24 + u16(pe + 20)
  n32 <- 0
  for (i in seq_len(nsec) - 1) {
    o <- sh + 40 * i
    nm <- rawToChar(b[o + 1:8][b[o + 1:8] != as.raw(0)])
    if (nm != ".rdata") {
      next
    }
    st <- u32(o + 20)
    en <- st + u32(o + 16) - 12
    k <- st
    while (k <= en) {
      if (u32(k) == 0 && u32(k + 4) == 0 && u32(k + 8) == 1) {
        j <- k + 12
        while (j + 12 <= en + 12) {
          fl <- u32(j + 8)
          if (!(fl %in% c(8, 16, 32, 64)) || u32(j) == 0) {
            break
          }
          n32 <- n32 + (fl == 32)
          j <- j + 12
        }
        k <- j
      } else {
        k <- k + 4
      }
    }
  }
  n32
}
if (.Platform$OS.type == "windows") {
  dlls <- Sys.glob(file.path(.Library, "*", "libs", "x64", "*.dll"))
  bad <- character()
  for (f in dlls) {
    n <- pseudo_reloc32(f)
    if (n > 0) {
      bad <- c(bad, sprintf("%s (%d)", basename(dirname(dirname(dirname(f)))), n))
    }
  }
  if (length(bad)) {
    stop("packages with 32-bit pseudo relocations (they can crash R on Windows): ", paste(bad, collapse = ", "))
  }
  say("DLLs checked for 32-bit pseudo relocations: ", length(dlls), ", none found")
}

g <- WaverideR::grey
g <- g[g[, 1] < 165, ]
grDevices::pdf(NULL)
for (id in c("spectrum", "evolutive", "filter", "mx_extremes", "ab_peaks", "at_spectest", "dc_emd", "dc_lomb")) {
  tl <- CycloDesk:::cd_tools()
  t <- NULL
  for (x in tl) {
    if (identical(x$id, id)) {
      t <- x
    }
  }
  if (is.null(t)) {
    stop("tool not found: ", id)
  }
  p <- CycloDesk:::cd_defaults(t, g)
  r <- CycloDesk:::cd_run_tool(id, d = g, p = p, file = "x.txt")
  if (!is.null(t$plot)) {
    t$plot(r$result, r$params)
  }
  say("tool ", id, ": ok")
}
grDevices::dev.off()
# astrochron's Fortran code
m <- astrochron::mtm(g, tbw = 2, genplot = FALSE, verbose = FALSE, output = 1)
stopifnot(nrow(m) > 10)
say("astrochron::mtm: ok")
# heavy tools run in a separate R process
v <- callr::r(function() {
  as.character(utils::packageVersion("CycloDesk"))
})
say("callr child process: CycloDesk ", v)

# The app answers on a free port
port <- 7799L
bg <- callr::r_bg(function(port) {
  options(waverider.dev_new = FALSE)
  shiny::runApp(CycloDesk::cd_app(folder = NULL, stop_on_close = FALSE), port = port, launch.browser = FALSE)
}, args = list(port = port))
on.exit(bg$kill(), add = TRUE)
page <- NULL
for (k in 1:60) {
  Sys.sleep(1)
  page <- tryCatch(suppressWarnings(readLines(sprintf("http://127.0.0.1:%d", port), warn = FALSE)), error = function(e) { NULL })
  if (!is.null(page)) {
    break
  }
}
if (is.null(page) || !any(grepl("<title>CycloDesk", page, fixed = TRUE))) {
  cat(bg$read_all_error(), "\n")
  stop("the app did not answer")
}
say("app page: ok (", length(page), " lines)")
bg$kill()
say("all checks passed")
