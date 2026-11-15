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
