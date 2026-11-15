# cyclodesk_start.R: starts CycloDesk from the desktop or menu icon of an installed copy ####
#
# Started by the CycloDesk window program (with --port=<n> --no-browser) or, as a fallback, directly from the icon:
# then an already running CycloDesk is opened again in the browser, or R's web server is started and the page opened.
# Messages go to cyclodesk_log.txt in the user's CycloDesk folder (tools::R_user_dir("CycloDesk", "data")).

local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- grep("^--file=", a, value = TRUE)
  dir <- if (length(f)) { dirname(normalizePath(sub("^--file=", "", f[1]))) } else { getwd() }
  assign("MENU_DIR", dir, envir = globalenv())
})
args <- commandArgs(trailingOnly = TRUE)
PORT <- 7792L
p <- grep("^--port=[0-9]+$", args, value = TRUE)
if (length(p)) {
  PORT <- as.integer(sub("^--port=", "", p[1]))
}
BROWSER <- !("--no-browser" %in% args)
Sys.setenv(CYCLODESK_PORT = PORT)

log_dir <- tools::R_user_dir("CycloDesk", "data")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
# updates go here when the installed library is not writable (cyclodesk_update.R)
user_lib <- file.path(log_dir, "library")
# Only CycloDesk's own libraries: packages of another R on this computer (R_LIBS, R_LIBS_USER) can crash this R.
# The settings are passed on to the background R processes (callr) and the updater.
.libPaths(if (dir.exists(user_lib)) { user_lib } else { character() })
Sys.setenv(R_LIBS = "", R_LIBS_USER = file.path(R.home(), "no-user-library"),
    R_ENVIRON_USER = file.path(MENU_DIR, "none.Renviron"), R_PROFILE_USER = file.path(MENU_DIR, "none.Rprofile"))
log_file <- file.path(log_dir, "cyclodesk_log.txt")
log_con <- file(log_file, open = "wt")
sink(log_con, split = FALSE)
sink(log_con, type = "message")
cat(sprintf("CycloDesk started %s\nR %s in %s\n", format(Sys.time()), R.version.string, R.home()))

port_open <- function() {
  con <- suppressWarnings(try(socketConnection("127.0.0.1", PORT, open = "r", blocking = TRUE, timeout = 1), silent = TRUE))
  if (inherits(con, "try-error")) {
    return(FALSE)
  }
  close(con)
  TRUE
}
url <- sprintf("http://127.0.0.1:%d", PORT)

# Opens the page; on Linux xdg-open, on macOS open, on Windows the default browser
open_page <- function(u) {
  if (.Platform$OS.type == "windows") {
    shell.exec(u)
  } else if (Sys.info()[["sysname"]] == "Darwin") {
    system2("open", u, wait = FALSE)
  } else {
    system2("xdg-open", u, wait = FALSE, stdout = FALSE, stderr = FALSE)
  }
}

if (BROWSER && port_open()) {
  cat("CycloDesk already runs: opening", url, "\n")
  open_page(url)
} else {
  # a broken ragg installation would stop every plot
  ragg_ok <- tryCatch(requireNamespace("ragg", quietly = TRUE), error = function(e) { FALSE },
      warning = function(w) { FALSE })
  options(waverider.dev_new = FALSE, shiny.useragg = isTRUE(ragg_ok), CycloDesk.launcher = MENU_DIR,
      CycloDesk.port = PORT, browser = open_page)
  app <- CycloDesk::cd_app(folder = NULL, stop_on_close = TRUE)
  shiny::runApp(app, port = PORT, launch.browser = BROWSER)
}
