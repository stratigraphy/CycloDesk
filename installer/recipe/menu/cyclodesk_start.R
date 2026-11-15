# cyclodesk_start.R: starts CycloDesk from the desktop or menu icon of an installed copy ####
#
# Installed by the CycloDesk installer (Windows, macOS, Linux) next to the icon files. When CycloDesk already runs,
# the page is opened again; otherwise R's web server is started and the page opened in the default browser.
# Messages go to cyclodesk_log.txt in the user's CycloDesk folder (tools::R_user_dir("CycloDesk", "data")).

local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- grep("^--file=", a, value = TRUE)
  dir <- if (length(f)) { dirname(normalizePath(sub("^--file=", "", f[1]))) } else { getwd() }
  assign("MENU_DIR", dir, envir = globalenv())
})
PORT <- 7792L

log_dir <- tools::R_user_dir("CycloDesk", "data")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
# updates go here when the installed library is not writable (cyclodesk_update.R)
user_lib <- file.path(log_dir, "library")
if (dir.exists(user_lib)) {
  .libPaths(c(user_lib, .libPaths()))
}
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

if (port_open()) {
  cat("CycloDesk already runs: opening", url, "\n")
  open_page(url)
} else {
  # a broken ragg installation would stop every plot
  ragg_ok <- tryCatch(requireNamespace("ragg", quietly = TRUE), error = function(e) { FALSE },
      warning = function(w) { FALSE })
  options(waverider.dev_new = FALSE, shiny.useragg = isTRUE(ragg_ok), CycloDesk.launcher = MENU_DIR,
      CycloDesk.port = PORT, browser = open_page)
  app <- CycloDesk::cd_app(folder = NULL, stop_on_close = TRUE)
  shiny::runApp(app, port = PORT, launch.browser = TRUE)
}
