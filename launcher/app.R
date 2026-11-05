# app.R: CycloDesk, point-and-click cyclostratigraphy with WaverideR and astrochron ####
#
# Started by the desktop shortcut "CycloDesk" (made by SETUP.bat). From R:
#   shiny::runApp("<this folder>", port = 7792, launch.browser = TRUE)

assign("APP_DIR", normalizePath(getwd(), winslash = "/"), envir = globalenv())
lib_file <- file.path(APP_DIR, "state", "libpaths.txt")
if (file.exists(lib_file)) {
  .libPaths(c(readLines(lib_file, warn = FALSE), .libPaths()))
}
# a broken ragg installation ("unable to load shared object ... ragg.dll") would stop every plot
ragg_ok <- tryCatch(requireNamespace("ragg", quietly = TRUE), error = function(e) { FALSE }, warning = function(w) { FALSE })
# CycloDesk.launcher: lets "Update and restart" hand over to update.R in this folder
options(waverider.dev_new = FALSE, shiny.useragg = isTRUE(ragg_ok), CycloDesk.launcher = APP_DIR, CycloDesk.port = 7792L)
CycloDesk::cd_app(folder = NULL, stop_on_close = TRUE)
