# cyclodesk_update.R: started by "Update and restart" in an installed CycloDesk (Windows, macOS, Linux) ####
#
# Waits until CycloDesk has stopped (a package must not be replaced while R uses it), installs the newest versions
# of the given GitHub repositories into CycloDesk's own R library, and starts CycloDesk again. Arguments: GitHub
# repositories, e.g. stratigraphy/WaverideR stratigraphy/CycloDesk. The log is written to cyclodesk_update_log.txt
# in tools::R_user_dir("CycloDesk", "data").

local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- grep("^--file=", a, value = TRUE)
  dir <- if (length(f)) { dirname(normalizePath(sub("^--file=", "", f[1]))) } else { getwd() }
  assign("MENU_DIR", dir, envir = globalenv())
})
PORT <- 7792L
repos <- commandArgs(trailingOnly = TRUE)
repos <- repos[grepl("^stratigraphy/[A-Za-z0-9.]+$", repos)]

data_dir <- tools::R_user_dir("CycloDesk", "data")
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
log_file <- file.path(data_dir, "cyclodesk_update_log.txt")
cat(sprintf("Update, %s\n", format(Sys.time())), file = log_file)
say <- function(...) {
  txt <- paste0(...)
  cat(txt, "\n", sep = "")
  cat(txt, "\n", sep = "", file = log_file, append = TRUE)
}

# The library that receives the update: the installed library when it is writable, else a library in the user's
# CycloDesk folder (cyclodesk_start.R puts that one first)
lib <- .libPaths()[1]
if (file.access(lib, 2) != 0) {
  lib <- file.path(data_dir, "library")
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  .libPaths(c(lib, .libPaths()))
}
say("Library: ", lib)

# wait (at most a minute) until the app no longer answers on its port
port_open <- function() {
  con <- suppressWarnings(try(socketConnection("127.0.0.1", PORT, open = "r", blocking = TRUE, timeout = 1), silent = TRUE))
  if (inherits(con, "try-error")) {
    return(FALSE)
  }
  close(con)
  TRUE
}
say("Waiting for CycloDesk to close ...")
k <- 0
while (port_open() && k < 60) {
  Sys.sleep(1)
  k <- k + 1
}
Sys.sleep(2)

# An interrupted install leaves a lock folder 00LOCK-<package> and an empty package folder; remove both ####
clear_locks <- function(lib, pkgs = c("WaverideR", "CycloDesk")) {
  for (pk in pkgs) {
    locks <- Sys.glob(file.path(lib, paste0("00LOCK-", pk, "*")))
    for (lk in locks) {
      say("Removing the lock ", lk, " left by an interrupted install ...")
      unlink(lk, recursive = TRUE, force = TRUE)
    }
    d <- file.path(lib, pk)
    if (dir.exists(d) && !file.exists(file.path(d, "DESCRIPTION"))) {
      say("Removing the incomplete folder ", d, " ...")
      unlink(d, recursive = TRUE, force = TRUE)
    }
  }
}
clear_locks(lib)

ok <- TRUE
options(repos = c(CRAN = "https://cloud.r-project.org"))
for (r in repos) {
  say("Installing the newest ", basename(r), " from GitHub ...")
  res <- try(remotes::install_github(r, lib = lib, upgrade = "never", dependencies = NA), silent = TRUE)
  if (inherits(res, "try-error")) {
    ok <- FALSE
    say("FAILED: ", conditionMessage(attr(res, "condition")))
  }
}
# a fresh check is needed after an update
unlink(file.path(tools::R_user_dir("CycloDesk", "cache"), "update_check.rds"))

if (ok) {
  say("Done. Starting CycloDesk ...")
} else {
  say("The update did not complete; CycloDesk keeps the previous version. Log: ", log_file)
}
if (.Platform$OS.type == "windows") {
  vbs <- normalizePath(file.path(MENU_DIR, "cyclodesk_start.vbs"), winslash = "\\", mustWork = FALSE)
  system2(file.path(Sys.getenv("SystemRoot", "C:\\Windows"), "System32", "wscript.exe"), shQuote(vbs, type = "cmd"),
      wait = FALSE)
} else {
  system2(file.path(R.home("bin"), "Rscript"), shQuote(file.path(MENU_DIR, "cyclodesk_start.R")), wait = FALSE,
      stdout = FALSE, stderr = FALSE)
}
Sys.sleep(3)
