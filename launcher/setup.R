# setup.R: installs CycloDesk and makes the desktop shortcut (run it with SETUP.bat) ####
#
# Installs the CRAN packages CycloDesk needs, the newest WaverideR and CycloDesk from GitHub, writes the hidden
# launcher start_app.vbs and the desktop shortcut "CycloDesk". Everything is also written to state/setup_log.txt.
# Developers: a file local_source.txt in this folder with the source folders of WaverideR and CycloDesk (one per
# line) installs those instead of the GitHub versions.

local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- grep("^--file=", a, value = TRUE)
  dir <- if (length(f)) { dirname(normalizePath(sub("^--file=", "", f[1]))) } else { getwd() }
  assign("APP_DIR", dir, envir = globalenv())
})

APP_TITLE <- "CycloDesk"
PORT <- 7792L
REPOS <- c("stratigraphy/WaverideR", "stratigraphy/CycloDesk")
CRAN <- "https://cloud.r-project.org"
NEED <- c("remotes", "shiny", "callr", "astrochron", "readxl", "png", "jpeg", "DecomposeR", "lomb", "dtw")

state_dir <- file.path(APP_DIR, "state")
dir.create(state_dir, showWarnings = FALSE)
log_file <- file.path(state_dir, "setup_log.txt")
cat(sprintf("Setup of %s, %s\n", APP_TITLE, format(Sys.time())), file = log_file)
say <- function(...) {
  txt <- paste0(...)
  cat(txt, "\n", sep = "")
  cat(txt, "\n", sep = "", file = log_file, append = TRUE)
}
win <- function(p) {
  normalizePath(p, winslash = "\\", mustWork = FALSE)
}
run_ps <- function(lines) {
  f <- tempfile(fileext = ".ps1")
  on.exit(unlink(f), add = TRUE)
  writeLines(lines, f)
  suppressWarnings(system2("powershell", c("-NoProfile", "-ExecutionPolicy", "Bypass", "-File", shQuote(f)),
      stdout = TRUE, stderr = TRUE))
}

# An install that was interrupted (e.g. while CycloDesk was running) leaves a lock folder 00LOCK-<package> and an
# empty package folder; every later install of that package then fails. Remove both. ####
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
# Installs and writes warnings and errors to the log (install.packages only warns when a package fails)
logged <- function(expr) {
  withCallingHandlers(tryCatch(expr, error = function(e) {
    say("FAILED: ", conditionMessage(e))
  }), warning = function(w) {
    say("Warning: ", conditionMessage(w))
    invokeRestart("muffleWarning")
  })
}

# A user library when the R library is not writable (R installed for all users without admin rights) ####
lib <- .libPaths()[1]
if (file.access(lib, 2) != 0) {
  lib <- Sys.getenv("R_LIBS_USER")
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  .libPaths(c(lib, .libPaths()))
  say("Packages go to the user library ", lib)
}

# Packages ####
for (pk in NEED) {
  if (!requireNamespace(pk, quietly = TRUE)) {
    say("Installing ", pk, " from CRAN ...")
    logged(utils::install.packages(pk, lib = lib, repos = CRAN))
  }
}
# CycloDesk must not run while it is replaced
port_open <- function() {
  con <- suppressWarnings(try(socketConnection("127.0.0.1", PORT, open = "r", blocking = TRUE, timeout = 1), silent = TRUE))
  if (inherits(con, "try-error")) {
    return(FALSE)
  }
  close(con)
  TRUE
}
if (port_open()) {
  say("CycloDesk is still running: close it (Close button in CycloDesk, or close its browser tab) ...")
  k <- 0
  while (port_open() && k < 120) {
    Sys.sleep(1)
    k <- k + 1
  }
  if (port_open()) {
    say("CycloDesk is still running; close it and run SETUP.bat again.")
    stop("CycloDesk is running", call. = FALSE)
  }
}
clear_locks(lib)
for (pk in c("CycloDesk", "WaverideR")) {
  if (isNamespaceLoaded(pk)) {
    unloadNamespace(pk)
  }
}
local_src <- file.path(APP_DIR, "local_source.txt")
if (file.exists(local_src)) {
  src <- trimws(readLines(local_src, warn = FALSE))
  src <- src[nzchar(src)]
  for (s in src) {
    say("Installing from the source folder ", s, " ...")
    logged(utils::install.packages(s, lib = lib, repos = NULL, type = "source"))
  }
} else {
  options(repos = c(CRAN = CRAN))
  for (r in REPOS) {
    say("Installing the newest ", basename(r), " from GitHub (", r, ") ...")
    res <- try(remotes::install_github(r, lib = lib, upgrade = "never", dependencies = NA), silent = TRUE)
    if (inherits(res, "try-error")) {
      say("FAILED: ", conditionMessage(attr(res, "condition")))
    }
  }
}
if (!requireNamespace("CycloDesk", quietly = TRUE)) {
  say("CycloDesk could not be installed; see the messages above and ", win(log_file), ".")
} else {
  say("CycloDesk ", as.character(utils::packageVersion("CycloDesk")), ", WaverideR ",
      as.character(utils::packageVersion("WaverideR")), ", astrochron ", as.character(utils::packageVersion("astrochron")),
      ", shiny ", as.character(utils::packageVersion("shiny")))
}

# The earlier test version was called RCycle: remove it and its shortcut ####
if (requireNamespace("RCycle", quietly = TRUE)) {
  say("Removing the old test version RCycle ...")
  try(utils::remove.packages("RCycle"), silent = TRUE)
}
old_lnk <- file.path(Sys.getenv("USERPROFILE"), "Desktop", "RCycle.lnk")
if (file.exists(old_lnk)) {
  unlink(old_lnk)
}

writeLines(.libPaths(), file.path(state_dir, "libpaths.txt"))
rscript <- win(file.path(R.home("bin"), "Rscript.exe"))
say("R: ", R.version.string, " (", rscript, ")")

# Launcher: opens the page if the app already runs, else starts R without a window ####
vbs <- file.path(APP_DIR, "start_app.vbs")
con <- file(vbs, "wb")
writeLines(c(
    'Set sh = CreateObject("WScript.Shell")',
    sprintf('rc = sh.Run("cmd /c netstat -ano | findstr /R /C:"":%d .*LISTENING"" >nul", 0, True)', PORT),
    "If rc = 0 Then",
    sprintf('  sh.Run "http://127.0.0.1:%d", 1, False', PORT),
    "Else",
    sprintf('  sh.Run """%s"" -e ""shiny::runApp(\'%s\', port = %d, launch.browser = TRUE)""", 0, False',
        rscript, gsub("\\\\", "/", APP_DIR), PORT),
    "End If"), con, sep = "\r\n")
close(con)

# Desktop shortcut ####
say(paste(run_ps(c(
    "$desktop = [Environment]::GetFolderPath('Desktop')",
    "$w = New-Object -ComObject WScript.Shell",
    sprintf("$s = $w.CreateShortcut((Join-Path $desktop '%s.lnk'))", APP_TITLE),
    "$s.TargetPath = 'wscript.exe'",
    sprintf("$s.Arguments = '\"%s\"'", win(vbs)),
    sprintf("$s.WorkingDirectory = '%s'", win(APP_DIR)),
    sprintf("$s.IconLocation = '%s,0'", win(file.path(APP_DIR, "cyclodesk.ico"))),
    "$s.Description = 'CycloDesk: point-and-click cyclostratigraphy with WaverideR and astrochron'",
    "$s.Save()",
    sprintf("Write-Output 'Desktop shortcut written: %s.lnk'", APP_TITLE))), collapse = "\n"))

say("Done. Start CycloDesk with the desktop icon. Log: ", win(log_file))
