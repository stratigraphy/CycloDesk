# Update check ####
#
# CycloDesk and WaverideR are installed from GitHub. Once a day (in a background process) the installed commits are
# compared with the newest commits on GitHub; when one is newer, the interface offers to update. With the desktop
# launcher the update is installed after the app has closed (a package must not be replaced while R uses it) and
# the app is started again; without the launcher the R code for the update is shown.

cd_github_repos <- function() {
  c(WaverideR = "stratigraphy/WaverideR", CycloDesk = "stratigraphy/CycloDesk")
}

# Where an installed package came from: "github" (with its commit), "cran", "local" (a development install from a
# source folder) or "none"
cd_pkg_origin <- function(pkg) {
  d <- suppressWarnings(utils::packageDescription(pkg))
  if (!is.list(d)) {
    return(list(origin = "none", sha = NA_character_, version = NA_character_))
  }
  if (!is.null(d$RemoteSha) && identical(tolower(as.character(d$RemoteType)), "github")) {
    return(list(origin = "github", sha = d$RemoteSha, version = d$Version))
  }
  if (identical(d$Repository, "CRAN")) {
    return(list(origin = "cran", sha = NA_character_, version = d$Version))
  }
  list(origin = "local", sha = NA_character_, version = d$Version)
}

# The newest commit of the default branch of a GitHub repository (NA when GitHub cannot be reached)
cd_github_head <- function(repo, timeout = 8) {
  old <- options(timeout = timeout)
  on.exit(options(old), add = TRUE)
  txt <- tryCatch(suppressWarnings(readLines(sprintf("https://api.github.com/repos/%s/commits/HEAD", repo), warn = FALSE)),
      error = function(e) { NULL })
  if (is.null(txt)) {
    return(NA_character_)
  }
  m <- regmatches(paste(txt, collapse = ""), regexpr("\"sha\" *: *\"[0-9a-f]{40}\"", paste(txt, collapse = "")))
  if (!length(m)) {
    return(NA_character_)
  }
  sub("^.*\"([0-9a-f]{40})\"$", "\\1", m)
}

# Status of CycloDesk and WaverideR: one row per package with the origin, the installed and the newest commit and
# whether an update is available. The result is cached for a day unless force = TRUE.
cd_update_status <- function(force = FALSE) {
  cache <- file.path(tools::R_user_dir("CycloDesk", "cache"), "update_check.rds")
  if (!force && file.exists(cache)) {
    old <- try(readRDS(cache), silent = TRUE)
    if (!inherits(old, "try-error") && difftime(Sys.time(), old$time, units = "hours") < 24) {
      return(old$status)
    }
  }
  repos <- cd_github_repos()
  rows <- vector("list", length(repos))
  for (i in seq_along(repos)) {
    o <- cd_pkg_origin(names(repos)[i])
    head <- if (identical(o$origin, "local")) { NA_character_ } else { cd_github_head(repos[[i]]) }
    upd <- !is.na(head) && (o$origin %in% c("cran", "none") || (identical(o$origin, "github") && !identical(o$sha, head)))
    rows[[i]] <- data.frame(package = names(repos)[i], repo = repos[[i]], origin = o$origin, version = o$version,
        installed = o$sha, latest = head, update = upd, stringsAsFactors = FALSE)
  }
  status <- do.call(rbind, rows)
  if (!all(is.na(status$latest) & status$origin != "local")) {
    dir.create(dirname(cache), recursive = TRUE, showWarnings = FALSE)
    try(saveRDS(list(time = Sys.time(), status = status), cache), silent = TRUE)
  }
  status
}

cd_update_code <- function(status) {
  repos <- status$repo[status$update]
  c("# close CycloDesk first, then in a fresh R session:",
      sprintf("remotes::install_github(%s, upgrade = \"never\")", deparse(repos, width.cutoff = 500L)))
}

# Starts the launcher's updater (it waits until this app has stopped, installs, and starts the app again)
cd_start_updater <- function(status) {
  dir <- getOption("CycloDesk.launcher")
  upd <- if (is.null(dir)) { "" } else { file.path(dir, "update.R") }
  if (.Platform$OS.type != "windows" || !nzchar(upd) || !file.exists(upd)) {
    return(FALSE)
  }
  rscript <- normalizePath(file.path(R.home("bin"), "Rscript.exe"), winslash = "\\", mustWork = FALSE)
  args <- c("/c", "start", "\"CycloDesk update\"", shQuote(rscript, type = "cmd"),
      shQuote(normalizePath(upd, winslash = "\\", mustWork = FALSE), type = "cmd"), status$repo[status$update])
  res <- try(system2("cmd", args, wait = FALSE), silent = TRUE)
  !inherits(res, "try-error")
}
