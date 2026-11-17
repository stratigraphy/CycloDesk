# build_installer.R: builds the CycloDesk installer for the computer it runs on ####
#
# Windows: CycloDesk-<version>-Windows-x86_64.exe; macOS: CycloDesk-<version>-MacOSX-<arm64|x86_64>.pkg;
# Linux: CycloDesk-<version>-Linux-x86_64.sh. Each brings its own R (from conda-forge) with all packages and makes
# a desktop/menu icon. Steps:
#   0. build the CycloDesk window program (installer/shell, Rust/Tauri) with cargo;
#   1. collect the R source packages that are not on conda-forge in installer/build/recipe/src (CycloDesk and
#      WaverideR from source folders, astrochron, palinsol, lomb, StratigrapheR and DecomposeR from CRAN);
#   2. build the conda package "cyclodesk" with rattler-build (installer/recipe);
#   3. build the installer with constructor (installer/construct).
# Needs R, cargo, rattler-build and constructor on the PATH (e.g. micromamba create -n build -c conda-forge rattler-build
# constructor). Usage, from the CycloDesk folder:
#   Rscript installer/build_installer.R --waverider=../WaverideR [--waverider-sha=<commit>] [--cyclodesk-sha=<commit>]
#       [--out=installer/dist] [--r-base=4.5]
# With a commit (sha) the installed packages are marked as GitHub installs, so that "Update and restart" in
# CycloDesk can update them; without it they count as development installs (no update check).

# Arguments ####
arg <- function(name, default = NULL) {
  a <- commandArgs(trailingOnly = TRUE)
  hit <- grep(paste0("^--", name, "="), a, value = TRUE)
  if (!length(hit)) {
    return(default)
  }
  sub(paste0("^--", name, "="), "", hit[1])
}
script <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1])
inst_dir <- normalizePath(dirname(script))
root <- dirname(inst_dir)
wr_dir <- arg("waverider")
if (is.null(wr_dir) || !file.exists(file.path(wr_dir, "DESCRIPTION"))) {
  stop("Give the WaverideR source folder: --waverider=<folder>", call. = FALSE)
}
wr_dir <- normalizePath(wr_dir)
wr_sha <- arg("waverider-sha", "")
cd_sha <- arg("cyclodesk-sha", "")
out_dir <- arg("out", file.path(inst_dir, "dist"))
r_base <- arg("r-base", "4.5")
cran <- "https://cloud.r-project.org"
version <- as.character(read.dcf(file.path(root, "DESCRIPTION"), fields = "Version")[1, 1])

sysname <- Sys.info()[["sysname"]]
platform <- if (.Platform$OS.type == "windows") { "win" } else if (sysname == "Darwin") { "osx" } else { "linux" }
arch <- if (platform == "osx" && grepl("arm64|aarch64", R.version$arch)) { "arm64" } else { "x86_64" }
if (platform == "osx") {
  # an x86_64 R running under Rosetta on an arm64 Mac would build the wrong installer
  m <- suppressWarnings(system2("uname", "-m", stdout = TRUE))
  if (length(m) && m[1] == "arm64") {
    arch <- "arm64"
  }
}
say <- function(...) {
  cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), paste0(...)))
}
run <- function(cmd, args, env = character()) {
  say(cmd, " ", paste(args, collapse = " "))
  res <- system2(cmd, args, env = env)
  if (!identical(as.integer(res), 0L)) {
    stop(sprintf("'%s' failed (exit status %s)", cmd, res), call. = FALSE)
  }
  invisible(res)
}
rcmd <- file.path(R.home("bin"), if (platform == "win") { "R.exe" } else { "R" })

work <- file.path(inst_dir, "build")
channel <- file.path(work, "channel")
# --reuse-channel=true skips steps 1 and 2 when the conda package was built before
reuse <- identical(arg("reuse-channel"), "true") && dir.exists(channel)
if (!reuse) {
  unlink(work, recursive = TRUE)
  recipe <- file.path(work, "recipe")
  src <- file.path(recipe, "src")
  dir.create(src, recursive = TRUE)
  file.copy(list.files(file.path(inst_dir, "recipe"), full.names = TRUE), recipe, recursive = TRUE)

  # 0. Window program ####
  # CycloDesk's own window (installer/shell, Rust/Tauri): starts the installed R and shows the app; needs cargo
  target <- file.path(work, "shell-target")
  run("cargo", c("build", "--release", "--manifest-path", shQuote(file.path(inst_dir, "shell", "Cargo.toml")),
      "--target-dir", shQuote(target)))
  bin_dir <- file.path(recipe, "shell_bin")
  dir.create(bin_dir)
  exe <- file.path(target, "release", if (platform == "win") { "cyclodesk.exe" } else { "cyclodesk" })
  if (platform == "win") {
    file.copy(exe, file.path(bin_dir, "CycloDesk.exe"))
  } else if (platform == "linux") {
    file.copy(exe, file.path(bin_dir, "cyclodesk-window"), copy.mode = TRUE)
  } else {
    app <- file.path(bin_dir, "CycloDesk.app", "Contents")
    dir.create(file.path(app, "MacOS"), recursive = TRUE)
    dir.create(file.path(app, "Resources"))
    file.copy(exe, file.path(app, "MacOS", "CycloDesk"), copy.mode = TRUE)
    file.copy(file.path(inst_dir, "shell", "icons", "icon.icns"), file.path(app, "Resources", "icon.icns"))
    plist <- c("<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
        "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">",
        "<plist version=\"1.0\"><dict>",
        "<key>CFBundleDevelopmentRegion</key><string>en</string>",
        "<key>CFBundleExecutable</key><string>CycloDesk</string>",
        "<key>CFBundleIconFile</key><string>icon.icns</string>",
        "<key>CFBundleIdentifier</key><string>eu.stratigraphy.cyclodesk</string>",
        "<key>CFBundleName</key><string>CycloDesk</string>",
        "<key>CFBundleDisplayName</key><string>CycloDesk</string>",
        "<key>CFBundlePackageType</key><string>APPL</string>",
        sprintf("<key>CFBundleShortVersionString</key><string>%s</string>", version),
        sprintf("<key>CFBundleVersion</key><string>%s</string>", version),
        "<key>LSMinimumSystemVersion</key><string>11.0</string>",
        "<key>NSHighResolutionCapable</key><true/>",
        "</dict></plist>")
    writeLines(plist, file.path(app, "Info.plist"))
    # ad-hoc signature (Apple silicon runs only signed code; a developer signature can replace it later)
    run("codesign", c("--force", "--deep", "--sign", "-", shQuote(file.path(bin_dir, "CycloDesk.app"))))
  }

  # 1. Sources ####
  # A copy of a source folder, marked as a GitHub install when the commit is known, built into a tarball
  build_tarball <- function(dir, repo, sha) {
    tmp <- file.path(work, "tmp", basename(repo))
    dir.create(tmp, recursive = TRUE)
    files <- list.files(dir, all.files = TRUE, no.. = TRUE)
    files <- files[!files %in% c(".git", "installer", ".Rproj.user", "docs")]
    file.copy(file.path(dir, files), tmp, recursive = TRUE)
    if (nzchar(sha)) {
      d <- read.dcf(file.path(tmp, "DESCRIPTION"))
      extra <- c(RemoteType = "github", RemoteHost = "api.github.com", RemoteUsername = dirname(repo),
          RemoteRepo = basename(repo), RemoteRef = "HEAD", RemoteSha = sha, GithubRepo = basename(repo),
          GithubUsername = dirname(repo), GithubRef = "HEAD", GithubSHA1 = sha)
      for (k in names(extra)) {
        if (k %in% colnames(d)) {
          d[1, k] <- extra[[k]]
        } else {
          d <- cbind(d, matrix(extra[[k]], 1, 1, dimnames = list(NULL, k)))
        }
      }
      write.dcf(d, file.path(tmp, "DESCRIPTION"), keep.white = colnames(d))
    }
    old <- setwd(src)
    on.exit(setwd(old))
    run(rcmd, c("CMD", "build", "--no-build-vignettes", "--no-manual", "--no-resave-data", shQuote(tmp)))
    pkg <- as.character(read.dcf(file.path(tmp, "DESCRIPTION"), fields = c("Package", "Version"))[1, ])
    paste0(pkg[1], "_", pkg[2], ".tar.gz")
  }
  say("CycloDesk ", version, " for ", platform, "-", arch)
  wr_tar <- build_tarball(wr_dir, "stratigraphy/WaverideR", wr_sha)
  cd_tar <- build_tarball(root, "stratigraphy/CycloDesk", cd_sha)
  # multitaper, spam and fields (Fortran) are built here: conda-forge lacks multitaper for R 4.5 on Apple silicon, and
  # its Windows builds of spam and fields contain 32-bit pseudo relocations that crash R when Windows loads the DLLs
  # far apart in memory ("Mingw-w64 runtime failure: 32 bit pseudo relocation ... out of range")
  cran_pkgs <- c("multitaper", "spam", "fields", "palinsol", "astrochron", "lomb", "StratigrapheR", "DecomposeR")
  got <- utils::download.packages(cran_pkgs, destdir = src, repos = cran, type = "source")
  cran_tar <- basename(got[match(cran_pkgs, got[, 1]), 2])
  if (anyNA(cran_tar)) {
    stop("Could not download from CRAN: ", paste(cran_pkgs[is.na(cran_tar)], collapse = ", "), call. = FALSE)
  }
  writeLines(c(cran_tar, wr_tar, cd_tar), file.path(src, "install_order.txt"))
  unlink(file.path(work, "tmp"), recursive = TRUE)

  # 2. Conda package ####
  # conda-forge's compiler settings, with R fixed to one version
  pin <- file.path(work, "conda_build_config.yaml")
  utils::download.file("https://raw.githubusercontent.com/conda-forge/conda-forge-pinning-feedstock/main/recipe/conda_build_config.yaml",
      pin, quiet = TRUE, mode = "wb")
  variants <- file.path(work, "variants.yaml")
  writeLines(c("r_base:", sprintf("  - \"%s\"", r_base)), variants)
  # system2(env = ) does not work on Windows
  Sys.setenv(CYCLODESK_VERSION = version)
  run("rattler-build", c("build", "--recipe", shQuote(file.path(recipe, "recipe.yaml")), "--output-dir", shQuote(channel),
      "-m", shQuote(pin), "-m", shQuote(variants)))

}
# 3. Installer ####
cons <- file.path(work, "construct")
unlink(cons, recursive = TRUE)
dir.create(cons)
file.copy(list.files(file.path(inst_dir, "construct"), full.names = TRUE), cons)
# the installers accept only ASCII licence text
lic <- readLines(file.path(root, "LICENSE.md"), encoding = "UTF-8", warn = FALSE)
lic <- iconv(lic, "UTF-8", "ASCII//TRANSLIT", sub = "")
writeLines(lic, file.path(cons, "LICENSE.txt"))
file.copy(file.path(inst_dir, "recipe", "menu", "cyclodesk.png"), file.path(cons, "cyclodesk.png"))
type <- c(win = "exe", osx = "pkg", linux = "sh")[[platform]]
os_label <- c(win = "Windows", osx = "MacOSX", linux = "Linux")[[platform]]
filename <- sprintf("CycloDesk-%s-%s-%s.%s", version, os_label, arch, type)
extra <- switch(platform,
    win = c("default_prefix: \"%LOCALAPPDATA%\\\\CycloDesk\"", "default_prefix_domain_user: \"%LOCALAPPDATA%\\\\CycloDesk\"",
        "default_prefix_all_users: \"%ProgramData%\\\\CycloDesk\"", "icon_image: cyclodesk.png",
        "uninstall_name: CycloDesk ${VERSION}"),
    osx = c("pkg_name: CycloDesk", "default_location_pkg: Library", "progress_notifications: true",
        "welcome_text: |", "  This installs CycloDesk with its own copy of R. Afterwards, start CycloDesk from",
        "  Applications or Launchpad."),
    linux = c("default_prefix: ${HOME}/CycloDesk", "post_install: post_install.sh"))
chan_url <- paste0("file://", if (platform == "win") { "/" } else { "" }, gsub("\\\\", "/", normalizePath(channel)))

# conda-forge's R depends on the compilers (gcc, clang, gfortran, binutils, sysroot: about 250 MB) for R CMD INSTALL
# of compiled packages. CycloDesk installs and updates only pure R packages, so they are left out of the installer,
# as is pandoc (only for rendering R Markdown).
solved <- suppressWarnings(system2("conda", c("create", "--dry-run", "--json", "-n", "cyclodesk_solve", "--override-channels",
    "-c", chan_url, "-c", "conda-forge", shQuote(paste0("r-base=", r_base)), shQuote(paste0("cyclodesk=", version))),
    stdout = TRUE, stderr = FALSE))
pk_names <- unique(sub("^\"name\": *\"([^\"]+)\"$", "\\1",
    regmatches(paste(solved, collapse = ""), gregexpr("\"name\": *\"[^\"]+\"", paste(solved, collapse = "")))[[1]]))
if (!"cyclodesk" %in% pk_names) {
  stop("conda could not solve the environment:\n", paste(utils::tail(solved, 30), collapse = "\n"), call. = FALSE)
}
drop_re <- paste0("^((gcc|gxx|gfortran|clang|clangxx|ld64|cctools|compiler-rt|binutils|sysroot|kernel-headers|",
    "libstdcxx-devel|libgcc-devel|libcxx-devel|llvm-tools|libclang-cpp[0-9.]*|libllvm[0-9.]*|ld_impl)([_-].*)?|",
    "libsanitizer|pandoc|tapi|m2w64-(gcc|gcc-fortran|gcc-ada|gcc-objc|binutils|make|pkg-config)|",
    "ucrt64-(gcc|gcc-fortran|binutils|make))$")
keep_re <- "(-libs|-libs-core)$|^(libgcc|libgcc-ng|libgfortran[0-9]*|libstdcxx|libstdcxx-ng|libgomp|libcxx)$"
exclude <- pk_names[grepl(drop_re, pk_names) & !grepl(keep_re, pk_names)]
say("Left out of the installer: ", paste(exclude, collapse = ", "))
if (length(exclude)) {
  extra <- c(extra, "exclude:", paste0("  - ", exclude))
}
tpl <- readLines(file.path(cons, "construct.yaml.in"))
tpl <- gsub("@VERSION@", version, tpl, fixed = TRUE)
tpl <- gsub("@TYPE@", type, tpl, fixed = TRUE)
tpl <- gsub("@FILENAME@", filename, tpl, fixed = TRUE)
tpl <- gsub("@CHANNEL@", chan_url, tpl, fixed = TRUE)
tpl <- gsub("@RBASE@", paste0(r_base, ".*"), tpl, fixed = TRUE)
k <- grep("@EXTRA@", tpl, fixed = TRUE)
tpl <- c(tpl[seq_len(k - 1)], extra, tpl[-seq_len(k)])
writeLines(tpl, file.path(cons, "construct.yaml"))
unlink(file.path(cons, "construct.yaml.in"))
if (platform != "linux") {
  unlink(file.path(cons, "post_install.sh"))
}
unlink(file.path(cons, "win_replace_earlier.nsi"))

# Windows: replace an earlier CycloDesk in the chosen folder ####
# constructor's installer refuses a folder that is not empty, so a new version could only be installed after
# uninstalling the old one and starting the installer again. The template of constructor is copied with the lines
# of construct/win_replace_earlier.nsi inserted at the start of OnDirectoryLeave (the check of the folder): when the
# folder holds an earlier CycloDesk, the installer asks once, runs its uninstaller and goes on.
if (platform == "win") {
  py <- tempfile(fileext = ".py")
  writeLines(c("import os, constructor", "print(os.path.join(os.path.dirname(constructor.__file__), 'nsis', 'main.nsi.tmpl'))"),
      py)
  tmpl_file <- suppressWarnings(system2("python", shQuote(py), stdout = TRUE))
  tmpl_file <- tmpl_file[length(tmpl_file)]
  if (!length(tmpl_file) || !file.exists(tmpl_file)) {
    stop("constructor's Windows installer template (main.nsi.tmpl) was not found", call. = FALSE)
  }
  tmpl <- readLines(tmpl_file, encoding = "UTF-8", warn = FALSE)
  k <- grep("^Function OnDirectoryLeave\\s*$", tmpl)
  if (length(k) != 1) {
    stop("constructor's template has no single 'Function OnDirectoryLeave'; check construct/win_replace_earlier.nsi",
        " against the new constructor version", call. = FALSE)
  }
  ins <- readLines(file.path(inst_dir, "construct", "win_replace_earlier.nsi"), warn = FALSE)
  tmpl <- c(tmpl[seq_len(k)], ins, tmpl[seq(k + 1, length(tmpl))])
  writeLines(tmpl, file.path(cons, "main_cyclodesk.nsi.tmpl"), useBytes = TRUE)
  say("Installer template: ", tmpl_file, " with construct/win_replace_earlier.nsi")
  k <- grep("^exclude:", tpl)
  if (!length(k)) {
    k <- length(tpl) + 1
  }
  tpl <- c(tpl[seq_len(k - 1)], "nsis_template: main_cyclodesk.nsi.tmpl", tpl[seq(k, length.out = length(tpl) - k + 1)])
  writeLines(tpl, file.path(cons, "construct.yaml"))
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
# as "python -m constructor": on Windows R starts programs by their short (8.3) name, and the constructor.exe launcher
# then looks for CONSTR~1-script.py
run("python", c("-m", "constructor", shQuote(cons), "--output-dir", shQuote(normalizePath(out_dir))))
say("Installer: ", file.path(normalizePath(out_dir), filename))
