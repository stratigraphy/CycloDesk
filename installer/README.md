# CycloDesk installers

One installer per system, each with its own R from conda-forge and all packages:

| System | File | Installs into |
|---|---|---|
| Windows | `CycloDesk-<version>-Windows-x86_64.exe` (NSIS) | `%LOCALAPPDATA%\CycloDesk`; Start menu and desktop icon; uninstall in Settings > Apps |
| macOS | `CycloDesk-<version>-MacOSX-<arm64/x86_64>.pkg` | `~/Library/CycloDesk`; CycloDesk.app in Applications |
| Linux | `CycloDesk-<version>-Linux-x86_64.sh` | `~/CycloDesk`; menu entry and desktop icon |

A Windows installer replaces an earlier CycloDesk in the chosen folder after asking once: `build_installer.R`
inserts `construct/win_replace_earlier.nsi` into constructor's NSIS template (`nsis_template`), at the start of the
folder check, because constructor itself refuses a folder that is not empty. The build stops with a message if a
new constructor version no longer has that check (`Function OnDirectoryLeave`). The macOS and Linux installers of
constructor still refuse an existing folder (the Linux one accepts `-u` to update).

## How they are built

`build_installer.R` builds the installer for the system it runs on:

1. collects the R source packages that are not on conda-forge (CycloDesk and WaverideR from source folders;
   astrochron, palinsol, lomb, StratigrapheR and DecomposeR from CRAN);
2. builds the conda package `cyclodesk` from `recipe/` with rattler-build (it installs those packages, with the
   start and update scripts and the icon definition in `recipe/menu/`);
3. builds the installer from `construct/` with conda's constructor, leaving out the compilers that conda-forge's R
   depends on (CycloDesk only installs pure R packages).

On GitHub, the workflow `.github/workflows/installers.yaml` does this on Windows, macOS (Apple silicon and Intel)
and Linux, installs each installer on a clean machine, runs `smoke_test.R` with the installed R, and attaches the
installers to the release (or keeps them as workflow artifacts when started by hand).

Locally (Linux or macOS shown; on Windows the same in a Miniforge prompt):

```sh
micromamba create -n build -c conda-forge r-base=4.5 rattler-build constructor conda   # plus Rust from rustup.rs
micromamba activate build
Rscript installer/build_installer.R --waverider=../WaverideR --out=installer/dist
```

`--waverider-sha` and `--cyclodesk-sha` (the commits) mark the packages as GitHub installs so that "Update and
restart" can update them; `--reuse-channel=true` skips steps 1-2 when only the installer changes.

## The window program (shell/)

CycloDesk runs as a desktop program: `shell/` is a small Rust/Tauri program (about 5 MB) that starts the installed R
with `cyclodesk_start.R --port=<free port> --no-browser`, shows a start page while R loads, and then shows CycloDesk
in its own window (WebView2 on Windows, WKWebView on macOS, WebKitGTK 4.1 on Linux). Closing the window stops R; when
R stops (Close button, Update and restart) the window closes. Links to other sites open in the default browser.
Messages from R go to `cyclodesk_console.txt` in the temporary folder. `CYCLODESK_SELFTEST=1` makes it start,
check the page and quit (used by the GitHub test).

Installed as `CycloDesk.exe` in the installation folder (Windows), `CycloDesk.app` in it (macOS; the shortcut in
Applications opens it) or `bin/cyclodesk-window` with the launcher `bin/cyclodesk`, which falls back to the browser
when WebKitGTK is missing (Linux).

## Started from the icon

- `cyclodesk_start.R` opens the page when CycloDesk already runs, otherwise starts it on port 7792 and opens the
  default browser; messages go to `cyclodesk_log.txt` in `tools::R_user_dir("CycloDesk", "data")`.
- `cyclodesk_start.vbs` (Windows) puts the DLL folders of the installed R on the PATH and runs the R script without a
  console window.
- `cyclodesk_update.R` is started by "Update and restart": it waits until CycloDesk has closed, installs the newest
  CycloDesk/WaverideR from GitHub into the installed library (or a user library when that is not writable) and
  starts CycloDesk again.

## Not code-signed (yet)

Windows SmartScreen shows "Windows protected your PC" (More info > Run anyway) and macOS Gatekeeper asks for
confirmation (Control-click > Open). Signing needs a certificate: free for open-source projects through the SignPath
Foundation (Windows); an Apple Developer account (99 USD a year) for signing and notarising the macOS packages.
constructor supports both (`signing_certificate`, `signing_identity_name`, `notarization_identity_name`).
