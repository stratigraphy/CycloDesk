// CycloDesk desktop window
//
// Starts CycloDesk's own R (installed next to this program by the CycloDesk installer) with the app on a free local
// port, shows a start page while R loads, then shows CycloDesk in this window. Closing the window stops R; when R
// stops (CycloDesk's Close button, "Update and restart"), the window closes. Links to other sites open in the
// default browser. Messages from R go to cyclodesk_console.txt in the temporary folder.
//
// Install layout (the "prefix" is the installation folder):
//   Windows  <prefix>\CycloDesk.exe              R: <prefix>\Lib\R\bin\x64\Rscript.exe
//   Linux    <prefix>/bin/cyclodesk-window        R: <prefix>/bin/Rscript
//   macOS    <prefix>/CycloDesk.app/Contents/MacOS/CycloDesk
// The start script is <prefix>/Menu/cyclodesk_start.R.

#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use std::io::{Read, Write};
use std::net::{SocketAddr, TcpListener, TcpStream};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};

use tauri::{Manager, RunEvent, Url, WebviewUrl, WebviewWindowBuilder};

type RChild = Arc<Mutex<Option<Child>>>;

// Links opened with target="_blank" or window.open go through a normal navigation, which on_navigation then sends
// to the default browser
const INIT_JS: &str = r#"
(function () {
  if (location.hostname !== '127.0.0.1') { return; }
  window.open = function (u) {
    if (u) { location.href = new URL(u, location.href).href; }
    return null;
  };
  document.addEventListener('click', function (e) {
    var a = e.target && e.target.closest ? e.target.closest('a[target="_blank"]') : null;
    if (a && a.href) { e.preventDefault(); location.href = a.href; }
  }, true);
})();
"#;

fn prefix_dir() -> PathBuf {
    if let Ok(p) = std::env::var("CYCLODESK_PREFIX") {
        return PathBuf::from(p);
    }
    let exe = std::env::current_exe().unwrap_or_default();
    let dir = exe.parent().map(Path::to_path_buf).unwrap_or_default();
    let name = dir.file_name().map(|s| s.to_string_lossy().to_string()).unwrap_or_default();
    if name == "MacOS" {
        // <prefix>/CycloDesk.app/Contents/MacOS
        if let Some(p) = dir.parent().and_then(Path::parent).and_then(Path::parent) {
            return p.to_path_buf();
        }
    }
    if name == "bin" {
        if let Some(p) = dir.parent() {
            return p.to_path_buf();
        }
    }
    dir
}

fn free_port() -> u16 {
    TcpListener::bind("127.0.0.1:0")
        .ok()
        .and_then(|l| l.local_addr().ok())
        .map(|a| a.port())
        .unwrap_or(7792)
}

// true when the app answers with HTTP 200
fn app_ready(port: u16) -> bool {
    let addr: SocketAddr = match format!("127.0.0.1:{port}").parse() {
        Ok(a) => a,
        Err(_) => return false,
    };
    let mut s = match TcpStream::connect_timeout(&addr, Duration::from_millis(500)) {
        Ok(s) => s,
        Err(_) => return false,
    };
    let _ = s.set_read_timeout(Some(Duration::from_secs(3)));
    if s.write_all(b"GET / HTTP/1.0\r\nHost: 127.0.0.1\r\n\r\n").is_err() {
        return false;
    }
    let mut buf = [0u8; 32];
    match s.read(&mut buf) {
        Ok(n) if n >= 12 => {
            let head = String::from_utf8_lossy(&buf[..n]).to_string();
            head.starts_with("HTTP/1.1 200") || head.starts_with("HTTP/1.0 200")
        }
        _ => false,
    }
}

fn log_path() -> PathBuf {
    std::env::temp_dir().join("cyclodesk_console.txt")
}

fn start_r(prefix: &Path, port: u16) -> std::io::Result<Child> {
    let menu = prefix.join("Menu");
    #[cfg(windows)]
    let rscript = {
        let a = prefix.join("Lib").join("R").join("bin").join("x64").join("Rscript.exe");
        if a.exists() {
            a
        } else {
            prefix.join("Scripts").join("Rscript.exe")
        }
    };
    #[cfg(not(windows))]
    let rscript = prefix.join("bin").join("Rscript");
    let mut cmd = Command::new(&rscript);
    cmd.arg("--no-environ")
        .arg("--no-init-file")
        .arg(menu.join("cyclodesk_start.R"))
        .arg(format!("--port={port}"))
        .arg("--no-browser");
    // settings of another R on this computer must not reach this one
    for v in [
        "R_HOME", "R_LIBS", "R_LIBS_SITE", "R_PROFILE", "R_ENVIRON", "R_ARCH", "R_SHARE_DIR", "R_INCLUDE_DIR",
        "R_DOC_DIR",
    ] {
        cmd.env_remove(v);
    }
    cmd.env("R_LIBS_USER", prefix.join("no-user-library"))
        .env("R_ENVIRON_USER", menu.join("none.Renviron"))
        .env("R_PROFILE_USER", menu.join("none.Rprofile"))
        .env("CYCLODESK_PORT", port.to_string());
    if let Ok(exe) = std::env::current_exe() {
        cmd.env("CYCLODESK_SHELL", exe);
    }
    #[cfg(windows)]
    {
        use std::os::windows::process::CommandExt;
        let mut paths: Vec<PathBuf> = vec![
            prefix.to_path_buf(),
            prefix.join("Library").join("mingw-w64").join("bin"),
            prefix.join("Library").join("usr").join("bin"),
            prefix.join("Library").join("bin"),
            prefix.join("Scripts"),
            prefix.join("bin"),
        ];
        if let Some(old) = std::env::var_os("PATH") {
            paths.extend(std::env::split_paths(&old));
        }
        if let Ok(p) = std::env::join_paths(paths) {
            cmd.env("PATH", p);
        }
        cmd.env("CONDA_PREFIX", prefix);
        // no console window
        cmd.creation_flags(0x0800_0000);
    }
    let f = std::fs::File::create(log_path())?;
    cmd.stdin(Stdio::null()).stdout(Stdio::from(f.try_clone()?)).stderr(Stdio::from(f));
    cmd.spawn()
}

fn r_exited(child: &RChild) -> bool {
    match child.lock() {
        Ok(mut g) => match g.as_mut() {
            Some(c) => !matches!(c.try_wait(), Ok(None)),
            None => true,
        },
        Err(_) => true,
    }
}

// lets R stop by itself (the app stops when its window is gone), then ends it
fn stop_r(child: &RChild) {
    let taken = child.lock().ok().and_then(|mut g| g.take());
    if let Some(mut c) = taken {
        let t0 = Instant::now();
        while t0.elapsed() < Duration::from_secs(4) {
            if !matches!(c.try_wait(), Ok(None)) {
                return;
            }
            thread::sleep(Duration::from_millis(100));
        }
        let _ = c.kill();
        let _ = c.wait();
    }
}

fn tail_of_log(n: usize) -> String {
    let txt = std::fs::read_to_string(log_path()).unwrap_or_default();
    let lines: Vec<&str> = txt.lines().collect();
    let k = lines.len().saturating_sub(n);
    lines[k..].join("\n")
}

fn is_local(url: &Url) -> bool {
    matches!(url.scheme(), "tauri" | "about" | "data" | "blob")
        || matches!(url.host_str(), Some("127.0.0.1") | Some("localhost") | Some("tauri.localhost"))
}

fn main() {
    let selftest = std::env::var("CYCLODESK_SELFTEST").is_ok();
    let child: RChild = Arc::new(Mutex::new(None));
    let child_setup = child.clone();
    let child_exit = child.clone();

    let app = tauri::Builder::default()
        .plugin(tauri_plugin_single_instance::init(|app, _args, _cwd| {
            if let Some(w) = app.get_webview_window("main") {
                let _ = w.unminimize();
                let _ = w.set_focus();
            }
        }))
        .setup(move |app| {
            let prefix = prefix_dir();
            let port = free_port();
            let win = WebviewWindowBuilder::new(app, "main", WebviewUrl::App("index.html".into()))
                .title("CycloDesk")
                .inner_size(1440.0, 920.0)
                .min_inner_size(900.0, 600.0)
                .disable_drag_drop_handler()
                .initialization_script(INIT_JS)
                .on_navigation(|url| {
                    if is_local(url) {
                        true
                    } else {
                        let _ = open::that_detached(url.as_str());
                        false
                    }
                })
                .build()?;
            let handle = app.handle().clone();
            match start_r(&prefix, port) {
                Ok(c) => {
                    if let Ok(mut g) = child_setup.lock() {
                        *g = Some(c);
                    }
                }
                Err(e) => {
                    let msg = format!("R could not be started from {}:\n{}", prefix.display(), e);
                    let w = win.clone();
                    let msg_js = msg.clone();
                    thread::spawn(move || {
                        thread::sleep(Duration::from_millis(800));
                        let _ = w.eval(&format!("showError({})", serde_json::to_string(&msg_js).unwrap_or_default()));
                    });
                    if selftest {
                        eprintln!("selftest: {msg}");
                        std::process::exit(1);
                    }
                    return Ok(());
                }
            }
            let rc = child_setup.clone();
            thread::spawn(move || {
                let t0 = Instant::now();
                // wait for the app
                loop {
                    if app_ready(port) {
                        if let Ok(u) = Url::parse(&format!("http://127.0.0.1:{port}/")) {
                            let _ = win.navigate(u);
                        }
                        break;
                    }
                    if r_exited(&rc) || t0.elapsed() > Duration::from_secs(300) {
                        let msg = format!(
                            "{}\n\nFull messages: {}",
                            tail_of_log(25),
                            log_path().display()
                        );
                        let _ = win.eval(&format!("showError({})", serde_json::to_string(&msg).unwrap_or_default()));
                        if selftest {
                            eprintln!("selftest: R stopped before the app answered\n{msg}");
                            handle.exit(1);
                        }
                        return;
                    }
                    thread::sleep(Duration::from_millis(300));
                }
                if selftest {
                    thread::sleep(Duration::from_secs(8));
                    let ok = !r_exited(&rc) && app_ready(port);
                    eprintln!("selftest: {}", if ok { "ok" } else { "R stopped after the page was shown" });
                    handle.exit(if ok { 0 } else { 1 });
                    return;
                }
                // R stops: close the window
                loop {
                    thread::sleep(Duration::from_millis(500));
                    if r_exited(&rc) {
                        handle.exit(0);
                        return;
                    }
                }
            });
            Ok(())
        })
        .build(tauri::generate_context!())
        .expect("CycloDesk: the window could not be created");

    app.run(move |_handle, event| {
        if let RunEvent::Exit = event {
            stop_r(&child_exit);
        }
    });
}
