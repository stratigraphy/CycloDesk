#!/bin/sh
# cyclodesk: starts CycloDesk in its own window (Linux). When the window program cannot run (WebKitGTK 4.1 is not
# installed), CycloDesk opens in the web browser instead.
d=$(cd "$(dirname "$0")/.." && pwd)
"$d/bin/cyclodesk-window" "$@"
s=$?
if [ "$s" -eq 126 ] || [ "$s" -eq 127 ]; then
  exec "$d/bin/Rscript" --no-environ --no-init-file "$d/Menu/cyclodesk_start.R"
fi
exit "$s"
