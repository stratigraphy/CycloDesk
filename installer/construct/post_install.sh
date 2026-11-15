#!/bin/bash
# Linux, after installation: repair the icon line of the menu entry (menuinst 2.5 writes the template text instead of
# the icon path) and put a copy of the entry on the desktop. macOS gets the app in Applications instead.
if [ "$(uname)" != "Linux" ]; then
  exit 0
fi
icon="$PREFIX/Menu/cyclodesk.png"
entry=""
for f in "$HOME"/.local/share/applications/*cyclodesk*.desktop /usr/share/applications/*cyclodesk*.desktop; do
  if [ -f "$f" ] && grep -q "$PREFIX" "$f"; then
    sed -i "s|^Icon=.*|Icon=$icon|" "$f" 2>/dev/null || true
    entry="$f"
  fi
done
desk=$(xdg-user-dir DESKTOP 2>/dev/null || true)
if [ -z "$desk" ]; then
  desk="$HOME/Desktop"
fi
if [ -n "$entry" ] && [ -d "$desk" ]; then
  cp "$entry" "$desk/CycloDesk.desktop"
  chmod +x "$desk/CycloDesk.desktop"
  gio set "$desk/CycloDesk.desktop" metadata::trusted true 2>/dev/null || true
fi
if [ -n "$entry" ] && command -v update-desktop-database >/dev/null 2>&1; then
  update-desktop-database "$(dirname "$entry")" >/dev/null 2>&1 || true
fi
exit 0
