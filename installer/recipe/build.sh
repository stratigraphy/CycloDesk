#!/bin/bash
# Installs the R source packages listed in install_order.txt (in that order), the menu files and the CycloDesk
# window program (built by build_installer.R into shell_bin/)
set -euo pipefail
LIB="$PREFIX/lib/R/library"
while read -r f; do
  [ -z "$f" ] && continue
  echo "Installing $f"
  "$PREFIX/bin/R" CMD INSTALL --no-test-load --library="$LIB" "$SRC_DIR/$f"
done < "$SRC_DIR/install_order.txt"
mkdir -p "$PREFIX/Menu"
cp "$RECIPE_DIR"/menu/cyclodesk* "$PREFIX/Menu/"
rm -f "$PREFIX/Menu/cyclodesk.sh"
if [ "$(uname)" = "Darwin" ]; then
  cp -R "$RECIPE_DIR/shell_bin/CycloDesk.app" "$PREFIX/CycloDesk.app"
else
  cp "$RECIPE_DIR/shell_bin/cyclodesk-window" "$PREFIX/bin/cyclodesk-window"
  cp "$RECIPE_DIR/menu/cyclodesk.sh" "$PREFIX/bin/cyclodesk"
  chmod +x "$PREFIX/bin/cyclodesk-window" "$PREFIX/bin/cyclodesk"
fi
