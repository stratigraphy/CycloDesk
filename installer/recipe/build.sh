#!/bin/bash
# Installs the R source packages listed in install_order.txt (in that order) and the menu files
set -euo pipefail
LIB="$PREFIX/lib/R/library"
while read -r f; do
  [ -z "$f" ] && continue
  echo "Installing $f"
  "$PREFIX/bin/R" CMD INSTALL --no-test-load --library="$LIB" "$SRC_DIR/$f"
done < "$SRC_DIR/install_order.txt"
mkdir -p "$PREFIX/Menu"
cp "$RECIPE_DIR"/menu/* "$PREFIX/Menu/"
