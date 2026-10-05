#!/bin/sh
# Render scripts/og-card.html to images/og-card.png (1200 x 630), the site's
# link-preview image. Uses headless Microsoft Edge with its own throwaway
# profile. Run from the repository root:
#   sh scripts/build_og_card.sh
EDGE="/c/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"
PROFILE="$(mktemp -d)"
"$EDGE" --headless=new --disable-gpu --hide-scrollbars --virtual-time-budget=3000 \
  --user-data-dir="$PROFILE" --window-size=1200,630 \
  --screenshot="$(pwd -W 2>/dev/null || pwd)/images/og-card.png" \
  "file:///$(pwd -W 2>/dev/null || pwd)/scripts/og-card.html"
