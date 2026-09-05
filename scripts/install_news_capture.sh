#!/bin/zsh
# Install (or reinstall) the news-capture launchd job on THIS machine.
# Run from anywhere inside the repo, on the machine that should capture
# (production = the MacMini; the laptop works the same way for staging):
#
#     ./scripts/install_news_capture.sh
#
# What it does: patches REPO_PATH into the plist template, installs it
# to ~/Library/LaunchAgents, and (re)loads it. RunAtLoad fires one
# capture immediately, then every 4 hours. Idempotent -- safe to re-run
# after a git pull updates the capture script (launchd re-reads the
# script on every firing, so a reinstall is only needed if the plist
# itself changed).
#
# Verify:    launchctl list | grep newscapture
#            tail ~/boxscore-news/logs/capture.log   (OK line within ~1 min)
# Health:    ~/boxscore-news/ALERT_news_capture exists = 3+ consecutive
#            failures (check in the Tuesday cadence).
# Uninstall: launchctl unload ~/Library/LaunchAgents/com.boxscoreprophet.newscapture.plist
set -e

REPO="$(cd "$(dirname "$0")/.." && pwd)"
LABEL="com.boxscoreprophet.newscapture"
TEMPLATE="$REPO/scripts/${LABEL}.plist"
TARGET="$HOME/Library/LaunchAgents/${LABEL}.plist"

if ! command -v Rscript > /dev/null; then
  echo "ERROR: Rscript not on PATH -- install R first." >&2
  exit 1
fi

mkdir -p "$HOME/Library/LaunchAgents"
sed "s|REPO_PATH|${REPO}|" "$TEMPLATE" > "$TARGET"

# Reload cleanly if a previous version is running.
launchctl unload "$TARGET" 2> /dev/null || true
launchctl load "$TARGET"

echo "Loaded ${LABEL} (repo: ${REPO})"
echo "First capture fires now; check: tail ~/boxscore-news/logs/capture.log"
