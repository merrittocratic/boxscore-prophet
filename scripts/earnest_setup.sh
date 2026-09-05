#!/usr/bin/env bash
# scripts/earnest_setup.sh -- MacMini production preflight + crontab arming.
# Run ON THE MACMINI from the repo root (or any path; it cds itself).
#
#   bash scripts/earnest_setup.sh          # preflight only (default, safe)
#   bash scripts/earnest_setup.sh --arm    # preflight, then install crontab
#
# Preflight checks everything the cadence needs; --arm refuses unless all
# checks pass. Arming is idempotent: the crontab block is marked and
# replaced wholesale on re-run. Per README, arm during the early-September
# pass -- an August arm would publish W1 boards built on NA Vegas lines
# and incomplete rookie ids.

set -uo pipefail
cd "$(dirname "$0")/.."
REPO="$(pwd)"
FAILURES=0

ok()   { echo "  [ok]   $1"; }
bad()  { echo "  [FAIL] $1"; FAILURES=$((FAILURES + 1)); }
warn() { echo "  [warn] $1"; }

echo "== Earnest preflight: $REPO =="

# --- git ---------------------------------------------------------------
[ -z "$(git status --porcelain)" ] && ok "working tree clean" || bad "working tree dirty -- resolve before arming"
git pull --ff-only >/dev/null 2>&1 && ok "ff-only pull works" || bad "cannot ff-only pull from origin"
git push --dry-run origin main >/dev/null 2>&1 && ok "push access to origin/main" || bad "no push access (credential helper?)"
[ -n "$(git config user.name)" ] && ok "git identity: $(git config user.name)" || bad "git user.name unset"

# --- R + packages (derived from the cadence scripts, not a stale list) --
if command -v Rscript >/dev/null; then
  ok "Rscript: $(Rscript --version 2>&1 | head -1)"
  PKGS=$(grep -h "^  library(" \
      R/build_rb_feature_layer.R R/04a_wr_feature_layer.R \
      R/12a_te_feature_layer.R R/08a_qb_feature_layer.R \
      R/11b_injury_state_layer.R R/10a_deployment_models.R \
      R/10b_weekly_slate.R R/10b2_player_slate.R R/10c_weekly_score.R \
      R/10d0_ecr_fetch.R R/10d_content_tables.R 2>/dev/null |
    sed 's/.*library(\([a-zA-Z0-9.]*\)).*/\1/' | sort -u)
  MISSING=$(Rscript -e 'a <- commandArgs(TRUE); m <- a[!sapply(a, requireNamespace, quietly = TRUE)]; cat(m, sep = ",")' $PKGS)
  [ -z "$MISSING" ] && ok "R packages present: $(echo $PKGS | tr '\n' ' ')" || bad "missing R packages: $MISSING"
else
  bad "Rscript not on PATH"
fi

# --- credentials (exit status only; never echo secrets) -----------------
if security find-generic-password -s fantasypros-api-key -w >/dev/null 2>&1; then
  ok "keychain: fantasypros-api-key present"
else
  warn "keychain: fantasypros-api-key MISSING -- 10d0 will skip ECR (non-fatal by design)"
fi

# --- deploy artifacts ---------------------------------------------------
N_MODELS=$(ls data/deploy_models/*.txt 2>/dev/null | wc -l | tr -d ' ')
[ "$N_MODELS" -ge 10 ] && ok "deploy models: $N_MODELS files" || bad "deploy models incomplete ($N_MODELS files; expect >= 10)"
[ -f data/deployment_params.rds ] && ok "deployment_params.rds present" || bad "deployment_params.rds missing"
[ -f data/vegas_open_lines.rds ] && ok "vegas opener sidecar present" || bad "vegas_open_lines.rds missing"

# --- OpenClaw delivery --------------------------------------------------
if command -v openclaw >/dev/null; then
  ok "openclaw CLI present"
else
  warn "openclaw CLI missing -- Telegram summary/media delivery disabled"
fi
if [ -f scripts/earnest_delivery.env ]; then
  # shellcheck disable=SC1091
  source scripts/earnest_delivery.env
  if [ -n "${OPENCLAW_DELIVERY_TARGET:-}" ]; then
    ok "delivery target configured for ${OPENCLAW_DELIVERY_CHANNEL:-telegram}:${OPENCLAW_DELIVERY_TARGET}"
  else
    warn "scripts/earnest_delivery.env present but OPENCLAW_DELIVERY_TARGET is empty"
  fi
else
  warn "scripts/earnest_delivery.env missing -- summary + media group delivery disabled"
fi

# --- environment --------------------------------------------------------
TZN=$(date +%Z)
case "$TZN" in EST|EDT) ok "system timezone: $TZN (crontab times are ET)";;
  *) bad "system timezone is $TZN -- crontab lines assume ET; adjust times or TZ";;
esac
AVAIL_GB=$(df -g . | awk 'NR==2 {print $4}')
[ "$AVAIL_GB" -ge 5 ] && ok "disk: ${AVAIL_GB}GB free" || warn "disk: only ${AVAIL_GB}GB free"

echo
if [ "$FAILURES" -gt 0 ]; then
  echo "== PREFLIGHT FAILED ($FAILURES) -- fix before arming =="
  exit 1
fi
echo "== preflight clean =="

# --- arm ----------------------------------------------------------------
# launchd, not cron (2026-09-06): the MacMini is launchd-only -- Earnest
# confirmed com.vix.cron is not registered, and every other periodic job
# on the machine (including the news-capture agent) is a launchd plist.
# Same times as the old crontab block: full Tue 23:30, rescore Thu/Sat/
# Mon 15:00 + Sun 08:00 (all local ET, checked in preflight). RunAtLoad
# is deliberately absent -- arming must never fire a production run.
# Idempotent: plists are regenerated and reloaded wholesale on re-run.
# Any stale crontab block from the pre-launchd version is removed.
LA_DIR="${EARNEST_LAUNCH_AGENT_DIR:-$HOME/Library/LaunchAgents}"

write_cadence_plist() {  # $1 label-suffix  $2 mode  $3 calendar-xml
  cat > "$LA_DIR/com.boxscoreprophet.cadence.$1.plist" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
  "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>Label</key>
  <string>com.boxscoreprophet.cadence.$1</string>
  <key>ProgramArguments</key>
  <array>
    <string>/bin/bash</string>
    <string>$REPO/scripts/earnest_cron.sh</string>
    <string>$2</string>
  </array>
  <key>StartCalendarInterval</key>
  $3
  <key>StandardOutPath</key>
  <string>$REPO/logs/cron.log</string>
  <key>StandardErrorPath</key>
  <string>$REPO/logs/cron.log</string>
</dict>
</plist>
PLIST
}

if [ "${1:-}" = "--arm" ]; then
  mkdir -p "$LA_DIR"

  write_cadence_plist full full \
'<dict><key>Weekday</key><integer>2</integer><key>Hour</key><integer>23</integer><key>Minute</key><integer>30</integer></dict>'

  write_cadence_plist rescore rescore \
'<array>
    <dict><key>Weekday</key><integer>4</integer><key>Hour</key><integer>15</integer><key>Minute</key><integer>0</integer></dict>
    <dict><key>Weekday</key><integer>6</integer><key>Hour</key><integer>15</integer><key>Minute</key><integer>0</integer></dict>
    <dict><key>Weekday</key><integer>0</integer><key>Hour</key><integer>8</integer><key>Minute</key><integer>0</integer></dict>
    <dict><key>Weekday</key><integer>1</integer><key>Hour</key><integer>15</integer><key>Minute</key><integer>0</integer></dict>
  </array>'

  for j in full rescore; do
    plutil -lint "$LA_DIR/com.boxscoreprophet.cadence.$j.plist" >/dev/null ||
      { echo "== ARM FAILED: generated plist $j does not lint =="; exit 1; }
    launchctl unload "$LA_DIR/com.boxscoreprophet.cadence.$j.plist" 2>/dev/null || true
    launchctl load "$LA_DIR/com.boxscoreprophet.cadence.$j.plist"
  done

  # retire any crontab block left by the pre-launchd arming path
  if crontab -l >/dev/null 2>&1; then
    crontab -l | sed "/^# BEGIN boxscore-prophet cadence/,/^# END boxscore-prophet cadence/d" | crontab -
  fi

  echo "== ARMED: launchd agents installed =="
  launchctl list | grep boxscoreprophet || true
else
  echo "(not armed -- run with --arm during the September pass)"
fi
