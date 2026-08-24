#!/usr/bin/env bash
# scripts/draft_content.sh -- fire a content-drafting skill headlessly via
# Claude Code (NOT OpenClaw/Earnest), separate from earnest_cron.sh so a
# failure here can never block or corrupt the real scoring pipeline.
#
# Design constraints from the 2026-08-24 automation test (see HANDOFF.md
# "Content-drafting automation (Option C)" and the earnest-content-automation
# memory) -- every one of these exists because of something that actually
# went wrong or was flagged as a real risk during that test, not by default
# caution:
#   - Never inline in earnest_cron.sh: a stuck/failed draft attempt must not
#     touch the scoring/commit/push pipeline at all.
#   - Hard timeout on the headless call: no TTY exists to rescue a hang.
#   - Zero writes inside this repo: a stray file here would trip Earnest's
#     own clean-tree guard and silently block its NEXT real run. This script
#     logs OUTSIDE the repo entirely (~/logs/draft-content/) for that reason,
#     and verifies the tree is still clean after the skill runs.
#   - Fire on evidence Earnest's run actually completed (a fresh commit),
#     not a guessed time offset after its known cron slot.
#   - Every outcome gets a Telegram notification, including a correct
#     decline -- the whole point of building this was that a decline
#     silently reaching nobody is indistinguishable from a forgotten week.
#
# Usage: draft_content.sh on-the-record|movers-column

set -uo pipefail
# Deliberately NOT `set -e`: a failed or declined draft is a reportable
# outcome we want to notify on, not a reason to crash before reaching the
# notify step.

# Homebrew's bin dir (Apple Silicon default) isn't on a non-interactive
# bash subshell's PATH even though it's on the interactive zsh login
# shell's -- found the hard way when `claude` resolved fine by hand but
# exited 127 (command not found) under this script. Prepending here means
# it doesn't matter what shell or context invokes this (manual test, or
# eventually cron, which has an even sparser PATH than this).
export PATH="/opt/homebrew/bin:$PATH"
cd "$(dirname "$0")/.."

SKILL="${1:?usage: draft_content.sh on-the-record|movers-column}"
case "$SKILL" in
  on-the-record|movers-column) ;;
  *) echo "[draft_content] unknown skill: $SKILL (expected on-the-record|movers-column)" >&2; exit 1 ;;
esac

TIMEOUT_SECS="${DRAFT_CONTENT_TIMEOUT_SECS:-600}"
MAX_COMMIT_AGE_SECS="${DRAFT_CONTENT_MAX_COMMIT_AGE_SECS:-1800}"
CONTENT_DRAFT_DIR="${DRAFT_CONTENT_TARGET_DIR:-$HOME/content/draft}"
LOG_DIR="$HOME/logs/draft-content"
mkdir -p "$LOG_DIR"
STAMP=$(date +%Y%m%d_%H%M)
LOG_PATH="$LOG_DIR/${SKILL}_${STAMP}.log"

notify() {
  local msg="$1"
  local env_file="${EARNEST_DELIVERY_ENV_FILE:-scripts/earnest_delivery.env}"
  [ -f "$env_file" ] && source "$env_file"
  local channel="${OPENCLAW_DELIVERY_CHANNEL:-telegram}"
  local target="${OPENCLAW_DELIVERY_TARGET:-}"
  local account="${OPENCLAW_DELIVERY_ACCOUNT:-default}"
  if [ -z "$target" ] || ! command -v openclaw >/dev/null 2>&1; then
    echo "[draft_content] notify skipped (no delivery target / openclaw CLI): $msg" >&2
    return 0
  fi
  openclaw message send --account "$account" --channel "$channel" --target "$target" --message "$msg" \
    || echo "[draft_content] notify call failed, message was: $msg" >&2
}

log() { echo "[draft_content] $1" | tee -a "$LOG_PATH" >&2; }

# macOS ships no `timeout` (that's GNU coreutils; Homebrew installs it as
# `gtimeout` to avoid clashing with anything, and it may not be installed
# at all) -- found the hard way when the real `timeout` call failed with
# "command not found" on the Mac Mini. Implemented in bash instead so this
# never depends on what happens to be installed: a watchdog subshell kills
# the job if it outlives the deadline; mimics `timeout`'s own convention of
# returning 124 on a timeout so the rest of this script doesn't need to care
# which one ran.
run_with_timeout() {
  local secs="$1"; shift
  "$@" &
  local pid=$!
  ( sleep "$secs"
    if kill -0 "$pid" 2>/dev/null; then
      kill -TERM "$pid" 2>/dev/null
      sleep 2
      kill -KILL "$pid" 2>/dev/null
    fi
  ) &
  local watchdog=$!
  local status=0
  wait "$pid" || status=$?
  kill "$watchdog" 2>/dev/null
  wait "$watchdog" 2>/dev/null || true
  if [ "$status" -ge 128 ]; then
    return 124
  fi
  return "$status"
}

# ---------------------------------------------------------------------------
# 0. Preflight: the permission grant this depends on has to already exist.
#    Missing it doesn't fail loudly inside Claude -- it fails as a silent
#    write-block deep in a headless session. Catch it here instead, since we
#    already burned a long debugging session finding this exact failure mode.
# ---------------------------------------------------------------------------
SETTINGS_FILE=".claude/settings.local.json"
if [ ! -f "$SETTINGS_FILE" ] || ! grep -q 'Edit(' "$SETTINGS_FILE" 2>/dev/null; then
  log "PREFLIGHT FAIL: $SETTINGS_FILE missing or has no Edit(...) permission rule for $CONTENT_DRAFT_DIR -- see HANDOFF.md 'Content-drafting automation'"
  notify "FAILED preflight: ${SKILL} -- ${SETTINGS_FILE} missing/misconfigured, draft would be write-blocked. Not attempted."
  exit 1
fi
if [ ! -d "$CONTENT_DRAFT_DIR" ]; then
  log "PREFLIGHT FAIL: $CONTENT_DRAFT_DIR does not exist -- the content repo needs to be cloned there first, this script will not create it"
  notify "FAILED preflight: ${SKILL} -- ${CONTENT_DRAFT_DIR} does not exist. Not attempted."
  exit 1
fi

# ---------------------------------------------------------------------------
# 1. Evidence gate: only proceed if Earnest's own run actually just
#    committed. A stale HEAD means Earnest hasn't run yet, is still running,
#    or failed upstream before reaching its commit step -- in every case,
#    the data this skill needs may not be there or may not be fresh.
# ---------------------------------------------------------------------------
LAST_COMMIT_EPOCH=$(git log -1 --format=%ct 2>/dev/null || echo 0)
NOW_EPOCH=$(date +%s)
COMMIT_AGE=$(( NOW_EPOCH - LAST_COMMIT_EPOCH ))
if [ "$COMMIT_AGE" -gt "$MAX_COMMIT_AGE_SECS" ]; then
  log "SKIP: last commit is ${COMMIT_AGE}s old (limit ${MAX_COMMIT_AGE_SECS}s) -- Earnest's run may not have completed/committed yet"
  notify "SKIPPED: ${SKILL} -- no fresh Earnest commit (last commit ${COMMIT_AGE}s ago). Not attempted this cycle."
  exit 0
fi

# ---------------------------------------------------------------------------
# 2. Clean-tree guard, same spirit as earnest_cron.sh's own pre-guard --
#    if something's already dirty, don't add a headless Claude session on
#    top of unknown state.
# ---------------------------------------------------------------------------
if [ -n "$(git status --porcelain)" ]; then
  log "ABORT: working tree not clean before starting -- refusing to run"
  notify "FAILED: ${SKILL} -- repo tree was dirty before starting, did not run. Investigate before next Earnest run."
  exit 1
fi
PRE_SHA=$(git rev-parse HEAD)

# ---------------------------------------------------------------------------
# 3. Fire the skill headlessly, hard-timeout enforced. No TTY exists to
#    approve anything mid-run, so anything requiring interactive approval
#    fails fast rather than hanging -- the timeout is a backstop, not the
#    primary safety mechanism.
# ---------------------------------------------------------------------------
REF_FILE=$(mktemp)

log "firing /${SKILL} (timeout ${TIMEOUT_SECS}s)"
run_with_timeout "$TIMEOUT_SECS" claude -p "/${SKILL}" >> "$LOG_PATH" 2>&1
CLAUDE_EXIT=$?

NEW_DRAFTS=$(find "$CONTENT_DRAFT_DIR" -type f -newer "$REF_FILE" 2>/dev/null || true)
rm -f "$REF_FILE"

# ---------------------------------------------------------------------------
# 4. Post-run tripwire: this repo must be exactly as it was. A stray file
#    or commit here would silently trip Earnest's clean-tree guard on its
#    next run -- surface it loudly instead of leaving it for Earnest to
#    discover as an unexplained abort.
# ---------------------------------------------------------------------------
POST_STATUS=$(git status --porcelain)
POST_SHA=$(git rev-parse HEAD)
if [ -n "$POST_STATUS" ] || [ "$POST_SHA" != "$PRE_SHA" ]; then
  log "TRIPWIRE: repo changed during the skill run (status: $POST_STATUS; HEAD $PRE_SHA -> $POST_SHA)"
  notify "ALERT: ${SKILL} left the boxscore-prophet repo dirty or advanced HEAD -- investigate before Earnest's next run. Log: ${LOG_PATH}"
  # Deliberately not auto-cleaning: a blind reset/checkout here could
  # destroy something real. Surface it and let a human look first.
  exit 1
fi

# ---------------------------------------------------------------------------
# 5. Report the outcome -- every branch notifies, including a clean decline.
# ---------------------------------------------------------------------------
if [ "$CLAUDE_EXIT" -eq 124 ]; then
  log "TIMEOUT after ${TIMEOUT_SECS}s"
  notify "ALERT: ${SKILL} timed out after ${TIMEOUT_SECS}s, no draft produced. Log: ${LOG_PATH}"
  exit 1
elif [ "$CLAUDE_EXIT" -ne 0 ]; then
  log "claude exited ${CLAUDE_EXIT}"
  notify "ALERT: ${SKILL} exited ${CLAUDE_EXIT} unexpectedly. Log: ${LOG_PATH}"
  exit 1
elif [ -z "$NEW_DRAFTS" ]; then
  log "completed, no draft written (declined -- see log for why)"
  notify "${SKILL}: no draft this cycle (declined -- see ${LOG_PATH} for the reason)."
  exit 0
else
  N_NEW=$(printf '%s\n' "$NEW_DRAFTS" | grep -c .)
  FIRST_NEW=$(printf '%s\n' "$NEW_DRAFTS" | head -1)
  EXTRA=""
  [ "$N_NEW" -gt 1 ] && EXTRA=" (+$((N_NEW - 1)) more file(s), see log)"
  log "draft written ($N_NEW file(s)): $NEW_DRAFTS"
  notify "${SKILL}: draft ready at $(basename "$FIRST_NEW")${EXTRA} -- review before publishing."
  exit 0
fi
