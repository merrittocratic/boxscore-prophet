#!/usr/bin/env bash
# scripts/earnest_cron.sh -- production cron entrypoint (MacMini / Earnest).
# Wraps weekly_run.sh with the git cadence the runner itself deliberately
# omits: clean-tree guard -> pull -> run -> commit outputs -> push.
#
# Cadence rules implemented here (README "SINGLE-WRITER RULE" + run-output
# hygiene, Steve 2026-07-26):
#   - Production runs COMMIT their outputs; they are the published record.
#   - A dirty tree means a ship pass landed mid-week or a prior run died:
#     ABORT loudly rather than run production on an unknown base.
#   - Pull is --ff-only: Earnest never merges. If the pull fails, a laptop
#     commit raced us -- abort, next cron slot retries.
# Usage: earnest_cron.sh full|rescore   (season/week auto-detect downstream)

set -euo pipefail
cd "$(dirname "$0")/.."

MODE="${1:?usage: earnest_cron.sh full|rescore}"
STAMP=$(date +%Y%m%d_%H%M)
LOG_PATH=""

fail() { echo "[earnest_cron] ABORT: $1" >&2; exit 1; }
warn() { echo "[earnest_cron] WARN: $1" >&2; }

# 1. Clean-tree guard + pull
[ -z "$(git status --porcelain)" ] || fail "working tree not clean -- refusing to run production (git status: $(git status --porcelain | head -3 | tr '\n' ' '))"
git pull --ff-only || fail "ff-only pull failed (concurrent laptop push?) -- will retry next slot"

# 2. The run itself (weekly_run.sh logs to logs/run_<stamp>_...)
bash scripts/weekly_run.sh "$MODE" || fail "weekly_run.sh $MODE exited nonzero -- NOT committing partial outputs"
LOG_PATH=$(ls -1t "logs/run_${STAMP}_${MODE}_"*.log 2>/dev/null | head -n1)
[ -n "$LOG_PATH" ] || fail "could not locate run log for stamp ${STAMP}"

RUN_META=$(python3 - "$LOG_PATH" <<'PY'
import os, re, sys
base = os.path.basename(sys.argv[1])
m = re.match(r"run_\d{8}_\d{4}_(full|rescore)_(\d{4})_w(\d+)\.log$", base)
if not m:
    raise SystemExit(1)
print(m.group(2))
print(int(m.group(3)))
PY
) || fail "could not parse season/week from ${LOG_PATH}"
SEASON=$(printf '%s\n' "$RUN_META" | sed -n '1p')
WEEK=$(printf '%s\n' "$RUN_META" | sed -n '2p')

# 3. Commit the published record: tracked modifications (feature tables,
#    deploy artifacts on full runs, ledgers) + new run outputs and logs.
git add -u
git add output/ logs/
if git diff --cached --quiet; then
  echo "[earnest_cron] nothing to commit (no-op refresh)"
  scripts/refresh_latest.sh "$SEASON" "$WEEK" "$MODE" "$LOG_PATH" || warn "latest handoff refresh failed after no-op run"
  scripts/earnest_notify.sh || warn "delivery failed after no-op run"
  exit 0
fi
git commit -m "earnest ${MODE} run ${STAMP} (auto: weekly cadence)"
git push origin main || fail "push failed -- commit is local; investigate before next slot"

scripts/refresh_latest.sh "$SEASON" "$WEEK" "$MODE" "$LOG_PATH" || warn "latest handoff refresh failed"
scripts/earnest_notify.sh || warn "delivery failed"

echo "[earnest_cron] done: ${MODE} ${STAMP}"
