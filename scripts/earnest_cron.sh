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

fail() { echo "[earnest_cron] ABORT: $1" >&2; exit 1; }

# 1. Clean-tree guard + pull
[ -z "$(git status --porcelain)" ] || fail "working tree not clean -- refusing to run production (git status: $(git status --porcelain | head -3 | tr '\n' ' '))"
git pull --ff-only || fail "ff-only pull failed (concurrent laptop push?) -- will retry next slot"

# 2. The run itself (weekly_run.sh logs to logs/run_<stamp>_...)
bash scripts/weekly_run.sh "$MODE" || fail "weekly_run.sh $MODE exited nonzero -- NOT committing partial outputs"

# 3. Commit the published record: tracked modifications (feature tables,
#    deploy artifacts on full runs, ledgers) + new run outputs and logs.
git add -u
git add output/ logs/
if git diff --cached --quiet; then
  echo "[earnest_cron] nothing to commit (no-op refresh)"
  exit 0
fi
git commit -m "earnest ${MODE} run ${STAMP} (auto: weekly cadence)"
git push origin main || fail "push failed -- commit is local; investigate before next slot"

echo "[earnest_cron] done: ${MODE} ${STAMP}"
