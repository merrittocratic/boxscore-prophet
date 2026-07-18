#!/usr/bin/env bash
# Weekly production runner for Earnest (MacMini cron). Laptop = stage: the
# same script must run identically in both places.
#
# Usage: scripts/weekly_run.sh full|rescore [season] [week]
#   full    -- Tuesday night: rebuild feature layers, retrain deployment
#              models (10a), then slate + score + content for the target
#              week. Models are frozen for the week after this run.
#   rescore -- game-day refresh: rebuild slates (injury reports, weather,
#              overrides), re-score REMAINING games only (10c skips any
#              game that has kicked off), regenerate content. Ledger
#              locking in 10c means published numbers for played games
#              are never touched.
#
# Target auto-detect (when season/week omitted): the week containing the
# earliest REG-season kickoff still in the future. This lands every window
# on the right week with one rule: the Tuesday-night full run resolves to
# W+1 (all of W is played), and Thu/Sat/Sun/Mon rescores resolve to the
# in-progress week.

set -euo pipefail
cd "$(dirname "$0")/.."

MODE="${1:?usage: weekly_run.sh full|rescore [season] [week]}"
SEASON="${2:-auto}"
WEEK="${3:-auto}"

if [ "$MODE" != "full" ] && [ "$MODE" != "rescore" ]; then
  echo "unknown mode '$MODE' (want full|rescore)" >&2
  exit 1
fi

if [ "$SEASON" = "auto" ]; then
  SEASON=$(Rscript -e 'm <- as.integer(format(Sys.Date(), "%m")); y <- as.integer(format(Sys.Date(), "%Y")); cat(if (m >= 8) y else y - 1)')
fi

if [ "$WEEK" = "auto" ]; then
  WEEK=$(Rscript -e '
    suppressMessages(s <- nflreadr::load_schedules(as.integer(commandArgs(TRUE)[1])))
    s <- subset(s, game_type == "REG")
    k <- as.POSIXct(paste(s$gameday, ifelse(is.na(s$gametime), "13:00", s$gametime)),
                    tz = "America/New_York")
    fut <- s$week[k > Sys.time()]
    cat(if (length(fut)) min(fut) else max(s$week))
  ' "$SEASON")
fi

STAMP=$(date +%Y%m%d_%H%M)
LOG="logs/run_${STAMP}_${MODE}_${SEASON}_w${WEEK}.log"
mkdir -p logs
echo "[weekly_run] mode=$MODE season=$SEASON week=$WEEK log=$LOG"

run() {
  echo "== Rscript $* ==" | tee -a "$LOG"
  Rscript "$@" >> "$LOG" 2>&1
}

if [ "$MODE" = "full" ]; then
  run R/build_rb_feature_layer.R
  run R/04a_wr_feature_layer.R
  run R/08a_qb_feature_layer.R          # also refreshes data/qb_def_adj.rds
  run R/10a_deployment_models.R         # weekly retrain; frozen after tonight
fi

run R/10b_weekly_slate.R   "$SEASON" "$WEEK"   # game slate + kickoff weather
run R/10b2_player_slate.R  "$SEASON" "$WEEK"
run R/10b3_wr_slate.R      "$SEASON" "$WEEK"
run R/10b4_qb_slate.R      "$SEASON" "$WEEK"
run R/10c_weekly_score.R   "$SEASON" "$WEEK"
run R/10d0_ecr_fetch.R     "$SEASON" "$WEEK"   # skips itself if no API key yet
run R/10d_content_tables.R "$SEASON" "$WEEK"

echo "[weekly_run] done: $MODE $SEASON w$WEEK"
