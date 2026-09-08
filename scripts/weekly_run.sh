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
  run R/12a_te_feature_layer.R
  run R/08a_qb_feature_layer.R          # also refreshes data/qb_def_adj.rds
  run R/11b_injury_state_layer.R        # injury states for new in-season rows; 10a stopifnot requires them
  run R/10a_deployment_models.R         # weekly retrain; frozen after tonight

  # S1 shadow mode (2026-09-08, MODEL_ARCH=fp1 in 10c below): keeps the
  # single-stage RB/WR deployment artifacts retraining on the SAME weekly
  # cadence as 10a above, so the shadow doesn't quietly go stale while
  # production keeps refreshing. Every step here is `|| true` -- a shadow
  # failure must never fail the real (twostage) run. RB's ship arm is
  # floor-free (MIN_OPP=1), which nothing else in this pipeline builds --
  # WR's ship arm is base, already covered by 04a_wr_feature_layer.R above.
  MIN_OPP=1 FT_RDS_OUT=data/rb_feature_table_floorfree.rds \
    FT_CSV_OUT=output/rb_feature_table_floorfree_v2.0.csv \
    run R/build_rb_feature_layer.R || true
  run R/21c_fp_train_tables.R      || true   # fp1 shadow: FP-target training tables (base + floor-free)
  run R/21n_fp_deployment_models.R || true   # fp1 shadow: weekly retrain, mirrors 10a's cadence above
fi

run R/10b_weekly_slate.R   "$SEASON" "$WEEK"   # game slate + kickoff weather
run R/10b2_player_slate.R  "$SEASON" "$WEEK"
run R/10b3_wr_slate.R      "$SEASON" "$WEEK"
run R/10b5_te_slate.R      "$SEASON" "$WEEK"
run R/10b4_qb_slate.R      "$SEASON" "$WEEK"
run R/10c_weekly_score.R   "$SEASON" "$WEEK"
MODEL_ARCH=fp1 OUT_SUFFIX=_fp1 run R/10c_weekly_score.R "$SEASON" "$WEEK" || true   # S1 shadow score, never fails the real run
run R/10d0_ecr_fetch.R     "$SEASON" "$WEEK"   # skips itself if no API key yet
run R/10d_content_tables.R "$SEASON" "$WEEK"
run R/10g_movers_table.R   "$SEASON" "$WEEK"   # movers vs trailing baseline; refreshes on rescores too. No-op before week 3.
if [ "$MODE" = "full" ]; then
  run R/10e_rookie_tracker.R "$SEASON"        # Tue only: prior week complete; content CSVs, no deploy surface
  run R/10f_weekly_eval.R    "$SEASON"        # Tue only: scorecard + watch cells + drift alarms; never aborts
fi

echo "[weekly_run] done: $MODE $SEASON w$WEEK"
