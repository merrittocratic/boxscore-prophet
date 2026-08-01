# R/10e_rookie_tracker.R
# Weekly rookie tracker: how the ~/nfl-draft-model 2026 verdicts are playing
# out. Consumes the 10e0 crosswalk + nflverse actuals + (when present) the
# 10c ledger's pre-kickoff probabilities. READ-ONLY of every pipeline
# artifact; writes content CSVs only -- no deployment surface.
#
# CONTENT RULE (Steve 2026-07-30): OPPORTUNITY (snap share, touches) and
# EFFICIENCY-WHEN-PLAYING (FP per opportunity) are separate columns and
# must never be collapsed -- a rookie who sits is "no data yet", not a
# bust; the draft model's target is multi-year AV. Early "bust watch"
# content may lean on efficiency only.
#
# Rebuilds from authoritative sources every run (idempotent, self-healing;
# no append). The ledger attach IS point-in-time honest -- it takes the
# last (= latest pre-kickoff by append order) ledger row per player-week.
#
# Usage: Rscript R/10e_rookie_tracker.R [season]     (default 2026)
# Cadence: weekly_run.sh full mode (Tuesdays; prior week's games complete).
# Pre-season: nflverse loaders guarded -> full roster baseline, zero games.
#
# Outputs:
#   output/10e_rookie_tracker_<season>.csv  season-to-date, 1 row/rookie
#   output/10e_rookie_weekly_<season>.csv   weekly long detail (active only)

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(cli)
})

source("R/10b_roster_helpers.R")   # load_season_or_empty()

args <- commandArgs(trailingOnly = TRUE)
TARGET_SEASON <- if (length(args) >= 1) as.integer(args[1]) else 2026L

cli_h1("Step 10e: rookie tracker -- {TARGET_SEASON}")

# args[2]: crosswalk override -- test harness only (e.g. a scratch crosswalk
# of a past season's rookies to exercise the active-week path off-season).
CROSSWALK <- if (length(args) >= 2) args[2] else "data/10e_rookie_crosswalk.csv"
if (!file.exists(CROSSWALK)) {
  cli_alert_danger("Crosswalk missing -- run R/10e0_rookie_crosswalk.R first.")
  quit(status = 1)
}
cw <- read_csv(CROSSWALK, show_col_types = FALSE)
cli_alert_info("Crosswalk rookies: {nrow(cw)} ({sum(!is.na(cw$draft_pick))} drafted, {sum(cw$gsis_provisional)} gsis still provisional)")

# ---- 1. Actuals -----------------------------------------------------------
stats <- load_season_or_empty(nflreadr::load_player_stats, TARGET_SEASON) |>
  filter(season_type == "REG") |>
  transmute(gsis_id = player_id, week,
            fp_ppr = fantasy_points_ppr,
            carries, targets, attempts, receptions)

snaps <- load_season_or_empty(nflreadr::load_snap_counts, TARGET_SEASON) |>
  filter(game_type == "REG") |>
  transmute(pfr_id = pfr_player_id, week, offense_pct)

cli_alert_info("Actuals: {nrow(stats)} stat rows, {nrow(snaps)} snap rows (0 = pre-season baseline mode)")

# ---- 2. Ledger pre-kickoff probabilities (optional enrichment) -----------
ledger_files <- list.files("output", sprintf("^10c_ledger_%d_w", TARGET_SEASON),
                           full.names = TRUE)
ledger <- if (length(ledger_files) > 0) {
  map(ledger_files, read_csv, show_col_types = FALSE) |>
    list_rbind() |>
    group_by(player_id, week) |>
    slice_tail(n = 1) |>            # last append = latest pre-kickoff row
    ungroup() |>
    transmute(gsis_id = player_id, week, p_start, p_boom)
} else {
  tibble(gsis_id = character(), week = integer(),
         p_start = numeric(), p_boom = numeric())
}
cli_alert_info("Ledger rows attached: {nrow(ledger)} ({length(ledger_files)} week file{?s})")

# ---- 3. Weekly detail (active rookie-weeks only) -------------------------
weekly <- cw |>
  filter(!is.na(gsis_id)) |>
  select(gsis_id, pfr_id, nfl_name, pos, draft_round, draft_pick, draft_team,
         model_verdict) |>
  inner_join(stats, by = "gsis_id") |>
  left_join(snaps, by = c("pfr_id", "week")) |>
  left_join(ledger, by = c("gsis_id", "week")) |>
  mutate(
    opportunities = if_else(pos == "QB",
                            coalesce(attempts, 0) + coalesce(carries, 0),
                            coalesce(carries, 0) + coalesce(targets, 0)),
    fp_per_opp = if_else(opportunities > 0, fp_ppr / opportunities, NA_real_)
  ) |>
  arrange(pos, draft_pick, week)

# ---- 4. Season-to-date summary (every crosswalk rookie) ------------------
agg <- weekly |>
  group_by(gsis_id) |>
  summarise(
    games          = n(),
    cum_fp_ppr     = sum(fp_ppr, na.rm = TRUE),
    fp_pg          = cum_fp_ppr / games,
    opps_pg        = mean(opportunities),
    snap_share_avg = mean(offense_pct, na.rm = TRUE),
    fp_per_opp     = sum(fp_ppr, na.rm = TRUE) / pmax(sum(opportunities), 1),
    last_week_active = suppressWarnings(max(week)),
    model_p_start_latest = last(p_start[!is.na(p_start)]),
    .groups = "drop"
  ) |>
  mutate(last_week_active = if_else(is.infinite(last_week_active),
                                    NA_integer_, as.integer(last_week_active)))

summary_tbl <- cw |>
  left_join(agg, by = "gsis_id") |>
  mutate(
    games = coalesce(games, 0L),
    tracker_status = case_when(
      is.na(gsis_id) | gsis_provisional ~ "id_pending",
      games == 0L                       ~ "no_nfl_data_yet",
      snap_share_avg < 0.25 | is.na(snap_share_avg) ~ "limited_role",
      TRUE                              ~ "active"
    )
  ) |>
  select(nfl_name, dm_name, pos, school, draft_round, draft_pick, draft_team,
         model_verdict, pred, p_boom, p_bust,
         games, cum_fp_ppr, fp_pg, opps_pg, snap_share_avg, fp_per_opp,
         last_week_active, model_p_start_latest, tracker_status,
         gsis_id, pfr_id, gsis_provisional) |>
  arrange(pos, draft_pick)

# ---- 5. Write + report ----------------------------------------------------
out_sum <- sprintf("output/10e_rookie_tracker_%d.csv", TARGET_SEASON)
out_wk  <- sprintf("output/10e_rookie_weekly_%d.csv", TARGET_SEASON)
write_csv(summary_tbl, out_sum)
write_csv(weekly, out_wk)

cli_alert_success("{out_sum} ({nrow(summary_tbl)} rookies)")
cli_alert_success("{out_wk} ({nrow(weekly)} active rookie-weeks)")
print(summary_tbl |> count(tracker_status) |> as.data.frame(), row.names = FALSE)

stopifnot(nrow(summary_tbl) == nrow(cw))
cli_h1("Step 10e complete")
