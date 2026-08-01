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
#   output/10e_rookie_leaders_<season>.csv  season-to-date fantasy leaders
#   output/10e_rookie_role_earners_<season>.csv latest-usage role earners
#   output/10e_rookie_efficiency_<season>.csv  efficiency flashes
#   output/10e_rookie_watch_<season>.csv   early-role caution / waiting room
#   output/10e_rookie_content_<season>.md  markdown content handoff

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(cli)
})

source("R/10b_roster_helpers.R")   # load_season_or_empty()

args <- commandArgs(trailingOnly = TRUE)
TARGET_SEASON <- if (length(args) >= 1) as.integer(args[1]) else 2026L

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
pct <- function(x, digits = 0) if_else(is.na(x), "--", paste0(round(100 * x, digits), "%"))
num1 <- function(x) if_else(is.na(x), "--", sprintf("%.1f", x))

md_table <- function(df, headers) {
  if (nrow(df) == 0) return("_None this run._")
  rows <- apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  paste(c(paste0("| ", paste(headers, collapse = " | "), " |"),
          paste0("|", paste(rep("---", length(headers)), collapse = "|"), "|"),
          rows), collapse = "\n")
}

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
    cum_opps       = sum(opportunities, na.rm = TRUE),
    opps_pg        = mean(opportunities),
    snap_share_avg = mean(offense_pct, na.rm = TRUE),
    fp_per_opp     = sum(fp_ppr, na.rm = TRUE) / pmax(sum(opportunities), 1),
    last_week_active = suppressWarnings(max(week)),
    model_p_start_latest = last(p_start[!is.na(p_start)]) %||% NA_real_,
    model_p_boom_latest  = last(p_boom[!is.na(p_boom)]) %||% NA_real_,
    latest_fp_ppr        = fp_ppr[which.max(week)] %||% NA_real_,
    latest_opportunities = opportunities[which.max(week)] %||% NA_real_,
    latest_snap_share    = offense_pct[which.max(week)] %||% NA_real_,
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
         games, cum_fp_ppr, fp_pg, cum_opps, opps_pg, snap_share_avg, fp_per_opp,
         last_week_active, latest_fp_ppr, latest_opportunities, latest_snap_share,
         model_p_start_latest, model_p_boom_latest, tracker_status,
         gsis_id, pfr_id, gsis_provisional) |>
  arrange(pos, draft_pick)

# ---- 5. Content surfaces --------------------------------------------------
leaders <- summary_tbl |>
  filter(tracker_status == "active", games > 0) |>
  arrange(desc(cum_fp_ppr), desc(fp_pg), desc(cum_opps)) |>
  mutate(rank = row_number()) |>
  select(rank, nfl_name, pos, draft_pick, draft_team, model_verdict,
         games, cum_fp_ppr, fp_pg, cum_opps, opps_pg,
         snap_share_avg, model_p_start_latest)

role_earners <- summary_tbl |>
  filter(games > 0, !tracker_status %in% c("id_pending")) |>
  filter(coalesce(latest_snap_share, snap_share_avg, 0) >= 0.35 |
           coalesce(latest_opportunities, 0) >= if_else(pos == "QB", 12, 5)) |>
  arrange(desc(coalesce(latest_snap_share, snap_share_avg, 0)),
          desc(coalesce(latest_opportunities, 0)), desc(games), draft_pick) |>
  mutate(rank = row_number()) |>
  select(rank, nfl_name, pos, draft_pick, draft_team, tracker_status,
         last_week_active, latest_fp_ppr, latest_opportunities, latest_snap_share,
         snap_share_avg, model_p_start_latest)

efficiency_flashes <- summary_tbl |>
  filter(games > 0, fp_per_opp > 0,
         cum_opps >= if_else(pos == "QB", 25, 12)) |>
  arrange(desc(fp_per_opp), desc(latest_opportunities), desc(cum_opps)) |>
  mutate(rank = row_number()) |>
  select(rank, nfl_name, pos, draft_pick, draft_team, model_verdict,
         games, cum_opps, fp_per_opp, latest_fp_ppr, latest_opportunities,
         latest_snap_share, model_p_boom_latest)

watchlist <- summary_tbl |>
  filter(!gsis_provisional,
         tracker_status %in% c("limited_role", "no_nfl_data_yet"),
         !is.na(draft_round), draft_round <= 3) |>
  arrange(draft_round, draft_pick, tracker_status) |>
  mutate(rank = row_number()) |>
  select(rank, nfl_name, pos, draft_round, draft_pick, draft_team,
         tracker_status, games, last_week_active, latest_opportunities,
         latest_snap_share, model_verdict)

latest_week <- if (nrow(weekly) > 0) max(weekly$week, na.rm = TRUE) else NA_integer_

md <- c(
  sprintf("# BOXSCORE PROPHET -- %d rookie tracker", TARGET_SEASON),
  "",
  if (!is.na(latest_week)) {
    sprintf("Latest completed rookie week in the tracker: Week %d.", latest_week)
  } else {
    "No active rookie weeks yet. This is the pre-season baseline handoff."
  },
  "",
  "## Top-performing rookies -- season to date", "",
  md_table(leaders |>
             slice_head(n = 12) |>
             transmute(rank, nfl_name, pos, draft_team,
                       verdict = model_verdict,
                       games, fp = num1(cum_fp_ppr), fp_pg = num1(fp_pg),
                       opps_pg = num1(opps_pg), snap = pct(snap_share_avg),
                       model_start = pct(model_p_start_latest)),
           c("#", "Player", "Pos", "Team", "Verdict", "G", "FP", "FP/G", "Opp/G", "Snap", "Model start")),
  "",
  "## Role earners -- latest usage is getting real", "",
  md_table(role_earners |>
             slice_head(n = 12) |>
             transmute(rank, nfl_name, pos, draft_team,
                       week = if_else(is.na(last_week_active), "--", as.character(last_week_active)),
                       latest_fp = num1(latest_fp_ppr),
                       opps = num1(latest_opportunities),
                       latest_snap = pct(latest_snap_share),
                       avg_snap = pct(snap_share_avg),
                       model_start = pct(model_p_start_latest)),
           c("#", "Player", "Pos", "Team", "Wk", "Latest FP", "Latest opps", "Latest snap", "Avg snap", "Model start")),
  "",
  "## Efficiency flashes -- when they touch it, it matters", "",
  md_table(efficiency_flashes |>
             slice_head(n = 12) |>
             transmute(rank, nfl_name, pos, draft_team,
                       verdict = model_verdict,
                       games, cum_opps,
                       fp_per_opp = num1(fp_per_opp),
                       latest_opps = num1(latest_opportunities),
                       latest_snap = pct(latest_snap_share),
                       model_boom = pct(model_p_boom_latest)),
           c("#", "Player", "Pos", "Team", "Verdict", "G", "Opps", "FP/opp", "Latest opps", "Latest snap", "Model boom")),
  "",
  "## Still waiting on runway -- not a bust list", "",
  "These are drafted rookies without a real role yet or with no usable NFL sample yet. Sitting is not the same thing as busting.", "",
  md_table(watchlist |>
             slice_head(n = 12) |>
             transmute(rank, nfl_name, pos,
                       draft = paste0("R", draft_round, "-", draft_pick),
                       draft_team,
                       status = tracker_status,
                       games,
                       last_week = if_else(is.na(last_week_active), "--", as.character(last_week_active)),
                       latest_opps = num1(latest_opportunities),
                       latest_snap = pct(latest_snap_share),
                       verdict = model_verdict),
           c("#", "Player", "Pos", "Draft", "Team", "Status", "G", "Last wk", "Latest opps", "Latest snap", "Verdict")),
  "",
  "---",
  "*Tracker status separates role from efficiency. Opportunity and efficiency stay split on purpose: no NFL data yet is not the same as limited-role underperformance.*"
)

# ---- 6. Write + report ----------------------------------------------------
out_sum <- sprintf("output/10e_rookie_tracker_%d.csv", TARGET_SEASON)
out_wk  <- sprintf("output/10e_rookie_weekly_%d.csv", TARGET_SEASON)
out_lead <- sprintf("output/10e_rookie_leaders_%d.csv", TARGET_SEASON)
out_role <- sprintf("output/10e_rookie_role_earners_%d.csv", TARGET_SEASON)
out_eff  <- sprintf("output/10e_rookie_efficiency_%d.csv", TARGET_SEASON)
out_watch <- sprintf("output/10e_rookie_watch_%d.csv", TARGET_SEASON)
out_md  <- sprintf("output/10e_rookie_content_%d.md", TARGET_SEASON)
write_csv(summary_tbl, out_sum)
write_csv(weekly, out_wk)
write_csv(leaders, out_lead)
write_csv(role_earners, out_role)
write_csv(efficiency_flashes, out_eff)
write_csv(watchlist, out_watch)
writeLines(paste(md, collapse = "\n"), out_md)

cli_alert_success("{out_sum} ({nrow(summary_tbl)} rookies)")
cli_alert_success("{out_wk} ({nrow(weekly)} active rookie-weeks)")
cli_alert_success("{out_lead} ({nrow(leaders)} rows)")
cli_alert_success("{out_role} ({nrow(role_earners)} rows)")
cli_alert_success("{out_eff} ({nrow(efficiency_flashes)} rows)")
cli_alert_success("{out_watch} ({nrow(watchlist)} rows)")
cli_alert_success("{out_md}")
print(summary_tbl |> count(tracker_status) |> as.data.frame(), row.names = FALSE)

stopifnot(nrow(summary_tbl) == nrow(cw))
cli_h1("Step 10e complete")
