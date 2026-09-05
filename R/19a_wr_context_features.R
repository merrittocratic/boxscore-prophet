# R/19a_wr_context_features.R
# WR context-feature layer for the top-of-board resolution rung
# (pre-registered spec approved 2026-09-05; bars: 19c grader). Builds
# data/wr_context_features.rds keyed (player_id, season, week) covering
# every wr_feature_table row. All features are ex-ante as of Friday
# lock; injury signal is masked with the SAME shared core as rung 1
# (R/11b_injury_state_fns.R), inheriting its documented 2025
# date_modified approximation (~7%).
#
# NOVELTY GUARD (vs rung-1 WR published null, 11d): own-injury states
# and SAME-POSITION vacated share were tested there and nulled -- they
# are deliberately ABSENT here. This layer is only the new families:
#
#   QB context
#     qb_out_int        prior primary QB (last played week's team leader
#                       in pass attempts) is Out/Doubtful this week
#     qb_q_int          ... is Questionable this week
#     qb_trail_epa4     team passing EPA per attempt, last 4 team games
#   Cross-position target competition (TE + RB only -- the same-position
#   version is the rung-1 null)
#     vacated_tgt_share_xpos    sum of trailing (wt_) target share of
#                               same-team TE/RB Out/Doubtful this week
#     returning_xpos_tgt_share  trailing share of same-team TE/RB absent
#                               2-8 weeks now practicing (masked signal)
#   Trajectory (the volume model sees rolling MEANS only; these add the
#   derivative)
#     tgt_share_slope3   OLS slope of target_share_obs over last 3
#                        played games (0 when fewer than 3)
#     tgt_share_delta1   last game's target_share_obs minus the current
#                        trailing wt_target_share
#   Opponent context (v1 proxy; documented deviation from the spec's
#   "EPA vs top-target-share receivers" -- FP-allowed needs no PBP pass
#   and is the classic reconstructable form; upgrade path noted in 19b)
#     def_wr_fp_allowed4  opponent's WR PPR FP allowed per game, last 4
#                         defense games, minus the league rolling mean
#
# Usage: Rscript R/19a_wr_context_features.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(cli)
})

source("R/11b_injury_state_fns.R")

cli_h1("19a: WR context feature layer")

wr <- readRDS("data/wr_feature_table.rds")
SEASONS <- sort(unique(wr$season))
cli_alert_info("wr_feature_table: {nrow(wr)} rows, seasons {min(SEASONS)}-{max(SEASONS)}")

lock_table <- build_lock_table(SEASONS)
inj <- clean_injury_reports(nflreadr::load_injuries(SEASONS), lock_table,
                            mask = TRUE)

stats <- nflreadr::load_player_stats(SEASONS) |>
  filter(season_type == "REG", !is.na(player_id))

# ============================================================ QB context --
cli_h2("QB context")

qb_weeks <- stats |>
  filter(position == "QB", !is.na(attempts), attempts > 0) |>
  group_by(team, season, week) |>
  arrange(desc(attempts), .by_group = TRUE) |>
  summarise(primary_qb = first(player_id),
            team_pass_epa = sum(passing_epa, na.rm = TRUE),
            team_att = sum(attempts), .groups = "drop")

# Prior primary QB as of week w = primary in the team's most recent
# played week < w; trailing EPA/att = last 4 team games strictly before w.
team_hist <- qb_weeks |>
  group_by(team, season) |>
  arrange(week, .by_group = TRUE) |>
  mutate(
    prior_primary_qb = lag(primary_qb),
    qb_trail_epa4 = (lag(cumsum(team_pass_epa), 1, default = 0) -
                       lag(cumsum(team_pass_epa), 5, default = 0)) /
      pmax(lag(cumsum(team_att), 1, default = 0) -
             lag(cumsum(team_att), 5, default = 0), 1)
  ) |>
  ungroup() |>
  select(posteam = team, season, week,
         prior_primary_qb, qb_trail_epa4)

# wr rows exist for played weeks; a missed team-week in qb_weeks (bye)
# is absent from both tables, so the week-key join is aligned.
qb_ctx <- wr |>
  distinct(posteam, season, week) |>
  left_join(team_hist, by = c("posteam", "season", "week")) |>
  left_join(inj |> rename(prior_primary_qb = player_id,
                          qb_report = report_std) |>
              select(prior_primary_qb, season, week, qb_report),
            by = c("prior_primary_qb", "season", "week")) |>
  mutate(qb_out_int = as.integer(coalesce(qb_report %in% c("out", "doubtful"), FALSE)),
         qb_q_int   = as.integer(coalesce(qb_report == "questionable", FALSE)),
         qb_trail_epa4 = coalesce(qb_trail_epa4, 0)) |>
  select(posteam, season, week, qb_out_int, qb_q_int, qb_trail_epa4)

# ========================================== cross-position competition --
cli_h2("Cross-position (TE/RB) target competition")

xpos <- map(c("te", "rb"), function(p) {
  readRDS(paste0("data/", p, "_feature_table.rds")) |>
    select(player_id, posteam, season, week, wt_target_share) |>
    mutate(pos = toupper(p))
}) |> list_rbind() |>
  filter(!is.na(wt_target_share))

# Vacated: TE/RB listed Out/Doubtful for week w; their trailing share is
# the last wt_target_share they carried into a played week (<= w).
xpos_last <- xpos |>
  group_by(player_id, season) |>
  arrange(week, .by_group = TRUE) |>
  mutate(last_played_week = week,
         carry_share = wt_target_share) |>
  ungroup()

xpos_inj <- inj |>
  inner_join(xpos_last |> distinct(player_id, season) |> mutate(is_xpos = TRUE),
             by = c("player_id", "season")) |>
  filter(is_xpos)

week_grid <- wr |> distinct(posteam, season, week)

xpos_status <- week_grid |>
  inner_join(xpos_last |> select(player_id, posteam, season,
                                 last_played_week, carry_share),
             by = c("posteam", "season"), relationship = "many-to-many") |>
  filter(last_played_week < week) |>
  group_by(posteam, season, week, player_id) |>
  slice_max(last_played_week, n = 1, with_ties = FALSE) |>
  ungroup() |>
  left_join(inj |> select(player_id, season, week, report_std, practice_int),
            by = c("player_id", "season", "week")) |>
  mutate(gap = week - last_played_week - 1L)

xpos_feats <- xpos_status |>
  group_by(posteam, season, week) |>
  summarise(
    vacated_tgt_share_xpos = sum(
      carry_share[coalesce(report_std %in% c("out", "doubtful"), FALSE)]),
    returning_xpos_tgt_share = sum(
      carry_share[gap >= 2L & gap <= 8L &
                    coalesce(practice_int >= 1L, FALSE) &
                    !coalesce(report_std %in% c("out", "doubtful"), FALSE)]),
    .groups = "drop"
  )

# =============================================================== trajectory --
cli_h2("Target-share trajectory")

slope3 <- function(y1, y2, y3) {
  # y1 = most recent; OLS slope on x = (1,2,3) oldest->newest
  ifelse(is.na(y1) | is.na(y2) | is.na(y3), 0, (y1 - y3) / 2)
}

# Computed inline on wr rows (no self-join: the table carries
# player_id = NA placeholder rows that would duplicate join keys).
wr <- wr |>
  group_by(player_id, season) |>
  arrange(week, .by_group = TRUE) |>
  mutate(
    l1 = lag(target_share_obs, 1),
    l2 = lag(target_share_obs, 2),
    l3 = lag(target_share_obs, 3),
    tgt_share_slope3 = if_else(is.na(player_id), 0, slope3(l1, l2, l3)),
    tgt_share_delta1 = if_else(is.na(player_id), 0,
                               coalesce(l1 - wt_target_share, 0))
  ) |>
  ungroup()

# ======================================================= opponent context --
cli_h2("Opponent WR FP allowed (rolling 4, league-centered)")

wr_fp_team <- stats |>
  filter(position == "WR") |>
  group_by(team, season, week) |>
  summarise(wr_fp = sum(fantasy_points_ppr, na.rm = TRUE), .groups = "drop")

sched <- nflreadr::load_schedules(SEASONS) |> filter(game_type == "REG")
def_allowed <- bind_rows(
  sched |> select(season, week, def = home_team, opp = away_team),
  sched |> select(season, week, def = away_team, opp = home_team)
) |>
  left_join(wr_fp_team, by = c("opp" = "team", "season", "week")) |>
  filter(!is.na(wr_fp)) |>
  group_by(def, season) |>
  arrange(week, .by_group = TRUE) |>
  mutate(games = row_number() - 1L,
         def_wr_fp_allowed4 = (lag(cumsum(wr_fp), 1, default = 0) -
                                 lag(cumsum(wr_fp), 5, default = 0)) /
           pmax(pmin(games, 4L), 1L),
         def_wr_fp_allowed4 = if_else(games == 0L, NA_real_,
                                      def_wr_fp_allowed4)) |>
  ungroup()

league_mean <- def_allowed |>
  filter(!is.na(def_wr_fp_allowed4)) |>
  group_by(season, week) |>
  summarise(lg = mean(def_wr_fp_allowed4), .groups = "drop")

def_feats <- def_allowed |>
  left_join(league_mean, by = c("season", "week")) |>
  mutate(def_wr_fp_allowed4 = coalesce(def_wr_fp_allowed4 - lg, 0)) |>
  select(defteam = def, season, week, def_wr_fp_allowed4)

# ================================================================ assemble --
out <- wr |>
  select(player_id, posteam, defteam, season, week,
         tgt_share_slope3, tgt_share_delta1) |>
  left_join(qb_ctx, by = c("posteam", "season", "week")) |>
  left_join(xpos_feats, by = c("posteam", "season", "week")) |>
  left_join(def_feats, by = c("defteam", "season", "week")) |>
  mutate(across(c(qb_out_int, qb_q_int), ~ coalesce(.x, 0L)),
         across(c(qb_trail_epa4, vacated_tgt_share_xpos,
                  returning_xpos_tgt_share, tgt_share_slope3,
                  tgt_share_delta1, def_wr_fp_allowed4),
                ~ coalesce(.x, 0))) |>
  select(-defteam)

# Join key downstream is (player_id, posteam, season, week): the table's
# player_id = NA placeholder rows are unique per team-week, real players
# unique per week -- the four-column key is unique for both.
stopifnot(nrow(out) == nrow(wr),
          !anyNA(out |> select(-player_id)),
          nrow(distinct(out, player_id, posteam, season, week)) == nrow(out))

CONTEXT_FEATURES <- setdiff(names(out),
                            c("player_id", "posteam", "season", "week"))
cli_h2("Feature summary")
out |>
  summarise(across(all_of(CONTEXT_FEATURES),
                   list(nonzero = ~ mean(.x != 0), sd = ~ sd(.x)))) |>
  pivot_longer(everything()) |>
  mutate(value = round(value, 4)) |>
  print(n = 20)

saveRDS(out, "data/wr_context_features.rds")
cli_alert_success("data/wr_context_features.rds ({nrow(out)} rows, {length(CONTEXT_FEATURES)} features)")
