# R/11a_injury_shock_diagnostic.R
# Ablation ladder rung 1, step 0: SIZE the usage-shock bias before building
# the injury state-machine feature layer.
#
# The pre-registered decomposition (building_in_public_log.md) claims the
# weak population is TRANSITION weeks: depth-chart shocks the rolling
# features see one week late. This diagnostic measures the volume-model
# residual (observed - predicted opportunities) from the SHIPPED backtest
# fold predictions inside three states, defined per player-week:
#
#   return_week : played W, missed W-1, had played earlier in the season
#                 (mid-season return; season debuts are the cold-start
#                 feature's territory, excluded here)
#   above_new_out : a same-team teammate with HIGHER ex-ante rolling usage
#                 share played W-1 but is absent in W (the fresh shock --
#                 McCaffrey -> Jordan Mason archetype)
#   above_long_out : a higher-usage teammate absent in W-1 AND W (rolling
#                 features have had >= 1 week to adapt)
#
# DIAGNOSTIC ONLY: states here use OBSERVED absence of the teammate in
# week W to define the population cleanly. The trained features (next
# step) must replace that with the ex-ante version (Friday report Out/
# Doubtful, or missed W-1 with no return signal). This script conditions
# on outcomes to MEASURE the bias, not to build features.
#
# PRE-STATED EXPECTATIONS (2026-07-18, before first run):
#   - above_new_out: POSITIVE mean vol residual (model underpredicts the
#     beneficiary), expected order 1-4 opportunities
#   - return_week: NEGATIVE mean vol residual (stale pre-injury rolling
#     shares overpredict the snap-ramp week)
#   - above_long_out: same sign as above_new_out, smaller magnitude
#   - steady state: ~0 (fold models are approximately unbiased overall)
# If the transition states show no bias, rung 1 dies here and the ladder
# moves to rung 2 -- publish the null either way.

suppressPackageStartupMessages({
  library(tidyverse)
  library(cli)
})

fmt <- function(x) sprintf("%+.2f", x)

POSITIONS <- list(
  RB = list(preds = "output/03a_v2_lgbm_fold_predictions.csv",
            table = "data/rb_feature_table.rds",
            share = "wt_carry_share"),
  WR = list(preds = "output/04c_wr_asym_fold_predictions.csv",
            table = "data/wr_feature_table.rds",
            share = "wt_target_share")
)

cli_h1("11a: usage-shock diagnostic (RB + WR fold residuals by injury state)")

results <- imap(POSITIONS, function(cfg, pos) {

  ft <- readRDS(cfg$table) |>
    filter(!is.na(player_id)) |>
    select(player_id, season, week, posteam,
           share = all_of(cfg$share))

  preds <- readr::read_csv(cfg$preds, show_col_types = FALSE) |>
    filter(!is.na(player_id)) |>
    select(player_id, season, week, opportunities, pred_vol)

  d <- ft |> inner_join(preds, by = c("player_id", "season", "week"))

  # Played-week index per player-season (from the feature table itself:
  # a row exists iff the player played that week)
  played <- ft |> distinct(player_id, season, week)

  last_played_before <- function(df) {
    df |>
      group_by(player_id, season) |>
      arrange(week, .by_group = TRUE) |>
      mutate(prev_week = lag(week)) |>
      ungroup()
  }
  d <- d |>
    left_join(last_played_before(played), by = c("player_id", "season", "week")) |>
    mutate(
      played_prior   = !is.na(prev_week),
      missed_last    = played_prior & (week - prev_week >= 2),
      return_week    = missed_last   # mid-season return (debut rows have no prev_week)
    )

  # Teammates ABOVE player i at week W: same team, higher ex-ante rolling
  # share entering W... share is backward-looking, so knowable pre-kickoff.
  # For each (team, season, week), take teammates from the WEEK W-1 roster
  # (played W-1) with their entering-W-1 share; absence in W = no row in W.
  roster_prev <- d |>
    select(season, week, posteam, tm_id = player_id, tm_share = share) |>
    mutate(week = week + 1)   # entering-week-W view of who played W-1

  played_w <- played |> transmute(season, week, tm_id = player_id, played_w = TRUE)

  above <- d |>
    select(player_id, season, week, posteam, share) |>
    inner_join(roster_prev, by = c("season", "week", "posteam"),
               relationship = "many-to-many") |>
    filter(tm_id != player_id, !is.na(tm_share), !is.na(share),
           tm_share > share) |>
    left_join(played_w, by = c("season", "week", "tm_id")) |>
    mutate(tm_absent_w = is.na(played_w)) |>
    group_by(player_id, season, week) |>
    summarise(
      above_absent_share = sum(tm_share[tm_absent_w]),
      n_above_absent     = sum(tm_absent_w),
      .groups = "drop"
    )

  # Long-out: a teammate with higher share who is absent in BOTH W-1 and W.
  # Detect via the W-1 view: teammates above i entering W-1 who did not
  # play W-1 (roster from W-2) and also do not play W.
  roster_prev2 <- d |>
    select(season, week, posteam, tm_id = player_id, tm_share = share) |>
    mutate(week = week + 2)   # who played W-2, viewed from W
  played_w1 <- played |> transmute(season, week = week + 1, tm_id = player_id,
                                   played_w1 = TRUE)
  long_out <- d |>
    select(player_id, season, week, posteam, share) |>
    inner_join(roster_prev2, by = c("season", "week", "posteam"),
               relationship = "many-to-many") |>
    filter(tm_id != player_id, !is.na(tm_share), !is.na(share),
           tm_share > share) |>
    left_join(played_w,  by = c("season", "week", "tm_id")) |>
    left_join(played_w1, by = c("season", "week", "tm_id")) |>
    mutate(long_absent = is.na(played_w) & is.na(played_w1)) |>
    group_by(player_id, season, week) |>
    summarise(above_long_absent = any(long_absent), .groups = "drop")

  d <- d |>
    left_join(above,    by = c("player_id", "season", "week")) |>
    left_join(long_out, by = c("player_id", "season", "week")) |>
    mutate(
      above_absent_share = coalesce(above_absent_share, 0),
      above_new_out      = above_absent_share > 0 & !coalesce(above_long_absent, FALSE),
      above_long_out     = coalesce(above_long_absent, FALSE),
      vol_resid          = as.numeric(opportunities) - pred_vol,
      state = case_when(
        return_week & above_new_out ~ "return+above_out",
        return_week                 ~ "return_week",
        above_new_out               ~ "above_new_out",
        above_long_out              ~ "above_long_out",
        .default                    = "steady"
      )
    )

  tbl <- d |>
    group_by(state) |>
    summarise(
      n            = n(),
      mean_resid   = mean(vol_resid),
      median_resid = median(vol_resid),
      mean_abs     = mean(abs(vol_resid)),
      mean_pred    = mean(pred_vol),
      mean_obs     = mean(as.numeric(opportunities)),
      .groups = "drop"
    ) |>
    mutate(position = pos, .before = 1) |>
    arrange(desc(abs(mean_resid)))

  cli_h2("{pos}: volume residual (obs - pred opportunities) by injury state")
  print(tbl |> mutate(across(where(is.numeric) & !c(n), ~ round(.x, 2))), n = Inf)

  # The share-weighted version: does the size of the vacated share scale
  # the bias? (It should, if the mechanism is real.)
  sw <- d |>
    filter(above_new_out) |>
    mutate(vacated = cut(above_absent_share, c(0, 0.1, 0.25, Inf),
                         labels = c("small", "mid", "large"), right = FALSE)) |>
    group_by(vacated) |>
    summarise(n = n(), mean_resid = round(mean(vol_resid), 2), .groups = "drop")
  cli_h2("{pos}: above_new_out residual by vacated share size")
  print(sw, n = Inf)

  tbl
})

readr::write_csv(list_rbind(results), "output/11a_injury_shock_diagnostic.csv")
cli_alert_success("output/11a_injury_shock_diagnostic.csv")
cli_h1("11a diagnostic complete")
