# R/11b_injury_state_layer.R
# Ablation ladder rung 1: EX-ANTE injury state-machine features.
#
# Materializes, for every feature-table player-week (RB/WR/QB), the injury
# state knowable at FRIDAY LOCK of that week:
#
# Own state:
#   own_q_int            report status Questionable (played-anyway rows;
#                        Out/Doubtful players rarely have table rows)
#   own_practice_int     final practice: 0 DNP, 1 Limited, 2 Full,
#                        3 not on report (healthy)
#   return_from_absence  played earlier this season, missed >= 1 week,
#                        back this week (snap-ramp state; diagnostic bias
#                        -1.24 RB touches)
#   weeks_missed         gap length entering this week (0 = played last
#                        week; capped at 8; season debut = 0, cold-start
#                        features own that regime)
#
# Depth-chart-above state (RB/WR; share = the position's ex-ante rolling
# usage share, so "above" is knowable pre-kickoff):
#   above_new_out_share  summed entering share of higher-share teammates
#                        who played W-1 but are designated Out/Doubtful on
#                        the Friday report for W (the fresh shock;
#                        diagnostic bias +2.55 RB touches, dose-responsive)
#   above_q_share        same, teammates designated Questionable
#   above_long_out_share summed share of higher-share teammates absent
#                        W-1 (from the W-2 roster) with no Friday return
#                        signal at W (practice Limited/Full)
#
# FRIDAY-LOCK MASKING: report rows modified after lock (Wednesday night
# for Thursday games, Friday night otherwise; ~7% of rows, mostly
# Saturday downgrades) have report_status masked to NA -- the final
# designation was not knowable at lock. practice_status is kept: the
# practice log (Wed-Fri) predates any Saturday modification. 2025 rows
# carry no date_modified and cannot be masked -- documented approximation,
# rate sized from 2014-2024 below. Late-breaking news is the ROUTER
# override layer's territory (pre-registration), never trained features.
#
# Output: data/injury_states_{rb,wr,qb}.rds keyed (player_id, season, week)
# -- consumed by the 11c A/B backtest and (if the rubric passes) the v2
# feature tables + 10b slates.

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(cli)
})

SEASONS  <- 2014L:2025L
GAP_CAP  <- 8L

`%||%` <- function(a, b) if (is.null(a)) b else a

cli_h1("11b: ex-ante injury state layer (Friday-lock masked)")

# ===========================================================================
# 1. CLEAN INJURY REPORTS + FRIDAY-LOCK MASK
# ===========================================================================

inj_raw <- nflreadr::load_injuries(SEASONS) |>
  filter(game_type == "REG", !is.na(gsis_id))

practice_map <- c(
  "Did Not Participate In Practice"   = 0L,
  "Limited Participation in Practice" = 1L,
  "Full Participation in Practice"    = 2L,
  "Out (Definitely Will Not Play)"    = 0L
)

inj <- inj_raw |>
  mutate(
    practice_int = practice_map[practice_status],   # junk/NA -> NA
    report_std   = case_when(
      report_status %in% c("Out")          ~ "out",
      report_status %in% c("Doubtful")     ~ "doubtful",
      report_status %in% c("Questionable") ~ "questionable",
      report_status %in% c("Probable")     ~ "probable",   # pre-2016
      .default                             = NA_character_
    )
  ) |>
  group_by(season, week, gsis_id) |>
  arrange(desc(date_modified), .by_group = TRUE) |>   # 4 dup rows: keep latest
  slice_head(n = 1) |>
  ungroup()

# Friday lock per team-week from the schedule: Wednesday 23:59 ET before
# Thursday games, Friday 23:59 ET otherwise (Sat/Sun/Mon games).
sched <- nflreadr::load_schedules(SEASONS) |>
  filter(game_type == "REG") |>
  select(season, week, gameday, weekday, home_team, away_team) |>
  pivot_longer(c(home_team, away_team), values_to = "team") |>
  mutate(
    gd = as.Date(gameday),
    lock_date = case_when(
      weekday == "Thursday" ~ gd - 1,                       # Wednesday
      weekday == "Saturday" ~ gd - 1,                       # Friday
      weekday == "Sunday"   ~ gd - 2,                       # Friday
      weekday == "Monday"   ~ gd - 3,                       # Friday
      .default              = gd - 2
    ),
    lock_ts = as.POSIXct(paste(lock_date, "23:59:59"), tz = "America/New_York")
  ) |>
  select(season, week, team, lock_ts)

inj <- inj |>
  left_join(sched, by = c("season", "week", "team")) |>
  mutate(
    post_lock = !is.na(date_modified) & !is.na(lock_ts) & date_modified > lock_ts,
    report_std = if_else(post_lock, NA_character_, report_std)
    # practice_int intentionally kept on post_lock rows (pre-lock info)
  )

mask_rate <- inj |>
  filter(season < 2025) |>
  summarise(pct = 100 * mean(post_lock)) |> pull(pct)
cli_alert_info("Friday-lock mask: {round(mask_rate, 1)}% of 2014-2024 report rows had post-lock modifications (report_status masked)")
cli_alert_warning("2025 has no date_modified -- unmaskable, accepted approximation at ~{round(mask_rate, 1)}% rate")

inj_slim <- inj |>
  select(player_id = gsis_id, season, week, report_std, practice_int)

# ===========================================================================
# 2. PER-POSITION STATE MATERIALIZATION
# ===========================================================================

POSITIONS <- list(
  rb = list(table = "data/rb_feature_table.rds", share = "wt_carry_share",  above = TRUE),
  wr = list(table = "data/wr_feature_table.rds", share = "wt_target_share", above = TRUE),
  qb = list(table = "data/qb_feature_table.rds", share = NULL,              above = FALSE)
)

build_states <- function(cfg, pos) {
  ft <- readRDS(cfg$table) |> filter(!is.na(player_id))
  base <- ft |>
    select(player_id, season, week, posteam,
           share = any_of(cfg$share %||% character(0)))

  played <- base |> distinct(player_id, season, week)

  # --- own state ---
  own <- base |>
    left_join(inj_slim, by = c("player_id", "season", "week")) |>
    group_by(player_id, season) |>
    arrange(week, .by_group = TRUE) |>
    mutate(prev_week = lag(week)) |>
    ungroup() |>
    mutate(
      own_q_int           = as.integer(!is.na(report_std) & report_std == "questionable"),
      own_practice_int    = coalesce(practice_int, 3L),   # not listed = healthy
      weeks_missed        = pmin(coalesce(week - prev_week - 1L, 0L), GAP_CAP),
      return_from_absence = as.integer(weeks_missed >= 1L)
    )

  out <- own |> select(player_id, season, week,
                       own_q_int, own_practice_int,
                       weeks_missed, return_from_absence)

  if (isTRUE(cfg$above)) {
    inj_flags <- inj_slim |>
      transmute(season, week, tm_id = player_id,
                tm_out  = !is.na(report_std) & report_std %in% c("out", "doubtful"),
                tm_q    = !is.na(report_std) & report_std == "questionable",
                tm_return_signal = !is.na(practice_int) & practice_int >= 1L)

    played_at <- function(offset) {
      played |> transmute(season, week = week + offset, tm_id = player_id, flag = TRUE)
    }

    # Above-set from the W-1 played roster (fresh-shock candidates)
    roster_w1 <- base |>
      select(season, week, posteam, tm_id = player_id, tm_share = share) |>
      mutate(week = week + 1L)
    new_out <- base |>
      inner_join(roster_w1, by = c("season", "week", "posteam"),
                 relationship = "many-to-many") |>
      filter(tm_id != player_id, !is.na(tm_share), !is.na(share),
             tm_share > share) |>
      left_join(inj_flags, by = c("season", "week", "tm_id")) |>
      group_by(player_id, season, week) |>
      summarise(
        above_new_out_share = sum(tm_share[coalesce(tm_out, FALSE)]),
        above_q_share       = sum(tm_share[coalesce(tm_q,  FALSE)]),
        .groups = "drop"
      )

    # Above-set from the W-2 roster who missed W-1: still absent at W unless
    # the Friday report shows a practice return signal (Limited/Full)
    roster_w2 <- base |>
      select(season, week, posteam, tm_id = player_id, tm_share = share) |>
      mutate(week = week + 2L)
    long_out <- base |>
      inner_join(roster_w2, by = c("season", "week", "posteam"),
                 relationship = "many-to-many") |>
      filter(tm_id != player_id, !is.na(tm_share), !is.na(share),
             tm_share > share) |>
      left_join(played_at(1L), by = c("season", "week", "tm_id")) |>   # played W-1?
      left_join(inj_flags, by = c("season", "week", "tm_id")) |>
      filter(is.na(flag)) |>                                           # missed W-1
      mutate(still_absent = coalesce(tm_out, FALSE) |
               !coalesce(tm_return_signal, FALSE)) |>
      group_by(player_id, season, week) |>
      summarise(above_long_out_share = sum(tm_share[still_absent]), .groups = "drop")

    out <- out |>
      left_join(new_out,  by = c("player_id", "season", "week")) |>
      left_join(long_out, by = c("player_id", "season", "week")) |>
      mutate(across(c(above_new_out_share, above_q_share, above_long_out_share),
                    ~ coalesce(.x, 0)))
  }

  path <- sprintf("data/injury_states_%s.rds", pos)
  saveRDS(out, path)
  cli_alert_success("{path}: {nrow(out)} player-weeks | return: {sum(out$return_from_absence)} | own Q: {sum(out$own_q_int)}{if (isTRUE(cfg$above)) paste0(' | above_new_out>0: ', sum(out$above_new_out_share > 0)) else ''}")
  out
}

states <- imap(POSITIONS, build_states)

# ===========================================================================
# 3. VALIDATION: ex-ante states vs the 11a observed-state diagnostic
# ===========================================================================

cli_h1("Validation: RB fold residuals inside EX-ANTE states (the trainable signal)")

rb_preds <- readr::read_csv("output/03a_v2_lgbm_fold_predictions.csv",
                            show_col_types = FALSE) |>
  filter(!is.na(player_id)) |>
  select(player_id, season, week, opportunities, pred_vol)

rb_val <- states$rb |>
  inner_join(rb_preds, by = c("player_id", "season", "week")) |>
  mutate(
    vol_resid = as.numeric(opportunities) - pred_vol,
    state = case_when(
      return_from_absence == 1 & above_new_out_share > 0 ~ "return+above_out",
      return_from_absence == 1                            ~ "return_week",
      above_new_out_share > 0                             ~ "above_new_out",
      above_long_out_share > 0                            ~ "above_long_out",
      above_q_share > 0                                   ~ "above_q_only",
      .default                                            = "steady"
    )
  )

val_tbl <- rb_val |>
  group_by(state) |>
  summarise(n = n(), mean_resid = round(mean(vol_resid), 2),
            median_resid = round(median(vol_resid), 2),
            mean_pred = round(mean(pred_vol), 1), .groups = "drop") |>
  arrange(desc(abs(mean_resid)))
print(val_tbl, n = Inf)

readr::write_csv(val_tbl, "output/11b_exante_state_validation.csv")
cli_alert_success("output/11b_exante_state_validation.csv")

cli_h1("11b complete -- injury state layer materialized")
