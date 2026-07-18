# R/11b_injury_state_layer.R
# Ablation ladder rung 1: EX-ANTE injury state-machine features -- historical
# materialization for training. Core computation lives in
# R/11b_injury_state_fns.R (shared with the 10b2 slate builder; the 10b2
# exact-match gate proves both call sites agree).
#
# Own state:
#   own_q_int            report status Questionable
#   own_practice_int     final practice: 0 DNP, 1 Limited, 2 Full, 3 not listed
#   return_from_absence  played earlier this season, missed >= 1 week, back
#   weeks_missed         gap length entering this week (capped 8; debut = 0)
# Depth-chart-above state (RB/WR; share = ex-ante rolling usage share):
#   above_new_out_share  entering share of higher-share teammates who played
#                        W-1 but are Out/Doubtful on the Friday report for W
#   above_q_share        same, Questionable
#   above_long_out_share share of higher-share teammates absent W-1 with no
#                        Friday practice return signal at W
#
# FRIDAY-LOCK MASKING: report rows modified after lock (Wed night for Thu
# games, Fri night otherwise; ~7% of 2014-2024 rows) have report_status
# masked -- not knowable at lock. practice_status kept (practices predate
# Saturday modifications). 2025 has no date_modified: unmaskable, accepted
# approximation. Late news = router override territory, never trained.
#
# Output: data/injury_states_{rb,wr,qb}.rds keyed (player_id, season, week).
# Validation receipt: output/11b_exante_state_validation.csv.

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(cli)
})

source("R/11b_injury_state_fns.R")

SEASONS <- 2014L:2025L

cli_h1("11b: ex-ante injury state layer (Friday-lock masked)")

inj_raw <- nflreadr::load_injuries(SEASONS)
locks   <- build_lock_table(SEASONS)
inj_slim <- clean_injury_reports(inj_raw, locks, mask = TRUE)

mask_rate <- inj_raw |>
  filter(game_type == "REG", !is.na(gsis_id), season < 2025) |>
  left_join(locks, by = c("season", "week", "team")) |>
  summarise(pct = 100 * mean(!is.na(date_modified) & !is.na(lock_ts) &
                               date_modified > lock_ts)) |> pull(pct)
cli_alert_info("Friday-lock mask: {round(mask_rate, 1)}% of 2014-2024 report rows post-lock (report_status masked)")
cli_alert_warning("2025 has no date_modified -- unmaskable, accepted approximation")

POSITIONS <- list(
  rb = list(table = "data/rb_feature_table.rds", share = "wt_carry_share",  above = TRUE),
  wr = list(table = "data/wr_feature_table.rds", share = "wt_target_share", above = TRUE),
  qb = list(table = "data/qb_feature_table.rds", share = NULL,              above = FALSE)
)

states <- imap(POSITIONS, function(cfg, pos) {
  base <- readRDS(cfg$table) |>
    filter(!is.na(player_id)) |>
    select(player_id, season, week, posteam,
           share = any_of(cfg$share %||% character(0)))
  out <- build_injury_states(base, inj_slim, above = isTRUE(cfg$above))
  path <- sprintf("data/injury_states_%s.rds", pos)
  saveRDS(out, path)
  cli_alert_success("{path}: {nrow(out)} player-weeks | return: {sum(out$return_from_absence)} | own Q: {sum(out$own_q_int)}{if (isTRUE(cfg$above)) paste0(' | above_new_out>0: ', sum(out$above_new_out_share > 0)) else ''}")
  out
})

# ===========================================================================
# VALIDATION: RB fold residuals inside ex-ante states (trainable signal)
# ===========================================================================

cli_h1("Validation: RB fold residuals inside EX-ANTE states")

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
