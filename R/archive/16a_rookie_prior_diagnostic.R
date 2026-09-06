# R/16a_rookie_prior_diagnostic.R
# Ablation ladder rung 5 (final pre-registered rung): SIZE the rookie-prior
# signal against the SHIPPED (Vegas-era, b5aeaed) chain.
#
# THE EXISTING PRIOR: feature layers give rookies is_cold_start = TRUE and
# a draft_tier-median baseline_epa_per_opp; the volume-conditional recal
# maps (6c walk-forward) already own the low-usage strata where rookies
# live. The question is whether rookie-SPECIFIC residual signal survives
# that machinery.
#
# CELLS (rookie/draft info from load_players; is_rookie = season ==
# rookie_season; no prep script -- the join is player-season level):
#   cohort : veteran vs rookie_all
#   tier   : rookies only -- r1 / day2 (R2-3) / day3_udfa (R4-7 + undrafted)
#   phase  : rookies only -- w1_4 / w5plus
#
# PRE-STATED EXPECTATIONS (2026-08-01, locked before first run):
#   - Aggregate rookie cells near-flat (recal owns low-usage).
#   - Live candidates: rookie weeks 1-4 (prior does all the work; tier
#     medians coarse), R1 rookie QBs (star-shrinkage limitation +
#     rushing-tier axis), day-3/UDFA VOLUME residual positive (breakout
#     class arrives faster than tier medians expect).
#   - Prior verdict: NULL or thin; 16b decides any richer prior (college
#     production percentiles buildable; draft-model .pred needs historical
#     fold backfill -- separate cross-repo project).
#
# PRE-COMMITTED PROCEED RULE (Steve sign-off 2026-08-01; ladder bars):
#   Advance to 16b only if
#     (a) any cell with n >= 300 has |mean tot resid| >= 0.8 EPA (QB >= 2.0), or
#     (b) any cell x threshold with n >= 400 has |stated - empirical| >= 4pp.
#   Below-floor cells are reported, non-triggering (windy15 precedent).
#   Otherwise the null is PUBLISHED and the pre-registered ladder is
#   COMPLETE (rest/travel was batched with rung 2).

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(cli)
})

fmt <- function(x, d = 2) sprintf("%+.*f", d, x)

PROCEED_RES_EPA   <- 0.8
PROCEED_RES_QB    <- 2.0
PROCEED_RES_MIN_N <- 300L
PROCEED_CAL_PP    <- 4.0
PROCEED_CAL_MIN_N <- 400L

# ===========================================================================
# 1. ROOKIE / DRAFT-CAPITAL KEY
# ===========================================================================
cli_h1("16a step 1: rookie + draft-capital key")

rookie_key <- nflreadr::load_players() |>
  transmute(player_id = gsis_id, rookie_season, draft_round)

annotate <- function(df) {
  df |>
    left_join(rookie_key, by = "player_id") |>
    mutate(
      is_rookie = !is.na(rookie_season) & season == rookie_season,
      cohort = if_else(is_rookie, "rookie_all", "veteran"),
      tier = case_when(
        !is_rookie                ~ NA_character_,
        draft_round == 1          ~ "r1",
        draft_round %in% 2:3      ~ "day2",
        TRUE                      ~ "day3_udfa"   # R4-7 and undrafted
      ),
      phase = case_when(
        !is_rookie ~ NA_character_,
        week <= 4  ~ "w1_4",
        TRUE       ~ "w5plus"
      )
    )
}

AXES <- c(cohort = "cohort", tier = "tier", phase = "phase")

# ===========================================================================
# 2. CELL 1: FOLD RESIDUALS (13e canonical arms)
# ===========================================================================
cli_h1("16a step 2: shipped fold residuals by rookie cell")

POS <- list(
  RB = "output/13e_rb_fold_predictions.csv",
  WR = "output/13e_wr_fold_predictions.csv",
  TE = "output/13e_te_fold_predictions.csv"
)

resid_rows <- imap(POS, function(path, pos) {
  readr::read_csv(path, show_col_types = FALSE) |>
    filter(!is.na(player_id)) |>
    transmute(position = pos, player_id, season, week,
              vol_resid = as.numeric(opportunities) - pred_vol,
              tot_resid = total_epa - pred_tot)
}) |> list_rbind()

qb_rows <- readr::read_csv("output/13e_qb_fold_predictions.csv", show_col_types = FALSE) |>
  filter(!is.na(player_id)) |>
  transmute(position = "QB", player_id, season, week,
            vol_resid = as.numeric(dropbacks) - pred_db,
            tot_resid = total_epa - pred_tot)

all_resid <- bind_rows(resid_rows, qb_rows) |> annotate()
cli_alert_success("Player-weeks: {paste(count(all_resid, position)$position, count(all_resid, position)$n, sep='=', collapse=' | ')} | rookie share {round(100 * mean(all_resid$is_rookie), 1)}%")

cell1 <- map(AXES, function(bcol) {
  all_resid |>
    filter(!is.na(.data[[bcol]])) |>
    group_by(position, bucket = .data[[bcol]]) |>
    summarise(n = n(), mean_tot_resid = mean(tot_resid),
              se = sd(tot_resid) / sqrt(n()),
              mean_vol_resid = mean(vol_resid), .groups = "drop")
}) |> list_rbind(names_to = "axis")

cli_h2("Cell 1: combined-EPA residual by rookie cell")
print(cell1 |>
        mutate(below_floor = if_else(n < PROCEED_RES_MIN_N, "*", ""),
               mean_tot_resid = fmt(mean_tot_resid), se = round(se, 2),
               mean_vol_resid = fmt(mean_vol_resid)) |>
        arrange(axis, position, bucket) |>
        as.data.frame(), row.names = FALSE)

# ===========================================================================
# 3. CELL 2: SHIPPED-PROBABILITY HONESTY
# ===========================================================================
cli_h1("16a step 3: shipped probability honesty by rookie cell")

fp_maps <- readRDS("data/fp_recal_maps.rds")
te_maps <- readRDS("data/te_fp_recal_maps.rds")
qb_maps <- readRDS("data/qb_fp_recal_maps.rds")
col_of <- function(stem, method) if (method == "raw") stem else paste0(stem, "_", method)

RECAL <- list(
  RB = list(file = "output/06c_recal_probabilities.csv", filter_pos = "RB",
            start = col_of("p_start", fp_maps[["RB_15+"]]$method),
            boom  = col_of("p_boom",  fp_maps[["RB_20+"]]$method)),
  WR = list(file = "output/06c_recal_probabilities.csv", filter_pos = "WR",
            start = col_of("p_start", fp_maps[["WR_15+"]]$method),
            boom  = col_of("p_boom",  fp_maps[["WR_20+"]]$method)),
  TE = list(file = "output/12e_te_recal_probabilities.csv", filter_pos = "TE",
            start = col_of("p_start", te_maps[["TE_12+"]]$method),
            boom  = col_of("p_boom",  te_maps[["TE_17+"]]$method)),
  QB = list(file = "output/09b_qb_recal_probabilities.csv", filter_pos = NA,
            start = col_of("p_start", qb_maps[["QB_20+"]]$method),
            boom  = col_of("p_boom",  qb_maps[["QB_25+"]]$method))
)

cal_rows <- imap(RECAL, function(cfg, pos) {
  df <- readr::read_csv(cfg$file, show_col_types = FALSE)
  if (!is.na(cfg$filter_pos)) df <- df |> filter(position == cfg$filter_pos)
  df |>
    transmute(position = pos, player_id, season, week,
              p_start = .data[[cfg$start]], p_boom = .data[[cfg$boom]],
              hit_start, hit_boom)
}) |> list_rbind() |> annotate()

cell2 <- map(AXES, function(bcol) {
  base <- cal_rows |> filter(!is.na(.data[[bcol]]))
  bind_rows(
    base |> group_by(position, bucket = .data[[bcol]]) |>
      summarise(n = n(), stated = mean(p_start), emp = mean(as.numeric(hit_start)),
                .groups = "drop") |> mutate(threshold = "start"),
    base |> group_by(position, bucket = .data[[bcol]]) |>
      summarise(n = n(), stated = mean(p_boom), emp = mean(as.numeric(hit_boom)),
                .groups = "drop") |> mutate(threshold = "boom")
  ) |> mutate(delta_pp = 100 * (emp - stated))
}) |> list_rbind(names_to = "axis")

cli_h2("Cell 2: honesty by rookie cell (delta pp = emp - stated)")
print(cell2 |>
        mutate(below_floor = if_else(n < PROCEED_CAL_MIN_N, "*", ""),
               stated = round(stated, 3), emp = round(emp, 3),
               delta_pp = fmt(delta_pp, 1)) |>
        arrange(axis, position, threshold, bucket) |>
        as.data.frame(), row.names = FALSE)

# ===========================================================================
# 4. VERDICT (pre-committed)
# ===========================================================================
cli_h1("16a verdict (pre-committed rule)")

res_trigger <- cell1 |>
  mutate(bar = if_else(position == "QB", PROCEED_RES_QB, PROCEED_RES_EPA)) |>
  filter(n >= PROCEED_RES_MIN_N, abs(mean_tot_resid) >= bar)
cal_trigger <- cell2 |>
  filter(n >= PROCEED_CAL_MIN_N, abs(delta_pp) >= PROCEED_CAL_PP)

if (nrow(res_trigger) == 0 && nrow(cal_trigger) == 0) {
  cli_alert_success("NO TRIGGER: all residual cells < {PROCEED_RES_EPA} EPA (QB < {PROCEED_RES_QB}) and all calibration cells < {PROCEED_CAL_PP}pp at the pre-committed n floors.")
  cli_alert_success("RUNG 5 VERDICT: NULL -- the cold-start prior + volume-conditional recal already handle rookies. Publish the receipt; the pre-registered ladder is COMPLETE.")
} else {
  if (nrow(res_trigger) > 0) {
    cli_alert_warning("RESIDUAL TRIGGER ({nrow(res_trigger)} cell{?s}):")
    print(res_trigger |> mutate(mean_tot_resid = fmt(mean_tot_resid)) |>
            as.data.frame(), row.names = FALSE)
  }
  if (nrow(cal_trigger) > 0) {
    cli_alert_warning("CALIBRATION TRIGGER ({nrow(cal_trigger)} cell{?s}):")
    print(cal_trigger |> mutate(delta_pp = fmt(delta_pp, 1)) |>
            as.data.frame(), row.names = FALSE)
  }
  cli_alert_warning("RUNG 5 VERDICT: PROCEED to 16b (richer rookie prior; shape decided there).")
}

readr::write_csv(cell1, "output/16a_rookie_residuals.csv")
readr::write_csv(cell2, "output/16a_rookie_calibration.csv")
cli_alert_success("output/16a_rookie_{{residuals,calibration}}.csv")
cli_h1("16a complete")
