# R/15a_ol_front_diagnostic.R
# Ablation ladder rung 4, step 1: SIZE the OL / opponent-front signal against
# the SHIPPED (Vegas-era, b5aeaed) chain before building anything.
#
# THE NULL HAS TEETH: the chain already carries opponent EPA-adjustments
# (def_prior, def_*_epa_adj) and opener lines, and Vegas moves on OL news.
# The question is whether TRENCH-SPECIFIC states add anything beyond the
# aggregate defensive adjustments and the market.
#
# AXES (15a0 tables; rates are ex-ante, terciles cut within season):
#   own_sack  : own-OL sack-rate allowed        (full era 2014-2025)
#   own_stuff : own-OL rush stuff-rate allowed  (full era)
#   cont      : OBSERVED starting-5 continuity state -- intact (5) / minor
#               (4) / broken (<=3); cross-season W1 rows excluded (offseason
#               turnover is a different animal than in-season disruption).
#               Observed-not-ex-ante per the 11a sizing precedent.
#   opp_sack  : opponent front sack-rate generated (full era)
#   opp_stuff : opponent front stuff-rate generated (full era)
#   box       : opponent heavy-box rate (FTN, 2022+ only)
#   blitz     : opponent blitz rate (FTN, 2022+ only)
#
# CELLS: (1) combined-EPA fold residual (13e canonical arms) + volume
# residual, position x axis-bucket; (2) shipped-probability honesty
# (deployed recal methods) by the same cells.
#
# PRE-STATED EXPECTATIONS (2026-08-01, locked before first run):
#   - Aggregate own-OL and opponent-front LEVELS flat -- priced by the
#     def adjustments and the lines.
#   - Live candidates: continuity BREAKS (this-week starter losses the
#     Tuesday market underprices; rung-1 transition-week precedent), and
#     heavy-box opponents vs RB efficiency beyond the aggregate adjustment.
#   - Prior verdict: NULL, or a thin RB/QB-side addition decided at 15b.
#
# PRE-COMMITTED PROCEED RULE (Steve sign-off 2026-08-01; rung-3 bars):
#   Advance to 15b only if
#     (a) any position x bucket with n >= 300 has |mean tot resid| >= 0.8
#         EPA (QB >= 2.0), or
#     (b) any position x threshold x bucket with n >= 400 has
#         |stated - empirical| >= 4pp.
#   Otherwise the null is PUBLISHED and the ladder moves to rung 5.

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
FTN_FLOOR         <- 2022L

# ===========================================================================
# 1. AXIS TABLE: one row per (season, week, team) with bucket columns
# ===========================================================================
cli_h1("15a step 1: axis buckets from 15a0 tables")

tabs <- readRDS("data/15a0_ol_front_tables.rds")

terc <- function(df, col, lab) {
  df |>
    group_by(season) |>
    mutate("{lab}" := factor(ntile(.data[[col]], 3), levels = 1:3,
                             labels = c("lo", "mid", "hi"))) |>
    ungroup()
}

own <- tabs$ol_rates |>
  terc("sack_rate", "own_sack_b") |>
  terc("stuff_rate", "own_stuff_b") |>
  select(season, week, team, own_sack_b, own_stuff_b) |>
  left_join(
    tabs$ol_continuity |>
      mutate(cont_b = case_when(
        w1_cross_season   ~ NA_character_,
        ol_overlap == 5L  ~ "intact",
        ol_overlap == 4L  ~ "minor",
        TRUE              ~ "broken"
      ) |> factor(levels = c("intact", "minor", "broken"))) |>
      select(season, week, team, cont_b),
    by = c("season", "week", "team")
  )

opp <- tabs$front_rates |>
  terc("sack_rate", "opp_sack_b") |>
  terc("stuff_rate", "opp_stuff_b") |>
  select(season, week, team, opp_sack_b, opp_stuff_b) |>
  left_join(
    tabs$ftn_front |>
      terc("heavy_box_rate", "box_b") |>
      terc("blitz_rate", "blitz_b") |>
      select(season, week, team, box_b, blitz_b),
    by = c("season", "week", "team")
  )

# game -> (team, opponent) map for joining opponent axes
sched <- nflreadr::load_schedules(2014:2025) |> filter(game_type == "REG")
team_opp <- bind_rows(
  sched |> transmute(game_id, season, week, posteam = home_team, opponent = away_team),
  sched |> transmute(game_id, season, week, posteam = away_team, opponent = home_team)
)

axis_tbl <- team_opp |>
  left_join(own, by = c("season", "week", "posteam" = "team")) |>
  left_join(opp, by = c("season", "week", "opponent" = "team")) |>
  select(game_id, posteam, own_sack_b, own_stuff_b, cont_b,
         opp_sack_b, opp_stuff_b, box_b, blitz_b)

AXES <- c(own_sack = "own_sack_b", own_stuff = "own_stuff_b",
          cont = "cont_b", opp_sack = "opp_sack_b",
          opp_stuff = "opp_stuff_b", box = "box_b", blitz = "blitz_b")

cli_h2("Axis coverage (player-side joins come next)")
print(axis_tbl |> summarise(across(all_of(unname(AXES)), ~ mean(!is.na(.x)))) |>
        pivot_longer(everything()) |> as.data.frame(), row.names = FALSE)

# ===========================================================================
# 2. CELL 1: FOLD RESIDUALS BY AXIS BUCKET (13e canonical arms)
# ===========================================================================
cli_h1("15a step 2: shipped fold residuals by OL/front bucket")

team_key <- function(table_path) {
  readRDS(table_path) |>
    filter(!is.na(player_id)) |>
    distinct(player_id, season, week, game_id, posteam)
}

POS <- list(
  RB = list(preds = "output/13e_rb_fold_predictions.csv", table = "data/rb_feature_table.rds"),
  WR = list(preds = "output/13e_wr_fold_predictions.csv", table = "data/wr_feature_table.rds"),
  TE = list(preds = "output/13e_te_fold_predictions.csv", table = "data/te_feature_table.rds")
)

resid_rows <- imap(POS, function(cfg, pos) {
  readr::read_csv(cfg$preds, show_col_types = FALSE) |>
    filter(!is.na(player_id)) |>
    transmute(position = pos, player_id, season, week,
              vol_resid = as.numeric(opportunities) - pred_vol,
              tot_resid = total_epa - pred_tot) |>
    inner_join(team_key(cfg$table), by = c("player_id", "season", "week")) |>
    inner_join(axis_tbl, by = c("game_id", "posteam"))
}) |> list_rbind()

qb_rows <- readr::read_csv("output/13e_qb_fold_predictions.csv", show_col_types = FALSE) |>
  filter(!is.na(player_id)) |>
  transmute(position = "QB", player_id, season, week,
            vol_resid = as.numeric(dropbacks) - pred_db,
            tot_resid = total_epa - pred_tot) |>
  inner_join(team_key("data/qb_feature_table.rds"), by = c("player_id", "season", "week")) |>
  inner_join(axis_tbl, by = c("game_id", "posteam"))

all_resid <- bind_rows(resid_rows, qb_rows)
cli_alert_success("Joined player-weeks: {paste(count(all_resid, position)$position, count(all_resid, position)$n, sep='=', collapse=' | ')}")

cell1 <- map(AXES, function(bcol) {
  all_resid |>
    filter(!is.na(.data[[bcol]])) |>
    group_by(position, bucket = .data[[bcol]]) |>
    summarise(n = n(), mean_tot_resid = mean(tot_resid),
              se = sd(tot_resid) / sqrt(n()),
              mean_vol_resid = mean(vol_resid), .groups = "drop")
}) |> list_rbind(names_to = "axis")

cli_h2("Cell 1: combined-EPA residual by bucket")
print(cell1 |>
        mutate(mean_tot_resid = fmt(mean_tot_resid), se = round(se, 2),
               mean_vol_resid = fmt(mean_vol_resid)) |>
        arrange(axis, position, bucket) |>
        as.data.frame(), row.names = FALSE)

# ===========================================================================
# 3. CELL 2: SHIPPED-PROBABILITY HONESTY BY AXIS BUCKET
# ===========================================================================
cli_h1("15a step 3: shipped probability honesty by OL/front bucket")

fp_maps <- readRDS("data/fp_recal_maps.rds")
te_maps <- readRDS("data/te_fp_recal_maps.rds")
qb_maps <- readRDS("data/qb_fp_recal_maps.rds")
col_of <- function(stem, method) if (method == "raw") stem else paste0(stem, "_", method)

RECAL <- list(
  RB = list(file = "output/06c_recal_probabilities.csv", filter_pos = "RB",
            table = "data/rb_feature_table.rds",
            start = col_of("p_start", fp_maps[["RB_15+"]]$method),
            boom  = col_of("p_boom",  fp_maps[["RB_20+"]]$method)),
  WR = list(file = "output/06c_recal_probabilities.csv", filter_pos = "WR",
            table = "data/wr_feature_table.rds",
            start = col_of("p_start", fp_maps[["WR_15+"]]$method),
            boom  = col_of("p_boom",  fp_maps[["WR_20+"]]$method)),
  TE = list(file = "output/12e_te_recal_probabilities.csv", filter_pos = "TE",
            table = "data/te_feature_table.rds",
            start = col_of("p_start", te_maps[["TE_12+"]]$method),
            boom  = col_of("p_boom",  te_maps[["TE_17+"]]$method)),
  QB = list(file = "output/09b_qb_recal_probabilities.csv", filter_pos = NA,
            table = "data/qb_feature_table.rds",
            start = col_of("p_start", qb_maps[["QB_20+"]]$method),
            boom  = col_of("p_boom",  qb_maps[["QB_25+"]]$method))
)

cal_rows <- imap(RECAL, function(cfg, pos) {
  df <- readr::read_csv(cfg$file, show_col_types = FALSE)
  if (!is.na(cfg$filter_pos)) df <- df |> filter(position == cfg$filter_pos)
  df |>
    transmute(position = pos, player_id, season, week,
              p_start = .data[[cfg$start]], p_boom = .data[[cfg$boom]],
              hit_start, hit_boom) |>
    inner_join(team_key(cfg$table), by = c("player_id", "season", "week")) |>
    inner_join(axis_tbl, by = c("game_id", "posteam"))
}) |> list_rbind()

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

cli_h2("Cell 2: honesty by bucket (delta pp = emp - stated)")
print(cell2 |>
        mutate(stated = round(stated, 3), emp = round(emp, 3),
               delta_pp = fmt(delta_pp, 1)) |>
        arrange(axis, position, threshold, bucket) |>
        as.data.frame(), row.names = FALSE)

# ===========================================================================
# 4. VERDICT (pre-committed)
# ===========================================================================
cli_h1("15a verdict (pre-committed rule)")

res_trigger <- cell1 |>
  mutate(bar = if_else(position == "QB", PROCEED_RES_QB, PROCEED_RES_EPA)) |>
  filter(n >= PROCEED_RES_MIN_N, abs(mean_tot_resid) >= bar)
cal_trigger <- cell2 |>
  filter(n >= PROCEED_CAL_MIN_N, abs(delta_pp) >= PROCEED_CAL_PP)

if (nrow(res_trigger) == 0 && nrow(cal_trigger) == 0) {
  cli_alert_success("NO TRIGGER: all residual cells < {PROCEED_RES_EPA} EPA (QB < {PROCEED_RES_QB}) and all calibration cells < {PROCEED_CAL_PP}pp at the pre-committed n floors.")
  cli_alert_success("RUNG 4 VERDICT: NULL -- def adjustments + the market already price the trenches. Publish the receipt; ladder moves to rung 5.")
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
  cli_alert_warning("RUNG 4 VERDICT: PROCEED to 15b (thin addition, shape decided there; ex-ante Friday-lock reconstruction required for any continuity feature).")
}

readr::write_csv(cell1, "output/15a_olfront_residuals.csv")
readr::write_csv(cell2, "output/15a_olfront_calibration.csv")
cli_alert_success("output/15a_olfront_{{residuals,calibration}}.csv")
cli_h1("15a complete")
