# R/13e_promote_opener_arms_volfix.R
# VOLFIX CLONE of R/13e_promote_opener_arms.R: promotes the OPENER Vegas
# arms to canonical fold predictions using the volume-carryforward-fixed
# baseline instead of the old shipped models.
#
# WHY A SEPARATE FILE (not a routed-around check): 13e has its own identity
# gate (original lines ~59-64: `max_d <- max(abs(chk$pred_vol - chk$pv));
# if (max_d > 1e-6) cli_abort(...)`) verifying that the "shipped" file and
# the 13b opener-arm output agree on untouched volume predictions before
# merging. That check exists because normally volume is untouched by the
# Vegas experiment. The volume-carryforward fix intentionally changed
# volume predictions (baseline_* carryforward features, 627e909 for
# RB/WR + the TE volfix retrain alongside this file), so pointing the
# ORIGINAL 13e at the volfix 13b outputs would make the gate correctly
# abort -- it would be comparing the OLD shipped pred_vol against the NEW
# volfix-trained pred_vol, which structurally differ. That is the gate
# doing its job. This clone instead redefines "shipped" to mean the volfix
# retrain, so the same 1e-6 identity check now certifies against the
# CORRECT (volfix) baseline.
#
# CHANGES vs 13e_promote_opener_arms.R (merge logic byte-identical):
#   - "shipped" inputs are the volfix fold predictions instead of the old
#     shipped ones:
#       RB: output/11c_rb_injury_fold_predictions_volfix.csv
#       WR: output/04c_wr_asym_fold_predictions_volfix.csv
#       TE: output/12c_te_asym_fold_predictions_volfix.csv
#   - "open" inputs are the volfix 13b opener-arm outputs (this file's
#     sibling, R/13b_vegas_ab_volfix.R, run with
#     VEGAS_LINES_RDS=data/vegas_open_lines.rds VEGAS_TAG=open):
#       RB: output/13b_rb_vegas_volfix_open_fold_predictions.csv
#       WR: output/13b_wr_vegas_volfix_open_fold_predictions.csv
#       TE: output/13b_te_vegas_volfix_open_fold_predictions.csv
#   - Outputs go to output/13e_{rb,wr,te}_fold_predictions_volfix.csv so
#     the original 13e canonical files are never overwritten.
#   - QB is OUT OF SCOPE: the volume-carryforward fix and its retrains
#     (627e909 RB/WR, the TE retrain alongside this file) only touched
#     RB/WR/TE. QB's volume components (db_vol, carry_vol) were never
#     retrained with baseline_* features, so there is no QB volfix
#     baseline to promote against. This file omits the QB block entirely
#     rather than silently promoting an untouched QB chain under a
#     "volfix" name.
#
# --- Original 13e header below, unchanged in substance ---
#
# Rung 2 ship pass, step 1: promote the OPENER Vegas arms to the canonical
# backtest fold predictions consumed by the FP translation chain.
#
# WHY A MERGE: the 13b A/B outputs carry the retrained component's columns
# (tot arm) but not the untouched component's interval frame (vol), which
# the FP simulations need (06b/12d use tot + vol). The untouched component
# REPRODUCED the volfix baseline at ~1e-15 (13b_volfix integrity gate), so
# its interval columns are taken verbatim from the volfix "shipped" file.
# Identity is re-verified here per position before writing.
#
# Known exclusion: the TE duplicate player-week (Conklin 2021-W18, table
# dup) is dropped from the TE canonical file -- the FP chain joins by
# (player_id, season, week) and a dup key would fan out downstream.
# NA-player WR rows (ghost rows) carry NA keys and are dropped here; the
# FP chain filtered them anyway (06b filter(!is.na(player_id))).

suppressPackageStartupMessages({
  library(tidyverse)
  library(cli)
})

cli_h1("13e VOLFIX: promote opener arms to canonical fold predictions (volfix baseline)")

promote_two_component <- function(pos, shipped_path, open_path, out_path) {
  shipped <- readr::read_csv(shipped_path, show_col_types = FALSE) |>
    filter(!is.na(player_id))
  open    <- readr::read_csv(open_path, show_col_types = FALSE) |>
    filter(!is.na(player_id))

  dup_keys <- bind_rows(shipped |> count(player_id, season, week),
                        open    |> count(player_id, season, week)) |>
    filter(n > 1) |> distinct(player_id, season, week)
  if (nrow(dup_keys)) {
    cli_alert_warning("{pos}: dropping {nrow(dup_keys)} duplicated player-week key{?s}")
    shipped <- shipped |> anti_join(dup_keys, by = c("player_id", "season", "week"))
    open    <- open    |> anti_join(dup_keys, by = c("player_id", "season", "week"))
  }

  # tot arm columns (incl. med_ if asym) + prediction points from the
  # opener arm; everything else (vol/eff frames, keys, alpha) from shipped
  # (volfix baseline)
  tot_cols <- grep("(_tot$)|(^pred_tot$)|(^med_tot$)", names(open), value = TRUE)
  merged <- shipped |>
    select(-all_of(intersect(tot_cols, names(shipped)))) |>
    inner_join(open |> select(player_id, season, week, pred_eff_open = pred_eff,
                              all_of(tot_cols)),
               by = c("player_id", "season", "week"))

  # Identity gate: untouched vol predictions must match to machine precision
  # (both sides now trace back to the volfix VOL_FEATURES + tune log)
  chk <- merged |>
    inner_join(open |> select(player_id, season, week, pv = pred_vol),
               by = c("player_id", "season", "week"))
  max_d <- max(abs(chk$pred_vol - chk$pv))
  if (max_d > 1e-6) cli_abort("{pos}: vol identity failed vs VOLFIX baseline ({format(max_d, scientific = TRUE)})")

  # The eff point prediction changed with the Vegas features: replace it
  merged <- merged |> mutate(pred_eff = pred_eff_open) |> select(-pred_eff_open)

  if (nrow(merged) != nrow(open)) {
    cli_abort("{pos}: row mismatch merged={nrow(merged)} open={nrow(open)}")
  }
  readr::write_csv(merged, out_path)
  cli_alert_success("{pos}: {out_path} ({nrow(merged)} rows; vol identity vs volfix baseline {format(max_d, scientific = TRUE)})")
}

promote_two_component("RB", "output/11c_rb_injury_fold_predictions_volfix.csv",
                      "output/13b_rb_vegas_volfix_open_fold_predictions.csv",
                      "output/13e_rb_fold_predictions_volfix.csv")
promote_two_component("WR", "output/04c_wr_asym_fold_predictions_volfix.csv",
                      "output/13b_wr_vegas_volfix_open_fold_predictions.csv",
                      "output/13e_wr_fold_predictions_volfix.csv")
promote_two_component("TE", "output/12c_te_asym_fold_predictions_volfix.csv",
                      "output/13b_te_vegas_volfix_open_fold_predictions.csv",
                      "output/13e_te_fold_predictions_volfix.csv")

cli_h1("13e VOLFIX complete -- canonical Vegas-era fold predictions written (RB/WR/TE only; QB out of scope, see header)")
