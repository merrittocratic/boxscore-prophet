# R/13e_promote_opener_arms.R
# Rung 2 ship pass, step 1: promote the OPENER Vegas arms to the canonical
# backtest fold predictions consumed by the FP translation chain.
#
# WHY A MERGE: the 13b/13c A/B outputs carry the retrained component's
# columns (tot arms for RB/WR/TE; tot + pass_eff arms for QB) but not the
# untouched components' interval frames, which the FP simulations need
# (06b/12d use tot + vol; 09a uses pass_eff/db/rush/carry). The untouched
# components REPRODUCED the shipped models at ~1e-15 (13b/13c integrity
# gates), so their interval columns are taken verbatim from the shipped
# files. Identity is re-verified here per position before writing.
#
# INPUTS (shipped arm -> opener arm):
#   RB: output/11c_rb_injury_fold_predictions.csv + 13b_rb_vegas_open_*
#   WR: output/04c_wr_asym_fold_predictions.csv   + 13b_wr_vegas_open_*
#   TE: output/12c_te_asym_fold_predictions.csv   + 13b_te_vegas_open_*
#   QB: output/08c_qb_fold_predictions.csv        + 13c_qb_vegas_open_*
#       (materialized run: pass_eff arms present)
# OUTPUTS (canonical for the Vegas-era chain):
#   output/13e_{rb,wr,te,qb}_fold_predictions.csv
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

cli_h1("13e: promote opener arms to canonical fold predictions")

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
  tot_cols <- grep("(_tot$)|(^pred_tot$)|(^med_tot$)", names(open), value = TRUE)
  merged <- shipped |>
    select(-all_of(intersect(tot_cols, names(shipped)))) |>
    inner_join(open |> select(player_id, season, week, pred_eff_open = pred_eff,
                              all_of(tot_cols)),
               by = c("player_id", "season", "week"))

  # Identity gate: untouched vol predictions must match to machine precision
  chk <- merged |>
    inner_join(open |> select(player_id, season, week, pv = pred_vol),
               by = c("player_id", "season", "week"))
  max_d <- max(abs(chk$pred_vol - chk$pv))
  if (max_d > 1e-6) cli_abort("{pos}: vol identity failed ({format(max_d, scientific = TRUE)})")

  # The eff point prediction changed with the Vegas features: replace it
  merged <- merged |> mutate(pred_eff = pred_eff_open) |> select(-pred_eff_open)

  if (nrow(merged) != nrow(open)) {
    cli_abort("{pos}: row mismatch merged={nrow(merged)} open={nrow(open)}")
  }
  readr::write_csv(merged, out_path)
  cli_alert_success("{pos}: {out_path} ({nrow(merged)} rows; vol identity {format(max_d, scientific = TRUE)})")
}

promote_two_component("RB", "output/11c_rb_injury_fold_predictions.csv",
                      "output/13b_rb_vegas_open_fold_predictions.csv",
                      "output/13e_rb_fold_predictions.csv")
promote_two_component("WR", "output/04c_wr_asym_fold_predictions.csv",
                      "output/13b_wr_vegas_open_fold_predictions.csv",
                      "output/13e_wr_fold_predictions.csv")
promote_two_component("TE", "output/12c_te_asym_fold_predictions.csv",
                      "output/13b_te_vegas_open_fold_predictions.csv",
                      "output/13e_te_fold_predictions.csv")

# --- QB: four components; pass_eff arms from the materialized opener run,
# db/rush/carry frames verbatim from shipped ---
qb_shipped <- readr::read_csv("output/08c_qb_fold_predictions.csv", show_col_types = FALSE) |>
  filter(!is.na(player_id))
qb_open <- readr::read_csv("output/13c_qb_vegas_open_fold_predictions.csv", show_col_types = FALSE) |>
  filter(!is.na(player_id))

qb_open_cols <- grep("(_tot$)|(_pass_eff$)|(^pred_tot$)|(^pred_pass_eff$)",
                     names(qb_open), value = TRUE)
if (!"lo_80_pass_eff" %in% qb_open_cols) {
  cli_abort("QB opener file lacks pass_eff arms -- run the 13c materialize pass first (REUSE_TUNE_LOG).")
}

qb_merged <- qb_shipped |>
  select(-all_of(intersect(qb_open_cols, names(qb_shipped)))) |>
  inner_join(qb_open |> select(player_id, season, week, all_of(qb_open_cols),
                               chk_db = pred_db, chk_carry = pred_carry,
                               chk_rush = pred_rush),
             by = c("player_id", "season", "week"))

max_d <- max(abs(qb_merged$pred_db - qb_merged$chk_db),
             abs(qb_merged$pred_carry - qb_merged$chk_carry),
             abs(qb_merged$pred_rush - qb_merged$chk_rush))
if (max_d > 1e-6) cli_abort("QB: component identity failed ({format(max_d, scientific = TRUE)})")
qb_merged <- qb_merged |> select(-chk_db, -chk_carry, -chk_rush)

if (nrow(qb_merged) != nrow(qb_open)) cli_abort("QB: row mismatch")
readr::write_csv(qb_merged, "output/13e_qb_fold_predictions.csv")
cli_alert_success("QB: output/13e_qb_fold_predictions.csv ({nrow(qb_merged)} rows; component identity {format(max_d, scientific = TRUE)})")

cli_h1("13e complete -- canonical Vegas-era fold predictions written")
