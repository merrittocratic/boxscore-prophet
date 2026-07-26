# R/06b0_predvol_rescale.R
# Rescale backtest fold-prediction combined intervals from OBS-VOL to
# PRED-VOL scaling, producing the deployment-consistent inputs for the FP
# translation chain (06b -> 06c).
#
# WHY (2026-07-18, 10c reconciliation diagnosis): the backtest scaled RB/WR
# combined interval widths by OBSERVED test-week volume; deployment scores
# pre-kickoff and scales by PREDICTED volume (the documented 10a seam).
# The recal maps were therefore FIT on probabilities from obs-vol intervals
# but APPLIED to probabilities from pred-vol intervals -- a train/serve
# skew that the strat_iso step maps amplify (20-30pp cliffs). Rebuilding
# 06b/06c on pred-vol-scaled inputs removes the skew at its root and makes
# the 10c reconciliation apples-to-apples by construction.
#
# MECHANICS: for each fold row, every combined-total arm (med_/lo_/hi_ x
# 50/80/90) is an offset from pred_tot of the form q * obs_opp^alpha_fold.
# Multiplying the offset by (pmax(pred_vol, 1) / obs_opp)^alpha_fold
# converts it to q * pmax(pred_vol, 1)^alpha_fold -- exactly the width 10c
# constructs at scoring time (pred_vol floored at 1, the minimum observable
# volume in the scaling domain). Component (eff/vol) arms are untouched:
# they were never volume-scaled. QB has no seam (const mechanism) and no
# rescale.
#
# Outputs (new files; the obs-vol originals remain canonical backtest
# receipts for the EPA-interval layer):
#   output/03a_v2_lgbm_fold_predictions_predvol.csv
#   output/04c_wr_asym_fold_predictions_predvol.csv

suppressPackageStartupMessages({
  library(tidyverse)
  library(cli)
})

ARM_STEMS <- c("med_", "lo_50_", "hi_50_", "lo_80_", "hi_80_", "lo_90_", "hi_90_")

rescale_file <- function(in_path, out_path, label) {
  preds <- readr::read_csv(in_path, show_col_types = FALSE)
  stopifnot(all(c("pred_tot", "pred_vol", "opportunities", "alpha_fold")
                %in% names(preds)))

  ratio <- (pmax(preds$pred_vol, 1) / pmax(preds$opportunities, 1))^preds$alpha_fold
  arm_cols <- intersect(paste0(ARM_STEMS, "tot"), names(preds))

  out <- preds
  for (cl in arm_cols) {
    out[[cl]] <- out$pred_tot + (out[[cl]] - out$pred_tot) * ratio
  }

  # Verification 1: rows where pred_vol (floored) equals obs volume must be
  # unchanged; ratio == 1 there by construction.
  idx_same <- which(abs(pmax(preds$pred_vol, 1) - pmax(preds$opportunities, 1)) < 1e-12)
  if (length(idx_same)) {
    max_same_diff <- max(abs(as.matrix(out[idx_same, arm_cols]) -
                             as.matrix(preds[idx_same, arm_cols])))
    stopifnot(max_same_diff < 1e-9)
  }

  # Verification 2: arm ordering preserved (positive ratio preserves the
  # ordering of offsets; check anyway -- a violated input would carry over).
  ord_cols <- intersect(paste0(c("lo_90_", "lo_80_", "lo_50_", "hi_50_",
                                 "hi_80_", "hi_90_"), "tot"), names(preds))
  M <- as.matrix(out[, ord_cols])
  n_inversions <- sum(M[, -1] < M[, -ncol(M)] - 1e-9)
  if (n_inversions > 0) {
    cli_alert_warning("{label}: {n_inversions} arm-order inversions (also present pre-rescale; 06b enforces monotone quantiles)")
  }

  # Verification 3: unchanged columns are byte-identical
  keep_cols <- setdiff(names(preds), arm_cols)
  stopifnot(identical(preds[keep_cols], out[keep_cols]))

  readr::write_csv(out, out_path)

  w80_obs  <- mean(preds$hi_80_tot - preds$lo_80_tot)
  w80_pred <- mean(out$hi_80_tot  - out$lo_80_tot)
  cli_alert_success(
    "{label}: {nrow(out)} rows -> {out_path} | mean 80% tot width {round(w80_obs, 2)} -> {round(w80_pred, 2)} EPA | median ratio {round(median(ratio), 3)}"
  )
  invisible(out)
}

cli_h1("06b0: rescale fold-prediction tot intervals to pred-vol scaling")

# RB/WR source = the rung-2 Vegas opener arms (13e canonical) since
# 2026-07-26: injury states (RB vol) + opener Vegas features (eff) are both
# in the shipped chain. Pre-Vegas receipts remain in git history.
rescale_file("output/13e_rb_fold_predictions.csv",
             "output/13e_rb_fold_predictions_predvol.csv", "RB (13e Vegas arm)")
rescale_file("output/13e_wr_fold_predictions.csv",
             "output/13e_wr_fold_predictions_predvol.csv", "WR (13e Vegas arm)")

cli_h1("06b0 complete -- feed via RB_PRED_FILE / WR_PRED_FILE into 06b")
