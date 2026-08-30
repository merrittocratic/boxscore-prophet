# R/12d0_te_predvol_rescale.R
# Rescale 12c TE fold-prediction combined intervals from OBS-VOL to PRED-VOL
# scaling. TE clone of 06b0 (same mechanics, same verification checks); see
# 06b0 header for the 2026-07-18 train/serve-skew diagnosis that motivates
# this step. TE uses Mechanism A power-law scaling, so it has the same seam.

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

  idx_same <- which(abs(pmax(preds$pred_vol, 1) - pmax(preds$opportunities, 1)) < 1e-12)
  if (length(idx_same)) {
    max_same_diff <- max(abs(as.matrix(out[idx_same, arm_cols]) -
                             as.matrix(preds[idx_same, arm_cols])))
    stopifnot(max_same_diff < 1e-9)
  }

  ord_cols <- intersect(paste0(c("lo_90_", "lo_80_", "lo_50_", "hi_50_",
                                 "hi_80_", "hi_90_"), "tot"), names(preds))
  M <- as.matrix(out[, ord_cols])
  n_inversions <- sum(M[, -1] < M[, -ncol(M)] - 1e-9)
  if (n_inversions > 0) {
    cli_alert_warning("{label}: {n_inversions} arm-order inversions (also present pre-rescale; 12d enforces monotone quantiles)")
  }

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

cli_h1("12d0: rescale TE fold-prediction tot intervals to pred-vol scaling")

# TE source = the rung-2 Vegas opener arm (13e canonical) since 2026-07-26.
#
# Overridable seam (2026-08-30, volfix recal refit): swap the input/output
# paths without touching the canonical 12d0 artifact, same pattern as the
# TE_PRED_FILE seam in 12d.
TE0_IN  <- Sys.getenv("TE0_IN",  "output/13e_te_fold_predictions.csv")
TE0_OUT <- Sys.getenv("TE0_OUT", "output/13e_te_fold_predictions_predvol.csv")

rescale_file(TE0_IN, TE0_OUT, "TE (13e Vegas arm)")

cli_h1("12d0 complete -- 12d reads the _predvol file by default")
