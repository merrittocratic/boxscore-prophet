#!/bin/zsh
# WR context rung: push BOTH arms through the identical 06b0/06b/06c
# chain via the env seams, then grade with 19c. Run AFTER
# R/19b_wr_context_ab.R has produced output/19b_wr_ctx_fold_predictions.csv.
#
#   arm A (control) = output/04c_wr_asym_fold_predictions_volfix.csv
#   arm B (context) = output/19b_wr_ctx_fold_predictions.csv
#   RB input held constant (13e volfix) so the only difference is WR.
#
# All outputs are 19c_-prefixed or scratch-bound: nothing here touches
# the shipped 06b/06c artifacts or data/ rds files.
set -e
cd "$(dirname "$0")/.."

SCRATCH="${TMPDIR:-/tmp}/wr_ctx_chain"
mkdir -p "$SCRATCH"

run_arm() {
  local arm="$1" wr_in="$2"
  echo "=== ARM ${arm}: 06b0 predvol rescale ==="
  RB0_IN=output/13e_rb_fold_predictions_volfix.csv \
  WR0_IN="$wr_in" \
  RB0_OUT="$SCRATCH/rb_predvol_shared.csv" \
  WR0_OUT="$SCRATCH/wr_${arm}_predvol.csv" \
    Rscript R/06b0_predvol_rescale.R

  echo "=== ARM ${arm}: 06b fp simulation ==="
  RB_PRED_FILE="$SCRATCH/rb_predvol_shared.csv" \
  WR_PRED_FILE="$SCRATCH/wr_${arm}_predvol.csv" \
  FP_OUT_PREFIX="19c_${arm}" \
  TRANS_FIT_OUT="$SCRATCH/trans_${arm}.rds" \
    Rscript R/06b_fp_simulation.R

  echo "=== ARM ${arm}: 06c recalibration ==="
  RECAL_PROBS_FILE="output/19c_${arm}_fp_sim_probabilities.csv" \
  RECAL_OUT_PREFIX="19c_${arm}" \
  RECAL_DEPLOY_MAPS_OUT="$SCRATCH/maps_${arm}.rds" \
    Rscript R/06c_recalibration.R
}

run_arm ctrl output/04c_wr_asym_fold_predictions_volfix.csv
run_arm ctx  output/19b_wr_ctx_fold_predictions.csv

echo "=== 19c grading ==="
Rscript R/19c_wr_context_grade.R
