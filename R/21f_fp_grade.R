# R/21f_fp_grade.R
# Stage C, step 3 of the D29 single-stage rebuild: ONE grader for every arm
# produced by R/21d. No arm gets bespoke scoring code -- that is how
# selective reporting creeps in.
#
# Grades an arm's RAW pred_fp (pre-recalibration -- R/21e doesn't exist yet)
# against the FROZEN discrimination baseline from R/21b, in the exact flex
# band + statistic the ship gate uses (R/21a::band_universe, disc_compare).
# The candidate's own walk-forward runs the FULL 2016-2025 window (matching
# R/21b's `ecr_full` cut, not the incumbent's 2023-2025-only window) -- so
# an arm is compared to `ecr_full`, never to `ecr_matched`/`incumbent`.
#
# WHAT THIS GRADES NOW vs LATER: pred_fp is the continuous point estimate,
# which is what R/21a's disc_compare() needs for Spearman/concordance/AUC --
# recalibration (R/21e) is a monotonic-ish transform of p_start/p_boom, not
# of pred_fp, and for CONDITIONAL recal methods (platt_vol, star_platt) it
# is not even globally monotonic in the raw probability, so it CAN change
# ordering. Until R/21e exists, this script reports pred_fp discrimination
# only, explicitly labeled as pending the recal-layer check the plan calls
# for ("if pred_fp discriminates and p_start_recal doesn't, the recal layer
# is destroying ordering").
#
# Usage: Rscript R/21f_fp_grade.R <RB|WR|TE> <base|floorfree>

suppressPackageStartupMessages({
  library(tidyverse)
  library(cli)
})

source("R/21a_discrimination_fns.R")

args     <- commandArgs(trailingOnly = TRUE)
POSITION <- if (length(args) >= 1) toupper(args[1]) else cli_abort("Usage: Rscript R/21f_fp_grade.R <RB|WR|TE> <base|floorfree>")
ARM      <- if (length(args) >= 2) args[2] else "base"

THRESH <- list(RB = c(start = 15, boom = 20),
               WR = c(start = 15, boom = 20),
               TE = c(start = 12, boom = 17))
FULL_SEASONS <- 2016:2025

th   <- THRESH[[POSITION]]
path <- sprintf("output/21d_%s_%s_fold_predictions.csv", tolower(POSITION), ARM)
cli_h1("21f: grading {POSITION} [{ARM}] ({path}) against the frozen R/21b baseline")

cand <- read_csv(path, show_col_types = FALSE) |>
  mutate(player_id = as.character(player_id))

# ---------------------------------------------------------------------------
# Population-level sanity numbers (ALL rows, not just the flex band) --
# reported for context, never used for the ship-gate verdict.
# ---------------------------------------------------------------------------
cli_h2("Population-level (all scored rows, n={nrow(cand)}) -- context only")
cli_alert_info(
  "Pearson r={round(cor(cand$pred_fp, cand$fantasy_points_ppr, use='complete.obs'),4)} | Spearman={round(cor(cand$pred_fp, cand$fantasy_points_ppr, method='spearman', use='complete.obs'),4)}"
)

# ---------------------------------------------------------------------------
# Flex-band universe, same construction as R/21b (band_universe -> ECR score
# = -pos_rank), joined to this arm's pred_fp on (season, week, gsis_id).
# ---------------------------------------------------------------------------
band_d <- band_universe(POSITION, FULL_SEASONS, th["start"], th["boom"]) |>
  mutate(ecr_score = -pos_rank)

paired <- band_d |>
  inner_join(cand |> select(season, week, player_id, pred_fp, p_start_raw, p_boom_raw),
             by = c("season", "week", "gsis_id" = "player_id"))

cli_alert_info("In-band paired universe: {nrow(paired)} rows / {n_distinct(paste(paired$season,paired$week))} season-weeks (band universe had {nrow(band_d)} rows -- the gap is candidate coverage, same signal as Defect 2)")

if (nrow(paired) < 30) {
  cli_abort("Too few paired in-band rows ({nrow(paired)}) to grade -- check that R/21d covers {POSITION}'s flex band season range")
}

# ---------------------------------------------------------------------------
# The actual ship-gate comparison: pred_fp (candidate) vs -pos_rank (ECR),
# paired week-clustered bootstrap, both primary statistics.
# ---------------------------------------------------------------------------
cmp_spearman    <- disc_compare(paired, "ecr_score", "pred_fp", stat = "spearman")
cmp_concordance <- disc_compare(paired, "ecr_score", "pred_fp", stat = "concordance")
cmp_auc         <- disc_compare(paired, "ecr_score", "pred_fp", stat = "auc")

compare <- bind_rows(cmp_spearman, cmp_concordance, cmp_auc) |>
  mutate(position = POSITION, arm = ARM, .before = 1)

cli_h2("Candidate (pred_fp, raw pre-recal) vs ECR -- paired bootstrap, positive = candidate better")
print(compare |> mutate(across(where(is.numeric), ~round(.x, 4))) |> as.data.frame(), row.names = FALSE)

# ---------------------------------------------------------------------------
# Verdict against the plan's revised bars: PASS = CI excludes 0 in the
# candidate's favor AND point delta >= +0.02 (Spearman) / +0.01 (concordance)
# AND stat_b exceeds the frozen per-position ECR floor (R/21b). Split
# primaries = NEUTRAL, not PASS. TE is reported, non-blocking, per the plan.
# ---------------------------------------------------------------------------
DELTA_BAR   <- c(spearman = 0.02, concordance = 0.01)
FLOOR_BY_POS <- c(RB = 0.174, WR = 0.114, TE = 0.027)  # frozen R/21b ecr_full point estimates

verdicts <- compare |>
  filter(stat %in% names(DELTA_BAR)) |>
  mutate(
    bar   = DELTA_BAR[stat],
    floor = FLOOR_BY_POS[POSITION],
    verdict = case_when(
      ci_hi < 0 ~ "FAIL",
      ci_lo > 0 & delta >= bar & stat_b >= floor ~ "PASS",
      .default = "NEUTRAL"
    )
  )

cli_h1("VERDICT -- {POSITION} [{ARM}] (pred_fp discrimination vs ecr_full, pre-recalibration)")
print(verdicts |> select(position, arm, stat, n, weeks, stat_a, stat_b, delta, ci_lo, ci_hi, verdict) |>
        mutate(across(where(is.numeric), ~round(.x, 4))) |> as.data.frame(), row.names = FALSE)
if (POSITION == "TE") cli_alert_info("TE is SECONDARY/reported per the plan -- not blocking the ship gate regardless of verdict.")

dir.create("output", showWarnings = FALSE, recursive = TRUE)
out_path <- sprintf("output/21f_%s_%s_grade.csv", tolower(POSITION), ARM)
write_csv(verdicts, out_path)
cli_alert_success("{out_path}")
cli_h1("21f complete -- {POSITION} [{ARM}]")
