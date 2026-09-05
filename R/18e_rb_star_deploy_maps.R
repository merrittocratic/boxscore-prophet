# R/18e_rb_star_deploy_maps.R
# D27 SHIP STEP 1: fit the DEPLOYMENT star_platt maps for RB 15+/20+ on
# the full validated history and write a CANDIDATE maps rds. Production
# data/fp_recal_maps.rds is NOT touched here -- promotion is a separate
# explicit step after the 10c hindcast gates pass (D24 pattern).
#
# Fit data: output/06c_volfix_10acand_recal_probabilities.csv RB rows --
# the exact rows the walk-forward validation (18d) graded, which are the
# held-out cal folds of the deployed models. Buckets come from the
# shared core (18e fns); this script also runs the EQUALITY GATE:
# shared-core buckets must exactly reproduce the 18d validation file's
# bucket column on its common support. Any mismatch is a STOP.
#
# Output rds layout matches data/fp_recal_maps.rds: named entries
# RB_15+/RB_20+/WR_15+/WR_20+; WR entries are carried over from the
# production rds byte-identical. New RB entries carry method
# "star_platt", needs_bucket = TRUE, the fitted coefficients, the
# bucket definition string, and the widened closure
# map(p, vol, spread, implied, bucket).
#
# Usage: Rscript R/18e_rb_star_deploy_maps.R
#   -> data/fp_recal_maps_starplatt_cand.rds

suppressPackageStartupMessages({
  library(tidyverse)
  library(cli)
})

source("R/18e_star_bucket_fns.R")

set.seed(42)

cli_h1("18e: RB star_platt deployment map fit (candidate)")

rb <- read_csv("output/06c_volfix_10acand_recal_probabilities.csv",
               show_col_types = FALSE) |>
  filter(position == "RB") |>
  select(player_id, season, week, p_start, p_boom, hit_start, hit_boom)

trailing <- star_trailing_fp(2016:2025)
rb <- star_assign_buckets(rb, trailing)

cli_alert_info("{nrow(rb)} RB fit rows; buckets: {paste(count(rb, star_bucket)$n, collapse = '/')} ({paste(count(rb, star_bucket)$star_bucket, collapse = '/')})")

# ---------------------------------------------------- equality gate vs 18d --
gate <- read_csv("output/18d_rb_star_recal_probabilities.csv",
                 show_col_types = FALSE) |>
  select(player_id, season, week, bucket_18d = bucket) |>
  inner_join(rb |> select(player_id, season, week, star_bucket),
             by = c("player_id", "season", "week"))
n_mismatch <- sum(gate$bucket_18d != gate$star_bucket)
cli_alert_info("Bucket equality gate vs 18d: {nrow(gate)} rows, {n_mismatch} mismatches")
if (n_mismatch > 0) {
  print(gate |> filter(bucket_18d != star_bucket) |> head(10))
  cli_abort("Shared-core buckets do not reproduce the validated 18d buckets -- STOP.")
}
cli_alert_success("Equality gate PASSED: shared core reproduces the validated construction exactly")

# ------------------------------------------------------------------ fit --
fit_start <- fit_star_platt_map(rb$p_start, rb$hit_start, rb$star_bucket)
fit_boom  <- fit_star_platt_map(rb$p_boom,  rb$hit_boom,  rb$star_bucket)

cli_h2("Fitted coefficients")
cli_alert_info("15+ start: {paste(sprintf('%s=%.4f', names(fit_start$coefs), fit_start$coefs), collapse = ', ')}")
cli_alert_info("20+ boom:  {paste(sprintf('%s=%.4f', names(fit_boom$coefs), fit_boom$coefs), collapse = ', ')}")

# In-sample verification (fit-on-all, so gaps should be ~0 by construction;
# a large residual gap would indicate a wiring bug, not a modeling issue).
verify <- rb |>
  mutate(p_new_start = fit_start$map(p_start, NA, NA, NA, star_bucket),
         p_new_boom  = fit_boom$map(p_boom, NA, NA, NA, star_bucket)) |>
  group_by(star_bucket) |>
  summarise(n = n(),
            gap_start_raw = mean(hit_start) - mean(p_start),
            gap_start_new = mean(hit_start) - mean(p_new_start),
            gap_boom_raw = mean(hit_boom) - mean(p_boom),
            gap_boom_new = mean(hit_boom) - mean(p_new_boom),
            .groups = "drop")
cli_h2("In-sample bucket gaps (raw vs star_platt)")
print(verify |> mutate(across(where(is.numeric), ~round(.x, 4))) |>
        as.data.frame(), row.names = FALSE)
stopifnot(all(abs(verify$gap_start_new) < 0.02),
          all(abs(verify$gap_boom_new) < 0.02))

# ------------------------------------------------------------- assemble --
# ERRATUM (2026-09-05, found during this ship): production RB maps are
# platt_vegas (the D24 promote used the big-backtest refit), NOT the
# raw picks in the volfix_10acand candidate rds that D25/D27 graded
# against. Verdicts hold a fortiori -- raw out-scored platt_vegas in
# the graded window, so the market comparison gave the model its best
# variant. star_platt replaces platt_vegas here.
prod <- readRDS("data/fp_recal_maps.rds")
stopifnot(prod[["RB_15+"]]$method == "platt_vegas",
          prod[["RB_20+"]]$method == "platt_vegas")

BUCKET_DEF <- paste(
  "trailing PPR FP/game, last 17 REG games played, >=6 games, ranked",
  "within the scored universe per week; b1=1-12, b2=13-24, b3=rest.",
  "Shared core: R/18e_star_bucket_fns.R")

cand <- prod
cand[["RB_15+"]] <- list(position = "RB", threshold = "15+",
                         method = "star_platt", needs_bucket = TRUE,
                         coefs = fit_start$coefs, bucket_def = BUCKET_DEF,
                         map = fit_start$map)
cand[["RB_20+"]] <- list(position = "RB", threshold = "20+",
                         method = "star_platt", needs_bucket = TRUE,
                         coefs = fit_boom$coefs, bucket_def = BUCKET_DEF,
                         map = fit_boom$map)

saveRDS(cand, "data/fp_recal_maps_starplatt_cand.rds")
cli_alert_success("data/fp_recal_maps_starplatt_cand.rds (RB star_platt candidate; WR entries carried over untouched)")
cli_h1("18e complete -- run the 10c hindcast gates before promoting")
