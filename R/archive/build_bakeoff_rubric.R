# R/build_bakeoff_rubric.R
# Step 2: Lock the bake-off rubric before any model runs.
# Produces: frozen fold map, validated metric functions, bucket row count.
# Nothing here is a model. The measuring apparatus is proved correct on
# known-answer synthetic data so it cannot be unconsciously tuned post-hoc.

suppressPackageStartupMessages({
  library(tidyverse)
  library(cli)
})

source("R/metrics.R")

# ===========================================================================
# PARAMETERS
# ===========================================================================
# Rationale for MIN_TRAIN_WEEKS = 5: aligns with ROLLING_WINDOW = 5 in the
# feature layer. By week 6, rolling efficiency/share features are fully
# populated for established starters. Starting earlier (4) puts week 5's test
# rows in the feature NA zone; starting later (6) wastes a test week for nothing.
MIN_TRAIN_WEEKS <- 5L

# Low-usage bucket: just above the 5-opp minimum-opportunity floor.
# Upper bound of 8 captures the "fringe starter / committee back" zone where
# calibration failures typically concentrate and where EPA/opp is most volatile.
LOW_OPP_LO <- 5L
LOW_OPP_HI <- 8L

# ===========================================================================
# 1. LOAD FROZEN TABLE
# ===========================================================================
cli_h1("Step 2: Lock bake-off rubric")

ft <- readRDS("data/rb_feature_table.rds")
cli_alert_success(
  "Frozen table: {nrow(ft)} rows | seasons {paste(sort(unique(ft$season)), collapse=', ')}"
)

# ===========================================================================
# 2. BUILD WALK-FORWARD FOLD MAP
# ===========================================================================
cli_h1("Step 2a: Walk-forward fold map")

season_weeks <- ft |>
  distinct(season, week) |>
  arrange(season, week) |>
  mutate(sw_index = row_number())

n_sw    <- nrow(season_weeks)
n_folds <- n_sw - MIN_TRAIN_WEEKS

cli_alert_info(
  "Season-weeks: {n_sw} | MIN_TRAIN_WEEKS: {MIN_TRAIN_WEEKS} | Test folds: {n_folds}"
)
cli_alert_info(
  "First test fold: {season_weeks$season[MIN_TRAIN_WEEKS+1]}-W{sprintf('%02d', season_weeks$week[MIN_TRAIN_WEEKS+1])}"
)
cli_alert_info(
  "Last  test fold: {season_weeks$season[n_sw]}-W{sprintf('%02d', season_weeks$week[n_sw])}"
)

# Row counts per season-week (to populate the fold summary)
sw_counts <- ft |>
  group_by(season, week) |>
  summarise(n_rows = n(), .groups = "drop") |>
  arrange(season, week) |>
  mutate(cum_rows = cumsum(n_rows))

fold_map <- tibble(
  fold             = seq_len(n_folds),
  test_season      = season_weeks$season[(MIN_TRAIN_WEEKS + 1):n_sw],
  test_week        = season_weeks$week[(MIN_TRAIN_WEEKS + 1):n_sw],
  train_end_season = season_weeks$season[MIN_TRAIN_WEEKS:(n_sw - 1)],
  train_end_week   = season_weeks$week[MIN_TRAIN_WEEKS:(n_sw - 1)],
  n_train_sw       = MIN_TRAIN_WEEKS + seq_len(n_folds) - 1L,
  n_train_rows     = sw_counts$cum_rows[MIN_TRAIN_WEEKS:(n_sw - 1)],
  n_test_rows      = sw_counts$n_rows[(MIN_TRAIN_WEEKS + 1):n_sw]
) |>
  mutate(
    crosses_season_boundary = test_season > train_end_season
  )

# Pretty-print the full fold table for eyeball inspection
fold_display <- fold_map |>
  mutate(
    test          = paste0(test_season,      "-W", sprintf("%02d", test_week)),
    train_through = paste0(train_end_season, "-W", sprintf("%02d", train_end_week)),
    note          = if_else(crosses_season_boundary, "<-- season boundary", "")
  ) |>
  select(fold, test, train_through, n_train_sw, n_train_rows, n_test_rows, note)

cli_h1("Full fold map")
print(fold_display, n = Inf)

# Hard assertions: catch any off-by-one that leaks test data into training
stopifnot(
  "Every test week must strictly follow its training end week" = all(
    fold_map$test_season > fold_map$train_end_season |
    (fold_map$test_season == fold_map$train_end_season &
     fold_map$test_week   >  fold_map$train_end_week)
  ),
  "Training window must grow by exactly 1 week each fold" =
    all(diff(fold_map$n_train_sw) == 1L),
  "No future season in any training set" = all(
    fold_map$train_end_season < fold_map$test_season |
    (fold_map$train_end_season == fold_map$test_season &
     fold_map$train_end_week   < fold_map$test_week)
  )
)
cli_alert_success("Fold map assertions passed: no leakage, monotone expansion, season boundary clean")

n_boundary <- sum(fold_map$crosses_season_boundary)
cli_alert_info("{n_boundary} fold(s) cross a season boundary")

# ===========================================================================
# 3. METRIC FUNCTION DUMMY TEST
# ===========================================================================
cli_h1("Step 2b: Validate metric functions on known-answer synthetic data")

set.seed(42)
N_DUMMY <- 2000L
y_dummy <- rnorm(N_DUMMY, mean = 0, sd = 1)

# Build constant prediction intervals from a N(0, sd_val) distribution.
# When sd_val == 1 (= truth), coverage must equal nominal within sampling noise.
make_const_intervals <- function(n, sd_val = 1) {
  purrr::map_dfc(NOMINAL_LEVELS, function(a) {
    pct <- as.integer(a * 100)
    tibble(
      !!paste0("lo_", pct) := qnorm((1 - a) / 2, mean = 0, sd = sd_val),
      !!paste0("hi_", pct) := qnorm((1 + a) / 2, mean = 0, sd = sd_val)
    )
  }) |>
  slice(rep(1L, n))
}

# Test A: perfectly calibrated — expect empirical ≈ nominal (within ±3pp)
preds_perfect <- make_const_intervals(N_DUMMY, sd_val = 1)
cal_A <- eval_calibration(y_dummy, preds_perfect) |> mutate(test = "A: perfect (sd=1)")

cli_alert_info("Test A: perfectly calibrated intervals — expect empirical near nominal")
print(cal_A |> select(test, nominal, empirical, delta, sharpness))

# Test B: over-narrow (sd=0.5) — must undercover; catches a ruler that always says "covered"
preds_narrow <- make_const_intervals(N_DUMMY, sd_val = 0.5)
cal_B <- eval_calibration(y_dummy, preds_narrow) |> mutate(test = "B: narrow (sd=0.5)")

cli_alert_info("Test B: over-narrow intervals — expect empirical << nominal")
print(cal_B |> select(test, nominal, empirical, delta, sharpness))

# Test C: over-wide (sd=2) — must overcover; confirms the ruler isn't capped at nominal
preds_wide <- make_const_intervals(N_DUMMY, sd_val = 2)
cal_C <- eval_calibration(y_dummy, preds_wide) |> mutate(test = "C: wide (sd=2)")

cli_alert_info("Test C: over-wide intervals — expect empirical >> nominal")
print(cal_C |> select(test, nominal, empirical, delta, sharpness))

stopifnot(
  "Test A: 50% PI within 3pp of nominal"  = abs(cal_A$delta[cal_A$nominal == 0.50]) < 0.03,
  "Test A: 80% PI within 3pp of nominal"  = abs(cal_A$delta[cal_A$nominal == 0.80]) < 0.03,
  "Test A: 90% PI within 3pp of nominal"  = abs(cal_A$delta[cal_A$nominal == 0.90]) < 0.03,
  "Test B: 90% PI undercovering by >10pp" = cal_B$delta[cal_B$nominal == 0.90] < -0.10,
  "Test C: 50% PI overcovering by >15pp"  = cal_C$delta[cal_C$nominal == 0.50] >  0.15
)
cli_alert_success("All metric function assertions passed -- ruler measures correctly in isolation")

# ===========================================================================
# 4. LOW-USAGE BUCKET DEFINITION AND ROW COUNT
# ===========================================================================
cli_h1("Step 2c: Low-usage bucket")

# Only count rows that actually appear in test folds (weeks MIN_TRAIN_WEEKS+1 onward)
test_sw  <- fold_map |> distinct(test_season, test_week)
ft_test  <- ft |> semi_join(test_sw, by = c("season" = "test_season", "week" = "test_week"))

n_test_total <- nrow(ft_test)
n_bucket_low <- ft_test |> filter(opportunities >= LOW_OPP_LO, opportunities <= LOW_OPP_HI) |> nrow()
n_bucket_hi  <- ft_test |> filter(opportunities > LOW_OPP_HI) |> nrow()

# Coverage SE at the bucket sample size (conservative at 80% nominal)
se_80_low <- sqrt(0.80 * 0.20 / n_bucket_low)

cli_alert_info("Test-fold rows (total):              {n_test_total}")
cli_alert_info("Low-usage bucket ({LOW_OPP_LO}-{LOW_OPP_HI} opp):          {n_bucket_low} ({round(100*n_bucket_low/n_test_total,1)}%)")
cli_alert_info("High-usage bucket (>{LOW_OPP_HI} opp):        {n_bucket_hi} ({round(100*n_bucket_hi/n_test_total,1)}%)")
cli_alert_info("Coverage SE at 80% nominal (low bucket, n={n_bucket_low}): ±{round(se_80_low*100,1)}pp")

if (se_80_low > 0.04) {
  cli::cli_warn("Low bucket SE > 4pp -- coverage estimate is directional only, not precise")
} else {
  cli_alert_success("Low bucket large enough for a meaningful coverage estimate")
}

# Opportunity distribution across test folds for reference
cli_alert_info("Opp distribution in test folds (opp 5-15):")
print(
  ft_test |>
    count(opportunities, name = "n") |>
    arrange(opportunities) |>
    filter(opportunities <= 15) |>
    mutate(cum_pct = round(100 * cumsum(n) / n_test_total, 1))
)

# ===========================================================================
# 5. HOW TO USE THE FOLD MAP IN STEP 3 (documented filter logic)
# ===========================================================================
cli_h1("Step 2d: Fold map usage contract")
cli_alert_info("Each step-3 contender must implement exactly this filter logic:")
cli_alert_info("  ft <- readRDS('data/rb_feature_table.rds')")
cli_alert_info("  fold_map <- readRDS('data/fold_map.rds')")
cli_alert_info("  For fold f:")
cli_alert_info("    test  = ft |> filter(season == fold_map$test_season[f], week == fold_map$test_week[f])")
cli_alert_info("    train = ft |> filter(season < fold_map$test_season[f] |")
cli_alert_info("                         (season == fold_map$test_season[f] & week < fold_map$test_week[f]))")
cli_alert_info("  No other split logic is permitted.")

# ===========================================================================
# 6. SAVE RUBRIC ARTIFACTS
# ===========================================================================
cli_h1("Step 2e: Save")
dir.create("data",   showWarnings = FALSE, recursive = TRUE)
dir.create("output", showWarnings = FALSE, recursive = TRUE)

saveRDS(fold_map, "data/fold_map.rds")
readr::write_csv(fold_map, "output/fold_map.csv")

cli_alert_success("data/fold_map.rds")
cli_alert_success("output/fold_map.csv")

cli_h1("Rubric locked")
cli_alert_info("Frozen feature table: output/rb_feature_table_v2.0.csv")
cli_alert_info("Fold map:             output/fold_map.csv  ({nrow(fold_map)} folds)")
cli_alert_info("Metric functions:     R/metrics.R  (source in step-3 scripts)")
cli_alert_info("Low-usage bucket:     opp {LOW_OPP_LO}-{LOW_OPP_HI}, n={n_bucket_low} test rows, SE ±{round(se_80_low*100,1)}pp")
cli_alert_info("Step 3 may now begin.")
