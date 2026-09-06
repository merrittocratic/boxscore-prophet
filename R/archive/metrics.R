# R/metrics.R
# Calibration and sharpness metric functions for the RB EPA bake-off.
# Source this file in step-3 contender scripts — do not copy-paste inline.
# Validated against known-answer dummy data in R/build_bakeoff_rubric.R.

NOMINAL_LEVELS <- c(0.50, 0.80, 0.90)

# Fraction of true values that fall inside [lo, hi]. NA-safe.
pi_coverage <- function(y, lo, hi) {
  ok <- !is.na(y) & !is.na(lo) & !is.na(hi)
  if (!any(ok)) return(NA_real_)
  mean(y[ok] >= lo[ok] & y[ok] <= hi[ok])
}

# Mean interval width. Smaller = sharper.
pi_sharpness <- function(lo, hi) {
  mean(hi - lo, na.rm = TRUE)
}

# Full calibration table at all nominal levels.
# preds: data frame with columns lo_50/hi_50, lo_80/hi_80, lo_90/hi_90.
# Returns: one row per level with nominal, empirical, sharpness, delta (empirical - nominal).
eval_calibration <- function(y, preds, levels = NOMINAL_LEVELS) {
  purrr::map_dfr(levels, function(a) {
    pct <- as.integer(a * 100)
    tibble::tibble(
      nominal   = a,
      empirical = pi_coverage(y, preds[[paste0("lo_", pct)]], preds[[paste0("hi_", pct)]]),
      sharpness = pi_sharpness(preds[[paste0("lo_", pct)]], preds[[paste0("hi_", pct)]]),
      delta     = empirical - nominal   # positive = overcovers, negative = undercovers
    )
  })
}

# Stratified calibration: run eval_calibration for each unique value of `strata`.
eval_calibration_stratified <- function(y, preds, strata, levels = NOMINAL_LEVELS) {
  purrr::map_dfr(sort(unique(strata)), function(s) {
    mask <- strata == s
    eval_calibration(y[mask], preds[mask, ], levels) |>
      dplyr::mutate(stratum = s, n = sum(mask), .before = 1)
  })
}
