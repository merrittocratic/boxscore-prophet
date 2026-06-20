# R/04_bakeoff_results.R
# Final bake-off comparison: reads all contender outputs and produces the
# rubric verdict table. Run after all step-3 scripts have completed.
#
# Contenders:
#   3A  : LightGBM defaults (control)
#   3A-v2: Nested-CV-tuned LightGBM + Mechanism A conformal (prior winner)
#   3B  : Random Forest + Mechanism A conformal
#   3C  : Hierarchical Bayes (Student-t, opp-conditional sigma)
#   3D  : TabPFN zero-shot foundation model
#   3E  : Direct-quantile LightGBM on total_epa (new contender)
#
# Rubric (frozen from build_bakeoff_rubric.R):
#   Veto:      |low_opp combined 80% delta| > 10pp -> eliminated
#   Primary:   pooled combined 80% delta closest to 0 (among non-vetoed)
#   Tiebreak:  pooled combined 80% sharpness (width), lower wins

suppressPackageStartupMessages({
  library(tidyverse)
  library(cli)
})

source("R/metrics.R")

LOW_OPP_LO <- 5L
LOW_OPP_HI <- 8L

fmt_pp <- function(x) sprintf("%+.1fpp", x * 100)
fmt_w  <- function(x) round(x, 3)

pi_cols <- function(df, suffix) {
  df |> transmute(
    lo_50 = .data[[paste0("lo_50_", suffix)]],
    hi_50 = .data[[paste0("hi_50_", suffix)]],
    lo_80 = .data[[paste0("lo_80_", suffix)]],
    hi_80 = .data[[paste0("hi_80_", suffix)]],
    lo_90 = .data[[paste0("lo_90_", suffix)]],
    hi_90 = .data[[paste0("hi_90_", suffix)]]
  )
}

# Load all fold predictions -------------------------------------------------

load_preds <- function(path, label) {
  if (!file.exists(path)) {
    cli_warn("Missing: {path} -- skipping {label}")
    return(NULL)
  }
  readr::read_csv(path, show_col_types = FALSE) |>
    mutate(model = label)
}

preds <- list(
  "3A-LightGBM(default)"   = load_preds("output/03a_lgbm_fold_predictions.csv",      "3A-LightGBM(default)"),
  "3A-v2-LightGBM(tuned)"  = load_preds("output/03a_v2_lgbm_fold_predictions.csv",   "3A-v2-LightGBM(tuned)"),
  "3B-RF"                  = load_preds("output/03b_rf_fold_predictions.csv",         "3B-RF"),
  "3C-HierBayes"           = load_preds("output/03c_hier_fold_predictions.csv",       "3C-HierBayes"),
  "3D-TabPFN"              = load_preds("output/03d_tabpfn_fold_predictions.csv",     "3D-TabPFN"),
  "3E-QuantileLGBM"        = load_preds("output/03e_quantile_lgbm_fold_predictions.csv", "3E-QuantileLGBM")
)

available <- purrr::keep(preds, Negate(is.null))
cli_h1("Bake-off Results -- {length(available)} of 6 contenders available")
cli_alert_info("Available: {paste(names(available), collapse=', ')}")

if (length(available) == 0) cli_abort("No model outputs found. Run step-3 scripts first.")

# Score each contender -------------------------------------------------------

score_model <- function(df, label) {
  n_total <- nrow(df)
  lo_df   <- df |> filter(opportunities >= LOW_OPP_LO, opportunities <= LOW_OPP_HI)
  n_low   <- nrow(lo_df)

  pooled_comb <- eval_calibration(df$total_epa, pi_cols(df, "tot")) |>
    mutate(model = label, stratum = "pooled", component = "combined")

  low_comb <- eval_calibration(lo_df$total_epa, pi_cols(lo_df, "tot")) |>
    mutate(model = label, stratum = paste0("low_", LOW_OPP_LO, "_", LOW_OPP_HI),
           component = "combined", n_rows = n_low)

  strat <- df |>
    mutate(
      opp_bucket = case_when(
        opportunities <= LOW_OPP_HI ~ paste0("low  (", LOW_OPP_LO, "-", LOW_OPP_HI, ")"),
        opportunities <= 13L        ~ "mid  (9-13)",
        TRUE                        ~ "high (14+)"
      ) |> factor(levels = c(paste0("low  (", LOW_OPP_LO, "-", LOW_OPP_HI, ")"),
                              "mid  (9-13)", "high (14+)"))
    )

  strat_comb <- eval_calibration_stratified(
    strat$total_epa,
    pi_cols(strat, "tot"),
    strata = strat$opp_bucket
  ) |>
    mutate(model = label, component = "combined")

  list(
    pooled = pooled_comb,
    low    = low_comb,
    strat  = strat_comb,
    n_total = n_total,
    n_low   = n_low
  )
}

scored <- purrr::imap(available, score_model)

# Rubric table ---------------------------------------------------------------

cli_h1("Rubric table -- combined interval, pooled and low-usage")

rubric_rows <- purrr::map_dfr(scored, function(s) {
  pool80 <- s$pooled |> filter(nominal == 0.80)
  low80  <- s$low    |> filter(nominal == 0.80)
  tibble(
    model         = pool80$model,
    n_total       = s$n_total,
    n_low         = s$n_low,
    pool_delta_80 = pool80$delta,
    pool_width_80 = pool80$sharpness,
    low_delta_80  = low80$delta,
    veto          = abs(low80$delta) > 0.10
  )
}) |>
  arrange(veto, abs(pool_delta_80), pool_width_80)

cli_alert_info(str_pad("model", 24) |> paste0(" | pool 80% delta | pool width | low 80% delta | veto"))
rubric_rows |>
  mutate(row = paste0(
    str_pad(model,         24), " | ",
    str_pad(fmt_pp(pool_delta_80), 14), " | ",
    str_pad(fmt_w(pool_width_80),  10), " | ",
    str_pad(fmt_pp(low_delta_80),  13), " | ",
    if_else(veto, "TRIGGERED", "pass")
  )) |>
  pull(row) |>
  walk(cli_alert_info)

# Stratified table -----------------------------------------------------------

cli_h1("Stratified combined coverage at 80% (sharpness honesty check)")

strat_all <- purrr::map_dfr(scored, ~ .x$strat) |>
  filter(nominal == 0.80) |>
  select(model, stratum, n, empirical, delta, sharpness) |>
  arrange(stratum, model)

cli_alert_info(str_pad("model", 24) |> paste0(" | stratum | n    | 80% delta | 80% width"))
strat_all |>
  mutate(row = paste0(
    str_pad(model,   24), " | ",
    str_pad(stratum,  7), " | ",
    str_pad(n,        4), " | ",
    fmt_pp(delta),        " | ",
    fmt_w(sharpness)
  )) |>
  pull(row) |>
  walk(cli_alert_info)

# Low-opp efficiency diagnostic ----------------------------------------------

cli_h1("Low-opp efficiency 80% delta (committee-back tail check)")

eff_lo <- purrr::imap_dfr(available, function(df, label) {
  lo_df <- df |> filter(opportunities >= LOW_OPP_LO, opportunities <= LOW_OPP_HI)
  eval_calibration(lo_df$epa_per_opp_obs, pi_cols(lo_df, "eff")) |>
    filter(nominal == 0.80) |>
    mutate(model = label)
}) |>
  arrange(abs(delta))

cli_alert_info(str_pad("model", 24) |> paste0(" | eff low delta | eff low width"))
eff_lo |>
  mutate(row = paste0(str_pad(model, 24), ": delta=", fmt_pp(delta), " | width=", fmt_w(sharpness))) |>
  pull(row) |>
  walk(cli_alert_info)

# Verdict --------------------------------------------------------------------

cli_h1("Rubric Verdict")

non_vetoed <- rubric_rows |> filter(!veto)

TIEBREAK_THRESH <- 0.02   # frozen: +-2pp pooled combined 80%

if (nrow(non_vetoed) == 0) {
  cli_warn("ALL contenders vetoed. Re-examine feature table or veto threshold.")
} else {
  within_thresh <- non_vetoed |> filter(abs(pool_delta_80) <= TIEBREAK_THRESH)
  if (nrow(within_thresh) >= 2) {
    # Multiple models within +-2pp: tiebreak to sharpness (narrowest width)
    winner <- within_thresh |> arrange(pool_width_80) |> slice(1)
    cli_alert_success("Winner (tiebreak -- sharpness): {winner$model}")
    cli_alert_info("  {nrow(within_thresh)} models within +-{TIEBREAK_THRESH*100}pp: {paste(within_thresh$model, collapse=', ')}")
  } else if (nrow(within_thresh) == 1) {
    winner <- within_thresh
    cli_alert_success("Winner (sole model within +-{TIEBREAK_THRESH*100}pp): {winner$model}")
  } else {
    # No model within +-2pp: closest to nominal wins
    winner <- non_vetoed |> arrange(abs(pool_delta_80)) |> slice(1)
    cli_alert_success("Winner (closest to nominal, none within +-{TIEBREAK_THRESH*100}pp): {winner$model}")
  }
  cli_alert_info(
    "  pooled combined 80%: delta={fmt_pp(winner$pool_delta_80)} | width={fmt_w(winner$pool_width_80)}"
  )
  cli_alert_info(
    "  low combined 80%:    delta={fmt_pp(winner$low_delta_80)}"
  )
}

cli_h1("Bake-off results complete")
