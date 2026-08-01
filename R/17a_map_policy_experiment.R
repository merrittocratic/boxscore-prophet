# R/17a_map_policy_experiment.R
# Deployment-procedure experiment (NOT a ladder rung): should the recal maps
# refit weekly in-season, or stay frozen at the ship-pass fit?
#
# ==========================================================================
# PRE-REGISTRATION (drafted 2026-08-01; LOCKED upon Steve sign-off --
# the body below refuses to run until APPROVED is flipped)
# ==========================================================================
#
# THE SEAM (two legs, from the 2026-08-01 review):
#   Leg 1: deployed maps are frozen at the ship fit (through 2025) while the
#     backtest that validated them refit walk-forward each week -- by late
#     season, production maps have never seen a current-season outcome.
#   Leg 2: maps were fit on backtest FOLD-model predictions but score the
#     weekly-retrained DEPLOYED model's predictions, which drifts away from
#     the fold models as the season accrues (pred-vol seam class).
#
# WHAT THIS EXPERIMENT CAN AND CANNOT MEASURE: the 2025 pretend-deploy
# sizes LEG 1 cleanly. Leg 2 cannot be measured until a live season's
# ledger exists (fold preds ARE the walk-forward preds here, so both
# policies see the same prediction distribution). Leg 2 argues
# directionally for refit; it is stated here so the decision is made with
# eyes open, not silently folded into the result.
#
# DESIGN (pretend-deploy on the 2025 season; fold preds = production sim,
# per "deployment is one more fold"):
#   POLICY A (frozen):   maps fit ONCE on fold predictions through 2024,
#                        applied unchanged to every 2025 week.
#   POLICY B (refit):    maps fit for each 2025 week W on fold predictions
#                        through W-1 (2025 weeks included as they accrue).
#                        This is EXACTLY the shipped 6c/12e/9b walk-forward
#                        -- Policy B's numbers already exist in the recal
#                        probability files; only Policy A is new compute.
#   HELD FIXED in both arms: the method PER CELL (the terminal-round
#     judge's picks -- platt_vegas etc.). 17a varies the fitting WINDOW
#     only; it does NOT re-open method selection.
#
# METRICS (2025 season, per position x threshold = 8 pools):
#   M1 pooled |stated - empirical| (calibration, pp)
#   M2 Brier score
#   SECONDARY (report, not decision): same metrics on weeks 10+ only,
#     where the frozen-map gap is largest; and per-cell deltas under the
#     ladder cell machinery (4pp / n>=400).
#
# PRE-COMMITTED DECISION RULE:
#   POLICY B (weekly refit) ships for 2026 IFF
#     (a) mean |stated-empirical| across the 8 pools improves (B < A) by
#         >= 0.5pp, AND
#     (b) no single pool worsens under B by >= 1.0pp, AND
#     (c) mean Brier does not worsen.
#   OTHERWISE POLICY A (frozen) ships -- pre-stated tiebreak: one season
#   cannot resolve fine gradations, and the less-adaptive procedure is the
#   safer default for a first live season. Either way the shipped policy
#   is documented as a D-entry and 10f monitors it live (its drift alarms
#   are the in-season check on whichever policy wins).
#   IF B ships: production refit uses fold-era rows + the accumulating
#   2026 LEDGER rows (the deployment's own pre-kickoff predictions vs
#   outcomes), which is what addresses Leg 2; the weekly refit slots into
#   Tuesday full mode between 10a and the slates.
#
# EXPECTATION (stated before running): mostly a wash on full-season pooled
# metrics (one season of extra data on a 10-season window is ~5% of rows);
# any real separation should appear in the W10+ secondary cut. Prior:
# tiebreak fires, A ships, and the decision value is having TESTED the
# frozen procedure the backtest never ran.
# ==========================================================================

APPROVED <- TRUE    # Steve sign-off 2026-08-01; header locked verbatim above

if (!APPROVED) {
  cli::cli_abort("17a pre-registration awaiting sign-off -- body does not run until APPROVED <- TRUE (and the header is locked verbatim at that point).")
}

# CAVEAT LOGGED AT APPROVAL: the recal files on disk predate the 2025
# opener backfill, so 2025 rows carry NA vegas covariates and the
# vegas-aware closures coalesce to neutral center. Both policies see the
# same rows, so the WINDOW comparison is apples-to-apples; but the
# experiment runs under 2025-vegas-NA conditions, not 2026's real-opener
# conditions. Stated here, not silently absorbed.

suppressPackageStartupMessages({
  library(tidyverse)
  library(cli)
})

# ---- fitting machinery: EXACT shipped code, extracted by parsing the
# recal scripts and evaluating only the wanted top-level definitions
# (no reimplementation, no heavy execution) ------------------------------
extract_defs <- function(path, wanted) {
  env <- new.env(parent = globalenv())
  for (e in parse(path)) {
    if (is.call(e) && identical(e[[1]], as.name("<-"))) {
      lhs <- tryCatch(as.character(e[[2]]), error = function(err) NULL)
      if (length(lhs) == 1 && lhs %in% wanted) eval(e, env)
    }
  }
  missing <- setdiff(wanted, ls(env))
  if (length(missing) > 0) cli_abort("defs missing from {path}: {missing}")
  env
}

env_fp <- extract_defs("R/06c_recalibration.R",
  c("P_EPS", "MIN_STRAT_N", "STRATA_LABELS",
    "fit_platt", "fit_isotonic", "fit_strat", "fit_platt_vegas"))
env_te <- extract_defs("R/12e_te_recalibration.R",
  c("P_EPS", "fit_platt", "fit_platt_vegas", "fit_platt_vol_vegas"))
env_qb <- extract_defs("R/09b_qb_recalibration.R",
  c("P_EPS", "fit_platt", "fit_platt_vegas", "fit_platt_vol_vegas"))

# Policy-A implied-total center: median over ARCHIVE games in seasons
# <= 2024 (the shipped, pre-backfill condition -- IT_CENTER's definition
# with the frozen window).
vegas_open <- readRDS("data/vegas_open_lines.rds") |>
  mutate(season = as.integer(substr(game_id, 1, 4)))
IT_CENTER_A <- median(vegas_open$implied_total[vegas_open$season <= 2024], na.rm = TRUE)
cli_alert_info("Policy-A it center (archive <= 2024): {round(IT_CENTER_A, 2)}")

FIT_MAX_SEASON <- 2024L
EVAL_SEASON    <- 2025L

POOLS <- tribble(
  ~pool,      ~file,                                   ~pos,  ~stem,     ~method,           ~env,   ~vol_col,
  "RB_start", "output/06c_recal_probabilities.csv",    "RB",  "p_start", "platt_vegas",     "fp",   "pred_vol",
  "RB_boom",  "output/06c_recal_probabilities.csv",    "RB",  "p_boom",  "platt_vegas",     "fp",   "pred_vol",
  "WR_start", "output/06c_recal_probabilities.csv",    "WR",  "p_start", "strat_platt",     "fp",   "pred_vol",
  "WR_boom",  "output/06c_recal_probabilities.csv",    "WR",  "p_boom",  "strat_iso",       "fp",   "pred_vol",
  "TE_start", "output/12e_te_recal_probabilities.csv", "TE",  "p_start", "platt_vol_vegas", "te",   "pred_vol",
  "TE_boom",  "output/12e_te_recal_probabilities.csv", "TE",  "p_boom",  "platt_vegas",     "te",   "pred_vol",
  "QB_start", "output/09b_qb_recal_probabilities.csv", NA,    "p_start", "platt_vol_vegas", "qb",   "pred_carry",
  "QB_boom",  "output/09b_qb_recal_probabilities.csv", NA,    "p_boom",  "platt",           "qb",   "pred_carry"
)

# Guard the pre-registration against drift: the deployed methods must
# still be what the pre-reg assumed (terminal-round picks).
maps_check <- c(
  readRDS("data/fp_recal_maps.rds")[["RB_15+"]]$method   == "platt_vegas",
  readRDS("data/fp_recal_maps.rds")[["RB_20+"]]$method   == "platt_vegas",
  readRDS("data/fp_recal_maps.rds")[["WR_15+"]]$method   == "strat_platt",
  readRDS("data/fp_recal_maps.rds")[["WR_20+"]]$method   == "strat_iso",
  readRDS("data/te_fp_recal_maps.rds")[["TE_12+"]]$method == "platt_vol_vegas",
  readRDS("data/te_fp_recal_maps.rds")[["TE_17+"]]$method == "platt_vegas",
  readRDS("data/qb_fp_recal_maps.rds")[["QB_20+"]]$method == "platt_vol_vegas",
  readRDS("data/qb_fp_recal_maps.rds")[["QB_25+"]]$method == "platt"
)
if (!all(maps_check)) cli_abort("Deployed method drifted from pre-registered POOLS table -- re-review before running.")

run_pool <- function(cfg) {
  env <- switch(cfg$env, fp = env_fp, te = env_te, qb = env_qb)
  df <- read_csv(cfg$file, show_col_types = FALSE)
  if (!is.na(cfg$pos)) df <- df |> filter(position == cfg$pos)
  hit_col <- paste0("hit_", sub("p_", "", cfg$stem))
  b_col   <- paste0(cfg$stem, "_", cfg$method)
  fit  <- df |> filter(season <= FIT_MAX_SEASON)
  eval <- df |> filter(season == EVAL_SEASON)
  stopifnot(nrow(fit) > 500, nrow(eval) > 50)

  p_f <- fit[[cfg$stem]];  h_f <- as.numeric(fit[[hit_col]])
  pA <- switch(cfg$method,
    platt = {
      f <- env$fit_platt(p_f, h_f); f(eval[[cfg$stem]])
    },
    strat_platt = {
      f <- env$fit_strat(p_f, h_f, fit$stratum, env$fit_platt)
      f(eval[[cfg$stem]], eval$stratum)
    },
    strat_iso = {
      f <- env$fit_strat(p_f, h_f, fit$stratum, env$fit_isotonic)
      f(eval[[cfg$stem]], eval$stratum)
    },
    platt_vegas = {
      f <- env$fit_platt_vegas(p_f, h_f, fit$team_spread, fit$implied_total, IT_CENTER_A)
      f(eval[[cfg$stem]], eval$team_spread, eval$implied_total)
    },
    platt_vol_vegas = {
      f <- env$fit_platt_vol_vegas(p_f, h_f, fit[[cfg$vol_col]],
                                   fit$team_spread, fit$implied_total, IT_CENTER_A)
      f(eval[[cfg$stem]], eval[[cfg$vol_col]], eval$team_spread, eval$implied_total)
    }
  )
  if (is.null(pA)) cli_abort("Policy-A fit failed for {cfg$pool}")

  h_e <- as.numeric(eval[[hit_col]])
  pB  <- eval[[b_col]]
  late <- eval$week >= 10
  tibble(
    pool = cfg$pool, n = nrow(eval),
    m1_A = 100 * abs(mean(pA) - mean(h_e)),
    m1_B = 100 * abs(mean(pB) - mean(h_e)),
    brier_A = mean((pA - h_e)^2),
    brier_B = mean((pB - h_e)^2),
    n_late = sum(late),
    m1_A_late = 100 * abs(mean(pA[late]) - mean(h_e[late])),
    m1_B_late = 100 * abs(mean(pB[late]) - mean(h_e[late]))
  )
}

cli_h1("17a: fitting + scoring 8 pools")
res <- POOLS |> rowwise() |> group_split() |> map(run_pool) |> list_rbind()
print(res |> mutate(across(where(is.numeric), ~round(.x, 3))) |> as.data.frame(), row.names = FALSE)

# ---- pre-committed decision rule ----------------------------------------
cli_h1("17a verdict (pre-committed rule)")
mean_A <- mean(res$m1_A); mean_B <- mean(res$m1_B)
improve_ok <- (mean_A - mean_B) >= 0.5
worsen     <- res |> filter(m1_B - m1_A >= 1.0)
brier_ok   <- mean(res$brier_B) <= mean(res$brier_A)

cli_alert_info("Mean |delta|: A(frozen) {round(mean_A, 2)}pp vs B(refit) {round(mean_B, 2)}pp | improvement {round(mean_A - mean_B, 2)}pp (need >= 0.5)")
cli_alert_info("Pools worsening >= 1pp under B: {nrow(worsen)} (need 0) | Brier: B {round(mean(res$brier_B), 4)} vs A {round(mean(res$brier_A), 4)} (B must not be worse)")
cli_alert_info("W10+ secondary: A {round(mean(res$m1_A_late), 2)}pp vs B {round(mean(res$m1_B_late), 2)}pp")

if (improve_ok && nrow(worsen) == 0 && brier_ok) {
  cli_alert_warning("VERDICT: POLICY B (weekly refit) SHIPS for 2026 -- production refit = fold rows + accumulating 2026 ledger, slotted between 10a and slates. Pre-register the implementation before building.")
} else {
  cli_alert_success("VERDICT: POLICY A (frozen) SHIPS for 2026 -- pre-stated tiebreak. Maps stay sealed; 10f drift alarms are the in-season check.")
}

readr::write_csv(res |> mutate(mean_A = mean_A, mean_B = mean_B,
                               improve_ok = improve_ok, brier_ok = brier_ok,
                               it_center_A = IT_CENTER_A),
                 "output/17a_policy_comparison.csv")
cli_alert_success("output/17a_policy_comparison.csv")
cli_h1("17a complete")
