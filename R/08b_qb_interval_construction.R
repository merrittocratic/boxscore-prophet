# R/08b_qb_interval_construction.R
# Step 8b: Selects (1) the rush-side decomposition and (2) the combined-interval
# construction for the QB model. The QB combination algebra is ADDITIVE
# (total = pass_epa + rush_epa), so the frozen RB/WR Mechanism A power-law
# (built for the eff x vol PRODUCT) cannot be assumed to transfer -- this is
# the 03a analog for the additive case, run with default-params LightGBM
# before any tuning (mirrors the 03a-control -> 03a-construction -> 03a-v2
# sequence).
#
# DECISION 1 -- rush-side decomposition (pre-committed judge):
#   Variant H (hybrid, primary): tot = pass_eff x dropbacks + rush_epa_direct
#   Variant S (symmetric):       tot = pass_eff x dropbacks + rush_eff x carries
#   Judge: max |mean combined residual| across rush tiers (statue 0-3 /
#   mover 4-7 / scrambler 8+ carries) on pooled test rows -- the scrambler
#   mis-pricing axis from feasibility Q2/Q4. Smaller max |tier bias| wins;
#   if the difference is < 0.10 EPA, tiebreak on pooled RMSE; if RMSE within
#   0.02, prefer H (fewer noise-fitted components).
#   STOP condition: if BOTH variants show max |tier bias| > 0.5 EPA, neither
#   decomposition prices the rushing floor -- verdict recorded as FAIL_STOP;
#   do NOT proceed to 08c without review.
#   CAVEAT (first run, 2026-07-07): observed-carry tiers condition on an
#   OUTCOME -- games land in the scrambler tier partly because game script
#   produced QB runs, so even a perfect ex-ante forecaster shows positive
#   residual there. The script therefore ALSO reports bias by EX-ANTE tier
#   (rolling wt_carries -- what is knowable on Tuesday) and decomposes the
#   combined bias into pass/rush component shares. The observed-tier verdict
#   is still recorded; the ex-ante table adjudicates selection vs mispricing.
#
# DECISION 2 -- combined-interval mechanism (pre-committed judge):
#   (i)   const: one global conformal quantile on the raw summed residual
#   (ii)  plaw:  power-law normalization by total volume (dropbacks + carries),
#         the Mechanism A transplant
#   (iii) tier:  group-conditional conformal, one quantile per rush tier
#   Judge: flatness of normalized residual (raw_resid / hw80) across rush
#   tiers, ratio scrambler/statue in [0.67, 1.50]. Among passers, the simplest
#   mechanism wins (const < tier < plaw) unless a more complex one improves
#   |ratio - 1| by more than 0.05.
#
# DEPLOYMENT SEAM (documented, mirrors 03a): tier assignment and volume
# normalization here use OBSERVED carries/volume, exactly as 03a rescaled by
# observed opportunities. At deployment these come from the predicted volume
# components; the 08c/translation layer must carry that substitution.
#
# Frozen inputs: qb_feature_table.rds, fold_map.rds, metrics.R.
# DO NOT tune toward passing.

suppressPackageStartupMessages({
  library(tidyverse)
  library(lightgbm)
  library(cli)
})

source("R/metrics.R")

# ===========================================================================
# PARAMETERS -- default LightGBM (identical to 03a control; no tuning here)
# ===========================================================================

CAL_FRAC <- 0.20

LGBM_PARAMS <- list(
  objective        = "regression",
  metric           = "rmse",
  num_leaves       = 31L,
  learning_rate    = 0.05,
  feature_fraction = 0.8,
  bagging_fraction = 0.8,
  bagging_freq     = 5L,
  min_data_in_leaf = 20L,
  seed             = 42L,
  verbose          = -1L,
  num_threads      = 1L
)
N_ROUNDS <- 200L

# Decision-1 pre-committed thresholds
BIAS_TIE_EPA    <- 0.10
RMSE_TIE_EPA    <- 0.02
BIAS_STOP_EPA   <- 0.50

# Mechanism parameters
ALPHA_LO        <- 0.20   # power-law clamps, identical to Mechanism A
ALPHA_HI        <- 0.90
ALPHA_FALLBACK  <- 0.50
MIN_TIER_N      <- 15L    # tier-conditional falls back to global below this
FLAT_LO         <- 0.67   # flatness pass band, identical to 03a
FLAT_HI         <- 1.50
SIMPLICITY_EDGE <- 0.05

TIER_BREAKS <- c(-Inf, 4, 8, Inf)
TIER_LABELS <- c("statue (0-3)", "mover (4-7)", "scrambler (8+)")

# ===========================================================================
# FEATURE SETS
# ===========================================================================

PASS_EFF_FEATURES <- c(
  "prior_pass_epa_per_db", "baseline_pass_epa_per_db", "rolling_pass_epa_per_db",
  "form_residual", "is_cold_start_int", "draft_tier_int",
  "def_short_pass_epa_adj", "def_deep_pass_epa_adj",
  "wt_snap_share", "games_played_so_far", "def_used_fallback_int"
)

DB_VOL_FEATURES <- c(
  "wt_dropbacks", "wt_team_total_plays", "wt_team_pass_rate",
  "def_short_pass_epa_adj", "def_deep_pass_epa_adj",
  "draft_tier_int", "is_cold_start_int", "games_played_so_far"
)

CARRY_VOL_FEATURES <- c(
  "wt_carries", "wt_carry_share", "prior_carries_pg", "wt_team_total_plays",
  "def_rush_epa_adj", "draft_tier_int", "is_cold_start_int", "games_played_so_far"
)

RUSH_DIRECT_FEATURES <- c(
  "wt_rush_epa_pg", "wt_carries", "wt_carry_share",
  "prior_rush_epa_pg", "prior_carries_pg",
  "def_rush_epa_adj", "draft_tier_int", "is_cold_start_int", "games_played_so_far"
)

RUSH_EFF_FEATURES <- c(
  "wt_rush_epa_per_carry", "prior_rush_epa_per_carry", "wt_carry_share",
  "def_rush_epa_adj", "draft_tier_int", "is_cold_start_int", "games_played_so_far"
)

# ===========================================================================
# HELPERS (shared with 03a/04b house pattern)
# ===========================================================================

TIER_ORDER <- c("udfa" = 1L, "r6_udfa" = 2L, "r4_5" = 3L, "r2_3" = 4L, "r1" = 5L)

encode_features <- function(df) {
  df |> mutate(
    draft_tier_int        = TIER_ORDER[draft_tier],
    is_cold_start_int     = as.integer(is_cold_start),
    def_used_fallback_int = as.integer(def_used_fallback)
  )
}

make_matrix <- function(df, features) {
  df |> select(all_of(features)) |> as.matrix()
}

fit_lgbm <- function(X, y) {
  keep   <- !is.na(y)
  dtrain <- lgb.Dataset(X[keep, , drop = FALSE], label = y[keep])
  lgb.train(params = LGBM_PARAMS, data = dtrain, nrounds = N_ROUNDS, verbose = -1L)
}

conformal_q <- function(abs_resid, alpha) {
  n    <- length(abs_resid)
  prob <- (1 + 1 / n) * alpha
  if (prob >= 1.0) return(Inf)
  quantile(abs_resid, prob, names = FALSE)
}

fit_power_alpha <- function(vol, raw_resid) {
  df  <- data.frame(log_vol = log(vol), log_resid = log(raw_resid + 1e-8))
  fit <- tryCatch(lm(log_resid ~ log_vol, data = df), error = function(e) NULL)
  if (is.null(fit)) return(ALPHA_FALLBACK)
  alpha <- unname(coef(fit)["log_vol"])
  if (!is.finite(alpha)) return(ALPHA_FALLBACK)
  max(ALPHA_LO, min(ALPHA_HI, alpha))
}

rush_tier <- function(carries) {
  cut(carries, TIER_BREAKS, labels = TIER_LABELS, right = FALSE)
}

build_intervals <- function(pred, qs, suffix) {
  out <- tibble(
    p    = pred,
    lo50 = pred - qs[1], hi50 = pred + qs[1],
    lo80 = pred - qs[2], hi80 = pred + qs[2],
    lo90 = pred - qs[3], hi90 = pred + qs[3]
  )
  names(out) <- c(
    paste0("pred_", suffix),
    paste0("lo_50_", suffix), paste0("hi_50_", suffix),
    paste0("lo_80_", suffix), paste0("hi_80_", suffix),
    paste0("lo_90_", suffix), paste0("hi_90_", suffix)
  )
  out
}

build_row_intervals <- function(pred, hw50, hw80, hw90, suffix) {
  out <- tibble(
    p    = pred,
    lo50 = pred - hw50, hi50 = pred + hw50,
    lo80 = pred - hw80, hi80 = pred + hw80,
    lo90 = pred - hw90, hi90 = pred + hw90
  )
  names(out) <- c(
    paste0("pred_", suffix),
    paste0("lo_50_", suffix), paste0("hi_50_", suffix),
    paste0("lo_80_", suffix), paste0("hi_80_", suffix),
    paste0("lo_90_", suffix), paste0("hi_90_", suffix)
  )
  out
}

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

score_component <- function(y, df, suffix, label, stratum = "pooled") {
  eval_calibration(y, pi_cols(df, suffix)) |>
    mutate(component = label, stratum = stratum, .before = 1)
}

# Conformal quantiles at the three levels for a residual vector
q3 <- function(abs_resid) {
  c(conformal_q(abs_resid, 0.50),
    conformal_q(abs_resid, 0.80),
    conformal_q(abs_resid, 0.90))
}

# Tier-conditional quantiles: one q3 per tier, global fallback when sparse
tier_q3 <- function(cal_tier, cal_resid) {
  glob <- q3(cal_resid)
  qs <- lapply(TIER_LABELS, function(tl) {
    idx <- cal_tier == tl
    if (sum(idx) < MIN_TIER_N) glob else q3(cal_resid[idx])
  })
  names(qs) <- TIER_LABELS
  qs
}

tier_hw <- function(test_tier, qs, level_idx) {
  vapply(as.character(test_tier),
         function(tl) qs[[tl]][level_idx],
         numeric(1), USE.NAMES = FALSE)
}

fmt_pp <- function(x) sprintf("%+.1fpp", x * 100)

# ===========================================================================
# LOAD FROZEN INPUTS
# ===========================================================================

cli_h1("Step 8b: QB Decomposition + Additive Combined-Interval Construction")
cli_alert_info("Decision 1: rush side direct (H) vs eff x carries (S) -- judge: rush-tier bias")
cli_alert_info("Decision 2: const vs power-law vs tier-conditional -- judge: rush-tier flatness [{FLAT_LO}, {FLAT_HI}]")
cli_alert_info("Model: default-params LightGBM (no tuning at this step)")

ft       <- readRDS("data/qb_feature_table.rds")
fold_map <- readRDS("data/fold_map.rds")

EXPECTED_TEST_N <- ft |>
  semi_join(fold_map, by = c("season" = "test_season", "week" = "test_week")) |>
  nrow()

cli_alert_success("QB feature table: {nrow(ft)} rows | Fold map: {nrow(fold_map)} folds | Expected test rows: {EXPECTED_TEST_N}")

ft <- encode_features(ft)

for (fs in list(PASS_EFF_FEATURES, DB_VOL_FEATURES, CARRY_VOL_FEATURES,
                RUSH_DIRECT_FEATURES, RUSH_EFF_FEATURES)) {
  miss <- setdiff(fs, names(ft))
  if (length(miss) > 0) cli::cli_abort("Missing features: {paste(miss, collapse=', ')}")
}
cli_alert_success("All model features present in QB feature table")

# ===========================================================================
# WALK-FORWARD LOOP
# ===========================================================================

cli_h1("Walk-forward fold loop ({nrow(fold_map)} folds x 5 component models)")

fold_results <- vector("list", nrow(fold_map))
alpha_log    <- rep(NA_real_, nrow(fold_map))

for (f in seq_len(nrow(fold_map))) {

  test_season <- fold_map$test_season[f]
  test_week   <- fold_map$test_week[f]

  test_data  <- ft |> filter(season == test_season, week == test_week)
  train_data <- ft |> filter(
    season < test_season |
    (season == test_season & week < test_week)
  )

  if (nrow(test_data) == 0L) cli_abort("Fold {f}: no test rows")

  overlap <- intersect(
    paste(train_data$season, train_data$week),
    paste(test_data$season,  test_data$week)
  )
  if (length(overlap) > 0L) cli_abort("Fold {f}: train/test overlap")

  train_sws <- train_data |> distinct(season, week) |> arrange(season, week)
  n_cal_sw  <- max(1L, floor(CAL_FRAC * nrow(train_sws)))
  cal_sws   <- tail(train_sws, n_cal_sw)

  if (any(cal_sws$season == test_season & cal_sws$week == test_week)) {
    cli_abort("Fold {f}: test season-week in calibration set")
  }

  fit_data <- train_data |> semi_join(head(train_sws, nrow(train_sws) - n_cal_sw), by = c("season", "week"))
  cal_data <- train_data |> semi_join(cal_sws, by = c("season", "week"))

  # --- Component models (default params, no tuning) ---
  targets <- list(
    pass_eff   = list(feats = PASS_EFF_FEATURES,    y = "pass_epa_per_db_obs"),
    db_vol     = list(feats = DB_VOL_FEATURES,      y = "dropbacks"),
    carry_vol  = list(feats = CARRY_VOL_FEATURES,   y = "carries"),
    rush_dir   = list(feats = RUSH_DIRECT_FEATURES, y = "rush_epa"),
    rush_eff   = list(feats = RUSH_EFF_FEATURES,    y = "rush_epa_per_carry_obs")
  )

  preds_cal  <- list()
  preds_test <- list()
  for (nm in names(targets)) {
    tg  <- targets[[nm]]
    mod <- fit_lgbm(make_matrix(fit_data, tg$feats), as.numeric(fit_data[[tg$y]]))
    preds_cal[[nm]]  <- predict(mod, make_matrix(cal_data,  tg$feats))
    preds_test[[nm]] <- predict(mod, make_matrix(test_data, tg$feats))
  }

  # --- Component conformal quantiles (scalar, like 03a components) ---
  qs_pass_eff <- q3(abs(cal_data$pass_epa_per_db_obs - preds_cal$pass_eff))
  qs_db       <- q3(abs(as.numeric(cal_data$dropbacks) - preds_cal$db_vol))
  qs_carry    <- q3(abs(as.numeric(cal_data$carries)   - preds_cal$carry_vol))
  qs_rush_dir <- q3(abs(cal_data$rush_epa - preds_cal$rush_dir))

  # --- Combined point predictions, both variants ---
  pred_cal_H  <- preds_cal$pass_eff  * preds_cal$db_vol  + preds_cal$rush_dir
  pred_test_H <- preds_test$pass_eff * preds_test$db_vol + preds_test$rush_dir
  pred_cal_S  <- preds_cal$pass_eff  * preds_cal$db_vol  + preds_cal$rush_eff  * preds_cal$carry_vol
  pred_test_S <- preds_test$pass_eff * preds_test$db_vol + preds_test$rush_eff * preds_test$carry_vol

  resid_cal_H <- abs(cal_data$total_epa - pred_cal_H)
  resid_cal_S <- abs(cal_data$total_epa - pred_cal_S)

  cal_tier  <- rush_tier(cal_data$carries)
  test_tier <- rush_tier(test_data$carries)
  cal_vol   <- as.numeric(cal_data$dropbacks)  + as.numeric(cal_data$carries)
  test_vol  <- as.numeric(test_data$dropbacks) + as.numeric(test_data$carries)

  # --- Mechanisms, computed for both variants (choice is made post-hoc
  #     from pooled test rows; nothing here peeks at test outcomes) ---
  mech_hw <- list()
  for (v in c("H", "S")) {
    resid_v <- if (v == "H") resid_cal_H else resid_cal_S

    # (i) const
    qs_const <- q3(resid_v)
    for (li in 1:3) mech_hw[[paste0(v, "_const_", li)]] <- rep(qs_const[li], nrow(test_data))

    # (ii) plaw on total volume
    alpha  <- fit_power_alpha(cal_vol, resid_v)
    if (v == "H") alpha_log[f] <- alpha
    q_norm <- q3(resid_v / cal_vol^alpha)
    for (li in 1:3) mech_hw[[paste0(v, "_plaw_", li)]] <- q_norm[li] * test_vol^alpha

    # (iii) tier-conditional
    qs_tier <- tier_q3(cal_tier, resid_v)
    for (li in 1:3) mech_hw[[paste0(v, "_tier_", li)]] <- tier_hw(test_tier, qs_tier, li)
  }

  base <- test_data |>
    select(player_id, season, week, dropbacks, carries, wt_carries,
           pass_epa, rush_epa, total_epa) |>
    mutate(
      fold      = f,
      rush_tier = test_tier,
      total_vol = test_vol,
      pred_tot_H = pred_test_H,
      pred_tot_S = pred_test_S
    )

  mech_cols <- as_tibble(mech_hw) |>
    rename_with(~ paste0("hw_", .x))

  fold_results[[f]] <- base |>
    bind_cols(
      build_intervals(preds_test$pass_eff, qs_pass_eff, "pass_eff"),
      build_intervals(preds_test$db_vol,   qs_db,       "db"),
      build_intervals(preds_test$carry_vol, qs_carry,   "carry"),
      build_intervals(preds_test$rush_dir, qs_rush_dir, "rush"),
      tibble(pred_rush_eff = preds_test$rush_eff),
      mech_cols
    )

  if (f %% 20 == 0 || f == nrow(fold_map)) {
    cli_alert_info("Fold {sprintf('%03d', f)} [{test_season}-W{sprintf('%02d', test_week)}]: {nrow(test_data)} rows")
  }
}

results <- bind_rows(fold_results)

# ===========================================================================
# HARNESS INTEGRITY REPORT
# ===========================================================================

cli_h1("Harness Integrity Report")

n_folds_ran <- n_distinct(results$fold)
if (n_folds_ran == nrow(fold_map)) {
  cli_alert_success("All {nrow(fold_map)} folds completed")
} else {
  cli_abort("Only {n_folds_ran} of {nrow(fold_map)} folds produced results")
}

n_scored <- nrow(results)
if (n_scored == EXPECTED_TEST_N) {
  cli_alert_success("Row count: {n_scored} / {EXPECTED_TEST_N}")
} else {
  cli_warn("Row count mismatch: scored {n_scored}, expected {EXPECTED_TEST_N}")
}

na_preds <- sum(is.na(results$pred_tot_H)) + sum(is.na(results$pred_tot_S))
if (na_preds == 0L) {
  cli_alert_success("Zero NA combined predictions")
} else {
  cli_warn("{na_preds} NA combined predictions")
}

cli_alert_info(
  "plaw alpha (variant H): range [{round(min(alpha_log, na.rm=TRUE),3)}, {round(max(alpha_log, na.rm=TRUE),3)}] | median {round(median(alpha_log, na.rm=TRUE),3)} | at lower clamp: {sum(alpha_log <= ALPHA_LO + 1e-9, na.rm=TRUE)}/{nrow(fold_map)} folds"
)

# ===========================================================================
# DECISION 1: RUSH-SIDE DECOMPOSITION
# ===========================================================================

cli_h1("Decision 1: Rush-side decomposition (judge: rush-tier bias)")

bias_tbl <- results |>
  mutate(res_H = total_epa - pred_tot_H,
         res_S = total_epa - pred_tot_S) |>
  group_by(rush_tier) |>
  summarise(
    n      = n(),
    bias_H = mean(res_H),
    bias_S = mean(res_S),
    .groups = "drop"
  )

print(as.data.frame(bias_tbl |> mutate(across(starts_with("bias"), ~ round(.x, 3)))))

max_bias_H <- max(abs(bias_tbl$bias_H))
max_bias_S <- max(abs(bias_tbl$bias_S))
rmse_H <- sqrt(mean((results$total_epa - results$pred_tot_H)^2))
rmse_S <- sqrt(mean((results$total_epa - results$pred_tot_S)^2))

cli_alert_info("Variant H (hybrid):    max |tier bias| = {round(max_bias_H,3)} EPA | pooled RMSE = {round(rmse_H,3)}")
cli_alert_info("Variant S (symmetric): max |tier bias| = {round(max_bias_S,3)} EPA | pooled RMSE = {round(rmse_S,3)}")

decomp_verdict <- if (max_bias_H > BIAS_STOP_EPA && max_bias_S > BIAS_STOP_EPA) "FAIL_STOP" else "PASS"
if (decomp_verdict == "FAIL_STOP") {
  cli_warn("STOP condition (pre-committed, observed-tier judge): both variants exceed {BIAS_STOP_EPA} EPA max tier bias. Verdict recorded; producing full diagnostics -- do NOT proceed to 08c without review.")
} else {
  cli_alert_success("Observed-tier bias within {BIAS_STOP_EPA} EPA stop threshold")
}

# --- Diagnostic A: pass/rush component share of the tier bias ---
# If the scrambler gap sits in the rush component, it is a rushing-floor
# pricing question; if it is in the pass component, something else is wrong.
comp_bias <- results |>
  mutate(
    res_pass   = pass_epa - pred_pass_eff * pred_db,
    res_rush_H = rush_epa - pred_rush,
    res_rush_S = rush_epa - pred_rush_eff * pred_carry
  ) |>
  group_by(rush_tier) |>
  summarise(
    n           = n(),
    bias_pass   = round(mean(res_pass),   3),
    bias_rush_H = round(mean(res_rush_H), 3),
    bias_rush_S = round(mean(res_rush_S), 3),
    .groups = "drop"
  )
cli_h2("Diagnostic A: component share of tier bias (pass vs rush)")
print(as.data.frame(comp_bias))

# --- Diagnostic B: bias by EX-ANTE tier (rolling wt_carries) ---
# The deployable conditioning: what the model knows before kickoff. Small
# ex-ante bias + large observed-tier bias = game-script selection, an
# interval-width problem, not a point-prediction problem.
ex_ante <- results |>
  filter(!is.na(wt_carries)) |>
  mutate(ex_tier = rush_tier(wt_carries)) |>
  group_by(ex_tier) |>
  summarise(
    n      = n(),
    bias_H = round(mean(total_epa - pred_tot_H), 3),
    bias_S = round(mean(total_epa - pred_tot_S), 3),
    .groups = "drop"
  )
n_ex_na <- sum(is.na(results$wt_carries))
cli_h2("Diagnostic B: bias by EX-ANTE rush tier (rolling wt_carries; {n_ex_na} week-1 rows excluded)")
print(as.data.frame(ex_ante))

max_ex_bias_H <- max(abs(ex_ante$bias_H))
max_ex_bias_S <- max(abs(ex_ante$bias_S))
cli_alert_info("Ex-ante max |tier bias|: H = {round(max_ex_bias_H,3)} EPA | S = {round(max_ex_bias_S,3)} EPA")

if (abs(max_bias_H - max_bias_S) >= BIAS_TIE_EPA) {
  variant <- if (max_bias_H < max_bias_S) "H" else "S"
  reason  <- "tier-bias judge"
} else if (abs(rmse_H - rmse_S) >= RMSE_TIE_EPA) {
  variant <- if (rmse_H < rmse_S) "H" else "S"
  reason  <- "RMSE tiebreak (tier bias within {BIAS_TIE_EPA})"
} else {
  variant <- "H"
  reason  <- "full tie -- prefer hybrid (fewer noise-fitted components)"
}

cli_alert_success("Decomposition winner: Variant {variant} ({reason})")

# ===========================================================================
# DECISION 2: COMBINED-INTERVAL MECHANISM (winner variant only)
# ===========================================================================

cli_h1("Decision 2: Combined-interval mechanism (judge: rush-tier flatness)")

results <- results |>
  mutate(
    pred_tot      = .data[[paste0("pred_tot_", variant)]],
    raw_resid_tot = abs(total_epa - pred_tot)
  )

MECHS <- c("const", "plaw", "tier")

flat_list <- list()
ratios    <- numeric(length(MECHS))
names(ratios) <- MECHS

for (m in MECHS) {
  hw80 <- results[[paste0("hw_", variant, "_", m, "_2")]]
  norm <- results$raw_resid_tot / hw80
  tab  <- results |>
    mutate(norm = norm) |>
    group_by(rush_tier) |>
    summarise(
      n               = n(),
      mean_raw_resid  = round(mean(raw_resid_tot), 3),
      mean_norm_resid = round(mean(norm), 3),
      sd_norm_resid   = round(sd(norm), 3),
      .groups = "drop"
    )
  ratios[m] <- tab$mean_norm_resid[tab$rush_tier == "scrambler (8+)"] /
               tab$mean_norm_resid[tab$rush_tier == "statue (0-3)"]
  flat_list[[m]] <- tab |> mutate(mechanism = m, scrambler_statue_ratio = round(ratios[m], 3))
  cli_h2("Mechanism {m}")
  print(as.data.frame(tab))
  cli_alert_info("{m} scrambler/statue ratio: {round(ratios[m], 3)}")
}

passes <- ratios >= FLAT_LO & ratios <= FLAT_HI
for (m in MECHS) {
  if (passes[m]) cli_alert_success("{m}: FLAT") else cli_warn("{m}: NOT FLAT")
}

mech_verdict <- if (any(passes)) "PASS" else "FAIL_STOP"
if (mech_verdict == "FAIL_STOP") {
  cli_warn("STOP condition (pre-committed): no mechanism flattened the rush-tier axis. Verdict recorded; reporting best-distance mechanism for diagnostics only -- do NOT proceed to 08c without review.")
}

# Simplicity-ordered selection: const < tier < plaw. A more complex mechanism
# only wins if it improves |ratio - 1| by more than SIMPLICITY_EDGE over every
# simpler passing mechanism. On FAIL_STOP, fall back to min-distance for the
# diagnostic coverage report.
SIMPLICITY <- c("const", "tier", "plaw")
dist <- abs(ratios - 1.0)
mech_winner <- NA_character_
for (m in SIMPLICITY) {
  if (!passes[m]) next
  if (is.na(mech_winner)) {
    mech_winner <- m
  } else if (dist[m] < dist[mech_winner] - SIMPLICITY_EDGE) {
    mech_winner <- m
  }
}
if (is.na(mech_winner)) mech_winner <- names(which.min(dist))

cli_alert_success(
  "Mechanism winner: {mech_winner} | ratios: {paste(MECHS, round(ratios,3), sep='=', collapse=' | ')}"
)

# ===========================================================================
# COVERAGE REPORT -- WINNER (variant x mechanism) + COMPONENTS
# ===========================================================================

cli_h1("Coverage: Variant {variant} x mechanism {mech_winner}")

results <- results |>
  bind_cols(
    build_row_intervals(
      results$pred_tot,
      results[[paste0("hw_", variant, "_", mech_winner, "_1")]],
      results[[paste0("hw_", variant, "_", mech_winner, "_2")]],
      results[[paste0("hw_", variant, "_", mech_winner, "_3")]],
      "tot"
    ) |> select(-pred_tot)
  )

pooled <- bind_rows(
  score_component(results$pass_epa / results$dropbacks, results, "pass_eff", "pass_efficiency"),
  score_component(as.numeric(results$dropbacks),        results, "db",       "dropback_volume"),
  score_component(as.numeric(results$carries),          results, "carry",    "carry_volume"),
  score_component(results$rush_epa,                     results, "rush",     "rush_epa_direct"),
  score_component(results$total_epa,                    results, "tot",      "combined")
)
print(pooled |> select(component, stratum, nominal, empirical, delta, sharpness), n = Inf)

cli_h2("Combined coverage stratified by rush tier (the veto axis)")

tier_cov <- eval_calibration_stratified(
  results$total_epa,
  pi_cols(results, "tot"),
  strata = results$rush_tier
) |> mutate(component = "combined")

print(tier_cov |> select(stratum, n, nominal, empirical, delta, sharpness), n = Inf)

# Veto: every rush tier within +-10pp at 80% (the RB/WR low-usage analog)
cli_h1("Decision Rule Evaluation")

tier_80 <- tier_cov |> filter(nominal == 0.80)
worst   <- tier_80 |> slice_max(abs(delta), n = 1)

cli_alert_info(
  "Pooled combined 80%: {fmt_pp(pooled |> filter(component == 'combined', nominal == 0.80) |> pull(delta))}"
)
for (i in seq_len(nrow(tier_80))) {
  cli_alert_info("  {tier_80$stratum[i]} (n={tier_80$n[i]}): {fmt_pp(tier_80$delta[i])}")
}

veto_verdict <- if (max(abs(tier_80$delta)) > 0.10) "FAIL" else "PASS"
if (veto_verdict == "FAIL") {
  cli_warn("VETO TRIGGERED: {worst$stratum} 80% delta = {fmt_pp(worst$delta)}")
} else {
  cli_alert_success("Veto check passed: worst tier delta = {fmt_pp(worst$delta)} ({worst$stratum})")
}

# Deployable honesty check: same coverage stratified by EX-ANTE tier
cli_h2("Combined coverage stratified by EX-ANTE rush tier (deployable conditioning)")
res_ex <- results |> filter(!is.na(wt_carries)) |> mutate(ex_tier = rush_tier(wt_carries))
ex_cov <- eval_calibration_stratified(
  res_ex$total_epa,
  pi_cols(res_ex, "tot"),
  strata = res_ex$ex_tier
) |> mutate(component = "combined")
print(ex_cov |> select(stratum, n, nominal, empirical, delta, sharpness), n = Inf)

# ===========================================================================
# SAVE
# ===========================================================================

cli_h1("Save outputs")
dir.create("output", showWarnings = FALSE, recursive = TRUE)

decomp_out <- bias_tbl |>
  mutate(
    winner     = variant,
    max_bias_H = round(max_bias_H, 4),
    max_bias_S = round(max_bias_S, 4),
    rmse_H     = round(rmse_H, 4),
    rmse_S     = round(rmse_S, 4)
  )
readr::write_csv(decomp_out, "output/08b_qb_decomposition_bias.csv")

flat_out <- bind_rows(flat_list) |>
  mutate(variant = variant, mech_winner = mech_winner)
readr::write_csv(flat_out, "output/08b_qb_construction_flatness.csv")

readr::write_csv(comp_bias, "output/08b_qb_component_bias.csv")
readr::write_csv(ex_ante,   "output/08b_qb_ex_ante_bias.csv")
readr::write_csv(ex_cov,    "output/08b_qb_ex_ante_coverage.csv")

decision_summary <- tibble(
  decision = c("decomposition", "mechanism", "tier_veto_80"),
  winner   = c(variant, mech_winner, NA_character_),
  verdict  = c(decomp_verdict, mech_verdict, veto_verdict),
  detail   = c(
    sprintf("max obs-tier bias H=%.3f S=%.3f | ex-ante H=%.3f S=%.3f | rmse H=%.3f S=%.3f",
            max_bias_H, max_bias_S, max_ex_bias_H, max_ex_bias_S, rmse_H, rmse_S),
    sprintf("scrambler/statue ratios: %s",
            paste(MECHS, round(ratios, 3), sep = "=", collapse = " | ")),
    sprintf("worst tier %s delta=%+.3f", worst$stratum, worst$delta)
  )
)
readr::write_csv(decision_summary, "output/08b_qb_decision_summary.csv")

# Fold predictions: keys, components, winner combined intervals (canonical names)
results_final <- results |>
  select(
    player_id, season, week, fold, dropbacks, carries, rush_tier, total_vol,
    pass_epa, rush_epa, total_epa,
    starts_with("pred_pass_eff"), starts_with("lo_50_pass_eff"), starts_with("hi_50_pass_eff"),
    starts_with("lo_80_pass_eff"), starts_with("hi_80_pass_eff"),
    starts_with("lo_90_pass_eff"), starts_with("hi_90_pass_eff"),
    pred_db, lo_50_db, hi_50_db, lo_80_db, hi_80_db, lo_90_db, hi_90_db,
    pred_carry, lo_50_carry, hi_50_carry, lo_80_carry, hi_80_carry, lo_90_carry, hi_90_carry,
    pred_rush, lo_50_rush, hi_50_rush, lo_80_rush, hi_80_rush, lo_90_rush, hi_90_rush,
    pred_rush_eff, pred_tot_H, pred_tot_S,
    pred_tot, lo_50_tot, hi_50_tot, lo_80_tot, hi_80_tot, lo_90_tot, hi_90_tot
  )
readr::write_csv(results_final, "output/08b_qb_fold_predictions.csv")

readr::write_csv(pooled,   "output/08b_qb_pooled_coverage.csv")
readr::write_csv(tier_cov, "output/08b_qb_tier_coverage.csv")

cli_alert_success("output/08b_qb_decomposition_bias.csv    (winner: {variant})")
cli_alert_success("output/08b_qb_construction_flatness.csv (winner: {mech_winner})")
cli_alert_success("output/08b_qb_fold_predictions.csv      ({nrow(results_final)} rows)")
cli_alert_success("output/08b_qb_pooled_coverage.csv")
cli_alert_success("output/08b_qb_tier_coverage.csv")
cli_alert_success("output/08b_qb_component_bias.csv + ex_ante_bias + ex_ante_coverage + decision_summary")

if (decomp_verdict == "PASS" && mech_verdict == "PASS") {
  cli_alert_info("Construction locked for 08c: variant {variant} + mechanism {mech_winner}")
} else {
  cli_warn("One or more pre-committed verdicts FAILED -- review decision_summary before 08c")
}

cli_h1("Step 8b complete")
