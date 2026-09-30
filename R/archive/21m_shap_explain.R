# R/21m_shap_explain.R
# Stage F of the D29 single-stage rebuild: per-feature "here's why"
# explanations for the fp1 point-head prediction. Approved 2026-09-07 as a
# differentiation feature -- the "here's why" capability the old eff x vol
# architecture structurally couldn't provide (an efficiency x volume
# product has no clean per-feature decomposition; a single LGBM point head
# does).
#
# MECHANISM: lightgbm::predict(model, X, type = "contrib") implements
# TreeSHAP (Lundberg) for tree ensembles -- exact per-feature Shapley
# contributions, not an approximation, verified here to sum EXACTLY to the
# point prediction (regression objective, no link transform, so
# rowSums(contrib) == predict(type="response") to floating-point
# precision). Output is one column per feature PLUS a trailing intercept
# column (the model's base rate); column order matches the deployed
# model's own `features` vector, NOT feature-named by lightgbm itself
# (predict() returns unnamed columns for type="contrib" as of lightgbm
# 4.6.0 -- verified by inspection, not assumed).
#
# SCOPE: RB/WR only, reading data/deployment_params_fp.rds (R/21n) --
# whichever arm is currently deployed there (ship decision as of writing:
# RB=floor-free, WR=base). Explains the POINT prediction only; does not
# touch the aux volume head, the conformal interval, recal maps, or
# p_bust -- those are separate layers with their own (non-SHAP) semantics.
# TE/QB are two-stage (eff x vol product) and have no single clean point
# model to decompose this way -- out of scope, not an oversight.
#
# Usage: Rscript R/21m_shap_explain.R <RB|WR> [n_demo_rows]
#   Demos against the most recent rows of the deployed arm's training
#   table (real historical players, most recent season/week first) --
#   no live slate file required. For a live weekly slate, source this
#   file and call shap_contrib() directly with an encode_features()'d
#   slate data frame (same convention as R/10c_weekly_score.R).

suppressPackageStartupMessages({
  library(tidyverse)
  library(lightgbm)
  library(cli)
})

# ===========================================================================
# CORE: shap_contrib() -- the reusable function other scripts (10c/10d/
# content) source this file for.
# ===========================================================================

# dp_fp: the loaded data/deployment_params_fp.rds object (or a subset,
#   dp_fp$rb / dp_fp$wr, passed as `dpos` directly -- both forms accepted).
# position: "RB" or "WR" (ignored if `dpos` was passed pre-selected).
# X_df: a data frame containing at least dpos$point$features columns,
#   already encoded (draft_tier_int/is_cold_start_int/def_used_fallback_int
#   -- same convention as R/10c's encode_features()). Extra columns are
#   ignored (select(all_of(features)) below).
#
# Returns a tibble, one row per input row: feat_<name> columns (FP-unit
# contribution, sums with intercept to pred_fp), `intercept`, `pred_fp`.
shap_contrib <- function(dp_fp, position, X_df) {
  dpos <- if (!is.null(dp_fp$point)) dp_fp else dp_fp[[tolower(position)]]
  feats <- dpos$point$features
  m     <- lightgbm::lgb.load(dpos$point$model_file)

  X <- X_df |> select(all_of(feats)) |> as.matrix()
  contrib <- predict(m, X, type = "contrib")
  pred_fp <- predict(m, X, type = "response")
  stopifnot(ncol(contrib) == length(feats) + 1L,
            max(abs(rowSums(contrib) - pred_fp)) < 1e-6)

  colnames(contrib) <- c(feats, "intercept")
  as_tibble(contrib) |>
    rename_with(~ paste0("feat_", .x), all_of(feats)) |>
    mutate(pred_fp = pred_fp, .after = last_col())
}

# ===========================================================================
# READABLE LABELS -- plain-English names for the raw feature columns, per
# feedback_output_tone (translate model output into player-projection
# terms, not stats-column names). Union of RB + WR point features; a
# feature missing here falls back to its raw column name (never errors).
# ===========================================================================

FEATURE_LABELS <- c(
  prior_epa_per_opp          = "prior-season per-opportunity efficiency",
  baseline_epa_per_opp       = "career-baseline per-opportunity efficiency",
  rolling_epa_per_opp        = "recent-form per-opportunity efficiency",
  form_residual               = "hot/cold form vs baseline",
  is_cold_start_int           = "cold-start (no recent role history)",
  draft_tier_int               = "draft pedigree tier",
  def_rush_epa_adj             = "opponent run defense",
  def_short_pass_epa_adj       = "opponent short-pass defense",
  def_deep_pass_epa_adj        = "opponent deep-pass defense",
  wt_snap_share                 = "recent snap share",
  games_played_so_far           = "games played this season",
  def_used_fallback_int         = "opponent defense data fallback flag",
  team_spread                    = "Vegas point spread",
  implied_total                  = "Vegas implied team total",
  wt_carry_share                  = "recent carry share",
  wt_target_share                 = "recent target share",
  wt_team_total_plays             = "recent team play volume",
  baseline_carry_share             = "season carry share",
  baseline_target_share            = "season target share",
  baseline_snap_share               = "season snap share",
  baseline_team_total_plays         = "season team play volume",
  own_q_int                          = "own injury designation",
  own_practice_int                    = "practice participation",
  weeks_missed                         = "recent weeks missed to injury",
  return_from_absence                   = "returning from absence",
  above_new_out_share                    = "teammates newly out (share)",
  above_q_share                           = "teammates questionable (share)",
  above_long_out_share                     = "teammates on long-term out (share)",
  wt_air_yards_per_target                   = "recent air yards per target",
  wt_air_yards_share                         = "recent air yards share",
  baseline_air_yards_share                    = "season air yards share"
)

feature_label <- function(x) coalesce(FEATURE_LABELS[x], x)

# ===========================================================================
# explain_row(): one row of shap_contrib() output -> readable top-N drivers
# ===========================================================================

explain_row <- function(row, top_n = 5) {
  feat_cols <- grep("^feat_", names(row), value = TRUE)
  contribs <- tibble(
    feature = sub("^feat_", "", feat_cols),
    value   = unlist(row[feat_cols], use.names = FALSE)
  ) |>
    arrange(desc(abs(value))) |>
    head(top_n)

  lines <- sprintf("  %+5.2f FP  %s", contribs$value, feature_label(contribs$feature))
  c(
    sprintf("Base rate: %.2f FP", row$intercept),
    lines,
    sprintf("= %.2f FP predicted", row$pred_fp)
  )
}

# ===========================================================================
# CLI demo: most recent rows of the deployed arm's training table
# ===========================================================================

if (sys.nframe() == 0L || identical(environment(), globalenv())) {
  args     <- commandArgs(trailingOnly = TRUE)
  POSITION <- if (length(args) >= 1) toupper(args[1]) else cli_abort("Usage: Rscript R/21m_shap_explain.R <RB|WR> [n_demo_rows]")
  stopifnot(POSITION %in% c("RB", "WR"))
  N_DEMO <- if (length(args) >= 2) as.integer(args[2]) else 5L

  cli_h1("21m: SHAP explain demo -- {POSITION}")

  dp_fp <- readRDS("data/deployment_params_fp.rds")
  dpos  <- dp_fp[[tolower(POSITION)]]
  arm   <- dpos$arm
  cli_alert_info("Deployed arm: {arm} | trained through {dpos$trained_through$season}-W{dpos$trained_through$week}")

  train_path <- sprintf("data/fp_train_%s%s.rds", tolower(POSITION),
                        switch(arm, floorfree = "_floorfree", ""))
  ft <- readRDS(train_path) |>
    arrange(desc(season), desc(week)) |>
    head(N_DEMO)

  contrib <- shap_contrib(dp_fp, POSITION, ft)
  out <- bind_cols(
    ft |> select(player_id, player_name, season, week, fantasy_points_ppr) |>
      rename(actual_fp = fantasy_points_ppr),
    contrib
  )

  for (i in seq_len(nrow(out))) {
    r <- out[i, ]
    cli_h2("{r$player_name} -- {r$season} W{r$week} (actual {round(r$actual_fp,1)} FP)")
    cat(paste(explain_row(r, top_n = 5), collapse = "\n"), "\n\n")
  }

  out_path <- sprintf("output/21m_shap_demo_%s.csv", tolower(POSITION))
  readr::write_csv(out |> select(-starts_with("feat_"), starts_with("feat_")), out_path)
  cli_alert_success("{out_path} ({nrow(out)} rows, full per-feature contributions)")
  cli_h1("21m complete")
}
