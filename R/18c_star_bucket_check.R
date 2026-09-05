# R/18c_star_bucket_check.R
# PRE-REGISTERED D26 STAGE 1 (bars signed by Steve 2026-09-05, amended:
# star = top-24 trailing FP, TE top-12; confirmation bar 7pp): are the
# shipped RB/WR start/boom probabilities systematically underconfident
# for star players? Follow-on from D25 (market-edge FAIL) via the 18b
# diagnostics (deficit concentrated in model-bearish disagreements and
# ECR ranks 1-24; Achane/CMC/Bijan-class rows).
#
# DESIGN (locked before running):
#   Data: shipped walk-forward probabilities only, 2023-2025 --
#     RB/WR from output/06c_volfix_10acand_recal_probabilities.csv
#     (RB p_start/p_boom raw; WR p_start raw, p_boom = p_boom_iso),
#     TE (reported tier) from 12e (raw/raw). hit_start/hit_boom columns
#     are the same outcomes graded in D25. PURELY INTERNAL: no ECR.
#   Star (primary, ex-ante): trailing PPR FP per game over the last 17
#     REG games played strictly before the week (cross-season), >= 6
#     games required; rank within (position, season, week) among the
#     model-universe rows with trailing history; star = rank <= 24
#     (TE <= 12).
#   Star (secondary, reported): top decile of pred_vol within
#     position-week.
#   Measurement per cell (RB-start, RB-boom, WR-start, WR-boom graded;
#     TE reported): gap = realized hit rate - mean predicted p inside
#     the star bucket; week-clustered bootstrap 95% CI (B = 2000,
#     seed 42). Non-star bucket as control.
#   EXPECTATION: star gap >= +10pp on RB/WR start; boom same sign,
#     smaller; non-star ~0.
#   DECISION RULE (locked): CONFIRMED for a cell iff
#     star_gap >= +0.07 AND bootstrap CI excludes 0 AND
#     (star_gap - nonstar_gap) >= +0.05  [star-specific, not global].
#     A large gap in BOTH buckets = inconsistency flag (contradicts 06c
#     pooled calibration), investigate, not a confirmation.
#     Otherwise DISCONFIRMED -> published null; star shrinkage dropped
#     as the D25 explanation.
#   Reported, non-binding: trailing-rank sub-slices 1-12 / 13-24;
#     reliability by predicted-probability quintile in the star bucket;
#     star-bucket calibration gap + Brier for every walk-forward
#     candidate variant (recon for Stage 2; NOT grounds for a ship).
#   Stage 2 (fix + 18a re-run) proceeds only on confirmation, with its
#   own signed bars.
#
# Usage: Rscript R/18c_star_bucket_check.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(cli)
})

set.seed(42)
B_BOOT <- 2000L
STAR_CUT <- c(RB = 24L, WR = 24L, TE = 12L)

cli_h1("18c: star-bucket calibration check (pre-registered D26 stage 1)")

# ============================================================== inputs --
rbwr <- read_csv("output/06c_volfix_10acand_recal_probabilities.csv",
                 show_col_types = FALSE) |>
  mutate(p_ship_start = p_start,
         p_ship_boom  = if_else(position == "WR", p_boom_iso, p_boom))
te <- read_csv("output/12e_te_volfix_10acand_recal_probabilities.csv",
               show_col_types = FALSE) |>
  mutate(position = "TE", p_ship_start = p_start, p_ship_boom = p_boom)

probs <- bind_rows(rbwr, te) |>
  filter(!is.na(p_ship_start), !is.na(p_ship_boom),
         !is.na(hit_start), !is.na(hit_boom))

cli_alert_info("{nrow(probs)} shipped player-weeks ({paste(count(probs, position)$n, collapse = '/')} by position)")

# ================================================ trailing star metric --
# Last-17-games PPR FP per game, strictly before (season, week),
# cross-season, >= 6 games. Point-in-time by construction.
cli_h2("Trailing FP/game (last 17 played, ex-ante)")

game_log <- load_player_stats(2016:2025) |>
  filter(season_type == "REG", !is.na(player_id),
         !is.na(fantasy_points_ppr)) |>
  select(player_id, season, week, fantasy_points_ppr) |>
  arrange(player_id, season, week)

trailing <- game_log |>
  group_by(player_id) |>
  mutate(
    g = row_number(),
    cum = cumsum(fantasy_points_ppr),
    cum_lag17 = lag(cum, 17, default = 0),
    prior_games = g - 1L,
    trail_n  = pmin(prior_games, 17L),
    trail_fp = if_else(
      prior_games > 0,
      (lag(cum, 1, default = 0) - if_else(prior_games > 17L,
                                          lag(cum, 18, default = 0), 0)) /
        trail_n,
      NA_real_)
  ) |>
  ungroup() |>
  select(player_id, season, week, trail_fp, trail_n)

probs <- probs |>
  left_join(trailing, by = c("player_id", "season", "week")) |>
  group_by(position, season, week) |>
  mutate(
    trail_rank = ifelse(!is.na(trail_fp) & trail_n >= 6,
                        rank(-ifelse(trail_n >= 6, trail_fp, -Inf),
                             ties.method = "first"),
                        NA_integer_),
    star = !is.na(trail_rank) & trail_rank <= STAR_CUT[position],
    vol_star = pred_vol >= quantile(pred_vol, 0.9)
  ) |>
  ungroup()

cli_alert_info("Star rows (trailing top-{STAR_CUT['RB']}/{STAR_CUT['TE']}): {sum(probs$star)} | pred_vol top-decile rows: {sum(probs$vol_star)}")

# ============================================================= scoring --
gap_ci <- function(d, p_col, hit_col) {
  wk <- paste(d$season, d$week)
  ibw <- split(seq_len(nrow(d)), wk); uw <- names(ibw)
  gaps <- map_dbl(seq_len(B_BOOT), function(b) {
    i <- unlist(ibw[sample(uw, length(uw), TRUE)], use.names = FALSE)
    mean(d[[hit_col]][i]) - mean(d[[p_col]][i])
  })
  tibble(
    n = nrow(d), weeks = length(uw),
    mean_pred = mean(d[[p_col]]), realized = mean(d[[hit_col]]),
    gap = mean(d[[hit_col]]) - mean(d[[p_col]]),
    ci_lo = quantile(gaps, 0.025), ci_hi = quantile(gaps, 0.975)
  )
}

cell_check <- function(pos, outcome, tier) {
  p_col <- paste0("p_ship_", outcome); h_col <- paste0("hit_", outcome)
  d <- probs |> filter(position == pos)
  s  <- gap_ci(d |> filter(star),  p_col, h_col) |> mutate(bucket = "star")
  ns <- gap_ci(d |> filter(!star), p_col, h_col) |> mutate(bucket = "non_star")
  both <- bind_rows(s, ns) |>
    mutate(position = pos, outcome = outcome, tier = tier, .before = 1)
  verdict <- case_when(
    s$gap >= 0.07 & s$ci_lo > 0 & (s$gap - ns$gap) >= 0.05 &
      !(ns$gap >= 0.07 & ns$ci_lo > 0) ~ "CONFIRMED",
    s$gap >= 0.07 & s$ci_lo > 0 & ns$gap >= 0.07 & ns$ci_lo > 0
      ~ "INCONSISTENT (global gap)",
    .default = "DISCONFIRMED"
  )
  both |> mutate(verdict = if_else(bucket == "star", verdict, ""))
}

CELLS <- tribble(
  ~pos, ~outcome, ~tier,
  "RB", "start", "graded", "RB", "boom", "graded",
  "WR", "start", "graded", "WR", "boom", "graded",
  "TE", "start", "reported", "TE", "boom", "reported"
)

results <- pmap(CELLS, cell_check) |> list_rbind()

cli_h1("PRIMARY RESULT (trailing-FP star buckets, bars locked)")
print(results |> mutate(across(where(is.numeric), ~round(.x, 4))) |>
        as.data.frame(), row.names = FALSE)

# ======================================================== reported cuts --
cli_h2("Reported: trailing-rank gradient (1-12 vs 13-24), start prob")
grad <- probs |>
  filter(!is.na(trail_rank), trail_rank <= 24, position != "TE") |>
  mutate(slice = if_else(trail_rank <= 12, "rank 1-12", "rank 13-24")) |>
  group_by(position, slice) |>
  summarise(n = n(), mean_pred = mean(p_ship_start),
            realized = mean(hit_start), gap = realized - mean_pred,
            gap_boom = mean(hit_boom) - mean(p_ship_boom), .groups = "drop")
print(grad |> mutate(across(where(is.numeric), ~round(.x, 4))) |>
        as.data.frame(), row.names = FALSE)

cli_h2("Reported: secondary star definition (pred_vol top decile)")
vol_res <- map(c("RB", "WR", "TE"), function(pos) {
  d <- probs |> filter(position == pos)
  bind_rows(
    gap_ci(d |> filter(vol_star), "p_ship_start", "hit_start") |>
      mutate(bucket = "vol_star"),
    gap_ci(d |> filter(!vol_star), "p_ship_start", "hit_start") |>
      mutate(bucket = "non")
  ) |> mutate(position = pos, .before = 1)
}) |> list_rbind()
print(vol_res |> mutate(across(where(is.numeric), ~round(.x, 4))) |>
        as.data.frame(), row.names = FALSE)

cli_h2("Reported: reliability by predicted-prob quintile, star bucket")
rel <- probs |> filter(star) |>
  group_by(position,
           q = cut(p_ship_start, quantile(p_ship_start, seq(0, 1, 0.2)),
                   include.lowest = TRUE, labels = paste0("Q", 1:5))) |>
  summarise(n = n(), mean_pred = mean(p_ship_start),
            realized = mean(hit_start), .groups = "drop")
print(rel |> mutate(across(where(is.numeric), ~round(.x, 3))) |>
        as.data.frame(), row.names = FALSE)

cli_h2("Reported: candidate-variant recon in star bucket (NOT a ship basis)")
var_cols <- grep("^p_(start|boom)_", names(probs), value = TRUE)
recon <- map(var_cols, function(v) {
  oc <- if (startsWith(v, "p_start")) "hit_start" else "hit_boom"
  d <- probs |> filter(star, !is.na(.data[[v]]), position != "TE")
  tibble(variant = v, n = nrow(d),
         gap = mean(d[[oc]]) - mean(d[[v]]),
         brier = mean((d[[v]] - d[[oc]])^2))
}) |> list_rbind() |> arrange(brier)
print(recon |> mutate(across(where(is.numeric), ~round(.x, 4))) |>
        as.data.frame(), row.names = FALSE)

write_csv(results, "output/18c_star_calibration.csv")
write_csv(grad, "output/18c_star_gradient.csv")
write_csv(vol_res, "output/18c_star_volstar.csv")
write_csv(rel, "output/18c_star_reliability.csv")
write_csv(recon, "output/18c_star_variant_recon.csv")

cli_h1("18c complete -- graded against the locked D26 bars")
