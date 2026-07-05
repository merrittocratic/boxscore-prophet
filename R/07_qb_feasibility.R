# R/07_qb_feasibility.R
# Step 7 (feasibility): Is the QB position a clone of the RB/WR spine, or does
# it need the two-component (pass/rush) architecture? Answered with data
# BEFORE any model work, per house discipline.
#
# QUESTIONS THIS SCRIPT SETTLES:
#   Q1. Scoring/threshold: under 4pt-pass-TD standard scoring, which FP
#       thresholds hit at the rates that made 15/20 work for RB/WR
#       (~27% start / ~13% boom of starter games)?
#   Q2. Exchange rates: does a rushing EPA point buy materially more fantasy
#       than a passing EPA point? (If yes, a single total-EPA translation
#       mis-prices scramblers and the pipeline needs pass/rush separated.)
#   Q3. Functional form: is the residual flat across pass/rush EPA deciles,
#       or is there RB/WR-style TD convexity again?
#   Q4. Veto axis: do rushing tiers (statue / mover / scrambler) differ in
#       FP variance enough to justify stratifying the honesty check there?
#
# DATA: nflreadr weekly player stats (passing_epa, rushing_epa, fantasy_points
# = 4pt pass TD standard). Starter game = 15+ dropbacks (filters mop-up duty).

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(cli)
})

SEASONS      <- 2014L:2025L
MIN_DROPBACK <- 15L

skew <- function(x) mean((x - mean(x))^3) / sd(x)^3
fmt  <- function(x, d = 2) sprintf(paste0("%.", d, "f"), x)

# ===========================================================================
# 1. PULL QB PLAYER-WEEKS
# ===========================================================================

cli_h1("Step 1: Pull QB weekly stats")

qb <- nflreadr::load_player_stats(seasons = SEASONS) |>
  filter(season_type == "REG", position == "QB", !is.na(player_id)) |>
  mutate(
    dropbacks   = attempts + sacks_suffered,
    passing_epa = coalesce(passing_epa, 0),
    rushing_epa = coalesce(rushing_epa, 0),
    total_epa   = passing_epa + rushing_epa
  ) |>
  filter(!is.na(fantasy_points))

qb_starters <- qb |> filter(dropbacks >= MIN_DROPBACK)

cli_alert_success("QB player-weeks: {nrow(qb)} total | {nrow(qb_starters)} starter games (dropbacks >= {MIN_DROPBACK})")
cli_alert_info("Unique QBs in starter sample: {n_distinct(qb_starters$player_id)}")
cli_alert_info("Starter games per season: ~{round(nrow(qb_starters) / length(SEASONS))}")

# Sanity: verify fantasy_points is 4pt-pass-TD standard by recomputing
qb_check <- qb_starters |>
  mutate(fp_manual = 0.04 * passing_yards + 4 * passing_tds - 2 * passing_interceptions +
           0.1 * rushing_yards + 6 * rushing_tds -
           2 * (sack_fumbles_lost + rushing_fumbles_lost + receiving_fumbles_lost) +
           2 * passing_2pt_conversions + 2 * rushing_2pt_conversions +
           0.1 * receiving_yards + 6 * receiving_tds + 2 * receiving_2pt_conversions)
fp_gap <- mean(abs(qb_check$fp_manual - qb_check$fantasy_points))
cli_alert_info("fantasy_points vs manual 4pt recompute: mean |gap| = {fmt(fp_gap, 3)} pts (small = 4pt confirmed)")

# ===========================================================================
# 2. Q1: THRESHOLD RATES
# ===========================================================================

cli_h1("Q1: Threshold hit rates (starter games, 4pt pass TD)")

thresholds <- 14:28
thresh_tbl <- tibble(
  threshold = thresholds,
  hit_rate  = vapply(thresholds, function(t) mean(qb_starters$fantasy_points >= t), numeric(1))
) |>
  mutate(hit_pct = sprintf("%.1f%%", 100 * hit_rate))

print(as.data.frame(thresh_tbl |> select(threshold, hit_pct)))
cli_alert_info("RB/WR reference rates: start threshold hit ~27% of games, boom ~13%")

# ===========================================================================
# 3. Q2: EXCHANGE RATES -- PASS EPA VS RUSH EPA
# ===========================================================================

cli_h1("Q2: Fantasy exchange rates per EPA point")

fit_split <- lm(fantasy_points ~ passing_epa + rushing_epa + dropbacks + carries,
                data = qb_starters)
fit_total <- lm(fantasy_points ~ total_epa + I(dropbacks + carries),
                data = qb_starters)

cli_h2("Two-component model: fp ~ pass_epa + rush_epa + dropbacks + carries")
print(round(summary(fit_split)$coefficients, 3))
cli_alert_info("R-squared: {fmt(summary(fit_split)$r.squared, 3)} | resid SD: {fmt(sigma(fit_split))} | resid skew: {fmt(skew(residuals(fit_split)))}")

cli_h2("Single-total model: fp ~ total_epa + total_volume")
cli_alert_info("R-squared: {fmt(summary(fit_total)$r.squared, 3)} | resid SD: {fmt(sigma(fit_total))} | resid skew: {fmt(skew(residuals(fit_total)))}")

b <- coef(fit_split)
cli_alert_info("Exchange rates: {fmt(b[['passing_epa']])} FP per passing EPA vs {fmt(b[['rushing_epa']])} FP per rushing EPA ({fmt(b[['rushing_epa']] / b[['passing_epa']], 1)}x)")

# Does the single-total model mis-price scramblers? Residual by rushing tier.
qb_starters <- qb_starters |>
  mutate(rush_tier = cut(carries, c(-Inf, 4, 8, Inf),
                         labels = c("statue (0-3)", "mover (4-7)", "scrambler (8+)"),
                         right = FALSE))

bias_tbl <- qb_starters |>
  mutate(res_total = residuals(fit_total), res_split = residuals(fit_split)) |>
  group_by(rush_tier) |>
  summarise(n = n(),
            bias_total_model = round(mean(res_total), 2),
            bias_split_model = round(mean(res_split), 2), .groups = "drop")

cli_h2("Mean residual (bias) by rushing tier -- the scrambler mis-pricing test")
print(as.data.frame(bias_tbl))

# ===========================================================================
# 4. Q3: FUNCTIONAL FORM -- RESIDUAL BY EPA DECILE
# ===========================================================================

cli_h1("Q3: Residual mean by EPA decile (TD convexity check)")

decile_tbl <- qb_starters |>
  mutate(res = residuals(fit_split),
         pass_dec = ntile(passing_epa, 10),
         rush_dec = ntile(rushing_epa, 10))

cli_h2("By passing EPA decile")
print(as.data.frame(decile_tbl |> group_by(dec = pass_dec) |>
  summarise(n = n(), epa_mid = round(median(passing_epa), 1),
            mean_res = round(mean(res), 2), skew_res = round(skew(res), 2))))

cli_h2("By rushing EPA decile")
print(as.data.frame(decile_tbl |> group_by(dec = rush_dec) |>
  summarise(n = n(), epa_mid = round(median(rushing_epa), 1),
            mean_res = round(mean(res), 2), skew_res = round(skew(res), 2))))

# ===========================================================================
# 5. Q4: VETO AXIS -- VARIANCE BY RUSHING TIER
# ===========================================================================

cli_h1("Q4: FP distribution by rushing tier (veto axis check)")

tier_tbl <- qb_starters |>
  group_by(rush_tier) |>
  summarise(n = n(),
            mean_fp = round(mean(fantasy_points), 1),
            sd_fp   = round(sd(fantasy_points), 1),
            skew_fp = round(skew(fantasy_points), 2),
            p_18    = sprintf("%.1f%%", 100 * mean(fantasy_points >= 18)),
            p_24    = sprintf("%.1f%%", 100 * mean(fantasy_points >= 24)),
            .groups = "drop")
print(as.data.frame(tier_tbl))

# ===========================================================================
# 6. SAVE
# ===========================================================================

cli_h1("Save outputs")

readr::write_csv(thresh_tbl, "output/07_qb_feasibility_thresholds.csv")
readr::write_csv(bias_tbl,   "output/07_qb_feasibility_tier_bias.csv")
readr::write_csv(tier_tbl,   "output/07_qb_feasibility_tier_dist.csv")

cli_alert_success("output/07_qb_feasibility_thresholds.csv")
cli_alert_success("output/07_qb_feasibility_tier_bias.csv")
cli_alert_success("output/07_qb_feasibility_tier_dist.csv")

cli_h1("Feasibility pass complete")
