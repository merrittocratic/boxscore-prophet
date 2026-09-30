# R/21b_discrimination_baseline.R
# Stage A, step 2 of the D29 single-stage rebuild: compute and FREEZE the
# in-band discrimination numbers for (a) ECR and (b) the incumbent two-stage
# model, using R/21a's ruler, BEFORE any single-stage candidate model
# exists. This ordering is deliberate -- see R/21a's header. Nothing after
# this point may recompute the numbers a candidate arm (R/21d onward) gets
# graded against; output/21b_disc_baseline.csv is the frozen reference.
#
# INCUMBENT MODEL SOURCE (the actually-shipped recal method per position,
# per data/fp_recal_maps.rds / data/te_fp_recal_maps.rds as of 2026-09-06):
#   RB  star_platt  15+/20+  -- output/18d_rb_star_recal_probabilities.csv
#                                (p_start_new / p_boom_new), walk-forward,
#                                2023-2025 only (D27 ship).
#   WR  strat_platt 15+ / iso 20+ -- output/06c_volfix_10acand_recal_
#                                probabilities.csv (p_start_strat_platt /
#                                p_boom_iso), walk-forward, 2023-2025 only.
#   TE  platt_vol_vegas 12+ / platt 17+ -- output/12e_te_volfix_10acand_
#                                recal_probabilities.csv (p_start_platt_
#                                vol_vegas / p_boom_platt), 2023-2025 only.
# All three incumbent files are walk-forward weekly refits within their own
# source scripts -- no leakage from reusing them here.
#
# WINDOW HONESTY: the incumbent model's walk-forward probabilities only
# exist for 2023-2025 (~39-41 season-weeks per position) -- verified by
# inspection, same limitation the D25 market-edge backtest had. ECR history
# covers 2016-2025. This script therefore reports THREE things per
# position, never conflating them:
#   ecr_full     -- ECR's own discrimination over its full covered window
#                   (2016-2025). This is what a full-window single-stage
#                   candidate (R/21d onward) will actually be graded
#                   against, since the new model is not limited to 2023+.
#   ecr_matched  -- ECR's discrimination restricted to the SAME 2023-2025
#                   window the incumbent model is measurable in. Exists
#                   only so incumbent-vs-ECR is an apples-to-apples paired
#                   comparison.
#   incumbent    -- the two-stage model, 2023-2025 only.
# The ship gate for future candidates compares against ecr_full. Only
# ecr_matched vs incumbent is a fair paired delta.
#
# Usage: Rscript R/21b_discrimination_baseline.R

source("R/21a_discrimination_fns.R")

cli_h1("21b: freezing the discrimination baseline (ECR + incumbent model)")

THRESH <- list(RB = c(start = 15, boom = 20),
               WR = c(start = 15, boom = 20),
               TE = c(start = 12, boom = 17))

INCUMBENT_SEASONS <- 2023:2025
FULL_SEASONS      <- 2016:2025

# ---------------------------------------------------------------------------
# Incumbent model probabilities, one row per (position, player_id, season,
# week) with p_model_start / p_model_boom in the SHIPPED method's units.
# ---------------------------------------------------------------------------
incumbent_rb <- read_csv("output/18d_rb_star_recal_probabilities.csv",
                         show_col_types = FALSE) |>
  transmute(position = "RB", gsis_id = player_id, season, week,
            p_model_start = p_start_new, p_model_boom = p_boom_new)

incumbent_wr <- read_csv("output/06c_volfix_10acand_recal_probabilities.csv",
                         show_col_types = FALSE) |>
  filter(position == "WR") |>
  transmute(position, gsis_id = player_id, season, week,
            p_model_start = p_start_strat_platt, p_model_boom = p_boom_iso)

incumbent_te <- read_csv("output/12e_te_volfix_10acand_recal_probabilities.csv",
                         show_col_types = FALSE) |>
  transmute(position = "TE", gsis_id = player_id, season, week,
            p_model_start = p_start_platt_vol_vegas, p_model_boom = p_boom_platt)

incumbent <- bind_rows(incumbent_rb, incumbent_wr, incumbent_te) |>
  filter(!is.na(p_model_start), !is.na(p_model_boom))

cli_alert_info("Incumbent probabilities loaded: RB={nrow(incumbent_rb)} WR={nrow(incumbent_wr)} TE={nrow(incumbent_te)}")

# ---------------------------------------------------------------------------
# Per-position cells
# ---------------------------------------------------------------------------
positions <- c("RB", "WR", "TE")

results <- map(positions, function(pos) {
  th <- THRESH[[pos]]

  ecr_full_d <- band_universe(pos, FULL_SEASONS, th["start"], th["boom"]) |>
    mutate(ecr_score = -pos_rank)
  ecr_full <- disc_cell(ecr_full_d, "ecr_score") |>
    mutate(position = pos, cut = "ecr_full", .before = 1)

  ecr_matched_d <- band_universe(pos, INCUMBENT_SEASONS, th["start"], th["boom"]) |>
    mutate(ecr_score = -pos_rank)
  ecr_matched <- disc_cell(ecr_matched_d, "ecr_score") |>
    mutate(position = pos, cut = "ecr_matched", .before = 1)

  # Paired universe: ECR band rows AND an incumbent model score, same
  # (season, week, gsis_id) key, 2023-2025 only.
  paired_d <- ecr_matched_d |>
    inner_join(incumbent |> filter(position == pos),
               by = c("season", "week", "gsis_id"),
               suffix = c("", ".inc"))

  inc_cell <- disc_cell(paired_d, "p_model_start") |>
    mutate(position = pos, cut = "incumbent", .before = 1)

  cmp_spearman <- disc_compare(paired_d, "ecr_score", "p_model_start",
                               stat = "spearman") |>
    mutate(position = pos, .before = 1)
  cmp_concordance <- disc_compare(paired_d, "ecr_score", "p_model_start",
                                  stat = "concordance") |>
    mutate(position = pos, .before = 1)

  list(
    cells = bind_rows(ecr_full, ecr_matched, inc_cell),
    compare = bind_rows(cmp_spearman, cmp_concordance)
  )
})

cells   <- map(results, "cells")   |> list_rbind()
compare <- map(results, "compare") |> list_rbind()

cli_h2("Frozen discrimination cells (point estimate + 95% CI)")
print(cells |> select(position, cut, n, weeks, spearman, spearman_lo, spearman_hi,
                       concordance, auc) |>
        mutate(across(where(is.numeric) & !c(n, weeks), ~round(.x, 4))) |>
        as.data.frame(), row.names = FALSE)

cli_h2("Incumbent vs ECR, matched 2023-2025 window (paired bootstrap, positive = incumbent better)")
print(compare |> mutate(across(where(is.numeric), ~round(.x, 4))) |> as.data.frame(),
      row.names = FALSE)

write_csv(cells, "output/21b_disc_baseline_cells.csv")
write_csv(compare, "output/21b_disc_baseline_incumbent_vs_ecr.csv")

cli_h1("21b complete -- output/21b_disc_baseline_*.csv are now the frozen reference")
cli_alert_warning("These numbers must not be recomputed to move the goalposts once R/21d candidates exist.")
