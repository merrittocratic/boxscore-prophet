# R/21f1_ae_vs_a2_compare.R
# PFF-enrichment build, step 9 (see ~/.claude/plans/dapper-sleeping-lollipop.md):
# the actual pre-registered bar -- "A3 must beat A2, not just A1." R/21f
# only grades an arm against ECR; this compares A2's and A3's pred_fp
# directly against EACH OTHER, on the identical in-band universe, via the
# same paired week-clustered bootstrap (R/21a::disc_compare) that already
# powers the ECR comparison in R/21f.
#
# Usage: Rscript R/21f1_ae_vs_a2_compare.R <RB|WR>

suppressPackageStartupMessages({
  library(tidyverse)
  library(cli)
})
source("R/21a_discrimination_fns.R")

args     <- commandArgs(trailingOnly = TRUE)
POSITION <- if (length(args) >= 1) toupper(args[1]) else cli_abort("Usage: Rscript R/21f1_ae_vs_a2_compare.R <RB|WR>")
stopifnot(POSITION %in% c("RB", "WR"))
WIDE_GRID <- Sys.getenv("WIDE_GRID", unset = "0") == "1"

THRESH <- list(RB = c(start = 15, boom = 20), WR = c(start = 15, boom = 20))
FULL_SEASONS <- 2016:2025
th <- THRESH[[POSITION]]

cli_h1("21f1: A3 (AE) vs A2 (enriched raw-lagged){if (WIDE_GRID) ' -- WIDE GRID' else ''}, {POSITION}")

a2_path <- sprintf("output/21d_%s_lagusage_pff%s_fold_predictions.csv", tolower(POSITION), if (WIDE_GRID) "_wide" else "")
a3_path <- sprintf("output/21d_%s_ae%s_fold_predictions.csv", tolower(POSITION), if (WIDE_GRID) "_wide" else "")

a2 <- read_csv(a2_path, show_col_types = FALSE) |>
  mutate(player_id = as.character(player_id)) |>
  select(season, week, player_id, pred_fp_a2 = pred_fp)
a3 <- read_csv(a3_path, show_col_types = FALSE) |>
  mutate(player_id = as.character(player_id)) |>
  select(season, week, player_id, pred_fp_a3 = pred_fp)

band_d <- band_universe(POSITION, FULL_SEASONS, th["start"], th["boom"])

paired <- band_d |>
  inner_join(a2, by = c("season", "week", "gsis_id" = "player_id")) |>
  inner_join(a3, by = c("season", "week", "gsis_id" = "player_id"))

cli_alert_info(
  "In-band paired universe (A2 AND A3 both scored): {nrow(paired)} rows / {n_distinct(paste(paired$season, paired$week))} season-weeks (band universe had {nrow(band_d)} rows)"
)

if (nrow(paired) < 30) {
  cli_abort("Too few paired in-band rows ({nrow(paired)}) to grade.")
}

cmp_spearman    <- disc_compare(paired, "pred_fp_a2", "pred_fp_a3", stat = "spearman")
cmp_concordance <- disc_compare(paired, "pred_fp_a2", "pred_fp_a3", stat = "concordance")
cmp_auc         <- disc_compare(paired, "pred_fp_a2", "pred_fp_a3", stat = "auc")

compare <- bind_rows(cmp_spearman, cmp_concordance, cmp_auc) |>
  mutate(position = POSITION, comparison = "ae_vs_lagusage_pff", .before = 1)

cli_h2("A3 (pred_fp_a3) vs A2 (pred_fp_a2) -- paired bootstrap, positive = A3 better")
print(compare |> mutate(across(where(is.numeric), ~round(.x, 4))) |> as.data.frame(), row.names = FALSE)

# Verdict: same delta bars as the ship gate (+0.02 Spearman / +0.01
# concordance), but NO ECR floor condition -- that concept is specific to
# beating a market baseline and doesn't apply to an arm-vs-arm test. PASS
# requires CI excluding 0 in A3's favor AND the delta bar; split primaries
# = NEUTRAL, not PASS.
DELTA_BAR <- c(spearman = 0.02, concordance = 0.01)
verdicts <- compare |>
  filter(stat %in% names(DELTA_BAR)) |>
  mutate(
    bar = DELTA_BAR[stat],
    verdict = case_when(
      ci_hi < 0 ~ "FAIL",
      ci_lo > 0 & delta >= bar ~ "PASS",
      .default = "NEUTRAL"
    )
  )

cli_h1("VERDICT -- {POSITION}: does the AE's compression beat the enriched raw-lagged control?")
print(verdicts |> select(position, stat, n, weeks, stat_a, stat_b, delta, ci_lo, ci_hi, verdict) |>
        mutate(across(where(is.numeric), ~round(.x, 4))) |> as.data.frame(), row.names = FALSE)

dir.create("output", showWarnings = FALSE)
out_path <- sprintf("output/21f1_%s_ae_vs_a2%s_verdict.csv", tolower(POSITION), if (WIDE_GRID) "_wide" else "")
write_csv(verdicts, out_path)
cli_alert_success("{out_path}")
cli_h1("21f1 complete -- {POSITION}")
