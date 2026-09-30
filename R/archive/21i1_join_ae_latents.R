# R/21i1_join_ae_latents.R
# PFF-enrichment build, step 8 prep: join R/21i gate (a)'s per-season-
# refit latents onto the PLAIN fp_train_<pos>.rds table (A1's base table,
# NOT the lagusage variant -- A3 is "A1 + AE latents," not "A1 + lag
# columns + AE latents," so the latents are tested as a substitute for the
# raw lag information, not a supplement to it). 2014 rows get genuine NA
# on z1-z8 (no per-season-refit model exists for 2014, by design) --
# LightGBM handles NA natively via its missing-value split direction,
# which is the correct treatment here, not a 0-fill that would falsely
# claim "no role signal" for what could be a real workhorse season.
#
# Usage: Rscript R/21i1_join_ae_latents.R <RB|WR>

suppressPackageStartupMessages({
  library(tidyverse)
  library(cli)
})

args     <- commandArgs(trailingOnly = TRUE)
POSITION <- if (length(args) >= 1) toupper(args[1]) else cli_abort("Usage: Rscript R/21i1_join_ae_latents.R <RB|WR>")
stopifnot(POSITION %in% c("RB", "WR"))

cli_h1("21i1: join AE latents onto fp_train_{tolower(POSITION)}.rds")

ft <- readRDS(sprintf("data/fp_train_%s.rds", tolower(POSITION)))
latents <- readRDS(sprintf("output/21i_ae_latents_%s_pff.rds", tolower(POSITION))) |>
  mutate(player_id = as.character(player_id))

out <- ft |> left_join(latents, by = c("player_id", "season", "week"))
matched <- sum(!is.na(out$z1))
cli_alert_info("{nrow(ft)} fp_train rows -> {matched} matched a latent ({round(100 * matched / nrow(ft), 1)}%), {nrow(ft) - matched} genuinely NA (2014 rows + any unmatched)")

out_path <- sprintf("data/fp_train_%s_ae.rds", tolower(POSITION))
saveRDS(out, out_path)
cli_alert_success("{out_path} written")
cli_h1("21i1 complete -- {POSITION}")
