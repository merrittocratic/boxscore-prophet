# R/21c1_lagged_usage_features.R
# Stage B, step 3 of the D29 single-stage rebuild: flat raw-lagged-usage
# features for ablation arm A2 (A1 + 42 raw lagged usage columns). This is
# the AE's control arm -- same 6 usage channels the AE tensor uses, same
# trailing-window/no-season-crossing rule, but flattened into named columns
# instead of windowed into a (T,C) tensor, so a plain LightGBM can eat them
# directly. If LightGBM on these 42 columns matches the AE's latents, the
# AE is a compression result, not a modeling result -- that's the whole
# point of A2 existing.
#
# NOTE: this flat arm has no equivalent to the AE's mask channel (real vs
# padded/bye/pre-debut week) -- "42 raw lagged usage columns" was
# pre-registered in the plan before the AE build started, and changing that
# column count now would be moving a pre-registered bar after the fact.
# Documented here, not silently fixed.
#
# Usage: Rscript R/21c1_lagged_usage_features.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(cli)
})

N_LAGS <- 7L

CHANNELS_MAP <- list(
  RB = c("snap_share", "carry_share_obs", "target_share_obs",
         "rz_carry_share", "rz_target_share", "routes_proxy"),
  WR = c("snap_share", "target_share_obs", "air_yards_share_obs",
         "air_yards_per_target_obs", "rz_target_share", "routes_proxy"),
  TE = c("snap_share", "target_share_obs", "air_yards_share_obs",
         "air_yards_per_target_obs", "rz_target_share", "routes_proxy")
)

# PFF charting channels (R/21j/21c0a) -- added only when AE_ENRICH_PFF=1,
# only for RB/WR (no PFF pull exists for TE in this round). Same field
# list as R/21g's enriched tensor, so A2's enriched control mirrors the
# AE's enriched input exactly -- that mirroring is the whole point of A2.
PFF_CHANNELS_MAP <- list(
  RB = c("yco_attempt", "avoided_tackle_rate", "breakaway_percent",
         "zone_attempt_share", "gap_attempt_share"),
  WR = c("contested_catch_rate", "avoided_tackle_rate", "drop_rate",
         "route_rate", "avg_depth_of_target", "yac_per_reception")
)
ENRICH_PFF <- Sys.getenv("AE_ENRICH_PFF", unset = "0") == "1"

mk_key <- function(pid, s, w) paste(pid, s, w, sep = "_")

# For every row in usage_seq_<pos>.rds, build lag1..lag7 of each channel
# (7 x 6 = 42 columns). A missing lag (bye/DNP/pre-debut, or W-K < 1) is 0 --
# a plain R environment keyed by pasted (player,season,week) makes both
# cases fall out of the same lookup-miss path, same trick as R/21g's tensor
# lookup.
build_lagged <- function(position) {
  channels <- CHANNELS_MAP[[position]]
  if (ENRICH_PFF) channels <- c(channels, PFF_CHANNELS_MAP[[position]])
  usage_path <- sprintf("data/usage_seq_%s%s.rds", tolower(position), if (ENRICH_PFF) "_pff" else "")
  usage <- readRDS(usage_path) |>
    filter(!is.na(player_id))
  cli_alert_info("{position}: {nrow(usage)} usage rows (NA player_id dropped)")

  lookup <- new.env(parent = emptyenv())
  keys <- mk_key(usage$player_id, usage$season, usage$week)
  chan_mat <- as.matrix(usage[, channels])
  for (i in seq_len(nrow(usage))) assign(keys[i], chan_mat[i, ], envir = lookup)

  N <- nrow(usage)
  col_names <- unlist(lapply(seq_len(N_LAGS), function(k) paste0("lag", k, "_", channels)))
  lag_mat <- matrix(0, nrow = N, ncol = length(col_names), dimnames = list(NULL, col_names))

  for (i in seq_len(N)) {
    pid <- usage$player_id[i]; s <- usage$season[i]; w <- usage$week[i]
    for (k in seq_len(N_LAGS)) {
      hit <- get0(mk_key(pid, s, w - k), envir = lookup, inherits = FALSE)
      if (!is.null(hit)) {
        cols <- ((k - 1) * length(channels) + 1):(k * length(channels))
        lag_mat[i, cols] <- hit
      }
    }
  }

  list(
    lagged = usage |> select(player_id, season, week) |> bind_cols(as_tibble(lag_mat)),
    usage  = usage,
    channels = channels
  )
}

# Sample >=500 non-week-1 rows and directly recompute lag1_<channel> against
# usage_seq itself (not against the lookup env) -- an independent check that
# the flattening didn't introduce an off-by-one or a season-boundary leak.
leakage_check <- function(position, built) {
  usage <- built$usage
  channels <- built$channels
  candidates <- built$lagged |> filter(week > 1)
  n_check <- min(500L, nrow(candidates))
  set.seed(42)
  samp <- candidates |> slice_sample(n = n_check)

  mismatches <- 0L
  for (i in seq_len(nrow(samp))) {
    pid <- samp$player_id[i]; s <- samp$season[i]; w <- samp$week[i]
    prior <- usage |> filter(player_id == pid, season == s, week == w - 1L)
    for (ch in channels) {
      expected <- if (nrow(prior) == 1L) prior[[ch]] else 0
      got <- samp[[paste0("lag1_", ch)]][i]
      if (!isTRUE(all.equal(unname(expected), unname(got)))) mismatches <- mismatches + 1L
    }
  }
  if (mismatches > 0L) {
    cli_abort("{position}: leakage sanity check FAILED -- {mismatches} lag1 mismatches out of {n_check * length(channels)} checked")
  }
  cli_alert_success("{position}: leakage sanity check PASSED -- {n_check} sampled rows x {length(channels)} channels, 0 mismatches")
}

cli_h1("21c1: raw lagged usage features (A2 control arm){if (ENRICH_PFF) ' -- PFF-enriched' else ''}")

specs <- if (ENRICH_PFF) {
  tribble(
    ~position, ~ft_path,               ~out_path,
    "RB",      "data/fp_train_rb.rds", "data/fp_train_rb_lagusage_pff.rds",
    "WR",      "data/fp_train_wr.rds", "data/fp_train_wr_lagusage_pff.rds"
  )
} else {
  tribble(
    ~position, ~ft_path,               ~out_path,
    "RB",      "data/fp_train_rb.rds", "data/fp_train_rb_lagusage.rds",
    "WR",      "data/fp_train_wr.rds", "data/fp_train_wr_lagusage.rds",
    "TE",      "data/fp_train_te.rds", "data/fp_train_te_lagusage.rds"
  )
}

pwalk(specs, function(position, ft_path, out_path) {
  built <- build_lagged(position)
  leakage_check(position, built)

  ft  <- readRDS(ft_path)
  lag_cols <- setdiff(names(built$lagged), c("player_id", "season", "week"))
  out <- ft |> left_join(built$lagged, by = c("player_id", "season", "week"))

  matched <- sum(!is.na(out[[lag_cols[1]]]))
  out <- out |> mutate(across(all_of(lag_cols), ~ coalesce(.x, 0)))

  cli_alert_success(
    "{position}: {nrow(ft)} fp_train rows -> {matched} matched a usage_seq lag row ({round(100 * matched / nrow(ft), 1)}%), {nrow(ft) - matched} coalesced to 0"
  )
  saveRDS(out, out_path)
  cli_alert_success("{out_path} written ({nrow(out)} rows, {length(lag_cols)} lag columns)")
})

cli_h1("21c1 complete -- data/fp_train_<pos>_lagusage.rds written for RB/WR/TE")
