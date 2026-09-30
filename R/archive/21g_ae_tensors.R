# R/21g_ae_tensors.R
# Stage D, increment 0 of the D29 single-stage rebuild: turn a position's
# per-week usage table (R/21c0's output) into the (N, T=6, C) float32
# tensors the autoencoder (increments 1+) will actually train on. This
# script does ONLY the tensor construction + standardization -- no model
# code lives here.
#
# POSITION + ENRICHMENT PARAMETERIZATION (added for the PFF-enrichment
# build, see ~/.claude/plans/dapper-sleeping-lollipop.md): `Rscript
# R/21g_ae_tensors.R` with NO arguments/env vars reproduces the ORIGINAL
# RB usage-only run byte-for-byte (POSITION defaults to RB, ENRICH_PFF
# defaults off) -- the already-shipped Increment 0 baseline is never
# silently changed by this generalization. `Rscript R/21g_ae_tensors.R WR`
# builds WR's tensor; `AE_ENRICH_PFF=1 Rscript R/21g_ae_tensors.R RB` adds
# the PFF charting channels (R/21c0a's output) on top of the usage-only
# six, writing to a differently-named output file so the base artifact is
# never overwritten.
#
# THIS FILE IS HEAVILY COMMENTED ON PURPOSE (an exception to this repo's
# normal no-comments style) -- it's Steve's first autoencoder build and the
# whole point is that the code itself is what gets re-read later to learn
# from, not just an in-session chat explanation. See
# feedback_autoencoder_teaching_pace memory.
#
# TENSOR SHAPE: for every played RB week (a "target row"), the AE's input is
# that player's PRECEDING 6 weeks of usage -- never the target week itself,
# that's the leakage boundary the whole rebuild is built around. T=6 slots
# are ordered chronologically, oldest first: slot 1 = target_week - 6, slot
# 6 = target_week - 1. C=7 = 6 usage channels + 1 mask channel.
#
# WHY THESE SIX CHANNELS (RB): snap_share (overall role size), carry_share_obs
# and target_share_obs (how that role splits between run/pass usage),
# rz_carry_share and rz_target_share (goal-line/scoring-chance trust, which
# moves independently of overall share), routes_proxy (pass-game routes run,
# a proxy for true route data which isn't reconstructable pre-2023 -- see
# R/21c0's header). This is a smaller set than raw opportunities/team_plays
# because those are context-scaled (a 12-touch game means something
# different behind a fast vs slow offense) -- the share-based channels are
# already normalized and comparable across teams/eras.
#
# WHY A MASK CHANNEL, NOT JUST ZERO-FILLING: byes, DNPs, and pre-debut weeks
# are MISSING ROWS in usage_seq_rb.rds, not NA rows (verified directly --
# e.g. a durable 2018 RB has rows for weeks 1-3,5-17; week 4 is just absent).
# A raw 0 in snap_share is ambiguous -- did he play and get zero snaps, or
# did the week not happen for him at all? The mask channel (1 = real played
# week, 0 = padding) resolves that ambiguity for the network instead of
# forcing it to guess from the value alone.
#
# STANDARDIZATION: per-season refit, the plan's leakage-safe default -- for
# every row in season S, channel means/sds come from ONLY real (non-padded)
# usage_seq_rb rows in seasons < S, never from S itself or later. This means
# a fold that has never seen season S's actual outcomes also never saw its
# usage stats when it standardized S's rows. EXCEPTION: 2014 is the first
# season in this dataset -- there are no "seasons < S" to fit on, so 2014
# rows are standardized on 2014's OWN real values instead (a documented,
# one-season-only leak, not silently swept under the rug).
#
# Usage: Rscript R/21g_ae_tensors.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(cli)
})

T_WINDOW <- 6L  # trailing weeks per tensor

args     <- commandArgs(trailingOnly = TRUE)
POSITION <- if (length(args) >= 1) toupper(args[1]) else "RB"
stopifnot(POSITION %in% c("RB", "WR"))
ENRICH_PFF <- Sys.getenv("AE_ENRICH_PFF", unset = "0") == "1"

# The 6 usage channels, per position, in a fixed column order -- this order
# is the tensor's channel order for the usage-channel slots; mask is
# appended last regardless of how many channels precede it. WR's list
# matches R/21c0's original design (same channels TE would also use).
CHANNELS_MAP <- list(
  RB = c("snap_share", "carry_share_obs", "target_share_obs",
         "rz_carry_share", "rz_target_share", "routes_proxy"),
  WR = c("snap_share", "target_share_obs", "air_yards_share_obs",
         "air_yards_per_target_obs", "rz_target_share", "routes_proxy")
)
# PFF charting channels (R/21j/21c0a), added only when ENRICH_PFF=1 --
# charting facts only, never grades_*/elusive_rating (Steve's scoping rule).
PFF_CHANNELS_MAP <- list(
  RB = c("yco_attempt", "avoided_tackle_rate", "breakaway_percent",
         "zone_attempt_share", "gap_attempt_share"),
  WR = c("contested_catch_rate", "avoided_tackle_rate", "drop_rate",
         "route_rate", "avg_depth_of_target", "yac_per_reception")
)

CHANNELS <- CHANNELS_MAP[[POSITION]]
if (ENRICH_PFF) CHANNELS <- c(CHANNELS, PFF_CHANNELS_MAP[[POSITION]])

usage_path <- sprintf("data/usage_seq_%s%s.rds", tolower(POSITION), if (ENRICH_PFF) "_pff" else "")
out_path   <- sprintf("data/ae_tensors_%s%s.rds", tolower(POSITION), if (ENRICH_PFF) "_pff" else "")

cli_h1("21g: {POSITION} usage tensors{if (ENRICH_PFF) ' (PFF-enriched)' else ''} -- (N, T={T_WINDOW}, C={length(CHANNELS) + 1})")

usage <- readRDS(usage_path) |>
  filter(!is.na(player_id))
cli_alert_info("Loaded {nrow(usage)} {POSITION} player-weeks from {usage_path} (NA player_id rows dropped)")

# ---------------------------------------------------------------------------
# Fast lookup: player_id + season + week -> that week's 6 channel values.
# Built as a plain R environment keyed by a pasted string, so a lookup for a
# week that never happened (bye/DNP/pre-debut, OR a week before the season
# started, e.g. week 0 or -2) just returns "not found" -- there is no
# separate season-boundary check needed, because a key like "player_2018_0"
# was never inserted in the first place. That's what makes zero-filling
# byes AND respecting the season boundary the same mechanism.
# ---------------------------------------------------------------------------
mk_key <- function(player_id, season, week) paste(player_id, season, week, sep = "_")

lookup <- new.env(parent = emptyenv())
usage_keys <- mk_key(usage$player_id, usage$season, usage$week)
usage_chan <- as.matrix(usage[, CHANNELS])
for (i in seq_len(nrow(usage))) {
  assign(usage_keys[i], usage_chan[i, ], envir = lookup)
}
cli_alert_success("Lookup built: {length(ls(lookup))} player-week keys")

# ---------------------------------------------------------------------------
# For one target row, pull its preceding T_WINDOW weeks. Returns a
# T_WINDOW x length(CHANNELS) matrix of raw (unstandardized) values, plus a
# length-T_WINDOW mask vector (1 = real week found, 0 = padding).
# ---------------------------------------------------------------------------
get_window <- function(player_id, season, week) {
  vals <- matrix(0, nrow = T_WINDOW, ncol = length(CHANNELS))
  mask <- numeric(T_WINDOW)
  for (t in seq_len(T_WINDOW)) {
    lag <- T_WINDOW - t + 1L   # slot 1 = lag 6 (oldest), slot T_WINDOW = lag 1 (most recent)
    key <- mk_key(player_id, season, week - lag)
    hit <- get0(key, envir = lookup, inherits = FALSE)
    if (!is.null(hit)) { vals[t, ] <- hit; mask[t] <- 1 }
    # else: leave the pre-allocated 0 in place -- this IS the padding convention
  }
  list(vals = vals, mask = mask)
}

# ---------------------------------------------------------------------------
# Build every row's raw window first (before standardizing), so standard-
# ization stats and tensor construction stay cleanly separate steps.
# ---------------------------------------------------------------------------
N <- nrow(usage)
raw_vals <- array(0, dim = c(N, T_WINDOW, length(CHANNELS)))
raw_mask <- matrix(0, nrow = N, ncol = T_WINDOW)

cli_alert_info("Building {N} raw windows...")
for (i in seq_len(N)) {
  w <- get_window(usage$player_id[i], usage$season[i], usage$week[i])
  raw_vals[i, , ] <- w$vals
  raw_mask[i, ]   <- w$mask
}
cli_alert_success("Raw windows built. Mean real (non-padded) weeks per window: {round(mean(rowSums(raw_mask)), 2)} / {T_WINDOW}")

# ---------------------------------------------------------------------------
# Per-season standardization stats: for every row whose target season is S,
# each channel's mean/sd comes from REAL usage_seq_rb values in seasons < S
# only. 2014 (first season in the data) falls back to its own real values --
# the one documented exception noted in the header above.
# ---------------------------------------------------------------------------
seasons <- sort(unique(usage$season))
chan_stats <- map(seasons, function(S) {
  hist_rows <- if (S == min(seasons)) usage$season == S else usage$season < S
  hist <- usage[hist_rows, CHANNELS]
  tibble(season = S, channel = CHANNELS,
         mean = map_dbl(CHANNELS, ~ mean(hist[[.x]], na.rm = TRUE)),
         sd   = map_dbl(CHANNELS, ~ sd(hist[[.x]],   na.rm = TRUE)))
}) |> bind_rows()

cli_alert_success("Standardization stats fit per season ({length(seasons)} seasons; 2014 uses its own in-season values, documented exception)")

# Standardized tensor: same shape as raw_vals, but every REAL (mask=1) cell
# is z-scored using that row's OWN season's stats; padded (mask=0) cells stay
# exactly 0 -- standardizing a fake value would give it a spurious non-zero
# "meaning" the network would have to learn to ignore twice (once via value,
# once via mask) instead of once.
std_vals <- raw_vals
for (S in seasons) {
  rows <- which(usage$season == S)
  if (length(rows) == 0) next
  for (c in seq_along(CHANNELS)) {
    st <- chan_stats |> filter(season == S, channel == CHANNELS[c])
    mu <- st$mean
    sg <- if (!is.na(st$sd) && st$sd > 0) st$sd else 1  # guard a degenerate/NA zero-variance season
    for (i in rows) {
      real_t <- which(raw_mask[i, ] == 1)
      std_vals[i, real_t, c] <- (raw_vals[i, real_t, c] - mu) / sg
    }
  }
}

# A handful of REAL weeks are still NA on specific channels (mostly
# snap_share, whose gsis_id<->pfr_id crosswalk coverage is 54%-93% depending
# on era -- see R/21c0's header). The single mask channel can't distinguish
# "week didn't happen" from "week happened but this one measurement is
# missing" -- both get the same treatment (0), reported explicitly rather
# than silently produced, per this repo's "nulls get receipts" rule.
n_na <- sum(is.na(std_vals))
std_vals[is.na(std_vals)] <- 0
cli_alert_info("{n_na} real-week channel cells were NA (crosswalk gaps, not byes) -- zero-filled, same as padding")

# Clip standardized values to [-5, 5] -- a defensive step against small-
# sample rate stats (e.g. a PFF contested-catch-rate week built on a single
# contested target reads as a literal 0% or 100%, which standardizes to an
# extreme z-score against a historical population that's mostly near 0).
# Found empirically: WR's contested_catch_rate hit a standardized max of
# 100 (every other channel sits within +/-5), which alone was enough to
# make gate (a)'s reconstruction check uninterpretable -- one outlier row
# dominates both the model's loss AND the "predict 0" baseline's loss.
# Applied uniformly to every channel, not special-cased to this one field,
# since it's a general defense against any channel's small-sample noise.
n_clipped <- sum(abs(std_vals) > 5, na.rm = TRUE)
std_vals <- pmin(pmax(std_vals, -5), 5)
if (n_clipped > 0) cli_alert_info("{n_clipped} real-week channel cells clipped to +/-5 (extreme small-sample outliers)")

# ---------------------------------------------------------------------------
# Final (N, T, C=7) tensor: append mask as the 7th channel.
# ---------------------------------------------------------------------------
tensor <- array(0, dim = c(N, T_WINDOW, length(CHANNELS) + 1L))
tensor[, , seq_along(CHANNELS)]    <- std_vals
tensor[, , length(CHANNELS) + 1L]  <- raw_mask
storage.mode(tensor) <- "double"  # increment 1 casts this to float32 when it enters torch

meta <- usage |> select(player_id, season, week) |> mutate(row = row_number())

cli_h1("Sanity check -- one player's window (payoff: read a 6x7 matrix, see his usage)")
# Pick a durable veteran back's mid-career target week so the printed window
# has a full 6 real weeks behind it, not padding.
sample_row <- meta |>
  semi_join(usage |> count(player_id) |> filter(n >= 32), by = "player_id") |>
  filter(week == 10, season == 2019) |>
  slice(1)
if (nrow(sample_row) == 1) {
  r <- sample_row$row
  cli_alert_info("player_id={sample_row$player_id}, target season={sample_row$season}, week={sample_row$week} (window = weeks {sample_row$week - T_WINDOW}-{sample_row$week - 1})")
  disp <- as.data.frame(tensor[r, , ])
  colnames(disp) <- c(CHANNELS, "mask")
  rownames(disp) <- paste0("t-", T_WINDOW:1)
  print(round(disp, 3))
} else {
  cli_alert_warning("No row matched the sample criteria -- skipping printed sanity check (tensor still saved)")
}

dir.create("data", showWarnings = FALSE)
saveRDS(list(tensor = tensor, meta = meta, channels = CHANNELS, t_window = T_WINDOW),
        out_path)
cli_alert_success("{out_path} written: tensor dim {paste(dim(tensor), collapse = ' x ')}")
cli_h1("21g increment 0 complete")
