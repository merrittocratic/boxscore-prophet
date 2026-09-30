# R/21c0_usage_sequence_build.R
# Stage B, step 2 of the D29 single-stage rebuild: build the per-player-week
# usage table the usage-trajectory autoencoder (Stage D, R/21g onward) will
# window into 6-week sequences. This script only builds the FLAT per-week
# table -- windowing into (N, T=6, C) tensors, masking, and per-fold/
# per-season standardization are R/21g's job, not this one.
#
# WHY data/{rb,wr,te}_outcomes.rds, NOT the feature tables: outcomes.rds
# carries every played week (30,910 WR rows), while the feature tables
# apply MIN_OPPORTUNITIES and drop 29-52% of them. The AE's whole point is
# to characterize a player's role INCLUDING the low-usage weeks that
# precede or follow a role change -- exactly the population the floor
# removes. Using outcomes.rds here is deliberate, not an oversight.
#
# CHANNELS BUILT (position-relevant subset selected later, in R/21g):
#   from outcomes.rds        -- target_share_obs, carry_share_obs (RB only),
#                                air_yards_share_obs / air_yards_per_target_obs
#                                (WR/TE only), opportunities,
#                                team_total_plays_obs
#   snap_share                -- nflreadr::load_snap_counts(), crosswalked
#                                pfr_player_id -> gsis_id via load_rosters()
#                                (same pattern as R/10b2_player_slate.R:335-365)
#   rz_carries / rz_targets   -- NEW derivation from load_pbp(), plays with
#                                yardline_100 <= 20 (redzone looks don't
#                                exist anywhere else in this repo yet)
#   rz_carry_share /
#   rz_target_share            -- rz_carries or rz_targets divided by the
#                                team's total redzone rush/pass plays that
#                                week (0 when the team had none)
#   routes_proxy               -- snap_share * team_pass_plays. Routes
#                                themselves are NOT reconstructable
#                                pre-2023 (load_participation()'s coverage
#                                fields are ~0% populated in 2016, ~38% in
#                                2019-2022 -- verified 2026-09-06 while
#                                scoping the deferred defensive-archetype
#                                arm); this proxy has full 2014+ coverage.
#
# Usage: Rscript R/21c0_usage_sequence_build.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(cli)
})

SEASONS <- 2014:nflreadr::most_recent_season()

cli_h1("21c0: usage sequence raw material -- seasons {min(SEASONS)}-{max(SEASONS)}")

# ---------------------------------------------------------------------------
# Redzone looks + team pass-play counts, both from play-by-play (one pull,
# reused for every position).
# ---------------------------------------------------------------------------
cli_h2("Pulling play-by-play for redzone looks + team pass volume")
pbp <- load_pbp(SEASONS) |>
  filter(!is.na(posteam), yardline_100 <= 20 | pass_attempt == 1)

rz <- pbp |> filter(yardline_100 <= 20)

rz_rush <- rz |> filter(rush_attempt == 1, !is.na(rusher_player_id)) |>
  count(player_id = rusher_player_id, posteam, season, week, name = "rz_carries")

rz_pass <- rz |> filter(pass_attempt == 1, !is.na(receiver_player_id)) |>
  count(player_id = receiver_player_id, posteam, season, week, name = "rz_targets")

team_rz_rush <- rz |> filter(rush_attempt == 1) |>
  count(posteam, season, week, name = "team_rz_rush")
team_rz_pass <- rz |> filter(pass_attempt == 1) |>
  count(posteam, season, week, name = "team_rz_pass")

team_pass_plays <- pbp |> filter(pass_attempt == 1) |>
  count(posteam, season, week, name = "team_pass_plays")

cli_alert_success("Redzone: {nrow(rz_rush)} rusher-weeks, {nrow(rz_pass)} receiver-weeks with a redzone look")

# ---------------------------------------------------------------------------
# Snap share -- full-history pull, gsis_id crosswalk via rosters (same
# pattern as R/10b2_player_slate.R's id_xwalk, generalized across seasons
# since rosters/crosswalks can shift pfr_id<->gsis_id mapping year to year).
# ---------------------------------------------------------------------------
cli_h2("Snap share (nflreadr::load_snap_counts, full history)")
rosters_all <- load_rosters(SEASONS)
id_xwalk <- rosters_all |>
  filter(!is.na(gsis_id), !is.na(pfr_id)) |>
  arrange(desc(season)) |>
  distinct(pfr_id, .keep_all = TRUE) |>
  select(gsis_id, pfr_id)

# FETCHED PER-SEASON, not batched (2026-09-09, hardened proactively --
# same GitHub #1 pattern as R/build_rb_feature_layer.R: a batched multi-
# season nflreadr fetch does not gracefully skip one bad/not-yet-
# available season, and this call had no guard at all). Not wired into
# weekly_run.sh, so lower urgency than the production fixes, but the
# same landmine the moment someone reruns this script pre-Week-1.
REQ_SNAP_COLS <- c("pfr_player_id", "season", "week", "game_type", "offense_pct")
snaps_raw <- map(SEASONS, function(s) {
  d <- tryCatch(load_snap_counts(s), error = function(e) {
    cli_alert_warning("Snap counts unavailable for {s} ({conditionMessage(e)}) -- skipping (expected before that season's games are played)")
    NULL
  })
  if (!is.null(d) && !all(REQ_SNAP_COLS %in% names(d))) {
    cli_alert_warning("Snap counts for {s} came back missing required columns ({paste(setdiff(REQ_SNAP_COLS, names(d)), collapse=', ')}) -- skipping (expected before that season's games are played)")
    d <- NULL
  }
  d
}) |> compact() |> list_rbind() |>
  filter(game_type == "REG", !is.na(pfr_player_id), !is.na(offense_pct))
if (nrow(snaps_raw) == 0) {
  cli_abort("No snap count data fetched for ANY season in {paste(range(SEASONS), collapse='-')} -- this is a real failure (network/nflreadr issue), not the expected single-new-season gap")
}
snap_pct_divisor <- if (max(snaps_raw$offense_pct, na.rm = TRUE) > 1.5) 100 else 1

snap_share <- snaps_raw |>
  mutate(snap_share = offense_pct / snap_pct_divisor) |>
  left_join(id_xwalk, by = c("pfr_player_id" = "pfr_id")) |>
  filter(!is.na(gsis_id)) |>
  distinct(gsis_id, season, week, .keep_all = TRUE) |>
  select(player_id = gsis_id, season, week, snap_share)

cli_alert_success("Snap share matched: {nrow(snap_share)} player-weeks")

# ---------------------------------------------------------------------------
# Per-position assembly: outcomes.rds (every played week, not floor-filtered)
# left-joined to redzone looks, team redzone volume, snap share, and the
# routes proxy.
# ---------------------------------------------------------------------------
build_usage <- function(position, outcomes_path, extra_share_cols) {
  base <- readRDS(outcomes_path)

  out <- base |>
    left_join(rz_rush, by = c("player_id", "posteam", "season", "week")) |>
    left_join(rz_pass, by = c("player_id", "posteam", "season", "week")) |>
    left_join(team_rz_rush, by = c("posteam", "season", "week")) |>
    left_join(team_rz_pass, by = c("posteam", "season", "week")) |>
    left_join(team_pass_plays, by = c("posteam", "season", "week")) |>
    left_join(snap_share, by = c("player_id", "season", "week")) |>
    mutate(
      rz_carries       = coalesce(rz_carries, 0L),
      rz_targets       = coalesce(rz_targets, 0L),
      team_rz_rush     = coalesce(team_rz_rush, 0L),
      team_rz_pass     = coalesce(team_rz_pass, 0L),
      team_pass_plays  = coalesce(team_pass_plays, 0L),
      rz_carry_share   = if_else(team_rz_rush > 0, rz_carries / team_rz_rush, 0),
      rz_target_share  = if_else(team_rz_pass > 0, rz_targets / team_rz_pass, 0),
      routes_proxy     = snap_share * team_pass_plays
    ) |>
    select(player_id, posteam, defteam, game_id, season, week,
           all_of(extra_share_cols), opportunities, team_total_plays_obs,
           snap_share, rz_carries, rz_targets, rz_carry_share, rz_target_share,
           routes_proxy)

  cli_alert_success("{position}: {nrow(out)} player-weeks, snap_share coverage {round(100*mean(!is.na(out$snap_share)),1)}%")
  out
}

rb <- build_usage("RB", "data/rb_outcomes.rds",
                  c("carry_share_obs", "target_share_obs"))
wr <- build_usage("WR", "data/wr_outcomes.rds",
                  c("target_share_obs", "air_yards_share_obs", "air_yards_per_target_obs"))
te <- build_usage("TE", "data/te_outcomes.rds",
                  c("target_share_obs", "air_yards_share_obs", "air_yards_per_target_obs"))

saveRDS(rb, "data/usage_seq_rb.rds")
saveRDS(wr, "data/usage_seq_wr.rds")
saveRDS(te, "data/usage_seq_te.rds")

cli_h1("21c0 complete -- data/usage_seq_<pos>.rds written for RB/WR/TE")
