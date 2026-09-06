# R/15a0_ol_front_tables.R
# Ablation ladder rung 4, step 0: build the OL / opponent-front team-week
# tables the 15a diagnostic will bucket on. NO residuals are touched here --
# this is data prep only; the proceed rule is locked in 15a's header.
#
# TABLES (all keyed season x week x team; saved together in one rds):
#   ol_pass_block : ex-ante sack-rate + QB-hit-rate ALLOWED (own OL, offense)
#   ol_run_block  : ex-ante stuff-rate ALLOWED (rush yards <= 0 share)
#   front_def     : ex-ante sack-rate + stuff-rate GENERATED (defense)
#   ol_continuity : OBSERVED starting-5 overlap vs prior week (snap counts).
#                   Observed, not ex-ante -- diagnostic sizing only, exactly
#                   like 11a sized observed injury states before 11b built
#                   the masked ex-ante version. 15b owns the Friday-lock
#                   reconstruction if the rung triggers.
#   ftn_front     : ex-ante heavy-box rate (rush, n_defense_box >= 8) +
#                   blitz rate (dropback, n_blitzers >= 1), 2022+ only
#                   (FTN charting floor -- weather-archive precedent: a
#                   short-era axis is a diagnostic axis, not feature surgery).
#
# EX-ANTE CONVENTION for the rate tables: value at week W = season-to-date
# rate over weeks < W; below 100 relevant plays, fall back to the prior
# season's full-season rate (NA if neither exists). Week-1 rows therefore
# carry the prior-season rate -- reconstructable at any Tuesday build.
#
# VALIDITY GATES (abort on failure):
#   - rate tables cover >= 95% of 2014-2025 REG team-weeks
#   - continuity table covers >= 95% (5+ OL rows in snap counts both weeks)
#   - FTN join to pbp resolves >= 95% of 2022-2025 charted plays
# Receipt: output/15a0_coverage.csv

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(cli)
})

SEASONS      <- 2014L:2025L
PRIOR_FLOOR  <- 2013L          # prior-season fallback for 2014
FTN_SEASONS  <- 2022L:2025L
MIN_PLAYS    <- 100L           # below this, prior-season fallback
HEAVY_BOX    <- 8L

cli_h1("15a0: OL / opponent-front team-week tables")

# ===========================================================================
# 1. PBP-DERIVED PLAY TABLE (2013-2025, REG)
# ===========================================================================
pbp <- nflreadr::load_pbp(c(PRIOR_FLOOR, SEASONS)) |>
  filter(season_type == "REG", !is.na(posteam)) |>
  transmute(
    season, week, game_id, play_id,
    posteam, defteam,
    is_dropback = qb_dropback == 1L,
    is_sack     = sack == 1L,
    is_qb_hit   = qb_hit == 1L,
    is_rush     = rush_attempt == 1L & qb_scramble != 1L,
    is_stuff    = rush_attempt == 1L & qb_scramble != 1L & yards_gained <= 0
  )
cli_alert_info("pbp plays loaded: {nrow(pbp)} ({PRIOR_FLOOR}-{max(SEASONS)})")

# Per-team-week raw counts, offense (allowed) and defense (generated)
count_side <- function(df, team_col) {
  df |>
    group_by(season, week, team = .data[[team_col]]) |>
    summarise(
      dropbacks = sum(is_dropback, na.rm = TRUE),
      sacks     = sum(is_sack, na.rm = TRUE),
      qb_hits   = sum(is_qb_hit, na.rm = TRUE),
      rushes    = sum(is_rush, na.rm = TRUE),
      stuffs    = sum(is_stuff, na.rm = TRUE),
      .groups = "drop"
    )
}
off_counts <- count_side(pbp, "posteam")
def_counts <- count_side(pbp, "defteam")

# Ex-ante rate: season-to-date over weeks < W, prior-season full fallback
exante_rates <- function(counts) {
  prior <- counts |>
    group_by(season, team) |>
    summarise(across(c(dropbacks, sacks, qb_hits, rushes, stuffs), sum),
              .groups = "drop") |>
    transmute(season = season + 1L, team,
              ps_sack_rate  = sacks / pmax(dropbacks, 1),
              ps_hit_rate   = qb_hits / pmax(dropbacks, 1),
              ps_stuff_rate = stuffs / pmax(rushes, 1))
  counts |>
    filter(season %in% SEASONS) |>
    arrange(season, team, week) |>
    group_by(season, team) |>
    mutate(
      cum_db    = lag(cumsum(dropbacks), default = 0),
      cum_sk    = lag(cumsum(sacks),     default = 0),
      cum_hit   = lag(cumsum(qb_hits),   default = 0),
      cum_ru    = lag(cumsum(rushes),    default = 0),
      cum_st    = lag(cumsum(stuffs),    default = 0)
    ) |>
    ungroup() |>
    left_join(prior, by = c("season", "team")) |>
    transmute(
      season, week, team,
      sack_rate  = if_else(cum_db >= MIN_PLAYS, cum_sk / cum_db,  ps_sack_rate),
      hit_rate   = if_else(cum_db >= MIN_PLAYS, cum_hit / cum_db, ps_hit_rate),
      stuff_rate = if_else(cum_ru >= MIN_PLAYS, cum_st / cum_ru,  ps_stuff_rate)
    )
}
ol_rates    <- exante_rates(off_counts)   # allowed (own OL)
front_rates <- exante_rates(def_counts)   # generated (opponent front)

# ===========================================================================
# 2. OL CONTINUITY (snap counts; OBSERVED, diagnostic sizing only)
# ===========================================================================
cli_h1("15a0 step 2: OL continuity from snap counts")

# Label drift by era: most seasons use C/G/T, but 2020 and 2025 carry
# generic OL plus OT/OG (and guard LT/RT/LG/RG variants). Identity of the
# five comes from snap share; labels only gate who counts as a lineman.
OL_LABELS <- c("C", "G", "T", "OL", "OT", "OG", "LT", "RT", "LG", "RG")
snaps_ol <- nflreadr::load_snap_counts(c(PRIOR_FLOOR, SEASONS)) |>
  filter(game_type == "REG", position %in% OL_LABELS) |>
  group_by(season, week, team) |>
  slice_max(offense_pct, n = 5, with_ties = FALSE) |>
  summarise(top5 = list(sort(pfr_player_id)), n_ol = n(), .groups = "drop")

ol_continuity <- snaps_ol |>
  arrange(team, season, week) |>
  group_by(team) |>
  mutate(
    prev_top5       = lag(top5),
    prev_season     = lag(season),
    overlap         = map2_int(top5, prev_top5,
                               ~ if (is.null(.y)) NA_integer_
                                 else length(intersect(.x, .y))),
    w1_cross_season = season != prev_season
  ) |>
  ungroup() |>
  filter(season %in% SEASONS, n_ol == 5, !is.na(overlap)) |>
  transmute(season, week, team,
            ol_overlap = overlap,
            ol_new_starters = 5L - overlap,
            w1_cross_season)
cli_alert_info("continuity rows: {nrow(ol_continuity)} | broken-line (overlap<=3) share: {round(100 * mean(ol_continuity$ol_overlap <= 3), 1)}%")

# ===========================================================================
# 3. FTN FRONT TENDENCIES (2022+; ex-ante)
# ===========================================================================
cli_h1("15a0 step 3: FTN box / blitz tendencies")

ftn <- nflreadr::load_ftn_charting(FTN_SEASONS) |>
  select(nflverse_game_id, nflverse_play_id, n_defense_box, n_blitzers)

ftn_joined <- pbp |>
  filter(season %in% FTN_SEASONS) |>
  inner_join(ftn, by = c("game_id" = "nflverse_game_id",
                         "play_id" = "nflverse_play_id"))
# Join-rate denominator: FTN plays in REG-numbered games only -- FTN charts
# playoffs (~4.6%) which the REG-scrimmage pbp side excludes by design.
ftn_reg_n <- sum(grepl("_(0[1-9]|1[0-8])_", ftn$nflverse_game_id))
ftn_join_rate <- nrow(ftn_joined) / ftn_reg_n
cli_alert_info("FTN join: {nrow(ftn_joined)} of {ftn_reg_n} REG charted plays ({round(100 * ftn_join_rate, 1)}%)")

ftn_counts <- ftn_joined |>
  group_by(season, week, team = defteam) |>
  summarise(
    ch_rushes    = sum(is_rush & !is.na(n_defense_box), na.rm = TRUE),
    heavy_box    = sum(is_rush & n_defense_box >= HEAVY_BOX, na.rm = TRUE),
    ch_dropbacks = sum(is_dropback & !is.na(n_blitzers), na.rm = TRUE),
    blitzes      = sum(is_dropback & n_blitzers >= 1, na.rm = TRUE),
    .groups = "drop"
  )

ftn_front <- ftn_counts |>
  arrange(season, team, week) |>
  group_by(season, team) |>
  mutate(cum_ru = lag(cumsum(ch_rushes), default = 0),
         cum_hb = lag(cumsum(heavy_box), default = 0),
         cum_db = lag(cumsum(ch_dropbacks), default = 0),
         cum_bl = lag(cumsum(blitzes), default = 0)) |>
  ungroup() |>
  left_join(
    ftn_counts |>
      group_by(season, team) |>
      summarise(across(c(ch_rushes, heavy_box, ch_dropbacks, blitzes), sum),
                .groups = "drop") |>
      transmute(season = season + 1L, team,
                ps_heavy_box_rate = heavy_box / pmax(ch_rushes, 1),
                ps_blitz_rate     = blitzes / pmax(ch_dropbacks, 1)),
    by = c("season", "team")
  ) |>
  transmute(
    season, week, team,
    # Fallback chain: season-to-date if seasoned; else prior season; else
    # current partial (2022 early weeks -- FTN has no 2021, so a noisy
    # partial beats a structural NA; ex-ante either way).
    heavy_box_rate = case_when(
      cum_ru >= MIN_PLAYS        ~ cum_hb / cum_ru,
      !is.na(ps_heavy_box_rate)  ~ ps_heavy_box_rate,
      cum_ru >= 1                ~ cum_hb / cum_ru,
      TRUE                       ~ NA_real_
    ),
    blitz_rate = case_when(
      cum_db >= MIN_PLAYS    ~ cum_bl / cum_db,
      !is.na(ps_blitz_rate)  ~ ps_blitz_rate,
      cum_db >= 1            ~ cum_bl / cum_db,
      TRUE                   ~ NA_real_
    )
  )

# ===========================================================================
# 4. GATES + SAVE
# ===========================================================================
cli_h1("15a0 step 4: gates + save")

expected_tw <- nflreadr::load_schedules(SEASONS) |>
  filter(game_type == "REG") |>
  (\(d) bind_rows(d |> transmute(season, week, team = home_team),
                  d |> transmute(season, week, team = away_team)))() |>
  distinct()

cov <- tibble(
  table = c("ol_rates", "front_rates", "ol_continuity", "ftn_front"),
  coverage = c(
    nrow(semi_join(expected_tw, ol_rates |> filter(!is.na(sack_rate)),
                   by = c("season", "week", "team"))) / nrow(expected_tw),
    nrow(semi_join(expected_tw, front_rates |> filter(!is.na(sack_rate)),
                   by = c("season", "week", "team"))) / nrow(expected_tw),
    nrow(semi_join(expected_tw, ol_continuity, by = c("season", "week", "team"))) / nrow(expected_tw),
    nrow(semi_join(expected_tw |> filter(season %in% FTN_SEASONS),
                   ftn_front |> filter(!is.na(heavy_box_rate)),
                   by = c("season", "week", "team"))) /
      nrow(expected_tw |> filter(season %in% FTN_SEASONS))
  )
)
print(as.data.frame(cov), row.names = FALSE)
readr::write_csv(cov |> mutate(ftn_join_rate = ftn_join_rate), "output/15a0_coverage.csv")

if (any(cov$coverage < 0.95)) cli_abort("COVERAGE GATE FAILED: a table is below 95%")
if (ftn_join_rate < 0.95) cli_abort("FTN JOIN GATE FAILED: below 95%")

saveRDS(list(ol_rates = ol_rates, front_rates = front_rates,
             ol_continuity = ol_continuity, ftn_front = ftn_front),
        "data/15a0_ol_front_tables.rds")
cli_alert_success("data/15a0_ol_front_tables.rds saved")
cli_h1("15a0 complete")
