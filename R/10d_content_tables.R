# R/10d_content_tables.R
# Step 10d: Content products from the 10c scored slate.
#
# Products (Steve's 2026-07-17 runner content decisions):
#   1. Flagship START board per position -- ranked by P(15+ PPR) for RB/WR,
#      P(12+ PPR) for TE (12_te rate-matched cuts), P(20+ standard) for QB.
#      CSV + markdown + X board image.
#   2. BOOM board -- flex (RB+WR+TE, Steve 2026-07-19) + QB P(25+). Bars
#      are position-calibrated (RB/WR 20+, TE 17+ -- equal rarity by
#      construction) and disclosed per row + footnote. The X content hook.
#   3. STREAMER/WAIVER board -- the exante_low volume stratum (RB pred_vol
#      < 10 touches, WR < 5 targets, TE < 4 targets -- the 06c/12e strata
#      cuts), ranked by start odds. The conditional-recal payoff population.
#   4. RECEIPTS -- when the target week has been played: stated pre-kickoff
#      probabilities vs outcomes, calibration by stated-odds band, biggest
#      hit/miss callouts. The trust engine; Monday post-mortem input.
#   5. ECR GAP -- model rank vs FantasyPros consensus rank, via file-drop
#      feed at data/ecr/ecr_<season>_w<week>.csv (player_name, position,
#      ecr_rank). Feed sourcing is an open decision; the join is built and
#      skips gracefully when the file is absent.
#
# EDITORIAL CAPS (pre-committed): DISPLAYED probabilities are clamped to
# [2%, 95%] in markdown/images -- the model never publishes a certainty.
# Raw uncapped values stay in the CSVs.
#
# Usage: Rscript R/10d_content_tables.R [season] [week]

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(cli)
})

source("R/10d_name_helpers.R")

args <- commandArgs(trailingOnly = TRUE)
TARGET_SEASON <- if (length(args) >= 1) as.integer(args[1]) else 2026L
TARGET_WEEK   <- if (length(args) >= 2) as.integer(args[2]) else 15L
WTAG <- sprintf("%d_w%02d", TARGET_SEASON, TARGET_WEEK)

DISPLAY_FLOOR <- 0.02
DISPLAY_CEIL  <- 0.95
BOARD_N       <- 12L    # rows on rendered X images
STREAMER_CUT  <- c(RB = 10, WR = 5, TE = 4)   # 06c/12e exante_low upper bounds
RECEIPT_BANDS <- c(0, 0.10, 0.25, 0.50, 1)
BAND_LABELS   <- c("under 10%", "10-25%", "25-50%", "50%+")

VOL_LABEL   <- c(RB = "proj touches", WR = "proj targets", TE = "proj targets",
                 QB = "proj dropbacks")
START_LABEL <- c(RB = "P(15+ PPR)", WR = "P(15+ PPR)", TE = "P(12+ PPR)",
                 QB = "P(20+ std)")
BOOM_LABEL  <- c(RB = "P(20+ PPR)", WR = "P(20+ PPR)", TE = "P(17+ PPR)",
                 QB = "P(25+ std)")
CONTENT_CHALK_ECR_CUT   <- c(RB = 6L, WR = 6L, TE = 4L)
CONTENT_DECISION_ECR_MAX <- c(RB = 24L, WR = 30L, TE = 18L)
CONTENT_DECISION_MODEL_MAX <- c(RB = 18L, WR = 24L, TE = 12L)
CONTENT_SWING_ECR_FLOOR <- c(RB = 12L, WR = 12L, TE = 8L)
CONTENT_MIN_START <- c(RB = 28L, WR = 28L, TE = 25L)
CONTENT_MAX_START <- c(RB = 60L, WR = 60L, TE = 60L)
CONTENT_MIN_BOOM  <- c(RB = 14L, WR = 14L, TE = 12L)
CONTENT_EDGE_GAP  <- c(RB = 4L, WR = 4L, TE = 3L)
CONTENT_FADE_GAP  <- c(RB = 4L, WR = 4L, TE = 3L)
CONTENT_FADE_MAX_START <- c(RB = 52L, WR = 52L, TE = 50L)
# Flex-tier receipts (Steve 2026-09-24): On the Record grades the
# decision-relevant tier, not auto-starts or deep-roster longshots. Bands
# match CONTENT_GUIDE.md's Decision-Relevant Tiers. Pool = ECR rank in
# band, OR model rank in band while ECR has him below the band / unranked.
# Consensus auto-starts (ECR above the band) are excluded even when the
# model is low on them -- nobody benches them, so it is not a decision.
FLEX_BAND_LO <- c(RB = 20L, WR = 20L, TE = 10L, QB = 10L)
FLEX_BAND_HI <- c(RB = 39L, WR = 39L, TE = 19L, QB = 19L)
FLEX_ECR_GAP <- c(RB = 4L, WR = 4L, TE = 3L, QB = 3L)   # "real disagreement"

# Validated board accents (dataviz palette check, light surface #fcfcfb)
ACCENT_START <- "#2F6DB3"
ACCENT_BOOM  <- "#C4622D"
SURFACE      <- "#fcfcfb"
INK          <- "#1F2937"
INK_MUTED    <- "#6B7280"

`%||%` <- function(a, b) if (is.null(a)) b else a

cap_pct <- function(p) round(100 * pmin(pmax(p, DISPLAY_FLOOR), DISPLAY_CEIL))

inj_tag <- function(report_status) {
  case_when(
    is.na(report_status)              ~ "",
    report_status == "Questionable"   ~ " (Q)",
    report_status == "Doubtful"       ~ " (D)",
    report_status == "Out"            ~ " (O)",
    .default                          = paste0(" (", substr(report_status, 1, 1), ")")
  )
}

# ===========================================================================
# 1. LOAD SCORED SLATE
# ===========================================================================

cli_h1("Step 10d: content products for {TARGET_SEASON} week {TARGET_WEEK}")

scored_path <- sprintf("output/10c_scored_slate_%s.csv", WTAG)
detail_path <- sprintf("output/10c_scored_detail_%s.csv", WTAG)
if (!file.exists(scored_path)) {
  cli_abort("Missing {scored_path} -- run 10c for this week first.")
}
scored <- readr::read_csv(scored_path, show_col_types = FALSE)

# QB display volume = predicted dropbacks (pred_vol in the slim slate is
# pred_carry, the recal-map axis -- not what a reader wants to see)
qb_db <- readr::read_csv(detail_path, show_col_types = FALSE) |>
  filter(position == "QB") |>
  transmute(position, player_id, disp_vol = round(as.numeric(pred_db)))

boards_base <- scored |>
  left_join(qb_db, by = c("position", "player_id")) |>
  mutate(
    disp_vol   = coalesce(disp_vol, round(pred_vol)),
    player_disp = paste0(player_name, inj_tag(report_status)),
    start_pct  = cap_pct(p_start_recal),
    boom_pct   = cap_pct(p_boom_recal)
  )

cli_alert_success("Scored slate: {nrow(boards_base)} players")

# ===========================================================================
# 2. FORWARD BOARDS
# ===========================================================================

start_board <- boards_base |>
  group_by(position) |>
  arrange(desc(p_start_recal), .by_group = TRUE) |>
  mutate(rank = row_number()) |>
  ungroup() |>
  select(position, rank, player_id, player_name, player_disp, posteam, defteam,
         start_pct, boom_pct, disp_vol, report_status,
         p_start_recal, p_boom_recal, pred_vol)

# TE included per Steve 2026-07-19. Bars differ by position (RB/WR 20+,
# TE 17+ -- position-calibrated, see D17); the board carries a per-row
# bar column and the footnote states it, so ranking across bars is
# disclosed rather than hidden.
boom_flex <- boards_base |>
  filter(position %in% c("RB", "WR", "TE")) |>
  mutate(boom_bar = thresh_boom) |>
  arrange(desc(p_boom_recal)) |>
  mutate(rank = row_number()) |>
  select(position, rank, player_id, player_name, player_disp, posteam, defteam,
         boom_pct, start_pct, disp_vol, boom_bar, p_boom_recal)

boom_qb <- start_board |>
  filter(position == "QB") |>
  arrange(desc(p_boom_recal)) |>
  mutate(rank = row_number())

streamer_board <- boards_base |>
  filter(position %in% c("RB", "WR", "TE"),
         pred_vol < STREAMER_CUT[position]) |>
  group_by(position) |>
  arrange(desc(p_start_recal), .by_group = TRUE) |>
  mutate(rank = row_number()) |>
  ungroup() |>
  select(position, rank, player_id, player_name, player_disp, posteam, defteam,
         start_pct, boom_pct, disp_vol, p_start_recal, pred_vol)

readr::write_csv(start_board,    sprintf("output/10d_start_board_%s.csv", WTAG))
readr::write_csv(bind_rows(boom_flex |> mutate(board = "flex"),
                           boom_qb |> mutate(board = "qb") |>
                             select(any_of(names(boom_flex)), board)),
                 sprintf("output/10d_boom_board_%s.csv", WTAG))
readr::write_csv(streamer_board, sprintf("output/10d_streamer_board_%s.csv", WTAG))

# ===========================================================================
# 3. ECR GAP (file-drop feed; skips gracefully)
# ===========================================================================

ecr_path <- sprintf("data/ecr/ecr_%s.csv", WTAG)
ecr_gap <- NULL
ecr_slim <- tibble(player_name_norm = character(), position = character(), ecr_rank = integer())
if (file.exists(ecr_path)) {
  ecr <- readr::read_csv(ecr_path, show_col_types = FALSE)
  if (!"player_name_norm" %in% names(ecr)) {   # manual drops lack the column
    ecr <- ecr |> mutate(player_name_norm = normalize_player_name(player_name))
  }
  ecr_slim <- ecr |>
    select(player_name_norm, position, ecr_rank) |>
    distinct(player_name_norm, position, .keep_all = TRUE)

  joined <- start_board |>
    mutate(player_name_norm = normalize_player_name(player_name)) |>
    left_join(ecr_slim, by = c("player_name_norm", "position"))

  # Join coverage: unmatched slate players inside ECR's ranked depth are
  # name mismatches (alias candidates for 10d_name_helpers.R); unmatched
  # beyond the depth are free-tier truncation, not name problems.
  ecr_depth <- ecr_slim |> count(position, name = "depth")
  unmatched <- joined |>
    filter(is.na(ecr_rank)) |>
    left_join(ecr_depth, by = "position") |>
    filter(rank <= depth)
  if (nrow(unmatched) > 0) {
    cli_alert_warning("ECR join: {nrow(unmatched)} in-depth slate players unmatched (alias candidates): {paste(head(unmatched$player_name, 8), collapse = ', ')}")
  }

  ecr_gap <- joined |>
    filter(!is.na(ecr_rank)) |>
    mutate(rank_gap = ecr_rank - rank) |>   # positive = model higher than market
    arrange(desc(abs(rank_gap))) |>
    select(position, player_name, posteam, model_rank = rank, ecr_rank,
           rank_gap, start_pct)
  readr::write_csv(ecr_gap, sprintf("output/10d_ecr_gap_%s.csv", WTAG))

  # ECR LOCK: 10c rescores only score not-yet-kicked-off games, so the gap
  # file above shrinks as the week plays out (by Monday it is MNF only).
  # Upsert every player on the current (all pre-kickoff) slate into a
  # per-week lock; players whose games already started keep the rank from
  # their last pre-kickoff run. Receipts grade against this lock.
  ecr_lock_path <- sprintf("output/10d_ecr_lock_%s.csv", WTAG)
  # Guard: only players whose game has NOT kicked off may be (re)locked --
  # the Tuesday prior-week receipts pass re-runs 10d on a finished week.
  ledger_ko_path <- sprintf("output/10c_ledger_%s.csv", WTAG)
  not_started <- if (file.exists(ledger_ko_path)) {
    readr::read_csv(ledger_ko_path, show_col_types = FALSE,
                    col_types = readr::cols(kickoff_et = readr::col_datetime(),
                                            player_id = readr::col_character(),
                                            .default = readr::col_guess())) |>
      group_by(player_id) |>
      slice_max(run_ts, n = 1, with_ties = FALSE) |>
      ungroup() |>
      filter(kickoff_et > Sys.time()) |>
      pull(player_id)
  } else character()
  ecr_now <- joined |>
    filter(!is.na(ecr_rank), player_id %in% not_started) |>
    transmute(position, player_id, player_name, posteam, ecr_rank,
              ecr_as_of = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
  ecr_lock <- if (file.exists(ecr_lock_path)) {
    readr::read_csv(ecr_lock_path, show_col_types = FALSE,
                    col_types = readr::cols(ecr_as_of = readr::col_character(),
                                            player_id = readr::col_character())) |>
      filter(!player_id %in% ecr_now$player_id) |>
      bind_rows(ecr_now)
  } else ecr_now
  readr::write_csv(ecr_lock |> arrange(position, ecr_rank), ecr_lock_path)
  cli_alert_success("ECR lock: {nrow(ecr_now)} upserted, {nrow(ecr_lock)} locked for the week")
  cli_alert_success("ECR gap: {nrow(ecr_gap)} matched | depth by pos: {paste(ecr_depth$position, ecr_depth$depth, collapse = ', ')}")
} else {
  cli_alert_info("No ECR feed at {ecr_path} -- gap table skipped (drop a CSV or run 10d0 once the API key is active)")
}

content_board <- start_board |>
  filter(position %in% c("RB", "WR", "TE")) |>
  mutate(player_name_norm = normalize_player_name(player_name)) |>
  left_join(ecr_slim, by = c("player_name_norm", "position")) |>
  mutate(
    rank_gap = ecr_rank - rank,
    consensus_chalk = !is.na(ecr_rank) & ecr_rank <= CONTENT_CHALK_ECR_CUT[position],
    decision_band = rank <= CONTENT_DECISION_MODEL_MAX[position] |
      (!is.na(ecr_rank) & ecr_rank <= CONTENT_DECISION_ECR_MAX[position]),
    real_volume = pred_vol >= STREAMER_CUT[position],
    leverage_call = !consensus_chalk & !is.na(ecr_rank) &
      ecr_rank <= CONTENT_DECISION_ECR_MAX[position] &
      start_pct >= CONTENT_MIN_START[position] &
      rank_gap >= CONTENT_EDGE_GAP[position],
    caution_call = !is.na(ecr_rank) & decision_band & real_volume &
      start_pct >= CONTENT_MIN_START[position] &
      start_pct <= CONTENT_FADE_MAX_START[position] &
      rank_gap <= -CONTENT_FADE_GAP[position],
    upside_swing = !consensus_chalk & real_volume & decision_band &
      (is.na(ecr_rank) | ecr_rank > CONTENT_SWING_ECR_FLOOR[position]) &
      start_pct >= CONTENT_MIN_START[position] &
      start_pct <= CONTENT_MAX_START[position] &
      boom_pct >= CONTENT_MIN_BOOM[position],
    content_tier = case_when(
      leverage_call ~ "leverage",
      caution_call  ~ "caution",
      upside_swing  ~ "swing",
      TRUE          ~ NA_character_
    ),
    content_score = p_boom_recal +
      0.01 * pmax(replace_na(rank_gap, 0L), 0L) +
      0.002 * pmax(0, 12 - abs(start_pct - 42))
  ) |>
  filter(!is.na(content_tier)) |>
  arrange(factor(content_tier, levels = c("leverage", "caution", "swing")),
          dplyr::if_else(content_tier == "caution", rank_gap, -replace_na(rank_gap, 0L)),
          desc(content_score), desc(replace_na(rank_gap, 0L)),
          desc(boom_pct), desc(start_pct)) |>
  group_by(content_tier) |>
  mutate(content_rank = row_number()) |>
  ungroup() |>
  select(content_tier, content_rank, position, player_id, player_name, player_disp,
         posteam, defteam, model_rank = rank, ecr_rank, rank_gap,
         start_pct, boom_pct, disp_vol, report_status,
         p_start_recal, p_boom_recal, pred_vol, content_score)

readr::write_csv(content_board, sprintf("output/10d_content_board_%s.csv", WTAG))
cli_alert_success("Board CSVs written (start / boom / content / streamer)")

# ===========================================================================
# 4. RECEIPTS (played weeks only)
# ===========================================================================

cli_h1("Receipts (stated odds vs outcomes, from the locked ledger)")

# The graded statement is the LAST pre-kickoff probability per player (the
# 10c ledger; every ledger row is pre-kickoff by construction). Games not
# yet resolved at AS_OF are published as PENDING (Monday-with-pending,
# Steve 2026-07-18); the Tuesday full run finalizes them.
as_of_env <- Sys.getenv("AS_OF", "")
RECEIPT_AS_OF <- if (nzchar(as_of_env)) {
  as.POSIXct(as_of_env, tz = "America/New_York")
} else {
  Sys.time()
}
RESOLVE_LAG_S <- 6 * 3600   # a game is gradeable this long after kickoff

ledger_path <- sprintf("output/10c_ledger_%s.csv", WTAG)

fp_obs <- tryCatch({
  nflreadr::load_player_stats(seasons = TARGET_SEASON) |>
    filter(season_type == "REG", week == TARGET_WEEK, !is.na(player_id)) |>
    select(player_id, fantasy_points_ppr, fantasy_points)
}, error = function(e) NULL)

receipts <- NULL
pending  <- NULL
if (file.exists(ledger_path)) {
  locked <- readr::read_csv(
      ledger_path, show_col_types = FALSE,
      # A header-only ledger (0 data rows) makes read_csv's type-inference
      # guess kickoff_et as character instead of datetime -- pin it so the
      # game_over arithmetic below never sees a non-POSIXct column.
      col_types = readr::cols(kickoff_et = readr::col_datetime())
    ) |>
    group_by(player_id) |>
    slice_max(run_ts, n = 1, with_ties = FALSE) |>
    ungroup() |>
    mutate(start_pct = cap_pct(p_start_recal),
           boom_pct  = cap_pct(p_boom_recal))

  graded <- locked |>
    left_join(fp_obs %||% tibble(player_id = character(),
                                 fantasy_points_ppr = numeric(),
                                 fantasy_points = numeric()),
              by = "player_id") |>
    mutate(
      fp_played  = if_else(position == "QB", fantasy_points, fantasy_points_ppr),
      # A game is only gradeable once it has KICKED OFF before AS_OF --
      # outcome rows must never resolve a game the clock says is future
      # (matters for replays/hindcast tests; production stats simply lag).
      started    = kickoff_et < RECEIPT_AS_OF,
      game_over  = kickoff_et + RESOLVE_LAG_S < RECEIPT_AS_OF,
      resolved   = started & (!is.na(fp_played) | game_over),
      dnp        = resolved & is.na(fp_played),
      fp_actual  = coalesce(fp_played, 0)   # resolved w/o stat line = inactive
    )

  # DNP rows (resolved, no stat line: inactives, late scratches, coach's-
  # decision healthy scratches) stay in the receipts CSV for transparency
  # but are NOT graded -- hit_* is NA and they are excluded from bands,
  # misses, and longshots. Steve's call 2026-09-24: a player who never
  # took the field is not a model miss (W2 had 161 of 524 resolved rows
  # DNP, mostly deep-roster backups, dragging every band's hit rate down).
  receipts <- graded |>
    filter(resolved) |>
    mutate(hit_start  = if_else(dnp, NA, fp_actual >= thresh_start),
           hit_boom   = if_else(dnp, NA, fp_actual >= thresh_boom),
           band_start = cut(p_start_recal, RECEIPT_BANDS, labels = BAND_LABELS,
                            include.lowest = TRUE)) |>
    select(position, player_id, player_name, posteam, defteam, kickoff_et,
           thresh_start, thresh_boom, p_start_recal, p_boom_recal,
           start_pct, boom_pct, fp_actual, dnp, hit_start, hit_boom, band_start)

  pending <- graded |>
    filter(!resolved) |>
    select(position, player_id, player_name, posteam, defteam, kickoff_et,
           start_pct, boom_pct)

  if (nrow(receipts) > 0) {
    receipt_bands <- receipts |>
      filter(!dnp) |>
      group_by(band_start) |>
      summarise(n = n(),
                stated = mean(p_start_recal),
                hit_rate = mean(hit_start), .groups = "drop") |>
      mutate(delta_pp = round(100 * (hit_rate - stated), 1))
    readr::write_csv(receipts, sprintf("output/10d_receipts_%s.csv", WTAG))
    readr::write_csv(receipt_bands, sprintf("output/10d_receipt_bands_%s.csv", WTAG))
    cli_alert_success("Receipts: {nrow(receipts)} graded ({sum(receipts$dnp)} DNP) | {nrow(pending)} pending | start hits: {sum(receipts$hit_start, na.rm = TRUE)} | booms: {sum(receipts$hit_boom, na.rm = TRUE)}")
    print(receipt_bands, n = Inf)
  } else {
    cli_alert_info("Nothing resolved yet at as-of {format(RECEIPT_AS_OF, '%Y-%m-%d %H:%M')} -- {nrow(pending)} statements pending")
    receipts <- NULL
  }

  # FLEX-TIER RECEIPTS. Model rank = rank of the locked (graded) start
  # chance within position across the WHOLE week's ledger, i.e. the board
  # as it stood at each player's lock. ECR rank from the week's ECR lock.
  flex <- NULL
  if (!is.null(receipts)) {
    model_ranks <- locked |>
      group_by(position) |>
      arrange(desc(p_start_recal), .by_group = TRUE) |>
      mutate(model_rank = row_number()) |>
      ungroup() |>
      select(player_id, model_rank)
    ecr_lock_path <- sprintf("output/10d_ecr_lock_%s.csv", WTAG)
    ecr_locked <- if (file.exists(ecr_lock_path)) {
      readr::read_csv(ecr_lock_path, show_col_types = FALSE,
                      col_types = readr::cols(player_id = readr::col_character())) |>
        select(player_id, ecr_rank)
    } else tibble(player_id = character(), ecr_rank = integer())
    if (nrow(ecr_locked) == 0) {
      cli_alert_warning("No ECR lock at {ecr_lock_path} -- flex receipts use model rank only (no consensus comparison)")
    }
    in_band <- function(r, pos) !is.na(r) & r >= FLEX_BAND_LO[pos] & r <= FLEX_BAND_HI[pos]
    flex <- receipts |>
      filter(!dnp) |>
      left_join(model_ranks, by = "player_id") |>
      left_join(ecr_locked, by = "player_id") |>
      filter(in_band(ecr_rank, position) |
               (in_band(model_rank, position) &
                  (is.na(ecr_rank) | ecr_rank > FLEX_BAND_HI[position]))) |>
      mutate(rank_gap = ecr_rank - model_rank,   # positive = model higher than ECR
             view = case_when(
               !is.na(rank_gap) & rank_gap >=  FLEX_ECR_GAP[position] ~ "model_higher",
               !is.na(rank_gap) & rank_gap <= -FLEX_ECR_GAP[position] ~ "model_lower",
               is.na(ecr_rank) ~ "no_ecr",
               TRUE ~ "agree")) |>
      arrange(position, model_rank) |>
      select(position, player_id, player_name, posteam, defteam, model_rank,
             ecr_rank, rank_gap, view, start_pct, boom_pct, fp_actual,
             thresh_start, hit_start, hit_boom)
    readr::write_csv(flex, sprintf("output/10d_flex_receipts_%s.csv", WTAG))
    cli_alert_success("Flex receipts: {nrow(flex)} flex-tier players graded | model higher than ECR: {sum(flex$view == 'model_higher')} | lower: {sum(flex$view == 'model_lower')}")
  }
} else {
  cli_alert_info("No ledger at {ledger_path} -- run 10c for this week first; receipts skipped")
}

# ===========================================================================
# 5. MARKDOWN BOARDS
# ===========================================================================

md_table <- function(df, headers) {
  rows <- apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  paste(c(paste0("| ", paste(headers, collapse = " | "), " |"),
          paste0("|", paste(rep("---", length(headers)), collapse = "|"), "|"),
          rows), collapse = "\n")
}

md <- c(sprintf("# BOXSCORE PROPHET -- %d Week %d boards", TARGET_SEASON, TARGET_WEEK), "")

for (pos in c("RB", "WR", "TE", "QB")) {
  b <- start_board |> filter(position == pos) |> slice_head(n = 20)
  md <- c(md,
    sprintf("## %s start board -- %s", pos, START_LABEL[pos]), "",
    md_table(b |> transmute(rank, player_disp, posteam, defteam,
                            start = paste0(start_pct, "%"),
                            boom = paste0(boom_pct, "%"), disp_vol),
             c("#", "Player", "Team", "Opp", "Start", "Boom", VOL_LABEL[pos])),
    "")
}

if (nrow(content_board) > 0) {
  leverage_md <- content_board |>
    filter(content_tier == "leverage") |>
    slice_head(n = 12)
  caution_md <- content_board |>
    filter(content_tier == "caution") |>
    slice_head(n = 12)
  swing_md <- content_board |>
    filter(content_tier == "swing") |>
    slice_head(n = 12)

  md <- c(md,
    "## Content board -- the hard middle", "",
    "These are the non-obvious starts worth writing about: players with real ceiling, real volume, or a stronger model view than consensus.", "")

  if (nrow(leverage_md) > 0) {
    md <- c(md,
      "### Leverage calls -- model materially higher than consensus", "",
      md_table(leverage_md |>
                 mutate(rank_gap = sprintf("%+d", rank_gap)) |>
                 transmute(position, content_rank, player_disp, posteam, defteam,
                           start = paste0(start_pct, "%"),
                           boom = paste0(boom_pct, "%"),
                           model = model_rank, ecr = ecr_rank,
                           gap = rank_gap, disp_vol),
               c("Pos", "#", "Player", "Team", "Opp", "Start", "Boom", "Model", "ECR", "Gap", "proj vol")),
      "")
  }

  if (nrow(caution_md) > 0) {
    md <- c(md,
      "### Caution calls -- consensus is richer than the model", "",
      md_table(caution_md |>
                 mutate(rank_gap = sprintf("%+d", rank_gap)) |>
                 transmute(position, content_rank, player_disp, posteam, defteam,
                           start = paste0(start_pct, "%"),
                           boom = paste0(boom_pct, "%"),
                           model = model_rank, ecr = ecr_rank,
                           gap = rank_gap, disp_vol),
               c("Pos", "#", "Player", "Team", "Opp", "Start", "Boom", "Model", "ECR", "Gap", "proj vol")),
      "")
  }

  if (nrow(swing_md) > 0) {
    md <- c(md,
      "### Swing starts -- viable volume, real ceiling, not just the obvious chalk", "",
      md_table(swing_md |>
                 mutate(rank_gap = if_else(is.na(rank_gap), "--", sprintf("%+d", rank_gap)),
                        ecr_rank = if_else(is.na(ecr_rank), "--", as.character(ecr_rank))) |>
                 transmute(position, content_rank, player_disp, posteam, defteam,
                           start = paste0(start_pct, "%"),
                           boom = paste0(boom_pct, "%"),
                           ecr = ecr_rank, gap = rank_gap, disp_vol),
               c("Pos", "#", "Player", "Team", "Opp", "Start", "Boom", "ECR", "Gap", "proj vol")),
      "")
  }
}

md <- c(md, "## Flex boom board -- P(elite week), position-calibrated bars", "",
  md_table(boom_flex |> slice_head(n = 15) |>
             transmute(rank, position, player_disp, posteam, defteam,
                       boom = paste0(boom_pct, "%"),
                       bar = paste0(boom_bar, "+"), disp_vol),
           c("#", "Pos", "Player", "Team", "Opp", "Boom", "Bar", "proj vol")),
  "",
  "*Elite-week bars are position-calibrated: RB/WR 20+ PPR, TE 17+ PPR (equal rarity by construction).*",
  "")

md <- c(md, "## Streamer / waiver board (low projected volume, live start odds)", "",
  md_table(streamer_board |> group_by(position) |> slice_head(n = 8) |> ungroup() |>
             transmute(position, rank, player_disp, posteam, defteam,
                       start = paste0(start_pct, "%"), disp_vol),
           c("Pos", "#", "Player", "Team", "Opp", "Start", "proj vol")),
  "")

if (!is.null(ecr_gap)) {
  md <- c(md, "## Model vs market (ECR gap)", "",
    md_table(ecr_gap |> slice_head(n = 15) |>
               mutate(rank_gap = sprintf("%+d", rank_gap)),
             c("Pos", "Player", "Team", "Model rank", "ECR", "Gap", "Start")),
    "",
    "*Consensus ranks: Expert Consensus Rankings courtesy of [FantasyPros](https://www.fantasypros.com).*",
    "")
}

md <- c(md, "---",
  sprintf("*Displayed probabilities are editorially capped at %d-%d%%: the model never publishes a certainty. QB uses standard (4pt pass TD) scoring; RB/WR/TE are PPR. TE start/boom bars are 12/17 (position-calibrated), RB/WR are 15/20.*",
          round(100 * DISPLAY_FLOOR), round(100 * DISPLAY_CEIL)))

writeLines(paste(md, collapse = "\n"), sprintf("output/10d_boards_%s.md", WTAG))
cli_alert_success("output/10d_boards_{WTAG}.md")

if (!is.null(receipts)) {
  played     <- receipts |> filter(!dnp)
  n_dnp      <- sum(receipts$dnp)
  worst_miss <- played |> filter(!hit_start) |> slice_max(p_start_recal, n = 3)
  best_hit   <- played |> filter(hit_start)  |> slice_min(p_start_recal, n = 3)
  rmd <- c(
    sprintf("# BOXSCORE PROPHET -- %d Week %d receipts", TARGET_SEASON, TARGET_WEEK), "",
    "Every week we grade the probabilities we published before kickoff.", "",
    "## Calibration by stated start odds", "",
    sprintf("Graded on players who took the field. %d player%s with no stat line (inactive or scratched) %s not counted.",
            n_dnp, if (n_dnp == 1) "" else "s", if (n_dnp == 1) "is" else "are"), "",
    md_table({
      rb <- played |> group_by(band_start) |>
        summarise(n = n(), stated = mean(p_start_recal),
                  hit = mean(hit_start), .groups = "drop")
      rb |> transmute(band_start, n,
                      stated = paste0(round(100 * stated), "%"),
                      actual = paste0(round(100 * hit), "%"))
    }, c("Stated band", "n", "Avg stated", "Actual hit rate")), "",
    "## The model's worst misses (highest stated odds that did not hit)", "",
    md_table(worst_miss |> transmute(position, player_name, posteam,
                                     stated = paste0(start_pct, "%"),
                                     actual = sprintf("%.1f FP", fp_actual)),
             c("Pos", "Player", "Team", "Stated", "Actual")), "",
    "## Longshots that hit (lowest stated odds that cleared the bar)", "",
    md_table(best_hit |> transmute(position, player_name, posteam,
                                   stated = paste0(start_pct, "%"),
                                   actual = sprintf("%.1f FP", fp_actual)),
             c("Pos", "Player", "Team", "Stated", "Actual")), "")
  if (!is.null(flex) && nrow(flex) > 0) {
    flex_tbl <- function(df) md_table(df |> transmute(
        position, player_name, posteam,
        model = model_rank, ecr = replace_na(as.character(ecr_rank), "--"),
        stated = paste0(start_pct, "%"),
        actual = sprintf("%.1f FP", fp_actual),
        result = if_else(hit_start, "HIT", "miss")),
      c("Pos", "Player", "Team", "Model #", "ECR #", "Stated", "Actual", "Result"))
    rate <- function(df) if (nrow(df) == 0) "none" else
      sprintf("%d of %d cleared the start bar", sum(df$hit_start), nrow(df))
    hi <- flex |> filter(view == "model_higher") |> arrange(desc(rank_gap))
    lo <- flex |> filter(view == "model_lower")  |> arrange(rank_gap)
    rmd <- c(rmd,
      "## Flex-tier receipts (the decisions that actually get made)", "",
      sprintf("Players FantasyPros ECR ranked in the flex tier (RB/WR %d-%d, QB/TE %d-%d), plus players the model moved into that range from below it. Consensus auto-starts are excluded. Graded on players who took the field. %d players.",
              FLEX_BAND_LO[["RB"]], FLEX_BAND_HI[["RB"]], FLEX_BAND_LO[["QB"]],
              FLEX_BAND_HI[["QB"]], nrow(flex)), "",
      sprintf("- Model ranked higher than consensus: %s.", rate(hi)),
      sprintf("- Model ranked lower than consensus: %s.", rate(lo)),
      sprintf("- Model and consensus within a few spots: %s.", rate(flex |> filter(view == "agree"))), "")
    if (nrow(hi) > 0) rmd <- c(rmd,
      "### Model higher than consensus", "", flex_tbl(slice_head(hi, n = 8)), "")
    if (nrow(lo) > 0) rmd <- c(rmd,
      "### Model lower than consensus", "", flex_tbl(slice_head(lo, n = 8)), "")
    rmd <- c(rmd,
      "### Flex misses the model backed (highest stated chances that missed)", "",
      flex_tbl(flex |> filter(!hit_start) |> slice_max(start_pct, n = 5, with_ties = FALSE)), "",
      "### Flex hits the model doubted (lowest stated chances that hit)", "",
      flex_tbl(flex |> filter(hit_start) |> slice_min(start_pct, n = 5, with_ties = FALSE)), "")
  }
  if (!is.null(pending) && nrow(pending) > 0) {
    rmd <- c(rmd,
      "## Still on the board (games not yet played)", "",
      "These statements are locked and will be graded as-is.", "",
      md_table(pending |> arrange(desc(start_pct)) |>
                 transmute(position, player_name, posteam, defteam,
                           start = paste0(start_pct, "%"),
                           boom = paste0(boom_pct, "%")),
               c("Pos", "Player", "Team", "Opp", "Start", "Boom")), "")
  }
  writeLines(paste(rmd, collapse = "\n"), sprintf("output/10d_receipts_%s.md", WTAG))
  cli_alert_success("output/10d_receipts_{WTAG}.md")
}

# ===========================================================================
# 6. X BOARD IMAGES
# ===========================================================================

cli_h1("Board images")

dir.create("output/img", showWarnings = FALSE)

board_image <- function(df, value_col, order_col, accent, title, subtitle, out_path) {
  # order by the raw probability, not the rounded display pct -- ties in the
  # displayed number must keep the same order as the published CSV/markdown
  d <- df |>
    slice_head(n = BOARD_N) |>
    mutate(label = sprintf("%d%%", .data[[value_col]]),
           name  = fct_reorder(paste0(player_disp, "  (", posteam, " v ", defteam, ")"),
                               .data[[order_col]]))
  g <- ggplot(d, aes(x = .data[[value_col]], y = name)) +
    geom_col(fill = accent, width = 0.62) +
    geom_text(aes(label = label), hjust = -0.25, size = 3.4, color = INK) +
    scale_x_continuous(limits = c(0, max(d[[value_col]]) * 1.18),
                       expand = expansion(mult = c(0, 0.02))) +
    labs(title = title, subtitle = subtitle,
         caption = "boxscore-prophet | probabilities graded publicly every Monday",
         x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      plot.background  = element_rect(fill = SURFACE, color = NA),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_line(color = "#E5E7EB", linewidth = 0.3),
      axis.text.x      = element_blank(),
      axis.text.y      = element_text(color = INK, size = 10),
      plot.title       = element_text(color = INK, face = "bold", size = 15),
      plot.subtitle    = element_text(color = INK_MUTED, size = 10),
      plot.caption     = element_text(color = INK_MUTED, size = 7.5),
      plot.margin      = margin(12, 16, 8, 10)
    )
  ggsave(out_path, g, width = 6.4, height = 5.4, dpi = 200, bg = SURFACE)
  cli_alert_success(out_path)
}

for (pos in c("RB", "WR", "TE", "QB")) {
  board_image(
    start_board |> filter(position == pos),
    "start_pct", "p_start_recal", ACCENT_START,
    sprintf("Week %d %s start board", TARGET_WEEK, pos),
    sprintf("Chance of a startable week: %s", START_LABEL[pos]),
    sprintf("output/img/10d_start_%s_%s.png", tolower(pos), WTAG)
  )
}

board_image(
  boom_flex,
  "boom_pct", "p_boom_recal", ACCENT_BOOM,
  sprintf("Week %d flex boom board", TARGET_WEEK),
  "Chance of an elite week: RB/WR P(20+ PPR), TE P(17+ PPR)",
  sprintf("output/img/10d_boom_flex_%s.png", WTAG)
)

cli_h1("Step 10d complete -- {TARGET_SEASON} week {TARGET_WEEK}")
