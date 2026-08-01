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
cli_alert_success("Board CSVs written (start / boom / streamer)")

# ===========================================================================
# 3. ECR GAP (file-drop feed; skips gracefully)
# ===========================================================================

ecr_path <- sprintf("data/ecr/ecr_%s.csv", WTAG)
ecr_gap <- NULL
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
  cli_alert_success("ECR gap: {nrow(ecr_gap)} matched | depth by pos: {paste(ecr_depth$position, ecr_depth$depth, collapse = ', ')}")
} else {
  cli_alert_info("No ECR feed at {ecr_path} -- gap table skipped (drop a CSV or run 10d0 once the API key is active)")
}

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
  locked <- readr::read_csv(ledger_path, show_col_types = FALSE) |>
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

  receipts <- graded |>
    filter(resolved) |>
    mutate(hit_start  = fp_actual >= thresh_start,
           hit_boom   = fp_actual >= thresh_boom,
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
      group_by(band_start) |>
      summarise(n = n(),
                stated = mean(p_start_recal),
                hit_rate = mean(hit_start), .groups = "drop") |>
      mutate(delta_pp = round(100 * (hit_rate - stated), 1))
    readr::write_csv(receipts, sprintf("output/10d_receipts_%s.csv", WTAG))
    readr::write_csv(receipt_bands, sprintf("output/10d_receipt_bands_%s.csv", WTAG))
    cli_alert_success("Receipts: {nrow(receipts)} graded ({sum(receipts$dnp)} DNP) | {nrow(pending)} pending | start hits: {sum(receipts$hit_start)} | booms: {sum(receipts$hit_boom)}")
    print(receipt_bands, n = Inf)
  } else {
    cli_alert_info("Nothing resolved yet at as-of {format(RECEIPT_AS_OF, '%Y-%m-%d %H:%M')} -- {nrow(pending)} statements pending")
    receipts <- NULL
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
  worst_miss <- receipts |> filter(!hit_start) |> slice_max(p_start_recal, n = 3)
  best_hit   <- receipts |> filter(hit_start)  |> slice_min(p_start_recal, n = 3)
  rmd <- c(
    sprintf("# BOXSCORE PROPHET -- %d Week %d receipts", TARGET_SEASON, TARGET_WEEK), "",
    "Every week we grade the probabilities we published before kickoff.", "",
    "## Calibration by stated start odds", "",
    md_table({
      rb <- receipts |> group_by(band_start) |>
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
