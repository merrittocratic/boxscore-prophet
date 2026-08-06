# R/10g_movers_table.R
# Step 10g: MOVERS table -- this week's published P(start)/P(boom) vs each
# player's own trailing baseline, with matchup/environment context columns.
#
# Content product only (spec: Steve 2026-08-06). Reads published ledgers and
# slate feature files; never touches the scoring chain, so no gates apply.
#
# Definitions:
#   - "now"  = latest as_of snapshot per player in this week's 10c ledger
#     (Tuesday board on Tuesday; the freshest re-score later in the week).
#   - "base" = mean of the player's published probabilities over the prior
#     BASE_WINDOW scored weeks (latest snapshot per week), same season, from
#     our own ledger files -- a mover is always vs what readers saw.
#   - Context columns are raw component shifts (opponent defense adjustment,
#     implied total, spread, predicted volume, injury status), NOT a
#     per-factor attribution. The writer narrates; the model does not claim
#     a decomposition it cannot back. Counterfactual re-scoring is a v2.
#
# Eligibility: >= MIN_BASE_WEEKS prior scored weeks (so the board starts at
# week MIN_BASE_WEEKS + 1) and baseline P(start) >= REL_FLOOR. Rookies with
# thin history belong to the 10e tracker, not here.
#
# Outputs: output/10g_movers_<season>_w<week>.csv  (all eligible players)
#          output/10g_movers_<season>_w<week>.md   (top movers per position)
#
# Usage: Rscript R/10g_movers_table.R [season] [week]

suppressPackageStartupMessages({
  library(tidyverse)
  library(cli)
})

args <- commandArgs(trailingOnly = TRUE)
TARGET_SEASON <- if (length(args) >= 1) as.integer(args[1]) else 2026L
TARGET_WEEK   <- if (length(args) >= 2) as.integer(args[2]) else 15L
WTAG <- sprintf("%d_w%02d", TARGET_SEASON, TARGET_WEEK)

BASE_WINDOW    <- 4L      # trailing weeks in the baseline
MIN_BASE_WEEKS <- 2L      # minimum scored weeks to qualify
REL_FLOOR      <- 0.15    # baseline P(start) relevance floor
TOP_N          <- 5L      # risers/fallers shown per position in the md
DISPLAY_FLOOR  <- 0.02    # same editorial caps as 10d
DISPLAY_CEIL   <- 0.95

SLATE_FILE <- c(RB = "10b2_rb_slate", WR = "10b3_wr_slate",
                QB = "10b4_qb_slate", TE = "10b5_te_slate")
VOL_LABEL  <- c(RB = "touches", WR = "targets", TE = "targets",
                QB = "dropbacks")

cap_pct <- function(p) round(100 * pmin(pmax(p, DISPLAY_FLOOR), DISPLAY_CEIL))

cli_h1("Step 10g: movers table for {TARGET_SEASON} week {TARGET_WEEK}")

# ---- 1. Ledgers: now + baseline -------------------------------------------

read_ledger_week <- function(wk) {
  f <- sprintf("output/10c_ledger_%d_w%02d.csv", TARGET_SEASON, wk)
  if (!file.exists(f)) return(NULL)
  read_csv(f, show_col_types = FALSE) |>
    group_by(position, player_id) |>
    slice_max(as_of, n = 1, with_ties = FALSE) |>   # latest snapshot only
    ungroup() |>
    transmute(position, player_id, player_name, posteam, defteam, week,
              report_status, pred_vol,
              p_start = p_start_recal, p_boom = p_boom_recal)
}

now <- read_ledger_week(TARGET_WEEK)
if (is.null(now)) {
  cli_abort("No ledger for {WTAG} -- run 10c first.")
}

base_weeks <- seq(max(1L, TARGET_WEEK - BASE_WINDOW), TARGET_WEEK - 1L)
base_raw <- map(base_weeks, read_ledger_week) |> compact() |> list_rbind()
if (length(unique(base_raw$week)) < MIN_BASE_WEEKS) {
  cli_alert_warning(paste0(
    "Only ", length(unique(base_raw$week)), " prior ledger week(s) found; ",
    "movers needs ", MIN_BASE_WEEKS, ". Writing nothing."))
  quit(save = "no", status = 0)
}

base <- base_raw |>
  group_by(position, player_id) |>
  summarise(p_start_base  = mean(p_start),
            p_boom_base   = mean(p_boom),
            pred_vol_base = mean(pred_vol),
            n_base_weeks  = n_distinct(week), .groups = "drop")

# ---- 2. Context: opponent defense + environment from slate files ----------

opp_adj <- function(df, pos) {
  # position-relevant opponent adjustment (EPA/play scale, as trained)
  if (pos == "RB") df$def_rush_epa_adj
  else rowMeans(cbind(df$def_short_pass_epa_adj, df$def_deep_pass_epa_adj))
}

read_slate_week <- function(pos, wk) {
  f <- sprintf("output/%s_%d_w%02d.csv", SLATE_FILE[[pos]], TARGET_SEASON, wk)
  if (!file.exists(f)) return(NULL)
  df <- read_csv(f, show_col_types = FALSE)
  tibble(position = pos, player_id = df$player_id, week = wk,
         opp_def_adj = opp_adj(df, pos),
         implied_total = df$implied_total, team_spread = df$team_spread)
}

ctx <- expand_grid(pos = names(SLATE_FILE), wk = c(base_weeks, TARGET_WEEK)) |>
  pmap(\(pos, wk) read_slate_week(pos, wk)) |> compact() |> list_rbind()

ctx_now <- ctx |> filter(week == TARGET_WEEK) |>
  select(position, player_id, opp_def_adj_now = opp_def_adj,
         implied_total_now = implied_total, team_spread_now = team_spread)
ctx_base <- ctx |> filter(week != TARGET_WEEK) |>
  group_by(position, player_id) |>
  summarise(opp_def_adj_base   = mean(opp_def_adj, na.rm = TRUE),
            implied_total_base = mean(implied_total, na.rm = TRUE),
            team_spread_base   = mean(team_spread, na.rm = TRUE),
            .groups = "drop")

# ---- 3. Assemble, filter, rank --------------------------------------------

movers <- now |>
  inner_join(base,     by = c("position", "player_id")) |>
  left_join(ctx_now,   by = c("position", "player_id")) |>
  left_join(ctx_base,  by = c("position", "player_id")) |>
  filter(n_base_weeks >= MIN_BASE_WEEKS, p_start_base >= REL_FLOOR) |>
  mutate(delta_start_pp = 100 * (p_start - p_start_base),
         delta_boom_pp  = 100 * (p_boom  - p_boom_base),
         delta_vol      = pred_vol - pred_vol_base,
         injury_flag    = !is.na(report_status)) |>
  arrange(desc(abs(delta_start_pp)))

readr::write_csv(movers, sprintf("output/10g_movers_%s.csv", WTAG))
cli_alert_success("{nrow(movers)} eligible players -> 10g_movers_{WTAG}.csv")

# ---- 4. Markdown: top movers per position ---------------------------------

fmt_row <- function(r) {
  ctx_bits <- c(
    sprintf("proj %s %.0f (base %.0f)", VOL_LABEL[[r$position]],
            r$pred_vol, r$pred_vol_base),
    if (!is.na(r$implied_total_now))
      sprintf("implied total %.1f (base %.1f)",
              r$implied_total_now, r$implied_total_base),
    if (!is.na(r$opp_def_adj_now))
      sprintf("opp def adj %+.2f (base %+.2f)",
              r$opp_def_adj_now, r$opp_def_adj_base),
    if (r$injury_flag) paste0("status: ", r$report_status)
  )
  sprintf("| %s | %s | %s | %d%% | %d%% | %+.0f | %s |",
          r$player_name, r$posteam, r$defteam,
          cap_pct(r$p_start), cap_pct(r$p_start_base), r$delta_start_pp,
          paste(ctx_bits, collapse = "; "))
}

md <- c(sprintf("# BOXSCORE PROPHET -- %d Week %d movers", TARGET_SEASON,
                TARGET_WEEK), "",
        sprintf(paste0("P(start) this week vs the player's own trailing ",
                       "%d-week published baseline (n >= %d weeks, baseline ",
                       ">= %d%%). Context columns are raw component shifts, ",
                       "not an attribution."),
                BASE_WINDOW, MIN_BASE_WEEKS, round(100 * REL_FLOOR)), "")

for (pos in c("RB", "WR", "QB", "TE")) {
  pm <- movers |> filter(position == pos)
  if (nrow(pm) == 0) next
  risers  <- pm |> filter(delta_start_pp > 0) |> slice_head(n = TOP_N)
  fallers <- pm |> filter(delta_start_pp < 0) |> slice_head(n = TOP_N)
  for (grp in list(c("risers", "Risers"), c("fallers", "Fallers"))) {
    rows <- if (grp[1] == "risers") risers else fallers
    if (nrow(rows) == 0) next
    md <- c(md, sprintf("## %s %s", pos, grp[2]), "",
            "| Player | Team | Opp | Now | Base | Chg (pp) | Context |",
            "|---|---|---|---|---|---|---|",
            map_chr(seq_len(nrow(rows)), \(i) fmt_row(rows[i, ])), "")
  }
}

writeLines(paste(md, collapse = "\n"), sprintf("output/10g_movers_%s.md", WTAG))
cli_alert_success("Markdown -> 10g_movers_{WTAG}.md")
cli_h1("Step 10g complete -- {TARGET_SEASON} week {TARGET_WEEK}")
