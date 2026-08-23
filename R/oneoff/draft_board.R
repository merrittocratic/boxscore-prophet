# R/oneoff/draft_board.R
# ONE-OFF personal draft board (not a pipeline stage, not published, not
# wired into weekly_run.sh / refresh_latest.sh / Earnest's cron).
#
# WHAT THIS IS
# Season-long draft prep from the weekly engine. Because no 2026 games have
# been played, every player's form features are identical in every week --
# the ONLY things that move week to week are the opponent defense, the
# Vegas line, and the bye. So the board is built by scoring one base slate
# 18 times against 18 different opponents.
#
# WHAT IT IS NOT
# A player ranking. On the walk-forward record the engine's discrimination
# at cold start is AUC ~0.59-0.61 (RB/WR, week 1) versus ~0.70-0.74 from
# week 2 on. Calibration survives; RANKING does not. ECR will beat the
# model's own ordering. The differentiated column here is sched_effect,
# which is opponent-driven and cancels the weak player baseline out.
#
# THREE SCORING PASSES
#   blind   -- all 18 weeks, Vegas features forced NA. PRIMARY basis for
#              exp_startable_weeks and sched_effect. Consistent by
#              construction: nflverse carries 2026 lines for weeks 1-3
#              only, so a Vegas-informed sum would mix two regimes and
#              charge the difference to "schedule".
#   vegas   -- weeks 1-3 with real posted lines. Feeds p_start_wk1_vegas
#              (a genuine Week 1 number) and sizes the Vegas seam.
#   neutral -- league-mean defense, Vegas NA. Gives each player his
#              P(start) against an average opponent, which is the
#              counterfactual sched_effect is measured against.
#
#   sched_effect = sum(p_start over played weeks) - n_games * p_neutral
#
# ARTIFACT HYGIENE (this matters -- see HANDOFF)
# 10c APPENDS to output/10c_ledger_<wtag>.csv, which 10g uses as the
# movers baseline. Draft-prep rows left in output/ would corrupt the real
# season's movers deltas. Every artifact this script creates is swept into
# the run directory on exit, including on error. The sweep is stem-specific
# and refuses to touch any git-tracked file (output/10f_weekly_eval_2026_w00.csv
# is tracked and matches a naive *2026_w* glob).
#
# Usage: Rscript R/oneoff/draft_board.R [season]
#   Env: DRAFT_PREP_OUT -- run directory (default ~/ff_draft_prep_2026/<today>)

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(cli)
})

source("R/10b_roster_helpers.R")   # vegas_slate_lines, team-code helpers
source("R/10d_name_helpers.R")     # normalize_player_name

args <- commandArgs(trailingOnly = TRUE)
SEASON <- if (length(args) >= 1) as.integer(args[1]) else 2026L
WEEKS  <- 1:18

OUT_DIR <- Sys.getenv(
  "DRAFT_PREP_OUT",
  unset = file.path(path.expand("~/ff_draft_prep_2026"), format(Sys.Date(), "%Y-%m-%d"))
)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
RAW_DIR <- file.path(OUT_DIR, "raw")
dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)

POS_STEMS <- c(RB = "10b2_rb_slate", WR = "10b3_wr_slate",
               TE = "10b5_te_slate", QB = "10b4_qb_slate")

cli_h1("One-off draft board -- {SEASON}")
cli_alert_info("Run directory: {OUT_DIR}")

# ===========================================================================
# ARTIFACT SWEEP -- registered before anything is written
# ===========================================================================

TRACKED <- tryCatch(
  system2("git", c("ls-files", "output/"), stdout = TRUE, stderr = FALSE),
  error = function(e) character(0)
)

created_stems <- function(season) {
  stems <- c("10b_game_slate", unname(POS_STEMS),
             "10c_scored_slate", "10c_ledger", "10c_scored_detail")
  unlist(lapply(stems, function(s)
    sprintf("output/%s_%d_w%02d.csv", s, season, WEEKS)))
}

sweep_artifacts <- function() {
  cand <- created_stems(SEASON)
  cand <- cand[file.exists(cand)]
  # Absolute refusal to move a tracked file, whatever the stem match said.
  keep <- cand %in% TRACKED
  if (any(keep)) {
    cli_alert_warning("Refusing to sweep tracked file(s): {paste(cand[keep], collapse=', ')}")
  }
  cand <- cand[!keep]
  if (!length(cand)) return(invisible(NULL))
  ok <- file.rename(cand, file.path(RAW_DIR, basename(cand)))
  cli_alert_success("Swept {sum(ok)} artifact(s) out of output/ into {RAW_DIR}")
  invisible(NULL)
}
# NOTE: on.exit() does NOT work here. At script top level there is no
# enclosing function frame, so the handler never fires on the paths that
# matter and the slates stay in output/ (observed on the first run). A
# finalizer on the global env with onexit=TRUE covers the error/abort path;
# the success path calls sweep_artifacts() explicitly at the end.
invisible(reg.finalizer(globalenv(),
                        function(e) try(sweep_artifacts(), silent = TRUE),
                        onexit = TRUE))

# ===========================================================================
# 1. BASE SLATES + DEFENSE LOOKUP
# ===========================================================================

cli_h1("Step 1: base slates")

# Build the base slates ourselves rather than trusting whatever is sitting
# in output/. This script WRITES to those same paths while generating each
# week (including the neutral pass, which sets defteam = "AVG"), so a
# previous run leaves a contaminated week-1 slate behind. Rebuilding makes
# the script self-contained and safe to re-run the morning of a draft --
# which is the whole point of it.
run_script <- function(path) {
  res <- system2("Rscript", c(path, SEASON, 1), stdout = FALSE, stderr = FALSE)
  if (res != 0) cli_abort("{path} failed (exit {res})")
}
cli_alert_info("Rebuilding week 1 base slates (4 builders + game slate)")
run_script("R/10b_weekly_slate.R")
for (s in c("R/10b2_player_slate.R", "R/10b3_wr_slate.R",
            "R/10b5_te_slate.R", "R/10b4_qb_slate.R")) {
  run_script(s)
  cli_alert_info("  {basename(s)}")
}

base_slates <- map(POS_STEMS, function(stem) {
  p <- sprintf("output/%s_%d_w01.csv", stem, SEASON)
  if (!file.exists(p)) cli_abort("Base slate {p} was not produced")
  s <- read_csv(p, show_col_types = FALSE)
  if (any(s$defteam == "AVG", na.rm = TRUE)) {
    cli_abort("Base slate {p} is contaminated (defteam == 'AVG' from a neutral pass)")
  }
  s
})

# Keep a pristine copy outside output/ so the run is auditable after the sweep.
dir.create(file.path(RAW_DIR, "base"), recursive = TRUE, showWarnings = FALSE)
iwalk(base_slates, function(s, pos)
  write_csv(s, file.path(RAW_DIR, "base", sprintf("%s_base_w01.csv", tolower(pos)))))

walk2(names(base_slates), base_slates, function(pos, s)
  cli_alert_info("{pos}: {nrow(s)} players, {n_distinct(s$defteam)} distinct defteam"))

# Defense lookup: the numeric opponent-adjustment columns, one row per
# defense. All 32 must be present or the schedule swap silently loses a
# team (exactly the AZ/ARI failure mode, one layer down).
def_lookups <- imap(base_slates, function(s, pos) {
  adj_cols <- grep("^def_.*_adj$", names(s), value = TRUE)
  lk <- s |> distinct(defteam, across(all_of(adj_cols)))
  dup <- lk |> count(defteam) |> filter(n > 1)
  if (nrow(dup)) cli_abort("{pos}: defense lookup not unique for {paste(dup$defteam, collapse=', ')}")
  if (nrow(lk) != 32) cli_abort("{pos}: defense lookup has {nrow(lk)} teams, expected 32")
  lk
})
cli_alert_success("Defense lookups built (32 teams x {length(def_lookups)} positions)")

# ===========================================================================
# 2. SCHEDULE + VEGAS FOR ALL WEEKS
# ===========================================================================

cli_h1("Step 2: schedule and lines")

sched <- load_schedules(SEASON) |> filter(game_type == "REG", week %in% WEEKS)

games_long <- bind_rows(
  sched |> transmute(game_id, week, posteam = home_team, defteam = away_team, is_home = TRUE),
  sched |> transmute(game_id, week, posteam = away_team, defteam = home_team, is_home = FALSE)
)

# Reuse the frozen helper rather than reimplement the sign convention
# (home = +spread_line, away = -spread_line, implied = (total + spread)/2).
vegas <- vegas_slate_lines(games_long |> select(game_id, posteam), SEASON, hindcast = FALSE)

games_long <- games_long |> left_join(vegas, by = c("game_id", "posteam"))

n_games_by_team <- games_long |> count(posteam, name = "n_games")
cli_alert_info("Weeks with posted lines: {paste(sort(unique(games_long$week[!is.na(games_long$team_spread)])), collapse=', ')}")
cli_alert_info("Games per team: {paste(sort(unique(n_games_by_team$n_games)), collapse='/')}")

# ===========================================================================
# 3. SCORING HELPERS
# ===========================================================================

# Build the four slate CSVs for one week under a given variant, run 10c,
# read the scored output, then remove the 10c artifacts so the next variant
# can reuse the same week tag without appending to a stale ledger.
write_week_slates <- function(week, use_vegas, neutral = FALSE) {
  gl <- games_long |> filter(week == !!week)
  imap(base_slates, function(s, pos) {
    adj_cols <- grep("^def_.*_adj$", names(s), value = TRUE)
    stripped <- s |> select(-all_of(adj_cols), -defteam, -game_id, -week,
                            -team_spread, -implied_total)
    if (neutral) {
      # League-average opponent: every adjustment column at its mean over
      # the 32 defenses. Vegas held neutral (NA -> maps coalesce).
      means <- def_lookups[[pos]] |> summarise(across(all_of(adj_cols), mean))
      out <- stripped |>
        inner_join(gl |> select(posteam, game_id), by = "posteam") |>
        mutate(defteam = "AVG", week = 1L, season = SEASON,
               team_spread = NA_real_, implied_total = NA_real_) |>
        bind_cols(means[rep(1, nrow(stripped)), , drop = FALSE])
    } else {
      out <- stripped |>
        inner_join(gl, by = "posteam") |>
        left_join(def_lookups[[pos]], by = "defteam") |>
        mutate(season = SEASON,
               team_spread   = if (use_vegas) team_spread   else NA_real_,
               implied_total = if (use_vegas) implied_total else NA_real_) |>
        select(-is_home)
    }
    out <- out |> select(any_of(names(s)))
    write_csv(out, sprintf("output/%s_%d_w%02d.csv", POS_STEMS[[pos]], SEASON, week))
    nrow(out)
  })
}

score_week <- function(week, label) {
  wtag <- sprintf("%d_w%02d", SEASON, week)
  res <- system2("Rscript", c("R/10c_weekly_score.R", SEASON, week),
                 stdout = FALSE, stderr = FALSE)
  scored_path <- sprintf("output/10c_scored_slate_%s.csv", wtag)
  if (res != 0 || !file.exists(scored_path)) {
    cli_abort("Scoring failed for {label} week {week} (exit {res})")
  }
  out <- read_csv(scored_path, show_col_types = FALSE) |>
    select(position, player_id, player_name, posteam, defteam, week,
           pred_vol, p_start_recal, p_boom_recal) |>
    mutate(variant = label)
  # Clear 10c artifacts so the next pass over this week tag starts clean
  # (the ledger appends -- reusing a tag without this double-counts).
  file.remove(Filter(file.exists, sprintf(
    c("output/10c_scored_slate_%s.csv", "output/10c_ledger_%s.csv",
      "output/10c_scored_detail_%s.csv"), wtag)))
  out
}

# ===========================================================================
# 4. THREE SCORING PASSES
# ===========================================================================

cli_h1("Step 3: scoring passes")

cli_alert_info("Pass 1/3: blind (all {length(WEEKS)} weeks, Vegas forced NA)")
blind <- map(WEEKS, function(w) {
  write_week_slates(w, use_vegas = FALSE)
  cli_alert_info("  week {w}")
  score_week(w, "blind")
}) |> list_rbind()

vegas_weeks <- sort(unique(games_long$week[!is.na(games_long$team_spread)]))
cli_alert_info("Pass 2/3: vegas (weeks {paste(vegas_weeks, collapse=', ')})")
vegas_scored <- map(vegas_weeks, function(w) {
  write_week_slates(w, use_vegas = TRUE)
  score_week(w, "vegas")
}) |> list_rbind()

cli_alert_info("Pass 3/3: neutral (league-average opponent)")
write_week_slates(1, use_vegas = FALSE, neutral = TRUE)
neutral <- score_week(1, "neutral") |>
  select(position, player_id, p_neutral_start = p_start_recal,
         p_neutral_boom = p_boom_recal)

cli_alert_success("Scored {nrow(blind)} blind + {nrow(vegas_scored)} vegas + {nrow(neutral)} neutral rows")

# ===========================================================================
# 5. AGGREGATE
# ===========================================================================

cli_h1("Step 4: aggregate")

# Vegas seam: how far the Vegas-informed number sits from the blind one on
# the same player-weeks. Reported so the choice of blind as primary is
# auditable rather than asserted.
seam <- blind |>
  filter(week %in% vegas_weeks) |>
  select(position, player_id, week, p_blind = p_start_recal) |>
  inner_join(vegas_scored |> select(position, player_id, week, p_vegas = p_start_recal),
             by = c("position", "player_id", "week")) |>
  mutate(d = p_vegas - p_blind)

seam_summary <- seam |>
  group_by(position) |>
  summarise(n = n(), mean_abs_pp = 100 * mean(abs(d)),
            max_abs_pp = 100 * max(abs(d)), .groups = "drop")
print(as.data.frame(seam_summary), row.names = FALSE)

season_totals <- blind |>
  group_by(position, player_id, player_name, posteam) |>
  summarise(weeks_scored       = n(),
            exp_startable_wks  = sum(p_start_recal),
            exp_boom_wks       = sum(p_boom_recal),
            mean_p_start       = mean(p_start_recal),
            best_wk_p_start    = max(p_start_recal),
            worst_wk_p_start   = min(p_start_recal),
            .groups = "drop")

wk1_vegas <- vegas_scored |>
  filter(week == 1) |>
  select(position, player_id, p_start_wk1 = p_start_recal, p_boom_wk1 = p_boom_recal)

bye <- games_long |>
  group_by(posteam) |>
  summarise(bye_week = setdiff(WEEKS, week)[1], .groups = "drop")

board <- season_totals |>
  left_join(neutral, by = c("position", "player_id")) |>
  left_join(wk1_vegas, by = c("position", "player_id")) |>
  left_join(bye, by = "posteam") |>
  mutate(
    exp_startable_neutral = weeks_scored * p_neutral_start,
    sched_effect          = exp_startable_wks - exp_startable_neutral
  )

# ===========================================================================
# 6. ECR + ROOKIE JOINS
# ===========================================================================

cli_h1("Step 5: ECR and rookie joins")

ecr_path <- file.path(OUT_DIR, "ecr_draft.csv")
if (file.exists(ecr_path)) {
  ecr <- read_csv(ecr_path, show_col_types = FALSE) |>
    select(position, player_name_norm, ecr_rank, ecr_best, ecr_worst) |>
    distinct(position, player_name_norm, .keep_all = TRUE)
  board <- board |>
    mutate(player_name_norm = normalize_player_name(player_name)) |>
    left_join(ecr, by = c("position", "player_name_norm"))
  cli_alert_success("ECR joined: {sum(!is.na(board$ecr_rank))}/{nrow(board)} rows")
} else {
  board <- board |> mutate(ecr_rank = NA_integer_, ecr_best = NA_integer_,
                           ecr_worst = NA_integer_)
  cli_alert_warning("No ECR file at {ecr_path} -- board carries no anchor column")
}

cw_path <- file.path(OUT_DIR, "rookie_crosswalk.csv")
CANONICAL_CW <- "data/10e_rookie_crosswalk.csv"
if (!file.exists(cw_path) && file.exists(CANONICAL_CW)) {
  file.copy(CANONICAL_CW, cw_path)
  cli_alert_info("Copied rookie crosswalk from {CANONICAL_CW} into run dir")
}
if (file.exists(cw_path)) {
  cw <- read_csv(cw_path, show_col_types = FALSE) |>
    filter(!is.na(gsis_id)) |>
    select(player_id = gsis_id, dm_pred = pred, dm_p_boom = p_boom,
           dm_p_bust = p_bust, dm_verdict = model_verdict) |>
    distinct(player_id, .keep_all = TRUE)
  board <- board |> left_join(cw, by = "player_id")
  cli_alert_success("Rookie verdicts joined: {sum(!is.na(board$dm_verdict))} rows")
} else {
  board <- board |> mutate(dm_pred = NA_real_, dm_p_boom = NA_real_,
                           dm_p_bust = NA_real_, dm_verdict = NA_character_)
  cli_alert_warning("No rookie crosswalk at {cw_path}")
}

board <- board |>
  group_by(position) |>
  mutate(
    model_rank = rank(-exp_startable_wks, ties.method = "min"),
    # CENTERED is the column to read. Raw sched_effect carries a systematic
    # positive offset that is NOT schedule quality: P(start) is nonlinear in
    # the defense adjustment, so summing over 17 actual opponents does not
    # equal 17x the value at the mean adjustment (Jensen), and a team plays
    # a specific 17-opponent subset rather than the league. Both shift the
    # whole position by a constant. Subtracting the positional mean removes
    # the artifact and leaves the part that differs BETWEEN players, which
    # is what a draft tiebreaker actually needs.
    sched_effect_centered = sched_effect - mean(sched_effect),
    sched_rank = rank(-sched_effect_centered, ties.method = "min"),
    vs_ecr     = if (all(is.na(ecr_rank))) NA_real_ else ecr_rank - model_rank
  ) |>
  ungroup() |>
  mutate(is_rookie = !is.na(dm_verdict))

# Report how much schedule signal each position actually carries. A position
# whose centered spread is ~0 has no usable schedule tiebreaker, and saying
# so is more useful than shipping a column of noise.
sched_signal <- board |>
  group_by(position) |>
  summarise(sd_centered = sd(sched_effect_centered),
            p5  = quantile(sched_effect_centered, 0.05),
            p95 = quantile(sched_effect_centered, 0.95),
            .groups = "drop") |>
  mutate(usable = if_else(sd_centered < 0.05, "NO -- schedule barely moves this position", "yes"))
print(as.data.frame(sched_signal |> mutate(across(where(is.numeric), ~round(.x, 3)))), row.names = FALSE)
write_csv(sched_signal, file.path(OUT_DIR, "schedule_signal_by_position.csv"))

# ===========================================================================
# 7. WRITE
# ===========================================================================

cli_h1("Step 6: write outputs")

COLS <- c("player_name", "posteam", "position", "bye_week", "ecr_rank",
          "sched_effect_centered", "sched_rank",
          "model_rank", "vs_ecr", "exp_startable_wks", "exp_startable_neutral",
          "sched_effect", "exp_boom_wks", "p_neutral_start",
          "p_start_wk1", "p_boom_wk1", "best_wk_p_start", "worst_wk_p_start",
          "weeks_scored", "is_rookie", "dm_verdict", "dm_pred", "dm_p_boom",
          "dm_p_bust", "ecr_best", "ecr_worst", "player_id")

for (pos in names(POS_STEMS)) {
  b <- board |>
    filter(position == pos) |>
    arrange(ifelse(is.na(ecr_rank), 9999L, ecr_rank), desc(exp_startable_wks)) |>
    select(any_of(COLS)) |>
    mutate(across(where(is.numeric), ~ round(.x, 4)))
  p <- file.path(OUT_DIR, sprintf("%s_board.csv", tolower(pos)))
  write_csv(b, p)
  cli_alert_success("{p} ({nrow(b)} players)")
}

grid <- blind |>
  select(position, player_name, posteam, player_id, week, defteam, p_start_recal) |>
  mutate(p_start_recal = round(p_start_recal, 4)) |>
  arrange(position, player_name, week)
write_csv(grid, file.path(OUT_DIR, "schedule_grid.csv"))
cli_alert_success("schedule_grid.csv ({nrow(grid)} player-weeks)")

write_csv(seam_summary, file.path(OUT_DIR, "vegas_seam_summary.csv"))

# ---- changes vs the previous run -----------------------------------------
runs <- list.dirs(dirname(OUT_DIR), recursive = FALSE, full.names = TRUE)
runs <- sort(runs[basename(runs) != basename(OUT_DIR) &
                  grepl("^\\d{4}-\\d{2}-\\d{2}$", basename(runs))])
if (length(runs)) {
  prior_dir <- tail(runs, 1)
  prior <- map(names(POS_STEMS), function(pos) {
    p <- file.path(prior_dir, sprintf("%s_board.csv", tolower(pos)))
    if (file.exists(p)) read_csv(p, show_col_types = FALSE) else NULL
  }) |> compact() |> list_rbind()

  if (nrow(prior)) {
    chg <- board |>
      select(position, player_id, player_name, posteam,
             exp_startable_wks, sched_effect, model_rank, ecr_rank) |>
      inner_join(prior |> select(position, player_id,
                                 prior_exp = exp_startable_wks,
                                 prior_rank = model_rank,
                                 prior_ecr = ecr_rank),
                 by = c("position", "player_id")) |>
      mutate(d_exp_startable = exp_startable_wks - prior_exp,
             d_model_rank    = prior_rank - model_rank,
             d_ecr_rank      = prior_ecr - ecr_rank) |>
      arrange(desc(abs(d_exp_startable))) |>
      mutate(across(where(is.numeric), ~ round(.x, 4)))

    added <- board |> anti_join(prior, by = c("position", "player_id")) |>
      select(position, player_name, posteam, exp_startable_wks, ecr_rank)
    dropped <- prior |> anti_join(board, by = c("position", "player_id")) |>
      select(position, player_name, posteam)

    write_csv(chg, file.path(OUT_DIR, "changes_vs_prior_run.csv"))
    write_csv(added, file.path(OUT_DIR, "changes_added_players.csv"))
    write_csv(dropped, file.path(OUT_DIR, "changes_dropped_players.csv"))
    cli_alert_success("changes vs {basename(prior_dir)}: {nrow(chg)} compared, {nrow(added)} added, {nrow(dropped)} dropped")
  }
} else {
  cli_alert_info("No prior run directory -- skipping change report (this is the baseline run)")
}

# ---- README ---------------------------------------------------------------
readme <- c(
  sprintf("BOXSCORE PROPHET -- PERSONAL DRAFT BOARD (%s season)", SEASON),
  sprintf("Run: %s", basename(OUT_DIR)),
  "PERSONAL USE ONLY. NOT PUBLISHED. Full PPR.",
  "",
  "READ THIS FIRST -- WHICH COLUMN IS DOING THE WORK",
  "",
  "  sched_effect_centered is the column worth acting on. It is expected",
  "  startable weeks against the ACTUAL schedule, minus the same player",
  "  against a league-average opponent, minus the positional mean. Because",
  "  it is the same player and the same cold-start baseline on both sides,",
  "  his baseline cancels and what remains is opponent-driven. Units are",
  "  startable weeks: +0.5 means roughly half an extra startable week of",
  "  schedule relative to the average player at his position.",
  "",
  "  USE THE CENTERED COLUMN, NOT RAW sched_effect. The raw number carries",
  "  a systematic positive offset that is not schedule quality: P(start) is",
  "  nonlinear in the defense adjustment, so 17 actual opponents do not sum",
  "  to 17x the value at the average adjustment, and each team faces a",
  "  specific 17-opponent subset rather than the league. Both shift a whole",
  "  position by a constant. Raw RB sched_effect averages about +0.28, so a",
  "  raw -0.08 is genuinely BELOW average, not slightly below neutral.",
  "",
  "  Check schedule_signal_by_position.csv first. Schedule does not move",
  "  every position: TE centered spread is about +/-0.05 startable weeks,",
  "  i.e. no usable tiebreaker. RB and WR carry real signal.",
  "",
  "  model_rank and vs_ecr are the WEAK columns. On the walk-forward record",
  "  this engine's cold-start discrimination is AUC 0.593 (RB) / 0.609 (WR)",
  "  at week 1, versus 0.70-0.74 from week 2 on. Calibration holds at cold",
  "  start; RANKING does not. ECR will out-rank the model here. Treat a big",
  "  vs_ecr as 'spend thirty seconds thinking about this guy', never as a",
  "  recommendation to move him up the board.",
  "",
  "COLUMNS",
  "  exp_startable_wks     sum of weekly P(start) over the player's games",
  "  exp_startable_neutral same, against a league-average opponent",
  "  sched_effect          exp_startable_wks - exp_startable_neutral",
  "  sched_rank            rank within position on sched_effect",
  "  p_neutral_start       P(startable) vs an average opponent, one game",
  "  p_start_wk1           Week 1 P(startable) WITH real posted lines",
  "  best/worst_wk_p_start most and least favorable single week",
  "  vs_ecr                ecr_rank - model_rank (positive = model higher)",
  "  dm_*                  ROOKIES ONLY: your draft model's pre-draft",
  "                        verdict. The boxscore engine has nothing for",
  "                        rookies (the rookie-prior rung was a published",
  "                        null), so for rookies read dm_verdict and IGNORE",
  "                        model_rank.",
  "",
  "HOW THE NUMBERS WERE MADE",
  sprintf("  Thresholds (full PPR): RB/WR P(FP>=15) start, >=20 boom;"),
  "  TE >=12 / >=17; QB >=20 / >=25.",
  "  No 2026 games are played, so every player's form features are identical",
  "  in all 18 weeks. Only opponent, Vegas, and the bye move. The board is",
  "  one base slate scored 18 times against 18 opponents.",
  "  Primary pass forces Vegas features NA in ALL weeks. nflverse carries",
  "  2026 lines for weeks 1-3 only; mixing them into a season sum would",
  "  charge a data-availability seam to 'schedule'. p_start_wk1 is the one",
  "  column that does use real lines. See vegas_seam_summary.csv for how",
  "  much Vegas moves a week-1 number.",
  "",
  "KNOWN LIMITATIONS",
  "  - Cold-start ranking is weak (see above). This is the big one.",
  "  - LEVELS ARE COMPRESSED ~3x. exp_startable_wks is NOT a forecast of",
  "    how many startable weeks a player will actually have. Measured on",
  "    2023-2025 RBs with 8+ games, the real per-player startable rate runs",
  "    median 0.29, p95 0.70, max 0.88. This board's p_neutral_start runs",
  "    median 0.17, p95 0.30, max 0.38 -- the single most confident RB on",
  "    the board sits BELOW the 75th percentile (0.47) of real outcomes.",
  "    Spread from median to p95 is 0.13 here versus 0.41 in reality. So a",
  "    real RB1 is worth ~12 startable weeks and this board caps near 6.4.",
  "    Compare players to each other, never read the number as a forecast.",
  "  - Star shrinkage is the mechanism above: the engine pulls the top of",
  "    each position toward the mean, which is exactly where a draft board",
  "    needs resolution.",
  "  - Rookies: no prior baseline. dm_* is the only real rookie signal.",
  "  - Roster state is as of the run date. Before final cuts the late-round",
  "    backup tier is unreliable; re-run the morning of the draft.",
  "  - Defense adjustments are prior-season carryover (no 2026 games), so",
  "    sched_effect assumes 2025 defensive quality persists.",
  "  - Weather is not modeled (published null) and is absent this far out.",
  "",
  "FILES",
  "  {rb,wr,te,qb}_board.csv   per-position boards, sorted by ECR",
  "  schedule_grid.csv         per-player, per-week P(start)",
  "  vegas_seam_summary.csv    blind vs Vegas-informed gap, weeks 1-3",
  "  changes_vs_prior_run.csv  movement since the last run (if any)",
  "  raw/                      slates and scored output, swept from output/",
  "",
  "Rookie draft-model verdicts via the 10e0 crosswalk. ECR: FantasyPros",
  "(personal, non-commercial tier)."
)
writeLines(readme, file.path(OUT_DIR, "README.txt"))
cli_alert_success("README.txt")

sweep_artifacts()

leftover <- created_stems(SEASON)
leftover <- leftover[file.exists(leftover)]
if (length(leftover)) {
  cli_abort("Sweep incomplete -- {length(leftover)} artifact(s) still in output/: {paste(head(leftover,3), collapse=', ')}")
}
cli_alert_success("output/ verified clean of draft-prep artifacts")

cli_h1("Draft board complete -- {OUT_DIR}")
