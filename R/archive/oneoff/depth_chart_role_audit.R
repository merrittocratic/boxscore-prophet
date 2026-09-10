# Read-only audit: does the QB role-signal blind spot (fixed 2026-09-10,
# R/10c_weekly_score.R commit 48817ba) also affect WR/TE? RB is explicitly
# OUT of scope -- see the plan this script implements
# (~/.claude/plans/unified-painting-gosling.md) for why.
#
# BACKGROUND: at a player's season debut, in-season rolling volume features
# (wt_target_share, wt_snap_share, ...) are NA -- confirmed 100% NA for
# every WR/TE row at Week 1 2026. The 2026-08-31 WR Cold-Start Volume Gap
# fix added a baseline_* fallback ladder (own prior-season share -> draft-
# tier median -> position median), which QB's db_vol component never got --
# so this is NOT a repeat of the QB investigation, it's a smaller-but-real
# residual. Two distinct populations survive that fix:
#   (a) cold-start players (is_cold_start==TRUE): fall to a FLAT draft-tier
#       constant with zero player-specific role information. A rookie
#       starter and a rookie WR5 with the same draft tier get the identical
#       number.
#   (b) non-cold-start role-changers (is_cold_start==FALSE): carry forward
#       their OWN prior-season share, which is actively wrong (not just
#       missing) if their role changed -- a cold-start-only gate (like the
#       QB fix) would miss this population entirely.
#
# This script flags both populations for WR and TE, against the CURRENT
# official depth chart (via the new load_current_depth_chart() helper in
# R/10b_roster_helpers.R -- built here because the QB fix's per-player-
# latest-snapshot approach is unsafe for WR/TE's multi-lane structure, see
# that helper's own header comment). Writes a report only -- makes ZERO
# changes to R/10c_weekly_score.R or any live scoring output.
#
# Usage: Rscript R/archive/oneoff/depth_chart_role_audit.R [season] [week]
# Output: output/10c_depthchart_audit_<season>_w<week>.csv

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(cli)
})

source("R/10b_roster_helpers.R")

args   <- commandArgs(trailingOnly = TRUE)
SEASON <- if (length(args) >= 1) as.integer(args[1]) else 2026L
WEEK   <- if (length(args) >= 2) as.integer(args[2]) else 1L
WTAG   <- sprintf("%d_w%02d", SEASON, WEEK)

cli_h1("Depth-chart role-signal audit -- {SEASON} week {WEEK} (WR/TE only, RB out of scope)")

as_of_env <- Sys.getenv("AS_OF", "")
AS_OF <- if (nzchar(as_of_env)) as.POSIXct(as_of_env, tz = "America/New_York") else Sys.time()

# ---------------------------------------------------------------------------
# 1. Pre-existing manual override hook -- report what it already covers so
#    this audit doesn't re-flag something Steve's already handling by hand.
# ---------------------------------------------------------------------------
OVERRIDES_FILE <- "data/overrides/depth_overrides.csv"
if (file.exists(OVERRIDES_FILE)) {
  ov <- read_csv(OVERRIDES_FILE, show_col_types = FALSE)
  cli_alert_info("{OVERRIDES_FILE}: {nrow(ov)} manual roster override(s) already active -- {paste(ov$player_id, collapse=', ')}")
} else {
  cli_alert_info("{OVERRIDES_FILE} does not exist -- no manual roster overrides active (this hook is roster add/drop only, unrelated to role/volume signal -- confirmed by reading R/10b_roster_helpers.R:135-147)")
}

# ---------------------------------------------------------------------------
# 2. Per-position audit
# ---------------------------------------------------------------------------
audit_position <- function(pos, slate_path, starter_rank_max) {
  cli_h2("{pos}")

  slate <- read_csv(slate_path, show_col_types = FALSE) |>
    select(player_id, player_name, posteam, draft_tier, is_cold_start,
           baseline_target_share, wt_target_share)

  dc <- load_current_depth_chart(SEASON, pos, AS_OF) |>
    rename(player_id = gsis_id) |>
    mutate(starter = pos_rank <= starter_rank_max) |>
    select(player_id, dc_pos_rank = pos_rank, dc_pos_slot = pos_slot, starter)

  scored <- read_csv(sprintf("output/10c_scored_slate_%s.csv", WTAG), show_col_types = FALSE) |>
    filter(position == pos) |>
    select(player_id, p_start_recal)

  ecr <- read_csv(sprintf("output/10d_ecr_gap_%s.csv", WTAG), show_col_types = FALSE) |>
    filter(position == pos) |>
    select(player_name, posteam, model_rank, ecr_rank, rank_gap)

  d <- slate |>
    inner_join(dc, by = "player_id") |>
    left_join(scored, by = "player_id") |>
    left_join(ecr, by = c("player_name", "posteam"))

  n_no_dc <- nrow(slate) - nrow(semi_join(slate, dc, by = "player_id"))
  if (n_no_dc > 0) {
    cli_alert_info("{n_no_dc} {pos} slate player(s) had no current depth-chart row -- excluded from this audit, not flagged")
  }

  # Reference populations: non-cold-start players whose OWN carried-forward
  # share is presumably trustworthy (they have real recent role history AND
  # the depth chart independently confirms their current bucket). These
  # anchor both the floor (for under-ranked starters) and the ceiling (for
  # over-ranked backups) -- same "median of confirmed peers" pattern as the
  # QB fix, not an arbitrary boost (depth-chart rank is ordinal, not a
  # workload magnitude -- see the plan).
  starter_pool <- d$baseline_target_share[!d$is_cold_start & d$starter]
  ref_starter    <- median(starter_pool, na.rm = TRUE)
  ref_starter_q30 <- quantile(starter_pool, 0.30, na.rm = TRUE)
  ref_backup  <- median(d$baseline_target_share[!d$is_cold_start & !d$starter], na.rm = TRUE)
  cli_alert_info("Reference: confirmed non-cold-start starter median share = {round(ref_starter,3)} (n={sum(!d$is_cold_start & d$starter, na.rm=TRUE)}, 30th pctile = {round(ref_starter_q30,3)}) | confirmed non-cold-start backup median share = {round(ref_backup,3)} (n={sum(!d$is_cold_start & !d$starter, na.rm=TRUE)})")

  # stale_baseline_starter threshold: below the 30th percentile of OTHER
  # confirmed starters' own share, not merely "as low as a typical backup"
  # (i.e. below ref_backup) -- that stricter comparison missed real cases
  # found in the live-severity investigation (Malik Washington 0.125,
  # Gunnar Helm 0.093, Chig Okonkwo 0.126 all sit above their position's
  # backup median but are still unusually low for a CONFIRMED starter, and
  # all three carry a real, large ECR disagreement). 30th percentile is a
  # judgment call, not a bright line -- reviewed by Steve before anything
  # here becomes a live correction, so erring toward catching more real
  # candidates rather than under-flagging.
  flagged <- d |>
    mutate(
      flag_reason = case_when(
        is_cold_start  & starter  ~ "cold_start_starter",
        is_cold_start  & !starter ~ "cold_start_backup",
        !is_cold_start & starter  & baseline_target_share < ref_starter_q30 ~ "stale_baseline_starter",
        TRUE ~ NA_character_
      ),
      proposed_share = case_when(
        flag_reason %in% c("cold_start_starter", "stale_baseline_starter") ~ pmax(baseline_target_share, ref_starter),
        flag_reason == "cold_start_backup" & baseline_target_share > ref_backup ~ pmin(baseline_target_share, ref_backup),
        TRUE ~ NA_real_
      )
    ) |>
    filter(!is.na(flag_reason), !is.na(proposed_share)) |>
    mutate(position = pos) |>
    select(position, player_name, posteam, draft_tier, dc_pos_rank, dc_pos_slot,
           is_cold_start, flag_reason, baseline_target_share, proposed_share,
           model_rank, ecr_rank, rank_gap, p_start_recal) |>
    arrange(flag_reason, baseline_target_share)

  cli_alert_success("{pos}: {nrow(flagged)} flagged ({sum(flagged$flag_reason %in% c('cold_start_starter','stale_baseline_starter'))} under-ranked candidates, {sum(flagged$flag_reason=='cold_start_backup')} over-ranked candidates)")
  flagged
}

wr_flags <- audit_position("WR", sprintf("output/10b3_wr_slate_%s.csv", WTAG), starter_rank_max = 3L)
te_flags <- audit_position("TE", sprintf("output/10b5_te_slate_%s.csv", WTAG), starter_rank_max = 1L)

# ---------------------------------------------------------------------------
# 3. RB sanity check -- confirm it's genuinely out of scope this week, not
#    just unexamined. Flags using the SAME logic as WR/TE for a direct,
#    apples-to-apples comparison (not a separate methodology), reported but
#    NOT included in the written audit CSV -- RB was scoped out for a
#    different reason (real error is over-ranked veteran RB3/4 handcuffs,
#    not this blind spot), not because this check would find zero.
# ---------------------------------------------------------------------------
rb_flags <- audit_position("RB", sprintf("output/10b2_rb_slate_%s.csv", WTAG), starter_rank_max = 1L)
cli_alert_info("RB: {nrow(rb_flags)} flagged by this SAME mechanism -- reported for completeness, intentionally excluded from the written audit (see plan for why RB needs a different fix, not this one)")

# ---------------------------------------------------------------------------
# 4. Write report (WR/TE only)
# ---------------------------------------------------------------------------
out_path <- sprintf("output/10c_depthchart_audit_%s.csv", WTAG)
all_flags <- bind_rows(wr_flags, te_flags)
write_csv(all_flags, out_path)
cli_alert_success("{out_path} ({nrow(all_flags)} total flagged rows, WR+TE)")
cli_h1("Audit complete -- report only, no scoring changes made")
