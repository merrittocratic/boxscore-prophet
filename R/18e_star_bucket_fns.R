# R/18e_star_bucket_fns.R
# Shared core for the RB trailing-FP star buckets (D27 star_platt ship).
# Sourced by BOTH:
#   - R/18e_rb_star_deploy_maps.R (deployment map fit -> fp_recal_maps.rds)
#   - R/10c_weekly_score.R        (ex-ante slate-week computation)
# One implementation, two call sites -- the 10c gate proves they agree by
# bucket equality against the 18d validation file on hindcast weeks
# (11b pattern).
#
# Definition (identical to 18c/18d, the validated construction):
#   trail_fp = PPR FP per game over the last 17 REG games PLAYED,
#              strictly before (season, week), cross-season; requires
#              >= 6 prior games, else no rank (bucket b3).
#   rank     = within the supplied universe of rows per (season, week),
#              descending trail_fp, ties.method = "first" -- only rows
#              with a valid trail_fp receive ranks (placeholder -Inf
#              rows sort last and their ranks are discarded as NA).
#   bucket   = b1 (rank 1-12), b2 (13-24), b3 (rest / no history).
# The universe is whatever rows are passed in: the 06c cal-fold rows at
# fit time, the week's slate rows at score time. Ex-ante by
# construction -- only completed games enter trail_fp.

star_trailing_fp <- function(seasons) {
  # A season with no released stats yet (scoring Week 1 of a new season)
  # loads as an error from nflreadr -- skip it with a warning; trailing
  # FP then comes from prior seasons, which is correct pre-W1 behavior.
  # Stay LOUD on real data loss: the season before the newest requested
  # one must load, else abort (found by the 2026 W1 stage run 2026-09-05;
  # hindcast weeks cannot catch this).
  loaded <- lapply(seasons, function(s) {
    tryCatch(nflreadr::load_player_stats(s),
             error = function(e) { warning(sprintf(
               "star_trailing_fp: season %d stats unavailable (%s) -- skipped",
               s, conditionMessage(e)), call. = FALSE); NULL })
  })
  loaded <- Filter(Negate(is.null), loaded)
  got <- vapply(loaded, function(d) d$season[1], numeric(1))
  if (length(loaded) == 0 || !(max(seasons) - 1) %in% got)
    stop(sprintf("star_trailing_fp: required history missing (loaded: %s)",
                 paste(got, collapse = ",")))
  # State AFTER each played game (last-17 mean including that game).
  # star_assign_buckets rolls the most recent pre-week state forward, so
  # a query week needs no log row of its own -- required for live weeks
  # (no stats row exists before kickoff; found by the 2026 W1 stage run:
  # the old exact-week join sent the entire live slate to b3). For a row
  # whose player DID play that week, the most recent prior state equals
  # the old strictly-before-this-week definition exactly, so fit-side
  # buckets are unchanged (re-proven by the 18e equality gate).
  dplyr::bind_rows(loaded) |>
    dplyr::filter(season_type == "REG", !is.na(player_id),
                  !is.na(fantasy_points_ppr)) |>
    dplyr::select(player_id, season, week, fantasy_points_ppr) |>
    dplyr::arrange(player_id, season, week) |>
    dplyr::group_by(player_id) |>
    dplyr::mutate(
      g = dplyr::row_number(),
      cum = cumsum(fantasy_points_ppr),
      games_after = g,
      trail_n_after = pmin(g, 17L),
      trail_fp_after = (cum - dplyr::lag(cum, 17, default = 0)) /
        trail_n_after,
      sw = season * 100L + week
    ) |>
    dplyr::ungroup() |>
    dplyr::select(player_id, sw, trail_fp_after, games_after)
}

# rows: any frame with player_id, season, week (one row per player-week
# within the universe). Returns rows + bucket (character: b1/b2/b3).
star_assign_buckets <- function(rows, trailing) {
  rows$..rid <- seq_len(nrow(rows))
  state <- rows |>
    dplyr::select(..rid, player_id, season, week) |>
    dplyr::mutate(q_sw = season * 100L + week) |>
    dplyr::inner_join(trailing, by = "player_id",
                      relationship = "many-to-many") |>
    dplyr::filter(sw < q_sw) |>
    dplyr::group_by(..rid) |>
    dplyr::slice_max(sw, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(..rid, trail_fp = trail_fp_after, trail_n = games_after)
  rows |>
    dplyr::left_join(state, by = "..rid") |>
    dplyr::mutate(trail_n = pmin(trail_n, 17L)) |>
    dplyr::group_by(season, week) |>
    dplyr::mutate(
      trail_rank = ifelse(
        !is.na(trail_fp) & trail_n >= 6,
        rank(-ifelse(!is.na(trail_fp) & trail_n >= 6, trail_fp, -Inf),
             ties.method = "first"),
        NA_integer_),
      star_bucket = dplyr::case_when(
        !is.na(trail_rank) & trail_rank <= 12 ~ "b1",
        !is.na(trail_rank) & trail_rank <= 24 ~ "b2",
        .default = "b3"
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-trail_fp, -trail_n, -trail_rank, -..rid)
}

# Self-contained star_platt closure: coefficients captured by value, no
# model object in the environment (09b deployment-closure convention).
# Signature extends the uniform map(p, vol, spread, implied) with a
# fifth bucket argument; consumers check the needs_bucket flag.
star_platt_closure <- function(coefs) {
  cf <- function(nm) {
    v <- unname(coefs[nm])
    if (length(v) == 0 || is.na(v)) 0 else v
  }
  a <- cf("(Intercept)")
  b <- cf("lp")
  d_b1 <- cf("bucketb1")
  d_b2 <- cf("bucketb2")
  force(a); force(b); force(d_b1); force(d_b2)
  function(pnew, vnew, spnew, itnew, bucket) {
    lp <- qlogis(pmin(pmax(pnew, 0.001), 0.999))
    shift <- ifelse(bucket == "b1", d_b1, ifelse(bucket == "b2", d_b2, 0))
    plogis(a + b * lp + shift)
  }
}

fit_star_platt_map <- function(p, hit, bucket) {
  df <- data.frame(hit = hit,
                   lp = qlogis(pmin(pmax(p, 0.001), 0.999)),
                   bucket = factor(bucket, levels = c("b3", "b1", "b2")))
  fit <- glm(hit ~ lp + bucket, data = df, family = binomial())
  list(coefs = coef(fit), map = star_platt_closure(coef(fit)))
}
