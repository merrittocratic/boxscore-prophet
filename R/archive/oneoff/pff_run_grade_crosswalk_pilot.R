# pff_run_grade_crosswalk_pilot.R -- can public nflverse/NGS fields predict
# PFF's grades_run well enough to stand in for it in published content?
# One-off pilot, not wired into any pipeline. Run from repo root:
#   Rscript R/archive/oneoff/pff_run_grade_crosswalk_pilot.R
#
# PFF data (data/vendor_raw/pff/rushing_summary/) used here ONLY to calibrate
# and validate the proxy -- per Steve's 2026-09-16 call, same accepted-risk
# category as the D29 PFF training arm. Nothing PFF-sourced is published;
# the deliverable is the fit quality of a public-only formula.

suppressMessages({
  library(dplyr)
  library(jsonlite)
  library(ggplot2)
  library(nflreadr)
})

out_dir <- "output/oneoff/pff_run_grade_pilot"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

## ---- load + stack PFF rushing_summary weekly files (2016-2024) ------------

pff_files <- list.files("data/vendor_raw/pff/rushing_summary", full.names = TRUE)
pff_files <- pff_files[grepl("^20(1[6-9]|2[0-4])_[0-9]{2}\\.json$", basename(pff_files))]

read_pff_week <- function(path) {
  fname <- basename(path)
  parts <- regmatches(fname, regexec("^([0-9]{4})_([0-9]{2})\\.json$", fname))[[1]]
  season <- as.integer(parts[2])
  week   <- as.integer(parts[3])
  raw <- fromJSON(path)$rushing_summary
  if (is.null(raw) || nrow(raw) == 0) return(NULL)
  raw$season <- season
  raw$week   <- week
  raw |> select(season, week, player, team, position, attempts,
                 grades_run, grades_offense)
}

pff <- bind_rows(lapply(pff_files, read_pff_week))

norm_name <- function(x) {
  x |> tolower() |>
    gsub("[.']", "", x = _) |>
    gsub("\\s+(jr|sr|ii|iii|iv)$", "", x = _) |>
    trimws()
}

pff <- pff |>
  filter(position %in% c("HB", "FB"), attempts >= 6) |>
  mutate(norm_name = norm_name(player))

cat("PFF rushing_summary rows (RB/FB, 6+ att):", nrow(pff), "\n")

## ---- nflverse: official stats + NGS rushing + play-level detail ------------

seasons <- sort(unique(pff$season))

stats <- load_player_stats(seasons) |>
  filter(position %in% c("RB", "FB"), week >= 1) |>
  transmute(season, week, player_id, norm_name = norm_name(player_display_name),
            carries, rushing_yards, rushing_tds, rushing_epa,
            rushing_first_downs, rushing_fumbles_lost, rushing_10)

ngs <- load_nextgen_stats(stat_type = "rushing", seasons = seasons) |>
  filter(week >= 1) |>
  transmute(season, week, player_id = player_gsis_id,
            efficiency, pct_attempts_gte_8def = percent_attempts_gte_eight_defenders,
            avg_time_to_los, rush_yards_over_expected, rush_yards_over_expected_per_att,
            rush_pct_over_expected)

## per-player-week success/stuff rate + the defense faced, straight from PBP
pbp_rush <- load_pbp(seasons) |>
  filter(week >= 1, rush == 1, !is.na(rusher_player_id))

play_level <- pbp_rush |>
  group_by(season, week, player_id = rusher_player_id) |>
  summarise(success_rate = mean(success, na.rm = TRUE),
            stuff_rate = mean(yards_gained <= 0, na.rm = TRUE),
            defteam = dplyr::first(defteam),
            .groups = "drop")

## team-season run-defense strength (crude full-season average -- pilot only,
## not out-of-sample; fine for "does this feature carry signal" purposes)
def_strength <- pbp_rush |>
  group_by(season, defteam) |>
  summarise(def_rush_epa_allowed = mean(epa, na.rm = TRUE), .groups = "drop")

nflv <- stats |>
  inner_join(ngs, by = c("season", "week", "player_id")) |>
  inner_join(play_level, by = c("season", "week", "player_id")) |>
  left_join(def_strength, by = c("season", "defteam")) |>
  mutate(first_down_rate = rushing_first_downs / pmax(carries, 1),
         fumble_rate = rushing_fumbles_lost / pmax(carries, 1),
         explosive_rate = rushing_10 / pmax(carries, 1))

## ---- join PFF to nflverse ---------------------------------------------------

merged <- pff |> inner_join(nflv, by = c("season", "week", "norm_name"))

match_rate <- round(100 * nrow(merged) / nrow(pff), 1)
cat("Matched", nrow(merged), "of", nrow(pff), "PFF player-weeks (", match_rate, "% ) to nflverse+NGS\n")

## ---- single-variable correlations with grades_run --------------------------

candidates <- c("carries", "rushing_yards", "rushing_tds", "rushing_epa",
                 "efficiency", "pct_attempts_gte_8def", "avg_time_to_los",
                 "rush_yards_over_expected", "rush_yards_over_expected_per_att",
                 "rush_pct_over_expected", "success_rate", "stuff_rate",
                 "first_down_rate", "fumble_rate", "explosive_rate",
                 "def_rush_epa_allowed")

cor_tbl <- lapply(candidates, function(v) {
  ok <- complete.cases(merged[[v]], merged$grades_run)
  data.frame(metric = v,
             r = round(cor(merged[[v]][ok], merged$grades_run[ok]), 3),
             n = sum(ok))
}) |> bind_rows() |> arrange(desc(abs(r)))

cat("\n=== single-metric correlation with PFF grades_run ===\n")
print(cor_tbl, row.names = FALSE)
write.csv(cor_tbl, file.path(out_dir, "pff_run_grade_correlations.csv"), row.names = FALSE)

## ---- combined public-data proxy model --------------------------------------

model_formula <- grades_run ~ rushing_epa + rush_yards_over_expected_per_att +
  rush_pct_over_expected + efficiency + avg_time_to_los + pct_attempts_gte_8def +
  success_rate + stuff_rate + first_down_rate + fumble_rate + explosive_rate +
  def_rush_epa_allowed

fit <- lm(model_formula, data = merged)

r2 <- round(summary(fit)$r.squared, 3)
cat("\nCombined public-data proxy model R-squared vs PFF grades_run (6+ att):", r2, "\n")
cat("\n=== coefficients ===\n")
print(round(coef(summary(fit))[, c("Estimate", "Pr(>|t|)")], 4))

merged$proxy_fitted <- predict(fit, newdata = merged)

## ---- does R^2 rise for featured (higher-carry) backs? ----------------------

thresholds <- c(6, 10, 12, 15, 18, 20)
threshold_tbl <- lapply(thresholds, function(th) {
  sub <- merged |> filter(attempts >= th)
  if (nrow(sub) < 30) return(NULL)
  m <- lm(model_formula, data = sub)
  data.frame(min_attempts = th, n = nrow(sub), r2 = round(summary(m)$r.squared, 3))
}) |> bind_rows()

cat("\n=== R^2 by minimum carries-in-game (does volume tighten the fit?) ===\n")
print(threshold_tbl, row.names = FALSE)
write.csv(threshold_tbl, file.path(out_dir, "pff_run_grade_r2_by_carries.csv"), row.names = FALSE)

## ---- chart: composite proxy vs actual PFF grade ----------------------------

surface <- "#fcfcfb"; ink <- "#0b0b0b"; ink2 <- "#52514e"
blue_lt <- "#86b6ef"; blue_dk <- "#2a78d6"

theme_pilot <- theme_minimal(base_size = 13) +
  theme(
    plot.background  = element_rect(fill = surface, color = NA),
    panel.background = element_rect(fill = surface, color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#e8e7e3", linewidth = 0.3),
    plot.title.position = "plot",
    plot.title    = element_text(color = ink, face = "bold", size = 14),
    plot.subtitle = element_text(color = ink2, size = 10.5),
    plot.caption  = element_text(color = ink2, size = 8.5),
    axis.text  = element_text(color = ink2),
    axis.title = element_text(color = ink2, size = 10)
  )

p <- ggplot(merged, aes(x = proxy_fitted, y = grades_run)) +
  geom_point(color = blue_lt, alpha = 0.35, size = 1.6) +
  geom_smooth(method = "lm", color = blue_dk, se = FALSE, linewidth = 1) +
  labs(
    title = "Can public data recreate PFF's run grade?",
    subtitle = paste0("RB/FB weeks, 6+ carries, ", min(seasons), "-", max(seasons),
                       " | n = ", nrow(merged), " | R2 = ", r2),
    x = "Proxy score (fit from nflverse + NGS + PBP rushing fields only)",
    y = "PFF grades_run (actual)",
    caption = "PFF data used for validation only, not published. Proxy: EPA, rush yards over expected,\nefficiency, time to LOS, box count, success/stuff rate, first-down rate, fumble rate,\nexplosive rate, opponent run-D strength."
  ) +
  theme_pilot

ggsave(file.path(out_dir, "pff_run_grade_fit.png"), p,
       width = 7.5, height = 6, dpi = 150, bg = surface)

## same chart, restricted to the best-fitting featured-back cut
featured_th <- threshold_tbl$min_attempts[which.max(threshold_tbl$r2)]
featured <- merged |> filter(attempts >= featured_th)
fit_featured <- lm(model_formula, data = featured)
r2_featured <- round(summary(fit_featured)$r.squared, 3)
featured$proxy_fitted <- predict(fit_featured, newdata = featured)

p2 <- ggplot(featured, aes(x = proxy_fitted, y = grades_run)) +
  geom_point(color = blue_lt, alpha = 0.45, size = 1.8) +
  geom_smooth(method = "lm", color = blue_dk, se = FALSE, linewidth = 1) +
  labs(
    title = "Same proxy, featured backs only",
    subtitle = paste0(featured_th, "+ carries in-game | n = ", nrow(featured), " | R2 = ", r2_featured,
                       "  (vs R2 = ", r2, " at 6+ carries)"),
    x = "Proxy score (fit from nflverse + NGS + PBP rushing fields only)",
    y = "PFF grades_run (actual)",
    caption = "Model refit on this subset -- not the 6+ carry model re-scored on fewer rows."
  ) +
  theme_pilot

ggsave(file.path(out_dir, "pff_run_grade_fit_featured.png"), p2,
       width = 7.5, height = 6, dpi = 150, bg = surface)

cat("\nWrote:", file.path(out_dir, "pff_run_grade_correlations.csv"), "\n")
cat("Wrote:", file.path(out_dir, "pff_run_grade_r2_by_carries.csv"), "\n")
cat("Wrote:", file.path(out_dir, "pff_run_grade_fit.png"), "\n")
cat("Wrote:", file.path(out_dir, "pff_run_grade_fit_featured.png"), "\n")

## ---- editorial nugget: score this week's storyline RBs ---------------------
## Directional only -- percentile within the historical proxy distribution,
## never a manufactured "grade" number. No PFF data touched for these players.

target_teams <- c("KC", "NYG", "CIN", "LAC")
opp_2025_lookup <- c(KC = "DEN", NYG = "DAL", CIN = "TB", LAC = "ARI")

wk1_stats <- load_player_stats(2026) |>
  filter(week == 1, position == "RB", team %in% target_teams) |>
  group_by(team) |>
  slice_max(carries, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(team, player_display_name, player_id,
            carries, rushing_yards, rushing_epa, rushing_first_downs,
            rushing_fumbles_lost, rushing_10)

wk1_ngs <- load_nextgen_stats(stat_type = "rushing", seasons = 2026) |>
  filter(week == 1) |>
  transmute(player_id = player_gsis_id, efficiency,
            pct_attempts_gte_8def = percent_attempts_gte_eight_defenders,
            avg_time_to_los, rush_yards_over_expected_per_att, rush_pct_over_expected)

wk1_pbp <- load_pbp(2026) |>
  filter(week == 1, rush == 1, !is.na(rusher_player_id))

wk1_play <- wk1_pbp |>
  group_by(player_id = rusher_player_id) |>
  summarise(success_rate = mean(success, na.rm = TRUE),
            stuff_rate = mean(yards_gained <= 0, na.rm = TRUE), .groups = "drop")

def_2025 <- load_pbp(2025) |>
  filter(week >= 1, rush == 1, defteam %in% opp_2025_lookup) |>
  group_by(defteam) |>
  summarise(def_rush_epa_allowed = mean(epa, na.rm = TRUE), .groups = "drop")

wk1 <- wk1_stats |>
  inner_join(wk1_ngs, by = "player_id") |>
  inner_join(wk1_play, by = "player_id") |>
  mutate(defteam = opp_2025_lookup[team]) |>
  left_join(def_2025, by = "defteam") |>
  mutate(first_down_rate = rushing_first_downs / pmax(carries, 1),
         fumble_rate = rushing_fumbles_lost / pmax(carries, 1),
         explosive_rate = rushing_10 / pmax(carries, 1))

wk1$proxy_fitted <- predict(fit, newdata = wk1)
wk1$percentile <- round(100 * sapply(wk1$proxy_fitted, function(x) {
  mean(merged$proxy_fitted <= x, na.rm = TRUE)
}))

nugget_tbl <- wk1 |>
  select(team, player_display_name, carries, rushing_yards, rushing_epa,
         proxy_fitted, percentile) |>
  arrange(desc(percentile))

cat("\n=== W1 2026 storyline RBs: public-data proxy percentile (directional only) ===\n")
print(as.data.frame(nugget_tbl))
write.csv(nugget_tbl, file.path(out_dir, "w1_2026_storyline_rb_proxy.csv"), row.names = FALSE)
