# R/13d0_opener_lines.R
# Rung 2, opener variant: build point-in-time-honest Vegas lines from the
# aussportsbetting historical file (OPENING spread + total, posted Sunday
# night / Monday for the following week -- available at Tuesday slate build
# and at Friday lock, so unambiguously reconstructable, unlike closers).
#
# Source: data/vegas/nfl_odds_aussports.xlsx (fetched via Wayback snapshot
# 2025-08-27 of aussportsbetting.com/historical_data/nfl.xlsx; the live
# site is Cloudflare-gated). Third-party data, gitignored like data/ecr.
# Coverage: 2006-09-07 .. 2025-02-09 => complete 2014-2024 seasons; the
# 2025 season has NO openers in this file (rows carry NA; LightGBM handles
# NA natively and the A/B comparison reports the coverage split).
#
# JOIN + VALIDATION (all gates abort on failure):
#   - Odds file keys games by (date, era-specific full team names); map
#     nickname -> franchise abbr with relocation-era logic, join on
#     (gameday, home_abbr) to nflverse schedules.
#   - GATE 1: >= 99% of 2014-2024 REG games matched.
#   - GATE 2 (join fingerprint): the file's CLOSING spread must reproduce
#     nflverse spread_line (r >= 0.98) -- a wrong join scrambles this.
#   - Sign convention verified against game results (slope ~ +1).
#
# Output: data/vegas_open_lines.rds keyed (game_id, posteam) with
# team_spread + implied_total built from OPENERS (same column names the
# 13b/13c harnesses expect, so the A/B re-runs via VEGAS_LINES_RDS env).

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(readxl)
  library(cli)
})

SEASONS <- 2014L:2025L
XLSX    <- "data/vegas/nfl_odds_aussports.xlsx"

cli_h1("13d0: opener lines from aussportsbetting archive")

odds <- read_excel(XLSX) |>
  transmute(
    gameday    = as.Date(Date),
    home_name  = `Home Team`,
    away_name  = `Away Team`,
    home_line_open  = as.numeric(`Home Line Open`),
    home_line_close = as.numeric(`Home Line Close`),
    total_open      = as.numeric(`Total Score Open`),
    total_close     = as.numeric(`Total Score Close`),
    playoff    = `Playoff Game?`
  ) |>
  filter(is.na(playoff) | playoff != "Y")

cli_alert_success("Odds rows: {nrow(odds)} | {min(odds$gameday)} .. {max(odds$gameday)}")

# Nickname -> era abbr (nflverse schedules use era codes: STL/SD/OAK)
nickname_of <- function(name) {
  case_when(
    str_starts(name, "Washington") ~ "washington",
    TRUE ~ str_to_lower(word(name, -1))
  )
}
abbr_of <- function(nick, season) {
  case_when(
    nick == "cardinals"  ~ "ARI", nick == "falcons"    ~ "ATL",
    nick == "ravens"     ~ "BAL", nick == "bills"      ~ "BUF",
    nick == "panthers"   ~ "CAR", nick == "bears"      ~ "CHI",
    nick == "bengals"    ~ "CIN", nick == "browns"     ~ "CLE",
    nick == "cowboys"    ~ "DAL", nick == "broncos"    ~ "DEN",
    nick == "lions"      ~ "DET", nick == "packers"    ~ "GB",
    nick == "texans"     ~ "HOU", nick == "colts"      ~ "IND",
    nick == "jaguars"    ~ "JAX", nick == "chiefs"     ~ "KC",
    nick == "dolphins"   ~ "MIA", nick == "vikings"    ~ "MIN",
    nick == "patriots"   ~ "NE",  nick == "saints"     ~ "NO",
    nick == "giants"     ~ "NYG", nick == "jets"       ~ "NYJ",
    nick == "eagles"     ~ "PHI", nick == "steelers"   ~ "PIT",
    nick == "49ers"      ~ "SF",  nick == "seahawks"   ~ "SEA",
    nick == "buccaneers" ~ "TB",  nick == "titans"     ~ "TEN",
    nick == "washington" ~ "WAS",
    nick == "rams"       ~ if_else(season <= 2015L, "STL", "LA"),
    nick == "chargers"   ~ if_else(season <= 2016L, "SD",  "LAC"),
    nick == "raiders"    ~ if_else(season <= 2019L, "OAK", "LV"),
    TRUE ~ NA_character_
  )
}

sched <- nflreadr::load_schedules(SEASONS) |>
  filter(game_type == "REG") |>
  select(game_id, season, week, gameday, home_team, away_team,
         spread_line, total_line, result) |>
  mutate(gameday = as.Date(gameday))

odds_keyed <- odds |>
  mutate(nick = nickname_of(home_name)) |>
  # season for era mapping: games Jan/Feb belong to the prior season
  mutate(season_est = if_else(month(gameday) <= 3, year(gameday) - 1L, year(gameday)),
         home_abbr = abbr_of(nick, season_est)) |>
  filter(!is.na(home_abbr))

joined <- sched |>
  left_join(odds_keyed |>
              select(gameday, home_abbr, home_line_open, home_line_close,
                     total_open, total_close),
            by = c("gameday", "home_team" = "home_abbr"))

# GATE 1: match rate on covered seasons (2014-2024)
cov <- joined |> filter(season <= 2024L)
match_rate <- mean(!is.na(cov$home_line_open))
cli_alert_info("Match rate 2014-2024: {round(100 * match_rate, 2)}% ({sum(is.na(cov$home_line_open))} unmatched of {nrow(cov)})")
if (match_rate < 0.99) {
  print(cov |> filter(is.na(home_line_open)) |> count(season) |> as.data.frame())
  cli_abort("GATE 1 FAILED: match rate below 99%")
}
n_2025 <- joined |> filter(season == 2025L) |> summarise(m = mean(!is.na(home_line_open))) |> pull(m)
cli_alert_info("2025 season opener coverage: {round(100 * n_2025, 1)}% (expected ~0 -- snapshot predates the season)")

# GATE 2: closer fingerprint. aussportsbetting home line is the handicap
# APPLIED to the home team (favorite negative); nflverse spread_line is
# positive when home is favored => spread_line ~ -home_line_close.
fp <- cov |> filter(!is.na(home_line_close), !is.na(spread_line))
r_spread <- cor(fp$spread_line, -fp$home_line_close)
r_total  <- cor(fp$total_line, fp$total_close, use = "complete.obs")
cli_alert_info("Closer fingerprint: cor(spread_line, -home_line_close) = {round(r_spread, 4)} | cor(total) = {round(r_total, 4)}")
if (r_spread < 0.98 || r_total < 0.98) cli_abort("GATE 2 FAILED: join fingerprint mismatch")

# Sign convention direct check: home margin ~ -home_line_open slope ~ +1
sl <- coef(lm(result ~ I(-home_line_open), data = fp))[[2]]
cli_alert_info("Sign check: home margin ~ -home_line_open slope = {round(sl, 3)} (want ~ +1)")
if (!is.finite(sl) || sl < 0.5) cli_abort("Opener sign convention failed")

# Opener-vs-closer movement stats (how stale is the opener?)
mv <- fp |> summarise(
  spread_move_sd = sd(home_line_close - home_line_open),
  total_move_sd  = sd(total_close - total_open),
  spread_move_p90 = quantile(abs(home_line_close - home_line_open), 0.9),
  total_move_p90  = quantile(abs(total_close - total_open), 0.9)
)
cli_alert_info("Open->close movement: spread sd={round(mv$spread_move_sd, 2)} p90={mv$spread_move_p90} | total sd={round(mv$total_move_sd, 2)} p90={mv$total_move_p90}")

# Build team-level opener lines (same schema as the closer path in 13b/13c)
team_lines_open <- bind_rows(
  joined |> transmute(game_id, posteam = home_team,
                      team_spread = -home_line_open, total_line = total_open),
  joined |> transmute(game_id, posteam = away_team,
                      team_spread =  home_line_open, total_line = total_open)
) |>
  mutate(implied_total = (total_line + team_spread) / 2) |>
  select(game_id, posteam, team_spread, implied_total)

saveRDS(team_lines_open, "data/vegas_open_lines.rds")
cli_alert_success("data/vegas_open_lines.rds ({nrow(team_lines_open)} team-game rows, {sum(!is.na(team_lines_open$team_spread))} with openers)")

readr::write_csv(mv |> mutate(match_rate = match_rate, r_spread = r_spread,
                              r_total = r_total, sign_slope = sl),
                 "output/13d0_opener_join_receipt.csv")
cli_alert_success("output/13d0_opener_join_receipt.csv")

cli_h1("13d0 complete")
