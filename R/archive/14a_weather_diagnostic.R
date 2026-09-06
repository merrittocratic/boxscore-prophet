# R/14a_weather_diagnostic.R
# Ablation ladder rung 3, step 0: SIZE the weather signal against the
# SHIPPED (Vegas-era, e5f36be) chain before building any adjustment layer.
#
# THE NULL CHANGED SINCE PRE-REGISTRATION: rung 2 put the market into the
# chain three layers deep, and Vegas totals PRICE weather (wind games open
# lower). So the question is not "does weather matter" but "does the
# kickoff-hour forecast add anything BEYOND what the opener lines already
# said." Residuals measured here are already market-conditioned.
#
# DATA: kickoff-hour HISTORICAL FORECASTS (14a0; as-of-lock discipline,
# never observed weather), 2021-2025 only (archive floor) -- 5 seasons,
# OUTDOOR games only. Both teams in a game share its weather.
#
# CELLS:
#   1. Combined-EPA fold residual (13e canonical arms) by wind / temp /
#      precip bucket, per position. Volume residual alongside.
#   2. Shipped-probability honesty (deployed recal methods) by bucket.
#
# PRE-STATED EXPECTATIONS (2026-07-26, before first run):
#   - MOSTLY ABSORBED: temp and precip cells flat; wind under 15 mph flat.
#   - The one live candidate: 15+ mph wind degrading passing beyond market
#     pricing -- if present, a small negative pass-side residual (QB order
#     1-2 EPA, WR/TE < 0.5) and a few pp of boom overstatement in wind.
#   - Prior verdict: NULL or a thin wind-only adjustment.
#
# PRE-COMMITTED PROCEED RULE (bars wider than rung 2's -- half the seasons,
# outdoor-only, extreme buckets are thin; locked before the run):
#   Rung 3 advances to 14b (a thin POST-PREDICTION adjustment per the
#   pre-registration -- explicitly NOT feature-table surgery) only if
#     (a) any position x bucket with n >= 300 has |mean tot resid| >= 0.8
#         EPA (QB >= 2.0), or
#     (b) any position x threshold x bucket with n >= 400 has
#         |stated - empirical| >= 4pp.
#   Otherwise the null is PUBLISHED and the ladder moves on.
# VALIDITY: weather coverage >= 95% of outdoor 2021-2025 games.

suppressPackageStartupMessages({
  library(tidyverse)
  library(cli)
})

fmt <- function(x, d = 2) sprintf("%+.*f", d, x)

PROCEED_RES_EPA   <- 0.8
PROCEED_RES_QB    <- 2.0
PROCEED_RES_MIN_N <- 300L
PROCEED_CAL_PP    <- 4.0
PROCEED_CAL_MIN_N <- 400L

# ===========================================================================
# 1. WEATHER TABLE + BUCKETS
# ===========================================================================

cli_h1("14a step 1: weather table (outdoor, 2021-2025)")

wx <- readRDS("data/weather_forecast_hist.rds") |>
  mutate(wind_mph = wind_kmh / 1.609,
         gust_mph = gust_kmh / 1.609,
         temp_f   = temp_c * 9 / 5 + 32)

outdoor <- wx |> filter(!is_indoor)
cov <- mean(!is.na(outdoor$wind_mph))
cli_alert_info("Outdoor games: {nrow(outdoor)} | weather coverage {round(100 * cov, 1)}%")
if (cov < 0.95) cli_abort("Coverage below 95% -- rerun 14a0 before the diagnostic.")

wind_bucket <- function(w) cut(w, c(-Inf, 10, 15, Inf),
  labels = c("calm", "breezy", "windy15"))
temp_bucket <- function(t) cut(t, c(-Inf, 25, 40, Inf),
  labels = c("frigid", "cold", "mild"))
precip_bucket <- function(p) factor(if_else(p > 0, "wet", "dry"),
  levels = c("dry", "wet"))

outdoor <- outdoor |>
  mutate(wb = wind_bucket(wind_mph), tb = temp_bucket(temp_f),
         pb = precip_bucket(precip_mm))
cli_h2("Bucket counts (games)")
print(outdoor |> count(wb) |> as.data.frame(), row.names = FALSE)
print(outdoor |> count(tb) |> as.data.frame(), row.names = FALSE)
print(outdoor |> count(pb) |> as.data.frame(), row.names = FALSE)

wx_slim <- outdoor |> select(game_id, wb, tb, pb, wind_mph, temp_f)

# ===========================================================================
# 2. CELL 1: FOLD RESIDUALS BY WEATHER BUCKET (13e canonical arms)
# ===========================================================================

cli_h1("14a step 2: shipped fold residuals by weather bucket")

team_key <- function(table_path) {
  readRDS(table_path) |>
    filter(!is.na(player_id)) |>
    distinct(player_id, season, week, game_id, posteam)
}

POS <- list(
  RB = list(preds = "output/13e_rb_fold_predictions.csv", table = "data/rb_feature_table.rds"),
  WR = list(preds = "output/13e_wr_fold_predictions.csv", table = "data/wr_feature_table.rds"),
  TE = list(preds = "output/13e_te_fold_predictions.csv", table = "data/te_feature_table.rds")
)

resid_rows <- imap(POS, function(cfg, pos) {
  readr::read_csv(cfg$preds, show_col_types = FALSE) |>
    filter(!is.na(player_id), season >= 2021) |>
    transmute(position = pos, player_id, season, week,
              vol_resid = as.numeric(opportunities) - pred_vol,
              tot_resid = total_epa - pred_tot) |>
    inner_join(team_key(cfg$table), by = c("player_id", "season", "week")) |>
    inner_join(wx_slim, by = "game_id")
}) |> list_rbind()

qb_rows <- readr::read_csv("output/13e_qb_fold_predictions.csv", show_col_types = FALSE) |>
  filter(!is.na(player_id), season >= 2021) |>
  transmute(position = "QB", player_id, season, week,
            vol_resid = as.numeric(dropbacks) - pred_db,
            tot_resid = total_epa - pred_tot) |>
  inner_join(team_key("data/qb_feature_table.rds"), by = c("player_id", "season", "week")) |>
  inner_join(wx_slim, by = "game_id")

all_resid <- bind_rows(resid_rows, qb_rows)
cli_alert_success("Joined outdoor player-weeks: {paste(count(all_resid, position)$position, count(all_resid, position)$n, sep='=', collapse=' | ')}")

cell1 <- map(c(wb = "wb", tb = "tb", pb = "pb"), function(bcol) {
  all_resid |>
    group_by(position, bucket = .data[[bcol]]) |>
    summarise(n = n(), mean_tot_resid = mean(tot_resid),
              se = sd(tot_resid) / sqrt(n()),
              mean_vol_resid = mean(vol_resid), .groups = "drop")
}) |> list_rbind(names_to = "axis")

cli_h2("Cell 1: combined-EPA residual by weather bucket")
print(cell1 |>
        mutate(mean_tot_resid = fmt(mean_tot_resid), se = round(se, 2),
               mean_vol_resid = fmt(mean_vol_resid)) |>
        arrange(axis, position, bucket) |>
        as.data.frame(), row.names = FALSE)

# ===========================================================================
# 3. CELL 2: SHIPPED-PROBABILITY HONESTY BY WEATHER BUCKET
# ===========================================================================

cli_h1("14a step 3: shipped probability honesty by weather bucket")

fp_maps <- readRDS("data/fp_recal_maps.rds")
te_maps <- readRDS("data/te_fp_recal_maps.rds")
qb_maps <- readRDS("data/qb_fp_recal_maps.rds")
col_of <- function(stem, method) if (method == "raw") stem else paste0(stem, "_", method)

RECAL <- list(
  RB = list(file = "output/06c_recal_probabilities.csv", filter_pos = "RB",
            table = "data/rb_feature_table.rds",
            start = col_of("p_start", fp_maps[["RB_15+"]]$method),
            boom  = col_of("p_boom",  fp_maps[["RB_20+"]]$method)),
  WR = list(file = "output/06c_recal_probabilities.csv", filter_pos = "WR",
            table = "data/wr_feature_table.rds",
            start = col_of("p_start", fp_maps[["WR_15+"]]$method),
            boom  = col_of("p_boom",  fp_maps[["WR_20+"]]$method)),
  TE = list(file = "output/12e_te_recal_probabilities.csv", filter_pos = "TE",
            table = "data/te_feature_table.rds",
            start = col_of("p_start", te_maps[["TE_12+"]]$method),
            boom  = col_of("p_boom",  te_maps[["TE_17+"]]$method)),
  QB = list(file = "output/09b_qb_recal_probabilities.csv", filter_pos = NA,
            table = "data/qb_feature_table.rds",
            start = col_of("p_start", qb_maps[["QB_20+"]]$method),
            boom  = col_of("p_boom",  qb_maps[["QB_25+"]]$method))
)

cal_rows <- imap(RECAL, function(cfg, pos) {
  df <- readr::read_csv(cfg$file, show_col_types = FALSE)
  if (!is.na(cfg$filter_pos)) df <- df |> filter(position == cfg$filter_pos)
  df |>
    filter(season >= 2021) |>
    transmute(position = pos, player_id, season, week,
              p_start = .data[[cfg$start]], p_boom = .data[[cfg$boom]],
              hit_start, hit_boom) |>
    inner_join(team_key(cfg$table), by = c("player_id", "season", "week")) |>
    inner_join(wx_slim, by = "game_id")
}) |> list_rbind()

cell2 <- map(c(wb = "wb", tb = "tb", pb = "pb"), function(bcol) {
  bind_rows(
    cal_rows |> group_by(position, bucket = .data[[bcol]]) |>
      summarise(n = n(), stated = mean(p_start), emp = mean(as.numeric(hit_start)),
                .groups = "drop") |> mutate(threshold = "start"),
    cal_rows |> group_by(position, bucket = .data[[bcol]]) |>
      summarise(n = n(), stated = mean(p_boom), emp = mean(as.numeric(hit_boom)),
                .groups = "drop") |> mutate(threshold = "boom")
  ) |> mutate(delta_pp = 100 * (emp - stated))
}) |> list_rbind(names_to = "axis")

cli_h2("Cell 2: honesty by weather bucket (delta pp = emp - stated)")
print(cell2 |>
        mutate(stated = round(stated, 3), emp = round(emp, 3),
               delta_pp = fmt(delta_pp, 1)) |>
        arrange(axis, position, threshold, bucket) |>
        as.data.frame(), row.names = FALSE)

# ===========================================================================
# 4. VERDICT (pre-committed)
# ===========================================================================

cli_h1("14a verdict (pre-committed rule)")

res_trigger <- cell1 |>
  mutate(bar = if_else(position == "QB", PROCEED_RES_QB, PROCEED_RES_EPA)) |>
  filter(n >= PROCEED_RES_MIN_N, abs(mean_tot_resid) >= bar)
cal_trigger <- cell2 |>
  filter(n >= PROCEED_CAL_MIN_N, abs(delta_pp) >= PROCEED_CAL_PP)

if (nrow(res_trigger) == 0 && nrow(cal_trigger) == 0) {
  cli_alert_success("NO TRIGGER: all residual cells < {PROCEED_RES_EPA} EPA (QB < {PROCEED_RES_QB}) and all calibration cells < {PROCEED_CAL_PP}pp at the pre-committed n floors.")
  cli_alert_success("RUNG 3 VERDICT: NULL -- the market already prices the weather. Publish the receipt; ladder moves on.")
} else {
  if (nrow(res_trigger) > 0) {
    cli_alert_warning("RESIDUAL TRIGGER ({nrow(res_trigger)} cell{?s}):")
    print(res_trigger |> mutate(mean_tot_resid = fmt(mean_tot_resid)) |>
            as.data.frame(), row.names = FALSE)
  }
  if (nrow(cal_trigger) > 0) {
    cli_alert_warning("CALIBRATION TRIGGER ({nrow(cal_trigger)} cell{?s}):")
    print(cal_trigger |> mutate(delta_pp = fmt(delta_pp, 1)) |>
            as.data.frame(), row.names = FALSE)
  }
  cli_alert_warning("RUNG 3 VERDICT: PROCEED to 14b (thin post-prediction adjustment; NOT feature-table surgery, per pre-registration).")
}

readr::write_csv(cell1, "output/14a_weather_residuals.csv")
readr::write_csv(cell2, "output/14a_weather_calibration.csv")
cli_alert_success("output/14a_weather_{{residuals,calibration}}.csv")
cli_h1("14a complete")
