# R/21i_ae_validate.R
# Stage D, increment 4 of the D29 single-stage rebuild: the three
# validation gates an AE has to clear before its latents earn a feature
# slot. Heavily commented -- see R/21g's header for why.
#
# GATE (b), THE LEAKAGE TRIPWIRE (built first -- cheapest, and it's the one
# gate the plan calls out as "the control that would have caught the
# shipped defect" if it had existed from the start). R/21g's window-
# building code only ever looks up weeks (W-1) through (W-6) for a target
# week W -- it should be STRUCTURALLY IMPOSSIBLE for a target row's own
# week to leak into its own encoding. This gate doesn't trust that design
# intent, it empirically PROVES it: for a sample of target rows, physically
# delete the target week's own row from the source lookup, recompute the
# window and latent from scratch, and require the result to be
# BIT-IDENTICAL (max|diff| < 1e-9) to the original. If this ever fails,
# there's a real leak in the window-building code, not a theoretical risk.
#
# WHY 2014 IS EXCLUDED FROM THE SAMPLE: R/21g's standardization uses each
# row's OWN season's stats for 2014 specifically (the one documented
# exception -- there's no "seasons < 2014" to fit on). Deleting a 2014 row
# would shift ITS OWN season's mean/sd slightly, producing a nonzero diff
# that reflects that already-known, already-accepted exception -- not a
# leak. Sampling only from 2015+ means standardization for any sampled
# row's season always comes from strictly earlier seasons, so it can never
# be touched by deleting that one row -- any nonzero diff we see can only
# be a genuine bug in the window-building logic.
#
# NOTE ON DUPLICATED LOGIC: get_window()/mk_key() below mirror R/21g's
# functions of the same name exactly, on purpose -- this gate has to test
# the REAL pipeline's behavior, not an idealized description of it. If
# R/21g's windowing logic ever changes, this copy must change with it.
#
# POSITION + ENRICHMENT PARAMETERIZATION (added for the PFF-enrichment
# build, see ~/.claude/plans/dapper-sleeping-lollipop.md) -- same pattern
# as R/21g/R/21c1: no args/env reproduces the original usage-only RB gate
# (b) run; `Rscript R/21i_ae_validate.R WR` + `AE_ENRICH_PFF=1` targets the
# enriched builds. GRU_HIDDEN/BOTTLENECK are the carried-forward constants
# from the usage-only Increment 2/3 decision (16/8) -- NOT re-derived from
# the RB-only sweep output files, since those don't apply to WR and the
# explicit decision was not to re-sweep per channel-set variant anyway.
#
# Usage: Rscript R/21i_ae_validate.R <RB|WR>   (AE_ENRICH_PFF=1 env optional)

suppressPackageStartupMessages({
  library(torch)
  library(tidyverse)
  library(cli)
})

set.seed(42)

args     <- commandArgs(trailingOnly = TRUE)
POSITION <- if (length(args) >= 1) toupper(args[1]) else "RB"
stopifnot(POSITION %in% c("RB", "WR"))
ENRICH_PFF <- Sys.getenv("AE_ENRICH_PFF", unset = "0") == "1"

GRU_HIDDEN <- 16L
BOTTLENECK <- 8L

T_WINDOW <- 6L
CHANNELS_MAP <- list(
  RB = c("snap_share", "carry_share_obs", "target_share_obs",
         "rz_carry_share", "rz_target_share", "routes_proxy"),
  WR = c("snap_share", "target_share_obs", "air_yards_share_obs",
         "air_yards_per_target_obs", "rz_target_share", "routes_proxy")
)
PFF_CHANNELS_MAP <- list(
  RB = c("yco_attempt", "avoided_tackle_rate", "breakaway_percent",
         "zone_attempt_share", "gap_attempt_share"),
  WR = c("contested_catch_rate", "avoided_tackle_rate", "drop_rate",
         "route_rate", "avg_depth_of_target", "yac_per_reception")
)
CHANNELS <- CHANNELS_MAP[[POSITION]]
if (ENRICH_PFF) CHANNELS <- c(CHANNELS, PFF_CHANNELS_MAP[[POSITION]])
C_TOTAL  <- length(CHANNELS) + 1L

usage_path <- sprintf("data/usage_seq_%s%s.rds", tolower(POSITION), if (ENRICH_PFF) "_pff" else "")
model_state_path <- if (ENRICH_PFF) {
  sprintf("data/ae_model_%s_pff_sanity_state.pt", tolower(POSITION))
} else {
  "data/ae_model_rb_state.pt"
}

cli_h1("21i increment 4, gate (b): leakage tripwire -- {POSITION}{if (ENRICH_PFF) ' (PFF-enriched)' else ''}")

usage <- readRDS(usage_path) |> filter(!is.na(player_id))
mk_key <- function(player_id, season, week) paste(player_id, season, week, sep = "_")

lookup <- new.env(parent = emptyenv())
usage_keys <- mk_key(usage$player_id, usage$season, usage$week)
usage_chan <- as.matrix(usage[, CHANNELS])
for (i in seq_len(nrow(usage))) assign(usage_keys[i], usage_chan[i, ], envir = lookup)

get_window <- function(lookup, player_id, season, week) {
  vals <- matrix(0, nrow = T_WINDOW, ncol = length(CHANNELS))
  mask <- numeric(T_WINDOW)
  for (t in seq_len(T_WINDOW)) {
    lag <- T_WINDOW - t + 1L
    key <- mk_key(player_id, season, week - lag)
    hit <- get0(key, envir = lookup, inherits = FALSE)
    if (!is.null(hit)) { vals[t, ] <- hit; mask[t] <- 1 }
  }
  list(vals = vals, mask = mask)
}

# Per-season standardization stats, recomputed here (not saved by R/21g) --
# identical logic to R/21g's chan_stats, needed to standardize a freshly
# rebuilt window the same way the original tensor was standardized.
seasons <- sort(unique(usage$season))
chan_stats <- map(seasons, function(S) {
  hist_rows <- if (S == min(seasons)) usage$season == S else usage$season < S
  hist <- usage[hist_rows, CHANNELS]
  tibble(season = S, channel = CHANNELS,
         mean = map_dbl(CHANNELS, ~ mean(hist[[.x]], na.rm = TRUE)),
         sd   = map_dbl(CHANNELS, ~ sd(hist[[.x]],   na.rm = TRUE)))
}) |> bind_rows()

# Build ONE (T, C_TOTAL) standardized tensor row from a raw window --
# mirrors R/21g's standardization + mask-append + NA-zero-fill steps.
standardize_row <- function(raw_vals, raw_mask, season) {
  out <- matrix(0, nrow = T_WINDOW, ncol = length(CHANNELS))
  real_t <- which(raw_mask == 1)
  for (c in seq_along(CHANNELS)) {
    st <- chan_stats |> filter(season == !!season, channel == CHANNELS[c])
    mu <- st$mean
    sg <- if (!is.na(st$sd) && st$sd > 0) st$sd else 1
    out[real_t, c] <- (raw_vals[real_t, c] - mu) / sg
  }
  out[is.na(out)] <- 0
  cbind(out, raw_mask)   # append mask as the 7th column -> (T, C_TOTAL)
}

encode_row <- function(model, player_id, season, week, exclude_self) {
  # exclude_self = TRUE physically deletes this row's own key from the
  # lookup before building its window (the tripwire); FALSE builds the
  # window normally (the baseline to compare against).
  key_self <- mk_key(player_id, season, week)
  had_self <- exists(key_self, envir = lookup, inherits = FALSE)
  if (exclude_self && had_self) rm(list = key_self, envir = lookup)

  w <- get_window(lookup, player_id, season, week)
  row_tensor <- standardize_row(w$vals, w$mask, season)
  x <- torch_tensor(array(row_tensor, dim = c(1, T_WINDOW, C_TOTAL)), dtype = torch_float())
  z <- as.numeric(with_no_grad(model$encode(x)))

  if (exclude_self && had_self) assign(key_self, usage_chan[usage_keys == key_self, ], envir = lookup)
  z
}

# Reconstruct the architecture fresh and load its state_dict -- torch_load()
# of a whole GRU-containing module is unreliable across R sessions (the RNN
# implementation holds C++ pointer state that doesn't survive plain
# serialization; state_dict() holds only plain tensors, which always does).
# gru_ae() mirrors R/21h_ae_fns.R's class definition exactly -- same
# documented-coupling caveat as get_window()/mk_key() above.
gru_ae <- nn_module(
  "GruAE",
  initialize = function(n_channels, t_window, hidden, bottleneck) {
    self$t_window   <- t_window
    self$enc_gru    <- nn_gru(input_size = n_channels + 1L, hidden_size = hidden, batch_first = TRUE)
    self$enc_to_z   <- nn_linear(hidden, bottleneck)
    self$z_to_h0    <- nn_linear(bottleneck, hidden)
    self$dec_gru    <- nn_gru(input_size = 1L, hidden_size = hidden, batch_first = TRUE)
    self$dec_to_out <- nn_linear(hidden, n_channels)
  },
  encode = function(x) {
    enc_out <- self$enc_gru(x)
    h_last  <- enc_out[[2]][1, , ]
    self$enc_to_z(h_last)
  },
  forward = function(x) {
    batch    <- x$size(1)
    z        <- self$encode(x)
    h0       <- self$z_to_h0(z)$unsqueeze(1)
    dummy_in <- torch_zeros(batch, self$t_window, 1)
    dec_out  <- self$dec_gru(dummy_in, h0)
    self$dec_to_out(dec_out[[1]])
  }
)

cli_alert_info("Reconstructing carried-forward architecture: GRU, hidden={GRU_HIDDEN}, bottleneck={BOTTLENECK}")

model <- gru_ae(length(CHANNELS), T_WINDOW, GRU_HIDDEN, BOTTLENECK)
model$load_state_dict(torch_load(model_state_path))
model$eval()

# Sample 500 target rows from 2015+ only -- see header note on why 2014 is
# excluded from this specific gate.
candidates <- usage |> filter(season >= 2015)
sample_idx <- sample(seq_len(nrow(candidates)), size = min(500L, nrow(candidates)))
sample_rows <- candidates[sample_idx, ]

cli_alert_info("Testing {nrow(sample_rows)} rows (seasons 2015+ only)")

diffs <- vapply(seq_len(nrow(sample_rows)), function(i) {
  r <- sample_rows[i, ]
  z_intact   <- encode_row(model, r$player_id, r$season, r$week, exclude_self = FALSE)
  z_deleted  <- encode_row(model, r$player_id, r$season, r$week, exclude_self = TRUE)
  max(abs(z_intact - z_deleted))
}, numeric(1))

max_diff <- max(diffs)
cli_h1("Gate (b) result -- {POSITION}{if (ENRICH_PFF) ' (PFF-enriched)' else ''}")
cli_alert_info("Max |diff| across {length(diffs)} rows x {BOTTLENECK} latent dims: {format(max_diff, scientific = TRUE)}")
if (max_diff < 1e-9) {
  cli_alert_success("GATE (b) PASSED -- deleting a target row's own week never changes its own latent")
} else {
  cli_abort("GATE (b) FAILED -- a target row's own week is leaking into its own encoding (max diff {max_diff}). Do not proceed to gate (c) until this is fixed.")
}

dir.create("output", showWarnings = FALSE)
gate_b_out <- sprintf("output/21i_leakage_tripwire_%s%s_summary.rds", tolower(POSITION), if (ENRICH_PFF) "_pff" else "")
saveRDS(list(max_diff = max_diff, n_rows = length(diffs), passed = max_diff < 1e-9),
        gate_b_out)
cli_alert_success("{gate_b_out} written")
cli_h1("21i gate (b) complete -- {POSITION}")

# ===========================================================================
# GATE (a): the real per-season-refit reconstruction check + latent
# production. For every season S from 2015 onward (2014 excluded -- no
# "seasons < S" to refit on, same exception as R/21g's standardization
# rule), train a fresh GRU on ALL rows from seasons < S, evaluate masked-
# MSE reconstruction on season S's OWN rows with that model, and save its
# latents for season S's rows -- this IS the plan's per-season-refit
# discipline (fit season S's AE on seasons < S, reuse within S). The
# reconstruction numbers are the REAL gate (a) result (>=30% vs baseline,
# not increment 1-3's informal single-split preview); the latents are what
# gate (c) joins onto the FP training table.
#
# Only runs for the enriched channel set (ENRICH_PFF) -- per the explicit
# sequencing decision, gate (a) was never built for the usage-only case,
# specifically to avoid paying for this expensive loop twice.
# ===========================================================================
if (!ENRICH_PFF) {
  cli_alert_info("Gate (a) only runs for the PFF-enriched channel set (sequencing decision) -- skipping for usage-only.")
} else {
  cli_h1("21i gate (a): per-season-refit reconstruction check -- {POSITION}")

  ae_in      <- readRDS(sprintf("data/ae_tensors_%s_pff.rds", tolower(POSITION)))
  tensor     <- ae_in$tensor
  meta       <- ae_in$meta
  N_CHANNELS <- length(ae_in$channels)

  X_seq   <- torch_tensor(tensor,                   dtype = torch_float())
  Y_usage <- torch_tensor(tensor[, , 1:N_CHANNELS], dtype = torch_float())
  M       <- torch_tensor(tensor[, , C_TOTAL],      dtype = torch_float())

  masked_mse_seq <- function(pred, target, mask) {
    w  <- mask$unsqueeze(3)
    se <- (pred - target)^2 * w
    se$sum() / (mask$sum()$clamp(min = 1e-8) * N_CHANNELS)
  }

  seasons       <- sort(unique(meta$season))
  refit_seasons <- seasons[seasons >= 2015]
  cli_alert_info("Refitting {length(refit_seasons)} seasons (2015-{max(refit_seasons)}); 2014 excluded (no prior seasons to refit on)")

  MAX_EPOCHS_REFIT <- 300L
  PATIENCE_REFIT   <- 20L
  BATCH_SIZE_REFIT <- 256L
  INNER_VAL_FRAC   <- 0.15   # inner holdout WITHIN the training population only, for early stopping -- never season S itself

  # Same season-week semi_join split idiom as R/21d's fold loop (fit_sws/
  # cal_sws), just renamed for an inner early-stopping split instead of a
  # calibration split.
  train_one_season <- function(S) {
    train_idx_all <- which(meta$season < S)
    eval_idx      <- which(meta$season == S)

    train_sws <- meta[train_idx_all, c("season", "week")] |> distinct() |> arrange(season, week)
    n_inner   <- max(1L, floor(INNER_VAL_FRAC * nrow(train_sws)))
    inner_val_sws <- tail(train_sws, n_inner)
    fit_sws       <- head(train_sws, nrow(train_sws) - n_inner)

    train_meta_all <- meta[train_idx_all, ] |> mutate(.idx = train_idx_all)
    fit_idx <- train_meta_all |> semi_join(fit_sws, by = c("season", "week")) |> pull(.idx)
    val_idx <- train_meta_all |> semi_join(inner_val_sws, by = c("season", "week")) |> pull(.idx)

    torch_manual_seed(42)
    m   <- gru_ae(N_CHANNELS, T_WINDOW, GRU_HIDDEN, BOTTLENECK)
    opt <- optim_adam(m$parameters, lr = 1e-3)
    best_val <- Inf; best_state <- NULL; pctr <- 0L
    n_fit <- length(fit_idx)
    for (epoch in seq_len(MAX_EPOCHS_REFIT)) {
      m$train()
      perm <- sample(n_fit)
      for (b in seq(1, n_fit, by = BATCH_SIZE_REFIT)) {
        idx <- fit_idx[perm[b:min(b + BATCH_SIZE_REFIT - 1L, n_fit)]]
        opt$zero_grad()
        loss <- masked_mse_seq(m(X_seq[idx, , ]), Y_usage[idx, , ], M[idx, ])
        loss$backward()
        opt$step()
      }
      m$eval()
      vloss <- with_no_grad(masked_mse_seq(m(X_seq[val_idx, , ]), Y_usage[val_idx, , ], M[val_idx, ]))$item()
      if (vloss < best_val - 1e-6) {
        best_val <- vloss; best_state <- lapply(m$state_dict(), function(t) t$clone()); pctr <- 0L
      } else {
        pctr <- pctr + 1L
      }
      if (pctr >= PATIENCE_REFIT) break
    }
    m$load_state_dict(best_state)
    m$eval()

    eval_loss       <- with_no_grad(masked_mse_seq(m(X_seq[eval_idx, , ]), Y_usage[eval_idx, , ], M[eval_idx, ]))$item()
    eval_real_cells <- sum(as.numeric(M[eval_idx, ])) * N_CHANNELS
    latents         <- as.matrix(with_no_grad(m$encode(X_seq[eval_idx, , ])))

    list(season = S, eval_loss = eval_loss, eval_real_cells = eval_real_cells,
         eval_idx = eval_idx, latents = latents)
  }

  cli_h2("Per-season refits ({length(refit_seasons)} seasons)")
  results <- vector("list", length(refit_seasons))
  for (i in seq_along(refit_seasons)) {
    S <- refit_seasons[i]
    t0 <- proc.time()[["elapsed"]]
    results[[i]] <- train_one_season(S)
    t1 <- proc.time()[["elapsed"]]
    cli_alert_info("Season {S}: {length(results[[i]]$eval_idx)} rows, masked MSE={round(results[[i]]$eval_loss, 5)} ({round(t1 - t0, 1)}s)")
  }

  # Aggregate weighted by real-cell count per season (not just row count),
  # vs a baseline computed over the exact same pooled population.
  total_loss_num <- sum(vapply(results, function(r) r$eval_loss * r$eval_real_cells, numeric(1)))
  total_cells    <- sum(vapply(results, function(r) r$eval_real_cells, numeric(1)))
  agg_loss       <- total_loss_num / total_cells

  all_eval_idx  <- unlist(lapply(results, function(r) r$eval_idx))
  baseline_loss <- masked_mse_seq(torch_zeros_like(Y_usage[all_eval_idx, , ]), Y_usage[all_eval_idx, , ], M[all_eval_idx, ])$item()

  pct_improve <- 100 * (baseline_loss - agg_loss) / baseline_loss
  passed_a    <- pct_improve >= 30

  cli_h1("Gate (a) result -- {POSITION} (PFF-enriched, real per-season-refit)")
  cli_alert_info("Aggregate (real-cell-weighted) masked MSE across {length(refit_seasons)} season-refits: {round(agg_loss, 5)}")
  cli_alert_info("Baseline (predict 0), same pooled population: {round(baseline_loss, 5)}")
  if (passed_a) {
    cli_alert_success("GATE (a) PASSED: {round(pct_improve, 1)}% improvement (>=30% required)")
  } else {
    cli_alert_warning("GATE (a) NOT MET: {round(pct_improve, 1)}% improvement (<30% required)")
  }

  # ---- Latent production (gate (c)'s input) --------------------------------
  latent_tbl <- map_dfr(results, function(r) {
    z <- r$latents
    colnames(z) <- paste0("z", seq_len(ncol(z)))
    bind_cols(meta[r$eval_idx, c("player_id", "season", "week")], as_tibble(z))
  })
  latent_path <- sprintf("output/21i_ae_latents_%s_pff.rds", tolower(POSITION))
  saveRDS(latent_tbl, latent_path)
  cli_alert_success("{latent_path} written ({nrow(latent_tbl)} rows, seasons {min(refit_seasons)}-{max(refit_seasons)}; 2014 rows have no latent, by design)")

  gate_a_path <- sprintf("output/21i_gate_a_%s_pff_summary.rds", tolower(POSITION))
  saveRDS(list(agg_loss = agg_loss, baseline_loss = baseline_loss, pct_improve = pct_improve,
               passed = passed_a, n_seasons = length(refit_seasons)), gate_a_path)
  cli_alert_success("{gate_a_path} written")
  cli_h1("21i gate (a) complete -- {POSITION}")
}
