# R/21h_ae_fns.R
# Stage D, increments 1-3 of the D29 single-stage rebuild: the actual
# autoencoder. Heavily commented -- see R/21g's header for why (Steve's
# first AE, the code itself is the artifact to re-read and learn from).
#
# INCREMENT 1 (this pass): the simplest possible dense autoencoder. Input
# is a FLATTENED (T=6, C=7) window -- 42 numbers per row -- fed straight
# through two linear layers down to a 4-number bottleneck, then back up
# through two more linear layers to a 42-number reconstruction. No
# recurrence, no attention, nothing sequence-aware: the point of doing it
# this way first is to see whether ANY compression of a usage window is
# learnable at all before adding architecture that assumes order matters
# (that's increment 2, a GRU, adopted only if it actually beats this).
#
# WHY THE MASK CHANNEL IS PART OF THE INPUT BUT NOT PART OF THE LOSS: each
# of the 6 timesteps carries its own mask value (real week vs bye/DNP/
# pre-debut padding) as the 7th number in that timestep's block, so the
# network SEES which weeks were real -- but the loss only scores the 6
# usage numbers per timestep, and only for timesteps that were real to
# begin with. A padded timestep's "true" value is an arbitrary zero we
# chose to fill in, not real data -- there's nothing genuine there for the
# network to learn to reconstruct, so it's excluded from the loss entirely
# rather than rewarded for memorizing our padding convention.
#
# SPLIT (a simplification, flagged explicitly): the plan's real rule for
# Stage D is per-season refit (train on seasons < S, reuse within S) once
# this AE is wired into the actual walk-forward grading. This increment
# isn't graded yet -- it's a mechanics check ("does a dense AE learn
# anything on this data at all") -- so it uses one plain train/validation
# split (train = seasons < 2025, val = season 2025) rather than the full
# per-season-refit loop. That loop gets built once increments 2-3 have
# picked an architecture to wire in.
#
# Usage: Rscript R/21h_ae_fns.R

suppressPackageStartupMessages({
  library(torch)
  library(tidyverse)
  library(cli)
})

set.seed(42)
torch_manual_seed(42)

# ---------------------------------------------------------------------------
# Load increment 0's tensor and flatten (N, T=6, C=7) -> (N, 42). Flatten
# order is "timestep-major": for row i, the 42 numbers are
# [ch1..ch7 @ t-6, ch1..ch7 @ t-5, ..., ch1..ch7 @ t-1] -- so positions
# 7, 14, 21, 28, 35, 42 are the six timesteps' mask flags, and every other
# position is one of the six usage channels at some timestep.
# ---------------------------------------------------------------------------
ae_in    <- readRDS("data/ae_tensors_rb.rds")
tensor   <- ae_in$tensor
meta     <- ae_in$meta
T_WINDOW <- ae_in$t_window
C_TOTAL  <- length(ae_in$channels) + 1L   # 6 usage + 1 mask = 7
N        <- dim(tensor)[1]
INPUT_DIM <- T_WINDOW * C_TOTAL           # 42

cli_h1("21h increment 1: dense autoencoder -- input dim {INPUT_DIM}")

flat <- t(vapply(seq_len(N), function(i) as.vector(t(tensor[i, , ])),
                  numeric(INPUT_DIM)))
cli_alert_success("Flattened to {nrow(flat)} x {ncol(flat)}")

# Per-row loss-weight matrix: 1 for a usage value whose timestep was real,
# 0 for a usage value whose timestep was padding, and 0 for every mask
# column itself (never scored -- it's context for the network, not a
# reconstruction target).
weight <- matrix(0, N, INPUT_DIM)
for (t in seq_len(T_WINDOW)) {
  usage_idx <- ((t - 1) * C_TOTAL + 1):((t - 1) * C_TOTAL + C_TOTAL - 1)
  mask_col  <- t * C_TOTAL
  weight[, usage_idx] <- flat[, mask_col]   # broadcast that timestep's mask over its 6 usage cols
}
cli_alert_success("Loss-weight matrix built: {round(100 * sum(weight) / (N * (C_TOTAL - 1) * T_WINDOW), 1)}% of usage cells are real (rest are padding, excluded from loss)")

X <- torch_tensor(flat,   dtype = torch_float())
W <- torch_tensor(weight, dtype = torch_float())

train_idx <- which(meta$season < 2025)
val_idx   <- which(meta$season == 2025)
cli_alert_info("Train rows: {length(train_idx)} (seasons <2025) | Val rows: {length(val_idx)} (season 2025)")

X_train <- X[train_idx, ]; W_train <- W[train_idx, ]
X_val   <- X[val_idx, ];   W_val   <- W[val_idx, ]

# ---------------------------------------------------------------------------
# The model: encoder collapses 42 -> 16 -> 4 (the bottleneck), decoder
# mirrors it back up 4 -> 16 -> 42. ReLU between layers so the network can
# learn non-linear structure; no activation on the final layer since the
# targets are standardized z-scores (can be negative, unbounded), not
# probabilities or pixel intensities.
# ---------------------------------------------------------------------------
BOTTLENECK <- 4L
HIDDEN     <- 16L

dense_ae <- nn_module(
  "DenseAE",
  initialize = function(input_dim, bottleneck, hidden) {
    self$enc1 <- nn_linear(input_dim, hidden)     # 42 -> 16
    self$enc2 <- nn_linear(hidden, bottleneck)    # 16 -> 4   (the bottleneck)
    self$dec1 <- nn_linear(bottleneck, hidden)    # 4  -> 16
    self$dec2 <- nn_linear(hidden, input_dim)     # 16 -> 42  (reconstruction)
  },
  encode = function(x) self$enc2(nnf_relu(self$enc1(x))),
  forward = function(x) self$dec2(nnf_relu(self$dec1(self$encode(x))))
)

# Masked MSE: squared error at every position, zeroed out by `weight`
# before averaging, so padded/mask positions contribute nothing to the
# gradient. Dividing by weight$sum() (not N*INPUT_DIM) means the loss is
# the average error PER REAL CELL, not diluted by how much padding a batch
# happened to contain.
masked_mse <- function(pred, target, weight) {
  se <- (pred - target)^2 * weight
  se$sum() / weight$sum()$clamp(min = 1e-8)
}

model <- dense_ae(INPUT_DIM, BOTTLENECK, HIDDEN)
opt   <- optim_adam(model$parameters, lr = 1e-3)

MAX_EPOCHS <- 300L
PATIENCE   <- 20L
BATCH_SIZE <- 256L
n_train    <- length(train_idx)

best_val <- Inf
best_state <- NULL
patience_ctr <- 0L

cli_h2("Training (early stop on val masked MSE, patience {PATIENCE})")
for (epoch in seq_len(MAX_EPOCHS)) {
  model$train()
  perm <- sample(n_train)
  for (b in seq(1, n_train, by = BATCH_SIZE)) {
    idx <- perm[b:min(b + BATCH_SIZE - 1L, n_train)]
    opt$zero_grad()
    pred <- model(X_train[idx, ])
    loss <- masked_mse(pred, X_train[idx, ], W_train[idx, ])
    loss$backward()
    opt$step()
  }

  model$eval()
  val_loss <- with_no_grad(masked_mse(model(X_val), X_val, W_val))$item()

  if (val_loss < best_val - 1e-6) {
    best_val <- val_loss
    best_state <- lapply(model$state_dict(), function(t) t$clone())
    patience_ctr <- 0L
  } else {
    patience_ctr <- patience_ctr + 1L
  }
  if (epoch %% 10 == 0 || epoch == 1L) {
    cli_alert_info("epoch {epoch}: val masked MSE = {round(val_loss, 5)} (best {round(best_val, 5)}, patience {patience_ctr}/{PATIENCE})")
  }
  if (patience_ctr >= PATIENCE) {
    cli_alert_info("Early stop at epoch {epoch}")
    break
  }
}
model$load_state_dict(best_state)

# ---------------------------------------------------------------------------
# Informal preview of Increment 4's actual reconstruction gate (NOT the
# gate itself -- that requires the leakage tripwire and the real
# per-season-refit split too). "Baseline" = predict 0 for every real cell,
# which is what a per-channel mean predicts in standardized space.
# ---------------------------------------------------------------------------
baseline_loss <- masked_mse(torch_zeros_like(X_val), X_val, W_val)$item()
final_loss    <- with_no_grad(masked_mse(model(X_val), X_val, W_val))$item()
pct_improve   <- 100 * (baseline_loss - final_loss) / baseline_loss

cli_h1("Increment 1 result")
cli_alert_info("Baseline (predict 0 for every real cell) val masked MSE: {round(baseline_loss, 5)}")
cli_alert_info("Dense AE val masked MSE: {round(final_loss, 5)}")
cli_alert_success("Improvement vs baseline: {round(pct_improve, 1)}% -- informal only, Increment 4's real gate (>=30%, plus the leakage tripwire) is separate and not run here")

cli_h2("Sample latents (5 val rows, the 4 bottleneck numbers per player-week)")
sample_latents <- as.matrix(with_no_grad(model$encode(X_val[1:5, ])))
rownames(sample_latents) <- with(meta[val_idx[1:5], ], paste(player_id, season, week, sep = "_"))
colnames(sample_latents) <- paste0("z", seq_len(BOTTLENECK))
print(round(sample_latents, 3))

dir.create("output", showWarnings = FALSE)
torch_save(model$state_dict(), "data/ae_dense_rb_state.pt")
saveRDS(list(val_loss = final_loss, baseline_loss = baseline_loss,
             pct_improve = pct_improve, bottleneck = BOTTLENECK, hidden = HIDDEN),
        "output/21h_dense_ae_rb_summary.rds")
cli_alert_success("data/ae_dense_rb_model.pt + output/21h_dense_ae_rb_summary.rds written")
cli_h1("21h increment 1 complete")

# ===========================================================================
# INCREMENT 2: GRU sequence encoder. Same tensor, same bottleneck (4), same
# train/val split, same optimizer, same patience -- ONLY the architecture
# changes, so any difference in the result is attributable to ONE thing:
# giving the network an explicit notion that these 6 weeks are ORDERED. A
# GRU reads the window one timestep at a time and carries a hidden state
# forward, so week 3's influence on the encoding has to pass THROUGH weeks
# 1-2's hidden state -- the network can't see all 42 numbers at once the
# way increment 1's dense AE could. If that structural bias doesn't help on
# this data, that's a real, useful negative result, not a wasted build.
#
# PRE-REGISTERED GATE (plan, Stage D): adopt the GRU only if its val masked
# MSE beats increment 1's dense AE by >= 5%, decided AFTER training both --
# not by picking whichever number looks better once we already have it.
# Otherwise increment 1 ships and this becomes a documented null (kept in
# the repo, not deleted -- a null is still a real result).
# ===========================================================================
cli_h1("21h increment 2: GRU sequence encoder")

N_CHANNELS <- length(ae_in$channels)   # 6 usage channels; mask is handled as a 7th INPUT feature, not a target

# Sequence-shaped tensors -- no flattening this time. X_seq feeds the GRU
# one timestep at a time, each timestep a 7-number vector (6 usage channels
# + that timestep's mask flag, same "mask is input context, not a
# reconstruction target" rule as increment 1). Y_usage/M are the
# reconstruction target and its per-timestep weight.
X_seq   <- torch_tensor(tensor,                   dtype = torch_float())   # (N, T, 7)
Y_usage <- torch_tensor(tensor[, , 1:N_CHANNELS], dtype = torch_float())   # (N, T, 6)
M       <- torch_tensor(tensor[, , C_TOTAL],      dtype = torch_float())   # (N, T)    -- mask alone

X_seq_train <- X_seq[train_idx, , ]; Y_train <- Y_usage[train_idx, , ]; M_train <- M[train_idx, ]
X_seq_val   <- X_seq[val_idx, , ];   Y_val   <- Y_usage[val_idx, , ];   M_val   <- M[val_idx, ]

# Same masked-MSE idea as increment 1, adapted to a (N, T, 6) shape instead
# of a flat 42-vector: a real timestep's mask (1) broadcasts across its 6
# usage channels; a padded timestep (0) zeroes out all 6 at once, since
# padding is a per-timestep, not per-channel, event (established in
# increment 0 -- a bye/DNP/pre-debut week is missing entirely, never a
# partial week).
masked_mse_seq <- function(pred, target, mask) {
  w  <- mask$unsqueeze(3)                     # (N, T) -> (N, T, 1), broadcasts across the 6 channels
  se <- (pred - target)^2 * w
  se$sum() / (mask$sum()$clamp(min = 1e-8) * N_CHANNELS)
}

gru_ae <- nn_module(
  "GruAE",
  initialize = function(n_channels, t_window, hidden, bottleneck) {
    self$t_window   <- t_window
    self$enc_gru    <- nn_gru(input_size = n_channels + 1L, hidden_size = hidden, batch_first = TRUE)
    self$enc_to_z   <- nn_linear(hidden, bottleneck)   # encoder's final hidden state -> the bottleneck
    self$z_to_h0    <- nn_linear(bottleneck, hidden)   # bottleneck -> decoder's STARTING hidden state
    self$dec_gru    <- nn_gru(input_size = 1L, hidden_size = hidden, batch_first = TRUE)
    self$dec_to_out <- nn_linear(hidden, n_channels)   # decoder hidden state at each step -> 6 usage numbers
  },
  encode = function(x) {
    enc_out <- self$enc_gru(x)
    h_last  <- enc_out[[2]][1, , ]      # (batch, hidden) -- hidden state after reading all 6 real timesteps
    self$enc_to_z(h_last)
  },
  forward = function(x) {
    batch    <- x$size(1)
    z        <- self$encode(x)
    h0       <- self$z_to_h0(z)$unsqueeze(1)               # (1, batch, hidden)
    # The decoder GRU has nothing informative to read at each step (its
    # per-step input is literally zero) -- every bit of what it reconstructs
    # has to come from the propagated hidden state, which started life as
    # z. This is the simplest possible sequence decoder: no teacher
    # forcing, no per-step signal, all the compression pressure lands on z.
    dummy_in <- torch_zeros(batch, self$t_window, 1)
    dec_out  <- self$dec_gru(dummy_in, h0)
    self$dec_to_out(dec_out[[1]])                          # (batch, T, 6)
  }
)

GRU_HIDDEN <- 16L   # matches the dense AE's hidden width, so the comparison isn't confounded by capacity

gru_model <- gru_ae(N_CHANNELS, T_WINDOW, GRU_HIDDEN, BOTTLENECK)
gru_opt   <- optim_adam(gru_model$parameters, lr = 1e-3)

best_val_gru <- Inf
best_state_gru <- NULL
patience_ctr <- 0L

cli_h2("Training GRU AE (identical split/patience/optimizer to increment 1)")
for (epoch in seq_len(MAX_EPOCHS)) {
  gru_model$train()
  perm <- sample(n_train)
  for (b in seq(1, n_train, by = BATCH_SIZE)) {
    idx <- perm[b:min(b + BATCH_SIZE - 1L, n_train)]
    gru_opt$zero_grad()
    pred <- gru_model(X_seq_train[idx, , ])
    loss <- masked_mse_seq(pred, Y_train[idx, , ], M_train[idx, ])
    loss$backward()
    gru_opt$step()
  }
  gru_model$eval()
  val_loss <- with_no_grad(masked_mse_seq(gru_model(X_seq_val), Y_val, M_val))$item()
  if (val_loss < best_val_gru - 1e-6) {
    best_val_gru <- val_loss
    best_state_gru <- lapply(gru_model$state_dict(), function(t) t$clone())
    patience_ctr <- 0L
  } else {
    patience_ctr <- patience_ctr + 1L
  }
  if (epoch %% 10 == 0 || epoch == 1L) {
    cli_alert_info("epoch {epoch}: val masked MSE = {round(val_loss, 5)} (best {round(best_val_gru, 5)}, patience {patience_ctr}/{PATIENCE})")
  }
  if (patience_ctr >= PATIENCE) {
    cli_alert_info("Early stop at epoch {epoch}")
    break
  }
}
gru_model$load_state_dict(best_state_gru)

# ---------------------------------------------------------------------------
# THE GATE. `final_loss` is increment 1's dense AE val masked MSE, already
# computed above in this same run.
# ---------------------------------------------------------------------------
pct_vs_dense <- 100 * (final_loss - best_val_gru) / final_loss

cli_h1("Increment 2 result -- GRU vs dense AE")
cli_alert_info("Dense AE (increment 1) val masked MSE: {round(final_loss, 5)}")
cli_alert_info("GRU AE   (increment 2) val masked MSE: {round(best_val_gru, 5)}")
cli_alert_info("GRU improvement over dense: {round(pct_vs_dense, 1)}%")

GRU_ADOPTED <- pct_vs_dense >= 5
if (GRU_ADOPTED) {
  cli_alert_success("GATE PASSED (>= 5%) -- GRU adopted as the arm A3 architecture")
} else {
  cli_alert_warning("GATE NOT MET (< 5%) -- dense AE (increment 1) ships; GRU kept in the repo as a documented null, not deleted")
}

torch_save(gru_model$state_dict(), "data/ae_gru_rb_state.pt")
saveRDS(list(dense_val_loss = final_loss, gru_val_loss = best_val_gru,
             pct_improve_vs_dense = pct_vs_dense, adopted = GRU_ADOPTED,
             gru_hidden = GRU_HIDDEN, bottleneck = BOTTLENECK),
        "output/21h_gru_vs_dense_rb_summary.rds")
cli_alert_success("data/ae_gru_rb_model.pt + output/21h_gru_vs_dense_rb_summary.rds written")
cli_h1("21h increment 2 complete")

# ===========================================================================
# INCREMENT 3: bottleneck sweep. Same GRU architecture (the one increment 2
# just adopted), same tensor/split/optimizer/patience -- vary ONLY the
# bottleneck size L and see how much of increment 2's win survives at each
# size. Rule (pre-registered in the plan): pick the SMALLEST L within 5% of
# the best L's val masked MSE, not simply whichever L scores lowest -- a
# smaller bottleneck is a stronger, more falsifiable compression claim
# (fewer numbers doing the same job), so a near-tie goes to the simpler
# model. Same "ties go to the simpler arm" rule this project's other
# ablations already use.
# ===========================================================================
cli_h1("21h increment 3: GRU bottleneck sweep")

# Increment 2's training loop, extracted into a function so the same ~15
# lines aren't copy-pasted four times -- only `bottleneck` changes between
# runs. Same seed every time so bottleneck size is the only thing that
# differs, not random initialization luck.
train_gru_ae <- function(bottleneck, hidden = GRU_HIDDEN, seed = 42L) {
  torch_manual_seed(seed)
  m   <- gru_ae(N_CHANNELS, T_WINDOW, hidden, bottleneck)
  opt <- optim_adam(m$parameters, lr = 1e-3)
  best_val <- Inf; best_state <- NULL; pctr <- 0L
  for (epoch in seq_len(MAX_EPOCHS)) {
    m$train()
    perm <- sample(n_train)
    for (b in seq(1, n_train, by = BATCH_SIZE)) {
      idx <- perm[b:min(b + BATCH_SIZE - 1L, n_train)]
      opt$zero_grad()
      loss <- masked_mse_seq(m(X_seq_train[idx, , ]), Y_train[idx, , ], M_train[idx, ])
      loss$backward()
      opt$step()
    }
    m$eval()
    val_loss <- with_no_grad(masked_mse_seq(m(X_seq_val), Y_val, M_val))$item()
    if (val_loss < best_val - 1e-6) {
      best_val <- val_loss
      best_state <- lapply(m$state_dict(), function(t) t$clone())
      pctr <- 0L
    } else {
      pctr <- pctr + 1L
    }
    if (pctr >= PATIENCE) break
  }
  m$load_state_dict(best_state)
  list(model = m, val_loss = best_val, epochs_run = epoch)
}

BOTTLENECKS <- c(2L, 4L, 6L, 8L)
sweep <- vector("list", length(BOTTLENECKS))
names(sweep) <- as.character(BOTTLENECKS)

for (L in BOTTLENECKS) {
  if (L == BOTTLENECK) {
    # Increment 2 already trained this exact config (same seed, same data,
    # same hidden size) -- reuse it rather than burn duplicate compute on
    # an identical run.
    cli_alert_info("L={L}: reusing increment 2's result (val masked MSE {round(best_val_gru, 5)}) -- not retraining")
    sweep[[as.character(L)]] <- list(bottleneck = L, val_loss = best_val_gru, model = gru_model)
    next
  }
  cli_h2("Training GRU AE, bottleneck L={L}")
  res <- train_gru_ae(L)
  cli_alert_info("L={L}: val masked MSE = {round(res$val_loss, 5)} (stopped after {res$epochs_run} epochs)")
  sweep[[as.character(L)]] <- list(bottleneck = L, val_loss = res$val_loss, model = res$model)
}

sweep_tbl <- tibble(
  bottleneck = BOTTLENECKS,
  val_loss   = vapply(sweep, function(s) s$val_loss, numeric(1))
)
best_loss <- min(sweep_tbl$val_loss)
sweep_tbl <- sweep_tbl |>
  mutate(pct_worse_than_best = 100 * (val_loss - best_loss) / best_loss,
         within_5pct = val_loss <= best_loss * 1.05)
chosen_L <- min(sweep_tbl$bottleneck[sweep_tbl$within_5pct])

cli_h1("Increment 3 result -- bottleneck sweep")
print(sweep_tbl |> mutate(across(where(is.numeric), ~round(.x, 5))) |> as.data.frame(), row.names = FALSE)
cli_alert_success("Chosen bottleneck: L={chosen_L} (smallest L within 5% of the best val masked MSE, {round(best_loss, 5)})")

chosen_model <- sweep[[as.character(chosen_L)]]$model
# Whole-module torch_save()/torch_load() is unreliable for GRU-containing
# modules across R sessions -- the RNN implementation holds internal C++
# pointer state that doesn't round-trip through plain serialization
# (surfaces as "external pointer is not valid" on reload). state_dict()
# holds only plain tensors, which always serialize safely; reloading means
# reconstructing the architecture fresh (same gru_ae() call) and calling
# load_state_dict(), not torch_load()-ing a whole model object.
torch_save(chosen_model$state_dict(), "data/ae_model_rb_state.pt")
for (L in BOTTLENECKS) {
  if (L != BOTTLENECK) torch_save(sweep[[as.character(L)]]$model$state_dict(), sprintf("data/ae_gru_rb_L%d_state.pt", L))
}
saveRDS(list(sweep = sweep_tbl, chosen_L = chosen_L, best_loss = best_loss),
        "output/21h_bottleneck_sweep_rb_summary.rds")
cli_alert_success("data/ae_model_rb_state.pt (chosen, L={chosen_L}) + output/21h_bottleneck_sweep_rb_summary.rds written")
cli_h1("21h increment 3 complete")
