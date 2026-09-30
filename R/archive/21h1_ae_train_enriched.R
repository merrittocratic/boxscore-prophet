# R/21h1_ae_train_enriched.R
# PFF-enrichment build, step 5 (see ~/.claude/plans/dapper-sleeping-lollipop.md):
# a quick sanity retrain of the ALREADY-ADOPTED architecture (GRU, hidden=16,
# bottleneck=8, early stopping -- carried forward from the usage-only
# Increment 2/3 decision, NOT re-swept here) on each PFF-enriched tensor.
# This is a fast fail-fast check before committing to gate (a)'s much more
# expensive ~11-season-refit loop (R/21i) -- if something about the
# enriched channel set trains badly (NaN loss, no convergence), better to
# find out in one quick run than after 30-60 minutes of per-season retrains.
#
# gru_ae()/masked_mse_seq() mirror R/21h_ae_fns.R's definitions exactly --
# same documented-coupling caveat as R/21i's copy: if R/21h's architecture
# ever changes, this copy (and R/21i's) must change with it.
#
# Usage: Rscript R/21h1_ae_train_enriched.R <RB|WR>

suppressPackageStartupMessages({
  library(torch)
  library(tidyverse)
  library(cli)
})

set.seed(42)
torch_manual_seed(42)

args     <- commandArgs(trailingOnly = TRUE)
POSITION <- if (length(args) >= 1) toupper(args[1]) else cli_abort("Usage: Rscript R/21h1_ae_train_enriched.R <RB|WR>")
stopifnot(POSITION %in% c("RB", "WR"))

GRU_HIDDEN <- 16L   # carried forward, not re-tuned
BOTTLENECK <- 8L    # carried forward from the usage-only sweep, not re-swept
MAX_EPOCHS <- 300L
PATIENCE   <- 20L
BATCH_SIZE <- 256L

cli_h1("21h1: enriched AE sanity retrain -- {POSITION}")

ae_in    <- readRDS(sprintf("data/ae_tensors_%s_pff.rds", tolower(POSITION)))
tensor   <- ae_in$tensor
meta     <- ae_in$meta
T_WINDOW <- ae_in$t_window
N_CHANNELS <- length(ae_in$channels)
C_TOTAL    <- N_CHANNELS + 1L
N          <- dim(tensor)[1]

cli_alert_info("Tensor: {N} rows, T={T_WINDOW}, {N_CHANNELS} usage channels + mask")

X_seq   <- torch_tensor(tensor,                     dtype = torch_float())
Y_usage <- torch_tensor(tensor[, , 1:N_CHANNELS],   dtype = torch_float())
M       <- torch_tensor(tensor[, , C_TOTAL],        dtype = torch_float())

train_idx <- which(meta$season < 2025)
val_idx   <- which(meta$season == 2025)
cli_alert_info("Train rows: {length(train_idx)} (seasons <2025) | Val rows: {length(val_idx)} (season 2025)")

X_seq_train <- X_seq[train_idx, , ]; Y_train <- Y_usage[train_idx, , ]; M_train <- M[train_idx, ]
X_seq_val   <- X_seq[val_idx, , ];   Y_val   <- Y_usage[val_idx, , ];   M_val   <- M[val_idx, ]

masked_mse_seq <- function(pred, target, mask) {
  w  <- mask$unsqueeze(3)
  se <- (pred - target)^2 * w
  se$sum() / (mask$sum()$clamp(min = 1e-8) * N_CHANNELS)
}

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

model <- gru_ae(N_CHANNELS, T_WINDOW, GRU_HIDDEN, BOTTLENECK)
opt   <- optim_adam(model$parameters, lr = 1e-3)

best_val <- Inf
best_state <- NULL
patience_ctr <- 0L
n_train <- length(train_idx)

cli_h2("Training (identical hyperparameters/split convention to the usage-only build)")
for (epoch in seq_len(MAX_EPOCHS)) {
  model$train()
  perm <- sample(n_train)
  for (b in seq(1, n_train, by = BATCH_SIZE)) {
    idx <- perm[b:min(b + BATCH_SIZE - 1L, n_train)]
    opt$zero_grad()
    pred <- model(X_seq_train[idx, , ])
    loss <- masked_mse_seq(pred, Y_train[idx, , ], M_train[idx, ])
    loss$backward()
    opt$step()
  }
  model$eval()
  val_loss <- with_no_grad(masked_mse_seq(model(X_seq_val), Y_val, M_val))$item()
  if (val_loss < best_val - 1e-6) {
    best_val <- val_loss
    best_state <- lapply(model$state_dict(), function(t) t$clone())
    patience_ctr <- 0L
  } else {
    patience_ctr <- patience_ctr + 1L
  }
  if (epoch %% 20 == 0 || epoch == 1L) {
    cli_alert_info("epoch {epoch}: val masked MSE = {round(val_loss, 5)} (best {round(best_val, 5)}, patience {patience_ctr}/{PATIENCE})")
  }
  if (patience_ctr >= PATIENCE) {
    cli_alert_info("Early stop at epoch {epoch}")
    break
  }
}
model$load_state_dict(best_state)

baseline_loss <- masked_mse_seq(torch_zeros_like(Y_val), Y_val, M_val)$item()
pct_improve   <- 100 * (baseline_loss - best_val) / baseline_loss

cli_h1("21h1 sanity retrain result -- {POSITION}")
cli_alert_info("Baseline (predict 0) val masked MSE: {round(baseline_loss, 5)}")
cli_alert_info("Enriched GRU AE val masked MSE: {round(best_val, 5)}")
cli_alert_success("Improvement vs baseline: {round(pct_improve, 1)}% -- informal, single-split sanity check only; gate (a)'s per-season-refit loop is the real number")

dir.create("output", showWarnings = FALSE)
torch_save(model$state_dict(), sprintf("data/ae_model_%s_pff_sanity_state.pt", tolower(POSITION)))
saveRDS(list(val_loss = best_val, baseline_loss = baseline_loss, pct_improve = pct_improve,
             bottleneck = BOTTLENECK, hidden = GRU_HIDDEN),
        sprintf("output/21h1_%s_pff_sanity_summary.rds", tolower(POSITION)))
cli_alert_success("Sanity artifacts written")
cli_h1("21h1 complete -- {POSITION}")
