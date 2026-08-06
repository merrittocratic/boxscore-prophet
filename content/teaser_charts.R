# teaser_charts.R -- two figures for the 2026 season teaser article
# Run from repo root: Rscript content/teaser_charts.R
# Fig 1: sample Tuesday board (2025 W15 RB hindcast, shipped output)
# Fig 2: calibration of deployed maps, pooled across positions (backtest)

library(ggplot2)

dir.create("content/img", showWarnings = FALSE, recursive = TRUE)

surface <- "#fcfcfb"
ink     <- "#0b0b0b"
ink2    <- "#52514e"
blue_lt <- "#86b6ef"
blue_dk <- "#2a78d6"

theme_teaser <- theme_minimal(base_size = 13) +
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

## ---- Fig 1: the Tuesday board, RB sample -----------------------------------

board <- read.csv("output/10d_start_board_2025_w15.csv")
rb <- board[board$position == "RB", ]
rb <- rb[order(rb$rank), ][15:35, ]
rb$label <- paste0(rb$player_disp, "  vs ", rb$defteam)
rb$label <- factor(rb$label, levels = rev(rb$label))

p1 <- ggplot(rb) +
  geom_col(aes(x = start_pct, y = label), fill = blue_lt, width = 0.62) +
  geom_col(aes(x = boom_pct,  y = label), fill = blue_dk, width = 0.62) +
  geom_text(aes(x = start_pct, y = label, label = paste0(start_pct, "%")),
            hjust = -0.25, size = 3.4, color = ink) +
  geom_text(aes(x = boom_pct, y = label, label = boom_pct),
            hjust = 1.35, size = 3.1, color = surface, fontface = "bold") +
  scale_x_continuous(limits = c(0, 60), expand = c(0, 0)) +
  labs(
    title = "The Tuesday board: running backs 15-35, 2025 Week 15",
    subtitle = "Full bar: chance of a startable week (15+ PPR).\nDark bar: chance of a boom week (20+).",
    x = NULL, y = NULL,
    caption = "Shipped model output, scored before kickoff."
  ) +
  theme_teaser +
  theme(panel.grid.major.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = ink, size = 10.5))

ggsave("content/img/teaser_board_rb_w15.png", p1,
       width = 7.5, height = 8.2, dpi = 150, bg = surface)

## ---- Fig 2: calibration of the deployed maps -------------------------------
# One (pred, hit) pair per player-week per threshold, using the map each pool
# actually ships: RB platt_vegas / WR strat_platt + strat_iso /
# QB platt_vol_vegas + platt / TE platt_vol_vegas + platt_vegas.

rbwr <- read.csv("output/06c_recal_probabilities.csv")
qb   <- read.csv("output/09b_qb_recal_probabilities.csv")
te   <- read.csv("output/12e_te_recal_probabilities.csv")

pairs <- rbind(
  data.frame(pred = rbwr$p_start_platt_vegas[rbwr$position == "RB"],
             hit  = rbwr$hit_start[rbwr$position == "RB"]),
  data.frame(pred = rbwr$p_boom_platt_vegas[rbwr$position == "RB"],
             hit  = rbwr$hit_boom[rbwr$position == "RB"]),
  data.frame(pred = rbwr$p_start_strat_platt[rbwr$position == "WR"],
             hit  = rbwr$hit_start[rbwr$position == "WR"]),
  data.frame(pred = rbwr$p_boom_strat_iso[rbwr$position == "WR"],
             hit  = rbwr$hit_boom[rbwr$position == "WR"]),
  data.frame(pred = qb$p_start_platt_vol_vegas, hit = qb$hit_start),
  data.frame(pred = qb$p_boom_platt,            hit = qb$hit_boom),
  data.frame(pred = te$p_start_platt_vol_vegas, hit = te$hit_start),
  data.frame(pred = te$p_boom_platt_vegas,      hit = te$hit_boom)
)
pairs <- pairs[!is.na(pairs$pred) & !is.na(pairs$hit), ]
pairs$hit <- as.logical(pairs$hit)

pairs$bin <- cut(pairs$pred, breaks = seq(0, 1, 0.1),
                 include.lowest = TRUE, right = FALSE)
cal <- aggregate(cbind(pred, hit) ~ bin, data = pairs, FUN = mean)
cal$n <- as.vector(table(pairs$bin)[as.character(cal$bin)])
cal <- cal[cal$n >= 100, ]

cat("rows pooled:", nrow(pairs), "\n")
cat("max |pred - emp| (pp), bins n>=100:",
    round(100 * max(abs(cal$pred - cal$hit)), 2), "\n")
print(data.frame(bin = as.character(cal$bin), n = cal$n,
                 pred = round(100 * cal$pred, 1),
                 emp  = round(100 * cal$hit, 1)))

p2 <- ggplot(cal, aes(x = 100 * pred, y = 100 * hit)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = ink2, linewidth = 0.4) +
  geom_point(color = blue_dk, size = 3) +
  annotate("text", x = 62, y = 47, label = "perfect honesty",
           color = ink2, size = 3.3, angle = 37) +
  scale_x_continuous(limits = c(0, 90), breaks = seq(0, 80, 20),
                     labels = function(x) paste0(x, "%")) +
  scale_y_continuous(limits = c(0, 90), breaks = seq(0, 80, 20),
                     labels = function(x) paste0(x, "%")) +
  coord_equal() +
  labs(
    title = "When it says 60%, it happens 60% of the time",
    subtitle = "Every published probability, 2016-2025 backtest,\ngrouped by what the model said.",
    x = "What the model said", y = "How often it actually happened",
    caption = paste0("All four positions, start + boom thresholds, ",
                     "deployed probability maps. Bins with 100+ player-weeks.")
  ) +
  theme_teaser

ggsave("content/img/teaser_calibration.png", p2,
       width = 6.2, height = 6.2, dpi = 150, bg = surface)

cat("done\n")
