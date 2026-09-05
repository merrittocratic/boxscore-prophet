# content/rb_r1_bellcow_chart.R -- dot/strip chart for "Your Draft Board Is
# Somebody Else's Problem" (Substack, 2026-09-01)
# Run from repo root: Rscript content/rb_r1_bellcow_chart.R
#
# Every first-round rookie RB in the shipped model's eval window, plotted by
# how far their actual P(15+ PPR) rate landed from what the model stated
# before role was known (output/16a_rb_r1_per_player.csv, built by
# R/oneoff/16a_rb_r1_per_player.R). Bell-cow beats and committee misses land
# on opposite ends of the same draft-capital tier -- the bracket at top shows
# that full observed range as the honest answer for Jeremiyah Love's 2026
# rookie season, still unresolved at kickoff.

library(ggplot2)

dir.create("content/img", showWarnings = FALSE, recursive = TRUE)

surface   <- "#fcfcfb"
ink       <- "#0b0b0b"
ink2      <- "#52514e"
ink3      <- "#898781"
blue_dk   <- "#2a78d6"
orange_dk <- "#c1541c"
gray_wash <- "#9a988f"

d <- read.csv("output/16a_rb_r1_per_player.csv")

d$bucket <- ifelse(d$delta_pp >= 10, "Bell-cow beat",
             ifelse(d$delta_pp <= -10, "Committee miss", "Wash"))
d$bucket <- factor(d$bucket, levels = c("Bell-cow beat", "Wash", "Committee miss"))

d$label <- paste0(d$player_name, " (", d$rookie_season, ")")
d <- d[order(d$delta_pp), ]
d$label <- factor(d$label, levels = d$label)

col_map <- c("Bell-cow beat" = blue_dk, "Wash" = gray_wash, "Committee miss" = orange_dk)

LOVE_Y   <- nlevels(d$label) + 1.3
RANGE_LO <- min(d$delta_pp)
RANGE_HI <- max(d$delta_pp)

theme_dot <- theme_minimal(base_size = 13) +
  theme(
    plot.background     = element_rect(fill = surface, color = NA),
    panel.background    = element_rect(fill = surface, color = NA),
    panel.grid.minor    = element_blank(),
    panel.grid.major.y  = element_blank(),
    panel.grid.major.x  = element_line(color = "#e8e7e3", linewidth = 0.3),
    plot.title.position = "plot",
    plot.title    = element_text(color = ink, face = "bold", size = 15),
    plot.subtitle = element_text(color = ink2, size = 10.5),
    plot.caption  = element_text(color = ink3, size = 8.3),
    axis.text     = element_text(color = ink2, size = 10),
    axis.title    = element_text(color = ink2, size = 10.5),
    legend.position = "top",
    legend.title  = element_blank(),
    legend.text   = element_text(color = ink2, size = 9.5)
  )

p <- ggplot(d, aes(x = delta_pp, y = label)) +
  geom_vline(xintercept = 0, color = ink3, linewidth = 0.4, linetype = "dashed") +
  annotate("segment", x = RANGE_LO, xend = RANGE_HI, y = LOVE_Y, yend = LOVE_Y,
           color = ink2, linewidth = 1.1, lineend = "butt") +
  annotate("segment", x = c(RANGE_LO, RANGE_HI), xend = c(RANGE_LO, RANGE_HI),
           y = LOVE_Y - 0.18, yend = LOVE_Y + 0.18, color = ink2, linewidth = 1.1) +
  annotate("text", x = (RANGE_LO + RANGE_HI) / 2, y = LOVE_Y + 0.55,
           label = "Jeremiyah Love: somewhere in here",
           color = ink, size = 3.7, fontface = "italic") +
  geom_point(aes(color = bucket), size = 3.6) +
  scale_color_manual(values = col_map) +
  scale_x_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "pp"),
                      limits = c(RANGE_LO - 6, RANGE_HI + 6)) +
  scale_y_discrete(expand = expansion(add = c(0.6, 2.4))) +
  labs(
    title = "Same Draft Slot, Opposite Outcomes",
    subtitle = paste0("First-round rookie RBs: actual startable-week rate vs. what the\n",
                       "model said before role was known. n=13, shipped model (volfix baseline)."),
    x = "Deviation from stated pre-season P(15+ PPR)",
    y = NULL,
    caption = "output/16a_rb_r1_per_player.csv -- R/oneoff/16a_rb_r1_per_player.R"
  ) +
  theme_dot

ggsave("content/img/rb_r1_bellcow_dotplot.png", p,
       width = 8.5, height = 7.6, dpi = 150, bg = surface)

cat("done\n")
