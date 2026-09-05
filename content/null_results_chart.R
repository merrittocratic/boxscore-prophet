# null_results_chart.R -- quadrant chart for the "Nothing to See Here" null article
# Run from repo root: Rscript content/null_results_chart.R
#
# Every honesty-bar miss big enough to tempt a finding this summer, plotted by
# miss size (percentage points) against sample size (player-weeks). All five
# clear the 4-point honesty bar but miss the 400-week floor, so all five sit
# in the watch registry rather than shipping as findings.

library(ggplot2)
library(ggrepel)

dir.create("content/img", showWarnings = FALSE, recursive = TRUE)

surface <- "#fcfcfb"
ink     <- "#0b0b0b"
ink2    <- "#52514e"
ink3    <- "#898781"
blue_dk <- "#2a78d6"

FLOOR <- 400
BAR   <- 4
XMAX  <- 480
YMAX  <- 13

pts <- data.frame(
  label = c("QB -- cold games", "TE -- rain", "QB -- high wind",
            "RB -- Day 1 rookies", "WR -- high wind"),
  n     = c(46, 68, 107, 182, 265),
  miss  = c(9.5, 11.4, 8.6, 10.6, 6.6)
)

theme_null <- theme_minimal(base_size = 13) +
  theme(
    plot.background     = element_rect(fill = surface, color = NA),
    panel.background    = element_rect(fill = surface, color = NA),
    panel.grid.minor    = element_blank(),
    panel.grid.major.x  = element_blank(),
    panel.grid.major.y  = element_line(color = "#e1e0d9", linewidth = 0.3),
    plot.title.position = "plot",
    plot.title    = element_text(color = ink, face = "bold", size = 15),
    plot.subtitle = element_text(color = ink2, size = 10.5),
    axis.text     = element_text(color = ink3),
    axis.title    = element_text(color = ink2, size = 10.5)
  )

p <- ggplot(pts, aes(x = n, y = miss)) +
  geom_vline(xintercept = FLOOR, color = ink3, linewidth = 0.4) +
  geom_hline(yintercept = BAR, color = ink3, linewidth = 0.4) +
  annotate("text", x = FLOOR - 8, y = YMAX - 0.4, label = "400-week floor",
           hjust = 1, vjust = 1, color = ink2, size = 3.5, fontface = "bold") +
  annotate("text", x = XMAX - 6, y = BAR + 0.35, label = "4-pt honesty bar",
           hjust = 1, vjust = 0, color = ink2, size = 3.5, fontface = "bold") +
  annotate("text", x = 8, y = BAR + 0.5,
           label = "WATCH REGISTRY -- parked, not answerable yet",
           hjust = 0, vjust = 0, color = ink2, size = 3.6, fontface = "bold") +
  annotate("text", x = XMAX - 6, y = YMAX - 0.4, label = "would ship",
           hjust = 1, vjust = 1, color = ink3, size = 3.3, fontface = "italic") +
  annotate("text", x = XMAX - 6, y = 0.3, label = "settled null",
           hjust = 1, vjust = 0, color = ink3, size = 3.3, fontface = "italic") +
  annotate("text", x = 8, y = 0.3, label = "too soon to say",
           hjust = 0, vjust = 0, color = ink3, size = 3.3, fontface = "italic") +
  geom_point(size = 4.2, color = blue_dk) +
  geom_text_repel(aes(label = label), color = ink2, size = 3.8, seed = 42,
                   box.padding = 0.6, point.padding = 0.3,
                   segment.color = ink3, segment.size = 0.3,
                   min.segment.length = 0.2) +
  scale_x_continuous(limits = c(0, XMAX), expand = c(0, 0),
                      breaks = seq(0, 400, 100)) +
  scale_y_continuous(limits = c(0, YMAX), expand = c(0, 0)) +
  labs(
    title = "Every Temptation Landed the Same Place",
    subtitle = paste("Five effects big enough to clear the honesty bar this summer",
                      "-- none from a sample big enough to trust."),
    x = "Sample size (player-weeks)",
    y = "Miss size (percentage points)"
  ) +
  theme_null

ggsave("content/img/null_results_temptations.png", p,
       width = 9, height = 6, dpi = 150, bg = surface)

cat("done\n")
