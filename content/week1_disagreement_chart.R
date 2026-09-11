# content/week1_disagreement_chart.R -- dot/strip chart for "The Model
# Didn't Get The Memo" (Substack, 2026 W1)
# Run from repo root: Rscript content/week1_disagreement_chart.R
#
# Five named flex-decision-zone players (model_rank RB/WR 20-39, QB/TE
# 10-19 per CONTENT_GUIDE.md's decision-relevant tier) where the model and
# FantasyPros ECR disagree most this week, pulled live from
# output/10d_ecr_gap_2026_w01.csv -- not hardcoded, so this stays accurate
# if the board refreshes before publish. Pre-game rank comparison ONLY;
# actual outcomes (e.g. Deebo Samuel's box score) stay out of this chart
# and in the surrounding prose -- CONTENT_GUIDE.md's rule that descriptive
# stats stay visually separate from model probability output.

library(ggplot2)
library(dplyr)
library(readr)

dir.create("content/img", showWarnings = FALSE, recursive = TRUE)

surface   <- "#fcfcfb"
ink       <- "#0b0b0b"
ink2      <- "#52514e"
ink3      <- "#898781"
blue_dk   <- "#2a78d6"
orange_dk <- "#c1541c"

NAMES <- c("Tyler Warren", "Terry McLaurin", "Kayshon Boutte",
           "Caleb Douglas", "Deebo Samuel Sr.")

d <- read_csv("output/10d_ecr_gap_2026_w01.csv", show_col_types = FALSE) |>
  filter(player_name %in% NAMES) |>
  distinct(player_name, posteam, .keep_all = TRUE) |>
  mutate(
    direction = if_else(rank_gap > 0, "Model higher than ECR", "Model lower than ECR"),
    label = paste0(player_name, " (", position, ", ", posteam, ")")
  ) |>
  arrange(rank_gap)

stopifnot(nrow(d) == length(NAMES))  # loud if a name drops off the board before publish

d$label <- factor(d$label, levels = d$label)
col_map <- c("Model higher than ECR" = blue_dk, "Model lower than ECR" = orange_dk)

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
    axis.text     = element_text(color = ink2, size = 10.5),
    axis.title    = element_text(color = ink2, size = 10.5),
    legend.position = "top",
    legend.title  = element_blank(),
    legend.text   = element_text(color = ink2, size = 9.5)
  )

p <- ggplot(d, aes(x = rank_gap, y = label)) +
  geom_vline(xintercept = 0, color = ink3, linewidth = 0.4, linetype = "dashed") +
  geom_segment(aes(x = 0, xend = rank_gap, y = label, yend = label),
               color = ink3, linewidth = 0.5) +
  geom_point(aes(color = direction), size = 4) +
  geom_text(aes(label = paste0("model ", model_rank, " / ECR ", ecr_rank)),
            hjust = if_else(d$rank_gap > 0, -0.15, 1.15),
            color = ink, size = 3.3) +
  scale_color_manual(values = col_map) +
  scale_x_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), x),
                      limits = c(min(d$rank_gap) - 32, max(d$rank_gap) + 38)) +
  labs(
    title = "Week 1: Where The Model And The Crowd Split",
    subtitle = paste0("Flex-decision-zone players (model rank RB/WR 20-39, QB/TE 10-19)\n",
                       "with the biggest model-vs-ECR rank gap this week."),
    x = "Rank gap (positive = model ranks them better than ECR does)",
    y = NULL,
    caption = paste0("output/10d_ecr_gap_2026_w01.csv -- pre-game ranks only, not a claimed edge.\n",
                     "Data: FantasyPros ECR. github.com/merrittocratic/boxscore-prophet")
  ) +
  theme_dot

ggsave("content/img/week1_disagreement_dotplot.png", p,
       width = 9.5, height = 5.6, dpi = 150, bg = surface)

cat("done\n")
