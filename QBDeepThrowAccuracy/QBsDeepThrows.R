library(tidyverse)
library(ggimage)
library(gt)
library(ggthemes)
library(ggrepel)
library(scales)
library(gtExtras)
library(nflreadr)
library(nflfastR)
library(nflplotR)

pbp <- load_pbp(2023)

qb_deep_throws <- pbp |> 
  filter(pass == 1) |> 
  filter(!is.na(epa)) |>
  filter(air_yards > 15) |> 
  group_by(id) |> 
  summarize(
    name = first(name),
    team = last(posteam),
    pass_attempts = n(),
    epa_play = mean(qb_epa),
    cpoe = mean(cpoe, na.rm = T)
  ) |> 
  filter(pass_attempts >= 15) |> 
  left_join(teams_colors_logos, by = c("team" = "team_abbr"))

qb_deep_throws |> 
  ggplot(aes(x = cpoe, y = epa_play)) +
  geom_point(aes(fill = team_color, color = team_color2, size = pass_attempts),
             shape = 21, alpha = 0.9) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  geom_text_repel(aes(label = name), max.overlaps = Inf) +
  theme_fivethirtyeight() +
  geom_hline(yintercept = mean(qb_deep_throws$epa_play), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mean(qb_deep_throws$cpoe), linetype = "dashed", color = "red") +
  labs(x = "CPOE (Completion Percentage over Expected)", y = "EPA / Play",
       title = "QB Efficiency on Deep Passes (15+ Air Yards)", 
       subtitle = "Weeks 1-8 2023, minimum 15 attempts",
       size = "Number of Deep Pass Attempts:",
       caption = "By: Harvin M. | @Themumster7 | nflreadr") +
  scale_x_continuous(breaks = pretty_breaks(n = 8)) +
  scale_y_continuous(breaks = pretty_breaks(n = 8)) +
  theme(plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 16, hjust = 0.5), 
        axis.title = element_text(),
        legend.position = "bottom",
        legend.direction = "horizontal")

ggsave('qb_deep_ball_accuracy.png', width = 9, height = 7, dpi = "retina")
