library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ggthemes)
library(ggrepel)
library(scales)
library(gtExtras)
library(nflreadr)

player_stats <- load_player_stats(2023)

wr_targets <- player_stats |>
  filter(position == "WR") |> 
  group_by(player_id) |> 
  summarize(name = first(player_name), 
            team = last(recent_team),
            games = n(),
            total_targets = sum(targets),
            targets_per_game = mean(targets),
            epa_target = mean(receiving_epa, na.rm = T) / sum(total_targets)) |>
  filter(total_targets >= 20) |> 
  left_join(teams_colors_logos, by = c("team" = "team_abbr"))

wr_targets |> 
  ggplot(aes(x = targets_per_game, y = epa_target)) +
  geom_point(aes(fill = team_color, color = team_color2),
             shape = 21, alpha = 0.9, size = 3) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  geom_text_repel(aes(label = name), size = 3, max.overlaps = Inf) +
  theme_bw() + 
  geom_hline(yintercept = mean(wr_targets$epa_target), linetype = "dashed") +
  geom_vline(xintercept = mean(wr_targets$targets_per_game), linetype = "dashed") +
  labs(x = "Targets Per Game", y = "EPA/Target",
       title = "EPA/Target and Targets per Game for WRs",
       subtitle = "Weeks 1-5 2023, min. 20 targets",
       caption = "By: Harvin M. | @Themumster7 | nflreadR") +
  theme(plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 16, hjust = 0.5))

ggsave('epa-target-wrs.png', width = 9, height = 7, dpi = "retina")
