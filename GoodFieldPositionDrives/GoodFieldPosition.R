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

drive_and_field_position <- pbp |> 
  filter(!is.na(drive)) |> 
  group_by(game_id, drive) |> 
  filter(play_type != "kickoff") |> 
  summarize(
    team = first(posteam),
    drive_start_yard_line = first(drive_start_yard_line),
    yards_to_go = first(yardline_100),
    td_team = ifelse(!is.na(last(td_team)), last(td_team), NA),
    touchdown = sum(touchdown) * 6,
    extra_point = ifelse(last(extra_point_result) == "good", 1, 0),
    fg = ifelse(last(field_goal_result) == "made", 3, 0),
    wp = first(wp)
  ) |> 
  filter(wp < 0.95 & wp > 0.05) |> 
  mutate(
    extra_point = ifelse(is.na(extra_point), 0, extra_point),
    fg = ifelse(is.na(fg), 0, fg),
    touchdown = ifelse(!is.na(td_team) & td_team != team, 0, touchdown),
    drive_points = touchdown + extra_point + fg,
    drive_points = ifelse(is.na(drive_points), 0, drive_points))

good_field_position <- drive_and_field_position |> 
  filter(yards_to_go <= 60) |> 
  group_by(team) |> 
  summarize(
    team = first(team),
    avg_points = mean(drive_points),
    avg_yards_to_go = mean(yards_to_go),
    num_drives = n()
  ) |> 
  left_join(teams_colors_logos, by = c("team" = "team_abbr"))

good_field_position |> 
  ggplot(aes(x = fct_reorder(team, avg_points), y = avg_points)) +
  geom_bar(aes(fill = team_color, color = team_color2), 
           stat = "identity", alpha = 0.9, width = 0.85) + 
  scale_color_identity(aesthetics = c("fill", "color")) +
  theme_classic() +
  geom_text(aes(label = round(avg_points, 1)), vjust = 1.5, size = 3.5, color = "white") +
  labs(x = "Team",
       y = "Points Per Drive",
       title = "How Effective are NFL Teams at Capitalizing on \n Good Field Position?",
       subtitle = "Average points teams score on drives starting at their own 40 or better\n Weeks 1-6 2023, excl. garbage time",
       caption = "By: Harvin M. | @Themumster7 | nflreadr") +
  theme(plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        panel.grid.major.y = element_line(linetype = "dashed"),
        axis.text.x = element_nfl_logo(size = 0.73))

ggsave('points_in_good_field_position.png', width = 9, height = 7, dpi = "retina")

good_field_position |> 
  ggplot(aes(x = avg_yards_to_go, y = avg_points)) + 
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
  scale_x_reverse() + 
  geom_hline(yintercept = mean(good_field_position$avg_points), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mean(good_field_position$avg_yards_to_go), linetype = "dashed", color = "red") +
  theme_bw() + 
  scale_color_identity(aesthetics = c("fill", "color")) +
  labs(x = "Avg. Yards to Go",
       y = "Points Per Drive",
       title = "How Effective are NFL Teams at Capitalizing on \n Good Field Position?",
       subtitle = "Average points teams score on drives starting at their own 40 or better\n Weeks 1-7 2023, excl. garbage time",
       caption = "By: Harvin M. | @Themumster7 | nflreadr") +
  theme(plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 16, hjust = 0.5))

ggsave('good_field_position_averages.png', width = 9, height = 7, dpi = "retina")

  
  
  
  
