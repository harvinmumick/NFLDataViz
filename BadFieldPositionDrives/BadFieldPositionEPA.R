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
    wp = first(wp),
    epa_play = mean(epa)
  ) |> 
  filter(wp < 0.95 & wp > 0.05) |> 
  mutate(
    extra_point = ifelse(is.na(extra_point), 0, extra_point),
    fg = ifelse(is.na(fg), 0, fg),
    touchdown = ifelse(!is.na(td_team) & td_team != team, 0, touchdown),
    drive_points = touchdown + extra_point + fg,
    drive_points = ifelse(is.na(drive_points), 0, drive_points))

bad_field_position <- drive_and_field_position |> 
  filter(yards_to_go >= 80) |> 
  group_by(team) |> 
  summarize(
    team = first(team),
    num_drives = n(),
    avg_points = mean(drive_points),
    epa_play = mean(epa_play)
  ) |> 
  left_join(teams_colors_logos, by = c("team" = "team_abbr"))

bad_field_position |> 
  ggplot(aes(x = epa_play, y = avg_points)) + 
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
  geom_hline(yintercept = mean(bad_field_position$avg_points), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mean(bad_field_position$epa_play), linetype = "dashed", color = "red") +
  theme_bw() + 
  scale_color_identity(aesthetics = c("fill", "color")) +
  labs(x = "EPA / Play",
       y = "Points Per Drive",
       title = "How Effective are NFL Teams when Pinned \nDeep in their Own Territory?",
       subtitle = "Pinned Territory: Offense starting inside their own 20 \n Weeks 1-7 2023, excl. garbage time",
       caption = "By: Harvin M. | @Themumster7 | nflreadr") +
  theme(plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 16, hjust = 0.5))

ggsave('bad_field_position_averages.png', width = 9, height = 7, dpi = "retina")

field_position_counts <- bad_field_position |>
  arrange(-avg_points) |> 
  mutate(rank = row_number()) |> 
  dplyr::select(rank, team_wordmark, num_drives, epa_play, avg_points) |>
  mutate(epa_play = round(epa_play, 2),
         avg_points = round(avg_points, 2)) |> 
  gt() |> 
  cols_align(align = "center") |> 
  gt_img_rows((team_wordmark)) |> 
  cols_label(rank = "Rank",
             team_wordmark = "",
             num_drives = "Number of Drives",
             epa_play = "EPA/Play",
             avg_points = "Points per Drive") |> 
  gt_theme_espn() |> 
  gt_hulk_col_numeric(avg_points) |> 
  tab_footnote("By: Harvin M. | @Themumster7 | nflreadr")

gtsave(field_position_counts, "Num_Bad_Field_Position_Drives.png")



