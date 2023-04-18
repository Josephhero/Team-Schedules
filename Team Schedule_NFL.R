library(dplyr)
library(nflfastR)
library(nflplotR)
library(nflreadr)
library(ggthemes)
library(ggplot2)
library(stringr)
library(lubridate)
library(tidyverse)
library(grid)
library(ggnewscale)
# dataset -----------------------------------------------------------------

image_path = "/Users/Firefly/Desktop/_iMac_Drive/Joseph/Coding/NFL/RStudio/Images"

team = "BUF"
season = 2022
# all times are set as eastern time zone, so set a differential based on that number
time_zone_diff = -2
time_zone_name = "CST"
dst_observed = "y"

sched <- load_schedules(season)

df <- sched %>% 
  filter((away_team == team | home_team == team) & game_type == "REG")
  
df1 <- df %>% 
  mutate(opponent = ifelse(home_team==team, df$away_team, home_team)) %>% 
  mutate(hour = substr(gametime, 1, 2)) %>%
  mutate(minute = substr(gametime, 4, 6)) %>% 
  mutate(gameday = parse_date_time(gameday, "ymd")) %>% 
  mutate(gamedate = format(gameday, format="%a, %b %d")) %>% 
  mutate(location = ifelse(home_team==team, "", "@")) %>%
  select(season, week, gameday, gamedate, weekday, gametime, hour, minute, opponent, location)

df1[,7] <- lapply(df1[,7], as.numeric)

df2 <- df1 %>% 
  mutate(my_hour = hour + time_zone_diff) %>% 
  # uncomment this line and change the above to 'my_hour1' for location that doesn't observe DST
  #mutate(my_hour = ifelse(week >= 9, my_hour1 + 1, my_hour1)) %>% 
  mutate(am_pm = ifelse(my_hour >11, "PM", "AM")) %>% 
  mutate(new_hour = ifelse(my_hour > 12, my_hour - 12, my_hour)) %>% 
  mutate(game_time = paste(new_hour, ":", minute, " ", am_pm, sep = "")) %>% 
  select(week, gamedate, game_time, hour, my_hour, opponent, location)

bye = setdiff(1:17, df2$week)

df3 <- df2%>%
  add_row(week=bye, opponent=team) %>% 
  arrange(week)

df4 <- df3 %>% 
  mutate(xmin = ifelse(week > 9, 18, 1)) %>% 
  mutate(xmax = ifelse(week > 9, 33, 16)) %>% 
  mutate(ymin = ifelse(week > 9, (week - 9) * 5, week * 5) -1) %>% 
  mutate(ymax = ymin + 4) %>% 
  select(week, gamedate, game_time, opponent, location, xmin, xmax, ymin, ymax)

# we want to use the team_color and team_color2 from teams_colors_logos, 
# but they're not always the same as the primary and secondary colors 
# from scale_x_nfl(). We want our schedules to match, so we're going 
# to create a bar plot with the primary and secondary colors, then 
# extract the colors and bind that df to the teams_colors_logos df as 
# "primary" and "secondary", so our background colors and team name colors 
# match the rest of the plot. 
p <- ggplot() + 
  geom_bar(data=teams_colors_logos, aes(x=team_abbr, fill=team_abbr, color=team_abbr, size=0.3)) + 
  scale_fill_nfl("primary") + 
  scale_color_nfl("secondary")

bar_colors <- ggplot_build(p)$data[[1]]

colors <- bar_colors %>% 
  rename(primary=fill, secondary=colour) %>% 
  select(primary, secondary)

primary_colors_logos <- cbind(teams_colors_logos, colors) %>% 
  relocate(primary, secondary, .before=team_color)


# Now that we have done all that, we will join the primary_colors_logos to df4
teams <- df4 %>%
  left_join(primary_colors_logos, by = c('opponent' = 'team_abbr'))

# get a df of just our team
my_team_df <- teams %>% 
  filter(opponent == team) 

# assign variables for our team
my_color = my_team_df$primary[1]
my_color2 = my_team_df$secondary[1]
my_team_nick = my_team_df$team_nick[1]

# set a gradient for the background, and number of color changes in gradient
grad = colorRampPalette(c("gray10", my_color, "gray10"))(50)
x_center = max(df4$xmax/2)
y_center = max(df4$ymax/2)
team_schedule <- ggplot() + 
  annotation_custom(rasterGrob(t(grad), width=unit(1, "npc"), height=unit(1, "npc"))) + 
  annotate("label", x = 17, y = 1.5, label = paste(my_team_nick, "Schedule", season), color = my_color2, fill = my_color, size = 10, fontface = "bold", label.size = 1) + 
  geom_nfl_logos(data=my_team_df, aes(x=x_center+1, y=y_center+2, team_abbr=opponent, alpha = 0.6), width=0.8) +
  geom_nfl_logos(data=my_team_df, aes(x=x_center+1, y=y_center+2, team_abbr=opponent, alpha = 0.6), width=0.8) +
  geom_rect(data=teams, mapping=aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill = fct_inorder(opponent)), color="black") +
  scale_fill_nfl(type = "primary", guide = "none") +
  scale_y_reverse() +
  geom_nfl_logos(data=teams, aes(x = (xmax - 2), y = (ymin + 2), team_abbr = opponent), width = 0.095) + 
  geom_text(data=teams, aes(x = (xmin + 1), y = (ymin + 1.5), label = gamedate, color = opponent, hjust = "left", fontface = "bold"), size = 6.5) +
  geom_text(data=teams, aes(x = (xmin + 1), y = (ymax - 1.1), label = game_time, color = opponent, hjust = "left", fontface = "bold"), size = 6.5) +
  geom_text(data=teams, aes(x = (xmin + 10.5), y = (ymin + 1.9), label = location, color = opponent, fontface = "bold"), size = 6.5) +
  geom_text(data=my_team_df, aes(x = (xmin + 6.5), y = (ymin + 2), label = "Bye Week", color = opponent, fontface = "bold"), size = 7) +
  scale_color_nfl(type = "secondary", guide = "none") + 
  # ggnewscale package allows you to reset the scale_color and scale_fill options, which 
  # we need to make the "Week X" boxes the opposite colors. We had to put "guide='none'" 
  # in the previous scales to make this work.
  ggnewscale::new_scale_color() + 
  ggnewscale::new_scale_fill() + 
  geom_label(data=teams, aes(x = (xmin + 7.5), y = (ymin + 0.15), label = paste("Week", week), color = opponent, fill = opponent, fontface = "bold"), size = 5) + 
  scale_color_nfl(type = "primary") +
  scale_fill_nfl(type = "secondary") + 
  annotate(geom = "label", label = "Data: @nflfastR | Chart: @josephjefe", x = Inf, y = Inf, hjust = "right", vjust = "bottom", fill = "black", color = "white") + 
  annotate(geom = "label", label = paste("Time Zone: ", time_zone_name), x = -Inf, y = Inf, hjust = "left", vjust = "bottom", fill = "black", color = "white") + 
  theme_void()

team_schedule

ggsave(paste(my_team_nick, " Schedule ", season, " ", time_zone_name, ".png", sep = ""), team_schedule, path = image_path)



  




