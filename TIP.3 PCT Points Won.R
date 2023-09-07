# PCT Points Won

# Libraries
library(datavolley)
library(tidyverse)
library(moderndive)
library(mosaic)
library(dplyr)
library(base)
library(ggplot2)


# create point-winning team as categorical variable
data0 = mutate(data0, team_won_point = factor(point_won_by))

# pct points won table
data0 %>%
  filter(skill_subtype == "Tip") %>%
  group_by(team) %>%
  summarise(pct_points_won = prop(team_won_point == team),
            num_points = count(team)) %>%
  filter(num_points >= 101) %>%
  arrange(desc(pct_points_won)) %>%
  head(20)
  # Texas leads the way nationally at 63.7%
  # They are followed by Minnesota(62.8%), Purdue(62.7%), Oklahoma(61.8%)

# compared to non-tip attacks
data0 %>%
  filter(skill == "Attack", skill_subtype != "Tip", attack_description != "Setter Dump") %>%
  group_by(team) %>%
  summarise(pct_points_won = prop(team_won_point == team),
            num_points = count(team)) %>%
  filter(num_points >= 500) %>%
  arrange(desc(pct_points_won)) %>%
  head(20)
  # Shockingly, Texas leads the nation yet again at 70.6%
  # They are followed by San Diego(68.8%), Wisconsin(66.2%), Louisville(66.1%)
  # This metric may not be meaningful: this measures how often we won the point
  # when we got a non-tip attack at some point in the rally - likely lots of duplicates

# eliminate duplicates
nodupes_tips = data0 %>%
  filter(skill_subtype == "Tip") %>%
  select(match_id, point_id, team, team_won_point)
nodupes_tips = unique(nodupes_tips)

nodupes_tips %>%
  group_by(team) %>%
  summarise(pct_points_won = prop(team_won_point == team)*100,
            num_points = count(team)) %>%
  filter(num_points >= 101) %>%
  arrange(desc(pct_points_won)) %>%
  head(20)
  # This data does not differ significantly from the data with duplicates
  # Texas leads the way nationally at 63.1%
  # They are followed by Purdue(62.2%), Minnesota(62.1%), Kentucky(61.7%)

