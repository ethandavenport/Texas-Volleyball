# Kill PCT

# Libraries
library(datavolley)
library(tidyverse)
library(moderndive)
library(mosaic)
library(dplyr)
library(base)
library(ggplot2)


data0 %>%
  filter(skill_subtype == "Tip") %>%
  group_by(team) %>%
  summarise(kill_pct = prop(winning_attack),
            num_tips = count(team)) %>%
  filter(num_tips >= 101) %>%
  arrange(desc(kill_pct)) %>%
  head(20)
  # Texas(.315) comes in at no. 3, behind Purdue(.322) and Kentucky(.317)
  # These were the only three teams to kill at least .300 of their tips

# compared to non-tip attacks
data0 %>%
  filter(skill == "Attack", skill_subtype != "Tip", attack_description != "Setter Dump") %>%
  group_by(team) %>%
  summarise(kill_pct = prop(winning_attack),
            num_attacks = count(team)) %>%
  filter(num_attacks >= 500) %>%
  arrange(desc(kill_pct)) %>%
  head(20)
  # Texas(.497) comes in easily at no. 1
  # Following them is San Diego(.442), Louisville(.440), Marquette(.439)

