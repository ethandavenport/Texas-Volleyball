# Optimal Distance in terms of Kill% to set the ball. 
# Measured in terms of distance from the setter to the attacker.  Measuring this by Set Code.

# Libraries
library(datavolley)
library(tidyverse)
library(moderndive)
library(mosaic)
library(dplyr)
library(base)
library(ggplot2)


settingresults = data0 %>%
  mutate(set_code = lag(set_code,1),
         set_description = lag(set_description,1),
         set_type = lag(set_type,1)) %>%
  filter(skill == "Attack",
         lag(player_name,1) == "Averi Carlson",
         lag(skill,1) == "Set",
         team == lag(team,1),
         !is.na(set_code)) %>%
  #select(set_code,evaluation) %>%
  group_by(set_code) %>%
  summarise(Kill = round(prop(evaluation_code == "#"),3),
            Positive = round(prop(evaluation_code == "+"),3),
            Poor = round(prop(evaluation_code == "-"),3),
            #BFR = prop(evaluation_code == "!"),
            Blocked = round(prop(evaluation_code == "/"),3),
            Error = round(prop(evaluation_code == "="),3),
            count = count(set_code)) %>%
  mutate(score = round(3*Kill + 2*Positive + Poor,3),
         hitting_pct = round(Kill - Error - Blocked,3)) %>%
  filter(count >= 20) %>%
  arrange(desc(score))

# what player(s) were responsible for a particular type of attack?
data0 %>%
  filter(lag(set_code,1) == "KS",
         lag(set_type,1) == "C",
         team_id == 126) %>%
  group_by(player_name) %>%
  summarise(count = n())
  

write_csv(settingresults, "C:/Users/ethan/OneDrive/Texas Volleyball/Set Distance/SettingResults.csv")


