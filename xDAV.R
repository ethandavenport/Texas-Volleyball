# Expected Determined Attack Value (xDAV)

# Libraries
library(tidyverse)
library(mosaic)

# create pre-attack dataframe with only the information we need:
# our three pieces of information: setup, attack_description, and number of blockers
# the opposing team, so we can adjust for it later
# info about what actually happened: evaluation_code, dig_eval, and point_won_by
# FYI: the 'attacks' dataframe was created in the previous R file called 'DAV'

pre_attack <- attacks %>%
  mutate(setup = ifelse(!is.na(dig_setup), paste("D", dig_setup),
                        ifelse(!is.na(reception_setup), paste("R", reception_setup),
                               ifelse(!is.na(freeball_setup), paste("F", freeball_setup), 
                                      ifelse(!is.na(overpass_setup), overpass_setup, NA))))) %>%
  mutate(
    setup = replace_na(setup, "missing"),
    attack_description = replace_na(attack_description, "missing"),
    num_players = replace_na(num_players, "missing")
  ) %>%
  select(player_name, team, setup, attack_description, num_players, point_won_by, 
         evaluation_code, dig_eval, opp_team) %>%
  mutate(team_won = team == point_won_by) %>%
  select(-point_won_by)

# create DAV formula list
DAV_formula = list("blocked #"=0.580, "other"=0.538, "blocked +"=0.515, "blocked -"=0.354)

# create xTP (Expected Transition Points - expected value of a dig) formula list
xTP_formula = list("d#"=0.604, "d+"=0.514, "d-"=0.358)

# find expected DAV for each situation (group of setup, attack_description, and num_players)
sDAV <- pre_attack %>%
  group_by(setup, attack_description, num_players) %>%
  summarize(sDAV = 
              1*prop(evaluation_code == "#") +
              (1-xTP_formula[["d-"]])*prop(dig_eval == "-") +
              DAV_formula[["blocked #"]]*prop(dig_eval == "blocked #") +
              DAV_formula[["other"]]*prop(dig_eval == "other") +
              DAV_formula[["blocked +"]]*prop(dig_eval == "blocked +") +
              (1-xTP_formula[["d+"]])*prop(dig_eval == "+") +
              (1-xTP_formula[["d#"]])*prop(dig_eval == "#") +
              DAV_formula[["blocked -"]]*prop(dig_eval == "blocked -") +
              0*prop(dig_eval == "blocked =") +
              0*prop(evaluation_code == "=") +
              0*prop(evaluation_code == "/" & dig_eval == "none"),
            count = n()) %>%
  filter(count >= 30) %>%
  select(-count)

# add this value on to the pre_attack data
pre_attack <- left_join(pre_attack, sDAV, by = c("setup","attack_description","num_players"))

# how often is there some alternative situation?
nrow(pre_attack %>% filter(is.na(sDAV))) / nrow(pre_attack)

# group all other situations into one group and find the average DAV
nasituation_DAV <- pre_attack %>% 
  filter(is.na(sDAV)) %>%
  summarise(
    DAV =
      1*prop(evaluation_code == "#") +
      (1-xTP_formula[["d-"]])*prop(dig_eval == "-") +
      DAV_formula[["blocked #"]]*prop(dig_eval == "blocked #") +
      DAV_formula[["other"]]*prop(dig_eval == "other") +
      DAV_formula[["blocked +"]]*prop(dig_eval == "blocked +") +
      (1-xTP_formula[["d+"]])*prop(dig_eval == "+") +
      (1-xTP_formula[["d#"]])*prop(dig_eval == "#") +
      DAV_formula[["blocked -"]]*prop(dig_eval == "blocked -") +
      0*prop(dig_eval == "blocked =") +
      0*prop(evaluation_code == "=") +
      0*prop(evaluation_code == "/" & dig_eval == "none"))

# fill NA values with this one value
pre_attack$sDAV[is.na(pre_attack$sDAV)] <- nasituation_DAV[1]

# for each defense, what DAV do they allow to their opponents on average?
oDAV <- pre_attack %>%
  group_by(opp_team) %>%
  summarise(
    oDAV = 
      1*prop(evaluation_code == "#") +
      (1-xTP_formula[["d-"]])*prop(dig_eval == "-") +
      DAV_formula[["blocked #"]]*prop(dig_eval == "blocked #") +
      DAV_formula[["other"]]*prop(dig_eval == "other") +
      DAV_formula[["blocked +"]]*prop(dig_eval == "blocked +") +
      (1-xTP_formula[["d+"]])*prop(dig_eval == "+") +
      (1-xTP_formula[["d#"]])*prop(dig_eval == "#") +
      DAV_formula[["blocked -"]]*prop(dig_eval == "blocked -") +
      0*prop(dig_eval == "blocked =") +
      0*prop(evaluation_code == "=") +
      0*prop(evaluation_code == "/" & dig_eval == "none"),
    count = n()) %>%
  filter(count >= 200) %>%
  select(-count) %>%
  arrange(oDAV)

# how often is there some alternative rare team?
nrow(pre_attack %>% filter(!opp_team %in% oDAV$opp_team)) / nrow(pre_attack)

# group all of these rare teams into one group and find the average opponent DAV
nateam_DAV <- pre_attack %>%
  filter(!opp_team %in% oDAV$opp_team) %>%
  summarise(
    oDAV = 
      1*prop(evaluation_code == "#") +
      (1-xTP_formula[["d-"]])*prop(dig_eval == "-") +
      DAV_formula[["blocked #"]]*prop(dig_eval == "blocked #") +
      DAV_formula[["other"]]*prop(dig_eval == "other") +
      DAV_formula[["blocked +"]]*prop(dig_eval == "blocked +") +
      (1-xTP_formula[["d+"]])*prop(dig_eval == "+") +
      (1-xTP_formula[["d#"]])*prop(dig_eval == "#") +
      DAV_formula[["blocked -"]]*prop(dig_eval == "blocked -") +
      0*prop(dig_eval == "blocked =") +
      0*prop(evaluation_code == "=") +
      0*prop(evaluation_code == "/" & dig_eval == "none"))

# add the team's opponent DAV on to the pre_attack data
# additionally, fill any attack against rare teams with this grouped value
# next, create a new column for a defense's DAV allowed above/below average
pre_attack <- 
  left_join(pre_attack, oDAV, by = "opp_team") %>% 
  mutate(oDAV = ifelse(is.na(oDAV), as.numeric(nateam_DAV), oDAV),
         aoDAV = oDAV - mean(oDAV))

# finally, define xDAV column as the situation (sDAV) plus the difficulty of opponent (aoDAV)
pre_attack <- pre_attack %>% mutate(xDAV = as.numeric(sDAV) + as.numeric(aoDAV))

# take a view at the breakdown of xDAV
attach(pre_attack)
hist(xDAV, breaks = 26, ylab = "", yaxt = "n")


