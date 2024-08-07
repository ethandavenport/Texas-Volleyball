# Determined Attack Value (DAV)

### This assumes you have a VolleyMetrics file named 'data0' ###

# Libraries
library(tidyverse)
library(mosaic)

# mutate variable for opposing team
data0 <- data0 %>% 
  mutate(opp_team = 
           ifelse(team == visiting_team, home_team,
                  ifelse(team == home_team, visiting_team, NA)))

# mutate variable for evaluation code of the preceding dig/reception
data0 <- data0 %>%
  mutate(
    dig_setup = 
      ifelse(
        skill == "Attack" & lag(skill,1) == "Dig" & team == lag(team,1),
        lag(evaluation_code,1), 
        NA))
data0 <- data0 %>%
  mutate(
    dig_setup = 
      ifelse(
        skill == "Attack" & lag(skill,2) == "Dig" & team == lag(team,2),
        lag(evaluation_code,2), 
        dig_setup))

data0 <- data0 %>%
  mutate(
    reception_setup = 
      ifelse(skill == "Attack" & lag(skill,1) == "Reception" & team == lag(team,1),
             lag(evaluation_code,1),
             NA))
data0 <- data0 %>% 
  mutate(
    reception_setup = 
      ifelse(skill == "Attack" & lag(skill,2) == "Reception" & team == lag(team,1) & team == lag(team,2),
             lag(evaluation_code,2), 
             reception_setup))

data0 <- data0 %>% 
  mutate(
    freeball_setup = 
      ifelse(skill == "Attack" & lag(skill,1) == "Freeball" & team == lag(team,1),
             lag(evaluation_code,1), 
             NA))
data0 <- data0 %>% 
  mutate(
    freeball_setup = 
      ifelse(skill == "Attack" & lag(skill,2) == "Freeball" & team == lag(team,1) & team == lag(team,2),
             lag(evaluation_code,2), 
             freeball_setup))

table(data0$evaluation_code[data0$skill == "Dig"])

# mutate variable for evaluation code of the following dig (or whatever else happens)

# no dig present. this could be a kill, stuff block, error, or 'other'
leadna <- data0 %>% 
  mutate(
    dig_eval = "none",
    set_code = lag(set_code,1),
    set_type = lag(set_type,1)) %>%
  filter(
    skill == "Attack",
    is.na(lead(skill,1)) | lead(skill,1) != "Dig",
    is.na(lead(skill,2)) | lead(skill,2) != "Dig")

# dig immediately follows attack
lead1 <- data0 %>% 
  mutate(
    dig_eval = lead(evaluation_code,1),
    set_code = lag(set_code,1),
    set_type = lag(set_type,1)) %>%
  filter(
    skill == "Attack",
    lead(skill,1) == "Dig",
    lead(team,1) == opp_team)

# attack goes through block touch to dig
lead2 <- data0 %>% 
  mutate(
    dig_eval = lead(evaluation_code,2),
    set_code = lag(set_code,1),
    set_type = lag(set_type,1)) %>%
  filter(
    skill == "Attack",
    lead(skill,1) == "Block",
    lead(team,1) == opp_team,
    lead(skill,2) == "Dig",
    lead(team,2) == opp_team)

# blocked back over with some cover by the attacking team
leadblock = data0 %>% 
  mutate(
    dig_eval = paste("blocked", lead(evaluation_code,2)),
    set_code = lag(set_code,1),
    set_type = lag(set_type,1)) %>%
  filter(
    skill == "Attack",
    lead(team,1) == opp_team,
    lead(skill,1) == "Block",
    lead(team,2) == team, 
    lead(skill,2) == "Dig")

other <- leadna %>% filter(!evaluation_code %in% c("#","/","=")) %>% mutate(dig_eval = "other")
leadna <- leadna %>% filter(evaluation_code %in% c("#","/","="))
attacks <- rbind(leadna,lead1,lead2,leadblock,other)

nrow(other) / nrow(attacks)

# are different types of 'other' outcomes significantly different?
data0 %>%
  filter(
    skill == "Attack",
    lead(skill) == "Block",
    lead(team) == opp_team,
    lead(skill,2) == "Attack",
    lead(team,2) == opp_team) %>%
  summarise(prop(team == point_won_by))

# how many instances of various types of 'other' attacks?
data0 %>%
  filter(
    skill == "Attack",
    lead(skill) == "Block",
    lead(team) == opp_team,
    lead(skill,2) == "Attack",
    lead(team,2) == opp_team
  ) %>%
  summarize(count = n())

# see how evaluation code of attack and evaluation code of dig compare
attacks %>%
  group_by(evaluation_code, dig_eval) %>%
  summarise(count = n()) %>%
  filter(count > 3)

# value of a blocked (not for point) attack
leadblock %>% 
  group_by(dig_eval) %>%
  summarise(prop = prop(team == point_won_by), 
            count = n())

# value of other attack situation
other %>% summarise(prop = prop(team == point_won_by))

# create DAV formula list
DAV_formula = list("blocked #"=0.580, "other"=0.538, "blocked +"=0.515, "blocked -"=0.354)

# value of a dig
data0 %>%
  filter(skill == "Dig") %>%
  group_by(evaluation_code) %>%
  summarize(prop = prop(team == point_won_by),
            count = n())

# create xTP (Expected Transition Points - expected value of a dig) formula list
xTP_formula = list("d#"=0.604, "d+"=0.514, "d-"=0.358)

# evaluate players based on DAV
attacks %>%
  group_by(player_name, team) %>%
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
      0*prop(evaluation_code == "/" & dig_eval == "none"),
    prop_sum = 
      prop(evaluation_code == "#") +
      prop(dig_eval == "-") +
      prop(dig_eval == "blocked #") +
      prop(dig_eval == "other") +
      prop(dig_eval == "blocked +") +
      prop(dig_eval == "+") +
      prop(dig_eval == "#") +
      prop(dig_eval == "blocked -") +
      prop(dig_eval == "blocked =") +
      prop(evaluation_code == "=") +
      prop(evaluation_code == "/" & dig_eval == "none"), # sanity check, should be 1
    count = n()) %>%
  filter(count >= 50) %>%
  arrange(desc(DAV))

