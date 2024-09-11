# Determined Attack Value Against Expected (DAVAx)

# Libraries
library(tidyverse)
library(mosaic) # prop() function
library(ggplot2) # plotting
library(ggimage) # plot images
library(scales) # plot labels in percent format
library(datavolley) # volleyball court plotting

# recreate our 'pre_attack' data with all the information we need
# this assumes you have the data called 'attack' created in the DAV.R file
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
         evaluation_code, dig_eval, opp_team, 
         start_coordinate_x, start_coordinate_y, end_coordinate_x, end_coordinate_y, 
         start_zone, end_zone, end_subzone, skill_subtype) %>%
  mutate(team_won = team == point_won_by) %>%
  select(-point_won_by) %>%
  left_join(sDAV, by = c("setup","attack_description","num_players")) %>%
  left_join(oDAV, by = "opp_team") %>%
  mutate(oDAV = ifelse(is.na(oDAV), as.numeric(nateam_DAV), oDAV),
         aoDAV = oDAV - mean(oDAV),
         xDAV = as.numeric(sDAV) + as.numeric(aoDAV)) %>%
  mutate(DAV = case_when(
    evaluation_code == "#" ~ 1,
    dig_eval == "-" ~ 1-xTP_formula[["d-"]],
    dig_eval == "blocked #" ~ DAV_formula[["blocked #"]],
    dig_eval == "other" ~ DAV_formula[["other"]],
    dig_eval == "blocked +" ~ DAV_formula[["blocked +"]],
    dig_eval == "+" ~ 1-xTP_formula[["d+"]],
    dig_eval == "#" ~ 1-xTP_formula[["d#"]],
    dig_eval == "blocked -" ~ DAV_formula[["blocked -"]],
    dig_eval == "blocked =" ~ 0,
    evaluation_code == "=" ~ 0,
    evaluation_code == "/" & dig_eval == "none" ~ 0
  ))

# read in the file with NCAA team logos from my GitHub
ncaaLogos <- read.csv("https://raw.githubusercontent.com/ethandavenport/Texas-Volleyball/main/team_logos.csv")

# create DAV formula list
DAV_formula = list("blocked #"=0.580, "other"=0.538, "blocked +"=0.515, "blocked -"=0.354)

# create xTP (Expected Transition Points - expected value of a dig) formula list
xTP_formula = list("d#"=0.604, "d+"=0.514, "d-"=0.358)

# create xSO (Expected Sideout - expected value of a pass) formula list
xSO_formula = list("r#"=0.646, "r+"=0.624, "r!"=0.587, "r-"=0.510, "r/"=0.305)

# Analyze First Balls

# create grouped data with all the information we need, and only the 25 most common teams
first_balls <- pre_attack %>%
  filter(setup %in% c("R #", "R +", "R !", "R -")) %>%
  group_by(team) %>%
  summarise(
    xSO =
      xSO_formula[["r#"]]*prop(setup == "R #") +
      xSO_formula[["r+"]]*prop(setup == "R +") +
      xSO_formula[["r!"]]*prop(setup == "R !") +
      xSO_formula[["r-"]]*prop(setup == "R -"),
    DAV = mean(DAV, na.rm = TRUE),
    xDAV = mean(xDAV, na.rm = TRUE),
    DAVAx = (DAV - xDAV),
    count = n()) %>%
  arrange(desc(count)) %>%
  head(25)

# merge the team logos onto this data for plotting
first_balls <- merge(first_balls, ncaaLogos, by = "team")

# plot each team's DAV against xDAV on first balls
ggplot(first_balls, aes(x = xDAV, y = DAV)) +
  geom_point(size = 0, alpha = 0) +
  geom_image(aes(image= URL), size = .08) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linetype = "dashed") +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = percent_format()) +
  xlab("xDAV") +
  ylab("DAV") +
  labs(title = "First Ball DAV vs xDAV") +
  theme(plot.title = element_text(hjust = 0.5))

# Analyze Setter Dumps

# create dumps data with all the information we need, and only the 10 most common players
dumps <- pre_attack %>%
  filter(attack_description == "Setter Dump") %>%
  group_by(player_name, team) %>%
  summarise(
    DAV = mean(DAV, na.rm = TRUE),
    xDAV = mean(xDAV, na.rm = TRUE),
    DAVAx = (DAV - xDAV),
    count = n()) %>%
  arrange(desc(count)) %>%
  head(10)

# merge the team logos onto this data for plotting
dumps <- merge(dumps, ncaaLogos, by = "team")

# plot players in order of DAVAx on setter dumps 
ggplot(dumps, aes(x = reorder(player_name, DAVAx), y = DAVAx)) +
  geom_bar(stat = "identity") +
  geom_image(aes(image=URL, y = ifelse(DAVAx > 0, DAVAx + 0.008, DAVAx - 0.008)), size = 0.07) +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  xlab("Player Name") +
  labs(title = "DAVAx on Setter Dumps") +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5))

# Look Deeper at Ella Swindle

# add a column to pre_attacks that encodes the categorical distance from the net of where the attack starts
pre_attack <- pre_attack %>% 
  mutate(offnet = case_when(
    start_coordinate_y >= 3.2 ~ "tight",
    start_coordinate_y >= 3.1 ~ "3-4 feet",
    start_coordinate_y >= 3.0 ~ "4-5 feet",
    start_coordinate_y < 3.0 ~ "off",
    TRUE ~ "NA"
  ))

# summarize Ella Swindle's setter dump results for each group of distance from the net
ella <- pre_attack %>%
  filter(
    player_name == "Ella Swindle", 
    attack_description == "Setter Dump") %>%
  group_by(offnet) %>%
  summarise(
    DAV = mean(DAV, na.rm = TRUE),
    xDAV = mean(xDAV, na.rm = TRUE),
    DAVAx = (DAV - xDAV),
    count = n())

# plot DAVAx for these groups
ggplot(ella, aes(x = factor(offnet, levels = c("off", "4-5 feet", "3-4 feet", "tight")), y = DAVAx)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  xlab("Distance From Net") +
  labs(title = "Ella Swindle DAVAx on Setter Dumps") +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5))

# what's happening on these off-net setter dumps?
pre_attack %>%
  filter(
    player_name == "Ella Swindle",
    offnet == "off",
    attack_description == "Setter Dump"
  ) %>%
  summarize(
    kills = prop(DAV == 1),
    errors = prop(DAV == 0),
    DAV = mean(ifelse(DAV %in% c(0,1), NA, DAV), na.rm = TRUE))

# Analyze Back Row Attacks

# create back row attacks data with all the information we need, and only the 20 most common players
backrow_player <- pre_attack %>%
  filter(attack_description %in% c("Pipe", "Bic", "High A", "High B", "High C", "High D")) %>%
  group_by(player_name, team) %>%
  summarise(
    errors = prop(evaluation_code %in% c("=","/")),
    DAV = mean(DAV, na.rm = TRUE),
    xDAV = mean(xDAV, na.rm = TRUE),
    DAVAx = (DAV - xDAV),
    count = n()) %>%
  arrange(desc(count)) %>%
  head(20) %>%
  arrange(errors)

# merge the team logos onto this data for plotting
backrow_player <- merge(backrow_player, ncaaLogos, by = "team")

# plot players in order of DAVAx on back row attacks
ggplot(backrow_player, aes(x = reorder(player_name, DAVAx), y = DAVAx)) +
  geom_bar(stat = "identity") +
  geom_image(aes(image=URL, y = ifelse(DAVAx > 0, DAVAx + 0.008, DAVAx - 0.008)), size = 0.06) +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  xlab("Player Name") +
  labs(title = "DAVAx on Back Row Attacks") +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5))

# Look Deeper at Court Zones

# create back row attacks data grouped by end zone and subzone instead of player
backrow_zone <- pre_attack %>%
  filter(
    attack_description %in% c("Pipe", "Bic", "High A", "High B", "High C", "High D"),
    !evaluation_code %in% c("=","/")) %>%
  group_by(end_zone, end_subzone) %>%
  summarise(
    DAV = mean(DAV, na.rm = TRUE),
    xDAV = mean(xDAV, na.rm = TRUE),
    DAVAx = (DAV - xDAV),
    count = n()) %>%
  filter(count > 1)

# assign x and y coordinates to each zone using DataVolley package for plotting
backrow_zone <- cbind(backrow_zone, 
                      dv_xy(backrow_zone$end_zone, subzones = backrow_zone$end_subzone, end = "upper"))

# plot average DAVAx in each subzone
ggplot(backrow_zone, aes(x, y, fill = DAVAx)) + 
  geom_tile() + 
  ggcourt(labels = c("Attacking Team", "Defending Team")) +
  scale_fill_gradient2(low = "red", mid = "yellow", high = muted("green"), 
                       midpoint = median(backrow_zone$DAVAx, na.rm = TRUE), name = "Back Row DAVAx")
