# Dashboard Data Setup

library(shiny)
library(bslib)
library(dplyr)
library(mosaic)
library(ggplot2)
library(ggExtra)
library(ggimage)
library(tidyr)

# Load in logo URLs
ncaaLogos <- read.csv("C:/Users/ethan/OneDrive/Texas Volleyball/Shiny Dashboard/team_logos.csv")

# Team-level Data
service <- data0 %>%
  filter(skill == "Serve") %>%
  group_by(team) %>%
  summarise("Ace Rate" = prop(evaluation_code == "#")*100,
            "Service Error Rate" = prop(evaluation_code == "=")*100,
            "Opponent SO%" = prop(point_won_by == opp_team)*100,
            serve_count = n())

nonerrors <- data0 %>%
  mutate(lead_code = lead(evaluation_code,1)) %>%
  filter(skill == "Serve", evaluation_code != "=") %>%
  group_by(team) %>%
  summarise("Opp Reception Good Pass Rate" = prop(lead_code %in% c("#","+"))*100,
            "Opp Reception Error Rate" = prop(lead_code == "=")*100,
            "Opp Reception Score" = 5*prop(lead_code == "#") +
              4*prop(lead_code == "+") +
              3*prop(lead_code == "!") +
              2*prop(lead_code == "-") +
              prop(lead_code == "/"))

attacking <- data0 %>%
  filter(skill == "Attack") %>%
  group_by(team) %>%
  summarise("Kill %" = prop(evaluation_code == "#"),
            "Hitting Percentage" = prop(evaluation_code == "#") -
              prop(evaluation_code == "=") -
              prop(evaluation_code == "/"),
            "Attack Score" = 3*prop(evaluation_code == "#") +
              2*prop(evaluation_code == "+") +
              prop(evaluation_code == "-"),
            attack_count = n())

lag1 = data0 %>% filter(skill == "Attack", lag(team,1) == team, lag(skill,1) == "Freeball")
lag2 = data0 %>% filter(skill == "Attack", lag(team,2) == team, lag(skill,2) == "Freeball")
free_attacks_data = rbind(lag1,lag2)

fbconversion <- free_attacks_data %>%
  group_by(team) %>%
  summarise("Free Ball Conversion" = prop(evaluation_code == "#"),
            fb_count = n())

defense <- data0 %>%
  filter(skill == "Dig") %>%
  group_by(team) %>%
  summarise("Dig Good Pass Rate" = prop(evaluation_code %in% c("#","+"))*100,
            "Dig Error Rate" = prop(evaluation_code == "=")*100,
            "Dig Score" = 3*prop(evaluation_code == "#") +
              2*prop(evaluation_code == "+") +
              prop(evaluation_code == "-"),
            defense_count = n())

receive <- data0 %>%
  filter(skill == "Reception") %>%
  group_by(team) %>%
  summarise("Reception Good Pass Rate" = prop(evaluation_code %in% c("#","+"))*100,
            "Reception Error Rate" = prop(evaluation_code == "=")*100,
            "Reception Score" = 5*prop(evaluation_code == "#") +
              4*prop(evaluation_code == "+") +
              3*prop(evaluation_code == "!") +
              2*prop(evaluation_code == "-") +
              prop(evaluation_code == "/"),
            receive_count = n())

team <- left_join(data0 %>%
                    group_by(match_id, point_id, team) %>%
                    summarize(length = n()) %>%
                    select(-length), 
                  data0 %>%
                    filter(!is.na(skill)) %>%
                    group_by(match_id, point_id) %>%
                    summarize(length = n()),
                  by = c("match_id", "point_id")) %>%
  group_by(team) %>%
  summarize("Avg Point Duration" = mean(length),
            point_count = n())

teamso <- data0 %>%
  filter(skill == "Serve") %>%
  group_by(opp_team) %>%
  summarise("SO%" = prop(point_won_by == opp_team)*100,
            opp_serve_count = n())


service <- full_join(service, nonerrors, by = "team")
offense <- full_join(attacking, fbconversion, by = "team")
defense <- full_join(defense, receive, by = "team")
team <- full_join(team, teamso, by = c("team" = "opp_team"))

team_level <- full_join(service, offense, by = "team")
team_level <- full_join(team_level, defense, by = "team")
team_level <- full_join(team_level, team, by = "team")
team_level <- full_join(team_level, ncaaLogos, by = "team")


# Player-level Data
service <- data0 %>%
  filter(skill == "Serve") %>%
  group_by(team, player_name) %>%
  summarise("Ace Rate" = prop(evaluation_code == "#")*100,
            "Service Error Rate" = prop(evaluation_code == "=")*100,
            "Opponent SO%" = prop(point_won_by == opp_team)*100,
            serve_count = n())

nonerrors <- data0 %>%
  mutate(lead_code = lead(evaluation_code,1)) %>%
  filter(skill == "Serve", evaluation_code != "=") %>%
  group_by(team, player_name) %>%
  summarise("Opp Reception Good Pass Rate" = prop(lead_code %in% c("#","+"))*100,
            "Opp Reception Error Rate" = prop(lead_code == "=")*100,
            "Opp Reception Score" = 5*prop(lead_code == "#") +
              4*prop(lead_code == "+") +
              3*prop(lead_code == "!") +
              2*prop(lead_code == "-") +
              prop(lead_code == "/"))

attacking <- data0 %>%
  filter(skill == "Attack") %>%
  group_by(team, player_name) %>%
  summarise("Kill %" = prop(evaluation_code == "#"),
            "Hitting Percentage" = prop(evaluation_code == "#") -
              prop(evaluation_code == "=") -
              prop(evaluation_code == "/"),
            "Attack Score" = 3*prop(evaluation_code == "#") +
              2*prop(evaluation_code == "+") +
              prop(evaluation_code == "-"),
            attack_count = n())

fbconversion <- free_attacks_data %>%
  group_by(team, player_name) %>%
  summarise("Free Ball Conversion" = prop(evaluation_code == "#"),
            fb_count = n())

defense <- data0 %>%
  filter(skill == "Dig") %>%
  group_by(team, player_name) %>%
  summarise("Dig Good Pass Rate" = prop(evaluation_code %in% c("#","+"))*100,
            "Dig Error Rate" = prop(evaluation_code == "=")*100,
            "Dig Score" = 3*prop(evaluation_code == "#") +
              2*prop(evaluation_code == "+") +
              prop(evaluation_code == "-"),
            defense_count = n())

receive <- data0 %>%
  filter(skill == "Reception") %>%
  group_by(team, player_name) %>%
  summarise("Reception Good Pass Rate" = prop(evaluation_code %in% c("#","+"))*100,
            "Reception Error Rate" = prop(evaluation_code == "=")*100,
            "Reception Score" = 5*prop(evaluation_code == "#") +
              4*prop(evaluation_code == "+") +
              3*prop(evaluation_code == "!") +
              2*prop(evaluation_code == "-") +
              prop(evaluation_code == "/"),
            receive_count = n())

service <- full_join(service, nonerrors, by = c("team","player_name"))
offense <- full_join(attacking, fbconversion, by = c("team","player_name"))
defense <- full_join(defense, receive, by = c("team","player_name"))

player_level <- full_join(service, offense, by = c("team","player_name"))
player_level <- full_join(player_level, defense, by = c("team","player_name"))
player_level <- full_join(player_level, ncaaLogos, by = "team")

cserves <- c("Ace Rate","Service Error Rate","Opponent SO%",
             "Opp Reception Good Pass Rate","Opp Reception Error Rate","Opp Reception Score")
cattacks <- c("Kill %","Hitting Percentage","Attack Score")
cfreeball <- c("Free Ball Conversion")
cdefense <- c("Dig Good Pass Rate","Dig Error Rate","Dig Score")
creceive <- c("Reception Good Pass Rate","Reception Error Rate","Reception Score")
cteam <- c("Avg Point Duration")
cteamso <- c("SO%")

