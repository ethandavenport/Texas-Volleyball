# Out Of System Final

# Libraries
library(datavolley)
library(tidyverse)
library(moderndive)
library(mosaic)
library(dplyr)
library(base)
library(ggplot2)

# THESIS: HOW CAN OUT-OF-SYSTEM BE SUCCESSFUL?
# BREAKDOWN BY PLAYER??


# Read in data0
data0 = read_csv("C:/Users/ethan/OneDrive/Documents/data0.csv")

# How many lines of data represented by each team?
data0 %>% count(team) %>% arrange(desc(n))

################################################################################

# Kill % by team on tips
data0 %>%
  filter(skill_subtype == "Tip", attack_description != "Setter Dump") %>%
  group_by(team) %>%
  summarise(kill_pct = prop(winning_attack),
            num_tips = count(team)) %>%
  filter(num_tips >= 100) %>%
  arrange(desc(kill_pct))

# compared to soft spike/topspin attacks
data0 %>%
  filter(skill_subtype == "Soft spike/topspin", attack_description != "Setter Dump") %>%
  group_by(team) %>%
  summarise(kill_pct = prop(winning_attack),
            num_attacks = count(team)) %>%
  filter(num_attacks >= 60) %>%
  arrange(desc(kill_pct))

  # Texas was about equally as effective in each of these categories relative to other teams
  # However, tips generally create much higher kill percentages
  # For Texas, 28.9% of tips were kills (2nd), and 23.9% of soft spikes/topspin were kills (2nd)

# Kill % by player on Texas
data0 %>%
  filter(skill_subtype == "Tip", attack_description != "Setter Dump", team_id == 126) %>%
  group_by(player_name) %>%
  summarise(kill_pct = prop(winning_attack),
            num_tips = count(team)) %>%
  #filter(num_tips >= 10) %>%
  arrange(desc(kill_pct), desc(num_tips))

data0 %>%
  filter(skill_subtype == "Soft spike/topspin", attack_description != "Setter Dump", team_id == 126) %>%
  group_by(player_name) %>%
  summarise(kill_pct = prop(winning_attack),
            num_tips = count(team)) %>%
  #filter(num_tips >= 10) %>%
  arrange(desc(kill_pct), desc(num_tips))

################################################################################

# Tip Evaluation

# create evaluation as categorical variable
data0 = mutate(data0, eval = factor(evaluation))

# eval table
data0 %>%
  filter(skill_subtype == "Tip", attack_description != "Setter Dump") %>%
  group_by(team) %>%
  summarise(Kill = round(prop(eval == "Winning attack"),3),
            Positive = round(prop(eval == "Positive, good attack"),3),
            Poor = round(prop(eval == "Poor, easily dug"),3),
            #BFR = prop(eval == "Blocked for reattack"),
            Blocked = round(prop(eval == "Blocked"),3),
            Error = round(prop(eval == "Error"),3),
            num_tips = count(team)) %>%
  mutate(total = Kill + Positive + Poor + Blocked + Error,
         tip_score = round(3*Kill + 2*Positive + Poor,3),
         hitting_pct = round(Kill - Error - Blocked,3)) %>%
  filter(num_tips >= 101) %>%
  arrange(desc(tip_score))

# compared to non-tip attacks
data0 %>%
  filter(skill_subtype == "Soft spike/topspin", attack_description != "Setter Dump") %>%
  group_by(team) %>%
  summarise(Kill = round(prop(eval == "Winning attack"),3),
            Positive = round(prop(eval == "Positive, good attack"),3),
            Poor = round(prop(eval == "Poor, easily dug"),3),
            #BFR = prop(eval == "Blocked for reattack"),
            Blocked = round(prop(eval == "Blocked"),3),
            Error = round(prop(eval == "Error"),3),
            num_attacks = count(team)) %>%
  mutate(total = Kill + Positive + Poor + Blocked + Error,
         nontip_score = round(3*Kill + 2*Positive + Poor,3),
         hitting_pct = round(Kill - Error - Blocked,3)) %>%
  filter(num_attacks >= 60) %>%
  arrange(desc(nontip_score))

  # Again, tipping outperforms soft spikes/topspin
  # This time, Texas is better tipping both relatively and absolutely
  # Hitting percentage is .192 (3rd) on tips vs .141 (6th) on non-tips
  # Attack scores are 1.64 (2nd) on tips vs 1.38 (11th) on non-tips

# Kill % by player on Texas
data0 %>%
  filter(skill_subtype == "Tip", attack_description != "Setter Dump", team_id == 126) %>%
  group_by(player_name) %>%
  summarise(Kill = round(prop(eval == "Winning attack"),3),
            Positive = round(prop(eval == "Positive, good attack"),3),
            Poor = round(prop(eval == "Poor, easily dug"),3),
            #BFR = prop(eval == "Blocked for reattack"),
            Blocked = round(prop(eval == "Blocked"),3),
            Error = round(prop(eval == "Error"),3),
            num_tips = count(team)) %>%
  mutate(tip_score = round(3*Kill + 2*Positive + Poor,3),
         hitting_pct = round(Kill - Error - Blocked,3)) %>%
  #filter(num_tips >= 10) %>%
  arrange(desc(tip_score), desc(num_tips))

data0 %>%
  filter(skill_subtype == "Soft spike/topspin", attack_description != "Setter Dump", team_id == 126) %>%
  group_by(player_name) %>%
  summarise(Kill = round(prop(eval == "Winning attack"),3),
            Positive = round(prop(eval == "Positive, good attack"),3),
            Poor = round(prop(eval == "Poor, easily dug"),3),
            #BFR = prop(eval == "Blocked for reattack"),
            Blocked = round(prop(eval == "Blocked"),3),
            Error = round(prop(eval == "Error"),3),
            num_attacks = count(team)) %>%
  mutate(nontip_score = round(3*Kill + 2*Positive + Poor,3),
         hitting_pct = round(Kill - Error - Blocked,3)) %>%
  #filter(num_attacks >= 10) %>%
  arrange(desc(nontip_score), desc(num_attacks))

################################################################################

# How well did the opponent dig the ball?

lag1 = data0 %>% filter(point_id == lag(point_id,1),
                        skill == "Dig",
                        opp_team != "University of Texas at Austin",
                        lag(team_id,1) != 126,
                        lag(skill_subtype,1) == "Tip")

lag2 = data0 %>% filter(point_id == lag(point_id,2),
                        skill == "Dig",
                        opp_team != "University of Texas at Austin",
                        lag(team_id,2) != 126,
                        lag(skill_subtype,2) == "Tip")

relevant_digs = rbind(lag1, lag2) # 267 observations

relevant_digs %>%
  summarise(Perfect = prop(evaluation_code == "#"),
            Good = prop(evaluation_code == "+"),
            Poor = prop(evaluation_code == "-"),
            Error = prop(evaluation_code == "=")) %>%
  mutate(dig_score = 3*Perfect + 2*Good + Poor)



lag1 = data0 %>% filter(point_id == lag(point_id,1),
                        skill == "Dig",
                        opp_team != "University of Texas at Austin",
                        lag(team_id,1) != 126,
                        lag(skill_subtype,1) == "Soft spike/topspin")

lag2 = data0 %>% filter(point_id == lag(point_id,2),
                        skill == "Dig",
                        opp_team != "University of Texas at Austin",
                        lag(team_id,2) != 126,
                        lag(skill_subtype,2) == "Soft spike/topspin")

relevant_digs = rbind(lag1, lag2) # 72 observations

relevant_digs %>%
  summarise(Perfect = prop(evaluation_code == "#"),
            Good = prop(evaluation_code == "+"),
            Poor = prop(evaluation_code == "-"),
            Error = prop(evaluation_code == "=")) %>%
  mutate(dig_score = 3*Perfect + 2*Good + Poor)

  # This information reveals the most stark contrast between tipping and soft spikes/topspin
  # Opponents had a MUCH easier time digging soft spikes/topspin (2.28) than tips (1.78)
  # In fact, opponents were able to get a perfect pass on soft spikes/topspin at a 51.4% rate
  # This compares to only 27.7% of the time among tip attacks

# CREATE A STACKED BAR CHART OUT OF THIS



data0 %>%
  filter(skill == "Dig",
         opp_team == "University of Texas at Austin") %>%
  group_by(evaluation_code) %>%
  summarize(count = n())

data0 %>%
  filter(skill == "Dig",
         evaluation_code == "=") %>%
  select(evaluation)


################################################################################

# PCT Points Won
# BREAK THIS DOWN FOR WITHIN A CERTAIN NUMBER OF TOUCHES
# The goal is to consider only points where that specific attack played a role in the end of the point
# Within 6 lines: block touch, dig, set, attack, dig, point called

# create point-winning team as categorical variable
data0 = mutate(data0, team_won_point = factor(point_won_by))

# pct points won table
lead1 = data0 %>% filter(skill_subtype == "Tip",
                         attack_description != "Setter Dump",
                         lead(point,1) == TRUE,
                         lead(point_id,1) == point_id)

lead2 = data0 %>% filter(skill_subtype == "Tip",
                         attack_description != "Setter Dump",
                         lead(point,2) == TRUE,
                         lead(point_id,2) == point_id)

lead3 = data0 %>% filter(skill_subtype == "Tip",
                         attack_description != "Setter Dump",
                         lead(point,3) == TRUE,
                         lead(point_id,3) == point_id)

lead4 = data0 %>% filter(skill_subtype == "Tip",
                         attack_description != "Setter Dump",
                         lead(point,4) == TRUE,
                         lead(point_id,4) == point_id)

lead5 = data0 %>% filter(skill_subtype == "Tip",
                         attack_description != "Setter Dump",
                         lead(point,5) == TRUE,
                         lead(point_id,5) == point_id)

lead6 = data0 %>% filter(skill_subtype == "Tip",
                         attack_description != "Setter Dump",
                         lead(point,6) == TRUE,
                         lead(point_id,6) == point_id)

relevant_tips = rbind(lead1, lead2, lead3, lead4, lead5, lead6)

relevant_tips %>%
  group_by(team) %>%
  summarise(pct_points_won = prop(team_won_point == team),
            num_points = count(team)) %>%
  filter(num_points >= 80) %>%
  arrange(desc(pct_points_won))

# compared to non-tip attacks
lead1 = data0 %>% filter(skill_subtype == "Soft spike/topspin",
                         attack_description != "Setter Dump",
                         lead(point,1) == TRUE,
                         lead(point_id,1) == point_id)

lead2 = data0 %>% filter(skill_subtype == "Soft spike/topspin",
                         attack_description != "Setter Dump",
                         lead(point,2) == TRUE,
                         lead(point_id,2) == point_id)

lead3 = data0 %>% filter(skill_subtype == "Soft spike/topspin",
                         attack_description != "Setter Dump",
                         lead(point,3) == TRUE,
                         lead(point_id,3) == point_id)

lead4 = data0 %>% filter(skill_subtype == "Soft spike/topspin",
                         attack_description != "Setter Dump",
                         lead(point,4) == TRUE,
                         lead(point_id,4) == point_id)

lead5 = data0 %>% filter(skill_subtype == "Soft spike/topspin",
                         attack_description != "Setter Dump",
                         lead(point,5) == TRUE,
                         lead(point_id,5) == point_id)

lead6 = data0 %>% filter(skill_subtype == "Soft spike/topspin",
                         attack_description != "Setter Dump",
                         lead(point,6) == TRUE,
                         lead(point_id,6) == point_id)

relevant_nontips = rbind(lead1, lead2, lead3, lead4, lead5, lead6)

relevant_nontips %>%
  group_by(team) %>%
  summarise(pct_points_won = prop(team_won_point == team),
            num_points = count(team)) %>%
  filter(num_points >= 40) %>%
  arrange(desc(pct_points_won))

  # Tipping gets a slight edge in this category as well
  # Texas wins 61.7% of points that end promptly following a Texas tip attack (4th)
  # Texas wins 60.7% of points that end promptly following a Texas soft spike/topspin attack (2nd)

################################################################################


# CREATE A HEAT MAP OF WHERE TIPS ARE KILLS/GOOD


  # Heat map
options(repos = c(openvolley = "https://openvolley.r-universe.dev",
                  CRAN = "https://cloud.r-project.org"))
install.packages("ovlytics")
library(ovlytics)
library(datavolley)

hx <- ov_heatmap_kde(data0 %>% dplyr::filter(skill_subtype == "Soft spike/topspin", team_id != 126) %>%
                       dplyr::select(end_coordinate_x, end_coordinate_y),
                     resolution = "coordinates", court = "upper")
ggplot(hx, aes(x, y, fill = density)) +
  scale_fill_distiller(palette = "Spectral", guide = "none") +
  geom_raster() +
  ggcourt(labels = NULL, court = "upper")




