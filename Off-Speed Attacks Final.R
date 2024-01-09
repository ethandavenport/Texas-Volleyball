# Off-Speed Attacks Final

# Libraries
library(datavolley)
library(tidyverse)
library(moderndive)
library(mosaic)
library(dplyr)
library(base)
library(ggplot2)


# read in data0 file
data0 = read_csv("C:/Users/ethan/OneDrive/Documents/data0.csv")

# how many lines of data represented by each team?
data0 %>% count(team) %>% arrange(desc(n))

# matches?
data0 %>%
  filter(!is.na(team)) %>%
  group_by(match_id, team) %>%
  summarize(match_count = n()) %>%
  group_by(team) %>%
  summarize(total_matches = n()) %>%
  arrange(desc(total_matches)) %>%
  head(11)


################################################################################

# Attack Evaluation

# kill % by team on tips
data0 %>%
  filter(skill_subtype == "Tip", attack_description != "Setter Dump") %>%
  group_by(team) %>%
  summarise(kill_pct = prop(winning_attack),
            num_tips = count(team)) %>%
  filter(num_tips >= 100) %>%
  arrange(desc(kill_pct))

# compared to soft spike attacks
data0 %>%
  filter(skill_subtype == "Soft spike/topspin", attack_description != "Setter Dump") %>%
  group_by(team) %>%
  summarise(kill_pct = prop(winning_attack),
            num_attacks = count(team)) %>%
  filter(num_attacks >= 60) %>%
  arrange(desc(kill_pct))

  # Texas was about equally as effective in each of these categories relative to other teams
  # However, tips generally create much higher kill percentages
  # For Texas, 28.9% of tips were kills (2nd), and 23.9% of soft spikes were kills (2nd)

# kill % by player on Texas on tip
data0 %>%
  filter(skill_subtype == "Tip", attack_description != "Setter Dump", team_id == 126) %>%
  group_by(player_name) %>%
  summarise(kill_pct = prop(winning_attack),
            num_tips = count(team)) %>%
  #filter(num_tips >= 10) %>%
  arrange(desc(kill_pct), desc(num_tips))

# compared to soft spike attacks
data0 %>%
  filter(skill_subtype == "Soft spike/topspin", attack_description != "Setter Dump", team_id == 126) %>%
  group_by(player_name) %>%
  summarise(kill_pct = prop(winning_attack),
            num_tips = count(team)) %>%
  #filter(num_tips >= 10) %>%
  arrange(desc(kill_pct), desc(num_tips))

# change evaluation to a categorical variable
data0 = mutate(data0, eval = factor(evaluation))

# eval table for tips
tipevals = data0 %>%
  filter(skill_subtype == "Tip", attack_description != "Setter Dump") %>%
  group_by(team) %>%
  summarise(Kill = round(prop(eval == "Winning attack"),3),
            Positive = round(prop(eval == "Positive, good attack"),3),
            Poor = round(prop(eval == "Poor, easily dug"),3),
            #BFR = prop(eval == "Blocked for reattack"),
            Blocked = round(prop(eval == "Blocked"),3),
            Error = round(prop(eval == "Error"),3),
            num_tips = count(team)) %>%
  mutate(tip_score = round(3*Kill + 2*Positive + Poor,3),
         hitting_pct = round(Kill - Error - Blocked,3)) %>%
  filter(num_tips >= 101) %>%
  arrange(desc(tip_score))

# write csv for easier data visualization in excel
write_csv(tipevals, "C:/Users/ethan/OneDrive/Texas Volleyball Internship/Tipping Success/tipevals.csv")

# compared to soft spike attacks
nontipevals = data0 %>%
  filter(skill_subtype == "Soft spike/topspin", attack_description != "Setter Dump") %>%
  group_by(team) %>%
  summarise(Kill = round(prop(eval == "Winning attack"),3),
            Positive = round(prop(eval == "Positive, good attack"),3),
            Poor = round(prop(eval == "Poor, easily dug"),3),
            #BFR = prop(eval == "Blocked for reattack"),
            Blocked = round(prop(eval == "Blocked"),3),
            Error = round(prop(eval == "Error"),3),
            num_attacks = count(team)) %>%
  mutate(nontip_score = round(3*Kill + 2*Positive + Poor,3),
         hitting_pct = round(Kill - Error - Blocked,3)) %>%
  filter(num_attacks >= 60) %>%
  arrange(desc(nontip_score))

# write csv for easier data visualization in excel
write_csv(nontipevals, "C:/Users/ethan/OneDrive/Texas Volleyball Internship/Tipping Success/nontipevals.csv")

  # Again, tipping outperforms soft spikes
  # This time, Texas is better tipping both relatively and absolutely
  # Hitting percentage is .192 (3rd) on tips vs .141 (6th) on soft spikes
  # Attack scores are 1.64 (2nd) on tips vs 1.38 (11th) on soft spikes

# tip evaluation by player on Texas
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

# soft spike evaluation by player on Texas
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

# Opponent's Dig

# mutate variable for opposing team
data0$opp_team = ifelse(data0$team == data0$visiting_team, data0$home_team,
                        ifelse(data0$team == data0$home_team, data0$visiting_team,""))

# create data set for digs off of tips
# potential for up to two lags: attack -> block touch -> dig
lag1 = data0 %>% filter(point_id == lag(point_id,1), #same point
                        skill == "Dig", #dig
                        opp_team == "University of Texas at Austin", #against Texas
                        lag(team_id,1) == 126, #previously a Texas touch
                        lag(skill_subtype,1) == "Tip", #previously a tip
                        lag(attack_description,1) != "Setter Dump") #previously not a setter dump

lag2 = data0 %>% filter(point_id == lag(point_id,2),
                        skill == "Dig",
                        opp_team == "University of Texas at Austin",
                        lag(team_id,2) == 126,
                        lag(skill_subtype,2) == "Tip",
                        lag(attack_description,2) != "Setter Dump")

relevant_digs = rbind(lag1, lag2) # 227 observations

# summarize this data with dig score and proportions
relevant_digs %>%
  summarise(Perfect = prop(evaluation_code == "#"),
            Good = prop(evaluation_code == "+"),
            Poor = prop(evaluation_code == "-"),
            Error = prop(evaluation_code == "=")) %>%
  mutate(dig_score = 3*Perfect + 2*Good + Poor)

# repeat this process for soft spikes:
# create data set for digs off of soft spikes
# potential for up to two lags: attack -> block touch -> dig
lag1 = data0 %>% filter(point_id == lag(point_id,1), #same point
                        skill == "Dig", #dig
                        opp_team == "University of Texas at Austin", #against Texas
                        lag(team_id,1) == 126, #previously a Texas touch
                        lag(skill_subtype,1) == "Soft spike/topspin", #previously a soft spike
                        lag(attack_description,1) != "Setter Dump") #previously not a setter dump

lag2 = data0 %>% filter(point_id == lag(point_id,2),
                        skill == "Dig",
                        opp_team == "University of Texas at Austin",
                        lag(team_id,2) == 126,
                        lag(skill_subtype,2) == "Soft spike/topspin",
                        lag(attack_description,2) != "Setter Dump")

relevant_digs = rbind(lag1, lag2) # 66 observations

# summarize this data with dig score and proportions
relevant_digs %>%
  summarise(Perfect = prop(evaluation_code == "#"),
            Good = prop(evaluation_code == "+"),
            Poor = prop(evaluation_code == "-"),
            Error = prop(evaluation_code == "=")) %>%
  mutate(dig_score = 3*Perfect + 2*Good + Poor)

  # This information reveals the most stark contrast between tipping and soft spikes
  # Opponents had a MUCH easier time digging soft spikes (2.33) than tips (1.83)
  # In fact, opponents were able to get a perfect pass on soft spikes at a 53.0% rate
  # This compares to only 27.3% of the time among tip attacks

################################################################################

# Opponent's Return Ball

# create data set for return balls off of tips
# potential for up to four lags: attack -> block touch -> dig -> set -> free ball or attack
lag1 = data0 %>% filter(skill == "Freeball" | skill == "Attack", #free ball or attack
                        opp_team == "University of Texas at Austin", #against Texas
                        lag(skill_subtype,1) == "Tip", #previously a tip
                        lag(attack_description,1) != "Setter Dump", #previously not a setter dump
                        lag(point_id,1) == point_id) #same point

lag2 = data0 %>% filter(skill == "Freeball" | skill == "Attack",
                        opp_team == "University of Texas at Austin",
                        lag(skill_subtype,2) == "Tip",
                        lag(attack_description,2) != "Setter Dump",
                        lag(point_id,2) == point_id)

lag3 = data0 %>% filter(skill == "Freeball" | skill == "Attack",
                        opp_team == "University of Texas at Austin",
                        lag(skill_subtype,3) == "Tip",
                        lag(attack_description,3) != "Setter Dump",
                        lag(point_id,3) == point_id)

lag4 = data0 %>% filter(skill == "Freeball" | skill == "Attack",
                        opp_team == "University of Texas at Austin",
                        lag(skill_subtype,4) == "Tip",
                        lag(attack_description,4) != "Setter Dump",
                        lag(point_id,4) == point_id)

tipreturns = rbind(lag1, lag2, lag3, lag4)

# for context: proportion of our tips that receive any type of return ball
nrow(tipreturns)/nrow(data0 %>% filter(team_id == 126, skill_subtype == "Tip"))

# remove attacks with 'NA' subtype label
tipreturns = tipreturns %>% filter(skill != "Attack" | !is.na(skill_subtype))

# summarize this data with proportions of subtype or free ball
tipreturns %>%
  group_by(skill, skill_subtype) %>%
  summarise(count = n()/nrow(tipreturns))

# repeat this process for soft spikes:
# create data set for return balls off of soft spike
# potential for up to four lags: attack -> block touch -> dig -> set -> free ball or attack
lag1 = data0 %>% filter(skill == "Freeball" | skill == "Attack", #free ball or attack
                        opp_team == "University of Texas at Austin", #against Texas
                        lag(skill_subtype,1) == "Soft spike/topspin", #previously a soft spike
                        lag(attack_description,1) != "Setter Dump", #previously not a setter dump
                        lag(point_id,1) == point_id) #same point

lag2 = data0 %>% filter(skill == "Freeball" | skill == "Attack",
                        opp_team == "University of Texas at Austin",
                        lag(skill_subtype,2) == "Soft spike/topspin",
                        lag(attack_description,2) != "Setter Dump",
                        lag(point_id,2) == point_id)

lag3 = data0 %>% filter(skill == "Freeball" | skill == "Attack",
                        opp_team == "University of Texas at Austin",
                        lag(skill_subtype,3) == "Soft spike/topspin",
                        lag(attack_description,3) != "Setter Dump",
                        lag(point_id,3) == point_id)

lag4 = data0 %>% filter(skill == "Freeball" | skill == "Attack",
                        opp_team == "University of Texas at Austin",
                        lag(skill_subtype,4) == "Soft spike/topspin",
                        lag(attack_description,4) != "Setter Dump",
                        lag(point_id,4) == point_id)

ssreturns = rbind(lag1, lag2, lag3, lag4)

# for context: proportion of our soft spikes that receive any type of return ball
nrow(ssreturns)/nrow(data0 %>% filter(team_id == 126, skill_subtype == "Soft spike/topspin"))

# remove attacks with 'NA' subtype label
ssreturns = ssreturns %>% filter(skill != "Attack" | !is.na(skill_subtype))

# summarize this data with proportions of subtype or free ball
ssreturns %>%
  group_by(skill, skill_subtype) %>%
  summarise(count = n()/nrow(ssreturns))

################################################################################

# Percent of Points Won

# the goal is to consider only points where that specific attack played a role in the end of the point
# my cutoff: within 5 touches aka 6 lines of code:
# attack -> block touch -> dig -> set-> attack -> dig-> point called

# create point-winning team as categorical variable
data0 = mutate(data0, team_won_point = factor(point_won_by))

# create data set for tips where the point ended within the relevant aftermath
lead1 = data0 %>% filter(skill_subtype == "Tip", #tip
                         attack_description != "Setter Dump", #not setter dump
                         lead(point,1) == TRUE, #future ended point
                         lead(point_id,1) == point_id) #same point

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

relevant_tips = rbind(lead1, lead2, lead3, lead4, lead5)

# summarize this data with percentage of those points that the attacking team won
tip_points = relevant_tips %>%
  group_by(team) %>%
  summarise(pct_points_won = round(prop(team_won_point == team),3),
            num_points = count(team)) %>%
  filter(num_points >= 80) %>%
  arrange(desc(pct_points_won))

# repeat this process for soft spikes:
# create data set for soft spikes where the point ended within the relevant aftermath
lead1 = data0 %>% filter(skill_subtype == "Soft spike/topspin", #soft spike
                         attack_description != "Setter Dump", #not setter dump
                         lead(point,1) == TRUE, #future ended point
                         lead(point_id,1) == point_id) #same point

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

relevant_nontips = rbind(lead1, lead2, lead3, lead4, lead5)

# summarize this data with percentage of those points that the attacking team won
nontip_points = relevant_nontips %>%
  group_by(team) %>%
  summarise(pct_points_won = round(prop(team_won_point == team),3),
            num_points = count(team)) %>%
  filter(num_points >= 40) %>%
  arrange(desc(pct_points_won))

# Tipping gets a slight edge in this category as well
# Texas wins 61.7% of points that end promptly following a Texas tip attack (4th)
# Texas wins 60.7% of points that end promptly following a Texas soft spike attack (2nd)

################################################################################

# Improving Location with Heat Maps

# where on the court do our attacks come from?
data0 %>%
  filter(skill == "Attack",
         skill_subtype == "Tip",
         team_id == 126) %>%
  group_by(start_zone) %>%
  summarize(count = n())

# load libraries
options(repos = c(openvolley = "https://openvolley.r-universe.dev",
                  CRAN = "https://cloud.r-project.org"))
install.packages("ovlytics")
library(ovlytics)
library(datavolley)

# heat map template: change filter constraints based on desired output
hx <- ov_heatmap_kde(data0 %>% dplyr::filter(skill_subtype == "Tip",
                                             #start_zone == 3,
                                             team_id == 126,
                                             evaluation_code == "#" | evaluation_code == "+") %>%
                       dplyr::select(end_coordinate_x, end_coordinate_y),
                     resolution = "coordinates", court = "upper")
ggplot(hx, aes(x, y, fill = density)) +
  scale_fill_distiller(palette = "Spectral", guide = "none") +
  geom_raster() +
  ggcourt(labels = NULL, court = "upper")




