# Our Following Attack

# Libraries
library(datavolley)
library(tidyverse)
library(moderndive)
library(mosaic)
library(dplyr)
library(base)
library(ggplot2)

  # selecting data for Texas' attack following their own tip
  # low end is lag two touches: Texas tip -> opponent dig (overpass) -> Texas attack
  # high end is lag eight touches:
  # Tip -> block touch -> dig -> set -> attack -> block touch -> dig -> set -> attack
  # same point, same match, attack, by Texas, after a Texas attack, which was a tip

lag2_attack = data0 %>% filter(point_id == lag(point_id,2),
                               match_id == lag(match_id,2),
                               skill == "Attack",
                               team == "University of Texas at Austin",
                               lag(team,2) == "University of Texas at Austin",
                               lag(skill_subtype,2) == "Tip")

lag3_attack = data0 %>% filter(point_id == lag(point_id,3),
                               match_id == lag(match_id,3),
                               skill == "Attack",
                               team == "University of Texas at Austin",
                               lag(team,3) == "University of Texas at Austin",
                               lag(skill_subtype,3) == "Tip")

lag4_attack = data0 %>% filter(point_id == lag(point_id,4),
                               match_id == lag(match_id,4),
                               skill == "Attack",
                               team == "University of Texas at Austin",
                               lag(team,4) == "University of Texas at Austin",
                               lag(skill_subtype,4) == "Tip")

lag5_attack = data0 %>% filter(point_id == lag(point_id,5),
                               match_id == lag(match_id,5),
                               skill == "Attack",
                               team == "University of Texas at Austin",
                               lag(team,5) == "University of Texas at Austin",
                               lag(skill_subtype,5) == "Tip")

lag6_attack = data0 %>% filter(point_id == lag(point_id,6),
                               match_id == lag(match_id,6),
                               skill == "Attack",
                               team == "University of Texas at Austin",
                               lag(team,6) == "University of Texas at Austin",
                               lag(skill_subtype,6) == "Tip")

lag7_attack = data0 %>% filter(point_id == lag(point_id,7),
                               match_id == lag(match_id,7),
                               skill == "Attack",
                               team == "University of Texas at Austin",
                               lag(team,7) == "University of Texas at Austin",
                               lag(skill_subtype,7) == "Tip")

lag8_attack = data0 %>% filter(point_id == lag(point_id,8),
                               match_id == lag(match_id,8),
                               skill == "Attack",
                               team == "University of Texas at Austin",
                               lag(team,8) == "University of Texas at Austin",
                               lag(skill_subtype,8) == "Tip")

next_attack_texas_data = rbind(lag2_attack, lag3_attack, lag4_attack, lag5_attack,
                               lag6_attack, lag7_attack, lag8_attack)

# What kind of attack were we able to get?
next_attack_texas_data %>%
  summarise(Hard_spike = prop(skill_subtype == "Hard spike"),
            Soft_spike = prop(skill_subtype == "Soft spike/topspin"),
            Tip = prop(skill_subtype == "Tip"))

# compared to subtype of our transition attack usually
data0 %>%
  filter(team == "University of Texas at Austin",
         skill == "Attack",
         phase == "Transition") %>%
  summarise(Hard_spike = prop(skill_subtype == "Hard spike"),
            Soft_spike = prop(skill_subtype == "Soft spike/topspin"),
            Tip = prop(skill_subtype == "Tip"))
  # We were able to get a hard spike slightly more often the attack after a tip
  # 74.1% vs 72.4% on all transition attacks
prop.test(x=0.7236534*162, n=162, conf.level=.95, correct=FALSE)
pbinom(0.7405063*193, size = 193, prob = 0.7236534)
  # This difference is not statistically significant. If the true transition hard spike
  # percentage was equal, there would be a 32.8% chance of a transition hard spike
  # percentage 74.1% or higher
  # Additionally, there is minimal practice significance here.

# How effective was that attack?
next_attack_texas_data %>%
  summarise(Kill = prop(eval == "Winning attack"),
            Positive = prop(eval == "Positive, good attack"),
            Poor = prop(eval == "Poor, easily dug"),
            BFR = prop(eval == "Blocked for reattack"),
            Blocked = prop(eval == "Blocked"),
            Error = prop(eval == "Error")) %>%
  mutate(attack_score = 3*Kill + 2*Positive + Poor,
         hitting_pct = Kill - Error - Blocked)

# compared to all of our transition attacks
data0 %>%
  filter(team == "University of Texas at Austin",
         skill == "Attack",
         phase == "Transition") %>%
  summarise(Kill = prop(eval == "Winning attack"),
            Positive = prop(eval == "Positive, good attack"),
            Poor = prop(eval == "Poor, easily dug"),
            BFR = prop(eval == "Blocked for reattack"),
            Blocked = prop(eval == "Blocked"),
            Error = prop(eval == "Error")) %>%
  mutate(attack_score = 3*Kill + 2*Positive + Poor,
         hitting_pct = Kill - Error - Blocked)
  # Again, there is not much of a difference here
  # Kill %: .426 vs .420 normally
  # Attack score: 1.802 vs 1.813 normally
  # Hitting %: .302 vs .284 normally
  # Are these results statistically significant?
prop.test(x=0.4204113*162, n=162, conf.level=.95, correct=FALSE)
pbinom(0.4259259*162, size = 162, prob = 0.4204113)
  # Kill %: No. CI: 0.347 to 0.497
prop.test(x=0.2840823*162, n=162, conf.level=.95, correct=FALSE)
pbinom(0.3024691*162, size = 162, prob = 0.2840823)
  # Hitting %: No. CI: 0.220 to 0.358

  # Dead ball number on digs: did I give away possession? /=
  # Separate info for tips and hard attacks/soft attacks
  # What is the practical significance of these values?
  # free balls (DONE)
  # our digs

data0 %>%
  filter(eval == "Poor, easily dug",
         match_id == 28)

data0 %>%
  filter(match_id == 28,
         point_id == 10) %>%
  tail(11)
data0 %>%
  filter(match_id == 28,
         point_id == 29) %>%
  tail(11)

# blocked for reattack
