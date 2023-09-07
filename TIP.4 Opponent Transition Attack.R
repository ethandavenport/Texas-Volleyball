# Opponent Transition Attack

# Libraries
library(datavolley)
library(tidyverse)
library(moderndive)
library(mosaic)
library(dplyr)
library(base)
library(ggplot2)

# create opposing team variable
data0$opp_team = ifelse(data0$team == data0$visiting_team,
                        data0$home_team, ifelse(data0$team == data0$home_team,
                                                data0$visiting_team,""))

  # selecting data for the attack after Texas' tip
  # low end is lag two touches: Texas attack -> opponent dig -> opponent attack
  # high end is lag four touches: Texas attack -> block touch -> dig -> set -> attack
  # same point, same match, attack, against Texas, after a Texas attack, which was a tip

lag2 = data0 %>% filter(point_id == lag(point_id,2),
                        match_id == lag(match_id,2),
                        skill == "Attack",
                        opp_team == "University of Texas at Austin",
                        lag(team,2) == "University of Texas at Austin",
                        lag(skill_subtype,2) == "Tip")

lag3 = data0 %>% filter(point_id == lag(point_id,3),
                        match_id == lag(match_id,3),
                        skill == "Attack",
                        opp_team == "University of Texas at Austin",
                        lag(team,3) == "University of Texas at Austin",
                        lag(skill_subtype,3) == "Tip")

lag4 = data0 %>% filter(point_id == lag(point_id,4),
                        match_id == lag(match_id,4),
                        skill == "Attack",
                        opp_team == "University of Texas at Austin",
                        lag(team,4) == "University of Texas at Austin",
                        lag(skill_subtype,4) == "Tip")

next_attack_data = rbind(lag2, lag3, lag4)
  # 188 observations

data0 %>%
  filter(team == "University of Texas at Austin",
         skill_subtype == "Spike cover",
         lag(skill_subtype,2) == "Tip",
         lag(eval,2) != "Blocked")
  # blocked for reattack, not labeled as such

data0 %>%
  filter(team == "University of Texas at Austin",
         skill_subtype == "Tip")
  # 435 observations

  # 255 of these attacks were not a kill, blocked, or error
  # So what am I missing? Where are the other 67 attacks?
  # Hypothesis: some of those attacks marked 'poor' were actually blocked for free ball
  # Alternate hypothesis: these are free balls returned to us
  # Is it reasonable for that to happen on 67/435 = 15.4% of attacks? Maybe.

  # Solved: 31 of these were free balls returned to us
  # The rest, indeed, were blocked for reattack but not labeled as such


# What kind of attack were they able to get?
next_attack_data %>%
  summarise(Hard_spike = prop(skill_subtype == "Hard spike")*188/(188+31),
            Soft_spike = prop(skill_subtype == "Soft spike/topspin")*188/(188+31),
            Tip = prop(skill_subtype == "Tip")*188/(188+31))
freeballs = 31
freeballs/(freeballs + count(next_attack_data))

# compared to subtype of opponents' transition attack usually
data0 %>%
  filter(opp_team == "University of Texas at Austin",
         skill == "Attack",
         phase == "Transition") %>%
  summarise(Hard_spike = prop(skill_subtype == "Hard spike")*1074/(1074+209),
            Soft_spike = prop(skill_subtype == "Soft spike/topspin")*1074/(1074+209),
            Tip = prop(skill_subtype == "Tip")*1074/(1074+209))
  # No significant difference here:
  # Opponents got a hard spike 70.2% vs 70.9% on all transition attacks
freeballs = 209
freeballs/(freeballs + 1074)

# How effective was that attack?
next_attack_data %>%
  summarise(Kill = prop(eval == "Winning attack"),
            Positive = prop(eval == "Positive, good attack"),
            Poor = prop(eval == "Poor, easily dug"),
            #BFR = prop(eval == "Blocked for reattack"),
            Blocked = prop(eval == "Blocked"),
            Error = prop(eval == "Error")) %>%
  mutate(attack_score = 3*Kill + 2*Positive + Poor,
         hitting_pct = Kill - Error - Blocked)

# compared to all of opponents' transition attacks against Texas
data0 %>%
  filter(opp_team == "University of Texas at Austin",
         skill == "Attack",
         phase == "Transition") %>%
  summarise(Kill = prop(eval == "Winning attack"),
            Positive = prop(eval == "Positive, good attack"),
            Poor = prop(eval == "Poor, easily dug"),
            #BFR = prop(eval == "Blocked for reattack"),
            Blocked = prop(eval == "Blocked"),
            Error = prop(eval == "Error")) %>%
  mutate(attack_score = 3*Kill + 2*Positive + Poor,
         hitting_pct = Kill - Error - Blocked)
  # Overall, the metrics were not significantly different
  # Following a Texas tip, opponents hit .303 kill % with a 1.564 attack score
  # In all transition attacks, opponents hit .303 kill % with a 1.558 attack score
  # One notable difference, though, was that hitting % was higher (.191 vs .153 normally)

