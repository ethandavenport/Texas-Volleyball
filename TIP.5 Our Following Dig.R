# Our Following Dig

# Libraries
library(datavolley)
library(tidyverse)
library(moderndive)
library(mosaic)
library(dplyr)
library(base)
library(ggplot2)

# selecting data for the dig of the attack after Texas' tip
# low end is lag two touches: Texas tip -> opponent dig (overpass) -> Texas attack
# high end is lag six touches: Tip -> block touch -> dig -> set -> attack -> block touch -> dig
# same point, same match, attack, against Texas, after a Texas attack, which was a tip

lag2_dig = data0 %>% filter(point_id == lag(point_id,2),
                            match_id == lag(match_id,2),
                            skill == "Dig" | skill == "Freeball",
                            team == "University of Texas at Austin",
                            lag(team,2) == "University of Texas at Austin",
                            lag(skill_subtype,2) == "Tip")

lag3_dig = data0 %>% filter(point_id == lag(point_id,3),
                            match_id == lag(match_id,3),
                            skill == "Dig" | skill == "Freeball",
                            team == "University of Texas at Austin",
                            lag(team,3) == "University of Texas at Austin",
                            lag(skill_subtype,3) == "Tip")

lag4_dig = data0 %>% filter(point_id == lag(point_id,4),
                            match_id == lag(match_id,4),
                            skill == "Dig" | skill == "Freeball",
                            team == "University of Texas at Austin",
                            lag(team,4) == "University of Texas at Austin",
                            lag(skill_subtype,4) == "Tip")

lag5_dig = data0 %>% filter(point_id == lag(point_id,5),
                            match_id == lag(match_id,5),
                            skill == "Dig" | skill == "Freeball",
                            team == "University of Texas at Austin",
                            lag(team,5) == "University of Texas at Austin",
                            lag(skill_subtype,5) == "Tip")

lag6_dig = data0 %>% filter(point_id == lag(point_id,6),
                            match_id == lag(match_id,6),
                            skill == "Dig" | skill == "Freeball",
                            team == "University of Texas at Austin",
                            lag(team,6) == "University of Texas at Austin",
                            lag(skill_subtype,6) == "Tip")

next_dig_data = rbind(lag2_dig, lag3_dig, lag4_dig, lag5_dig, lag6_dig)

# how often did we get a free ball after tipping?
next_dig_data %>%
  summarise(prop_free = prop(skill == "Freeball"))
  # 16.1% of the time

next_dig_data %>%
  filter(skill == "Freeball") %>%
  count()

# how often do we normally get a free ball?
data0 %>%
  filter(team == "University of Texas at Austin",
         skill == "Dig" | skill == "Freeball") %>%
  summarise(prop_free = prop(skill == "Freeball"))
  # 12.4% of the time

# evaluation of free ball usage
data0 %>%
  filter(team == "University of Texas at Austin",
         skill == "Attack",
         lag(skill,2) == "Freeball" | lag(skill,1) == "Freeball",
         lag(team,2) == "University of Texas at Austin") %>%
  summarise(Kill = prop(eval == "Winning attack"),
            Positive = prop(eval == "Positive, good attack"),
            Poor = prop(eval == "Poor, easily dug"),
            #BFR = prop(eval == "Blocked for reattack"),
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
            #BFR = prop(eval == "Blocked for reattack"),
            Blocked = prop(eval == "Blocked"),
            Error = prop(eval == "Error")) %>%
  mutate(attack_score = 3*Kill + 2*Positive + Poor,
         hitting_pct = Kill - Error - Blocked)
  # Kill percentage was significantly higher (.492 vs .420 normally)
  # Attack score was significantly higher (1.933 vs 1.813 normally)
  # Hitting percentage was significantly higher (.354 vs .284 normally)
  # Interestingly, we make error at essentially the same rate
prop.test(x=0.4204113*195, n=195, conf.level=.95, correct=FALSE)
pbinom(0.4923077*195, size = 195, prob = 0.4204113 )
  # Kill percentage is statistically significant: CI is .353 to .491
prop.test(x=0.2840823*195, n=195, conf.level=.95, correct=FALSE)
pbinom(0.3538462*195, size = 195, prob = 0.2840823 )
  # Hitting percentage is statistically significant: CI is .225 to .351

data0 %>%
  filter(skill == "Dig",
         evaluation_code == "/")

# evaluation of our next dig
next_dig_data %>%
  filter(skill == "Dig") %>%
  summarise(Perfect = prop(eval == "Perfect dig"),
            Good = prop(eval == "Good dig"),
            NSAP = prop(eval == "No structured attack possible"),
            Error = prop(eval == "Error")) %>%
  mutate(dig_score = 3*Perfect + 2*Good + NSAP,
         in_system = Perfect + Good)

# compared to all of our other digs
data0 %>%
  filter(team == "University of Texas at Austin",
         skill == "Dig")
summarise(Perfect = prop(eval == "Perfect dig"),
          Good = prop(eval == "Good dig"),
          NSAP = prop(eval == "No structured attack possible"),
          Error = prop(eval == "Error")) %>%
  mutate(dig_score = 3*Perfect + 2*Good + NSAP,
         in_system = Perfect + Good)
  # Our digging was noticeably better following our tip
  # Dig score came in at 2.02 following our tip, vs 1.87 in all scenarios
  # We got an in-system dig 74.7% of the time, vs 70.5% in all scenarios
prop.test(x=0.7054054*193, n=193, conf.level=.95, correct=FALSE)
pbinom(0.7469136*193, size = 193, prob = 0.7054054)
  # This difference is not statistically significant. If the true in-system dig rate was
  # equal, there would be a 9.2% chance of an in-system dig rate 74.7% or higher
  # However, this is different from practical significance.

# Digging Significance Analysis

# What happens on the attack after an in-system dig?
data0 %>%
  filter(team == "University of Texas at Austin",
         skill == "Attack",
         lag(team,2) == "University of Texas at Austin",
         lag(eval,2) == "Perfect dig" | lag(eval,2) == "Good dig") %>%
  summarise(Kill = prop(eval == "Winning attack"),
            Positive = prop(eval == "Positive, good attack"),
            Poor = prop(eval == "Poor, easily dug"),
            #BFR = prop(eval == "Blocked for reattack"),
            Blocked = prop(eval == "Blocked"),
            Error = prop(eval == "Error")) %>%
  mutate(attack_score = 3*Kill + 2*Positive + Poor,
         hitting_pct = Kill - Error - Blocked)

# compared to out-of-system digs?
data0 %>%
  filter(team == "University of Texas at Austin",
         skill == "Attack",
         lag(team,2) == "University of Texas at Austin",
         lag(evaluation_code,2) == "-" | lag(evaluation_code,2) == "=") %>%
  summarise(Kill = prop(eval == "Winning attack"),
            Positive = prop(eval == "Positive, good attack"),
            Poor = prop(eval == "Poor, easily dug"),
            #BFR = prop(eval == "Blocked for reattack"),
            Blocked = prop(eval == "Blocked"),
            Error = prop(eval == "Error")) %>%
  mutate(attack_score = 3*Kill + 2*Positive + Poor,
         hitting_pct = Kill - Error - Blocked)


