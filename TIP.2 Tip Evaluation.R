# Tip Evaluation

# Libraries
library(datavolley)
library(tidyverse)
library(moderndive)
library(mosaic)
library(dplyr)
library(base)
library(ggplot2)


# create evaluation as categorical variable
data0 = mutate(data0, eval = factor(evaluation))

# eval table
tip_evals = data0 %>%
  filter(skill_subtype == "Tip") %>%
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
  filter(num_tips >= 101)
  # According to this arbitrary tip score metric, Texas(1.690) comes in at no. 3 of 15
  # Purdue(1.696) leads the nation, followed by Kentucky(1.692)
  # Texas also comes in at no. 3 in the nation in hitting percentage at .218
  # Purdue(.228) and Kansas(.220) lead the way

# compared to non-tip attacks
nontip_evals = data0 %>%
  filter(skill == "Attack", skill_subtype != "Tip", attack_description != "Setter Dump") %>%
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
  filter(num_attacks >= 500)
  # Again, arbitrary metric, but Texas(1.95) leads the nation easily
  # They are followed by San Diego(1.89), Marquette(1.88), Louisville(1.85)
  # Texas also easily leads the nation in hitting percentage (.363)
  # They are followed by San Diego(.321), Marquette(.316), Louisville(.301)
  # Interestingly, Texas ranked dead last in attacks labeled 'Positive', but
  # that is because so many of their attacks were kills

