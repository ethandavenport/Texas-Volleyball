# Tipping Success

# Libraries
library(datavolley)
library(tidyverse)
library(moderndive)
library(mosaic)
library(dplyr)
library(base)
library(ggplot2)

# Table of Contents:
# How many lines of data represented by each team? .... 27
# Kill % by team ...................................... 33
# Tip Evaluation ...................................... 55
# PCT Points Won ...................................... 91
  # eliminate duplicates .............................. 116
# Opponent Transition Attack .......................... 132
  # Type of attack..................................... 175
  # Effectiveness of attack ........................... 195
# Our Following Dig ................................... 223
  # Digging Significance Analysis ..................... 340
# Our Following Attack ................................ 378
# Summary ............................................. 484


# How many lines of data represented by each team?
data0 %>%
  count(team) %>%
  arrange(desc(n))

# Kill % by team
data0 %>%
  filter(skill_subtype == "Tip") %>%
  group_by(team) %>%
  summarise(kill_pct = prop(winning_attack),
            num_tips = count(team)) %>%
  filter(num_tips >= 101) %>%
  arrange(desc(kill_pct)) %>%
  head(20)

  # compared to non-tip attacks
data0 %>%
  filter(skill == "Attack", skill_subtype != "Tip", attack_description != "Setter Dump") %>%
  group_by(team) %>%
  summarise(kill_pct = prop(winning_attack),
            num_attacks = count(team)) %>%
  filter(num_attacks >= 500) %>%
  arrange(desc(kill_pct)) %>%
  head(20)

################################################################################

# Tip Evaluation

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

################################################################################

# PCT Points Won

  # create point-winning team as categorical variable
data0 = mutate(data0, team_won_point = factor(point_won_by))

  # pct points won table
data0 %>%
  filter(skill_subtype == "Tip") %>%
  group_by(team) %>%
  summarise(pct_points_won = prop(team_won_point == team),
            num_points = count(team)) %>%
  filter(num_points >= 101) %>%
  arrange(desc(pct_points_won)) %>%
  head(20)

  # compared to non-tip attacks
data0 %>%
  filter(skill == "Attack", skill_subtype != "Tip", attack_description != "Setter Dump") %>%
  group_by(team) %>%
  summarise(pct_points_won = prop(team_won_point == team),
            num_points = count(team)) %>%
  filter(num_points >= 500) %>%
  arrange(desc(pct_points_won)) %>%
  head(20)

  # eliminate duplicates
nodupes_tips = data0 %>%
  filter(skill_subtype == "Tip") %>%
  select(match_id, point_id, team, team_won_point)
nodupes_tips = unique(nodupes_tips)

nodupes_tips %>%
  group_by(team) %>%
  summarise(pct_points_won = prop(team_won_point == team)*100,
            num_points = count(team)) %>%
  filter(num_points >= 101) %>%
  arrange(desc(pct_points_won)) %>%
  head(20)

################################################################################

# Opponent Transition Attack

  # create opposing team variable
data0$opp_team = ifelse(data0$team == data0$visiting_team,
                        data0$home_team, ifelse(data0$team == data0$home_team,
                                                data0$visiting_team,""))

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

################################################################################

# Our Following Dig

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

next_dig_data %>%
  filter(skill == "Freeball") %>%
  count()

  # how often do we normally get a free ball?
data0 %>%
  filter(team == "University of Texas at Austin",
         skill == "Dig" | skill == "Freeball") %>%
  summarise(prop_free = prop(skill == "Freeball"))

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

prop.test(x=0.4204113*195, n=195, conf.level=.95, correct=FALSE)
pbinom(0.4923077*195, size = 195, prob = 0.4204113 )

prop.test(x=0.2840823*195, n=195, conf.level=.95, correct=FALSE)
pbinom(0.3538462*195, size = 195, prob = 0.2840823 )


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

prop.test(x=0.7054054*193, n=193, conf.level=.95, correct=FALSE)
pbinom(0.7469136*193, size = 193, prob = 0.7054054)

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

data0 %>%
  filter(team == "University of Texas at Austin",
         skill == "Attack") %>%
  summarise(Kill = prop(eval == "Winning attack"))

################################################################################

# Our Following Attack

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

prop.test(x=0.7236534*162, n=162, conf.level=.95, correct=FALSE)
pbinom(0.7405063*193, size = 193, prob = 0.7236534)

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

prop.test(x=0.4204113*162, n=162, conf.level=.95, correct=FALSE)
pbinom(0.4259259*162, size = 162, prob = 0.4204113)

prop.test(x=0.2840823*162, n=162, conf.level=.95, correct=FALSE)
pbinom(0.3024691*162, size = 162, prob = 0.2840823)


################################################################################

# Summary:

# This is meant to be a comprehensive analysis of the effectiveness of Texas' tip attack

# That begins with the primary goals of an attack: a kill. How often did Texas' tip
# attack produce a kill? How does that compare to other teams nationally? Here are the
# results:
# 1. Purdue (.322)
# 2. Kentucky (.317)
# 3. Texas (.315)
# These were the only three teams (out of 15 relevant teams) to kill at least .300 of
# their tips. How does this result to compare to all other attacks?
# 1. Texas (.497)
# 2. San Diego (.442)
# 3. Louisville (.440)
# Obviously, kill percentage is higher when you're actually able to get a swing on the
# ball. But according to the kill percentage metric alone, Texas' tip attack appears to
# lag slightly behind its other attacks relative to the rest of the country, while also
# remaining among the very best.

# But we want to know more information than just how terminal the tip was. After all,
# tips are often meant to get the opponent out of system more than putting the point
# away. To discover this, I created an arbitrary attack score metric equal to:
# 3*prop of kills + 2*prop of positive attacks + prop of poor attacks
# Similar to a passing score, this metric is meant to give an aggregate view into the
# proficiency of attack. The accuracy of this metric, I open to disputation. Here are the
# rankings according to this metric:
# 1. Purdue (1.696)
# 2. Kentucky (1.692)
# 3. Texas (1.690)
# How do these rates compare to non-tip attacks?
# 1. Texas (1.950)
# 2. San Diego (1.893)
# 3. Marquette (1.876)
# These results are similar to kill percentage. When Texas tips the ball, they become
# more 'among the best in the country' rather than easily the best. Another interesting
# insight from this: Texas ranked dead last out of 15 teams in attacks labeled 'Positive',
# but that is because so many of their attacks were kills.

# Another metric to measure aggregate proficiency is hitting percentage. I included
# blocks in this metric, although I question that choice myself. Here are the rankings:
# 1. Purdue (.228)
# 2. Kansas (.220)
# 3. Texas (.218)
# How do these rates compare to non-tip attacks?
# 1. Texas (.363)
# 2. San Diego (.321)
# 3. Marquette (.316)
# Just as before, tipping overall puts a damper on the figures. Texas continues to rank
# 'among the best' here when tipping, compared to their massive national lead in hitting
# percentage on non-tips.

# Next, I wanted to see how often we won the point when we tipped the ball at some point
# in the rally. Here are the results:
# 1. Texas (63.7%)
# 2. Minnesota (62.8%)
# 3. Purdue (62.7%)
# How does this compare to non-tip attacks?
# 1. Texas (70.6%)
# 2. San Diego (68.8%)
# 3. Wisconsin (66.2%)
# However, it's important to note that this metric for non-tip attacks may not have much 
# meaning: this measures how often a given team won the point when they got a non-tip
# attack at some point in the rally. There are likely lots of duplicates in this data
# because of this fact.

# Considering this potential confounder, I also wanted to analyze the results for tip
# attacks while removing duplicates, since multiple tips in the same rally would double
# count the win or loss for that point. Here are those results:
# 1. Texas (63.1%)
# 2. Purdue (62.2%)
# 3. Minnesota (62.1%)
# This data does not differ significantly when we remove duplicates.

# Another significant factor of our tipping effectiveness is what happened on the 
# opponent's following attack. So let's analyze it:

# The first part of this is knowing what kind of ball came back over the net. It turns
# out that opponents returned free balls at a higher rate following a Texas tip
# (16.1% vs 12.4% normally). Out of the proportion of those where the opponent actually
# got an attack rather than sending a free ball, they got a hard spike on a lower
# proportion of attacks after a Texas tip (70.2% vs 75.2% normally). This difference
# was also reflected by a higher proportion of soft spikes (13.5% vs 8.5% normally).

# Next, we need to analyze how effective those attacks were. Following a Texas tip,
# kill percentage and positive attacks decreased in proportion (.303 vs .327 normally)
# and (.080 vs .114 normally), respectively. There was a corresponding increase in
# proportion of poor attacks (.495 vs .401 normally). However, the percentage of those
# attacks blocked decreased (.032 vs .072 normally). Overall, 'attack score', which,
# again, is an arbitrary metric open for disputation, is worse following a Texas tip
# (1.564 vs 1.612 normally).

# We can also analyze the effectiveness of these attacks by hitting percentage.
# Interestingly, the opponent's hitting percentage is actually slightly higher on
# average following a Texas tip attack (.191 vs .174 normally). Although, this is mostly
# due to the difference in blocking percentage, whose relevance I questioned earlier.

# Finally, I provide some information about how well we dug those attacks. I created a
# similar metric to attack score for passing given by:
# 3*prop of perfect digs + 2*prop of good digs + prop of 'no structured attack possible'
# This metric, however, is not seen as arbitrary, and is more commonly used in volleyball
# statistics. According to this metric, our digging was noticeably better following our
# tip (2.02 vs 1.87 normally)

# Overall, this analysis provides a mixed bag of results. Texas dominates nationally in
# metrics for non-tip attacks including kill percentage, attack score, and hitting
# percentage. However, Texas appears to be more 'among the best' in these metrics on
# tip attacks, as opposed to outright dominance.

# Despite this, Texas leads the way in tipping effectiveness in some other key ways.
# They win the greatest percentage of points when tipping the ball at some point in the
# rally. They are also noticeably more likely to receive a free ball and less likely
# to receive a hard spike following their tip. The overall attack score of the opponent
# is worse on attack following Texas' tip. As a result, Texas is able to get better
# digging numbers, hopefully setting them up for more success on their next attack.

# Some areas of future study: how do these results for tips compare to those for our soft
# spikes? This would provide some more insight into the question of whether it's better
# for a player to tip the ball or get a soft swing on the ball given an out of system
# set. Additionally, The quality of opponents' attack appears to decrease after Texas'
# tip. Is this also true for other teams? To what extent?



