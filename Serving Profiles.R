# Serving Profiles


library(tidyverse)


# xSO for each passing evaluation code
power %>%
  filter(skill == "Reception") %>%
  group_by(evaluation_code) %>%
  summarize(xSO = mean(team == point_won_by))

# baseline SO
baseline <- power %>%
  filter(skill == "Serve") %>%
  summarize(SO = mean(team != point_won_by)) %>%
  pull()

# identify good servers based on xSO
good_servers <- power %>%
  filter(skill == "Serve") %>%
  mutate(one_touch_future = ifelse(is.na(one_touch_future), "NA", one_touch_future)) %>% #NAs can mess up props
  filter(
    evaluation_code %in% c("=", "#") |
    one_touch_future %in% c("Reception #", "Reception +", "Reception !", "Reception -", "Reception /")
  ) %>% # remove any rare weird things going one
  group_by(player_name, team) %>%
  summarize(
    error = mean(evaluation_code == "="),
    pass4 = mean(one_touch_future == "Reception #"),
    pass3 = mean(one_touch_future == "Reception +"),
    in_system = mean(one_touch_future %in% c("Reception #", "Reception +")),
    pass2 = mean(one_touch_future == "Reception !"),
    pass1 = mean(one_touch_future == "Reception -"),
    OOS = mean(one_touch_future %in% c("Reception !", "Reception -")),
    overpass = mean(one_touch_future == "Reception /"),
    ace = mean(evaluation_code == "#"),
    prop_sum = 
      mean(evaluation_code %in% c("=", "#")) + 
      mean(one_touch_future %in% c("Reception #", "Reception +", "Reception !", "Reception -", "Reception /")),
    xSO =
      1*mean(evaluation_code == "=") +
      0.649*mean(one_touch_future == "Reception #") +
      0.633*mean(one_touch_future == "Reception +") +
      0.589*mean(one_touch_future == "Reception !") +
      0.504*mean(one_touch_future == "Reception -") +
      0.283*mean(one_touch_future == "Reception /"),
    count = n()
  ) %>%
  filter(count >= 50) %>%
  arrange(xSO) %>%
  filter(xSO < baseline) %>%
  mutate(aggression = ace + error)

# identify player of interest
player <- "Averi Carlson"

# create individual player profile
player_profile <- power %>%
  filter(skill == "Serve", player_name == player) %>%
  mutate(one_touch_future = ifelse(is.na(one_touch_future), "NA", one_touch_future)) %>% #NAs can mess up props
  filter(
    evaluation_code %in% c("=", "#") |
      one_touch_future %in% c("Reception #", "Reception +", "Reception !", "Reception -", "Reception /")
  ) %>% # remove any rare weird things going one
  group_by(player_name, team) %>%
  summarize(
    error = mean(evaluation_code == "="),
    pass4 = mean(one_touch_future == "Reception #"),
    pass3 = mean(one_touch_future == "Reception +"),
    in_system = mean(one_touch_future %in% c("Reception #", "Reception +")),
    pass2 = mean(one_touch_future == "Reception !"),
    pass1 = mean(one_touch_future == "Reception -"),
    OOS = mean(one_touch_future %in% c("Reception !", "Reception -")),
    overpass = mean(one_touch_future == "Reception /"),
    ace = mean(evaluation_code == "#"),
    prop_sum = 
      mean(evaluation_code %in% c("=", "#")) + 
      mean(one_touch_future %in% c("Reception #", "Reception +", "Reception !", "Reception -", "Reception /")),
    xSO =
      1*mean(evaluation_code == "=") +
      0.649*mean(one_touch_future == "Reception #") +
      0.633*mean(one_touch_future == "Reception +") +
      0.589*mean(one_touch_future == "Reception !") +
      0.504*mean(one_touch_future == "Reception -") +
      0.283*mean(one_touch_future == "Reception /"),
    count = n()
  ) %>%
  mutate(
    aggression = ace + error,
    aggression_group = player_name
  ) %>%
  pivot_longer(cols = c(ace, error, starts_with("pass"), in_system, OOS, overpass),
               names_to = "pass_type",
               values_to = "mean_value") %>%
  ungroup() %>%
  select(aggression_group, pass_type, mean_value)


# specify aggression group cutoffs
q25 <- quantile(good_servers$ace + good_servers$error, 0.25)
q75 <- quantile(good_servers$ace + good_servers$error, 0.75)
q90 <- quantile(good_servers$ace + good_servers$error, 0.90)

# create serving profiles
server_profiles <- good_servers %>%
  mutate(
    aggression_group = case_when(
      aggression > q90 ~ "Very High",
      aggression > q75 ~ "High",
      aggression > q25 ~ "Medium",
      TRUE ~ "Low"
  )) %>%
  group_by(aggression_group) %>%
  summarize(
    error = mean(error),
    pass4 = mean(pass4),
    pass3 = mean(pass3),
    in_system = mean(in_system),
    pass2 = mean(pass2),
    pass1 = mean(pass1),
    OOS = mean(OOS),
    overpass = mean(overpass),
    ace = mean(ace)
  ) %>%
  pivot_longer(cols = c(ace, error, starts_with("pass"), in_system, OOS, overpass),
               names_to = "pass_type",
               values_to = "mean_value") %>%
  mutate(
    aggression_group = factor(aggression_group, levels = c(player, "Low", "Medium", "High", "Very High")),
    pass_type = factor(pass_type, levels = 
                         c("ace", "overpass", 
                           "pass1", "pass2", "OOS", 
                           "pass3", "pass4", "in_system",
                           "error"))
  )

# plot profiles
server_profiles %>%
  rbind(player_profile) %>%
  filter(pass_type %in% c("ace", "overpass", "OOS", "in_system", "error")) %>%
  #filter(pass_type %in% c("ace", "overpass", "pass1", "pass2", "pass3", "pass4", "error")) %>%
  ggplot(aes(x = mean_value, y = aggression_group, fill = pass_type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(mean_value*100, 1), "%")),
            position = position_stack(vjust = 0.5), color = "white", size = 3.5) +
  scale_fill_manual(values =
                      c(
                        "ace" = "#74ADD1",
                        "overpass" = "#B7C68B",
                        "OOS" = "#66BD63",
                        "in_system" = "#FDAE61",
                        "error" = "#F46D43"
                      )) +
  labs(
    x = "Average Proportion of Each Pass Outcome",
    y = "Aggression Group",
    title = "Serve Outcome Quality by Aggression Group",
    fill = "Pass Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust = 0.5))

# output sample size for that player
power %>%
  filter(skill == "Serve", player_name == player) %>%
  summarize(n())

  
  