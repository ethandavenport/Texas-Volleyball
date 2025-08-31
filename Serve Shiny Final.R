# Serve Shiny



# KEY STEP: DEFINE ORIGINAL DATA FILE NAME
data <- power

# if you've already run the whole file once and just want to launch the shiny app, uncomment this:
shinyApp(ui = serve_arrows_ui, server = serve_arrows_server)





# Load Libraries
library(shiny)
library(tidyverse)
library(ClusterR)
library(grid)
library(datavolley)
library(tools)

# Load cluster data
clusters <- read.csv('clusters.csv')

########## Perform Data Wrangling ########## 


# 1. Bin only the data coordinates
bin_size <- 0.05
data_binned <- data %>%
  mutate(
    x_bin = round(end_coordinate_x / bin_size) * bin_size,
    y_bin = round(end_coordinate_y / bin_size) * bin_size
  )

# 2. Get all unique combinations needing matches
match_grid <- data_binned %>%
  filter(skill == "Reception") %>%
  distinct(evaluation_code, x_bin, y_bin)

# 3. Define the matching function
get_closest_epv <- function(eval_code, x, y, clusters) {
  filtered <- clusters %>% filter(eval_code == !!eval_code)
  
  if (nrow(filtered) == 0) {
    return(NA)  # no match found
  }
  
  filtered <- filtered %>%
    mutate(distance = sqrt((V1 - x)^2 + (V2 - y)^2)) %>%
    arrange(distance)
  
  filtered$pass_epv[1]  # return the pass_epv of the closest point
}

# 4. Find closest cluster pointt
lookup <- match_grid %>%
  rowwise() %>%
  mutate(pass_epv = if (!is.na(x_bin) & !is.na(y_bin)) {
    get_closest_epv(evaluation_code, x_bin, y_bin, clusters)
  } else {
    NA_real_
  }) %>%
  ungroup()

# 5. Join lookup table back to the binned data
pass_epv <- data_binned %>%
  left_join(lookup, by = c("evaluation_code", "x_bin", "y_bin")) %>%
  mutate(pass_epv = if_else(skill == "Reception", pass_epv, NA_real_)) %>%
  select(-x_bin, -y_bin)

# 6. Make anomaly corrections
baseline <- data %>%
  filter(skill == "Serve") %>%
  summarize(SO = mean(opponent == point_won_by)) %>%
  pull()

pass_epv <- pass_epv %>%
  mutate(pass_epv = case_when(
    skq == "Reception #" & (end_coordinate_y >= 3.5 | is.na(end_coordinate_y)) ~ 0.649,
    skq == "Reception +" & (end_coordinate_y >= 3.5 | is.na(end_coordinate_y)) ~ 0.633,
    skq == "Reception !" & (end_coordinate_y >= 3.5 | is.na(end_coordinate_y)) ~ 0.589,
    skq == "Reception -" & (end_coordinate_y >= 3.5 | is.na(end_coordinate_y)) ~ 0.504,
    skq == "Reception =" ~ 0,
    output_type == "reception_overpass_attack" ~ 0.228,
    output_type == "reception_overpass_non_attack" ~ 0.362,
    skq == "Reception /" ~ baseline,
    TRUE ~ pass_epv
  )) %>%
  mutate(pass_epv = case_when(
    skq == "Serve #" ~ 0,
    skq == "Serve =" ~ 1,
    skill == "Serve" & lead(skill) == "Reception" ~ lead(pass_epv),
    output_type == "serve_overpass_attack" ~ 0.228,
    output_type == "serve_overpass_non_attack" ~ 0.362,
    skill == "Serve" ~ baseline,
    TRUE ~ pass_epv
  ))


########## Perform Other Data Cleaning ########## 


# update outdated conferences (before conference realignment)
pass_epv <- pass_epv %>%
  mutate(conference = case_when(
    team_short %in% c("texas", "oklahoma") ~ "SEC",
    team_short %in% c("byu", "cincinnati", "houston", "centralflorida", 
                      "colorado", "utah", "arizona", "arizonastate") ~ "Big-12",
    team_short %in% c("oregon", "washington", "usc", "ucla") ~ "Big Ten",
    team_short %in% c("cal", "smu", "stanford") ~ "ACC",
    TRUE ~ conference
  ))

# # capitalize the team shortenings
# pass_epv <- pass_epv %>%
#   mutate(
#     team_short = paste0(toupper(substring(team_short, 1, 1)), tolower(substring(team_short, 2))),
#     opponent_short = paste0(toupper(substring(opponent_short, 1, 1)), tolower(substring(opponent_short, 2)))
#   )

#create simplified serve_type variable
pass_epv <- pass_epv %>%
  mutate(
    simple_serve_type = case_when(
      skill_type %in% c("Jump-float serve", "Float serve") ~ "Float Serve",
      skill_type %in% c("Topspin serve", "Jump serve") ~ "Topspin Serve",
      TRUE ~ "other" #optional catch-all for other types 
    ))

#create data set used for graphs in shiny app
serve_angle <- pass_epv %>%
  mutate(one_touch_future = ifelse(is.na(one_touch_future), "NA", one_touch_future)) %>% #NAs can mess up props
  filter(
    skill == "Serve",
    evaluation_code %in% c("#", "=") |
      one_touch_future %in% c("Reception #", "Reception +", "Reception !", "Reception -", "Reception /")
  ) %>%
  mutate(xSO = pass_epv) %>%
  select(player_name, team, team_short, conference, home_team, visiting_team, opponent, opponent_short,
         home_setter_position, visiting_setter_position, simple_serve_type, position,
         start_coordinate_x, start_coordinate_y, end_coordinate_x, end_coordinate_y, 
         evaluation_code, one_touch_future, xSO)


########## Set Baselines ########## 


no_error_xSO <- pass_epv %>% 
  filter(skill == "Serve", evaluation_code != "=") %>% 
  summarize(xSO = mean(pass_epv)) %>%
  pull()

no_error_stdev <- pass_epv %>%
  filter(skill == "Serve", evaluation_code != "=") %>%
  group_by(player_name) %>%
  summarize(xSO = mean(pass_epv), n = n()) %>%
  ungroup() %>%
  summarize(sd = sqrt(sum(n * (xSO - weighted.mean(xSO, n))^2) / sum(n))) %>%
  pull()


########## Build and Run Shiny App ##########


#ui
serve_arrows_ui = fluidPage(
  titlePanel(
    div("Serve Clustering and xSO Visualization", style = "text-align: center;")
  ),
  
  
  tabsetPanel(
    
    ######## TAB 1
    tabPanel("Player and Team Comparisons",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 
                 div(
                   style = "padding: 10px; margin-bottom: 15px; border: 1px solid #ddd; border-radius: 8px; background-color: #f9f9f9;",
                   tags$h4("Server Info", style = "margin-top: 0;"),
                   selectInput("conference", "Select Conference:", choices = NULL, selected = "SEC"),
                   selectInput("team", "Select Team:", choices = NULL, selected = "texas"),
                   selectInput("player", "Select Player:", choices = NULL, selected = "Emma Halter")
                 ),
                 
                 div(
                   style = "padding: 10px; border: 1px solid #ddd; border-radius: 8px; background-color: #f9f9f9;",
                   tags$h4("Opponent Info", style = "margin-top: 0;"),
                   selectInput("pass_conference", "Select Opponent Conference:", choices = NULL, selected = "SEC"),
                   selectInput("pass_team", "Select Opponent Team:", choices = NULL, selected = "Kentucky"),
                   selectInput("pass_setter_position", "Select Opponent Setter Position:", choices = 1:6, selected = 1)
                 )
               ),
               mainPanel(
                 width = 9,
                 tagList(
                   fluidRow(
                     column(6,
                            div(
                              style = "width: 100%;",
                              tagList(
                                plotOutput("player_plot", height = "700px", width = "100%"),
                                div(style = "text-align: center;", textOutput("player_sample_text"))
                              )
                            )
                     ),
                     column(6,
                            div(
                              style = "width: 100%;",
                              tagList(
                                plotOutput("rotation_plot", height = "700px", width = "100%"),
                                div(style = "text-align: center;", textOutput("rotation_sample_text"))
                              )
                            )
                     )
                   ),
                   div(
                     style = "text-align: center;", 
                     HTML(paste0(
                       "Average xSO (no errors): ", round(no_error_xSO, 3), "<br>",
                       "STDEV xSO (no errors): ", round(no_error_stdev, 3)
                     ))),
                 )
               )
             )
    ),
    
    ######## TAB 2
    tabPanel("Float and Topspin Servers",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 selectInput("player_types", "Select Player:", choices = NULL),
                 br(),
                 textOutput("player_team"),
                 textOutput("player_position")
               ),
               mainPanel(
                 width = 9,
                 tagList(
                   fluidRow(
                     column(6,
                            div(
                              style = "width: 100%;",
                              tagList(
                                plotOutput("plot_float", height = "700px", width = "100%"),
                                div(style = "text-align: center; margin-top: -20px;", uiOutput("float_sample_text"))
                              )
                            )
                     ),
                     column(6,
                            div(
                              style = "width: 100%;",
                              tagList(
                                plotOutput("plot_topspin", height = "700px", width = "100%"),
                                div(style = "text-align: center; margin-top: -20px;", uiOutput("topspin_sample_text"))
                              )
                            )
                     )
                   ),
                   
                   div(
                     style = "text-align: center;", 
                     HTML(paste0(
                       "Average xSO (no errors): ", round(no_error_xSO, 3), "<br>",
                       "STDEV xSO (no errors): ", round(no_error_stdev, 3)
                     ))),
                 )
               )
             )
    )
  )
)


#server
serve_arrows_server = function(input, output, session) {
  
  ######## SERVE TYPE
  
  #players with multiple serve types
  players_with_multiple <- serve_angle %>%
    filter(simple_serve_type %in% c("Float Serve", "Topspin Serve")) %>%
    group_by(player_name, simple_serve_type) %>%
    summarise(n_serves = n(), .groups = "drop") %>%
    filter(n_serves >= 20) %>%
    group_by(player_name) %>%
    filter(n_distinct(simple_serve_type) > 1) %>%
    pull(player_name) %>%
    unique()
  
  
  #update dropdown
  updateSelectInput(session, "player_types", choices = players_with_multiple)
  
  #get chosen player data
  player_data = reactive({
    req(input$player_types)
    serve_angle %>% 
      filter(player_name == input$player_types)
  })
  
  #update player team and position
  output$player_team = renderText({
    paste("Team:", unique(player_data()$team))
  })
  output$player_position = renderText({
    positions = unique(player_data()$position)
    paste("Position:", paste(positions, collapse = ", "))
  })
  
  
  output$plot_float = renderPlot({
    data <- player_data() %>% 
      filter(simple_serve_type == "Float Serve", evaluation_code != "=") %>%
      filter(complete.cases(start_coordinate_x, start_coordinate_y, end_coordinate_x, end_coordinate_y))
    
    if (nrow(data) < 6) return(NULL)
    
    n_clusters <- case_when(
      nrow(data) > 80 ~ 6,
      nrow(data) > 60 ~ 4,
      nrow(data) > 45 ~ 3,
      nrow(data) > 20 ~ 2,
      TRUE ~ 1
    )
    
    coords_matrix = as.matrix(data %>% select(start_coordinate_x, start_coordinate_y, end_coordinate_x, end_coordinate_y))
    set.seed(123)
    kmeans_result = ClusterR::KMeans_rcpp(coords_matrix, clusters = n_clusters, num_init = 10, initializer = "kmeans++")
    data$cluster = as.factor(kmeans_result$clusters)
    
    cluster_means = data %>%
      group_by(cluster) %>%
      summarize(
        start_coordinate_x = mean(start_coordinate_x, na.rm = TRUE),
        start_coordinate_y = mean(start_coordinate_y, na.rm = TRUE),
        end_coordinate_x = mean(end_coordinate_x, na.rm = TRUE),
        end_coordinate_y = mean(end_coordinate_y, na.rm = TRUE),
        xSO = mean(xSO, na.rm = TRUE),
        count = n(),
        .groups = "drop"
      ) %>%
      filter(count >= 10)
    
    ggplot(data, aes(start_coordinate_x, start_coordinate_y, xend = end_coordinate_x, yend = end_coordinate_y)) +
      geom_segment(arrow = arrow(length = unit(2, "mm"), type = "closed", angle = 20), color = "gray50", alpha = 0.3) +
      ggcourt(labels = c("Serving team", "Receiving team")) +
      geom_segment(data = cluster_means,
                   aes(x = start_coordinate_x, y = start_coordinate_y,
                       xend = end_coordinate_x, yend = end_coordinate_y, color = xSO),
                   linewidth = 3, arrow = arrow(length = unit(3, "mm"), type = "closed")) +
      scale_color_gradient2(low = "limegreen", mid = "yellow", high = "firebrick", 
                            midpoint = mean(c(max(cluster_means$xSO), min(cluster_means$xSO))), name = "xSO",
                            guide = guide_colorbar(barwidth = 1.5, barheight = 10,
                                                   title.theme = element_text(size = 13),
                                                   label.theme = element_text(size = 13))) +
      labs(title = "Float") +
      theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"))
  })
  
  output$plot_topspin = renderPlot({
    data = player_data() %>% 
      filter(simple_serve_type == "Topspin Serve", evaluation_code != "=") %>%
      filter(complete.cases(start_coordinate_x, start_coordinate_y, end_coordinate_x, end_coordinate_y))
    
    if (nrow(data) < 6) return(NULL)
    
    n_clusters <- case_when(
      nrow(data) > 80 ~ 6,
      nrow(data) > 60 ~ 4,
      nrow(data) > 45 ~ 3,
      nrow(data) > 20 ~ 2,
      TRUE ~ 1
    )
    
    coords_matrix = as.matrix(data %>% select(start_coordinate_x, start_coordinate_y, end_coordinate_x, end_coordinate_y))
    set.seed(123)
    kmeans_result = ClusterR::KMeans_rcpp(coords_matrix, clusters = n_clusters, num_init = 10, initializer = "kmeans++")
    data$cluster = as.factor(kmeans_result$clusters)
    
    cluster_means = data %>%
      group_by(cluster) %>%
      summarize(
        start_coordinate_x = mean(start_coordinate_x, na.rm = TRUE),
        start_coordinate_y = mean(start_coordinate_y, na.rm = TRUE),
        end_coordinate_x = mean(end_coordinate_x, na.rm = TRUE),
        end_coordinate_y = mean(end_coordinate_y, na.rm = TRUE),
        xSO = mean(xSO, na.rm = TRUE),
        count = n(),
        .groups = "drop"
      ) %>%
      filter(count >= 10)
    
    ggplot(data, aes(start_coordinate_x, start_coordinate_y, xend = end_coordinate_x, yend = end_coordinate_y)) +
      geom_segment(arrow = arrow(length = unit(2, "mm"), type = "closed", angle = 20), color = "gray50", alpha = 0.3) +
      ggcourt(labels = c("Serving team", "Receiving team")) +
      geom_segment(data = cluster_means,
                   aes(x = start_coordinate_x, y = start_coordinate_y,
                       xend = end_coordinate_x, yend = end_coordinate_y, color = xSO),
                   linewidth = 3, arrow = arrow(length = unit(3, "mm"), type = "closed")) +
      scale_color_gradient2(low = "limegreen", mid = "yellow", high = "firebrick", 
                            midpoint = mean(c(max(cluster_means$xSO), min(cluster_means$xSO))), name = "xSO",
                            guide = guide_colorbar(barwidth = 1.5, barheight = 10,
                                                   title.theme = element_text(size = 13),
                                                   label.theme = element_text(size = 13))) +
      labs(title = "Topspin") +
      theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"))
  })
  
  
  #text below graphs - sample size
  output$float_sample_text = renderUI({
    data = player_data() %>% 
      filter(simple_serve_type == "Float Serve") %>%
      filter(complete.cases(start_coordinate_x, start_coordinate_y, end_coordinate_x, end_coordinate_y))
    
    HTML(paste0(
      "Sample size: ", nrow(data %>% filter(evaluation_code != "=")), "<br>",
      "Error rate: ", round(mean(data$evaluation_code == "=") * 100, 1), "%"
    ))
  })
  
  output$topspin_sample_text <- renderUI({
    data <- player_data() %>% 
      filter(simple_serve_type == "Topspin Serve") %>%
      filter(complete.cases(start_coordinate_x, start_coordinate_y, end_coordinate_x, end_coordinate_y))
    
    HTML(paste0(
      "Sample size: ", nrow(data %>% filter(evaluation_code != "=")), "<br>",
      "Error rate: ", round(mean(data$evaluation_code == "=") * 100, 1), "%"
    ))
  })
  
  
  ######## PLAYER VS TEAM (DECISIONS)
  
  observe({
    updateSelectInput(session, "conference", choices = c("None", sort(unique(serve_angle$conference))), selected = "SEC")
    updateSelectInput(session, "pass_conference", choices = c("None", sort(unique(serve_angle$conference))), selected = "SEC")
  })
  
  observeEvent(input$conference, {
    if (input$conference == "None") {
      teams <- sort(unique(serve_angle$team_short))
    } else {
      teams <- serve_angle %>%
        filter(conference == input$conference) %>%
        distinct(team_short) %>%
        arrange(team_short) %>%
        pull(team_short)
    }
    updateSelectInput(session, "team", choices = teams, selected = "texas")
  })
  
  observeEvent(input$team, {
    players_on_team <- serve_angle %>%
      filter(team_short == input$team) %>%
      distinct(player_name) %>%
      arrange(player_name) %>%
      pull(player_name)
    updateSelectInput(session, "player", choices = players_on_team, selected = "Emma Halter")
  })
  
  observeEvent(input$pass_conference, {
    if (input$pass_conference == "None") {
      pass_teams <- sort(unique(serve_angle$team_short))
    } else {
      pass_teams <- serve_angle %>%
        filter(conference == input$pass_conference) %>%
        distinct(team_short) %>%
        arrange(team_short) %>%
        pull(team_short)
    }
    updateSelectInput(session, "pass_team", choices = pass_teams, selected = "Kentucky")
  })
  
  serve_data <- reactive({
    serve_angle %>%
      mutate(opp_setter_position = ifelse(team == home_team, visiting_setter_position, home_setter_position))
  })
  
  output$player_plot <- renderPlot({
    req(input$player)
    
    data <- serve_data()
    
    coords <- data %>%
      filter(player_name == input$player, evaluation_code != "=") %>%
      select(start_coordinate_x, start_coordinate_y, end_coordinate_x, end_coordinate_y) %>%
      na.omit()
    
    if (nrow(coords) < 6) return(NULL)
    
    # kmeans logic
    n_clusters = case_when(
      nrow(data) > 80 ~ 6,
      nrow(data) > 60 ~ 4,
      nrow(data) > 45 ~ 3,
      nrow(data) > 20 ~ 2,
      TRUE ~ 1
    )
    
    coords_matrix <- as.matrix(coords)
    set.seed(123)
    kmeans_result <- ClusterR::KMeans_rcpp(coords_matrix, clusters = n_clusters, num_init = 10, initializer = "kmeans++")
    
    clustered <- data %>%
      filter(player_name == input$player, evaluation_code != "=") %>%
      filter(complete.cases(start_coordinate_x, start_coordinate_y, end_coordinate_x, end_coordinate_y)) %>%
      mutate(cluster = as.factor(kmeans_result$clusters))
    
    cluster_means <- clustered %>%
      group_by(cluster) %>%
      summarize(
        start_coordinate_x = mean(start_coordinate_x, na.rm = TRUE),
        start_coordinate_y = mean(start_coordinate_y, na.rm = TRUE),
        end_coordinate_x = mean(end_coordinate_x, na.rm = TRUE),
        end_coordinate_y = mean(end_coordinate_y, na.rm = TRUE),
        xSO = mean(xSO),
        count = n(),
        .groups = "drop"
      ) %>%
      filter(count >= 10)
    
    ggplot(clustered, aes(start_coordinate_x, start_coordinate_y, xend = end_coordinate_x, yend = end_coordinate_y)) +
      geom_segment(arrow = arrow(length = unit(2, "mm"), type = "closed", angle = 20), color = "gray50", alpha = 0.3) +
      ggcourt(labels = c("Serving team", "Receiving team")) +
      geom_segment(data = cluster_means,
                   aes(x = start_coordinate_x, y = start_coordinate_y,
                       xend = end_coordinate_x, yend = end_coordinate_y, color = xSO),
                   linewidth = 3, arrow = arrow(length = unit(3, "mm"), type = "closed")) +
      scale_color_gradient2(low = "limegreen", mid = "yellow", high = "firebrick", 
                            midpoint = mean(c(max(cluster_means$xSO), min(cluster_means$xSO))), name = "xSO",
                            guide = guide_colorbar(
                              barwidth = 1.5,
                              barheight = 10,
                              title.theme = element_text(size = 13),
                              label.theme = element_text(size = 13)
                            )) +
      labs(title = paste(input$player)) +
      theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"))
  })
  
  output$rotation_plot <- renderPlot({
    req(input$pass_team, input$pass_setter_position)
    
    data <- serve_data()
    
    coords <- data %>%
      filter(opponent_short == input$pass_team,
             opp_setter_position == input$pass_setter_position,
             evaluation_code != "=") %>%
      select(start_coordinate_x, start_coordinate_y, end_coordinate_x, end_coordinate_y) %>%
      na.omit()
    
    if (nrow(coords) < 6) return(NULL)
    
    # kmeans logic
    n_clusters = case_when(
      nrow(data) > 80 ~ 6,
      nrow(data) > 60 ~ 4,
      nrow(data) > 45 ~ 3,
      nrow(data) > 20 ~ 2,
      TRUE ~ 1
    )
    
    coords_matrix <- as.matrix(coords)
    set.seed(123)
    kmeans_result <- ClusterR::KMeans_rcpp(coords_matrix, clusters = n_clusters, num_init = 10, initializer = "kmeans++")
    
    clustered <- data %>%
      filter(opponent_short == input$pass_team,
             opp_setter_position == input$pass_setter_position,
             evaluation_code != "=") %>%
      filter(complete.cases(start_coordinate_x, start_coordinate_y, end_coordinate_x, end_coordinate_y)) %>%
      mutate(cluster = as.factor(kmeans_result$clusters))
    
    cluster_means <- clustered %>%
      group_by(cluster) %>%
      summarize(
        start_coordinate_x = mean(start_coordinate_x, na.rm = TRUE),
        start_coordinate_y = mean(start_coordinate_y, na.rm = TRUE),
        end_coordinate_x = mean(end_coordinate_x, na.rm = TRUE),
        end_coordinate_y = mean(end_coordinate_y, na.rm = TRUE),
        xSO = mean(xSO),
        count = n(),
        .groups = "drop"
      ) %>%
      filter(count >= 10)
    
    ggplot(clustered, aes(start_coordinate_x, start_coordinate_y, xend = end_coordinate_x, yend = end_coordinate_y)) +
      geom_segment(arrow = arrow(length = unit(2, "mm"), type = "closed", angle = 20), color = "gray50", alpha = 0.3) +
      ggcourt(labels = c("Serving team", "Receiving team")) +
      geom_segment(data = cluster_means,
                   aes(x = start_coordinate_x, y = start_coordinate_y,
                       xend = end_coordinate_x, yend = end_coordinate_y, color = xSO),
                   linewidth = 3, arrow = arrow(length = unit(3, "mm"), type = "closed")) +
      scale_color_gradient2(low = "limegreen", mid = "yellow", high = "firebrick", 
                            midpoint = mean(c(max(cluster_means$xSO), min(cluster_means$xSO))), name = "xSO",
                            guide = guide_colorbar(
                              barwidth = 1.5,
                              barheight = 10,
                              title.theme = element_text(size = 13),
                              label.theme = element_text(size = 13)
                            )) +
      labs(title = paste(input$pass_team, "Ro", input$pass_setter_position)) +
      theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"))
  })
  
  
  #text below graphs - sample size
  output$player_sample_text = renderText({
    req(input$player)
    
    data = serve_data()
    coords = data %>%
      filter(player_name == input$player, evaluation_code != "=") %>%
      select(start_coordinate_x, start_coordinate_y, end_coordinate_x, end_coordinate_y) %>%
      na.omit()
    
    paste("Sample size: ", nrow(coords))
  })
  
  output$rotation_sample_text = renderText({
    req(input$pass_team, input$pass_setter_position)
    
    data = serve_data()
    coords = data %>%
      filter(opponent_short == input$pass_team,
             opp_setter_position == input$pass_setter_position,
             evaluation_code != "=") %>%
      select(start_coordinate_x, start_coordinate_y, end_coordinate_x, end_coordinate_y) %>%
      na.omit()
    
    paste("Sample size: ", nrow(coords))
  })
  
  
}

shinyApp(ui = serve_arrows_ui, server = serve_arrows_server)






