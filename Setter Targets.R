
library(tidyverse)
library(mgcv)
library(datavolley)
library(shiny)
library(scales)
# resource on gams: https://www.youtube.com/watch?v=sgw4cu8hrZM


epsilon <- 0.001
attacks <- power %>%
  filter(
    skill == "Attack",
    !is.na(start_coordinate_x), !is.na(start_coordinate_y),
    start_coordinate_y < 3.5,
    !is.na(attack_code)
  ) %>%
  mutate(epv_out_trans = (epv_out * (1 - 2 * epsilon)) + epsilon)


ui <- fluidPage(
  div(
    style = "text-align: center; font-size: 28px; font-weight: bold; margin-bottom: 20px;",
    "Setter Targets"
  ),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("level", "View by:",
                   choices = c("All" = "all", "Team" = "team", "Player" = "player"),
                   selected = "team"),
      
      # Team-level section with visual bubble
      tags$div(
        style = "border: 1px solid #ccc; border-radius: 10px; padding: 15px; margin-bottom: 20px; background-color: #f9f9f9;",
        #tags$strong("Team-Level Selection"),
        selectInput("team", "Select Team:",
                    choices = sort(unique(attacks$team_short)),
                    selected = "texas")
      ),
      
      # Player-level section with visual bubble
      tags$div(
        style = "border: 1px solid #ccc; border-radius: 10px; padding: 15px; margin-bottom: 20px; background-color: #f9f9f9;",
        #tags$strong("Player-Level Selection"),
        selectInput("player_team", "Select Team:",
                    choices = sort(unique(attacks$team_short)),
                    selected = "texas"),
        uiOutput("player_ui")
      ),
      
      # Attack code radio buttons section
      tags$div(
        style = "border: 1px solid #ccc; border-radius: 10px; padding: 15px; background-color: #f9f9f9;",
        uiOutput("attack_ui")
      )
    ),
    
    mainPanel(
      plotOutput("epv_plot")
    )
  )
)


server <- function(input, output, session) {
  # Update player dropdown based on selected player team
  output$player_ui <- renderUI({
    players <- attacks %>%
      filter(team_short == input$player_team) %>%
      pull(player_name) %>%
      unique() %>%
      sort()
    
    selectInput("player", "Select Player:", choices = players)
  })
  
  # Reactive: Get top attack codes based on level and filter
  attack_options <- reactive({
    filtered <- switch(input$level,
                       "all" = attacks,
                       "team" = attacks %>% filter(team_short == input$team),
                       "player" = attacks %>% filter(player_name == input$player))
    
    n <- switch(input$level,
                "all" = 10,
                "team" = 10,
                "player" = 5)
    
    filtered %>%
      filter(!is.na(attack_code)) %>%
      count(attack_code, attack_description, sort = TRUE) %>%
      slice_head(n = n) %>%
      mutate(display_label = paste0(attack_code, " (", attack_description, ")"))
  })
  
  # Update attack code radio buttons dynamically
  output$attack_ui <- renderUI({
    opts <- attack_options()
    radioButtons("attack", "Select Attack Code:",
                 choices = setNames(opts$attack_code, opts$display_label))
  })
  
  # Filter dataset based on current selection
  situation_data <- reactive({
    req(input$attack)
    
    df <- attacks %>% filter(attack_code == input$attack)
    
    df <- switch(input$level,
                 "all" = df,
                 "team" = df %>% filter(team_short == input$team),
                 "player" = df %>% filter(player_name == input$player))
    
    df
  })
  
  # Plot
  output$epv_plot <- renderPlot({
    situation <- situation_data()
    
    if (nrow(situation) < 10) {
      ggplot() +
        annotate("text", x = 0, y = 0, label = "Not enough data to fit model", size = 6) +
        theme_void()
    } else {
      gam_epv <- gam(epv_out_trans ~ s(start_coordinate_x, start_coordinate_y),
                     data = situation,
                     family = betar(),
                     bs = 'ds', # type of smoother (ds for spatial data),
                     method = "REML")
      
      # Make a grid over your observed range
      grid <- expand.grid(
        start_coordinate_x = seq(min(situation$start_coordinate_x), max(situation$start_coordinate_x), length = 200),
        start_coordinate_y = seq(min(situation$start_coordinate_y), max(situation$start_coordinate_y), length = 200)
      )
      
      # Keep only points inside the convex hull of the original data
      hull_idx <- chull(situation$start_coordinate_x, situation$start_coordinate_y)
      hull_coords <- situation[hull_idx, c("start_coordinate_x", "start_coordinate_y")]
      inside <- point.in.polygon(
        grid$start_coordinate_x, grid$start_coordinate_y,
        hull_coords$start_coordinate_x, hull_coords$start_coordinate_y
      ) > 0
      grid <- grid[inside, ]
      
      # Predict only for those inside points
      grid$pred_epv <- predict(gam_epv, newdata = grid, type = "response")
      
      ggplot(grid, aes(x = start_coordinate_x, y = start_coordinate_y, z = pred_epv)) +
        geom_tile(aes(fill = pred_epv)) +
        geom_contour(color = "black", binwidth = 0.03, linewidth = 0.5) +
        ggcourt(court = "lower", labels = c("", "")) +
        scale_fill_gradient2(
          low = "firebrick", mid = "yellow", high = "limegreen",
          midpoint = mean(range(grid$pred_epv)), 
          name = "Predicted EPV"
        ) +
        labs(
          title = "Predicted EPV From Attack Location",
          caption = paste("Number of observations:", prettyNum(nrow(situation), big.mark = ","))
        ) +
        theme(
          plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5, size = 16, face = "italic")
        )
    }
  })
}

shinyApp(ui, server)


power %>%
  filter(attack_code == "PP") %>%
  group_by(player_name, team_short) %>%
  summarize(n = n()) %>%
  arrange(desc(n))


