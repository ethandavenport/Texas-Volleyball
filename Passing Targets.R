


library(tidyverse)
library(mgcv)
library(datavolley)
library(shiny)
library(scales)
library(sp)
library(alphahull)
# resource on gams: https://www.youtube.com/watch?v=sgw4cu8hrZM


epsilon <- 0.001
passes <- power %>%
  mutate(
    fb_epv = case_when(
      skill != "Reception" ~ NA_real_,
      match_id == lead(match_id, 1) & point_id == lead(point_id, 1) & lead(team, 1) == opponent ~ 1 - lead(epv_out, 1),
      match_id == lead(match_id, 2) & point_id == lead(point_id, 2) & lead(team, 2) == opponent ~ 1 - lead(epv_out, 2),
      match_id == lead(match_id, 3) & point_id == lead(point_id, 3) & lead(team, 3) == opponent ~ 1 - lead(epv_out, 3),
      team == point_won_by ~ 1,
      team != point_won_by ~ 0
    )
  ) %>%
  filter(
    skill == "Reception",
    !is.na(end_coordinate_x), !is.na(end_coordinate_y)
    #end_coordinate_y < 3.5
  ) %>%
  mutate(epv_out_trans = (fb_epv * (1 - 2 * epsilon)) + epsilon)


ui <- fluidPage(
  div(
    style = "text-align: center; font-size: 28px; font-weight: bold; margin-bottom: 20px;",
    "Passing Targets"
  ),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("level", "View by:",
                   choices = c("All" = "all", "Team" = "team"),
                   selected = "all"),
      
      # Team-level section with visual bubble
      tags$div(
        style = "border: 1px solid #ccc; border-radius: 10px; padding: 15px; margin-bottom: 20px; background-color: #f9f9f9;",
        selectInput("team", "Select Team:",
                    choices = sort(unique(passes$team_short)),
                    selected = "texas")
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
  # Reactive: Get top evaluation codes based on level and filter
  attack_options <- reactive({
    filtered <- switch(input$level,
                       "all" = passes,
                       "team" = passes %>% filter(team_short == input$team))
    
    filtered %>%
      filter(!is.na(evaluation_code)) %>%
      count(evaluation_code, sort = TRUE) %>%
      mutate(display_label = evaluation_code)
  })
  
  # Set evaluation code radio buttons
  output$attack_ui <- renderUI({
    choices_named <- c(
      "All Codes" = "all_codes",
      "# (Perfect)" = "#",
      "+ (Positive)" = "+",
      "! (OK)" = "!",
      "- (Poor)" = "-",
      "/ (Overpass)" = "/"
    )
    
    radioButtons(
      "pass", 
      "Select Evaluation Code:", 
      choices = choices_named,
      selected = "all_codes"
    )
  })
  
  # Filter dataset based on current selection
  situation_data <- reactive({
    req(input$pass)
    
    df <- passes
    if (input$pass != "all_codes") {
      df <- df %>% 
        filter(
          evaluation_code == input$pass, 
          if (input$pass != "/") end_coordinate_y < 3.5 else TRUE
        ) %>%
        group_by(end_coordinate_x, end_coordinate_y) %>%
        filter(n() > 1) %>%
        ungroup()
    }
    
    df <- switch(input$level,
                 "all" = df,
                 "team" = df %>% filter(team_short == input$team))
    
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
      gam_epv <- gam(epv_out_trans ~ s(end_coordinate_x, end_coordinate_y),
                     data = situation,
                     family = betar(), # conditional distribution
                     bs = 'ds', # type of smoother (ds for spatial data)
                     method = "REML")
      
      # Make a grid over your observed range
      grid <- expand.grid(
        end_coordinate_x = seq(min(situation$end_coordinate_x), max(situation$end_coordinate_x), length = 200),
        end_coordinate_y = seq(min(situation$end_coordinate_y), max(situation$end_coordinate_y), length = 200)
      )
      
      # Keep only points inside the convex hull of the original data
      hull_idx <- chull(situation$end_coordinate_x, situation$end_coordinate_y)
      hull_coords <- situation[hull_idx, c("end_coordinate_x", "end_coordinate_y")]
      inside <- point.in.polygon(
        grid$end_coordinate_x, grid$end_coordinate_y,
        hull_coords$end_coordinate_x, hull_coords$end_coordinate_y
      ) > 0
      grid <- grid[inside, ]
      
      # Predict only for those inside points
      grid$pred_epv <- predict(gam_epv, newdata = grid, type = "response")
      
      p <- ggplot(grid, aes(x = end_coordinate_x, y = end_coordinate_y, z = pred_epv)) +
        geom_tile(aes(fill = pred_epv)) +
        geom_contour(color = "black", binwidth = 0.02, linewidth = 0.5)
      
      if (input$pass == "/") {
        p <- p + ggcourt(court = "upper", labels = c("", ""))
      } else if (input$pass == "all_codes") {
        p <- p + ggcourt(labels = c("", ""))
      } else {
        p <- p + ggcourt(court = "lower", labels = c("", ""))
      }
      
      p <- p +
        scale_fill_gradient2(
          low = "firebrick", mid = "yellow", high = "limegreen",
          midpoint = mean(range(grid$pred_epv)), 
          name = "Predicted EPV"
        ) +
        labs(
          title = "Predicted First Ball EPV From Pass End Location",
          caption = paste("Number of observations:", prettyNum(nrow(situation), big.mark = ","))
        ) +
        theme(
          plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5, size = 16, face = "italic")
        )
      
      p
    }
  })
}

shinyApp(ui, server)




# view the 'true' distribution of EV
passes_summary <- passes %>%
  filter(skq == "Reception +") %>%
  group_by(end_coordinate_x, end_coordinate_y) %>%
  summarise(
    fb_epv_avg = mean(fb_epv, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  )

ggplot(passes_summary, aes(x = end_coordinate_x, y = end_coordinate_y, 
                           color = fb_epv_avg, size = count)) +
  geom_point() +
  ggcourt(court = "lower", labels = c("", "")) +
  scale_color_gradient2(
    low = "firebrick", mid = "yellow", high = "limegreen",
    midpoint = mean(c(max(passes_summary$fb_epv_avg), 
                      min(passes_summary$fb_epv_avg))),
    name = "Predicted EPV"
  ) +
  labs(title = "Predicted EPV From Pass End Location",
       size = "Count") +
  theme(plot.title = element_text(hjust = 0.5))


# create a gam for custom situation
situation <- passes %>%
  filter(
    #conference == "SEC",
    #team_short == "texas",
    #evaluation_code == "#",
    !is.na(match_id)
  )
gam_epv <- gam(epv_out_trans ~ s(end_coordinate_x, end_coordinate_y),
               data = situation,
               family = betar(), # conditional distribution
               #bs = 'tp', # type of smoother (ds supposedly for spatial data)
               method = "REML")

summary(gam_epv)
gam.check(gam_epv)


