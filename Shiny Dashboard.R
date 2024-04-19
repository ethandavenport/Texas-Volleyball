# Shiny Dashboard

library(shiny)
library(bslib)
library(dplyr)
library(mosaic)
library(ggplot2)
library(ggExtra)
library(ggimage)

transparent <- function(img) {
  magick::image_fx(img, expression = "0.5*a", channel = "alpha")}

# Find subset of columns that are suitable for scatter plot
df_num <- team_level %>% select(where(is.numeric),
                                -serve_count, -attack_count, -fb_count, -defense_count, -receive_count,
                                -point_count, -opp_serve_count)

ui <- page_sidebar(
  sidebar = sidebar(
    varSelectInput("xvar", "X variable", df_num, selected = "Service Error Rate"),
    varSelectInput("yvar", "Y variable", df_num, selected = "Ace Rate"),
    radioButtons("unit", "Unit of Analysis",
                 choices = list("Team" = "Team",
                                "Player" = "Player"),
                 selected = "Team"),
    hr(),
    
    radioButtons("filter", "Filter Method",
                 choices = list("All" = "All",
                                "Conference" = "Conf",
                                "Teams" = "Teams"),
                 selected = "Conf"),
    radioButtons("conference", "Filter by Conference",
                 choices = unique(ncaaLogos$conference),
                 selected = "SEC"),
    textInput("team1", label = NULL, placeholder = "Enter team 1..."),
    textInput("team2", label = NULL, placeholder = "Enter team 2..."),
    hr(),
    
    radioButtons("samplesize", "Sample Size",
                 choices = list("None" = "None",
                                "Top N" = "Top",
                                "Count" = "Count"),
                 selected = "Top"),
    sliderInput("topn", "Filter N # of Data Points", min = 0, max = 50, value = 20, step = 5),
    sliderInput("xcount", "X Sample Size", min = 0, max = 150, value = 50, step = 10),
    sliderInput("ycount", "Y Sample Size", min = 0, max = 150, value = 50, step = 10),
  ),
  plotOutput("scatter"),
)

server <- function(input, output, session) {
  subsetted <- reactive({
    if (input$unit == "Team") {filtered <- team_level}
    else {filtered <- player_level}
    
    if (input$filter == "Conf") {
      filtered <- filtered %>% filter(conference == input$conference)} 
    if (input$filter == "Teams") {
      filtered <- filtered %>% filter(grepl(input$team1, team) | grepl(input$team2, team))}
    
    xselected <- as.character(input$xvar)
    yselected <- as.character(input$yvar)
    
    if (xselected %in% cserves) {xcount <- 'serve_count'}
    else if (xselected %in% cattacks) {xcount <- 'attack_count'}
    else if (xselected %in% cfreeball) {xcount <- 'fb_count'}
    else if (xselected %in% cdefense) {xcount <- 'defense_count'}
    else if (xselected %in% creceive) {xcount <- 'receive_count'}
    else if (xselected %in% cteam) {xcount <- 'point_count'}
    else if (xselected %in% cteamso) {xcount <- 'opp_serve_count'}
    
    if (yselected %in% cserves) {ycount <- 'serve_count'}
    else if (yselected %in% cattacks) {ycount <- 'attack_count'}
    else if (yselected %in% cfreeball) {ycount <- 'fb_count'}
    else if (yselected %in% cdefense) {ycount <- 'defense_count'}
    else if (yselected %in% creceive) {ycount <- 'receive_count'}
    else if (yselected %in% cteam) {ycount <- 'point_count'}
    else if (yselected %in% cteamso) {ycount <- 'opp_serve_count'}
    
    if (input$samplesize == "Top") {
      xfiltered <- filtered %>% arrange(desc(!!sym(xcount))) %>% head(input$topn)
      yfiltered <- filtered %>% arrange(desc(!!sym(ycount))) %>% head(input$topn)
      
      if (input$unit == "Team") {
        xfiltered <- xfiltered %>% select("team","URL",input$xvar,input$yvar)
        yfiltered <- yfiltered %>% select("team","URL")
        filtered <- merge(xfiltered, yfiltered, by = c("team","URL"))}
      else {
        xfiltered <- xfiltered %>% select("team","player_name","URL",input$xvar,input$yvar)
        yfiltered <- yfiltered %>% select("team","player_name","URL")
        filtered <- merge(xfiltered, yfiltered, by = c("team","player_name","URL"))}
    }
    
    if (input$samplesize == "Count") {
      filtered <- filtered %>% filter(!!sym(xcount) >= input$xcount,
                                      !!sym(ycount) >= input$ycount)
    }
    
    return(filtered)
  })
  
  output$scatter <- renderPlot({
    filtered <- na.omit(subsetted())
    
    x_mean <- mean(filtered[[input$xvar]])
    y_mean <- mean(filtered[[input$yvar]])
    x_padding <- max(abs(range(filtered[[input$xvar]]) - x_mean))*1.05
    y_padding <- max(abs(range(filtered[[input$yvar]]) - y_mean))*1.05
    
    lm_model <- lm(filtered[[input$yvar]] ~ filtered[[input$xvar]], data = subsetted())
    rsquared <- summary(lm_model)$r.squared
    
    p <- ggplot(subsetted(), aes(!!input$xvar, !!input$yvar)) + 
      geom_point(size = 0, alpha = 0) +
      geom_image(aes(image= URL), size = .08, image_fun = transparent) +
      xlim(max(x_mean - x_mean*1.1,x_mean - x_padding), x_mean + x_padding) +
      ylim(max(y_mean - y_mean*1.1,y_mean - y_padding), y_mean + y_padding) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.position = "bottom") + 
      geom_smooth(method = "lm", se = FALSE, color = "black", fullrange = TRUE) +
      geom_text(aes(x = max(x_mean - 0.8*x_padding,x_mean - 0.8*x_mean),
                    y = max(y_mean - y_padding,y_mean - y_mean),
                    label = paste("R-Squared: ", round(rsquared, 2))), color = "black", size = 4) +
      geom_vline(xintercept = mean(filtered[[input$xvar]])) +
      geom_hline(yintercept = mean(filtered[[input$yvar]]))
    
    if (input$unit == "Player") {
      p <- p + geom_text(aes(label = player_name))}
    if (input$unit == "Team") {
      p <- p + geom_image(aes(image= URL), size = .08)}
    
    p
  }, res = 100)
}

shinyApp(ui, server)
