library(shiny)
library(shinythemes)
library(dplyr)
library(tidyverse)
library(sf)
library(scico)
library(gt)
library(glue)
library(usmap)

small_vb_stats <- readRDS("../small_vb_stats.rds")



map <- us_map()

map2_data <- small_vb_stats %>% 
  group_by(state) %>%
  summarize(avg_attackAttempts = mean(attackAttempts),
            avg_attackErrors = mean(attackErrors), 
            avg_digs = mean(digs), 
            avg_ballHandlingErrors = mean(ballHandlingErrors), 
            avg_blockAssists = mean(blockAssists), 
            avg_blockSolos = mean(blockSolos),
            avg_blockingErrors = mean(blockingErrors),
            avg_hittingPercentage = mean(hittingPercentage),
            avg_kills = mean(kills), 
            avg_points = mean(points), 
            avg_receptionErrors = mean(receptionErrors), 
            avg_serviceAces = mean(serviceAces), 
            avg_serviceErrors = mean(serviceErrors))

map2_data <- map2_data %>% 
  left_join(map, by = c("state" = "full")) %>% 
  st_as_sf()

map2_data <- st_cast(map2_data, "MULTIPOLYGON")

team_counts <- small_vb_stats %>% 
  group_by(state) %>% 
  summarize(n_teams = length(unique(team)))

ui <- navbarPage(
  theme = shinytheme("flatly"),
  title = "Volleyball Stats",
  tabPanel(
    "Stats Table",
    sidebarPanel(
      selectizeInput(inputId = "team",
                     label = "Select team",
                     choices = NULL,
                     multiple = FALSE),
      selectizeInput(inputId = "player",
                     label = "Select player",
                     choices = "",
                     multiple = TRUE),
      selectizeInput(inputId = "year",
                     label = "Select year",
                     choices = "",
                     multiple = TRUE),
      radioButtons(inputId = "mean_or_all",
                   label = "Mean or All Stats",
                   choices = c("mean_stats", "all_stats"),
                   selected = "all_stats")
    ),
    mainPanel(
      gt_output("table")
    )),
  tabPanel(
    "Stats Bar Chart Player",
    sidebarPanel(
      selectizeInput(inputId = "playergraph",
                     label = "Select player",
                     choices = NULL,
                     multiple = FALSE),
      selectizeInput(inputId = "yeargraph",
                     label = "Select year",
                     choices = NULL,
                     multiple = FALSE)
    ),
    mainPanel(
      plotOutput(outputId = "barplotplayer")
    )
  ),
  tabPanel(
    "Stats Bar Chart Team",
    sidebarPanel(
      selectizeInput(inputId = "teamgraph",
                     label = "Select team",
                     choices = NULL,
                     multiple = FALSE),
      selectizeInput(inputId = "yeargraph2",
                     label = "Select year",
                     choices = NULL,
                     multiple = FALSE)
    ),
    mainPanel(
      plotOutput(outputId = "barplotteam")
    )
  ),
  tabPanel(
    "Stats Map",
    sidebarPanel(
      selectizeInput(inputId = "boxscore",
                     label = "Select Average Box Score of Interest",
                     choices = NULL,
                     multiple = FALSE)
    ),
    mainPanel(
      plotOutput(outputId = "map2")
    )
  ),
  tabPanel(
    "Popularity Map",
    mainPanel(
      plotOutput(outputId = "graph1")
    )
  ))

# Server ---------------------------------------------------------------------------
server <- function(input, output, session) {

  dat_filter <- reactive({ 
    small_vb_stats %>%
      filter(team %in% input$team) %>% 
      filter(player %in% input$player) %>%
      filter(year %in% input$year)
    
  }) 
  
  
  dat_filter_player <- reactive({ 
   player_df <- small_vb_stats %>%
      filter(team %in% input$team) 
   unique(player_df$player) 
  }) 
  
  dat_filter_year <- reactive({ 
   year_df <- small_vb_stats %>%
      filter(team %in% input$team) %>% 
      filter(player %in% input$player)
   unique(year_df$year)
  }) 
  
    updateSelectizeInput(session, 'team',
                         choices = unique(small_vb_stats$team),
                         selected = "washington-st",
                         server = TRUE)  
   
    observe({ 
      updateSelectizeInput(session, 'player',
                         choices = dat_filter_player(),
                         selected = dat_filter_player()[1], 
                         server = TRUE)
    })
    
    observe({
      updateSelectizeInput(session, 'year',
                         choices = dat_filter_year(),
                         selected = dat_filter_year()[1],
                         server = TRUE)
    })

       
    updateSelectizeInput(session, 'boxscore',
                         choices = colnames(map2_data)[-c(1, 15, 16, 17)],
                         selected = "avg_attackAttempts",
                         server = TRUE)
    
    updateSelectizeInput(session, 'teamgraph',
                         choices = unique(small_vb_stats$team),
                         selected = "washington-st",
                         server = TRUE)  
    
    updateSelectizeInput(session, 'playergraph',
                         choices = unique(small_vb_stats$player),
                         selected = "Rachel Todorovich", 
                         server = TRUE)
    
    updateSelectizeInput(session, 'yeargraph',
                         choices = unique(small_vb_stats$year),
                         selected = "2012",
                         server = TRUE)
    
    updateSelectizeInput(session, 'yeargraph2',
                         choices = unique(small_vb_stats$year),
                         selected = "2012",
                         server = TRUE)
  
  output$table <- render_gt({
 if (input$mean_or_all == "all_stats") {
   player_example <- dat_filter() 
   
   player_example_tbl <- gt(player_example)
   
   allstats_example_tbl <- player_example_tbl %>%
     tab_header(title = glue("All Stats {input$player} {input$year}")) %>%
     fmt_number(
       columns = c(attackAttempts, attackErrors, ballHandlingErrors, blockAssists, blockSolos, blockingErrors, digs, hittingPercentage, kills, points, receptionErrors, serviceAces, serviceErrors),
       decimals = 1
     )
   
   allstats_example_tbl  
 } else {
   player_example2 <- dat_filter() %>% 
     group_by(player) %>% 
     summarize(avg_attackAttempts = mean(attackAttempts),
               avg_attackErrors = mean(attackErrors), 
               avg_digs = mean(digs), 
               avg_ballHandlingErrors = mean(ballHandlingErrors), 
               avg_blockAssists = mean(blockAssists), 
               avg_blockSolos = mean(blockSolos),
               avg_blockingErrors = mean(blockingErrors),
               avg_hittingPercentage = mean(hittingPercentage),
               avg_kills = mean(kills), 
               avg_points = mean(points), 
               avg_receptionErrors = mean(receptionErrors), 
               avg_serviceAces = mean(serviceAces), 
               avg_serviceErrors = mean(serviceErrors))
   
   player_example_tbl2 <- gt(player_example2)
   
   avgstats_example_tbl <- player_example_tbl2 %>%
     tab_header(title = glue("Average Stats {input$player} {input$year}")) %>%
     fmt_number(
       columns = c(avg_attackAttempts, avg_attackErrors, avg_ballHandlingErrors, avg_blockAssists, avg_blockSolos, avg_blockingErrors, avg_digs, avg_hittingPercentage, avg_kills, avg_points, avg_receptionErrors, avg_serviceAces, avg_serviceErrors),
       decimals = 1
     )
   
   avgstats_example_tbl
 }
     
    
  }) 
  
  
 output$graph1 <- renderPlot({
 
   map_data <- team_counts %>% 
     left_join(map, by = c("state" = "full")) 
   
   ggplot() +
     geom_sf(data = map_data, mapping = aes(fill = n_teams, geometry = geom)) +
     scale_fill_scico(palette = "acton")
 })
    
 
output$map2 <- renderPlot({

  
ggplot() +
    geom_sf(data = map2_data, mapping = aes_string(fill = input$boxscore, geometry = "geom")) +
    scale_fill_scico(palette = "acton")
  

  
})

dat_filter2 <- reactive({ 
  small_vb_stats %>% 
    filter(player %in% input$playergraph) %>%
    filter(year %in% input$yeargraph) %>% 
    group_by(player) %>% 
    summarize(avg_attackAttempts = mean(attackAttempts),
              avg_attackErrors = mean(attackErrors), 
              avg_digs = mean(digs), 
              avg_ballHandlingErrors = mean(ballHandlingErrors), 
              avg_blockAssists = mean(blockAssists), 
              avg_blockSolos = mean(blockSolos),
              avg_blockingErrors = mean(blockingErrors),
              avg_hittingPercentage = mean(hittingPercentage),
              avg_kills = mean(kills), 
              avg_points = mean(points), 
              avg_receptionErrors = mean(receptionErrors), 
              avg_serviceAces = mean(serviceAces), 
              avg_serviceErrors = mean(serviceErrors)) %>% 
    pivot_longer(cols = tidyselect::starts_with("avg_"), 
                 names_to = "variable", values_to = "stat")
  
})

output$barplotplayer <- renderPlot({
  my_colors <- c("lightyellow", "yellow", "goldenrod", "orange", 
                 "darkorange", "red", "firebrick", "darkred", 
                 "maroon", "purple","blue", "darkblue", "midnightblue")
  
  ggplot(dat_filter2(), aes(x = variable, y = stat, fill = variable)) +
    geom_bar(stat = "identity", color = "black") +
    labs(title = glue("Average Stats {input$playergraph} {input$yeargraph}"),
         x = "Box scores",
         y = "Average Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(fill = FALSE) +
    scale_fill_manual(values = my_colors)  
  
})

dat_filter3 <- reactive({ 
  small_vb_stats %>% 
    filter(team %in% input$teamgraph) %>%
    filter(year %in% input$yeargraph2) %>% 
    group_by(team) %>% 
    summarize(avg_attackAttempts = mean(attackAttempts),
              avg_attackErrors = mean(attackErrors), 
              avg_digs = mean(digs), 
              avg_ballHandlingErrors = mean(ballHandlingErrors), 
              avg_blockAssists = mean(blockAssists), 
              avg_blockSolos = mean(blockSolos),
              avg_blockingErrors = mean(blockingErrors),
              avg_hittingPercentage = mean(hittingPercentage),
              avg_kills = mean(kills), 
              avg_points = mean(points), 
              avg_receptionErrors = mean(receptionErrors), 
              avg_serviceAces = mean(serviceAces), 
              avg_serviceErrors = mean(serviceErrors)) %>% 
    pivot_longer(cols = tidyselect::starts_with("avg_"), 
                 names_to = "variable", values_to = "stat")
  
})

output$barplotteam <- renderPlot({
  my_colors <- c("lightyellow", "yellow", "goldenrod", "orange", 
                 "darkorange", "red", "firebrick", "darkred", 
                 "maroon", "purple","blue", "darkblue", "midnightblue")
  
  ggplot(dat_filter3(), aes(x = variable, y = stat, fill = variable)) +
    geom_bar(stat = "identity", color = "black") +
    labs(title = glue("Average Stats {input$teamgraph} {input$yeargraph2}"),
         x = "Box scores",
         y = "Average Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(fill = FALSE) +
    scale_fill_manual(values = my_colors) 
})
   
}

# Creates app ----------------------------------------------------------------------
shinyApp(ui = ui, server = server)
