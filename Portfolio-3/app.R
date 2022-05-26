library(tidyverse)
library(dplyr)
library(ggplot2)
library(sf)
library(spData)
library(plotly)
library(stringr)
library(shiny)
library(shinythemes)
rm(list = ls())

happiness <- read_csv("https://raw.githubusercontent.com/neilbhutada/STAT-479/main/Portfolio-3/2022.csv")

world <- world %>%
  rename(Country = name_long)

happiness <- happiness%>%
  mutate(Country = str_remove(Country, "\\*"), 
         `Happiness score` = `Happiness score`/1000, 
         `Explained by: GDP per capita` = as.numeric(str_replace(`Explained by: GDP per capita`, ",", '\\.')), 
         `Explained by: Social support` = as.numeric(str_replace(`Explained by: Social support`, ",", '\\.')), 
         `Explained by: Healthy life expectancy` = as.numeric(str_replace(`Explained by: Healthy life expectancy`, ",", '\\.')), 
         `Explained by: Freedom to make life choices` = as.numeric(str_replace(`Explained by: Freedom to make life choices`, ",", '\\.')), 
         `Explained by: Generosity` = as.numeric(str_replace(`Explained by: Generosity`, ",", '\\.')), 
         `Explained by: Perceptions of corruption` = as.numeric(str_replace(`Explained by: Perceptions of corruption`, ",", '\\.'))
  ) %>%
  select(-starts_with("Whisker")) %>%
  select( -starts_with("Dystopia")) %>%
  mutate(Country= case_when(
    Country == "Czechia" ~ "Czech Republic",
    Country == "eSwatini" ~ "Eswatini, Kingdom of",
    Country == "Papua New Guinea" ~ "Guinea",
    Country == "North Cyprus" ~ "Northern Cyprus", 
    Country == "North Macedonia" ~ "Macedonia",
    Country == "Russia" ~ "Russian Federation",
    Country == "South Korea" ~ "Republic of Korea", 
    Country == "Congo" ~ "Republic of the Congo", 
    Country == "Gambia" ~ "The Gambia", 
    T ~ Country)
  )

world_with_hap <- world %>%
  left_join(happiness, by = "Country")


reset <- function(data, brush){
  brushedPoints(data, brush, allRows = T)$selected_
}

world_happiness <- function(data, selected)
{
  
  data_selected <- data %>%
    mutate(selected_ = selected)
  data_selected <- data_selected %>%
    mutate(selected_ = case_when(
      selected_ == F ~ 0.2, 
      selected_ == T ~ 1
    ))
  
  p1 <-  ggplot(data_selected, aes(label = Country, fill = `Happiness score`, alpha = selected_))+
    scale_alpha(range = c(min(data_selected$selected_), max(data_selected$selected_)), guide = "none")+
    geom_sf()+
    scale_fill_viridis_b()+
    labs(title = "Happiness Scores Map (fig. b)")+
    theme_void()
  
  ggplotly(p1, tooltip = c("label", "fill")) 
  
}

hist_happiness <- function(data , selected)
{
  sub  <- data[selected, ]
  ggplot(data, aes(x = `Happiness score`, fill = ..x..))+
    geom_histogram(alpha = 0.3)+
    geom_histogram(data = sub, alpha = 1)+
    scale_fill_viridis_b()+
    labs(y = "Frequency", 
         x = "Happiness Score", 
         title = "Happiness Score Histogram (fig. a)")+
    theme_bw()+
    scale_x_continuous(breaks = seq(0, 9, by = 1))+
    scale_y_continuous(breaks = seq(0, 14, by = 2))+
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.border = element_blank(), 
      title = element_text(size=15),
      legend.position = "none"
    )
}

ui <- fluidPage(theme = shinytheme("cerulean"),
  
  titlePanel("World Happines Report 2022"), 
  fluidRow(
    # splitLayout(cellWidths = c("30%", "70%"),
  column(3, h3("Brush over histogram"), plotOutput("hist", brush = brushOpts("brush", direction = "x", resetOnNew = T))),
    column(9, h3("Click on graph to activate tool-tip"),plotlyOutput("map"))
  ),
  h3("Happiness scores breakdown (fig. c)"),
  dataTableOutput("table")
)




server <- function(input, output){
  
  selected <- reactiveVal(T)
  observeEvent(input$brush, 
               {
                 selected(reset(world_with_hap,input$brush ))
               }
  )
  output$hist <- renderPlot({
    hist_happiness(world_with_hap, selected())
  },width = )
  
  output$map <- renderPlotly({
    world_happiness(world_with_hap, selected())%>%layout(
      font = list(color = '#a2a2a2'),
      yaxis = list(fixedrange = FALSE,title="",
                   showgrid = FALSE, showline = FALSE, showticklabels = TRUE),
      xaxis = list(fixedrange = TRUE,title="",zeroline = FALSE, showline = FALSE, showticklabels = FALSE, showgrid = FALSE))
    
  })
  
  output$table <- renderDataTable({
    
    countries <- world_with_hap %>% filter(selected()) %>% pull(Country)
    happiness %>% filter(Country %in% countries)
  })
  
  
}

shinyApp(ui, server)