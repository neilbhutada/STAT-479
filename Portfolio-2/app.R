library(lubridate)
library(ggplot2)
library(tidyverse)
library(plotly)
library(shiny)
library(scales )
library(shinythemes)

data <- read_csv('https://raw.githubusercontent.com/neilbhutada/STAT-479/main/Portfolio-2/Microsoft_Stock.csv')
data <-  data %>%
  separate(Date, sep = " ", into = c("Date", "Time")) %>%
  mutate(Date = mdy(Date), 
         year = year(Date), 
         month = month(Date)) %>%
  select(-Time)

time_series <- function(data, selected)
{
  
  time_trend <-data %>%
    filter(selected)%>%
    ggplot(aes(x = Date,  Volume = Volume, Open = Open, Close = Close))+
    geom_ribbon(aes(ymin = Low, ymax = High), fill = "blue")+
    labs(x = "Date", 
         y = "Price per Stock", 
         title = "Time Series of Stock Prices (plt b)")
  
  
  
  ggplotly(time_trend,tooltip = "all", dynamicTicks = T)
}

volume <- function(data, selected)
{
  sub <- data[selected, ]
  ggplot(data, aes(Volume))+
    geom_histogram(alpha = 0.3)+
    geom_histogram(data = sub, alpha = 1)+
    scale_x_continuous(label = label_number_si(), breaks = seq(0, 200e6, 10e6))+
    labs(
      title = "Volume of Stock Sold Histogram (plt a)",
      y = "Frequency",
      x = "Volume of Stock Sold"
    )
  
}

reset <- function(data, brush){
  brushedPoints(data, brush, allRows = T)$selected_
}

ui <-
  fluidPage(
    theme = shinytheme("cerulean"),
    titlePanel("Mircrosoft Stock Trend"), 
    dateRangeInput("date", "Select Date Range", start = "2015-04-01", end = "2021-03-31", min = "2015-04-01", max ="2021-03-31" ),
    plotOutput("Volume", brush = brushOpts("brush", direction = "x",resetOnNew = T)), 
    plotlyOutput("Time")
  )

server <- function(input, output){
  
  data_2 <- reactive({
    selected(T)
    l<- data %>%filter((Date >= input$date[1])&(Date <= input$date[2]))
    l
  })
  selected <- reactiveVal(T)
  observeEvent(input$brush,
               {
                 selected(reset(data_2(), input$brush))
               }
  )
  
  output$Volume <- renderPlot({volume(data_2(),selected())})
  output$Time <- renderPlotly({time_series(data_2(), selected())})
}

shinyApp(ui, server)