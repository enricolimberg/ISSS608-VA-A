#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(feasts)
library(tsibble)
library(readr)

# Manually create the dataset with 365 days for each of the 27 stations
station_names <- c("Admiralty", "Ang Mo Kio", "Changi", "Choa Chu Kang (South)", "Clementi",
                   "East Coast Parkway", "Jurong (West)", "Jurong Islang", "Newton", "Pasir Panjang",
                   "Paya Lebar", "Pulau Ubin", "Seletar", "Sembawang", "Sentosa Island",
                   "Tai Seng", "Tuas South")

ts_data <- tibble(
  station = rep(station_names, each = 365),  # 27 stations, each with 365 entries
  date = rep(seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day"), length.out = 365 * 17),
  mean_temperature = rnorm(365 * 17, 28, 2),
  min_temperature = rnorm(365 * 17, 26, 2),
  max_temperature = rnorm(365 * 17, 31, 2),
  total_rainfall = rnorm(365 * 17, 5, 2)
) %>%
  as_tsibble(index = date, key = station)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Univariate Forecasting - Time Series Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("station", "Select Weather Station:", choices = unique(ts_data$station)),
      selectInput("variable", "Select Variable:", 
                  choices = c("Mean Temperature" = "mean_temperature",
                              "Minimum Temperature" = "min_temperature",
                              "Maximum Temperature" = "max_temperature",
                              "Total Rainfall (mm)" = "total_rainfall")),
      radioButtons("resolution", "Select Time Resolution:", choices = c("Day" = "day", "Week" = "week")),
      dateRangeInput("daterange", "Select Date Range:", 
                     start = min(ts_data$date), end = max(ts_data$date)),
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("ACF & PACF", plotOutput("acf_plot"), plotOutput("pacf_plot")),
        tabPanel("STL Decomposition", plotOutput("stl_plot"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Reactive dataset based on user input
  filtered_data <- reactive({
    ts_data %>%
      filter(station == input$station, 
             date >= input$daterange[1], 
             date <= input$daterange[2]) %>%
      select(date, variable = input$variable) %>%
      mutate(variable = as.numeric(variable)) %>%
      as_tsibble(index = date) %>%
      fill_gaps()  # Fill missing gaps in the time series
  })
  
  # ACF Plot
  output$acf_plot <- renderPlot({
    filtered_data() %>%
      ACF(variable) %>%
      autoplot() +
      ggtitle(paste("ACF Plot for", input$variable, "at", input$station))
  })
  
  # PACF Plot
  output$pacf_plot <- renderPlot({
    filtered_data() %>%
      PACF(variable) %>%
      autoplot() +
      ggtitle(paste("PACF Plot for", input$variable, "at", input$station))
  })
  
  # STL Decomposition Plot
  output$stl_plot <- renderPlot({
    data <- filtered_data()
    
    # Check if the dataset has sufficient data for decomposition
    if (nrow(data) < 30) {  # Make sure there's enough data for STL decomposition
      return(plotly::plot_ly(x = c(), y = c(), type = 'scatter', mode = 'text', text = "Insufficient Data for Decomposition"))
    }
    
    tryCatch({
      data %>%
        model(STL(variable ~ season(window = "periodic"))) %>%
        components() %>%
        autoplot() +
        ggtitle(paste("STL Decomposition for", input$variable, "at", input$station))
    }, error = function(e) {
      # Catch error if STL decomposition fails
      plotly::plot_ly(x = c(), y = c(), type = 'scatter', mode = 'text', text = "STL Decomposition Failed")
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
