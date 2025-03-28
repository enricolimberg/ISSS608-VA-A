#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Load necessary libraries
library(shiny)
library(tidyverse)
library(lubridate)
library(fable)
library(tsibble)

# Load and preprocess data
weather_data <- read.csv("data/aspatial/weather.csv") %>%
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Convert to tsibble (time-series format)
weather_ts <- weather_data %>%
  as_tsibble(index = date, key = station)

# UI layout
ui <- fluidPage(
  titlePanel("Singapore Weather Forecast"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("weather_var", "Select Weather Variable", 
                  choices = c("Mean Temperature" = "mean_temperature_c", 
                              "Max Temperature" = "maximum_temperature_c",
                              "Min Temperature" = "minimum_temperature_c",
                              "Daily Rainfall" = "daily_rainfall_total_mm",
                              "Wind Speed" = "mean_wind_speed_km_h")),
      selectInput("station", "Select Station", 
                  choices = unique(weather_data$station))
    ),
    
    mainPanel(
      plotOutput("weather_plot"),
      verbatimTextOutput("forecast_summary")
    )
  )
)

# Server logic
server <- function(input, output) {
  
  # Filter past 3 months of data
  filtered_data <- reactive({
    weather_ts %>%
      filter(station == input$station & 
               date >= max(date) - months(3))
  })
  
  # Generate forecast
  forecast_data <- reactive({
    filtered_data() %>%
      model(ARIMA(.data[[input$weather_var]] ~ PDQ(0,1,1))) %>%
      forecast(h = 7)
  })
  
  # Plot historical data + forecast
  output$weather_plot <- renderPlot({
    filtered_data() %>%
      autoplot(.data[[input$weather_var]]) +
      autolayer(forecast_data(), level = 95) +
      labs(title = paste("7-Day Forecast for", input$weather_var, "at", input$station),
           x = "Date", y = input$weather_var) +
      theme_minimal()
  })
  
  # Display forecasted values
  output$forecast_summary <- renderPrint({
    forecast_data()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
