library(shinydashboard)
library(plotly)
library(dplyr)

source("helper.R")

# read input data from model evaluation

df_model_results <- read.csv("models.csv")

get_data <- function(ticker=""){
  
  # dummy function / dummy data being created --> later replaced by actual data / prediction results
  
  actuals <- rnorm(100, mean = 5)
  prediction <- rnorm(100, mean = 5.5)
  x <- c(1:100)
  date <- seq(ISOdate(2018,1,1), by = "days", length.out = 100)
  
  data <- data.frame(x, date, actuals, prediction)
  
  data[data$x>79,]$actuals = NA
  data[data$x<80,]$prediction = NA
  
  data[data$x==79,]$prediction = data[data$x==79,]$actuals
  
  data
  
}

get_stored_model_results <- function(ticker=""){
  if (ticker == ""){
    return
  }
  
  res <- df_model_results[df_model_results$Ticker == ticker,]
  res
  
}

# default data 

df_pred_data <- get_data()

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$predictChart <- renderPlotly({
    
    ticker <- input$predictTicker
    
    # get data by ticker and load models --> execute predictions
    
    df_pred_data <- get_data(ticker)

    p <- plot_ly(df_pred_data, x = ~date, y = ~actuals, name = 'Actuals', type = 'scatter', mode = 'lines', source = "subset") %>%
      add_trace(y = ~prediction, name = 'Prediction', mode = 'lines+markers', line = list(color = 'rgb(205, 12, 24)', width = 2, dash = 'dash'))
    p
    
  })
  
  output$predictTickerSelected <- renderValueBox({
    valueBox(
      paste0(input$predictTicker), "STOCK SELECTED", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$predictTickerDataAvailable <- renderValueBox({
    valueBox(
      
      paste0(length(df_pred_data[,1]), " DAYS"), "DATA", icon = icon("database"),
      color = "blue"
    )
  })
  
  output$predictTickerModelSelected <- renderValueBox({
    sel_model_results <- get_stored_model_results(input$predictTicker)
    valueBox(
      paste0(sel_model_results$ModelType), "MODEL", icon = icon("microchip"),
      color = "blue"
    )
  })
  
  output$predictTickerModel <- renderValueBox({
    sel_model_results <- get_stored_model_results(input$predictTicker)
    valueBox(
      paste0(format_accuracy(type=sel_model_results$ModelAccuracyMetricFormat, sel_model_results$ModelAccuracy)), sel_model_results$ModelAccuracyMetric, icon = icon("balance-scale"),
      color = "blue"
    )
  })
  
  output$outPredTable <- renderDataTable(df_pred_data)

  
}