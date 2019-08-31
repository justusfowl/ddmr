library(shinydashboard)
library(plotly)
library(dplyr)

source("helper.R")


load("data/APPL.Rda")

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

get_data_foundation <- function(ticket=""){
  
  actuals <- rnorm(100, mean = 5)
  x <- c(1:100)
  date <- seq(ISOdate(2018,1,1), by = "days", length.out = 100)
  
  data <- data.frame(x, date, actuals)
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
    modelhighPerforming <- input$predSelectRecommendedModel
    modelVar <- input$modelTypeSelect
    
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
    
    if(input$predSelectRecommendedModel){
      # get the one which is stored as best performing model for ticker
      sel_model_results <- get_stored_model_results(input$predictTicker)
      display <- sel_model_results$ModelType
      
    }else{
      sel_model_results <- input$modelTypeSelect
      display <- sel_model_results
    }
    
    valueBox(
      paste0(display), "MODEL", icon = icon("microchip"),
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
  
  # model evaluation
  
  output$txt_gen_approach <- renderText({"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean vitae leo in tellus imperdiet posuere. In fringilla neque faucibus velit vulputate, venenatis congue dolor gravida. Quisque posuere viverra cursus. Duis sapien metus, dapibus et tristique non, egestas eget dui. Ut et ante tortor. Aliquam erat volutpat. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Etiam felis quam, ullamcorper a rutrum id, tristique a tortor. Duis sem turpis, interdum in euismod at, ornare vel massa. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Donec viverra rhoncus aliquet. Praesent arcu turpis, volutpat nec risus in, cursus vehicula urna. Proin in tristique libero. Nam eleifend metus a fermentum ornare. Donec mauris mauris, sagittis nec leo tincidunt, pretium venenatis turpis. Morbi mattis ultricies purus, vitae scelerisque justo vulputate eu. Aliquam bibendum tellus sed lacinia dictum. Aliquam erat volutpat. Phasellus faucibus pretium nunc dictum condimentum. Duis congue mattis nisi volutpat tincidunt. Mauris tincidunt purus non lacus fermentum tempor. Sed bibendum vitae urna vitae porttitor. Suspendisse erat ipsum, viverra a tincidunt ut, venenatis eget orci. Mauris id ante eget massa iaculis varius a euismod velit. Pellentesque in felis quis odio rhoncus fermentum sed vitae turpis. Etiam et suscipit lorem, non posuere purus. Nullam semper eleifend metus ut consequat. Cras auctor mi sapien, at consequat lacus semper ut. Curabitur ornare convallis dui vitae vehicula. Sed congue quam eu consectetur accumsan. Curabitur non auctor magna."})

  output$modEvalChart <- renderPlotly({
    
    ticker <- input$modEvalTicker
    
    df_pred_data <- get_data_foundation(ticker)
    
    p <- plot_ly(df_pred_data, x = ~date, y = ~actuals, name = 'Actuals', type = 'scatter', mode = 'lines', source = "subset")
    p
    
  })
  
  output$txt_mod_data_summary <- renderText({
    ticker <- input$modEvalTicker
    summary(df_pred_data)
  })
  
  output$txt_mod_details <- renderText({
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean vitae leo in tellus imperdiet posuere. In fringilla neque faucibus velit vulputate, venenatis congue dolor gravida. Quisque posuere viverra cursus. Duis sapien metus, dapibus et tristique non, egestas eget dui. Ut et ante tortor. Aliquam erat volutpat. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Etiam felis quam, ullamcorper a rutrum id, tristique a tortor. Duis sem turpis, interdum in euismod at, ornare vel massa. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Donec viverra rhoncus aliquet. Praesent arcu turpis, volutpat nec risus in, cursus vehicula urna. Proin in tristique libero. Nam eleifend metus a fermentum ornare. Donec mauris mauris, sagittis nec leo tincidunt, pretium venenatis turpis. Morbi mattis ultricies purus, vitae scelerisque justo vulputate eu. Aliquam bibendum tellus sed lacinia dictum. Aliquam erat volutpat. Phasellus faucibus pretium nunc dictum condimentum. Duis congue mattis nisi volutpat tincidunt. Mauris tincidunt purus non lacus fermentum tempor. Sed bibendum vitae urna vitae porttitor. Suspendisse erat ipsum, viverra a tincidunt ut, venenatis eget orci. Mauris id ante eget massa iaculis varius a euismod velit. Pellentesque in felis quis odio rhoncus fermentum sed vitae turpis. Etiam et suscipit lorem, non posuere purus. Nullam semper eleifend metus ut consequat. Cras auctor mi sapien, at consequat lacus semper ut. Curabitur ornare convallis dui vitae vehicula. Sed congue quam eu consectetur accumsan. Curabitur non auctor magna."
  })
  
  output$txt_googl_mod_eval <- renderText({
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean vitae leo in tellus imperdiet posuere. In fringilla neque faucibus velit vulputate, venenatis congue dolor gravida. Quisque posuere viverra cursus. Duis sapien metus, dapibus et tristique non, egestas eget dui. Ut et ante tortor. Aliquam erat volutpat. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Etiam felis quam, ullamcorper a rutrum id, tristique a tortor. Duis sem turpis, interdum in euismod at, ornare vel massa. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Donec viverra rhoncus aliquet. Praesent arcu turpis, volutpat nec risus in, cursus vehicula urna. Proin in tristique libero. Nam eleifend metus a fermentum ornare. Donec mauris mauris, sagittis nec leo tincidunt, pretium venenatis turpis. Morbi mattis ultricies purus, vitae scelerisque justo vulputate eu. Aliquam bibendum tellus sed lacinia dictum. Aliquam erat volutpat. Phasellus faucibus pretium nunc dictum condimentum. Duis congue mattis nisi volutpat tincidunt. Mauris tincidunt purus non lacus fermentum tempor. Sed bibendum vitae urna vitae porttitor. Suspendisse erat ipsum, viverra a tincidunt ut, venenatis eget orci. Mauris id ante eget massa iaculis varius a euismod velit. Pellentesque in felis quis odio rhoncus fermentum sed vitae turpis. Etiam et suscipit lorem, non posuere purus. Nullam semper eleifend metus ut consequat. Cras auctor mi sapien, at consequat lacus semper ut. Curabitur ornare convallis dui vitae vehicula. Sed congue quam eu consectetur accumsan. Curabitur non auctor magna."
  })
  
  output$txt_appl_mod_eval <- renderText({
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean vitae leo in tellus imperdiet posuere. In fringilla neque faucibus velit vulputate, venenatis congue dolor gravida. Quisque posuere viverra cursus. Duis sapien metus, dapibus et tristique non, egestas eget dui. Ut et ante tortor. Aliquam erat volutpat. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Etiam felis quam, ullamcorper a rutrum id, tristique a tortor. Duis sem turpis, interdum in euismod at, ornare vel massa. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Donec viverra rhoncus aliquet. Praesent arcu turpis, volutpat nec risus in, cursus vehicula urna. Proin in tristique libero. Nam eleifend metus a fermentum ornare. Donec mauris mauris, sagittis nec leo tincidunt, pretium venenatis turpis. Morbi mattis ultricies purus, vitae scelerisque justo vulputate eu. Aliquam bibendum tellus sed lacinia dictum. Aliquam erat volutpat. Phasellus faucibus pretium nunc dictum condimentum. Duis congue mattis nisi volutpat tincidunt. Mauris tincidunt purus non lacus fermentum tempor. Sed bibendum vitae urna vitae porttitor. Suspendisse erat ipsum, viverra a tincidunt ut, venenatis eget orci. Mauris id ante eget massa iaculis varius a euismod velit. Pellentesque in felis quis odio rhoncus fermentum sed vitae turpis. Etiam et suscipit lorem, non posuere purus. Nullam semper eleifend metus ut consequat. Cras auctor mi sapien, at consequat lacus semper ut. Curabitur ornare convallis dui vitae vehicula. Sed congue quam eu consectetur accumsan. Curabitur non auctor magna."
  })
}