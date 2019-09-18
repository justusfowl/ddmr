library(shinydashboard)
library(plotly)
library(dplyr)
library(stringr)
library(fs)
library(lubridate)

source("helper.R")
source("config.R")

# read dataframes and ALL resulting model objects as stored from model training as training data foundation

load("models/models.Rda")

# read metadata from model evaluation/selection, incl. the reference to highest performing ones

df_model_results <- read.csv("models.csv")

# func to predict closing prices for given timeframe

# no_days -> number of days for which the prediction should be undertaken 
predict_data <- function(ticker="", target_model, no_days=10, no_back_months=20){
  
  if (ticker == "DEFAULT"){
    ticker <- "AMZN"
    target_model <- "RF"
  }
  
  data <- load_data(ticker)
  
  now_date <- now()
  s_date <- now_date %m-% months(no_back_months)
  
  start_date <- paste(year(s_date),str_pad(month(s_date), 2, "left", "0"),str_pad(day(s_date), 2, "left", "0"), "/",sep="")
  
  data <- data[start_date]
  
  raw_data <- extend_xts(data, no_days)
  
  df_data_model <- prepare_xts(raw_data)
  
  df_data_model_pred <- predict_on_xts(in_stock_data=df_data_model, no_days=no_days, ticker=ticker, model_type=target_model)
  
  df_data_model_pred
  
}

# func to load the data foundation that existed for training 

get_data_foundation <- function(ticker=""){
  
  # !!!! dummy function / dummy data being created --> later replaced by actual data from model training !!! 
  
  out <- load_data(ticker)
  out <- convert_xts_to_df(out, ticker)
  out
  
}

# func to return meta data on ticker from nodel training
get_stored_model_results <- function(ticker=""){
  if (ticker == ""){
    return
  }
  
  res <- df_model_results[df_model_results$Ticker == ticker,]
  res
  
}

# default data 

df_pred_data <- predict_data("DEFAULT")

# ramp up the server
  
server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  get_pred_data <- reactive({ 
    ticker <- input$predictTicker
    modelVar <- input$modelTypeSelect
    dates <- input$forecastingDates
    prev_hist_months <- input$displayLastMonth
    
    # get data by ticker and load models --> execute predictions
    
    
    if (input$modelTypeSelect == "AUTO"){
      
      sel_model_results <- get_stored_model_results(input$predictTicker)
      target_model_type <- sel_model_results$ModelType
      
    }else{
      target_model_type <- input$modelTypeSelect
    }
    
    start_date <- input$forecastingDates[1]
    end_date <- input$forecastingDates[2]
    
    no_days <- as.numeric(as.Date(as.character(end_date), format="%Y-%m-%d")-as.Date(as.character(start_date), format="%Y-%m-%d")) + 1
    predict_data(ticker, target_model_type, no_days=no_days, no_back_months=prev_hist_months)
    
    })
  
  ##TAB: dashboard
  
  output$predictChartLy <- renderPlotly({
    
    df_pred_data <<- get_pred_data()
    
    # separate act/preds into two traces for formatting reasons 
    
    d_a <- df_pred_data[df_pred_data$DataType == 'actuals',]
    d_p <- df_pred_data[df_pred_data$DataType == 'prediction',]

    p <- plot_ly(d_a, x = ~Date, y = ~Close, name = 'Actuals', type = 'scatter', mode = 'lines', source = "subset") %>%
      add_trace(data = d_p, y = ~Close, name = 'Prediction', mode = 'lines', line = list(color = 'rgb(205, 12, 24)', width = 2, dash = 'dash'))
    p
    
  })
  
  output$predictTickerSelected <- renderValueBox({
    valueBox(
      paste0(input$predictTicker), "STOCK SELECTED", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$predictTickerDataAvailable <- renderValueBox({
    
    d <- get_pred_data()
    
    valueBox(
      paste0(length(d[d$DataType == 'actuals',1]), "/", length(d[d$DataType == 'prediction',1]), " DAYS"), "ACT. / PRED.", icon = icon("database"),
        color = "blue"
      )
  })
  
  output$predictTickerModelSelected <- renderValueBox({
    sel_model_results <- get_stored_model_results(input$predictTicker)
    
    if(input$modelTypeSelect == "AUTO"){
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
  
  output$outPredTable <- renderDataTable({
    
    dates <- input$forecastingDates
    prev_hist_months <- input$displayLastMonth
    
    out <- get_pred_data() %>% 
      
      mutate(
        Date = as.Date(Date),
        ClosingPrice = scales::dollar_format(negative_parens = TRUE)(Close), 
        ModelType = input$modelTypeSelect, 
        Ticker = input$predictTicker
      ) %>% 
      
      select("Date", "ClosingPrice", "DataType", "ModelType", "Ticker") %>% arrange(desc(Date))
    out
    
    
    })
  
  output$downloadData <- downloadHandler(
        filename = function() {
          paste('data-stock-pred-', Sys.Date(), '.csv', sep='')
        },
        content = function(file) {
          write.csv(get_pred_data(), file, row.names = FALSE)
        }
      )
  
  ## TAB: model evaluation
  
  output$txt_gen_approach <- renderText({"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean vitae leo in tellus imperdiet posuere. In fringilla neque faucibus velit vulputate, venenatis congue dolor gravida. Quisque posuere viverra cursus. Duis sapien metus, dapibus et tristique non, egestas eget dui. Ut et ante tortor. Aliquam erat volutpat. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Etiam felis quam, ullamcorper a rutrum id, tristique a tortor. Duis sem turpis, interdum in euismod at, ornare vel massa. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Donec viverra rhoncus aliquet. Praesent arcu turpis, volutpat nec risus in, cursus vehicula urna. Proin in tristique libero. Nam eleifend metus a fermentum ornare. Donec mauris mauris, sagittis nec leo tincidunt, pretium venenatis turpis. Morbi mattis ultricies purus, vitae scelerisque justo vulputate eu. Aliquam bibendum tellus sed lacinia dictum. Aliquam erat volutpat. Phasellus faucibus pretium nunc dictum condimentum. Duis congue mattis nisi volutpat tincidunt. Mauris tincidunt purus non lacus fermentum tempor. Sed bibendum vitae urna vitae porttitor. Suspendisse erat ipsum, viverra a tincidunt ut, venenatis eget orci. Mauris id ante eget massa iaculis varius a euismod velit. Pellentesque in felis quis odio rhoncus fermentum sed vitae turpis. Etiam et suscipit lorem, non posuere purus. Nullam semper eleifend metus ut consequat. Cras auctor mi sapien, at consequat lacus semper ut. Curabitur ornare convallis dui vitae vehicula. Sed congue quam eu consectetur accumsan. Curabitur non auctor magna."})

  output$modEvalChart <- renderPlotly({
    
    ticker <- input$modEvalTicker
    
    df_pred_data <- get_data_foundation(ticker)
    
    p <- plot_ly(df_pred_data, x = ~Date, y = ~Close, name = 'Actuals', type = 'scatter', mode = 'lines', source = "subset")
    p
    
    
  })
  
  output$txt_mod_data_summary <- renderText({
    ticker <- input$modEvalTicker
    summary(df_pred_data)
  })
  
  output$pdfviewer <- renderText({
    
    url <- paste0("EVAL_", input$modEvalTicker, ".pdf")
    return(paste('<iframe style="height:600px; width:100%" src="', url, '"></iframe>', sep = ""))
  })
  
}