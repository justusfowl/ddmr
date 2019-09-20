if (!require("pacman")) install.packages("pacman")
pacman::p_load(tibble, quantmod, gbm, dplyr, plotly, lubridate, stringr, randomForest, e1071, fs, earth)

load("models/models.Rda")

cols_time_series_symbols_df <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
cols_time_series_symbols_xts <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")


format_accuracy <- function(type="perc", number, ...){
  
  if (type == "perc"){
    paste0(formatC(100 * number, format = "f", digits = 2, ...), "%")
  }
  
}

rename_time_xts_cols <- function(xts_o){
  colnames(xts_o) <- cols_time_series_symbols_xts
  xts_o
}

rename_time_df_cols <- function(df){
  colnames(df) <- cols_time_series_symbols_df
  df
}

convert_xts_to_df <- function(xts_obj, ticker){
  
  
  d <- as.data.frame(xts_obj)
  d <- rownames_to_column(d, var="date")
  
  d$date <- as.POSIXct(d$date)
  
  d <- rename_time_df_cols(d)
  
  d$Source <- ticker
  d$DataType <- "actuals"
  
  d
  
}



# prediction functions

load_data <- function(ticker){
  data <- getSymbols(ticker, auto.assign = FALSE)
  
  names(data) <- c("Open","High","Low","Close","Volume","Adjusted")
  
  data
  
}



## INTERFACE Function that shall be used / sourced only from here for training/prediction to ensure compatibility

prepare_xts <- function(forecast_stock_data){
  
  data_model <- forecast_stock_data$Close
  
  data_model$Close.Lag7 <- lag(forecast_stock_data$Close,7)
  data_model$Open.Lag7 <- lag(forecast_stock_data$Open,7) 
  data_model$High.Lag7 <- lag(forecast_stock_data$High,7) 
  data_model$Low.Lag7<- lag(forecast_stock_data$Low,7) 
  data_model$Volume.Lag7<- lag(forecast_stock_data$Volume,7) 
  data_model$Adjusted.Lag7<- lag(forecast_stock_data$Adjusted,7) 
  data_model$Close.Lag14 <- lag(forecast_stock_data$Close,14) 
  data_model$Open.Lag14 <- lag(forecast_stock_data$Open,14) 
  data_model$High.Lag14 <- lag(forecast_stock_data$High,14) 
  data_model$Low.Lag14<- lag(forecast_stock_data$Low,14) 
  data_model$Volume.Lag14<- lag(forecast_stock_data$Volume,14) 
  data_model$Adjusted.Lag14<- lag(forecast_stock_data$Adjusted,14) 
  data_model$Close.Lag21    <- lag(forecast_stock_data$Close,21) 
  data_model$Open.Lag21     <- lag(forecast_stock_data$Open,21) 
  data_model$High.Lag21     <- lag(forecast_stock_data$High,21) 
  data_model$Low.Lag21      <- lag(forecast_stock_data$Low,21) 
  data_model$Volume.Lag21   <- lag(forecast_stock_data$Volume,21) 
  data_model$Adjusted.Lag21 <- lag(forecast_stock_data$Adjusted,21) 
  data_model$Close.Lag28 <- lag(forecast_stock_data$Close,28) 
  data_model$Open.Lag28 <- lag(forecast_stock_data$Open,28) 
  data_model$High.Lag28 <- lag(forecast_stock_data$High,28) 
  data_model$Low.Lag28 <- lag(forecast_stock_data$Low,28) 
  data_model$Volume.Lag28 <- lag(forecast_stock_data$Volume,28) 
  data_model$Adjusted.Lag28 <- lag(forecast_stock_data$Adjusted,28)
  
  #Create dataframe
  df_data_model <- fortify.zoo(data_model)
  
  names(df_data_model)[1] <- "Date"
  df_data_model$Date <- as.POSIXct(df_data_model$Date)
  
  
  i <- seq(from=2, to=26) 
  
  df_data_model[ , i] <- apply(df_data_model[ , i], 2,            # Specify own function within apply
                               function(x) as.numeric(as.character(x)))
  
  df_data_model$DataType <- as.vector(forecast_stock_data$DataType)
  
  df_data_model[is.na(df_data_model)] <- 0
  
  df_data_model
  
}


extend_xts <- function(stock_data, no_days=NA){
  
  if (is.na(no_days) | no_days < 0 ){
    no_days <- 5
  }
  
  v_dates_end <- seq(as.Date(end(stock_data)),
                     by = "day",
                     length.out = (no_days+1))[-1]
  
  x_expand_time_index <- xts(, order.by = c(v_dates_end))
  
  x <- merge(stock_data, x_expand_time_index, fill = NA)
  x$DataType = NA
  x$DataType = "actuals"
  x[is.na(x$Close),]$DataType = "prediction"
  
  x
  
}

predict_on_xts <- function(in_stock_data, no_days, ticker, model_type){
  
  pred_data <- tail(in_stock_data, no_days)
  
  print(paste0("Predict for : ", ticker, ", with model: ", model_type))
  
  if (ticker == "AMZN"){
    if (model_type == "LM"){
      data_predict <- predict(Amazon_LM, pred_data)
    }else if (model_type == "MARS"){
      data_predict <- predict(Amazon_MARS, pred_data)
    }else if (model_type == "SVM"){
      data_predict <- predict(Amazon_SVM, pred_data)
    }else{
      stop("Unknown model type")
    }
  }else if (ticker == "BA"){
    if (model_type == "LM"){
      data_predict <- predict(Boeing_LM, pred_data)
    }else if (model_type == "MARS"){
      data_predict <- predict(Boeing_MARS, pred_data)
    }else if (model_type == "SVM"){
      data_predict <- predict(Boeing_SVM, pred_data)
    }else{
      stop("Unknown model type")
    }
  }else if (ticker == "GOOG"){
    if (model_type == "LM"){
      data_predict <- predict(Google_LM, pred_data)
    }else if (model_type == "MARS"){
      data_predict <- predict(Google_MARS, pred_data)
    }else if (model_type == "SVM"){
      data_predict <- predict(Google_SVM, pred_data)
    }else{
      stop("Unknown model type")
    }
  }else if (ticker == "MSFT"){
    if (model_type == "LM"){
      data_predict <- predict(Microsoft_LM, pred_data)
    }else if (model_type == "MARS"){
      data_predict <- predict(Microsoft_MARS, pred_data)
    }else if (model_type == "SVM"){
      data_predict <- predict(Microsoft_SVM, pred_data)
    }else{
      stop("Unknown model type")
    }
  }else if (ticker == "AAPL"){
    if (model_type == "LM"){
      data_predict <- predict(Apple_LM, pred_data)
    }else if (model_type == "MARS"){
      data_predict <- predict(Apple_MARS, pred_data)
    }else if (model_type == "SVM"){
      data_predict <- predict(Apple_SVM, pred_data)
    }else{
      stop("Unknown model type")
    }
  }else{
    stop("Unknown company ticker")
  }
  
  
  in_stock_data[in_stock_data$DataType == 'prediction',]$Close = data_predict
  
  in_stock_data
}


