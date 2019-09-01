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

