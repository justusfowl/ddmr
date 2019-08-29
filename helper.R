format_accuracy <- function(type="perc", number, ...){
  
  if (type == "perc"){
    paste0(formatC(100 * number, format = "f", digits = 2, ...), "%")
  }
  
}