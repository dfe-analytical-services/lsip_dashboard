format_pm <- function(x) {
  strNum <- format(abs(x), big.mark = ",", trim = TRUE)
  strNum <- paste0(ifelse(x<0,'-','+'),strNum)
  
}

