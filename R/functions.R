format_pm <- function(x) {
  strNum <- format(abs(x), big.mark = ",", trim = TRUE)
  strNum <- paste0(ifelse(x < 0, "-", "+"), strNum)
}

# Conditional color for widget
# Returns 'green' on true, 'red' on false, e.g. api usage % change > 0
#                                               load time % change < 0
cond_color <- function(condition, true_color = "green") {
  if (is.na(condition)) {
    return("black")
  }
  colours <- c("green", "#e00000")
  return(ifelse(condition, true_color, colours[!colours == true_color]))
}
# FUnction which returns background colour based on cell value
# also takes column name as an input, which allows to get max and min
orange_pal <- function(x) {
  if (!is.na(x)) {
    rgb(colorRamp(c("#F7FBFF", "#317ABF"))(x), maxColorValue = 255)
  } else {
    "#e9e9e9" # grey
  }
}

# function which returns background colour based on cell value (using colour map)
# also takes column name as an input, which allows to get max and min
stylefunc <- function(value, index, name) {
  if (value >= 0 && !is.na(value)) {
    data <- crosstabs_data %>%
      mutate_if(
        is.numeric,
        funs(ifelse(. < 0, NA, .))
      )
    
    normalized <- (value - min(data %>%
                                 select(-subject_name), na.rm = T)) /
      (max(data %>%
             select(-subject_name), na.rm = T) - min(data %>%
                                                       select(-subject_name), na.rm = T))
    color <- orange_pal(normalized)
    list(color = "#000000", background = color)
  }
}
