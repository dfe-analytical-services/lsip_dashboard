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


# function which returns background colour based on cell value (using colour map)
# also takes column name as an input, which allows to get max and min

color_gradient <- function(dt, column_name, gradient_colors = c("#718ea7","#F7FBFF")) {
  col_func <- colorRampPalette(gradient_colors)
  dt %>% 
    formatStyle(column_name, 
                backgroundColor = styleEqual(
                  sort(unique(dt$x$data[[column_name]]), decreasing = TRUE),
                  col_func(length(unique(dt$x$data[[column_name]])))
                )
    ) 
}
