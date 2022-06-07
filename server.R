# ---------------------------------------------------------
# File name: server.R
# Date created: 06/06/2022
#
# ---------------------------------------------------------

server <- function(input, output, session) {

  # Loading screen ---------------------------------------------------------------------------
  # Call initial loading screen

  hide(id = "loading-content", anim = TRUE, animType = "fade")
  show("app-content")

  # Simple server stuff goes here ------------------------------------------------------------


  # Define server logic required to draw a histogram
  output$distPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = "darkgray", border = "white")
  })

  # Define server logic to create a box

  output$box_info <- renderValueBox({

    # Put value into box to plug into app
    shinydashboard::valueBox(
      # take input number
      input$bins,
      # add subtitle to explain what it's showing
      paste0("Number that user has inputted"),
      color = "blue"
    )
  })
  
  ### page titles ------------------
  output$page1title <- renderText({
    page1title(input$LEP)
  })
#    style = "font-size: 24px;"
 # })
  
  # KPIs

  selection_kpis <- reactive ({
    
  })
  output$emp_count <- renderUI({
    emp_count2020 <- C_BRES1520 %>%
      filter(Year == 2020,
             Area == input$LEP
      ) %>%
      mutate(Total = sum(2:40))%>%
      select(Total)
  })
  
  output$kpi_emp_count <- renderUI({
    
  })
  
  

  # Stop app ---------------------------------------------------------------------------------

  session$onSessionEnded(function() {
    stopApp()
  })
}
