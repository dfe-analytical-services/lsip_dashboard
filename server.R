# ---------------------------------------------------------
# This is the server file.
# Use it to create interactive elements like tables, charts and text for your app.
#
# Anything you create in the server file won't appear in your app until you call it in the UI file.
# This server script gives an example of a plot and value box that updates on slider input.
# There are many other elements you can add in too, and you can play around with their reactivity.
# The "outputs" section of the shiny cheatsheet has a few examples of render calls you can use:
# https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
#
#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# ---------------------------------------------------------


server <- function(input, output, session) {
  
  # Loading screen ---------------------------------------------------------------------------
  # Call initial loading screen
  
  hide(id = "loading-content", anim = TRUE, animType = "fade")
  show("app-content")
  
  # Simple server stuff goes here ------------------------------------------------------------
  # Download data -----------------------------------------------------------
  
  # to_download_pg1 <- reactiveValues(
  #   subsectors_table = subsectors_table,
  #   highest_qualification_table = highest_qualification_table,
  #   qualifications_titles_table = qualifications_titles_table,
  #   subjects_table = subjects_table,
  #   income_proportions_table = income_proportions_table,
  #   working_futures_table = working_futures_table
  #   # qualifications_pathways_table = qualifications_pathways_table,
  #   # progression_to_work_by_level_table = progression_to_work_by_level_table
  # )
  # 
  # 
  # output$download_btn1 <- downloadHandler(
  #   filename = function() {
  #     paste("data_", Sys.Date(), ".zip", sep = "")
  #   },
  #   content = function(file) {
  #     temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
  #     dir.create(temp_directory)
  #     
  #     
  #     reactiveValuesToList(to_download_pg1) %>%
  #       imap(function(x, y) {
  #         if (!is.null(x)) {
  #           file_name <- glue("{y}_data.csv")
  #           write.csv(x, file.path(temp_directory, file_name), row.names = F)
  #         }
  #       })
  #     
  #     zip::zip(
  #       zipfile = file,
  #       files = dir(temp_directory),
  #       root = temp_directory
  #     )
  #   },
  #   contentType = "application/zip"
  # )
  
  # Define server logic required to draw a histogram
  # output$distPlot <- renderPlot({
  #   
  #   # generate bins based on input$bins from ui.R
  #   x <- faithful[, 2]
  #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
  #   
  #   # draw the histogram with the specified number of bins
  #   hist(x, breaks = bins, col = "darkgray", border = "white")
  # })
  
# Define page titles ----
  
  output$page1title <- renderUI({
    paste0(input$lep1, ": Overview of Local Landscape")
  })
  
  output$page2title <- renderUI({
    paste0(input$lep3, ": Overview of Skill Supply")
  })
  
  output$page3title <- renderUI({
    paste0(input$lep5, ": Overview of Skill Supply")
  })
  

# LOCAL LANDSCAPE ----

 ## KPIs ----

  ### Employment rate -----
  output$locland.emplrate <- renderValueBox({
    # Put value into box to plug into app
    valueBox(
      # take input number
      paste(
        format(100.*(C_EmpRate_APS1721 %>%
                       filter(area==input$lep1,year=="2021")
        )$empRate,digits=3),
        "%"),
      # add subtitle to explain what it's showing
      paste0("Employment rate in ",input$lep1),
      color="blue"
    )
  })

  ### Employment count ----
  output$locland.emplcnt <- renderValueBox({
    # Put value into box to plug into app
    valueBox(
      # take input number
      format((C_EmpOcc_APS1721 %>%
                filter(area==input$lep1,year=="Jan 2021-Dec 2021")
      )$t09a_1_all_people_corporate_managers_and_directors_soc2010_all_people,
      scientific=FALSE),
      # add subtitle to explain what it's showing
      paste0("In employment in ",input$lep1),
      color="blue"
    )
  })

  ## Employment rate over time line graph ----

  EmpRate_time <- reactive({
    C_EmpRate_APS1721 %>%
      select(year, area, empRate)%>%
      filter(area == "England" |
               area == input$lep1 |
               area == input$lep2) %>%
      ggplot(aes(x=year, y=empRate, group = area, colour = area))+
      geom_line()+
      theme_minimal()+
      expand_limits(y = 0)+
      labs(colour = "Area")+
      # theme(legend.position="bottom")+
      ggtitle("Employment Rate \n 2017-2021")+
      xlab("Year")+
      ylab("Employment Rate")+
      scale_y_continuous(labels = scales::percent_format(accuracy=1))
  })

  output$EmpRate_time <- renderPlotly({
    ggplotly(EmpRate_time())
  })

  ## Employment by occupation data table ----

  EmpOcc <- reactive({
    C_EmpOcc_APS1721 %>%
      filter(year == "Jan 2021-Dec 2021") %>%
      filter(area == "England" |
               area == input$lep1 |
               area == input$lep2)%>%
      select(-year)%>%
      t()%>%
      row_to_names(row_number=1)
  })

  output$EmpOcc <- renderDataTable({
    EmpOcc()
    })

# SKILL SUPPLY ----
  ## KPIs ----
  ### FE achievements -----
  output$skisup.FEach <- renderValueBox({
    # Put value into box to plug into app
    valueBox(
      format("XX,XXX", scientific=FALSE),
      paste0("FE Achievements in ", input$lep3, " in Year"),
      color="blue"
    )
  })

  ### Apprentichesip achievements ----
  output$skisup.APach <- renderValueBox({
    # Put value into box to plug into app
      valueBox(
        format("XX,XXX", scientific=FALSE),
        paste0("Apprenticeship achievements in ", input$lep3," in Year"),
        color="blue"
      )
  })

# SKILL DEMAND ----
  ## KPIs ----
   ### ONS job advert unit counts ----



# Stop app ---------------------------------------------------------------------------------
  
  session$onSessionEnded(function() {
    stopApp()
  })
}
