server <- function(input, output, session) {

  # Loading screen ---------------------------------------------------------------------------
  # Call initial loading screen

  hide(id = "loading-content", anim = TRUE, animType = "fade")
  show("app-content")

  # HOMEPAGE ---- 
  # Create link to overview tab
  observeEvent(input$link_to_tabpanel_overview, {
    updateTabsetPanel(session, "navbar", "Dashboard") # Get into app
    updateTabsetPanel(session, "datatabset", "Overview") # then pick tab
  })
  # Create link to employment data tab
  observeEvent(input$link_to_tabpanel_employment, {
    updateTabsetPanel(session, "navbar", "Dashboard")
    updateTabsetPanel(session, "datatabset", "Employment")
  })
  # Create link to vacancy data tab
  observeEvent(input$link_to_tabpanel_vacancies, {
    updateTabsetPanel(session, "navbar", "Dashboard")
    updateTabsetPanel(session, "datatabset", "Vacancies")
  })
  # Create link to skills data tab
  observeEvent(input$link_to_tabpanel_FE, {
    updateTabsetPanel(session, "navbar", "Dashboard")
    updateTabsetPanel(session, "datatabset", "Skills")
  })

  # OVERVIEW ----
  
  # turn off lep 2 for overview page (as not used here)
  output$lep2_off <- renderUI({
    if (input$datatabset == "Overview") {
      p("")
    } else {
      selectInput("lep2", "Choose comparison LEP area",
                  choices = c("\nNone", unique(C_LEP2020) %>% filter(LEP != input$lep1)), # filter out lep1
                  selected = input$lep2 # select this so lep two is consistent across tabs
      )
    }
  })

  #define page title
  output$page0title <- renderUI({
    paste0(input$lep1, ": overview of local landscape")
  })
  
  ### Downloads----
  # download all indicators
  list_of_datasets0 <- list(
    "1a.Emp by occupation" = D_EmpOcc_APS1721,
    "1b.Emp rate" = D_EmpRate_APS1721,
    "2.Vacancies" = C_Vacancy_ONS1722,
    "3a.FE achievements SSA" = D_Achieve_ILR21,
    "3b.FE achievements" = D_Achieve_ILR1621
  )
  output$download_btn0a <- downloadHandler(
    filename = function() {
      "CoreIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list_of_datasets0, path = file)
    }
  )

  # Download current LEP indicators
  filtered_data0 <- reactive({
    list(
      "1a.Emp by occupation" = filter(D_EmpOcc_APS1721, geographic_level == "lep", area == input$lep1),
      "1b.Emp rate" = filter(D_EmpRate_APS1721, geographic_level == "lep", area == input$lep1),
      "2.Vacancies" = filter(C_Vacancy_ONS1722, LEP == input$lep1),
      "3a.FE achievements SSA" = filter(D_Achieve_ILR21, LEP == input$lep1),
      "3b.FE achievements" = filter(D_Achieve_ILR1621, LEP == input$lep1)
    )
  })
  output$download_btn0b <- downloadHandler(
    filename = function() {
      "CurrentIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(filtered_data0(), path = file)
    }
  )

  ## KPIs and charts----

  #get emp data for current lep
  empLEP<-reactive({
    C_EmpRate_APS1721 %>%
      filter(area == input$lep1)
  })
  #get 2021 values
  emp2021<-reactive({
    empLEP() %>%
      filter(year == "2021")
  })
  #get 2020 values
  emp2020<-reactive({
    empLEP() %>%
      filter(year == "2020")
  })
  
  #### Employment count ----
  output$locland.emplcnt0 <- renderUI({
    #call 2020 and 2021 values for chosen LEP
    empCnt2021<-emp2021()$Employment
    empCntChange<-emp2021()$Employment-emp2020()$Employment

    # print with formatting
    h4(span("2021", style = "font-size: 16px;font-weight:normal;"), br(),
      format(empCnt2021, big.mark = ","), br(),
      span(
        format_pm(empCntChange) # plus-minus and comma sep formatting
        ,
        style = paste0("font-size: 16px;color:", cond_color(empCntChange > 0)) # colour formating

        , .noWS = c("before", "after") # remove whitespace
      ), br(),
      style = "font-size: 21px"
    )
  })

  # Emp chart
  empLineChart <- reactive({
    #call 2020 to 2021 change  for chosen LEP
    empCntChange<-emp2021()$Employment-emp2020()$Employment
    empLine <- empLEP()
    
#find min and max for lep
    empCntMinMax <- C_EmpRate_APS1721_max_min %>% 
      filter(area == input$lep1)

    ggplot(empLine, aes(x = Year, y = Employment, group = area, text = paste0(
      "Year: ", year, "<br>",
      "Employment: ", format(Employment, big.mark = ","), "<br>"
    ))) +
      geom_line(data = empLine %>% filter(Year <= 20)) +
      geom_ribbon(
        data = empLine %>% filter(Year >= 20),
        aes(ymin = min(Employment), ymax = Employment),
        fill = ifelse(empCntChange > 0, "#00703c", "#d4351c"),
        alpha = 0.3
      ) +
      geom_line(
        data = empLine %>% filter(Year >= 20),
        color = ifelse(empCntChange > 0, "#00703c", "#d4351c")
      ) +
      theme_classic() +
      theme(
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "#f3f2f1"),
        plot.background = element_rect(fill = "#f3f2f1")
      ) +
      scale_y_continuous(
        labels = label_number_si(accuracy = 1),
        breaks = c(empCntMinMax$minEmp, empCntMinMax$maxEmp)
      )
  })
  # set margins
  m <- list(
    l = 0,
    r = 4, # increase this margin a bit to prevent the last lable dissapearing
    b = 0,
    t = 0,
    pad = 0
  )

  output$empLineChart <- renderPlotly({
    ggplotly(empLineChart(),
      tooltip = "text",
      height = 81
    ) %>%
      layout(
        margin = m,
        xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })

  #### Employment rate -----
  output$locland.emplrate0 <- renderUI({
    #call 2021 values and 20-21 change for chosen LEP
    empRate2021<-emp2021()$empRate
    empRateChange<-emp2021()$empRate-emp2020()$empRate
    
        # print with formatting
    h4(span("2021", style = "font-size: 16px;font-weight:normal;"), br(),
      paste0(format(100 * empRate2021, digit = 2), "%"), br(),
      span(
        paste0(sprintf("%+.0f", 100 * empRateChange), "ppts"),
        style = paste0("font-size: 16px;color:", cond_color(empRateChange > 0)) # colour formating
        , .noWS = c("before", "after") # remove whitespace
      ), br(),
      style = "font-size: 21px"
    )
  })
  
  # Emp chart
  
  # find emp chart y axis min and max
  EmpRateMin <- C_EmpRate_APS1721 %>%
    summarise(min(empRate, na.rm = T), .groups = "drop")
  EmpRateMax <- C_EmpRate_APS1721 %>%
    summarise(max(empRate, na.rm = T), .groups = "drop")
  
  empRateLineChart <- reactive({
    empRateChange<-emp2021()$empRate-emp2020()$empRate
    empRateLine <-  C_EmpRate_APS1721 %>%
      filter(area == input$lep1 | area=="England")
    
    ggplot(empRateLine, aes(
      x = Year, y = empRate,
      group = area,
      text = paste0(
        "Year: ", year, "<br>",
        "Area: ", area, "<br>",
        "Employment rate: ", format(100 * empRate, digit = 2), "%<br>"
      )
    )) +
      geom_line(data = empRateLine %>% filter(Year <= 20, geographic_level == "lep")) +
      geom_line(data = empRateLine %>% filter(geographic_level == "country"), alpha = 0.5) +
      geom_ribbon(
        data = empRateLine %>% filter(Year >= 20, geographic_level == "lep"),
        aes(ymin = min(empRate), ymax = empRate),
        fill = ifelse(empRateChange > 0, "#00703c", "#d4351c"),
        alpha = 0.3
      ) +
      geom_line(
        data = empRateLine %>% filter(Year >= 20, geographic_level == "lep"),
        color = ifelse(empRateChange > 0, "#00703c", "#d4351c")
      ) +
      theme_classic() +
      theme(
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "#f3f2f1"),
        plot.background = element_rect(fill = "#f3f2f1")
      ) +
      scale_y_continuous(
        labels = scales::percent_format(accuracy = 1),
        breaks = c(EmpRateMin[1, 1], EmpRateMax[1, 1]),
        limits = c(EmpRateMin[1, 1], EmpRateMax[1, 1])
      )
  })
  # set margins
  m <- list(
    l = 0,
    r = 4, # increase this margin a bit to prevent the last lable dissapearing
    b = 0,
    t = 0,
    pad = 0
  )

  output$empRateLineChart <- renderPlotly({
    ggplotly(empRateLineChart(),
      tooltip = "text",
      height = 81
    ) %>%
      layout(
        margin = m,
        xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })

  # Add link to employment data
  observeEvent(input$link_to_tabpanel_employment2, {
    updateTabsetPanel(session, "navbar", "Dashboard")
    updateTabsetPanel(session, "datatabset", "Employment")
  })

  #### ONS job advert units  ----
  output$jobad.units <- renderUI({
    VacPc <- C_Vacancy_England %>%
      filter(
        year == "2022",
        LEP == input$lep1
      ) %>%
      summarise(job.pc = sum(pc_total), .groups = "drop")

    ### ONS job advert units change
    VacPcChange <- (C_Vacancy_England %>%
      filter(
        year == "2022",
        LEP == input$lep1
      ) %>%
      summarise(job.pc = sum(pc_total), .groups = "drop")
      - (C_Vacancy_England %>%
        filter(
          year == "2021",
          LEP == input$lep1
        ) %>%
        summarise(job.pc = sum(pc_total), .groups = "drop")))

    # print with formatting

    h4(span("2022", style = "font-size: 16px;font-weight:normal;"), br(),
      paste0(format(100 * VacPc, digit = 2), "%"), br(),
      span(
        paste0(sprintf("%+.1f", 100 * VacPcChange), "ppts"),
        style = paste0("font-size: 16px;color:", cond_color(VacPcChange > 0)) # colour formating
        , .noWS = c("before", "after") # remove whitespace
      ), br(),
      style = "font-size: 21px"
    )
  })

  # Vacancy chart
  VacLineChart <- reactive({
    VacLine <- C_Vacancy_England %>%
      filter(
        LEP == input$lep1
      ) %>%
      group_by(year) %>%
      summarise(job.pc = sum(pc_total), .groups = "drop") %>%
      mutate(Year = as.numeric(substr(year, 3, 4)), LEP = input$lep1)

    VacChange <- (VacLine %>% filter(Year == 22))$job.pc -
      (VacLine %>% filter(Year == 21))$job.pc
    VacMin <- VacLine %>% summarise(min = min(job.pc), .groups = "drop")
    VacMax <- VacLine %>% summarise(max = max(job.pc), .groups = "drop")

    ggplot(VacLine, aes(x = Year, y = job.pc, group = LEP, text = paste0(
      "Year: ", year, "<br>",
      "England vacancy share: ", format(100 * job.pc, digit = 2), "%<br>"
    ))) +
      geom_line(data = VacLine %>% filter(Year <= 21)) +
      geom_ribbon(
        data = VacLine %>% filter(Year >= 21),
        aes(ymin = min(job.pc), ymax = job.pc),
        fill = ifelse(VacChange > 0, "#00703c", "#d4351c"),
        alpha = 0.3
      ) +
      geom_line(
        data = VacLine %>% filter(Year >= 21),
        color = ifelse(VacChange > 0, "#00703c", "#d4351c")
      ) +
      # add a blank line for the formatted tooltip
      theme_classic() +
      theme(
        axis.line = element_blank(),
        # axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "#f3f2f1"),
        plot.background = element_rect(fill = "#f3f2f1")
      ) +
      scale_y_continuous(
        labels = scales::percent_format(accuracy = 0.1),
        breaks = c(VacMin$min, VacMax$max)
      )
  })
  # set margins
  m <- list(
    l = 0,
    r = 4, # increase this margin a bit to prevent the last lable dissapearing
    b = 0,
    t = 0,
    pad = 0
  )

  output$VacLineChart <- renderPlotly({
    ggplotly(VacLineChart(),
      tooltip = "text",
      height = 81
    ) %>%
      layout(
        margin = m,
        xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })

  # Add link to vacancy data
  observeEvent(input$link_to_tabpanel_vacancies2, {
    updateTabsetPanel(session, "navbar", "Dashboard")
    updateTabsetPanel(session, "datatabset", "Vacancies")
  })

  # Add link to skills data
  observeEvent(input$link_to_tabpanel_FE2, {
    updateTabsetPanel(session, "navbar", "Dashboard")
    updateTabsetPanel(session, "datatabset", "Skills")
  })

  ### E&T achievements ----
  output$skisup.ETach <- renderUI({
    ETach <- C_Achieve_ILR1621 %>%
      filter(
        time_period == "202021",
        LEP == input$lep1,
        level_or_type == "Education and training: Total"
      ) %>%
      summarise(ET_ach = sum(achievements), .groups = "drop")

    # E&T achievements change
    ETachChange <- ((C_Achieve_ILR1621 %>%
      filter(
        time_period == "202021",
        LEP == input$lep1,
        level_or_type == "Education and training: Total"
      ) %>%
      summarise(ET_ach = sum(achievements), .groups = "drop"))
    - (C_Achieve_ILR1621 %>%
        filter(
          time_period == "201920",
          LEP == input$lep1,
          level_or_type == "Education and training: Total"
        ) %>%
        summarise(ET_ach = sum(achievements), .groups = "drop")))

    # print with formatting
    h4(span("2020/21", style = "font-size: 16px;font-weight:normal;"), br(),
      format(ETach, big.mark = ","), br(),
      span(
        format_pm(ETachChange) # plus-minus and comma sep formatting
        ,
        style = paste0("font-size: 16px;color:", cond_color(ETachChange > 0)) # colour formating
        , .noWS = c("before", "after") # remove whitespace
      ), br(),
      style = "font-size: 21px"
    )
  })

  # e and t chart
  etLineChart <- reactive({
    etLine <- C_Achieve_ILR1621 %>%
      filter(
        LEP == input$lep1,
        level_or_type == "Education and training: Total",
        time_period != "202122"
      ) %>%
      group_by(time_period) %>%
      summarise(achievements = sum(achievements), .groups = "drop") %>%
      mutate(Year = as.numeric(substr(time_period, 3, 4)), LEP = input$lep1)

    etCntChange <- (etLine %>% filter(Year == 20))$achievements -
      (etLine %>% filter(Year == 19))$achievements
    etMin <- etLine %>% summarise(min = min(achievements), .groups = "drop")
    etMax <- etLine %>% summarise(max = max(achievements), .groups = "drop")

    ggplot(etLine, aes(x = Year, y = achievements, group = LEP, text = paste0(
      "Academic year: ", time_period, "<br>",
      "Achievements: ", format(achievements, big.mark = ","), "<br>"
    ))) +
      geom_line(data = etLine %>% filter(Year <= 19)) +
      geom_ribbon(
        data = etLine %>% filter(Year >= 19),
        aes(ymin = min(achievements), ymax = achievements),
        fill = ifelse(etCntChange > 0, "#00703c", "#d4351c"),
        alpha = 0.3
      ) +
      geom_line(
        data = etLine %>% filter(Year >= 19),
        color = ifelse(etCntChange > 0, "#00703c", "#d4351c")
      ) +
      # add a blank line for the formatted tooltip
      theme_classic() +
      theme(
        axis.line = element_blank(),
        # axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "#f3f2f1"),
        plot.background = element_rect(fill = "#f3f2f1")
      ) +
      scale_y_continuous(
        labels = label_number_si(accuracy = 1),
        breaks = c(etMin$min, etMax$max)
      )
  })
  # set margins
  m <- list(
    l = 0,
    r = 4, # increase this margin a bit to prevent the last lable dissapearing
    b = 0,
    t = 0,
    pad = 0
  )

  output$etLineChart <- renderPlotly({
    ggplotly(etLineChart(),
      tooltip = "text",
      height = 81
    ) %>%
      layout(
        margin = m,
        xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })

  ### App achievements ----
  output$skisup.APPach <- renderUI({
    Appach <- C_Achieve_ILR1621 %>%
      filter(
        time_period == "202021",
        LEP == input$lep1,
        level_or_type == "Apprenticeships: Total"
      ) %>%
      summarise(App_ach = sum(achievements, na.rm = TRUE), .groups = "drop")

    # App achievements change
    AppachChange <- ((C_Achieve_ILR1621 %>%
      filter(
        time_period == "202021",
        LEP == input$lep1,
        level_or_type == "Apprenticeships: Total"
      ) %>%
      summarise(App_ach = sum(achievements, na.rm = TRUE), .groups = "drop"))
    - (C_Achieve_ILR1621 %>%
        filter(
          time_period == "201920",
          LEP == input$lep1,
          level_or_type == "Apprenticeships: Total"
        ) %>%
        summarise(App_ach = sum(achievements, na.rm = TRUE), .groups = "drop")))

    # print with formatting
    h4(span("2020/21", style = "font-size: 16px;font-weight:normal;"), br(),
      format(Appach, big.mark = ","), br(),
      span(
        format_pm(AppachChange) # plus-minus and comma sep formatting
        ,
        style = paste0("font-size: 16px;color:", cond_color(AppachChange > 0)) # colour formating
        , .noWS = c("before", "after") # remove whitespace
      ), br(),
      style = "font-size: 21px"
    )
  })

  # app chart
  AppLineChart <- reactive({
    AppLine <- C_Achieve_ILR1621 %>%
      filter(
        LEP == input$lep1,
        level_or_type == "Apprenticeships: Total",
        time_period != "202122"
      ) %>%
      group_by(time_period) %>%
      summarise(achievements = sum(achievements, na.rm = TRUE)) %>%
      mutate(
        Year = as.numeric(substr(time_period, 3, 4)),
        LEP = input$lep1
      )

    AppCntChange <- (AppLine %>% filter(Year == 20))$achievements -
      (AppLine %>% filter(Year == 19))$achievements
    AppMin <- AppLine %>% summarise(min = min(achievements), .groups = "drop")
    AppMax <- AppLine %>% summarise(max = max(achievements), .groups = "drop")

    ggplot(AppLine, aes(x = Year, y = achievements, group = LEP, text = paste0(
      "Academic year: ", time_period, "<br>",
      "Achievements: ", format(achievements, big.mark = ","), "<br>"
    ))) +
      geom_line(data = AppLine %>% filter(Year <= 19)) +
      geom_ribbon(
        data = AppLine %>% filter(Year >= 19),
        aes(ymin = min(achievements), ymax = achievements),
        fill = ifelse(AppCntChange > 0, "#00703c", "#d4351c"),
        alpha = 0.3
      ) +
      geom_line(
        data = AppLine %>% filter(Year >= 19),
        color = ifelse(AppCntChange > 0, "#00703c", "#d4351c")
      ) +
      # add a blank line for the formatted tooltip
      theme_classic() +
      theme(
        axis.line = element_blank(),
        # axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "#f3f2f1"),
        plot.background = element_rect(fill = "#f3f2f1")
      ) +
      scale_y_continuous(
        labels = label_number_si(accuracy = 1),
        breaks = c(AppMin$min, AppMax$max)
      )
  })
  # set margins
  m <- list(
    l = 0,
    r = 4, # increase this margin a bit to prevent the last lable dissapearing
    b = 0,
    t = 0,
    pad = 0
  )

  output$AppLineChart <- renderPlotly({
    ggplotly(AppLineChart(),
      tooltip = "text",
      height = 81
    ) %>%
      layout(
        margin = m,
        xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })

  # EMPLOYMENT ----
  #define page title
  output$page1title <- renderUI({
    paste0(input$lep1, " employment trends")
  })
  
  ### Downloads----
  list_of_datasets1 <- list(
    "1a.Emp by occupation" = D_EmpOcc_APS1721,
    "1b.Emp rate" = D_EmpRate_APS1721
  )
  output$download_btn1a <- downloadHandler(
    filename = function() {
      "EmploymentIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list_of_datasets1, path = file)
    }
  )

  # Download current LEP indicators
  filtered_data1 <- reactive({
    list(
      "1a.Emp by occupation" = filter(D_EmpOcc_APS1721, geographic_level == "lep", area == input$lep1),
      "1b.Emp rate" = filter(D_EmpRate_APS1721, geographic_level == "lep", area == input$lep1)
    )
  })
  output$download_btn1b <- downloadHandler(
    filename = function() {
      "CurrentEmploymentIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(filtered_data1(), path = file)
    }
  )

  ## KPIs ----

  ### Employment rate -----
  output$locland.emplrate <- renderValueBox({
    valueBox(
      paste0(
        format(100. * emp2021()$empRate, digits = 2),
        "%"
      ),
      paste0("employment rate in 2021 in ", input$lep1),
      color = "blue"
    )
  })

  output$locland.emplrate.2 <- renderValueBox({
    valueBox(
      paste0(
        format(100. * (C_EmpRate_APS1721 %>%
          filter(
            area == input$lep2,
            year == "2021"
          )
        )$empRate, digits = 2),
        "%"
      ),
      paste0("employment rate in 2021 in ", input$lep2),
      color = "orange"
    )
  })
  ### Employment count ----
  output$locland.emplcnt <- renderValueBox({
    # Put value into box to plug into app
    valueBox(
      # take input number
      format(emp2021()$Employment,
      scientific = FALSE, big.mark = ","
      ),
      # add subtitle to explain what it's showing
      paste0("in employment in 2021 in ", input$lep1),
      color = "blue"
    )
  })

  output$locland.emplcnt.2 <- renderValueBox({
    valueBox(
      format((C_EmpRate_APS1721 %>%
        filter(
          area == input$lep2,
          year == "2021"
        )
      )$Employment,
      scientific = FALSE, big.mark = ","
      ),
      paste0("in employment in 2021 in ", input$lep2),
      color = "orange"
    )
  })

  # turn off comparison boxes if none is selected
  output$emp_comp <- renderUI({
    if ("lep2" %in% names(input)) {
      if (input$lep2 == "\nNone") {
        tagList(
          br(),
          p("")
        )
      } else {
        tagList(
          valueBoxOutput("locland.emplcnt.2"),
          valueBoxOutput("locland.emplrate.2")
        )
      }
    } else {
      p("")
    }
  })

  ## Employment rate over time line graph ----
  EmpRate_time <- reactive({
    EmpRateTime <- C_EmpRate_APS1721 %>%
      select(year, area, geographic_level, empRate) %>%
      filter(
        area == "England" |
          area == input$lep1 |
          area == if ("lep2" %in% names(input)) {
            input$lep2
          } else {
            "\nNone"
          }
      )
    # add an extra column so the colours work in ggplot when sorting alphabetically
    EmpRateTime$Areas <- factor(EmpRateTime$area,
      levels = c("England", input$lep1, input$lep2)
    )

    ggplot(
      EmpRateTime,
      aes(
        x = year, y = empRate,
        color = Areas, group = Areas,
        text = paste0(
          "Academic year: ", year, "<br>",
          "Area: ", Areas, "<br>",
          "Employment rate: ", scales::percent(round(empRate, 2)), "<br>"
        )
      )
    ) +
      geom_line() +
      theme_minimal() +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "bottom", legend.title = element_blank()) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(.65, .85)) +
      labs(colour = "") +
      scale_color_manual(values = c("#28a197", "#1d70b8", "#F46A25"))
  })

  output$EmpRate_time <- renderPlotly({
    ggplotly(EmpRate_time(), tooltip = "text") %>%
      layout(
        legend = list(orientation = "h", x = 0, y = -0.1),
        xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })

  ## Employment by occupation data table ----
  EmpOcc <- reactive({
    EmpOcc <- C_EmpOcc_APS1721 %>%
      filter(year == "2021") %>%
      filter(
        geographic_level == "lep" |
          geographic_level == "country", # cleans up for London and South East which is included as lep and gor
        area == "England" |
          area == input$lep1 |
          area == if ("lep2" %in% names(input)) {
            input$lep2
          } else {
            "\nNone"
          }
      ) %>%
      select(-year, -geographic_level) %>%
      rename_with(str_to_sentence) %>% # capitalise column titles
      t() %>%
      row_to_names(row_number = 1) %>%
      as.data.frame() %>%
      mutate_if(is.character, as.numeric) %>%
      mutate(across(where(is.numeric), ~ round(prop.table(.), 4)))
    # mutate(across(where(is.numeric), format, big.mark = ",")) %>%
  })

  output$EmpOcc <- renderDataTable({
    df <- EmpOcc()
    datatable(df, options = list(order = list(2, "desc"))) %>%
      formatPercentage(1:ncol(df), 1)
  })

  # VACANCIES ----
#define page title
  output$page3title <- renderUI({
    paste0(input$lep1, " vacancy trends")
  })
  
  ### Downloads----
  # download skills indicators
  list_of_datasets3 <- list("2.Vacancies" = C_Vacancy_ONS1722)
  output$download_btn3a <- downloadHandler(
    filename = function() {
      "VacancyIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list_of_datasets3, path = file)
    }
  )

  # Download current LEP indicators
  filtered_data3 <- reactive({
    list("2.Vacancies" = filter(C_Vacancy_ONS1722, LEP == input$lep1))
  })
  output$download_btn3b <- downloadHandler(
    filename = function() {
      "CurrentVacancyIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(filtered_data3(), path = file)
    }
  )

  ## KPIs ----
  ### ONS job advert unit percent of total  ----
  output$jobad.pc <- renderValueBox({
    valueBox(
      paste0(
        format(100. * (C_Vacancy_England %>%
          filter(
            year == "2022",
            LEP == input$lep1
          ) %>%
          summarise(job.pc = sum(pc_total), .groups = "drop")), digits = 3),
        "%"
      ),
      paste0("of online vacancies in England (Jan 2022) were in ", input$lep1),
      color = "blue"
    )
  })

  output$jobad.pc.2 <- renderValueBox({
    # Put value into box to plug into app
    valueBox(
      paste0(
        format(100. * (C_Vacancy_England %>%
          filter(
            year == "2022",
            LEP == input$lep2
          ) %>%
          summarise(job.pc = sum(pc_total), .groups = "drop")), digits = 3),
        "%"
      ),
      paste0("of online vacancies in England (Jan 2022) were in ", input$lep2),
      color = "orange"
    )
  })

  ### Skill Demand KPI 2 ----
  output$jobad.ch <- renderValueBox({
    valueBox(
      paste0(
        format(100. * (C_Vacancy_England %>%
          filter(
            year == "2022" |
              year == "2021",
            LEP == input$lep1
          ) %>%
          group_by(year) %>%
          summarise(job.cnt = sum(vacancy_unit), .groups = "drop") %>%
          mutate(Row = 1:n()) %>%
          mutate(Percentage_Change = (job.cnt / lag(job.cnt)) - 1) %>%
          ungroup() %>%
          filter(year == "2022") %>%
          select(Percentage_Change)), digits = 3),
        "%"
      ),
      paste0("change in online job vacancies in ", input$lep1, " from Jan 2021 to Jan 2022"),
      color = "blue"
    )
  })

  output$jobad.ch.2 <- renderValueBox({
    # Put value into box to plug into app
    valueBox(
      paste0(
        format(100. * (C_Vacancy_England %>%
          filter(
            year == "2022" |
              year == "2021",
            LEP == input$lep2
          ) %>%
          group_by(year) %>%
          summarise(job.cnt = sum(vacancy_unit), .groups = "drop") %>%
          mutate(Row = 1:n()) %>%
          mutate(Percentage_Change = (job.cnt / lag(job.cnt)) - 1) %>%
          ungroup() %>%
          filter(year == "2022") %>%
          select(Percentage_Change)), digits = 3),
        "%"
      ),
      paste0("change in online job vacancies in ", input$lep2, " from Jan 2021 to Jan 2022"),
      color = "orange"
    )
  })

  # turn off comparison boxes if none is selected
  output$vac_comp <- renderUI({
    if ("lep2" %in% names(input)) {
      if (input$lep2 == "\nNone") {
        tagList(
          br(),
          p("")
        )
      } else {
        tagList(
          valueBoxOutput("jobad.pc.2"),
          valueBoxOutput("jobad.ch.2")
        )
      }
    } else {
      p("")
    }
  })

  ## Online job vacancy units over time line chart ----
  jobad.time <- reactive({
    JobTime <- C_Vacancy_England %>%
      filter(LEP == input$lep1 |
        LEP == if ("lep2" %in% names(input)) {
          input$lep2
        } else {
          "\nNone"
        }) %>%
      select(-LA, -pc_total, -region, -England) %>%
      group_by(year, LEP) %>%
      summarise(total = sum(vacancy_unit), .groups = "drop")
    # add an extra column so the colours work in ggplot when sorting alphabetically
    JobTime$Areas <- factor(JobTime$LEP,
      levels = c(input$lep1, input$lep2)
    )
    ggplot(
      JobTime,
      aes(
        x = year, y = total, colour = Areas, group = Areas,
        text = paste0(
          "Year: ", year, "<br>",
          "Area: ", Areas, "<br>",
          "Job vacancy units: ", round(total, 0), "<br>"
        )
      )
    ) +
      geom_line() +
      theme_minimal() +
      labs(colour = "LEP") +
      theme(legend.position = "bottom", axis.title.x = element_blank(), axis.title.y = element_blank()) +
      labs(shape = "", colour = "") +
      scale_color_manual(values = c("#1d70b8", "#F46A25"))
  })

  output$jobad.time <- renderPlotly({
    ggplotly(jobad.time(), tooltip = c("text")) %>%
      layout(
        legend = list(orientation = "h", x = 0, y = -0.1),
        xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })


  # FE ----
  #define page title
  output$page2title <- renderUI({
    paste0(input$lep1, " Further Education (FE) and skills supply trends")
  })
  
  ### Downloads----
  # download skills indicators
  list_of_datasets2 <- list(
    "3a.FE achievements SSA" = D_Achieve_ILR21,
    "3b.FE achievements" = D_Achieve_ILR1621
  )
  output$download_btn2a <- downloadHandler(
    filename = function() {
      "SkillIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list_of_datasets2, path = file)
    }
  )

  # Download current LEP indicators
  filtered_data2 <- reactive({
    list(
      "3a.FE achievements SSA" = filter(D_Achieve_ILR21, LEP == input$lep1),
      "3b.FE achievements" = filter(D_Achieve_ILR1621, LEP == input$lep1)
    )
  })
  output$download_btn2b <- downloadHandler(
    filename = function() {
      "CurrentSkillIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(filtered_data2(), path = file)
    }
  )

  ## KPIs ----
  ### FE achievements -----
  output$skisup.FEach <- renderValueBox({
    # Put value into box to plug into app
    valueBox(
      format((C_Achieve_ILR1621 %>%
        filter(
          time_period == "202021",
          LEP == input$lep1,
          level_or_type == "Education and training: Total"
        ) %>%
        summarise(App_ach = sum(achievements), .groups = "drop")), scientific = FALSE, big.mark = ","),
      paste0("20/21 adult education and training achievements in ", input$lep1),
      color = "blue"
    )
  })

  output$skisup.FEach.2 <- renderValueBox({
    # Put value into box to plug into app
    valueBox(
      format((C_Achieve_ILR1621 %>%
        filter(
          time_period == "202021",
          LEP == input$lep2,
          level_or_type == "Education and training: Total"
        ) %>%
        summarise(App_ach = sum(achievements), .groups = "drop")), scientific = FALSE, big.mark = ","),
      paste0("20/21 adult education and training achievements in ", input$lep2),
      color = "orange"
    )
  })

  ### Apprentichesip achievements ----
  output$skisup.APach <- renderValueBox({
    # Put value into box to plug into app
    valueBox(
      format((C_Achieve_ILR1621 %>%
        filter(
          time_period == "202021",
          LEP == input$lep1,
          level_or_type == "Apprenticeships: Total"
        ) %>%
        summarise(App_ach = sum(achievements), .groups = "drop")), scientific = FALSE, big.mark = ","),
      paste0("20/21 apprenticeship achievements in ", input$lep1),
      color = "blue"
    )
  })

  output$skisup.APach.2 <- renderValueBox({
    # Put value into box to plug into app
    valueBox(
      format((C_Achieve_ILR1621 %>%
        filter(
          time_period == "202021",
          LEP == input$lep2,
          level_or_type == "Apprenticeships: Total"
        ) %>%
        summarise(App_ach = sum(achievements), .groups = "drop")), scientific = FALSE, big.mark = ","),
      paste0("20/21 apprenticeship achievements in ", input$lep2),
      color = "orange"
    )
  })

  # turn off comparison boxes if none is selected
  output$skill_comp <- renderUI({
    if ("lep2" %in% names(input)) {
      if (input$lep2 == "\nNone") {
        tagList(
          br(),
          p("")
        )
      } else {
        tagList(
          valueBoxOutput("skisup.FEach.2"),
          valueBoxOutput("skisup.APach.2")
        )
      }
    } else {
      p("")
    }
  })

  ## Achievements over time line chart ----
  Ach_time <- reactive({
    FETime <- C_Achieve_ILR1621 %>%
      select(time_period, area, LEP, level_or_type, achievements) %>%
      mutate(level_or_type = case_when(
        level_or_type == "Further education and skills: Total" ~ "Total FE and skills provision",
        level_or_type == "Education and training: Total" ~ "Education and training (adults only)",
        level_or_type == "Community learning: Total" ~ "Community learning (adults only)",
        level_or_type == "Apprenticeships: Total" ~ "Apprenticeships (all ages)",
        TRUE ~ level_or_type
      )) %>%
      filter(
        # area == "England" |
        LEP == input$lep1 |
          LEP == if ("lep2" %in% names(input)) {
            input$lep2
          } else {
            "\nNone"
          },
        level_or_type == input$skill_line,
        time_period != 202122
      ) %>%
      mutate(AY = paste(substr(time_period, 3, 4), "/", substr(time_period, 5, 6), sep = "")) %>%
      group_by(AY, LEP, level_or_type) %>%
      summarise(Achievements = sum(achievements, na.rm = T), .groups = "drop")
    # add an extra column so the colours work in ggplot when sorting alphabetically
    FETime$Area <- factor(FETime$LEP,
      levels = c(input$lep1, input$lep2)
    )
    ggplot(FETime, aes(
      x = AY, y = Achievements, colour = Area,
      group = interaction(level_or_type, LEP),
      text = paste0(
        "Academic year: ", AY, "<br>",
        "Area: ", Area, "<br>",
        "Achievements: ", format(Achievements, big.mark = ","), "<br>",
        "Provision: ", level_or_type, "<br>"
      )
    )) +
      geom_line() +
      theme_minimal() +
      theme(legend.position = "bottom", axis.title.x = element_blank(), axis.title.y = element_blank()) +
      labs(shape = "", colour = "") +
      scale_y_continuous(label = comma) +
      xlab("Year") +
      scale_color_manual(values = c("#1d70b8", "#F46A25"))
  })

  output$Ach_time <- renderPlotly({
    ggplotly(Ach_time(), tooltip = c("text")) %>%
      layout(
        legend = list(orientation = "h", x = 0, y = -0.1),
        xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })

  ## Achievements pc bar chart ----
  Ach_SSA_pc <- reactive({
    AchSSA_21 <- C_Achieve_ILR21 %>%
      filter(
        time_period == "202122",
        LEP == input$lep1 |
          LEP == if ("lep2" %in% names(input)) {
            input$lep2
          } else {
            "\nNone"
          }
      ) %>%
      select(LEP, SSA = ssa_t1_desc, Achievements = achievements) %>%
      group_by(LEP, SSA) %>%
      summarise(Achievements = sum(Achievements, na.rm = T), .groups = "drop") %>%
      ungroup()

    Ach_pc <- AchSSA_21 %>%
      filter(SSA == "Total") %>%
      select(LEP, Total = SSA, Total_ach = Achievements)

    FEBar <- AchSSA_21 %>%
      left_join(Ach_pc, by = "LEP") %>%
      group_by(LEP) %>%
      mutate(pc = Achievements / Total_ach) %>%
      filter(SSA != "Total")
    # add an extra column so the colours work in ggplot when sorting alphabetically
    FEBar$Area <- factor(FEBar$LEP,
      levels = c(input$lep1, input$lep2)
    )
    Ach_SSA_pc <- ggplot(FEBar, aes(x = reorder(SSA, desc(SSA)), y = pc, fill = Area, text = paste0(
      "SSA: ", SSA, "<br>",
      "Area: ", Area, "<br>",
      "Percentage of achievements: ", scales::percent(round(pc, 2)), "<br>",
      "Achievements: ", Achievements, "<br>"
    ))) +
      geom_col(
        position = "dodge"
      ) +
      scale_y_continuous(labels = scales::percent) +
      # scale_x_discrete(label = function(SSA) stringr::str_trunc(SSA, 12)) + # truncate labels because they can be very long
      scale_x_discrete(labels = function(SSA) str_wrap(SSA, width = 26)) +
      coord_flip() +
      theme_minimal() +
      labs(fill = "") +
      theme(
        legend.position = "bottom", axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_text(size = 7),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
      ) +
      scale_fill_manual(values = c("#1d70b8", "#F46A25"))
  })

  output$Ach_SSA_pc <- renderPlotly({
    ggplotly(Ach_SSA_pc(), tooltip = c("text"), height = 474) %>%
      layout(
        legend = list(orientation = "h", x = 0, y = -0.1),
        xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })

  # Stop app ---------------------------------------------------------------------------------

  session$onSessionEnded(function() {
    stopApp()
  })
}
