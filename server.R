server <- function(input, output, session) {

  # Loading screen ---------------------------------------------------------------------------
  # Call initial loading screen

  hide(id = "loading-content", anim = TRUE, animType = "fade")
  show("app-content")

  # HOMEPAGE ----
  # Create link to overview tab
  observeEvent(input$link_to_tabpanel_overview, {
    updateTabsetPanel(session, "navbar", "Local Skills") # Get into app
    updateTabsetPanel(session, "datatabset", "Overview") # then pick tab
  })
  # Create link to employment data tab
  observeEvent(input$link_to_tabpanel_employment, {
    updateTabsetPanel(session, "navbar", "Local Skills")
    updateTabsetPanel(session, "datatabset", "Employment")
  })
  # Create link to vacancy data tab
  observeEvent(input$link_to_tabpanel_vacancies, {
    updateTabsetPanel(session, "navbar", "Local Skills")
    updateTabsetPanel(session, "datatabset", "Vacancies")
  })
  # Create link to skills data tab
  observeEvent(input$link_to_tabpanel_FE, {
    updateTabsetPanel(session, "navbar", "Local Skills")
    updateTabsetPanel(session, "datatabset", "Skills")
  })

  # create table download datasets
  output$downloadData1 <- downloadHandler(
    filename = function() {
      "EmploymentRateIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list(
        "1b.Emp rate" = D_EmpRate_APS1721
      ), path = file)
    }
  )
  output$downloadData2 <- downloadHandler(
    filename = function() {
      "EmploymentByOccupationIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list(
        "1a.Emp by occupation" = D_EmpOcc_APS1721
      ), path = file)
    }
  )
  output$downloadData3 <- downloadHandler(
    filename = function() {
      "VacancyIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list(
        "2.Vacancies" = C_Vacancy_ONS1722
      ), path = file)
    }
  )
  output$downloadData4 <- downloadHandler(
    filename = function() {
      "AcheivementIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list(
        "3b.FE achievements" = D_Achieve_ILR1621
      ), path = file)
    }
  )
  output$downloadData5 <- downloadHandler(
    filename = function() {
      "AchievementBySSAIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list(
        "3a.FE achievements SSA" = D_Achieve_ILR21
      ), path = file)
    }
  )
  
  output$downloadData6 <- downloadHandler(
    filename = function() {
      "EmploymentbyIndustryIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list(
        "4a.Employment by Industry" = D_empind_APS1721
      ), path = file)
    }
  )
  
  output$downloadData7 <- downloadHandler(
    filename = function() {
      "Skilllevelbyage.xlsx"
    },
    content = function(file) {
      write_xlsx(list(
        "4a.Skill level by age" = D_skillsnvq_APS2021
      ), path = file)
    }
  )
  
  
  output$downloadData8 <- downloadHandler(
    filename = function() {
      "EnterprisebyemploymentsizeIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list(
        "6a.Enterprise by emp size" = D_empent_UBC1822
      ), path = file)
    }
  )
  
  
  output$downloadData9 <- downloadHandler(
    filename = function() {
      "Keystage4destinationsIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list(
        "7a.Key Stage 4 destinations" = D_KS4destin_1520
      ), path = file)
    }
  )
  
  output$downloadData10 <- downloadHandler(
    filename = function() {
      "Keystage5destinationsIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list(
        "8a.Key Stage 5 destinations" = D_KS5destin_1520
      ), path = file)
    }
  )
  

  # create download links
  output$hidden_downloads <- renderUI(
    lapply(1:10, function(i) {
      downloadLink(paste0("downloadData", i), "download", class = "hiddenLink")
    })
  )
  # create data table to show
  output$DataTbl <- renderDataTable({
    DT::datatable(I_DataTable %>%
      mutate("Dashboard data" = lapply(
        1:n(),
        function(i) {
          paste0('<a href="#" onClick=document.getElementById("downloadData', i, '").click() >Download</a>')
        }
      )), escape = FALSE, options = list(dom = "t"), rownames = FALSE)
  })

  # OVERVIEW ----

  # alter area dropdown depending if lep or lsip
  output$lep1_geo <- renderUI({
    if (input$GeoType == "LEP") {
      selectInput("lep1", "Choose primary LEP area",
        choices = C_LEP2020 %>% filter(geographic_level == "LEP") %>% select(Area),
        selected = input$lep1
      )
    } else {
      selectInput("lep1", "Choose primary LSIP area",
        choices = C_LEP2020 %>% filter(geographic_level == "LSIP") %>% select(Area),
        selected = input$lep1
      )
    }
  })

  # turn off lep 2 for overview page (as not used here)
  output$lep2_off <- renderUI({
    if (input$datatabset == "Overview") {
      p("")
    } else {
      if (input$GeoType == "LEP") {
        selectInput("lep2", "Choose comparison LEP area",
          choices = c("\nNone", C_LEP2020 %>% filter(geographic_level == "LEP", Area != input$lep1) %>% select(Area)),
          selected = input$lep2
        )
      } else {
        selectInput("lep2", "Choose comparison LSIP area",
          choices = c("\nNone", C_LEP2020 %>% filter(geographic_level == "LSIP", Area != input$lep1) %>% select(Area)),
          selected = input$lep2
        )
      }
    }
  })

  # define page title
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
      "1a.Emp by occupation" = filter(D_EmpOcc_APS1721, geographic_level == input$GeoType, area == input$lep1),
      "1b.Emp rate" = filter(D_EmpRate_APS1721, geographic_level == input$GeoType, area == input$lep1),
      "2.Vacancies" = filter(C_Vacancy_ONS1722, geographic_level == input$GeoType,area == input$lep1),
      "3a.FE achievements SSA" = filter(D_Achieve_ILR21, geographic_level == input$GeoType,area == input$lep1),
      "3b.FE achievements" = filter(D_Achieve_ILR1621, geographic_level == input$GeoType,area == input$lep1)
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

  # get emp data for current lep
  empLEP <- eventReactive(input$lep1,{
    C_EmpRate_APS1721 %>%
      filter(area == input$lep1, geographic_level == input$GeoType)
  })
  # get 2021 values
  emp2021 <- reactive({
    empLEP() %>%
      filter(year == "2021")
  })
  # get 2020 values
  emp2020 <- reactive({
    empLEP() %>%
      filter(year == "2020")
  })

  #### Employment count ----
  output$locland.emplcnt0 <- renderUI({
    # call 2020 and 2021 values for chosen LEP
    empCnt2021 <- emp2021()$Employment
    empCntChange <- emp2021()$Employment - emp2020()$Employment

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
  empLineChart <- eventReactive(input$lep1,{
    # call 2020 to 2021 change  for chosen LEP
    empCntChange <- emp2021()$Employment - emp2020()$Employment
    empLine <- empLEP()

    # find min and max for lep
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
    validate(
      need(input$lep1 != "", "") # if area not yet loaded don't try to load ch
    )
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
    # call 2021 values and 20-21 change for chosen LEP
    empRate2021 <- emp2021()$empRate
    empRateChange <- emp2021()$empRate - emp2020()$empRate

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

  empRateLineChart <- eventReactive(input$lep1,{
    empRateChange <- emp2021()$empRate - emp2020()$empRate
    empRateLine <- C_EmpRate_APS1721 %>%
      filter((geographic_level == input$GeoType & area == input$lep1) | (geographic_level == "COUNTRY" & area == "England"))

    ggplot(empRateLine, aes(
      x = Year, y = empRate,
      group = area,
      text = paste0(
        "Year: ", year, "<br>",
        "Area: ", area, "<br>",
        "Employment rate: ", format(100 * empRate, digit = 2), "%<br>"
      )
    )) +
      geom_line(data = empRateLine %>% filter(Year <= 20, geographic_level == input$GeoType)) +
      geom_line(data = empRateLine %>% filter(geographic_level == "COUNTRY"), alpha = 0.5) +
      geom_ribbon(
        data = empRateLine %>% filter(Year >= 20, geographic_level == input$GeoType),
        aes(ymin = min(empRate), ymax = empRate),
        fill = ifelse(empRateChange > 0, "#00703c", "#d4351c"),
        alpha = 0.3
      ) +
      geom_line(
        data = empRateLine %>% filter(Year >= 20, geographic_level == input$GeoType),
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
  # get vac data for current area chosen
  VacArea <- eventReactive(input$lep1,{
    C_Vacancy_England %>%
      filter(area == input$lep1, geographic_level == input$GeoType)
  })
  # get 2022 values
  Vac2022 <- reactive({
    VacArea() %>%
      filter(year == "2022")
  })
  # get 2021 values
  Vac2021 <- reactive({
    VacArea() %>%
      filter(year == "2021")
  })

  # Vacancy kpi
  output$jobad.units <- renderUI({
    ### ONS job advert units change
    VacPcChange <- Vac2022()$jobpc - Vac2021()$jobpc

    # print with formatting
    h4(span("2022", style = "font-size: 16px;font-weight:normal;"), br(),
      paste0(format(100 * Vac2022()$jobpc, digit = 2), "%"), br(),
      span(
        paste0(sprintf("%+.1f", 100 * VacPcChange), "ppts"),
        style = paste0("font-size: 16px;color:", cond_color(VacPcChange > 0)) # colour formating
        , .noWS = c("before", "after") # remove whitespace
      ), br(),
      style = "font-size: 21px"
    )
  })

  # Vacancy chart
  VacLineChart <- eventReactive(input$lep1,{
    VacLine <- VacArea()
    VacPcChange <- Vac2022()$jobpc - Vac2021()$jobpc

    VacMinMax <- C_Vacancy_England_max_min %>% filter(area == input$lep1, geographic_level == input$GeoType)

    ggplot(VacLine, aes(x = Year, y = jobpc, group = area, text = paste0(
      "Year: ", year, "<br>",
      "England vacancy share: ", format(100 * jobpc, digit = 2), "%<br>"
    ))) +
      geom_line(data = VacLine %>% filter(Year <= 21)) +
      geom_ribbon(
        data = VacLine %>% filter(Year >= 21),
        aes(ymin = min(jobpc), ymax = jobpc),
        fill = ifelse(VacPcChange > 0, "#00703c", "#d4351c"),
        alpha = 0.3
      ) +
      geom_line(
        data = VacLine %>% filter(Year >= 21),
        color = ifelse(VacPcChange > 0, "#00703c", "#d4351c")
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
        labels = scales::percent_format(accuracy = 0.1),
        breaks = c(VacMinMax$minVac, VacMinMax$maxVac)
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

  #### E&T achievements ----

  # get EandT data for current lep
  EtLEP <- eventReactive(input$lep1,{
    C_Achieve_ILR1621 %>%
      filter(
        geographic_level == input$GeoType,
        area == input$lep1,
        level_or_type == "Education and training: Total"
      )
  })
  # get 20/21 values
  Et2021 <- reactive({
    EtLEP() %>%
      filter(time_period == "202021")
  })
  # get 19/20 values
  Et1920 <- reactive({
    EtLEP() %>%
      filter(time_period == "201920")
  })

  output$skisup.ETach <- renderUI({
    ETach <- Et2021()$achievements

    # E&T achievements change
    ETachChange <- Et2021()$achievements - Et1920()$achievements

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
  etLineChart <- eventReactive(input$lep1,{
    etLine <- EtLEP()
    etCntChange <- Et2021()$achievements - Et1920()$achievements
    EtMinMax <- C_Achieve_ILR1621_max_min %>% filter(
      geographic_level == input$GeoType,
      area == input$lep1,
      level_or_type == "Education and training: Total"
    )

    ggplot(etLine, aes(x = Year, y = achievements, group = area, text = paste0(
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
        breaks = c(EtMinMax$minAch, EtMinMax$maxAch)
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

  #### App achievements ----
  # get App data for current lep
  AppLEP <- eventReactive(input$lep1,{
    C_Achieve_ILR1621 %>%
      filter(
        geographic_level == input$GeoType,
        area == input$lep1,
        level_or_type == "Apprenticeships: Total"
      )
  })
  # get 20/21 values
  App2021 <- reactive({
    AppLEP() %>%
      filter(time_period == "202021")
  })
  # get 19/20 values
  App1920 <- reactive({
    AppLEP() %>%
      filter(time_period == "201920")
  })
  output$skisup.APPach <- renderUI({
    Appach <- App2021()$achievements

    # E&T achievements change
    AppachChange <- App2021()$achievements - App1920()$achievements

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
  AppLineChart <- eventReactive(input$lep1,{
    AppLine <- AppLEP()
    AppCntChange <- App2021()$achievements - App1920()$achievements
    AppMinMax <- C_Achieve_ILR1621_max_min %>% filter(
      geographic_level == input$GeoType,
      area == input$lep1,
      level_or_type == "Apprenticeships: Total"
    )


    ggplot(AppLine, aes(x = Year, y = achievements, group = area, text = paste0(
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
        breaks = c(AppMinMax$minAch, AppMinMax$maxAch)
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

  # Add link to skills data
  observeEvent(input$link_to_tabpanel_FE2, {
    updateTabsetPanel(session, "navbar", "Dashboard")
    updateTabsetPanel(session, "datatabset", "Skills")
  })

  # EMPLOYMENT ----
  # define page title
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
      "1a.Emp by occupation" = filter(D_EmpOcc_APS1721, geographic_level == input$GeoType, area == input$lep1),
      "1b.Emp rate" = filter(D_EmpRate_APS1721, geographic_level == input$GeoType, area == input$lep1)
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
            geographic_level == input$GeoType,
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
          geographic_level == input$GeoType,
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
  EmpRate_time <- eventReactive(c(input$lep1,input$lep2),{
    EmpRateTime <- C_EmpRate_APS1721 %>%
      select(year, area, geographic_level, empRate) %>%
      filter(
        geographic_level == input$GeoType | geographic_level == "COUNTRY",
        (area == "England" |
          area == input$lep1 |
          area == if ("lep2" %in% names(input)) {
            input$lep2
          } else {
            "\nNone"
          })
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
  EmpOcc <- eventReactive(c(input$lep1,input$lep2),{
    EmpOcc <- C_EmpOcc_APS1721 %>%
      filter(
        geographic_level == input$GeoType | geographic_level == "COUNTRY",
        (area == "England" |
          area == input$lep1 |
          area == if ("lep2" %in% names(input)) {
            input$lep2
          } else {
            "\nNone"
          })
      ) %>%
      select(-year, -geographic_level) %>%
      rename_with(str_to_sentence) %>%
      t() %>%
      row_to_names(row_number = 1) %>%
      as.data.frame() %>%
      mutate_if(is.character, as.numeric) %>%
      mutate(across(where(is.numeric), ~ round(prop.table(.), 4))) %>%
      rownames_to_column("Occupation")
  })

  output$EmpOcc <- renderDataTable({
    df <- EmpOcc()
    datatable(df, options = list(order = list(2, "desc")), rownames = FALSE) %>%
      formatPercentage(2:ncol(df), 1)
  })

  # VACANCIES ----
  # define page title
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
    list("2.Vacancies" = filter(C_Vacancy_ONS1722, geographic_level == input$GeoType, area == input$lep1))
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
  ### ONS job advert unit percent of total area 1
  output$jobad.pc <- renderValueBox({
    valueBox(
      paste0(
        format(100. * Vac2022()$jobpc, digits = 3),
        "%"
      ),
      paste0("of online vacancies in England (Jan 2022) were in ", input$lep1),
      color = "blue"
    )
  })

  ### ONS job advert unit percent of total LEP 1
  output$jobad.pc.2 <- renderValueBox({
    # Put value into box to plug into app
    valueBox(
      paste0(
        format(100. *
          (C_Vacancy_England %>%
            filter(area == input$lep2, geographic_level == input$GeoType, year == "2022"))$jobpc,
        digits = 3
        ),
        "%"
      ),
      paste0("of online vacancies in England (Jan 2022) were in ", input$lep2),
      color = "orange"
    )
  })

  ### ONS job advert unit change  LEP 1
  output$jobad.ch <- renderValueBox({
    valueBox(
      paste0(
        format(100. * (C_Vacancy_England_change %>%
          filter(area == input$lep1, geographic_level == input$GeoType))$Percentage_Change,
        digits = 3
        ),
        "%"
      ),
      paste0("change in online job vacancies in ", input$lep1, " from Jan 2021 to Jan 2022"),
      color = "blue"
    )
  })

  ### ONS job advert unit change  LEP 2
  output$jobad.ch.2 <- renderValueBox({
    # Put value into box to plug into app
    valueBox(
      paste0(
        format(100. * (C_Vacancy_England_change %>%
          filter(area == input$lep2, geographic_level == input$GeoType))$Percentage_Change,
        digits = 3
        ),
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
  jobad.time <- eventReactive(c(input$lep1,input$lep2),{
    JobTime <- C_Vacancy_England %>%
      filter(geographic_level == input$GeoType & (area == input$lep1 |
        area == if ("lep2" %in% names(input)) {
          input$lep2
        } else {
          "\nNone"
        }))

    # add an extra column so the colours work in ggplot when sorting alphabetically
    JobTime$Areas <- factor(JobTime$area,
      levels = c(input$lep1, input$lep2)
    )

    ggplot(
      JobTime,
      aes(
        x = year, y = jobcnt, colour = Areas, group = Areas,
        text = paste0(
          "Year: ", year, "<br>",
          "Area: ", Areas, "<br>",
          "Job vacancy units: ", round(jobcnt, 0), "<br>"
        )
      )
    ) +
      geom_line() +
      theme_minimal() +
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
  # define page title
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
      "3a.FE achievements SSA" = filter(D_Achieve_ILR21, geographic_level == input$GeoType, area == input$lep1),
      "3b.FE achievements" = filter(D_Achieve_ILR1621, geographic_level == input$GeoType, area == input$lep1)
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
    valueBox(format(Et2021()$achievements, scientific = FALSE, big.mark = ","),
      paste0("20/21 adult education and training achievements in ", input$lep1),
      color = "blue"
    )
  })

  output$skisup.FEach.2 <- renderValueBox({
    # Put value into box to plug into app
    valueBox(
      format((C_Achieve_ILR1621 %>%
        filter(
          geographic_level == input$GeoType,
          area == input$lep2, time_period == "202021",
          level_or_type == "Education and training: Total"
        ))$achievements, scientific = FALSE, big.mark = ","),
      paste0("20/21 adult education and training achievements in ", input$lep2),
      color = "orange"
    )
  })

  ### Apprenticeship achievements ----
  output$skisup.APach <- renderValueBox({
    # Put value into box to plug into app
    valueBox(
      format(App2021()$achievements, scientific = FALSE, big.mark = ","),
      paste0("20/21 apprenticeship achievements in ", input$lep1),
      color = "blue"
    )
  })

  output$skisup.APach.2 <- renderValueBox({
    # Put value into box to plug into app
    valueBox(
      format((C_Achieve_ILR1621 %>%
        filter(
          geographic_level == input$GeoType,
          area == input$lep2, time_period == "202021",
          level_or_type == "Apprenticeships: Total"
        ))$achievements, scientific = FALSE, big.mark = ","),
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
    }
  })

  ## Achievements over time line chart ----
  Ach_time <- eventReactive(c(input$lep1,input$lep2),{
    FETime <- C_Achieve_ILR1621 %>%
      filter(
        geographic_level == input$GeoType &
          (area == input$lep1 |
            (area == if ("lep2" %in% names(input)) {
              input$lep2
            } else {
              "\nNone"
            })
          ),
        level_or_typeNeat == input$skill_line
      )

    # add an extra column so the colours work in ggplot when sorting alphabetically
    FETime$Area <- factor(FETime$area,
      levels = c(input$lep1, input$lep2)
    )
    ggplot(FETime, aes(
      x = AY, y = achievements, colour = Area,
      group = interaction(level_or_typeNeat, area),
      text = paste0(
        "Academic year: ", AY, "<br>",
        "Area: ", Area, "<br>",
        "Achievements: ", format(achievements, big.mark = ","), "<br>",
        "Provision: ", level_or_typeNeat, "<br>"
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
  Ach_SSA_pc <- eventReactive(c(input$lep1,input$lep2),{
    AchSSA_21 <- C_Achieve_ILR21 %>%
      filter(
        geographic_level == input$GeoType,
        (area == input$lep1 |
          area == if ("lep2" %in% names(input)) {
            input$lep2
          } else {
            "\nNone"
          })
      )

    # add an extra column so the colours work in ggplot when sorting alphabetically
    AchSSA_21$Area <- factor(AchSSA_21$area,
      levels = c(input$lep1, input$lep2)
    )
    ggplot(AchSSA_21, aes(x = reorder(SSA, desc(SSA)), y = pc, fill = Area, text = paste0(
      "SSA: ", SSA, "<br>",
      "Area: ", Area, "<br>",
      "Percentage of achievements: ", scales::percent(round(pc, 2)), "<br>",
      "Achievements: ", Achievements, "<br>"
    ))) +
      geom_col(
        position = "dodge"
      ) +
      scale_y_continuous(labels = scales::percent) +
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
    ggplotly(Ach_SSA_pc(),
      tooltip = c("text"), height = 474
    ) %>%
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
