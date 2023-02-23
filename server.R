server <- function(input, output, session) {
  # 1 Set up ----
  ## 1.1 Loading screen ----
  # Call initial loading screen
  hide(
    id = "loading-content",
    anim = TRUE,
    animType = "fade"
  )
  show("app-content")

  ## 1.2 Load chart colours ----
  # https://analysisfunction.civilservice.gov.uk/policy-store/data-visualisation-colours-in-charts/
  # England, geo1, geo2
  chartColors3 <- c("#BFBFBF", "#12436D", "#28A197")
  # geo1, geo2
  chartColors2 <- c("#12436D", "#28A197")
  chartColors6 <-
    c(
      "#BFBFBF",
      "#12436D",
      "#28A197",
      "#801650",
      "#F46A25",
      "#A285D1",
      "#3D3D3D",
      "#2073BC",
      "#6BACE6"
    )
  # for when no England
  chartColors5 <-
    c(
      "#12436D",
      "#28A197",
      "#801650",
      "#F46A25",
      "#A285D1",
      "#3D3D3D",
      "#2073BC",
      "#6BACE6"
    )

  # 2 Main page ----
  ## 2.1 Homepage ----
  ### 2.1.1 Make links ----
  # Create link to overview tab
  observeEvent(input$link_to_tabpanel_overview, {
    updateTabsetPanel(session, "navbar", "Overview")
  })

  # Create link to local skills tab
  observeEvent(input$link_to_tabpanel_localskills, {
    updateTabsetPanel(session, "navbar", "Local skills")
  })
  observeEvent(input$link_to_tabpanel_localskills2, {
    updateTabsetPanel(session, "navbar", "Local skills")
  })

  # Create link to further resources tab
  observeEvent(input$link_to_tabpanel_furtherresources, {
    updateTabsetPanel(session, "navbar", "Further resources")
  })

  # Create link to accessibility tab
  observeEvent(input$link_to_tabpanel_accessibility, {
    updateTabsetPanel(session, "navbar", "Accessibility")
  })

  # Create link to support and feedback tab
  observeEvent(input$link_to_tabpanel_supportandfeedback, {
    updateTabsetPanel(session, "navbar", "Support and feedback")
  })

  # Create link to employment data
  observeEvent(input$link_to_tabpanel_employment, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateSelectInput(session, "splashMetric",
      selected = "empRate"
    )
  })
  # Create link to job advert
  observeEvent(input$link_to_tabpanel_vacancies, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateSelectInput(session, "splashMetric",
      selected = "vacancies"
    )
  })
  # Create link to skills data tab
  observeEvent(input$link_to_tabpanel_FE, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateSelectInput(session, "splashMetric",
      selected = "achievements_rate_per_100000_population"
    )
  })
  # Create link to enterprises
  observeEvent(input$link_to_tabpanel_enterprise, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateSelectInput(session, "splashMetric",
      selected = "enterpriseCount"
    )
  })
  # Create link to qualification
  observeEvent(input$link_to_tabpanel_qualification, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateSelectInput(session, "splashMetric",
      selected = "level3AndAboveRate"
    )
  })
  # Create link to destinations
  observeEvent(input$link_to_tabpanel_destinations, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateSelectInput(session, "splashMetric",
      selected = "sustainedPositiveDestinationKS4Rate"
    )
  })
  # Create link to data tab
  observeEvent(input$link_to_tabpanel_data, {
    updateTabsetPanel(session, "navbar", "Data sources")
  })
  ## 2.5 Data information ----
  ### 2.5.1 Data table downloads ----
  output$downloadData1 <- downloadHandler(
    filename = function() {
      "EmploymentRateIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list("1b.Emp rate" = D_EmpRate_APS1822), path = file)
    }
  )
  output$downloadData2 <- downloadHandler(
    filename = function() {
      "EmploymentByOccupationIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list("1a.Emp by occupation" = D_EmpOcc_APS1721), path = file)
    }
  )
  output$downloadData3 <- downloadHandler(
    filename = function() {
      "EmpbyindustryIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list("1c.Emp by industry" = D_EmpInd_APS1822), path = file)
    }
  )
  output$downloadData4 <- downloadHandler(
    filename = function() {
      "AchievementIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list("3b.FE achievements" = D_Achieve_ILR1621), path = file)
    }
  )
  output$downloadData5 <- downloadHandler(
    filename = function() {
      "AchievementBySSAIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list("3a.FE achievements SSA" = D_Achieve_ILR21), path = file)
    }
  )
  output$downloadData6 <- downloadHandler(
    filename = function() {
      "Qualificationbyageandgendernvq.xlsx"
    },
    content = function(file) {
      write_xlsx(list("4a.Qualification by age and gender" = D_qual_APS1721),
        path = file
      )
    }
  )
  output$downloadData7 <- downloadHandler(
    filename = function() {
      "EnterprisebyemploymentsizeIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list("5a.Enterprise by emp size" = D_empent_UBC1822), path = file)
    }
  )
  output$downloadData8 <- downloadHandler(
    filename = function() {
      "EntbyempsizeandindustryIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list("6a.Ent by emp size and ind" = D_empentind_UBC1822),
        path = file
      )
    }
  )
  output$downloadData9 <- downloadHandler(
    filename = function() {
      "Enterprisedemography.xlsx"
    },
    content = function(file) {
      write_xlsx(list("7a.Enterprise demography" = D_enterprise_demo1621),
        path = file
      )
    }
  )
  output$downloadData10 <- downloadHandler(
    filename = function() {
      "EnterprisebyemploymentsizeIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list("8a.Key Stage 4 destinations" = D_KS4destin_1521),
        path = file
      )
    }
  )
  output$downloadData11 <- downloadHandler(
    filename = function() {
      "EntbyempsizeandindustryIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list("9a.Key Stage 5 destinations" = D_KS5destin_1721),
        path = file
      )
    }
  )
  output$downloadData12 <- downloadHandler(
    filename = function() {
      "JobAdvertIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(
        list(
          "2a.Adverts over time" = D_OnsProfTime,
          "2b.Adverts by detailed profession" = D_OnsProfDetail
        ),
        path = file
      )
    }
  )

  ### 2.5.2 Create download links ----
  output$hidden_downloads <- renderUI(lapply(1:12, function(i) {
    downloadLink(paste0("downloadData", i), "download", class = "hiddenLink")
  }))
  ### 2.5.3 Data table ----
  output$DataTbl <- renderDataTable({
    DT::datatable(
      I_DataTable %>%
        mutate("Dashboard data" = lapply(
          1:n(),
          function(i) {
            paste0(
              '<a onClick=document.getElementById("downloadData',
              i,
              '").click() >Download</a>'
            )
          }
        )),
      escape = FALSE,
      options = list(dom = "t", "pageLength" = 15),
      rownames = FALSE
    )
  })

  ## 2.2 Overview ----

  # define page title
  output$page0title <- renderUI({
    paste0("Overview of local landscape in ", input$geoChoiceOver)
  })

  ### 2.2.1 Filters ----
  # alter area dropdown depending if lep or lsip
  output$geoChoiceOver <- renderUI({
    selectizeInput(
      "geoChoiceOver",
      multiple = FALSE,
      label = NULL,
      choices = areaChoices[1:3]
    )
  })
  observeEvent(input$geoChoice, {
    updateSelectInput(session, "geoChoiceOver",
      selected = input$geoChoice
    )
  })
  # output$lep1_geo <- renderUI({
  #   if (input$GeoType == "LEP") {
  #     selectInput(
  #       "lep1",
  #       "Choose primary LEP area",
  #       choices = C_LEP2020 %>% filter(geographic_level == "LEP") %>% select(Area),
  #       selected = input$lep1
  #     )
  #   } else if (input$GeoType == "LSIP") {
  #     selectInput(
  #       "lep1",
  #       "Choose primary LSIP area",
  #       choices = C_LEP2020 %>% filter(geographic_level == "LSIP") %>% select(Area),
  #       selected = input$lep1
  #     )
  #   } else {
  #     selectInput(
  #       "lep1",
  #       "Choose primary MCA area",
  #       choices = C_LEP2020 %>% filter(geographic_level == "MCA") %>% select(Area),
  #       selected = input$lep1
  #     )
  #   }
  # })
  # observeEvent(input$geoChoice, {
  #   print(sub(" LEP| LSIP| MCA", "", input$geoChoice))
  #   updateSelectInput(session, "lep1",
  #                        selected = sub(" LEP| LSIP| MCA", "", input$geoChoice)
  #   )
  # })
  # observeEvent(input$geoChoice, {
  #   print(gsub(" ", "",str_sub(input$geoChoice,-4,-1)))
  #   updateSelectInput(session, "GeoType",
  #                        selected = gsub(" ", "",str_sub(input$geoChoice,-4,-1))
  #   )
  # })

  ### 2.2.2 Screenshot----
  output$screenshotOverview <- renderUI({
    capture::capture(
      selector = "body",
      filename = paste0(input$geoChoiceOver, "-overview", ".png"),
      icon("camera"),
      "Screenshot"
    )
  })

  ###  2.2.3 Downloads ----
  # download all indicators
  list_of_datasets0 <- list(
    "1a.Emp by occupation" = D_EmpOcc_APS1721,
    "1b.Emp rate" = D_EmpRate_APS1822,
    "1c.Emp by industry" = D_EmpInd_APS1822,
    "2a.Adverts over time" = D_OnsProfTime,
    "2b.Adverts by detailed profession" = D_OnsProfDetail,
    "3a.FE achievements SSA" = D_Achieve_ILR21,
    "3b.FE achievements" = D_Achieve_ILR1621,
    "4a.FE achievements" = D_qual_APS1721,
    "5a.Qual by age and gender" = D_qual_APS1721,
    "6a.Ent by emp size band" = D_empent_UBC1822,
    "7a.Ent by emp size & industry" = D_empentind_UBC1822,
    "8a.Enterprise demography" = D_enterprise_demo1621,
    "9a.Key Stage 4 destinations" = D_KS4destin_1521,
    "10a.Key Stage 5 destinations" = D_KS5destin_1721
  )
  output$download_btn0a <- downloadHandler(
    filename = function() {
      "CoreIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list_of_datasets0, path = file)
    }
  )

  # Download current area indicators
  filtered_data0 <- reactive({
    list(
      "1a.Emp by occupation" = filter(
        D_EmpOcc_APS1721,
        geogConcat == input$geoChoiceOver
      ),
      "1b.Emp rate" = filter(
        D_EmpRate_APS1822,
        geogConcat == input$geoChoiceOver
      ),
      "2a.Adverts over time" = filter(
        D_OnsProfTime,
        geogConcat == input$geoChoiceOver
      ),
      "2b.Adverts by detailed profession" = filter(
        D_OnsProfDetail,
        geogConcat == input$geoChoiceOver
      ),
      "3a.FE achievements SSA" = filter(
        D_Achieve_ILR21,
        geogConcat == input$geoChoiceOver
      ),
      "3b.FE achievements" = filter(
        D_Achieve_ILR1621,
        geogConcat == input$geoChoiceOver
      ),
      "4a.Ent by emp size band" = filter(
        D_empent_UBC1822,
        geogConcat == input$geoChoiceOver
      ),
      "5a.Key Stage 5 destinations" = filter(
        D_KS5destin_1721,
        geogConcat == input$geoChoiceOver
      ),
      "6a.Qual by age and gender" = filter(
        D_qual_APS1721,
        geogConcat == input$geoChoiceOver
      ),
      "7a.Ent by emp size & industry" = filter(
        D_empentind_UBC1822,
        geogConcat == input$geoChoiceOver
      ),
      "8a.Enterprise demography" = filter(
        D_enterprise_demo1621,
        geogConcat == input$geoChoiceOver
      ),
      "9a.Key Stage 4 destinations" = filter(
        D_KS4destin_1521,
        geogConcat == input$geoChoiceOver
      ),
      "10a.Key Stage 5 destinations" = filter(
        D_KS5destin_1721,
        geogConcat == input$geoChoiceOver
      )
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

  ### 2.2.3 KPIs and charts----
  # currentOverview<-eventReactive(input$lep1,input$GeoType {
  #   C_time%>%
  #   filter(area == input$lep1, geographic_level == input$GeoType)
  # })

  currentGeogData <- eventReactive(input$geoChoiceOver, {
    C_time %>%
      filter(geogConcat == input$geoChoiceOver)
  })
  englandData <- C_time %>%
    filter(geogConcat == "England")

  #### 2.2.3.1 Employment count ----
  # Employment count
  output$overviewEmpCntKPI <- renderUI({
    latest <- (currentGeogData() %>% filter(time_period == 2022, metric == "Employment"))$value
    change <- latest - (currentGeogData() %>% filter(time_period == 2021, metric == "Employment"))$value

    # print with formatting
    h4(
      span("Oct-Sep 2022", style = "font-size: 16px;font-weight:normal;"),
      br(),
      format(latest, big.mark = ","),
      br(),
      span(
        format_pm(change), # plus-minus and comma sep formatting
        style = paste0("font-size: 16px;color:", cond_color(change > 0)), # colour formating
        .noWS = c("before", "after") # remove whitespace
      ),
      br(),
      style = "font-size: 21px"
    )
  })

  # Emp chart
  empLineChart <- eventReactive(input$geoChoiceOver, {
    change <- (currentGeogData() %>% filter(time_period == 2022, metric == "Employment"))$value -
      (currentGeogData() %>% filter(time_period == 2021, metric == "Employment"))$value
    line <- currentGeogData() %>%
      filter(metric == "Employment") %>%
      mutate(Year = as.numeric(str_sub(time_period, -2, -1)))

    ggplot(
      line,
      aes(
        x = Year - 1,
        y = value,
        group = area,
        text = paste0(
          "Year: Oct-Sep ",
          Year,
          "<br>",
          "Employment: ",
          format(value, big.mark = ","),
          "<br>"
        )
      )
    ) +
      geom_line(data = line %>% filter(Year <= 21)) +
      geom_ribbon(
        data = line %>% filter(Year >= 21),
        aes(ymin = min(value), ymax = value),
        fill = ifelse(change > 0, "#00703c", "#d4351c"),
        alpha = 0.3
      ) +
      geom_line(
        data = line %>% filter(Year >= 21),
        color = ifelse(change > 0, "#00703c", "#d4351c")
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
        breaks = c(min(line$value), max(line$value))
      )
  })
  # set margins
  m <- list(
    l = 0,
    r = 4,
    # increase this margin a bit to prevent the last lable dissapearing
    b = 0,
    t = 0,
    pad = 0
  )

  output$empLineChart <- renderPlotly({
    validate(need(input$geoChoiceOver != "", "")) # if area not yet loaded don't try to load ch)
    ggplotly(empLineChart(),
      tooltip = "text",
      height = 81
    ) %>%
      layout(
        margin = m,
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })

  #### 2.2.3.2 Employment rate ----
  output$overviewEmpRateKPI <- renderUI({
    latest <- (currentGeogData() %>% filter(time_period == 2022, metric == "empRate"))$value
    change <- latest - (currentGeogData() %>% filter(time_period == 2021, metric == "empRate"))$value

    # print with formatting
    h4(
      span("Oct-Sep 2022", style = "font-size: 16px;font-weight:normal;"),
      br(),
      paste0(format(100 * latest, digit = 2), "%"),
      br(),
      span(
        paste0(sprintf("%+.0f", 100 * change), "ppts"),
        style = paste0("font-size: 16px;color:", cond_color(change > 0)), # colour formating
        .noWS = c("before", "after") # remove whitespace
      ),
      br(),
      style = "font-size: 21px"
    )
  })

  # Emp chart
  # find emp chart y axis min and max
  EmpRateMin <- C_time %>%
    filter(metric == "empRate", geographic_level != "LADU") %>%
    summarise(min(value, na.rm = T), .groups = "drop")
  EmpRateMax <- C_time %>%
    filter(metric == "empRate", geographic_level != "LADU") %>%
    summarise(max(value, na.rm = T), .groups = "drop")

  empRateLineChart <- eventReactive(input$geoChoiceOver, {
    change <- (currentGeogData() %>% filter(time_period == 2022, metric == "empRate"))$value -
      (currentGeogData() %>% filter(time_period == 2021, metric == "empRate"))$value
    line <- bind_rows(currentGeogData(), englandData) %>%
      filter(metric == "empRate") %>%
      mutate(Year = as.numeric(str_sub(time_period, -2, -1)))

    ggplot(
      line,
      aes(
        x = Year - 1,
        y = value,
        group = area,
        text = paste0(
          "Year: Oct-Sep ",
          Year,
          "<br>",
          "Employment rate: ",
          scales::percent(round(value, 2)),
          "<br>"
        )
      )
    ) +
      geom_line(data = line %>% filter(Year <= 21, geogConcat == input$geoChoiceOver)) +
      geom_line(
        data = line %>% filter(geogConcat == "England"),
        alpha = 0.5
      ) +
      geom_ribbon(
        data = line %>% filter(Year >= 21, geogConcat == input$geoChoiceOver),
        aes(ymin = min(value), ymax = value),
        fill = ifelse(change > 0, "#00703c", "#d4351c"),
        alpha = 0.3
      ) +
      geom_line(
        data = line %>% filter(Year >= 21, geogConcat == input$geoChoiceOver),
        color = ifelse(change > 0, "#00703c", "#d4351c")
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

  output$empRateLineChart <- renderPlotly({
    ggplotly(empRateLineChart(),
      tooltip = "text",
      height = 81
    ) %>%
      layout(
        margin = m,
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })

  # Add link to employment data
  observeEvent(input$link_to_tabpanel_employment2, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateSelectInput(session, "splashMetric",
      selected = "Employment"
    )
  })
  # Add link to employment rate
  observeEvent(input$link_to_tabpanel_empRate, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateSelectInput(session, "splashMetric",
      selected = "empRate"
    )
  })

  #### 2.2.3.3 Job adverts ----
  # Vacancy kpi
  output$overviewJobKPI <- renderUI({
    latest <- (currentGeogData() %>% filter(time_period == "2022-12-01", metric == "vacancies"))$value
    change <- latest - (currentGeogData() %>% filter(time_period == "2021-12-01", metric == "vacancies"))$value

    # print with formatting
    h4(
      span("Dec 2022", style = "font-size: 16px;font-weight:normal;"),
      br(),
      format(latest, big.mark = ","),
      br(),
      span(
        format_pm(change), # plus-minus and comma sep formatting
        style = paste0("font-size: 16px;color:", cond_color(change > 0)), # colour formating
        .noWS = c("before", "after") # remove whitespace
      ),
      br(),
      style = "font-size: 21px"
    )
  })

  # Vacancy chart
  jobLineChart <- eventReactive(input$geoChoiceOver, {
    change <- (currentGeogData() %>% filter(time_period == "2022-12-01", metric == "vacancies"))$value -
      (currentGeogData() %>% filter(time_period == "2021-12-01", metric == "vacancies"))$value
    line <- currentGeogData() %>%
      filter(metric == "vacancies") %>%
      mutate(Year = as.numeric(str_sub(time_period, -2, -1)))

    ggplot(
      line,
      aes(
        x = as.Date(time_period),
        y = value,
        group = area,
        text = paste0(
          "Period: ",
          format(as.Date(time_period), "%b %y"),
          "<br>",
          "Online job adverts: ",
          format(value, big.mark = ","),
          "<br>"
        )
      )
    ) +
      geom_line(data = line %>% filter(time_period <= as.Date("2021-12-01"))) +
      geom_ribbon(
        data = line %>% filter(time_period >= as.Date("2021-12-01")),
        aes(ymin = min(value), ymax = value),
        fill = ifelse(change > 0, "#00703c", "#d4351c"),
        alpha = 0.3
      ) +
      geom_line(
        data = line %>% filter(time_period >= as.Date("2021-12-01")),
        color = ifelse(change > 0, "#00703c", "#d4351c")
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
        breaks = c(min(line$value), max(line$value))
      ) +
      scale_x_date(
        name = "My date axis title",
        date_breaks = "1 years",
        date_labels = "%y"
      )
  })

  output$jobLineChart <- renderPlotly({
    ggplotly(jobLineChart(),
      tooltip = "text",
      height = 81
    ) %>%
      layout(
        margin = m,
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })

  # Add link to vacancy data
  observeEvent(input$link_to_tabpanel_vacancies2, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateSelectInput(session, "splashMetric",
      selected = "vacancies"
    )
  })

  #### 2.2.3.4 FE achieve ----

  # get EandT data for current area
  EtLEP <- eventReactive(input$geoChoiceOver, {
    C_Achieve_ILR1621 %>%
      mutate(geogConcat = paste0(area, " ", geographic_level)) %>%
      filter(
        geogConcat == input$geoChoiceOver,
        level_or_type == "Education and training: Total",
        age_group == "Total"
      )
  })
  # get 21/22 values
  EtLatest <- reactive({
    EtLEP() %>%
      filter(time_period == "202122")
  })
  # get 20/21 values
  EtLast <- reactive({
    EtLEP() %>%
      filter(time_period == "202021")
  })

  output$skisup.ETach <- renderUI({
    ETach <- EtLatest()$achievements

    # E&T achievements change
    ETachChange <- EtLatest()$achievements - EtLast()$achievements

    # print with formatting
    h4(
      span("2021/22", style = "font-size: 16px;font-weight:normal;"),
      br(),
      format(ETach, big.mark = ","),
      br(),
      span(
        format_pm(ETachChange) # plus-minus and comma sep formatting
        ,
        style = paste0("font-size: 16px;color:", cond_color(ETachChange > 0)) # colour formating
        ,
        .noWS = c("before", "after") # remove whitespace
      ),
      br(),
      style = "font-size: 21px"
    )
  })



  # e and t chart
  etLineChart <- eventReactive(input$geoChoiceOver, {
    etLine <- EtLEP()
    etCntChange <- EtLatest()$achievements - EtLast()$achievements
    EtMinMax <- C_Achieve_ILR1621_max_min %>%
      mutate(geogConcat = paste0(area, " ", geographic_level)) %>%
      filter(
        geogConcat == input$geoChoiceOver,
        level_or_type == "Education and training: Total"
      )

    ggplot(
      etLine,
      aes(
        x = Year,
        y = achievements,
        group = area,
        text = paste0(
          "Academic year: ",
          time_period,
          "<br>",
          "Achievements: ",
          format(achievements, big.mark = ","),
          "<br>"
        )
      )
    ) +
      geom_line(data = etLine %>% filter(Year <= 20 & Year >= 17)) +
      geom_ribbon(
        data = etLine %>% filter(Year >= 20),
        aes(ymin = min(achievements), ymax = achievements),
        fill = ifelse(etCntChange > 0, "#00703c", "#d4351c"),
        alpha = 0.3
      ) +
      geom_line(
        data = etLine %>% filter(Year >= 20),
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

  output$etLineChart <- renderPlotly({
    ggplotly(etLineChart(),
      tooltip = "text",
      height = 81
    ) %>%
      layout(
        margin = m,
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })

  #### 2.2.3.5 FE app achieve ----
  # get App data for current area
  AppLEP <- eventReactive(input$geoChoiceOver, {
    C_Achieve_ILR1621 %>%
      mutate(geogConcat = paste0(area, " ", geographic_level)) %>%
      filter(
        geogConcat == input$geoChoiceOver,
        level_or_type == "Apprenticeships: Total",
        age_group == "Total"
      )
  })
  # get 21/22 values
  AppLatest <- reactive({
    AppLEP() %>%
      filter(time_period == "202122")
  })
  # get 20/21 values
  AppLast <- reactive({
    AppLEP() %>%
      filter(time_period == "202021")
  })
  output$skisup.APPach <- renderUI({
    Appach <- AppLatest()$achievements

    # E&T achievements change
    AppachChange <-
      AppLatest()$achievements - AppLast()$achievements

    # print with formatting
    h4(
      span("2021/22", style = "font-size: 16px;font-weight:normal;"),
      br(),
      format(Appach, big.mark = ","),
      br(),
      span(
        format_pm(AppachChange) # plus-minus and comma sep formatting
        ,
        style = paste0("font-size: 16px;color:", cond_color(AppachChange > 0)) # colour formating
        ,
        .noWS = c("before", "after") # remove whitespace
      ),
      br(),
      style = "font-size: 21px"
    )
  })

  # app chart
  AppLineChart <- eventReactive(input$geoChoiceOver, {
    AppLine <- AppLEP()
    AppCntChange <-
      AppLatest()$achievements - AppLast()$achievements
    AppMinMax <- C_Achieve_ILR1621_max_min %>%
      mutate(geogConcat = paste0(area, " ", geographic_level)) %>%
      filter(
        geogConcat == input$geoChoiceOver,
        level_or_type == "Apprenticeships: Total"
      )


    ggplot(
      AppLine,
      aes(
        x = Year,
        y = achievements,
        group = area,
        text = paste0(
          "Academic year: ",
          time_period,
          "<br>",
          "Achievements: ",
          format(achievements, big.mark = ","),
          "<br>"
        )
      )
    ) +
      geom_line(data = AppLine %>% filter(Year <= 20 &
        Year >= 17)) +
      geom_ribbon(
        data = AppLine %>% filter(Year >= 20),
        aes(ymin = min(achievements), ymax = achievements),
        fill = ifelse(AppCntChange > 0, "#00703c", "#d4351c"),
        alpha = 0.3
      ) +
      geom_line(
        data = AppLine %>% filter(Year >= 20),
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

  output$AppLineChart <- renderPlotly({
    ggplotly(AppLineChart(),
      tooltip = "text",
      height = 81
    ) %>%
      layout(
        margin = m,
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })

  # Add link to skills data
  observeEvent(input$link_to_tabpanel_FE2, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateSelectInput(session, "splashMetric",
      selected = "achievements_rate_per_100000_population"
    )
  })

  #### 2.2.3.6 KS5 sustained positive destination rate ----
  # get dest data for current area
  KS5geo <- eventReactive(input$geoChoiceOver, {
    C_KS4_KS5eduempapp %>%
      mutate(geogConcat = paste0(area, " ", geographic_level)) %>%
      filter(
        geogConcat == input$geoChoiceOver,
        `Cohort Group` == "Total",
        `Key Stage` == "Key Stage 5"
      )
  })
  # get 21/22 values
  KS5Latest <- reactive({
    KS5geo() %>%
      filter(time_period == "202021")
  })
  # get 20/21 values
  KS5Last <- reactive({
    KS5geo() %>%
      filter(time_period == "201920")
  })

  # destinations overview KPI
  output$dest.ks5over <- renderUI({
    # change in positive sustained rate
    KS5sustChange <- KS5Latest()$rate - KS5Last()$rate

    # print with formatting
    h4(
      span("2020/21", style = "font-size: 16px;font-weight:normal;"),
      br(),
      paste0(format(100 * KS5Latest()$rate, digit = 2), "%"),
      br(),
      span(
        paste0(sprintf("%+.1f", 100 * KS5sustChange), "ppts"),
        style = paste0("font-size: 16px;color:", cond_color(KS5sustChange > 0)) # colour formating
        ,
        .noWS = c("before", "after") # remove whitespace
      ),
      br(),
      style = "font-size: 21px"
    )
  })

  # KS5 destinations chart
  KS5LineChart <- eventReactive(input$geoChoiceOver, {
    KS5Line <- C_KS4_KS5eduempapp %>%
      mutate(geogConcat = paste0(area, " ", geographic_level)) %>%
      filter(
        (geogConcat == input$geoChoiceOver |
          geogConcat == "England COUNTRY"),
        `Cohort Group` == "Total",
        `Key Stage` == "Key Stage 5"
      )
    KS5sustChange <- KS5Latest()$rate - KS5Last()$rate
    KS5MinMax <- C_KS5_eduempapp_max_min

    ggplot(KS5Line, aes(
      x = Year,
      y = rate,
      group = area,
      text = paste0(
        "Academic year: ",
        time_period,
        "<br>",
        "Area: ",
        area,
        "<br>",
        "Sustained positive destination rate: ",
        scales::percent(round(rate, 2)),
        "<br>"
      )
    )) +
      geom_line(data = KS5Line %>% filter(Year <= 19, geogConcat == input$geoChoiceOver)) +
      geom_line(
        data = KS5Line %>% filter(geogConcat == "England COUNTRY"),
        alpha = 0.5
      ) +
      geom_ribbon(
        data = KS5Line %>% filter(Year >= 19, geogConcat == input$geoChoiceOver),
        aes(ymin = min(rate), ymax = rate),
        fill = ifelse(KS5sustChange > 0, "#00703c", "#d4351c"),
        alpha = 0.3
      ) +
      geom_line(
        data = KS5Line %>% filter(Year >= 19, geogConcat == input$geoChoiceOver),
        color = ifelse(KS5sustChange > 0, "#00703c", "#d4351c")
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
        labels = scales::percent_format(accuracy = 1),
        breaks = c(KS5MinMax$minks5, KS5MinMax$maxks5),
        limits = c(KS5MinMax$minks5, KS5MinMax$maxks5)
      )
  })

  output$KS5LineChart <- renderPlotly({
    ggplotly(KS5LineChart(),
      tooltip = "text",
      height = 81
    ) %>%
      layout(
        margin = m,
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })

  # add link to destinations
  observeEvent(input$link_to_tabpanel_destinations2, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateSelectInput(session, "splashMetric",
      selected = "sustainedPositiveDestinationKS4Rate"
    )
  })

  #### 2.2.3.7 Micro enterprise ----
  # get Enterprise data for current area
  Entgeo <- eventReactive(input$geoChoiceOver, {
    C_empentind3_UBC1822 %>%
      mutate(geogConcat = paste0(area, " ", geographic_level)) %>%
      filter(
        geogConcat == input$geoChoiceOver,
        variable == "Micro 0 to 9",
        industry == "Total"
      )
  })
  # get 21/22 values
  EntLatest <- reactive({
    Entgeo() %>%
      filter(year == "2022")
  })
  # get 20/21 values
  EntLast <- reactive({
    Entgeo() %>%
      filter(year == "2021")
  })

  # enterprise overview KPI
  output$UBC.micro <- renderUI({
    # change in micro enterprises
    EntChange <- EntLatest()$rate - EntLast()$rate

    # print with formatting
    h4(
      span("Mar 2022", style = "font-size: 16px;font-weight:normal;"),
      br(),
      paste0(format(100 * EntLatest()$rate, digit = 2), "%"),
      br(),
      span(
        paste0(sprintf("%+.1f", 100 * EntChange), "ppts"),
        style = paste0("font-size: 16px;color:", cond_color(EntChange > 0)) # colour formating
        ,
        .noWS = c("before", "after") # remove whitespace
      ),
      br(),
      style = "font-size: 21px"
    )
  })

  # micro enterprise chart
  UBCLineChart <- eventReactive(input$geoChoiceOver, {
    EntLine <- C_empentind3_UBC1822 %>%
      mutate(geogConcat = paste0(area, " ", geographic_level)) %>%
      filter(
        (geogConcat == input$geoChoiceOver |
          geogConcat == "England COUNTRY"),
        variable == "Micro 0 to 9",
        industry == "Total"
      )
    EntChange <- EntLatest()$rate - EntLast()$rate
    EntMinMax <- C_empentind_max_min

    ggplot(EntLine, aes(
      x = Year - 1,
      y = rate,
      group = area,
      text = paste0(
        "March: ",
        year,
        "<br>",
        "Area: ",
        area,
        "<br>",
        "Percentage: ",
        scales::percent(round(rate, 2)),
        "<br>"
      )
    )) +
      geom_line(data = EntLine %>% filter(Year <= 21, geogConcat == input$geoChoiceOver)) +
      geom_line(
        data = EntLine %>% filter(geogConcat == "England COUNTRY"),
        alpha = 0.5
      ) +
      geom_ribbon(
        data = EntLine %>% filter(Year >= 21, geogConcat == input$geoChoiceOver),
        aes(ymin = min(rate), ymax = rate),
        fill = ifelse(EntChange > 0, "#00703c", "#d4351c"),
        alpha = 0.3
      ) +
      geom_line(
        data = EntLine %>% filter(Year >= 21, geogConcat == input$geoChoiceOver),
        color = ifelse(EntChange > 0, "#00703c", "#d4351c")
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
        labels = scales::percent_format(accuracy = 1),
        breaks = c(EntMinMax$minmic, EntMinMax$maxmic),
        limits = c(EntMinMax$minmic, EntMinMax$maxmic)
      )
  })

  output$UBCLineChart <- renderPlotly({
    ggplotly(UBCLineChart(),
      tooltip = "text",
      height = 81
    ) %>%
      layout(
        margin = m,
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })

  # add link to enterprise
  observeEvent(input$link_to_tabpanel_enterprise2, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateSelectInput(session, "splashMetric",
      selected = "enterpriseCount"
    )
  })

  #### 2.2.3.8 Qualifications NVQ ----
  # get entprise data for current area
  Qualgeo <- eventReactive(input$geoChoiceOver, {
    C_qualevel3plus_APS1721 %>%
      mutate(geogConcat = paste0(area, " ", geographic_level)) %>%
      filter(
        geogConcat == input$geoChoiceOver,
        Level == "Level 3 and above",
        age_band == "16-64",
        gender == "Total"
      )
  })
  # get 21/22 values
  QualLatest <- reactive({
    Qualgeo() %>%
      filter(year == "2021")
  })
  # get 20/21 values
  QualLast <- reactive({
    Qualgeo() %>%
      filter(year == "2020")
  })

  # NVQ3 or above overview KPI
  output$APS.nvq3plus <- renderUI({
    # change in NVQ3 or above
    QualChange <- QualLatest()$rate - QualLast()$rate

    # print with formatting
    h4(
      span("Jan-Dec 2021", style = "font-size: 16px;font-weight:normal;"),
      br(),
      paste0(format(100 * QualLatest()$rate, digit = 2), "%"),
      br(),
      span(
        paste0(sprintf("%+.1f", 100 * QualChange), "ppts"),
        style = paste0("font-size: 16px;color:", cond_color(QualChange > 0)) # colour formating
        ,
        .noWS = c("before", "after") # remove whitespace
      ),
      br(),
      style = "font-size: 21px"
    )
  })

  # qualification chart
  Nvq3plusLineChart <- eventReactive(input$geoChoiceOver, {
    QualLine <- C_qualevel3plus_APS1721 %>%
      mutate(geogConcat = paste0(area, " ", geographic_level)) %>%
      filter(
        (geogConcat == input$geoChoiceOver |
          geogConcat == "England COUNTRY"),
        Level == "Level 3 and above",
        age_band == "16-64",
        gender == "Total"
      )
    QualChange <- QualLatest()$rate - QualLast()$rate
    # QualMinMax <- C_qual_max_min

    ggplot(QualLine, aes(
      x = Year,
      y = rate,
      group = area,
      text = paste0(
        "Jan-Dec: ",
        year,
        "<br>",
        "Area: ",
        area,
        "<br>",
        "Percentage: ",
        scales::percent(round(rate, 2)),
        "<br>"
      )
    )) +
      geom_line(data = QualLine %>% filter(Year <= 20, geogConcat == input$geoChoiceOver)) +
      geom_line(
        data = QualLine %>% filter(geogConcat == "England COUNTRY"),
        alpha = 0.5
      ) +
      geom_ribbon(
        data = QualLine %>% filter(Year >= 20, geogConcat == input$geoChoiceOver),
        aes(ymin = min(rate), ymax = rate),
        fill = ifelse(QualChange > 0, "#00703c", "#d4351c"),
        alpha = 0.3
      ) +
      geom_line(
        data = QualLine %>% filter(Year >= 20, geogConcat == input$geoChoiceOver),
        color = ifelse(QualChange > 0, "#00703c", "#d4351c")
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
        labels = scales::percent_format(accuracy = 1),
        breaks = c(.39, .72),
        limits = c(.39, .72)
      )
  })

  output$Nvq3plusLineChart <- renderPlotly({
    ggplotly(Nvq3plusLineChart(),
      tooltip = "text",
      height = 81
    ) %>%
      layout(
        margin = m,
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })

  # add link to qualification level
  observeEvent(input$link_to_tabpanel_qualification2, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateSelectInput(session, "splashMetric",
      selected = "level3AndAboveRate"
    )
  })

  ## 2.3 Local skills----

  ### 2.3.2 Reusable variables----
  # get current metric in plain English
  currentMetric <- reactive({
    sub("fe", "FE", tolower(gsub("^.*\\.", "", names(
      unlist(metricChoices)[unlist(metricChoices) == input$splashMetric]
    ))))
  })
  # get current area name
  # areaClicked <- reactive({
  #   if ("map_shape_click" %in% names(input) &&
  #     (nrow(C_LEP2020 %>%
  #       filter(
  #         geographic_level == input$splashGeoType,
  #         Area == C_Geog$areaName[C_Geog$areaCode == input$map_shape_click$id]
  #       ))) != 0) {
  #     event <- input$map_shape_click
  #   } else {
  #     event <- data.frame(id = c("E37000025"))
  #   }
  #   C_Geog$areaName[C_Geog$areaCode == event$id]
  # })

  # areaClicked <- reactive({
  #   input$lep1
  # })



  # Update overview to match map
  # observe({
  #   updateSelectInput(session, "lep1",
  #     selected = areaClicked()
  #   )
  #   updateSelectInput(session, "GeoType",
  #     selected = input$splashGeoType
  #   )
  # })
  # observeEvent(input$lep1, {
  #   print(input$lep1)
  #   print(areaClicked())
  #   if(is.null(areaClicked())==FALSE){
  #   areaClicked()<-input$lep1}else{}
  # })

  # get current LA
  laClicked <- reactive({
    eventLA <- input$mapLA_shape_click
    C_Geog$areaName[C_Geog$areaCode == eventLA$id]
  })
  # ranking for each geog
  geogRank <- reactive({
    C_Geog %>%
      filter(geog == input$splashGeoType) %>%
      mutate(ranking = rank(desc(eval(
        parse(text = input$splashMetric)
      )), ties.method = c("first")))
  })
  # count of areas
  groupCount <- reactive({
    if (input$splashGeoType == "LEP") {
      "38 LEPs."
    } else {
      if (input$splashGeoType == "MCA") {
        "10 MCAs."
      } else {
        "38 LSIPs."
      }
    }
  })
  # filter for just england
  englandGeog <- C_Geog %>%
    filter(geog == "COUNTRY" & areaName == "England")

  ### 2.3.3 Screenshot----
  output$screenshotFile <- renderUI({
    capture::capture(
      selector = "body",
      filename = paste0(input$geoChoice, "-", input$splashMetric, ".png"),
      icon("camera"),
      "Screenshot"
    )
  })

  # create subheading
  output$subheading <- renderUI({
    (I_DataText %>% filter(metric == input$splashMetric))$subheading
  })

  ### 2.3.4 Data note----
  # create data source
  output$dataSource <- renderUI({
    HTML(paste0("<p>Source: ", (
      I_DataText %>% filter(metric == input$splashMetric)
    )$sourceText, "<p>"))
  })
  # create data note
  output$dataNote <- renderUI({
    (I_DataText %>% filter(metric == input$splashMetric))$dataText
  })
  # create data caveat
  output$dataCaveat <- renderUI({
    HTML((I_DataText %>% filter(metric == input$splashMetric))$caveatText)
  })

  ### 2.3.5 Comparison filter----
  # geogLookup <- C_Geog %>%
  #   st_drop_geometry() %>%
  #   distinct(areaName, geog) %>%
  #   mutate(concatGeog = paste0(areaName, geog))
  output$geoComp <- renderUI({
    selectizeInput(
      "geoComps",
      multiple = TRUE,
      label = NULL,
      choices = areaChoices,
      options = list(maxItems = 7, placeholder = "Choose comparison areas")
    )
  })

  observeEvent(input$mapLA_shape_click, {
    updateSelectizeInput(session, "geoComps",
      selected = c(input$geoComps, paste0(laClicked(), " LADU")), options = list()
    )
  })

  ### 2.3.5 National map ----
  #### 2.3.5.1 Dropdown area select----
  output$geoChoice <- renderUI({
    selectizeInput(
      "geoChoice",
      multiple = FALSE,
      label = NULL,
      choices = areaChoices[1:3],
      selected = input$geoChoiceOver
    )
  })

  observeEvent(input$map_shape_click, {
    updateSelectizeInput(session, "geoChoice",
      selected = C_Geog$geogConcat[C_Geog$areaCode == input$map_shape_click$id]
    )
  })

  observeEvent(input$geoChoiceOver, {
    updateSelectizeInput(session, "geoChoice",
      selected = input$geoChoiceOver
    )
  })

  observeEvent(input$geoChoice, {
    updateRadioGroupButtons(session, "splashGeoType",
      selected = gsub(" ", "", str_sub(input$geoChoice, -4, -1))
    )
  })
  #### 2.3.5.1 Title ----
  output$titleMap <- renderUI({
    paste0("Where does ", input$geoChoice, " fit in the national picture?")
  })

  #### 2.3.5.2 Comment ----
  output$commentMap <- renderUI({
    validate(need("geoChoice" %in% names(input), ""))
    compareNational <-
      if ((C_Geog %>%
        filter(geogConcat == input$geoChoice))[[input$splashMetric]]
      >
        (englandGeog)[[input$splashMetric]]) {
        "higher"
      } else {
        "lower"
      }
    areaRank <- (geogRank() %>%
      filter(geogConcat == input$geoChoice))$ranking
    suff <- case_when(
      areaRank %in% c(11, 12, 13) ~ "th",
      areaRank %% 10 == 1 ~ "st",
      areaRank %% 10 == 2 ~ "nd",
      areaRank %% 10 == 3 ~ "rd",
      TRUE ~ "th"
    )
    paste0(
      (I_DataText %>% filter(metric == input$splashMetric))$mapComment, " in ",
      input$geoChoice,
      " is ",
      compareNational,
      " than the national average. It is ranked ",
      areaRank,
      suff,
      " of the ",
      groupCount()
    )
  })

  #### 2.3.5.3 Map ----
  output$map <- renderLeaflet({
    mapData <- C_Geog %>% filter(geog == input$splashGeoType)
    pal <- colorNumeric("Blues", mapData[[input$splashMetric]])
    labels <-
      # if a percentage then format as %, else big number
      if (str_sub(input$splashMetric, start = -4) == "Rate") {
        sprintf(
          "<strong>%s</strong><br/>%s: %s%%",
          mapData$areaName,
          (I_DataText %>% filter(metric == input$splashMetric))$mapPop,
          round(mapData[[input$splashMetric]] * 100)
        ) %>% lapply(htmltools::HTML)
      } else {
        sprintf(
          "<strong>%s</strong><br/>%s: %s",
          mapData$areaName,
          (I_DataText %>% filter(metric == input$splashMetric))$mapPop,
          format(round(mapData[[input$splashMetric]]), big.mark = ",")
        ) %>% lapply(htmltools::HTML)
      }

    leaflet(options = leafletOptions(zoomSnap = 0.1)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data = mapData,
        fillColor = ~ pal(mapData[[input$splashMetric]]),
        color = "black",
        layerId = ~areaCode,
        weight = 1,
        highlightOptions = highlightOptions(
          weight = 2,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "12px",
          direction = "auto"
        )
      ) %>%
      setView(
        lng = -1.6,
        lat = 52.8,
        zoom = 5.7
      )
  })
  observe({
    validate(need("geoChoice" %in% names(input), ""))
    mapData <- C_Geog %>% filter(geogConcat == input$geoChoice)
    labels <-
      # if a percentage then format as %, else big number
      if (str_sub(input$splashMetric, start = -4) == "Rate") {
        sprintf(
          "<strong>%s</strong><br/>%s: %s%%",
          mapData$areaName,
          (I_DataText %>% filter(metric == input$splashMetric))$mapPop,
          round(mapData[[input$splashMetric]] * 100)
        ) %>% lapply(htmltools::HTML)
      } else {
        sprintf(
          "<strong>%s</strong><br/>%s: %s",
          mapData$areaName,
          (I_DataText %>% filter(metric == input$splashMetric))$mapPop,
          format(round(mapData[[input$splashMetric]]), big.mark = ",")
        ) %>% lapply(htmltools::HTML)
      }
    proxy <- leafletProxy("map")
    addPopups(
      proxy,
      lng = C_Geog$LONG[C_Geog$geogConcat == input$geoChoice],
      lat = C_Geog$LAT[C_Geog$geogConcat == input$geoChoice],
      popup = labels,
      layerId = "popup",
      options = popupOptions(
        className = "myspecial-popup",
        textsize = "12px",
        direction = "auto",
        closeOnClick = TRUE,
        closeButton = FALSE
      )
    )
  })

  #### 2.3.5.4 Map footnote ----
  output$mapFoot <- renderUI({
    paste0(
      (I_DataText %>% filter(metric == input$splashMetric))$LatestPeriod, " data. Click an area to update dashboard."
    )
  })

  ### 2.3.6 LA map ----
  #### 2.3.6.1 Title ----
  output$titleLaMap <- renderUI({
    paste0("What is the variation within ", input$geoChoice, "?")
  })
  #### 2.3.6.2 Comment----

  output$commentLA <- renderUI({
    validate(need("geoChoice" %in% names(input), ""))
    LaHighLow <- C_Geog %>%
      filter(
        geog == "LADU",
        eval(parse(text = gsub(" ", "", str_sub(input$geoChoice, -4, -1)))) == input$geoChoice
      ) %>%
      mutate(ranking = rank(desc(eval(
        parse(text = input$splashMetric)
      )), ties.method = c("first")))

    LaHigh <- (LaHighLow %>% filter(ranking == 1))$areaName
    LaLow <-
      (LaHighLow %>% filter(ranking == max(ranking)))$areaName
    if (input$geoChoice %in% c("London LEP", "Greater London LSIP") &
      currentMetric() == "online job adverts") {
      "ONS job adverts in London are not broken down by LA."
    } else {
      paste0(
        (I_DataText %>% filter(metric == input$splashMetric))$LaComment,
        " highest in ",
        LaHigh,
        " and lowest in ",
        LaLow,
        "."
      )
    }
  })

  #### 2.3.6.3 Map----
  output$mapLA <- renderLeaflet({
    validate(
      need(!(input$geoChoice %in% c("London LEP", "Greater London LSIP") &
        currentMetric() == "online job adverts"), "")
    )
    # Filter to those LAs in that region
    mapData <- C_Geog %>%
      filter(
        geog == "LADU",
        eval(parse(text = gsub(" ", "", str_sub(input$geoChoice, -4, -1)))) == input$geoChoice
      )
    pal <- colorNumeric("Blues", mapData[[input$splashMetric]])

    labels <-
      if (str_sub(input$splashMetric, start = -4) == "Rate") {
        sprintf(
          "<strong>%s</strong><br/>%s: %s%%",
          mapData$areaName,
          currentMetric(),
          round(mapData[[input$splashMetric]] * 100)
        ) %>% lapply(htmltools::HTML)
      } else {
        sprintf(
          "<strong>%s</strong><br/>%s: %s",
          mapData$areaName,
          currentMetric(),
          format(mapData[[input$splashMetric]], big.mark = ",")
        ) %>% lapply(htmltools::HTML)
      }

    leaflet(options = leafletOptions(zoomSnap = 0.1)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data = mapData,
        fillColor = ~ pal(mapData[[input$splashMetric]]),
        color = "black",
        layerId = ~areaCode,
        weight = 1,
        highlightOptions = highlightOptions(
          weight = 2,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      )
  })

  #### 2.3.6.4 Map footnote ----
  output$mapLaFoot <- renderUI({
    paste0(
      (I_DataText %>% filter(metric == input$splashMetric))$LatestPeriod, " data. Click an area to update other charts with LA data."
    )
  })

  ### 2.3.7 Time chart ----

  # create time header
  output$titleTime <- renderUI({
    paste0("How are ", (I_DataText %>% filter(metric == input$splashMetric))$timeTitle, " changing over time?")
  })

  #### 2.3.7.1 Comment ----
  output$commentTime <- renderUI({
    validate(need("geoChoice" %in% names(input), ""))
    currentArea <- C_time %>%
      filter(
        geogConcat == input$geoChoice,
        metric == input$splashMetric
      )
    englandArea <- C_time %>%
      filter(
        geographic_level == "COUNTRY",
        area == "England",
        metric == input$splashMetric
      )
    currentChange <- (currentArea %>%
      filter(chart_year == max(chart_year)))$value -
      (currentArea %>%
        filter(chart_year == (ymd(
          max(chart_year)
        ) - years( # ks5 only has the data fr the last 4 years
          if (input$splashMetric == "sustainedPositiveDestinationKS5Rate") {
            3
          } else {
            4
          }
        ))))$value
    englandChange <- (englandArea %>%
      filter(chart_year == max(chart_year)))$value -
      (englandArea %>%
        filter(chart_year == (ymd(
          max(chart_year)
        ) - years( # ks5 only has the data fr the last 4 years
          if (input$splashMetric == "sustainedPositiveDestinationKS5Rate") {
            3
          } else {
            4
          }
        ))))$value
    paste0(
      (I_DataText %>% filter(metric == input$splashMetric))$timeComment, " ",
      input$geoChoice, " has ",
      if (currentChange > 0) {
        "increased "
      } else {
        "decreased "
      },
      if (sign(currentChange) == sign(englandChange)) {
        if (abs(currentChange) > abs(englandChange)) {
          "faster than the national average"
        } else {
          "slower than the national average"
        }
      } else {
        paste0(" while nationally it has ", if (englandChange > 0) {
          "increased"
        } else {
          "decreased"
        })
      },
      # ks5 only has the data fr the last 4 years
      if (input$splashMetric == "sustainedPositiveDestinationKS5Rate") {
        " in the last four years."
      } else {
        " in the last five years."
      }
      # ,"It has the "
      # , areaRank, suff, " fastest growing ", currentMetric(), " of the ", groupCount)
    )
  })

  #### 2.3.7.2 Chart ----
  Splash_time <-
    eventReactive(
      c(
        input$map_shape_click,
        input$geoChoice,
        input$mapLA_shape_click,
        input$geoComps,
        input$splashMetric
      ),
      {
        SplashTime <- C_time %>%
          filter(
            # get lep/lsip/mca areas
            (geogConcat == input$geoChoice | geogConcat %in% if ("geoComps" %in% names(input)) {
              input$geoComps
            } else {
              "\nNone"
            }) |
              # get england for comparison (if a rate)
              (if (str_sub(input$splashMetric, start = -4) %in% c("Rate", ",000")) {
                (geogConcat == "England")
              } else {
                area == "\nNone"
              }),
            metric == input$splashMetric
          )
        # add an extra column so the colours work in ggplot when sorting alphabetically
        SplashTime$Areas <- factor(SplashTime$geogConcat,
          levels = c("England", input$geoChoice, input$geoComps) # paste0(laClicked()," LADU"),
        )

        ggplot(
          SplashTime,
          aes(
            x = as.Date(chart_year),
            y = value,
            color = Areas,
            group = Areas,
            text = paste0(
              "Year: ",
              chart_year,
              "<br>",
              "Area: ",
              Areas,
              "<br>",
              currentMetric(),
              ": ",
              if (str_sub(input$splashMetric, start = -4) == "Rate") {
                scales::percent(round(value, 3))
              } else {
                format(round(value), big.mark = ",")
              },
              "<br>"
            )
          )
        ) +
          geom_line() +
          theme_minimal() +
          theme(
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank()
          ) +
          scale_y_continuous(labels = if (str_sub(input$splashMetric, start = -4) == "Rate") {
            scales::percent
          } else {
            label_number_si()
          }) +
          labs(colour = "") +
          scale_color_manual(values = if (str_sub(input$splashMetric, start = -4) %in% c("Rate", ",000")) {
            chartColors6
          } else {
            chartColors5
          }) +
          scale_x_date(
            name = "My date axis title",
            date_breaks = "1 years",
            date_labels = "%Y"
          )
      }
    )

  output$Splash_time <- renderPlotly({
    validate(need("geoChoice" %in% names(input), ""))
    ggplotly(Splash_time(), tooltip = "text") %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0,
          y = -0.1
        ),
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })

  ### 2.3.8 Breakdown chart ----
  #### 2.3.8.1 Breakdown filter ----
  distinctSubgroups <- C_breakdown %>%
    distinct(metric, breakdown, subgroups)
  distinctBreakdowns <- C_breakdown %>%
    distinct(metric, breakdown)
  output$breakdownFilter <- renderUI({
    selectizeInput(
      inputId = "barBreakdown",
      label = NULL,
      choices =
        (as.vector(
          distinctBreakdowns %>%
            filter(metric == input$splashMetric)
        ))$breakdown
    )
  })
  #### 2.3.8.2 Optional summary profession filter ----
  summaryCategories <- c("All", (as.vector(
    distinctSubgroups %>%
      filter(breakdown == "Summary Profession Category")
  ))$subgroups)
  output$professionFilter <- renderUI({
    validate(
      need(input$barBreakdown != "", ""),
      need(input$barBreakdown == "Detailed Profession Category", "")
    )
    selectizeInput(
      inputId = "summaryProfession",
      label = "Limit to particular summary profession",
      choices = summaryCategories
    )
  })

  #### 2.3.8.2 Subgroup filter ----
  detailLookup <- D_OnsProfDetail %>% distinct(`Summary Profession Category`, `Detailed Profession Category`)

  output$subgroupFilter <- renderUI({
    validate(
      need(input$barBreakdown != "", ""),
      need(input$barBreakdown != "No breakdowns available", "")
    )
    pickerInput(
      inputId = "barSubgroup",
      label = NULL,
      choices =
        as.vector((
          distinctSubgroups %>%
            filter(
              metric == input$splashMetric,
              breakdown == input$barBreakdown,
              if (input$barBreakdown == "Detailed Profession Category" & "summaryProfession" %in% names(input) && input$summaryProfession != "All") {
                subgroups %in%
                  (detailLookup %>% filter(`Summary Profession Category` == input$summaryProfession))$`Detailed Profession Category`
              } else {
                TRUE
              }
            )
        ))$subgroups,
      multiple = TRUE,
      selected = (as.vector(
        topTenEachBreakdown %>%
          filter(
            metric == input$splashMetric,
            breakdown == input$barBreakdown,
            geogConcat == input$geoChoice,
            if (input$barBreakdown == "Detailed Profession Category" & "summaryProfession" %in% names(input) && input$summaryProfession != "All") {
              `Summary Profession Category` == input$summaryProfession
            } else {
              `Summary Profession Category` == "All"
            }
          )
      ))$subgroups,
      options = list(`actions-box` = TRUE, `live-search` = TRUE)
    )
  })

  #### 2.3.8.3 Title ----
  output$titleBreakdown <- renderUI({
    validate(
      need(input$barBreakdown != "", ""),
      need(input$barBreakdown != "No breakdowns available", "")
    )
    paste0(
      "How do ",
      (I_DataText %>% filter(metric == input$splashMetric))$breakdownTitle,
      " vary by ",
      tolower(input$barBreakdown),
      "?"
    )
  })

  #### 2.3.8.4 Comment ----
  output$commentBreakdown <- renderUI({
    validate(
      need(input$barBreakdown != "", "")
    )

    if (input$barBreakdown == "No breakdowns available") {
      paste0(
        str_to_sentence(currentMetric()),
        " currently has no breakdowns.",
        if (input$splashMetric %in% c(
          "empRate",
          "selfempRate",
          "unempRate",
          "inactiveRate",
          "SelfEmployed",
          "Unemployed",
          "Inactive"
        )) {
          " Switch to Employment metric for occupation and industry breakdowns."
        } else {
          ""
        }
      )
    } else {
      breakdownDiff <- C_breakdown %>%
        filter(
          geogConcat == input$geoChoice |
            geogConcat == "England",
          breakdown == input$barBreakdown,
          metric == input$splashMetric
        ) %>%
        group_by(subgroups) %>%
        mutate(change = (value - lag(value, default = 1)) / value) %>%
        ungroup() %>%
        filter(geogConcat == input$geoChoice) %>%
        mutate(ranking = rank(desc(abs(change)), ties.method = c("first"))) %>%
        filter(ranking == 1)

      breakdownDirection <-
        if (isTRUE(breakdownDiff$change) && breakdownDiff$change > 0) {
          "higher"
        } else {
          "lower"
        }

      paste0(
        input$geoChoice,
        " has a ",
        breakdownDirection,
        " ",
        (I_DataText %>% filter(metric == input$splashMetric))$breakdownComment,
        " in ",
        breakdownDiff$subgroups,
        " than the national average. ",
        if (nrow(C_breakdown %>%
          filter(breakdown == input$barBreakdown) %>%
          distinct(subgroups)) > 10) {
          "The top 10 subgroups are shown. Use the filter to add or remove subgroups. "
        } else {
          ""
        }
      )
    }
  })

  #### 2.3.8.3 Bar chart ----
  Splash_pc <- eventReactive(
    c(
      # input$map_shape_click,
      input$geoChoice,
      input$geoComps,
      # input$barBreakdown,
      input$barSubgroup,
      # input$mapLA_shape_click,
      input$splashMetric
    ),
    {
      validate(
        need(input$barBreakdown != "", ""),
        need(input$barSubgroup != "", ""),
        need(input$splashMetric != "", ""),
        need(input$barBreakdown != "No breakdowns available", "")
      )
      Splash_21 <- C_breakdown %>% filter(
        breakdown == input$barBreakdown,
        subgroups %in% input$barSubgroup,
        metric == input$splashMetric,
        # get lep/lsip/mca areas
        (geogConcat == input$geoChoice | geogConcat %in% if ("geoComps" %in% names(input)) {
          input$geoComps
        } else {
          "\nNone"
        }) |
          # get england for comparison
          (geogConcat == "England")
      )
      # if no rows (because of filter lag) then don't plot
      if (nrow(Splash_21) == 0) {
        "x"
      } else {
        # add an extra column so the colours work in ggplot when sorting alphabetically
        Splash_21$Area <- factor(Splash_21$geogConcat,
          levels = c("England", input$geoChoice, input$geoComps) # paste0(laClicked()," LADU"),
        )
        ggplot(
          Splash_21,
          aes(
            x = reorder(subgroups, value, mean),
            y = value,
            fill = Area,
            text = paste0(
              "Area: ",
              Area,
              "<br>",
              currentMetric(),
              ": ",
              if (str_sub(input$splashMetric, start = -4) == "Rate" |
                input$splashMetric == "Employment" |
                input$splashMetric == "vacancies" |
                input$splashMetric == "enterpriseCount" |
                input$splashMetric == "achievements" |
                input$splashMetric == "participation" |
                input$splashMetric == "starts") {
                scales::percent(round(value, 3))
              } else {
                round(value, 0)
              },
              "<br>"
            )
          )
        ) +
          geom_col(position = "dodge") +
          scale_y_continuous(labels = if (str_sub(input$splashMetric, start = -4) == "Rate" |
            input$splashMetric == "Employment" |
            input$splashMetric == "vacancies" |
            input$splashMetric == "enterpriseCount" |
            input$splashMetric == "achievements" |
            input$splashMetric == "participation" |
            input$splashMetric == "starts") {
            scales::percent
          } else {
            label_number_si(accuracy = 1)
          }) +
          scale_x_discrete(
            labels = function(x) {
              str_wrap(x, width = 26)
            }
          ) +
          coord_flip() +
          theme_minimal() +
          labs(fill = "") +
          theme(
            legend.position = "none",
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_text(size = 7),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
          ) +
          scale_fill_manual(values = chartColors6)
      }
    }
  )

  output$Splash_pc <- renderPlotly({
    # check it exists
    validate(need(Splash_pc() != "x", ""))
    ggplotly(Splash_pc(),
      tooltip = c("text")
    ) %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0,
          y = -0.1
        ),
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })
  #### 2.3.8.6 Bar footnote ----
  output$breakdownFoot <- renderUI({
    validate(
      need(input$barBreakdown != "", ""),
      need(input$barBreakdown != "No breakdowns available", "")
    )
    paste0(
      (I_DataText %>% filter(metric == input$splashMetric))$LatestPeriod, " data."
    )
  })

  ### 2.3.9 Downloads local skills ----
  # all areas
  listDownloadV1All <- reactive({
    list("All areas" = C_datahub %>% filter(metric == input$splashMetric))
  })
  nameDownloadV1All <- reactive({
    paste0(currentMetric(), "-all areas.xlsx")
  })
  output$downloadV1All <- downloadHandler(
    filename = function() {
      nameDownloadV1All()
    },
    content = function(file) {
      write_xlsx(listDownloadV1All(), path = file)
    }
  )
  # current area
  listDownloadV1Current <- reactive({
    list("Current area" = C_datahub %>%
      filter(
        metric == input$splashMetric,
        (paste0(area, " ", geographic_level) == input$geoChoice |
          paste0(area, " ", geographic_level) %in% input$geoComps |
          paste0(area, " ", geographic_level) == "England")
      ))
  })
  nameDownloadV1Current <- reactive({
    paste0(currentMetric(), "-", input$geoChoice, ".xlsx")
  })
  output$downloadV1Current <- downloadHandler(
    filename = function() {
      nameDownloadV1Current()
    },
    content = function(file) {
      write_xlsx(listDownloadV1Current(), path = file)
    }
  )

  ## 2.4 DataHub----
  ### 2.4.1 Filters----
  output$hubAreaInput <- renderUI({
    selectizeInput(
      "hubArea",
      multiple = TRUE,
      label = NULL,
      options = list(placeholder = "Choose LEP, LSIPs, MCAs, LAs*"),
      choices = areaChoices
    )
  })

  output$hubMetricInput <- renderUI({
    selectizeInput(
      "hubMetric",
      choices = C_datahub %>% filter(
        if (is.null(input$hubArea) == TRUE) {
          TRUE
        } else {
          geogConcat %in% input$hubArea
        }
      ) %>%
        distinct(Metrics = metricNeat),
      multiple = TRUE,
      label = NULL,
      options = list(placeholder = "Choose metrics*")
    )
  })

  output$hubBreakdownInput <- renderUI({
    selectizeInput(
      "hubBreakdowns",
      choices = C_datahub %>% filter(
        if (is.null(input$hubArea) == TRUE) {
          TRUE
        } else {
          geogConcat %in% input$hubArea
        },
        if (is.null(input$hubMetric) == TRUE) {
          TRUE
        } else {
          metricNeat %in% input$hubMetric
        }
      ) %>% distinct(Breakdowns = breakdown),
      multiple = TRUE,
      label = NULL,
      options = list(placeholder = "Choose breakdowns")
    )
  })

  output$hubYearInput <- renderUI({
    selectizeInput(
      "hubYears",
      choices = C_datahub %>% filter(
        if (is.null(input$hubArea) == TRUE) {
          TRUE
        } else {
          geogConcat %in% input$hubArea
        },
        if (is.null(input$hubMetric) == TRUE) {
          TRUE
        } else {
          metricNeat %in% input$hubMetric
        },
        if (is.null(input$hubBreakdowns) == TRUE) {
          TRUE
        } else {
          breakdown %in% input$hubBreakdowns
        }
      ) %>%
        distinct("Time period" = time_period),
      multiple = TRUE,
      label = NULL,
      options = list(placeholder = "Choose years*")
    )
  })

  ### 2.4.2 Table----
  datahubDataset <- reactive({
    C_datahub %>%
      filter(
        (if (is.null(input$hubArea) == TRUE) {
          TRUE
        } else {
          {
            geogConcat %in% input$hubArea
          } |
            (if ("Yes" %in% input$hubLA) {
              geogConcat %in% (
                C_Geog %>% filter(geog == "LADU", (LEP %in% input$hubArea | LSIP %in% input$hubArea | MCA %in% input$hubArea))
                  %>% distinct(geogConcat)
              )$geogConcat
            } else {
              geogConcat == "xxx"
            }) |
            (if ("National" %in% input$hubComparators) {
              geogConcat == "England"
            } else {
              geogConcat == "xxx"
            })
        }),
        if (is.null(input$hubYears) == TRUE) {
          TRUE
        } else {
          time_period %in% input$hubYears
        },
        if (is.null(input$hubMetric) == TRUE) {
          TRUE
        } else {
          metricNeat %in% input$hubMetric
        },
        (if (is.null(input$hubBreakdowns) == TRUE) {
          TRUE
        } else {
          breakdown %in% input$hubBreakdowns
        })
      ) %>%
      select(
        Year = time_period,
        Area = geogConcat,
        Data = metricNeat,
        Breakdown = breakdown,
        Splits = subgroups,
        Value = value
      )
  })

  output$hubTable <- renderDataTable({
    DT::datatable(datahubDataset()) %>%
      formatCurrency("Value", currency = "", interval = 3, mark = ",", digit = 0)
  })

  # Download button
  filtered_data1 <- reactive({
    list("LocalSkillIndicators" = datahubDataset())
  })
  output$hubDownload <- downloadHandler(
    filename = function() {
      "LocalSkillsV1Dataset.xlsx"
    },
    content = function(file) {
      write_xlsx(filtered_data1(), path = file)
    }
  )

  ### 2.4.3 Unique code----
  # allOptions <- bind_rows(
  #   data.frame(
  #     Choice = c("LEP", "LSIP", "MCA", "LA"),
  #     filterID = "a"
  #   ),
  #   C_datahub %>% distinct(Choice = area) %>% mutate(filterID = "b"),
  #   data.frame(Choice = c("Yes", "No"), filterID = "c"),
  #   data.frame(
  #     Choice = c("National", "Regional (to come)"),
  #     filterID = "d"
  #   ),
  #   C_datahub %>% distinct(Choice = metricNeat) %>% mutate(filterID = "e"),
  #   C_datahub %>% distinct(Choice = breakdown) %>% mutate(filterID = "f"),
  #   C_datahub %>% distinct(Choice = as.character(time_period)) %>% mutate(filterID = "g")
  # ) %>%
  #   group_by(filterID) %>%
  #   mutate(ChoiceNo = row_number()) %>%
  #   mutate(ChoiceID = paste0(filterID, ChoiceNo)) %>%
  #   ungroup() %>%
  #   select(-filterID, -ChoiceNo)
  #
  # output$uniqueCode <- renderUI({
  #   allOptions %>%
  #     mutate(
  #       chosen = case_when(
  #         Choice %in% input$hubArea ~ 1,
  #         Choice %in% input$hubMetric ~ 1,
  #         Choice %in% input$hubGeog ~ 1,
  #         Choice %in% input$hubComparators ~ 1,
  #         Choice %in% input$hubLA ~ 1,
  #         Choice %in% input$hubBreakdowns ~ 1,
  #         Choice %in% input$hubYears ~ 1,
  #         TRUE ~ 0
  #       )
  #     ) %>%
  #     filter(chosen == 1) %>%
  #     select(ChoiceID) %>%
  #     summarize(
  #       strong = str_c(ChoiceID, collapse = ""),
  #       .groups = "drop"
  #     )
  # })

  # ## 2.6 FE interventions table----
  # output$interventionTable <- DT::renderDataTable({
  #   DT::datatable(
  #     I_InterventionTable,
  #     escape = FALSE,
  #     options = list(dom = "t"),
  #     rownames = FALSE
  #   )
  # })

  ## 2.6 FE tools table----
  output$toolsTable <- DT::renderDataTable({
    DT::datatable(
      I_ToolsTable,
      escape = FALSE,
      options = list(dom = "t"),
      rownames = FALSE
    )
  })

  ## 2.7 FE sources table----
  output$sourcesTable <- DT::renderDataTable({
    DT::datatable(
      I_SourcesTable,
      escape = FALSE,
      options = list(dom = "t"),
      rownames = FALSE
    )
  })



  # 3.Stop app -----
  session$onSessionEnded(function() {
    stopApp()
  })
}
