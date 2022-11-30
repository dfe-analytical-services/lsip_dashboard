server <- function(input, output, session) {

  # Loading screen ---------------------------------------------------------------------------
  # Call initial loading screen

  hide(id = "loading-content", anim = TRUE, animType = "fade")
  show("app-content")

  # Load chart colours:
  # England, geo1, geo2
  chartColors3 <- c("#BFBFBF", "#12436D", "#28A197")
  # geo1, geo2
  chartColors2 <- c("#12436D", "#28A197")

  # HOMEPAGE ----
  # Create link to overview tab
  observeEvent(input$link_to_tabpanel_overview, {
    updateTabsetPanel(session, "navbar", "Local skills") # Get into app
    updateTabsetPanel(session, "datatabset", "Overview") # then pick tab
  })
  # Create link to employment data tab
  observeEvent(input$link_to_tabpanel_employment, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateTabsetPanel(session, "datatabset", "Employment")
  })
  # Create link to vacancy data tab
  observeEvent(input$link_to_tabpanel_vacancies, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateTabsetPanel(session, "datatabset", "Vacancies")
  })
  # Create link to skills data tab
  observeEvent(input$link_to_tabpanel_FE, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateTabsetPanel(session, "datatabset", "Skills")
  })
  # Create link to qualification data tab
  observeEvent(input$link_to_tabpanel_qualification_level, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateTabsetPanel(session, "datatabset", "Qualification level")
  })
  # Create link to data tab
  observeEvent(input$link_to_tabpanel_data, {
    updateTabsetPanel(session, "navbar", "Data & downloads")
  })

  # create table download datasets
  output$downloadData1 <- downloadHandler(
    filename = function() {
      "EmploymentRateIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list(
        "1b.Emp rate" = D_EmpRate_APS1822
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
      "EmpbyindustryIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list(
        "1c.Emp by industry" = D_EmpInd_APS1822
      ), path = file)
    }
  )
  output$downloadData4 <- downloadHandler(
    filename = function() {
      "VacancyIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list(
        "2.Vacancies" = C_Vacancy_ONS1722
      ), path = file)
    }
  )
  output$downloadData5 <- downloadHandler(
    filename = function() {
      "AchievementIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list(
        "3b.FE achievements" = D_Achieve_ILR1621
      ), path = file)
    }
  )
  output$downloadData6 <- downloadHandler(
    filename = function() {
      "AchievementBySSAIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list(
        "3a.FE achievements SSA" = D_Achieve_ILR21
      ), path = file)
    }
  )

  output$downloadData7 <- downloadHandler(
    filename = function() {
      "EnterprisebyemploymentsizeIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list(
        "4a.Enterprise by emp size" = D_empent_UBC1822
      ), path = file)
    }
  )


  output$downloadData8 <- downloadHandler(
    filename = function() {
      "Keystage4destinationsIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list(
        "5a.Key Stage 4 destinations" = D_KS4destin_1521
      ), path = file)
    }
  )

  output$downloadData9 <- downloadHandler(
    filename = function() {
      "Keystage5destinationsIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list(
        "6a.Key Stage 5 destinations" = D_KS5destin_1721
      ), path = file)
    }
  )


  # create download links
  output$hidden_downloads <- renderUI(
    lapply(1:9, function(i) {
      downloadLink(paste0("downloadData", i), "download", class = "hiddenLink")
    })
  )
  # create data table to show
  output$DataTbl <- renderDataTable({
    DT::datatable(I_DataTable %>%
      mutate("Dashboard data" = lapply(
        1:n(),
        function(i) {
          paste0('<a onClick=document.getElementById("downloadData', i, '").click() >Download</a>')
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
    } else if (input$GeoType == "LSIP") {
      selectInput("lep1", "Choose primary LSIP area",
        choices = C_LEP2020 %>% filter(geographic_level == "LSIP") %>% select(Area),
        selected = input$lep1
      )
    } else {
      selectInput("lep1", "Choose primary MCA area",
        choices = C_LEP2020 %>% filter(geographic_level == "MCA") %>% select(Area),
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
      } else if (input$GeoType == "LSIP") {
        selectInput("lep2", "Choose comparison LSIP area",
          choices = c("\nNone", C_LEP2020 %>% filter(geographic_level == "LSIP", Area != input$lep1) %>% select(Area)),
          selected = input$lep2
        )
      } else {
        selectInput("lep2", "Choose comparison MCA area",
          choices = c("\nNone", C_LEP2020 %>% filter(geographic_level == "MCA", Area != input$lep1) %>% select(Area)),
          selected = input$lep2
        )
      }
    }
  })

  # turn on extra filters where used
  output$age_on <- renderUI({
    selectInput("ageGroup", "Choose age group",
      choices = C_Achieve_ILR1621 %>%
        filter(
          typeNeat %in% if ("typeGroup" %in% names(input)) {
            input$typeGroup
          } else {
            "Total FE and skills provision"
          },
          level_or_type %in% if ("levelGroup" %in% names(input)) {
            input$levelGroup
          } else {
            "Further education and skills: Total"
          }
        ) %>%
        distinct(Age = age_group),
      multiple = FALSE, selected = "Total"
    )
  })

  output$type_on <- renderUI({
    selectizeInput("typeGroup", "Choose type of training",
      choices = C_Achieve_ILR1621 %>% distinct(Type = typeNeat),
      multiple = FALSE, selected = "Total FE and skills provision"
    )
  })

  output$level_on <- renderUI({
    selectizeInput("levelGroup", "Choose level of training",
      choices = C_Achieve_ILR1621 %>%
        filter(typeNeat %in% if ("typeGroup" %in% names(input)) {
          input$typeGroup
        } else {
          "Total FE and skills provision"
        }) %>% distinct(Level = level_or_type),
      multiple = FALSE, selected = "Further education and skills: Total"
    )
  })

  output$metric_on <- renderUI({
    selectizeInput("metricGroup", "Choose metric",
      choices = if ("typeGroup" %in% names(input)) {
        if ("Apprenticeships (all ages)" %in% input$typeGroup) {
          c("Achievements" = "achievements", "Starts (apprenticeships only)" = "starts", "Participation" = "participation")
        } else {
          c("Achievements" = "achievements", "Participation" = "participation")
        }
      } else {
        c("Achievements" = "achievements", "Participation" = "participation")
      }
    )
  })

  # define page title
  output$page0title <- renderUI({
    paste0("Overview of local landscape in ", input$lep1)
  })

  ### Downloads----
  # download all indicators
  list_of_datasets0 <- list(
    "1a.Emp by occupation" = D_EmpOcc_APS1721,
    "1b.Emp rate" = D_EmpRate_APS1822,
    "1c.Emp by industry" = D_EmpInd_APS1822,
    "2.Vacancies" = C_Vacancy_ONS1722,
    "3a.FE achievements SSA" = D_Achieve_ILR21,
    "3b.FE achievements" = D_Achieve_ILR1621,
    "4a.FE achievements" = D_qual_APS1721,
    "5a.Enterprise by emp size" = D_empent_UBC1822,
    "6a.Key Stage 4 destinations" = D_KS4destin_1521,
    "7a.Key Stage 5 destinations" = D_KS5destin_1721
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
      "1b.Emp rate" = filter(D_EmpRate_APS1822, geographic_level == input$GeoType, area == input$lep1),
      "2.Vacancies" = filter(C_Vacancy_ONS1722, geographic_level == input$GeoType, area == input$lep1),
      "3a.FE achievements SSA" = filter(D_Achieve_ILR21, geographic_level == input$GeoType, area == input$lep1),
      "3b.FE achievements" = filter(D_Achieve_ILR1621, geographic_level == input$GeoType, area == input$lep1)
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
  empLEP <- eventReactive(input$lep1, {
    C_EmpRate_APS1822 %>%
      filter(area == input$lep1, geographic_level == input$GeoType)
  })
  # get 2022 values
  emp2022 <- reactive({
    empLEP() %>%
      filter(year == "2022")
  })
  # get 2021 values
  emp2021 <- reactive({
    empLEP() %>%
      filter(year == "2021")
  })

  #### Employment count ----
  output$locland.emplcnt0 <- renderUI({
    # call 2022 and 2021 values for chosen LEP
    empCnt2022 <- emp2022()$Employment
    empCntChange <- emp2022()$Employment - emp2021()$Employment

    # print with formatting
    h4(span("Jul-Jun 2022", style = "font-size: 16px;font-weight:normal;"), br(),
      format(empCnt2022, big.mark = ","), br(),
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
  empLineChart <- eventReactive(input$lep1, {
    # call 2022 to 2021 change  for chosen LEP
    empCntChange <- emp2022()$Employment - emp2021()$Employment
    empLine <- empLEP()

    # find min and max for lep
    empCntMinMax <- C_EmpRate_APS1822_max_min %>%
      filter(area == input$lep1)

    ggplot(empLine, aes(x = Year - 1, y = Employment, group = area, text = paste0(
      "Year: Jul-Jun ", year, "<br>",
      "Employment: ", format(Employment, big.mark = ","), "<br>"
    ))) +
      geom_line(data = empLine %>% filter(Year <= 21)) +
      geom_ribbon(
        data = empLine %>% filter(Year >= 21),
        aes(ymin = min(Employment), ymax = Employment),
        fill = ifelse(empCntChange > 0, "#00703c", "#d4351c"),
        alpha = 0.3
      ) +
      geom_line(
        data = empLine %>% filter(Year >= 21),
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
    # call 2021 values and 21-22 change for chosen LEP
    empRate2022 <- emp2022()$empRate
    empRateChange <- emp2022()$empRate - emp2021()$empRate

    # print with formatting
    h4(span("Jul-Jun 2022", style = "font-size: 16px;font-weight:normal;"), br(),
      paste0(format(100 * empRate2022, digit = 2), "%"), br(),
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
  EmpRateMin <- C_EmpRate_APS1822 %>%
    summarise(min(empRate, na.rm = T), .groups = "drop")
  EmpRateMax <- C_EmpRate_APS1822 %>%
    summarise(max(empRate, na.rm = T), .groups = "drop")

  empRateLineChart <- eventReactive(input$lep1, {
    empRateChange <- emp2022()$empRate - emp2021()$empRate
    empRateLine <- C_EmpRate_APS1822 %>%
      filter((geographic_level == input$GeoType & area == input$lep1) | (geographic_level == "COUNTRY" & area == "England"))

    ggplot(empRateLine, aes(
      x = Year - 1, y = empRate,
      group = area,
      text = paste0(
        "Year: Jul-Jun ", year, "<br>",
        "Area: ", area, "<br>",
        "Employment rate: ", format(100 * empRate, digit = 2), "%<br>"
      )
    )) +
      geom_line(data = empRateLine %>% filter(Year <= 21, geographic_level == input$GeoType)) +
      geom_line(data = empRateLine %>% filter(geographic_level == "COUNTRY"), alpha = 0.5) +
      geom_ribbon(
        data = empRateLine %>% filter(Year >= 21, geographic_level == input$GeoType),
        aes(ymin = min(empRate), ymax = empRate),
        fill = ifelse(empRateChange > 0, "#00703c", "#d4351c"),
        alpha = 0.3
      ) +
      geom_line(
        data = empRateLine %>% filter(Year >= 21, geographic_level == input$GeoType),
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
  VacArea <- eventReactive(input$lep1, {
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
    h4(span("Jan 2022", style = "font-size: 16px;font-weight:normal;"), br(),
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
  VacLineChart <- eventReactive(input$lep1, {
    VacLine <- VacArea() %>% filter(year >= 2018)
    VacPcChange <- Vac2022()$jobpc - Vac2021()$jobpc

    VacMinMax <- C_Vacancy_England_max_min %>% filter(area == input$lep1, geographic_level == input$GeoType)

    ggplot(VacLine, aes(x = Year, y = jobpc, group = area, text = paste0(
      "Period: Jan ", year, "<br>",
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
  EtLEP <- eventReactive(input$lep1, {
    C_Achieve_ILR1621 %>%
      filter(
        geographic_level == input$GeoType,
        area == input$lep1,
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
    h4(span("2021/22", style = "font-size: 16px;font-weight:normal;"), br(),
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
  etLineChart <- eventReactive(input$lep1, {
    etLine <- EtLEP()
    etCntChange <- EtLatest()$achievements - EtLast()$achievements
    EtMinMax <- C_Achieve_ILR1621_max_min %>% filter(
      geographic_level == input$GeoType,
      area == input$lep1,
      level_or_type == "Education and training: Total"
    )

    ggplot(etLine, aes(x = Year, y = achievements, group = area, text = paste0(
      "Academic year: ", time_period, "<br>",
      "Achievements: ", format(achievements, big.mark = ","), "<br>"
    ))) +
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
  AppLEP <- eventReactive(input$lep1, {
    C_Achieve_ILR1621 %>%
      filter(
        geographic_level == input$GeoType,
        area == input$lep1,
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
    AppachChange <- AppLatest()$achievements - AppLast()$achievements

    # print with formatting
    h4(span("2021/22", style = "font-size: 16px;font-weight:normal;"), br(),
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
  AppLineChart <- eventReactive(input$lep1, {
    AppLine <- AppLEP()
    AppCntChange <- AppLatest()$achievements - AppLast()$achievements
    AppMinMax <- C_Achieve_ILR1621_max_min %>% filter(
      geographic_level == input$GeoType,
      area == input$lep1,
      level_or_type == "Apprenticeships: Total"
    )


    ggplot(AppLine, aes(x = Year, y = achievements, group = area, text = paste0(
      "Academic year: ", time_period, "<br>",
      "Achievements: ", format(achievements, big.mark = ","), "<br>"
    ))) +
      geom_line(data = AppLine %>% filter(Year <= 20 & Year >= 17)) +
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
    paste0(
      "Employment in ", input$lep1,
      if ("lep2" %in% names(input)) {
        if (input$lep2 == "\nNone") {
        } else {
          paste0(" compared to ", input$lep2)
        }
      }
    )
  })

  ### Downloads----
  list_of_datasets1 <- list(
    "1a.Emp by occupation" = D_EmpOcc_APS1721,
    "1b.Emp rate" = D_EmpRate_APS1822,
    "1c.Emp by industry" = D_EmpInd_APS1822
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
      "1b.Emp rate" = filter(D_EmpRate_APS1822, geographic_level == input$GeoType, area == input$lep1),
      "1c.Emp industry" = filter(D_EmpInd_APS1822, geographic_level == input$GeoType, area == input$lep1)
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
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo1",
        div(
          class = "inner",
          h3(paste0(
            format(100. * emp2022()$empRate, digits = 2),
            "%"
          )),
          p(paste0("employment rate Jul-Jun 2022 in ", input$lep1)),
        )
      )
    )
  })

  output$locland.emplrate.2 <- renderValueBox({
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo2",
        div(
          class = "inner",
          h3(paste0(
            format(100. * (C_EmpRate_APS1822 %>%
              filter(
                geographic_level == input$GeoType,
                area == input$lep2,
                year == "2022"
              )
            )$empRate, digits = 2),
            "%"
          )),
          p(paste0("employment rate Jul-Jun 2022 in ", input$lep2)),
        )
      )
    )
  })
  ### Employment count ----
  output$locland.emplcnt <- renderValueBox({
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo1",
        div(
          class = "inner",
          h3(format(emp2022()$Employment,
            scientific = FALSE, big.mark = ","
          )),
          p(paste0("in employment Jul-Jun 2022 in ", input$lep1)),
        )
      )
    )
  })

  output$locland.emplcnt.2 <- renderValueBox({
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo2",
        div(
          class = "inner",
          h3(format((C_EmpRate_APS1822 %>%
            filter(
              geographic_level == input$GeoType,
              area == input$lep2,
              year == "2022"
            )
          )$Employment,
          scientific = FALSE, big.mark = ","
          )),
          p(paste0("in employment Jul-Jun 2022 in ", input$lep2)),
        )
      )
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
  EmpRate_time <- eventReactive(c(input$lep1, input$lep2), {
    EmpRateTime <- C_EmpRate_APS1822 %>%
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
        x = year - 1, y = empRate,
        color = Areas, group = Areas,
        text = paste0(
          "Year: Jul-Jun ", year, "<br>",
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
      scale_color_manual(values = chartColors3)
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
  EmpOcc <- eventReactive(c(input$lep1, input$lep2), {
    EmpOcc <- C_EmpOcc_APS1721 %>%
      filter(
        geographic_level == input$GeoType | geographic_level == "COUNTRY",
        (area == "England" |
          area == input$lep1 |
          area == if (("lep2" %in% names(input))) {
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
      mutate_at(c(2), ~ replace(., is.na(.), 0)) %>%
      mutate(across(where(is.numeric), ~ round(prop.table(.), 4))) %>%
      filter(.[[2]] > 0) %>%
      rownames_to_column("Occupation") %>%
      relocate(input$lep1, .after = England)
    # %>%
    # filter(Occupation != "Alloccsremain")
  })

  output$EmpOcc <- renderDataTable({
    df <- EmpOcc()
    datatable(df, options = list(order = list(2, "desc")), rownames = FALSE) %>%
      formatPercentage(2:ncol(df), 0)
  })

  ## employment by industry (SIC 2007) bar chart ----
  empind <- eventReactive(c(input$lep1, input$lep2), {
    empind_22 <- C_EmpInd2_APS1822 %>%
      filter(
        year == "2022",
        geographic_level == input$GeoType,
        (area == input$lep1 |
          area == if ("lep2" %in% names(input)) {
            input$lep2
          } else {
            "\nNone"
          })
      )

    # add an extra column so the colours work in ggplot when sorting alphabetically
    empind_22$Area <- factor(empind_22$area,
      levels = c(input$lep1, input$lep2)
    )
    ggplot(empind_22, aes(x = reorder(variable, desc(variable)), y = rate, fill = Area, text = paste0(
      "Industry: ", variable, "<br>",
      "Area: ", Area, "<br>",
      "Percentage: ", scales::percent(round(rate, 2)), "<br>"
    ))) +
      geom_col(
        position = "dodge"
      ) +
      scale_y_continuous(labels = scales::percent) +
      scale_x_discrete(labels = function(rate) str_wrap(rate, width = 26)) +
      coord_flip() +
      theme_minimal() +
      labs(fill = "") +
      theme(
        legend.position = "bottom", axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_text(size = 7),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
      ) +
      scale_fill_manual(values = chartColors2)
  })

  output$empind <- renderPlotly({
    ggplotly(empind(),
      tooltip = c("text"), height = 474
    ) %>%
      layout(
        legend = list(orientation = "h", x = 0, y = -0.1),
        xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })


  # VACANCIES ----
  # define page title
  output$page3title <- renderUI({
    paste0(
      "Job vacancies in ", input$lep1,
      if ("lep2" %in% names(input)) {
        if (input$lep2 == "\nNone") {
        } else {
          paste0(" compared to ", input$lep2)
        }
      }
    )
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
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo1",
        div(
          class = "inner",
          h3(paste0(
            format(100. * Vac2022()$jobpc, digits = 2),
            "%"
          )),
          p(paste0("of online job adverts in England (Jan 2022) were in ", input$lep1)),
        )
      )
    )
  })

  ### ONS job advert unit percent of total LEP 1
  output$jobad.pc.2 <- renderValueBox({
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo2",
        div(
          class = "inner",
          h3(paste0(
            format(100. *
              (C_Vacancy_England %>%
                filter(area == input$lep2, geographic_level == input$GeoType, year == "2022"))$jobpc,
            digits = 2
            ),
            "%"
          )),
          p(paste0("of online job adverts in England (Jan 2022) were in ", input$lep2)),
        )
      )
    )
  })

  ### ONS job advert unit change  LEP 1
  output$jobad.ch <- renderValueBox({
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo1",
        div(
          class = "inner",
          h3(paste0(
            format(100. * (C_Vacancy_England_change %>%
              filter(area == input$lep1, geographic_level == input$GeoType))$Percentage_Change,
            digits = 2
            ),
            "%"
          )),
          p(paste0("change in online job adverts in ", input$lep1, " from Jan
                to Jan 2022")),
        )
      )
    )
  })

  ### ONS job advert unit change  LEP 2
  output$jobad.ch.2 <- renderValueBox({
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo2",
        div(
          class = "inner",
          h3(paste0(
            format(100. * (C_Vacancy_England_change %>%
              filter(area == input$lep2, geographic_level == input$GeoType))$Percentage_Change,
            digits = 2
            ),
            "%"
          )),
          p(paste0("change in online job adverts in ", input$lep2, " from Jan 2021 to Jan 2022")),
        )
      )
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
  jobad.time <- eventReactive(c(input$lep1, input$lep2), {
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
      scale_color_manual(values = chartColors2)
  })

  output$jobad.time <- renderPlotly({
    ggplotly(jobad.time(), tooltip = c("text")) %>%
      layout(
        legend = list(orientation = "h", x = 0, y = -0.1),
        xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })


  # Skills ----
  # define page title
  output$page2title <- renderUI({
    paste0(
      "Further Education and skills training in ", input$lep1,
      if ("lep2" %in% names(input)) {
        if (input$lep2 == "\nNone") {
        } else {
          paste0(" compared to ", input$lep2)
        }
      }
    )
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
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo1",
        div(
          class = "inner",
          h3(format((C_Achieve_ILR1621 %>%
            filter(
              geographic_level == input$GeoType,
              area == input$lep1, time_period == "202122",
              level_or_type %in% if ("levelGroup" %in% names(input) & !"Further education and skills: Total" %in% input$levelGroup) {
                input$levelGroup
              } else {
                "Further education and skills: Total"
              },
              age_group %in% if ("ageGroup" %in% names(input) & !"Total" %in% input$ageGroup) {
                input$ageGroup
              } else {
                "Total"
              }
            ) %>%
            select(input$metricGroup)
          )[1, 1], scientific = FALSE, big.mark = ",")),
          p(paste0(input$metricGroup, " in 2021/22 in ", input$lep1)),
        )
      )
    )
  })

  output$skisup.FEach.2 <- renderValueBox({
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo2",
        div(
          class = "inner",
          h3(format((C_Achieve_ILR1621 %>%
            filter(
              geographic_level == input$GeoType,
              area == input$lep2, time_period == "202122",
              level_or_type %in% if ("levelGroup" %in% names(input) & !"Further education and skills: Total" %in% input$levelGroup) {
                input$levelGroup
              } else {
                "Further education and skills: Total"
              },
              age_group %in% if ("ageGroup" %in% names(input) & !"Total" %in% input$ageGroup) {
                input$ageGroup
              } else {
                "Total"
              }
            ) %>%
            select(input$metricGroup)
          )[1, 1], scientific = FALSE, big.mark = ",")),
          p(paste0(input$metricGroup, " in 2021/22 in ", input$lep2)),
        )
      )
    )
  })

  ### Apprenticeship achievements ----
  output$skisup.APach <- renderValueBox({
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo1",
        div(
          class = "inner",
          h3(format(round((C_Achieve_ILR1621 %>%
            filter(
              geographic_level == input$GeoType,
              area == input$lep1, time_period == "202122",
              level_or_type %in% if ("levelGroup" %in% names(input) & !"Further education and skills: Total" %in% input$levelGroup) {
                input$levelGroup
              } else {
                "Further education and skills: Total"
              },
              age_group %in% if ("ageGroup" %in% names(input) & !"Total" %in% input$ageGroup) {
                input$ageGroup
              } else {
                "Total"
              }
            ) %>%
            select(if ("metricGroup" %in% names(input)) {
              paste0(input$metricGroup, "_rate_per_100000_population")
            } else {
              "achievements_rate_per_100000_population"
            })
          )[1, 1], 0), scientific = FALSE, big.mark = ",", nsmall = 0)),
          p(paste0(input$metricGroup, " rate per 100,000 in 2021/22 in ", input$lep1)),
        )
      )
    )
  })

  output$skisup.APach.2 <- renderValueBox({
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo2",
        div(
          class = "inner",
          h3(format(round((C_Achieve_ILR1621 %>%
            filter(
              geographic_level == input$GeoType,
              area == input$lep2, time_period == "202122",
              level_or_type == if ("levelGroup" %in% names(input)) {
                input$levelGroup
              } else {
                "Further education and skills: Total"
              },
              age_group %in% if ("ageGroup" %in% names(input) & !"Total" %in% input$ageGroup) {
                input$ageGroup
              } else {
                "Total"
              }
            ) %>%
            select(if ("metricGroup" %in% names(input)) {
              paste0(input$metricGroup, "_rate_per_100000_population")
            } else {
              "achievements_rate_per_100000_population"
            })
          )[1, 1], 0), scientific = FALSE, big.mark = ",", nsmall = 0)),
          p(paste0(input$metricGroup, " rate per 100,000 in 2021/22 in ", input$lep2)),
        )
      )
    )
  })

  output$skisup.APach.national <- renderValueBox({
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo3",
        div(
          class = "inner",
          h3(format((C_Achieve_ILR1621 %>%
            filter(
              geographic_level == "National",
              time_period == "202122",
              level_or_type == if ("levelGroup" %in% names(input)) {
                input$levelGroup
              } else {
                "Further education and skills: Total"
              },
              age_group == if ("ageGroup" %in% names(input)) {
                input$ageGroup
              } else {
                "Total"
              }
            ) %>%
            select(if ("metricGroup" %in% names(input)) {
              paste0(input$metricGroup, "_rate_per_100000_population")
            } else {
              "achievements_rate_per_100000_population"
            })
          )[1, 1], scientific = FALSE, big.mark = ",")),
          p(input$metricGroup, " rate per 100,000 in 2021/22 in England"),
        )
      )
    )
  })

  # turn off comparison boxes if none is selected
  output$skill_comp <- renderUI({
    if ("lep2" %in% names(input)) {
      if (input$lep2 == "\nNone") {
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
  # title
  output$feLineTitle <- renderUI({
    paste0(str_to_sentence(input$metricGroup), ": 2016/17 to 2021/22")
  })

  Ach_time <- eventReactive(c(input$lep1, input$lep2, input$levelGroup, input$ageGroup, input$metricGroup), { # , input$splitLine), {
    validate(
      need(input$ageGroup != "", "") # if area not yet loaded don't try to load ch
    )
    FETime <- C_Achieve_ILR1621 %>%
      filter(
        geographic_level == input$GeoType,
        (area == input$lep1 |
          (area == if ("lep2" %in% names(input)) {
            input$lep2
          } else {
            "\nNone"
          })
        ),
        #      if(input$splitLine=="typeNeat")
        #        {typeNeat!="Total FE and skills provision"}
        #      else{
        typeNeat == if ("typeGroup" %in% names(input)) {
          input$typeGroup
        } else {
          "Total FE and skills provision"
          # }
        },
        #   if(input$splitLine=="level_or_type"){level_or_type!="Total"}else{
        level_or_type == if ("levelGroup" %in% names(input)) {
          input$levelGroup
        } else {
          "Further education and skills: Total"
          #   }
        },
        #   if(input$splitLine=="age_group"){age_group!="Total"}else{
        age_group == if ("ageGroup" %in% names(input)) {
          input$ageGroup
        } else {
          "Total"
          #      }
        }
      ) %>%
      select(area, AY, level_or_type, age_group,
        # typeNeat,
        metric = if ("metricGroup" %in% names(input)) {
          input$metricGroup
        } else {
          "achievements"
        }
      )

    # add an extra column so the colours work in ggplot when sorting alphabetically
    FETime$Area <- factor(FETime$area,
      levels = c(input$lep1, input$lep2)
    )

    ggplot(FETime, aes(
      x = AY, y = metric, colour = area,
      # linetype=if(input$splitLine=="None"){}else{eval(parse(text = input$splitLine))},
      group =
      # interaction(
        area
      # ,if(input$splitLine=="None"){}else{eval(parse(text = input$splitLine))})
      ,
      text = paste0(
        "Academic year: ", AY, "<br>",
        "Area: ", Area, "<br>",
        str_to_sentence(input$metricGroup), ": ", format(metric, big.mark = ","), "<br>",
        "Provision and level: ", level_or_type, "<br>",
        "Age: ", age_group, "<br>"
      )
    )) +
      geom_line() +
      theme_minimal() +
      theme(legend.position = "bottom", axis.title.x = element_blank(), axis.title.y = element_blank()) +
      labs(shape = "", colour = "") +
      scale_y_continuous(label = comma) +
      xlab("Year") +
      scale_color_manual(values = chartColors2)
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
  Ach_SSA_pc <- eventReactive(c(input$lep1, input$lep2, input$levelBar, input$sexBar, input$metricBar), {
    AchSSA_21 <- C_Achieve_ILR21 %>%
      filter(
        geographic_level == input$GeoType,
        (area == input$lep1 |
          area == if ("lep2" %in% names(input)) {
            input$lep2
          } else {
            "\nNone"
          }),
        Level == input$levelBar,
        sex == input$sexBar
      ) %>%
      mutate(pc = case_when(input$metricBar == "Achievements" ~ pcAch, TRUE ~ pcEnr)) %>%
      select(area, SSA, metric = input$metricBar, pc)

    # add an extra column so the colours work in ggplot when sorting alphabetically
    AchSSA_21$Area <- factor(AchSSA_21$area,
      levels = c(input$lep1, input$lep2)
    )
    ggplot(AchSSA_21, aes(x = reorder(SSA, desc(SSA)), y = pc, fill = Area, text = paste0(
      "SSA: ", SSA, "<br>",
      "Area: ", Area, "<br>",
      "Percentage of ", str_to_lower(input$metricBar), ": ", scales::percent(round(pc, 2)), "<br>",
      input$metricBar, ": ", metric, "<br>"
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
      scale_fill_manual(values = chartColors2)
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
  
  # Qualification level ----
  # turn off comparison boxes if none is selected  
  output$qual_comp <- renderUI({
    if ("lep2" %in% names(input)) {
      if (input$lep2 == "\nNone") {
        tagList(
          br(),
          p("")
        )
      } else {
        tagList(
          valueBoxOutput("qualup.nvq3.2"),
          valueBoxOutput("qualup.nvq4.2")
        )
      }
    } else {
    }
  })
  
  
  
  # define page title
  output$page4title <- renderUI({
    paste0(
      "Qualification level in ", input$lep1,
      if ("lep2" %in% names(input)) {
        if (input$lep2 == "\nNone") {
        } else {
          paste0(" compared to ", input$lep2)
        }
      }
    )
  })
  
  
  
  
  #age filter
  output$age_on2 <- renderUI({
    selectInput("ageGroup", "Choose age group",
                choices = C_qual2_APS1721 %>%
                  distinct(Age = age_band), 
                multiple = FALSE, selected = "16-64"
    )
  })
  
  
  # qualification level filter
  output$qual_on <- renderUI({
    selectizeInput("qualGroup", "Choose qualification level",
                   choices = C_qual2_APS1721 %>% distinct(Qualificaton_level = Level),
                   multiple = FALSE, selected = "None"
    )
    
  })
  

  

  #turn off gender filter
  output$gen_off <- renderUI({
    if (input$datatabset == "Qualification level" & input$ageGroup == "16-64") {
      selectizeInput("genGroup", "Choose gender group",
                     choices = if ("ageGroup" %in% names(input)) {
                       if ("16-64" %in% input$ageGroup) {
                         c("Total", "Female", "Male")
                       } else {
                         c("Total")
                       }
                     } else {
                       c("Total")
                     }
      )
    } else {
      
    }
  })
  
  
  
  
  
  
  #KPI 1
  output$qualup.nvq3 <- renderValueBox({
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo1",
        div(
          class = "inner",
          h3(format((C_qual2_APS1721 %>%
                       filter(
                         geographic_level == input$GeoType,
                         area == input$lep1, year == "2021",
                         Level == "NVQ3",
                         age_band ==  "16-64",
                         group == "Total"
                       ) %>%
                       select(value)
          )[1, 1], scientific = FALSE, big.mark = ",")),
          p(paste0("NVQ3 and above qualifications for the 16-64 age group in 2021 in ", input$lep1)),
        )
      )
    )
  })
  
  
  #KPI 2 
  output$qualup.nvq3.2 <- renderValueBox({
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo2",
        div(
          class = "inner",
          h3(format((C_qual2_APS1721 %>%
                       filter(
                         geographic_level == input$GeoType,
                         area == input$lep2, year == "2021",
                         Level == "NVQ3",
                         age_band == "16-64",
                         group == "Total"
                       ) %>%
                       select(value)
          )[1, 1], scientific = FALSE, big.mark = ",")),
          p(paste0("NVQ3 and above qualifications for the 16-64 age group in 2021 in ", input$lep2)),
        )
      )
    )
  })
  
  
#KPI3
  output$qualup.nvq4 <- renderValueBox({
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo1",
        div(
          class = "inner",
          h3(paste0(format(100. *(C_qual2_APS1721 %>%
                                    filter(
                                      geographic_level == input$GeoType,
                                      area == input$lep1, year == "2021",
                                      Level == "NVQ4",
                                      age_band == "16-64",
                                      group == "Total"
                                    ) %>%
                                    select(rate)
          )[1, 1], scientific = FALSE, digits = 2)
          ,"%"))
          ,
          p(paste0("NVQ4 and above qualifications for the 16-64 age group in 2021 in ", input$lep1)),
        )
      )
    )
  })
  
  
  #KPI 4
  output$qualup.nvq4.2 <- renderValueBox({
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo2",
        div(
          class = "inner",
          h3(paste0(format(100. *(C_qual2_APS1721 %>%
                                    filter(
                                      geographic_level == input$GeoType,
                                      area == input$lep2, year == "2021",
                                      Level == "NVQ4",
                                      age_band == "16-64",
                                      group == "Total"
                                    ) %>%
                                    select(rate)
          )[1, 1], scientific = FALSE, digits = 2)
          ,"%")),
          p(paste0("NVQ4 and above qualifications for the 16-64 age group in 2021 in ", input$lep2)),
        )
      )
    )
  })
  
  
  qual_time <- eventReactive(c(input$lep1, input$lep2, input$qualGroup, input$ageGroup, input$genGroup), {
    qtime <- C_qual2_APS1721 %>%
      filter(
        geographic_level == input$GeoType,
        (area == input$lep1 |
           (area == if ("lep2" %in% names(input)) {
             input$lep2
           } else {
             "\nNone"
           })
        ),
        Level == if ("qualGroup" %in% names(input)) {
          input$qualGroup
        } else {
          "None"
        },
        age_band == if ("ageGroup" %in% names(input)) {
          input$ageGroup
        } else {
          "16-64"
        },
        group == if ("genGroup" %in% names(input) & if ("ageGroup" %in% names(input)){ input$ageGroup == "16-64"}) {
          input$genGroup
        } else {
          "Total"
        }
      ) %>%
      select(area, year, Level, age_band, group, value )
    
    # add an extra column so the colours work in ggplot when sorting alphabetically
    qtime$area <- factor(qtime$area,
                         levels = c(input$lep1, input$lep2)
    )
    ggplot(qtime, aes(
      x = year, y = value, group = area, colour = area, group = area,
      text = paste0(
        "Year: ", year, "<br>",
        "Area: ", area, "<br>",
        "Number: ", value, "<br>",
        "Qualification: ", Level, "<br>",
        "Age band: ", age_band, "<br>"
      )
    )) +
      geom_line() +
      theme_minimal() +
      theme(legend.position = "bottom", axis.title.x = element_blank(), axis.title.y = element_blank()) +
      labs(shape = "", colour = "") +
      scale_y_continuous(label = comma) +
      xlab("Year") +
      scale_color_manual(values = chartColors2)
  })
  
  
  
  output$qual_time <- renderPlotly({
    ggplotly(qual_time(), tooltip = c("text")) %>%
      layout(
        legend = list(orientation = "h", x = 0, y = -0.1),
        xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })
  
  
  ### Downloads----
  # download qualifications indicators
  list_of_datasets7 <- list(
    "4a. Qualification level" = D_qual_APS1721
  )
  output$download_btn4a <- downloadHandler(
    filename = function() {
      "QualificationIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list_of_datasets7, path = file)
    }
  )
  
  # Download current LEP indicators
  filtered_data7 <- reactive({
    list(
      "4b. Qualificationlevelbyagegen" = filter(D_qual_APS1721, geographic_level == input$GeoType, area == input$lep1,
                                                age_band == input$ageGroup, Level == input$qualGroup)
    )
  })
  output$download_btn4b <- downloadHandler(
    filename = function() {
      "QualificationIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(filtered_data7(), path = file)
    }
  )
  


  
  
  # Stop app ---------------------------------------------------------------------------------

  session$onSessionEnded(function() {
    stopApp()
  })
}
