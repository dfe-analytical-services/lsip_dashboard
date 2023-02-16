server <- function(input, output, session) {
  # 0.Loading screen ---------------------------------------------------------------------------
  # Call initial loading screen

  hide(id = "loading-content", anim = TRUE, animType = "fade")
  show("app-content")

  # Load chart colours:
  # England, geo1, geo2
  chartColors3 <- c("#BFBFBF", "#12436D", "#28A197")
  # geo1, geo2
  chartColors2 <- c("#12436D", "#28A197")

  # 1. HOMEPAGE ----
  ## Create link to overview tab ----
  observeEvent(input$link_to_tabpanel_overview, {
    updateTabsetPanel(session, "navbar", "Local skills") # Get into app
    updateTabsetPanel(session, "datatabset", "Overview") # then pick tab
  })
  ## Create link to employment data tab ----
  observeEvent(input$link_to_tabpanel_employment, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateTabsetPanel(session, "datatabset", "Employment")
  })
  ## Create link to vacancy data tab ----
  observeEvent(input$link_to_tabpanel_vacancies, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateTabsetPanel(session, "datatabset", "Online job adverts")
  })
  ## Create link to skills data tab ----
  observeEvent(input$link_to_tabpanel_FE, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateTabsetPanel(session, "datatabset", "Skills")
  })
  ## Create link to qualification data tab ----
  observeEvent(input$link_to_tabpanel_qualification_level, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateTabsetPanel(session, "datatabset", "Qualification level")
  })
  ## Create link to destinations data tab ----
  observeEvent(input$link_to_tabpanel_destinations_level, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateTabsetPanel(session, "datatabset", "Destinations")
  })
  ## Create link to enterprise data tab ----
  observeEvent(input$link_to_tabpanel_enterprise_level, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateTabsetPanel(session, "datatabset", "Enterprises")
  })
  ## Create link to data tab ----
  observeEvent(input$link_to_tabpanel_data, {
    updateTabsetPanel(session, "navbar", "Data & downloads")
  })

  ## Create table download datasets ----
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
  # output$downloadData4 <- downloadHandler(
  #   filename = function() {
  #     "VacancyIndicators.xlsx"
  #   },
  #   content = function(file) {
  #     write_xlsx(list(
  #       "2.Vacancies" = C_Vacancy_ONS1722
  #     ), path = file)
  #   }
  # )
  output$downloadData4 <- downloadHandler(
    filename = function() {
      "AchievementIndicators.xlsx"
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
      "Qualificationbyageandgendernvq.xlsx"
    },
    content = function(file) {
      write_xlsx(list(
        "4a.Qualification by age and gender" = D_qual_APS1721
      ), path = file)
    }
  )

  output$downloadData7 <- downloadHandler(
    filename = function() {
      "EnterprisebyemploymentsizeIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list(
        "5a.Enterprise by emp size" = D_empent_UBC1822
      ), path = file)
    }
  )

  output$downloadData8 <- downloadHandler(
    filename = function() {
      "EntbyempsizeandindustryIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list(
        "6a.Ent by emp size and ind" = D_empentind_UBC1822
      ), path = file)
    }
  )

  output$downloadData9 <- downloadHandler(
    filename = function() {
      "Enterprisedemography.xlsx"
    },
    content = function(file) {
      write_xlsx(list(
        "7a.Enterprise demography" = D_enterprise_demo1621
      ), path = file)
    }
  )

  output$downloadData10 <- downloadHandler(
    filename = function() {
      "Keystage4destinationsIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list(
        "8a.Key Stage 4 destinations" = D_KS4destin_1521
      ), path = file)
    }
  )

  output$downloadData11 <- downloadHandler(
    filename = function() {
      "Keystage5destinationsIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list(
        "9a.Key Stage 5 destinations" = D_KS5destin_1721
      ), path = file)
    }
  )

  output$downloadData12 <- downloadHandler(
    filename = function() {
      "JobAdvertIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list(
        "2a.Adverts over time" = D_OnsProfTime,
        "2b.Adverts by detailed profession" = D_OnsProfDetail
      ), path = file)
    }
  )


  ## create download links ----
  output$hidden_downloads <- renderUI(
    lapply(1:12, function(i) {
      downloadLink(paste0("downloadData", i), "download", class = "hiddenLink")
    })
  )
  ## create data table to show ----
  output$DataTbl <- renderDataTable({
    DT::datatable(I_DataTable %>%
      mutate("Dashboard data" = lapply(
        1:n(),
        function(i) {
          paste0('<a onClick=document.getElementById("downloadData', i, '").click() >Download</a>')
        }
      )), escape = FALSE, options = list(dom = "t", "pageLength" = 15), rownames = FALSE)
  })

  # 2. OVERVIEW ----
  ## 2.1 Drop downs ----

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


  ## 2.2 FE filters ----
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

  ##  2.3 Downloads ----
  ## download all indicators ----
  list_of_datasets0 <- list(
    "1a.Emp by occupation" = D_EmpOcc_APS1721,
    "1b.Emp rate" = D_EmpRate_APS1822,
    "1c.Emp by industry" = D_EmpInd_APS1822,
    # "2.Vacancies" = C_Vacancy_ONS1722,
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

  ## Download current area indicators ----
  filtered_data0 <- reactive({
    list(
      "1a.Emp by occupation" = filter(D_EmpOcc_APS1721, geographic_level == input$GeoType, area == input$lep1),
      "1b.Emp rate" = filter(D_EmpRate_APS1822, geographic_level == input$GeoType, area == input$lep1),
      # "2.Vacancies" = filter(C_Vacancy_ONS1722, geographic_level == input$GeoType, area == input$lep1),
      "2a.Adverts over time" = filter(D_OnsProfTime, geographic_level == input$GeoType, area == input$lep1),
      "2b.Adverts by detailed profession" = filter(D_OnsProfDetail, geographic_level == input$GeoType, area == input$lep1),
      "3a.FE achievements SSA" = filter(D_Achieve_ILR21, geographic_level == input$GeoType, area == input$lep1),
      "3b.FE achievements" = filter(D_Achieve_ILR1621, geographic_level == input$GeoType, area == input$lep1),
      "4a.Ent by emp size band" = filter(D_empent_UBC1822, geographic_level == input$GeoType, area == input$lep1),
      "5a.Key Stage 5 destinations" = filter(D_KS5destin_1721, geographic_level == input$GeoType, area == input$lep1),
      "6a.Qual by age and gender" = filter(D_qual_APS1721, geographic_level == input$GeoType, area == input$lep1),
      "7a.Ent by emp size & industry" = filter(D_empentind_UBC1822, geographic_level == input$GeoType, area == input$lep1),
      "8a.Enterprise demography" = filter(D_enterprise_demo1621, geographic_level == input$GeoType, area == input$lep1),
      "9a.Key Stage 4 destinations" = filter(D_KS4destin_1521, geographic_level == input$GeoType, area == input$lep1),
      "10a.Key Stage 5 destinations" = filter(D_KS5destin_1721, geographic_level == input$GeoType, area == input$lep1)
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

  ## 2.4 KPIs and charts----

  # get emp data for current area
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
    # call 2022 and 2021 values for chosen area
    empCnt2022 <- emp2022()$Employment
    empCntChange <- emp2022()$Employment - emp2021()$Employment

    # print with formatting
    h4(span("Oct-Sep 2022", style = "font-size: 16px;font-weight:normal;"), br(),
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
    # call 2022 to 2021 change  for chosen area
    empCntChange <- emp2022()$Employment - emp2021()$Employment
    empLine <- empLEP()

    # find min and max for area
    empCntMinMax <- C_EmpRate_APS1822_max_min %>%
      filter(area == input$lep1)

    ggplot(empLine, aes(x = Year - 1, y = Employment, group = area, text = paste0(
      "Year: Oct-Sep ", year, "<br>",
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
    shiny::validate(
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
    # call 2021 values and 21-22 change for chosen area
    empRate2022 <- emp2022()$empRate
    empRateChange <- emp2022()$empRate - emp2021()$empRate

    # print with formatting
    h4(span("Oct-Sep 2022", style = "font-size: 16px;font-weight:normal;"), br(),
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
        "Year: Oct-Sep ", year, "<br>",
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
    C_OnsProfTime %>%
      filter(area == input$lep1, geographic_level == input$GeoType) %>%
      group_by(area, time_period) %>%
      summarise(vacancies = sum(vacancies))
  })
  # get latest values
  VacLatest <- reactive({
    VacArea() %>%
      filter(time_period == "2022-12-01")
  })
  # get 2021 values
  VacLast <- reactive({
    VacArea() %>%
      filter(time_period == "2021-12-01")
  })

  # Vacancy kpi
  output$jobad.units <- renderUI({
    ### ONS job advert units change
    VacChange <- VacLatest()$vacancies - VacLast()$vacancies

    # print with formatting
    h4(span("Dec 2022", style = "font-size: 16px;font-weight:normal;"), br(),
      format(VacLatest()$vacancies, big.mark = ","), br(),
      span(
        format_pm(VacChange),
        style = paste0("font-size: 16px;color:", cond_color(VacChange > 0)) # colour formating
        , .noWS = c("before", "after") # remove whitespace
      ), br(),
      style = "font-size: 21px"
    )
  })

  # Vacancy chart
  VacLineChart <- eventReactive(input$lep1, {
    VacLine <- VacArea()
    VacPcChange <- VacLatest()$vacancies - VacLast()$vacancies

    ggplot(VacLine, aes(x = as.Date(time_period), y = vacancies, group = area, text = paste0(
      "Period: ", format(as.Date(time_period), "%b %y"), "<br>",
      "Online job adverts: ", format(vacancies, big.mark = ","), "<br>"
    ))) +
      geom_line(data = VacLine %>% filter(time_period <= as.Date("2021-12-01"))) +
      geom_ribbon(
        data = VacLine %>% filter(time_period >= as.Date("2021-12-01")),
        aes(ymin = min(vacancies), ymax = vacancies),
        fill = ifelse(VacPcChange > 0, "#00703c", "#d4351c"),
        alpha = 0.3
      ) +
      geom_line(
        data = VacLine %>% filter(time_period >= as.Date("2021-12-01")),
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
        labels = label_number_si(accuracy = 1),
        breaks = c(min(VacLine$vacancies), max(VacLine$vacancies))
      ) +
      scale_x_date(
        name = "My date axis title", date_breaks = "1 years",
        date_labels = "%y"
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
    updateTabsetPanel(session, "datatabset", "Online job adverts")
  })

  #### E&T achievements ----

  # get EandT data for current area
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
  # get App data for current area
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
    r = 4, # increase this margin a bit to prevent the last label dissapearing
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



  #### KS5 sustained positive destination rate ----
  # get dest data for current area

  KS5geo <- eventReactive(input$lep1, {
    C_KS4_KS5eduempapp %>%
      filter(
        geographic_level == input$GeoType & area == input$lep1,
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
    h4(span("2020/21", style = "font-size: 16px;font-weight:normal;"), br(),
      paste0(format(100 * KS5Latest()$rate, digit = 2), "%"), br(),
      span(
        paste0(sprintf("%+.1f", 100 * KS5sustChange), "ppts"),
        style = paste0("font-size: 16px;color:", cond_color(KS5sustChange > 0)) # colour formating
        , .noWS = c("before", "after") # remove whitespace
      ), br(),
      style = "font-size: 21px"
    )
  })

  # KS5 destinations chart
  KS5LineChart <- eventReactive(input$lep1, {
    KS5Line <- C_KS4_KS5eduempapp %>% filter(
      (geographic_level == input$GeoType & area == input$lep1) | (geographic_level == "COUNTRY" & area == "England"), `Cohort Group` == "Total",
      `Key Stage` == "Key Stage 5"
    )
    KS5sustChange <- KS5Latest()$rate - KS5Last()$rate
    KS5MinMax <- C_KS5_eduempapp_max_min

    ggplot(KS5Line, aes(x = Year, y = rate, group = area, text = paste0(
      "Academic year: ", time_period, "<br>",
      "Area: ", area, "<br>",
      "Sustained positive destination rate: ", scales::percent(round(rate, 2)), "<br>"
    ))) +
      geom_line(data = KS5Line %>% filter(Year <= 19, geographic_level == input$GeoType)) +
      geom_line(data = KS5Line %>% filter(geographic_level == "COUNTRY"), alpha = 0.5) +
      geom_ribbon(
        data = KS5Line %>% filter(Year >= 19, geographic_level == input$GeoType),
        aes(ymin = min(rate), ymax = rate),
        fill = ifelse(KS5sustChange > 0, "#00703c", "#d4351c"),
        alpha = 0.3
      ) +
      geom_line(
        data = KS5Line %>% filter(Year >= 19, geographic_level == input$GeoType),
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
  # set margins
  m <- list(
    l = 0,
    r = 4, # increase this margin a bit to prevent the last label dissapearing
    b = 0,
    t = 0,
    pad = 0
  )

  output$KS5LineChart <- renderPlotly({
    ggplotly(KS5LineChart(),
      tooltip = "text",
      height = 81
    ) %>%
      layout(
        margin = m,
        xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })

  # add link to destinations
  observeEvent(input$link_to_tabpanel_destinations2, {
    updateTabsetPanel(session, "navbar", "Dashboard")
    updateTabsetPanel(session, "datatabset", "Destinations")
  })



  #### Micro enterprise  ----
  # get Enterprise data for current area

  Entgeo <- eventReactive(input$lep1, {
    C_empentind3_UBC1822 %>%
      filter(
        geographic_level == input$GeoType & area == input$lep1,
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
    h4(span("Mar 2022", style = "font-size: 16px;font-weight:normal;"), br(),
      paste0(format(100 * EntLatest()$rate, digit = 2), "%"), br(),
      span(
        paste0(sprintf("%+.1f", 100 * EntChange), "ppts"),
        style = paste0("font-size: 16px;color:", cond_color(EntChange > 0)) # colour formating
        , .noWS = c("before", "after") # remove whitespace
      ), br(),
      style = "font-size: 21px"
    )
  })


  # micro enterprise chart
  UBCLineChart <- eventReactive(input$lep1, {
    EntLine <- C_empentind3_UBC1822 %>% filter(
      (geographic_level == input$GeoType & area == input$lep1) | (geographic_level == "COUNTRY" & area == "England"), variable == "Micro 0 to 9",
      industry == "Total"
    )
    EntChange <- EntLatest()$rate - EntLast()$rate
    EntMinMax <- C_empentind_max_min

    ggplot(EntLine, aes(x = Year - 1, y = rate, group = area, text = paste0(
      "March: ", year, "<br>",
      "Area: ", area, "<br>",
      "Percentage: ", scales::percent(round(rate, 2)), "<br>"
    ))) +
      geom_line(data = EntLine %>% filter(Year <= 21, geographic_level == input$GeoType)) +
      geom_line(data = EntLine %>% filter(geographic_level == "COUNTRY"), alpha = 0.5) +
      geom_ribbon(
        data = EntLine %>% filter(Year >= 21, geographic_level == input$GeoType),
        aes(ymin = min(rate), ymax = rate),
        fill = ifelse(EntChange > 0, "#00703c", "#d4351c"),
        alpha = 0.3
      ) +
      geom_line(
        data = EntLine %>% filter(Year >= 21, geographic_level == input$GeoType),
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
  # set margins
  m <- list(
    l = 0,
    r = 4, # increase this margin a bit to prevent the last label dissapearing
    b = 0,
    t = 0,
    pad = 0
  )

  output$UBCLineChart <- renderPlotly({
    ggplotly(UBCLineChart(),
      tooltip = "text",
      height = 81
    ) %>%
      layout(
        margin = m,
        xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })

  # add link to enterprise
  observeEvent(input$link_to_tabpanel_enterprise2, {
    updateTabsetPanel(session, "navbar", "Dashboard")
    updateTabsetPanel(session, "datatabset", "Enterprises")
  })

  #### qualifications NVQ  ----
  # get entprise data for current area

  Qualgeo <- eventReactive(input$lep1, {
    C_qualevel3plus_APS1721 %>%
      filter(
        geographic_level == input$GeoType & area == input$lep1,
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
    h4(span("Jan-Dec 2021", style = "font-size: 16px;font-weight:normal;"), br(),
      paste0(format(100 * QualLatest()$rate, digit = 2), "%"), br(),
      span(
        paste0(sprintf("%+.1f", 100 * QualChange), "ppts"),
        style = paste0("font-size: 16px;color:", cond_color(QualChange > 0)) # colour formating
        , .noWS = c("before", "after") # remove whitespace
      ), br(),
      style = "font-size: 21px"
    )
  })


  # qualification chart
  Nvq3plusLineChart <- eventReactive(input$lep1, {
    QualLine <- C_qualevel3plus_APS1721 %>% filter(
      (geographic_level == input$GeoType & area == input$lep1) | (geographic_level == "COUNTRY" & area == "England"),
      Level == "Level 3 and above",
      age_band == "16-64",
      gender == "Total"
    )
    QualChange <- QualLatest()$rate - QualLast()$rate
    # QualMinMax <- C_qual_max_min

    ggplot(QualLine, aes(x = Year, y = rate, group = area, text = paste0(
      "Jan-Dec: ", year, "<br>",
      "Area: ", area, "<br>",
      "Percentage: ", scales::percent(round(rate, 2)), "<br>"
    ))) +
      geom_line(data = QualLine %>% filter(Year <= 20, geographic_level == input$GeoType)) +
      geom_line(data = QualLine %>% filter(geographic_level == "COUNTRY"), alpha = 0.5) +
      geom_ribbon(
        data = QualLine %>% filter(Year >= 20, geographic_level == input$GeoType),
        aes(ymin = min(rate), ymax = rate),
        fill = ifelse(QualChange > 0, "#00703c", "#d4351c"),
        alpha = 0.3
      ) +
      geom_line(
        data = QualLine %>% filter(Year >= 20, geographic_level == input$GeoType),
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
  # set margins
  m <- list(
    l = 0,
    r = 4, # increase this margin a bit to prevent the last lable dissapearing
    b = 0,
    t = 0,
    pad = 0
  )

  output$Nvq3plusLineChart <- renderPlotly({
    ggplotly(Nvq3plusLineChart(),
      tooltip = "text",
      height = 81
    ) %>%
      layout(
        margin = m,
        xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })

  # add link to qualification level
  observeEvent(input$link_to_tabpanel_qualification2, {
    updateTabsetPanel(session, "navbar", "Dashboard")
    updateTabsetPanel(session, "datatabset", "Qualification level")
  })


  # 3.EMPLOYMENT ----
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

  ## 3.1 Downloads----
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

  # Download current area indicators
  filtered_data1 <- reactive({
    list(
      "1a.Emp by occupation" = filter(D_EmpOcc_APS1721, geographic_level == input$GeoType, (area == input$lep1 |
        area == if ("lep2" %in% names(input)) {
          input$lep2
        } else {
          "\nNone"
        })),
      "1b.Emp rate" = filter(D_EmpRate_APS1822, geographic_level == input$GeoType, (area == input$lep1 |
        area == if ("lep2" %in% names(input)) {
          input$lep2
        } else {
          "\nNone"
        })),
      "1c.Emp industry" = filter(D_EmpInd_APS1822, geographic_level == input$GeoType, (area == input$lep1 |
        area == if ("lep2" %in% names(input)) {
          input$lep2
        } else {
          "\nNone"
        }))
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

  ## 3.2 KPIs/Charts ----

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
          p(paste0("employment rate Oct-Sep 2022 in ", input$lep1)),
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
          p(paste0("employment rate Oct-Sep 2022 in ", input$lep2)),
        )
      )
    )
  })

  output$locland.engrate <- renderValueBox({
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo3",
        div(
          class = "inner",
          h3(paste0(
            format(100. * (C_EmpRate_APS1822 %>%
              filter(
                geographic_level == "COUNTRY",
                area == "England",
                year == "2022"
              )
            )$empRate, digits = 2),
            "%"
          )),
          p(paste0("employment rate Oct-Sep 2022 in ", "England")),
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
          p(paste0("in employment Oct-Sep 2022 in ", input$lep1)),
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
          h3(format(
            (C_EmpRate_APS1822 %>%
              filter(
                geographic_level == input$GeoType,
                area == input$lep2,
                year == "2022"
              )
            )$Employment,
            scientific = FALSE, big.mark = ","
          )),
          p(paste0("in employment Oct-Sep 2022 in ", input$lep2)),
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

  ### Employment rate over time line graph ----
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
          "Year: Oct-Sep ", year, "<br>",
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

  ### Employment by occupation data table ----
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

  ### employment by industry (SIC 2007) bar chart ----
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


  # 4.VACANCIES ----
  # # define page title
  # output$page3title <- renderUI({
  #   paste0(
  #     "Job vacancies in ", input$lep1,
  #     if ("lep2" %in% names(input)) {
  #       if (input$lep2 == "\nNone") {
  #       } else {
  #         paste0(" compared to ", input$lep2)
  #       }
  #     }
  #   )
  # })

  ## 4.1 Downloads----
  # # download skills indicators
  # list_of_datasets2 <- list("2.Vacancies" = C_Vacancy_ONS1722)
  # output$download_btn2a <- downloadHandler(
  #   filename = function() {
  #     "VacancyIndicators.xlsx"
  #   },
  #   content = function(file) {
  #     write_xlsx(list_of_datasets2, path = file)
  #   }
  # )
  #
  # # Download current LEP indicators
  # filtered_data2 <- reactive({
  #   list("2.Vacancies" = filter(C_Vacancy_ONS1722, geographic_level == input$GeoType, (area == input$lep1 |
  #     area == if ("lep2" %in% names(input)) {
  #       input$lep2
  #     } else {
  #       "\nNone"
  #     })))
  # })
  # output$download_btn2b <- downloadHandler(
  #   filename = function() {
  #     "CurrentVacancyIndicators.xlsx"
  #   },
  #   content = function(file) {
  #     write_xlsx(filtered_data2(), path = file)
  #   }
  # )

  ## 4.2 KPIs ----
  ### ONS job advert unit percent of total area 1
  # output$jobad.pc <- renderValueBox({
  #   div(
  #     class = "col-sm-4",
  #     div(
  #       class = "small-box bg-geo1",
  #       div(
  #         class = "inner",
  #         h3(paste0(
  #           format(100. * Vac2022()$jobpc, digits = 2),
  #           "%"
  #         )),
  #         p(paste0("of online job adverts in England (Jan 2022) were in ", input$lep1)),
  #       )
  #     )
  #   )
  # })
  #
  # ### ONS job advert unit percent of total LEP 1
  # output$jobad.pc.2 <- renderValueBox({
  #   div(
  #     class = "col-sm-4",
  #     div(
  #       class = "small-box bg-geo2",
  #       div(
  #         class = "inner",
  #         h3(paste0(
  #           format(
  #             100. *
  #               (C_Vacancy_England %>%
  #                 filter(area == input$lep2, geographic_level == input$GeoType, year == "2022"))$jobpc,
  #             digits = 2
  #           ),
  #           "%"
  #         )),
  #         p(paste0("of online job adverts in England (Jan 2022) were in ", input$lep2)),
  #       )
  #     )
  #   )
  # })
  #
  # ### ONS job advert unit change  LEP 1
  # output$jobad.ch <- renderValueBox({
  #   div(
  #     class = "col-sm-4",
  #     div(
  #       class = "small-box bg-geo1",
  #       div(
  #         class = "inner",
  #         h3(paste0(
  #           format(
  #             100. * (C_Vacancy_England_change %>%
  #               filter(area == input$lep1, geographic_level == input$GeoType))$Percentage_Change,
  #             digits = 2
  #           ),
  #           "%"
  #         )),
  #         p(paste0("change in online job adverts in ", input$lep1, " from Jan
  #               to Jan 2022")),
  #       )
  #     )
  #   )
  # })
  #
  # ### ONS job advert unit change  LEP 2
  # output$jobad.ch.2 <- renderValueBox({
  #   div(
  #     class = "col-sm-4",
  #     div(
  #       class = "small-box bg-geo2",
  #       div(
  #         class = "inner",
  #         h3(paste0(
  #           format(
  #             100. * (C_Vacancy_England_change %>%
  #               filter(area == input$lep2, geographic_level == input$GeoType))$Percentage_Change,
  #             digits = 2
  #           ),
  #           "%"
  #         )),
  #         p(paste0("change in online job adverts in ", input$lep2, " from Jan 2021 to Jan 2022")),
  #       )
  #     )
  #   )
  # })
  #
  # # turn off comparison boxes if none is selected
  # output$vac_comp <- renderUI({
  #   if ("lep2" %in% names(input)) {
  #     if (input$lep2 == "\nNone") {
  #       tagList(
  #         br(),
  #         p("")
  #       )
  #     } else {
  #       tagList(
  #         valueBoxOutput("jobad.pc.2"),
  #         valueBoxOutput("jobad.ch.2")
  #       )
  #     }
  #   } else {
  #     p("")
  #   }
  # })
  #
  ## 4.3 Online job vacancy units over time line chart ----
  # jobad.time <- eventReactive(c(input$lep1, input$lep2), {
  #   JobTime <- C_Vacancy_England %>%
  #     filter(geographic_level == input$GeoType & (area == input$lep1 |
  #       area == if ("lep2" %in% names(input)) {
  #         input$lep2
  #       } else {
  #         "\nNone"
  #       }))
  #
  #   # add an extra column so the colours work in ggplot when sorting alphabetically
  #   JobTime$Areas <- factor(JobTime$area,
  #     levels = c(input$lep1, input$lep2)
  #   )
  #
  #   ggplot(
  #     JobTime,
  #     aes(
  #       x = year, y = jobcnt, colour = Areas, group = Areas,
  #       text = paste0(
  #         "Year: ", year, "<br>",
  #         "Area: ", Areas, "<br>",
  #         "Job vacancy units: ", round(jobcnt, 0), "<br>"
  #       )
  #     )
  #   ) +
  #     geom_line() +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", axis.title.x = element_blank(), axis.title.y = element_blank()) +
  #     labs(shape = "", colour = "") +
  #     scale_color_manual(values = chartColors2)
  # })
  #
  # output$jobad.time <- renderPlotly({
  #   ggplotly(jobad.time(), tooltip = c("text")) %>%
  #     layout(
  #       legend = list(orientation = "h", x = 0, y = -0.1),
  #       xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
  #     ) %>% # disable zooming because it's awful on mobile
  #     config(displayModeBar = FALSE)
  # })


  # 5. SKILLS ----
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

  ## 5.1 Downloads----
  # download skills indicators
  list_of_datasets3 <- list(
    "3a.FE achievements SSA" = D_Achieve_ILR21,
    "3b.FE achievements" = D_Achieve_ILR1621
  )
  output$download_btn3a <- downloadHandler(
    filename = function() {
      "SkillIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list_of_datasets3, path = file)
    }
  )

  # Download current LEP indicators
  filtered_data3 <- reactive({
    list(
      "3a.FE achievements SSA" = filter(D_Achieve_ILR21, geographic_level == input$GeoType, (area == input$lep1 |
        area == if ("lep2" %in% names(input)) {
          input$lep2
        } else {
          "\nNone"
        })),
      "3b.FE achievements" = filter(D_Achieve_ILR1621, geographic_level == input$GeoType, (area == input$lep1 |
        area == if ("lep2" %in% names(input)) {
          input$lep2
        } else {
          "\nNone"
        }))
    )
  })
  output$download_btn3b <- downloadHandler(
    filename = function() {
      "CurrentSkillIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(filtered_data3(), path = file)
    }
  )

  ## 5.2 KPIs/Charts ----
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

  ### Achievements over time line chart ----
  # title
  output$feLineTitle <- renderUI({
    paste0(str_to_sentence(input$metricGroup), ": 2016/17 to 2021/22")
  })

  Ach_time <- eventReactive(c(input$lep1, input$lep2, input$levelGroup, input$ageGroup, input$metricGroup), { # , input$splitLine), {
    shiny::validate(
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
      scale_y_continuous(labels = scales::percent) +
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

  ### Achievements pc bar chart ----
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

  # 6. QUALIFICATION LEVEL ----
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
          valueBoxOutput("qualup.nvq2.2"),
          valueBoxOutput("qualup.nvq3.2")
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


  # qualification level filter
  output$qual_on <- renderUI({
    selectizeInput("qualGroup", "Choose qualification level",
      choices = C_qual2_APS1721 %>% distinct(Qualification_level = Level),
      multiple = FALSE, selected = "NVQ3"
    )
  })


  # turn off gender filter
  output$gen_off <- renderUI({
    if (input$datatabset == "Qualification level" & input$ageGroupQual == "16-64") {
      selectizeInput("genGroup", "Choose gender group",
        choices = if ("ageGroupQual" %in% names(input)) {
          if ("16-64" %in% input$ageGroupQual) {
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
  ## 6.1 KPIS and charts ----
  #### KPI 1 ----
  output$qualup.nvq2 <- renderValueBox({
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo1",
        div(
          class = "inner",
          h3(paste0(
            format(100. * (C_qualevel2_APS1721 %>%
              filter(
                geographic_level == input$GeoType,
                area == input$lep1, year == "2021",
                Level == "below Level 3",
                age_band == "16-64",
                gender == "Total"
              ) %>%
              select(rate)
            )[1, 1], scientific = FALSE, digits = 2),
            "%"
          )),
          p(paste0("of the 16-64 population in ", input$lep1, " have a highest qualification of level 2 or below in 2021")),
        )
      )
    )
  })

  ### KPI 2 ----
  output$qualup.nvq2.2 <- renderValueBox({
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo2",
        div(
          class = "inner",
          h3(paste0(
            format(100. * (C_qualevel2_APS1721 %>%
              filter(
                geographic_level == input$GeoType,
                area == input$lep2, year == "2021",
                Level == "below Level 3",
                age_band == "16-64",
                gender == "Total"
              ) %>%
              select(rate)
            )[1, 1], scientific = FALSE, digits = 2),
            "%"
          )),
          p(paste0("of the 16-64 population in ", input$lep2, " have a highest qualification of level 2 below in 2021")),
        )
      )
    )
  })


  ### KPI3 ----
  output$qualup.nvq3 <- renderValueBox({
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo1",
        div(
          class = "inner",
          h3(paste0(
            format(100. * (C_qualevel3plus_APS1721 %>%
              filter(
                geographic_level == input$GeoType,
                area == input$lep1, year == "2021",
                Level == "Level 3 and above",
                age_band == "16-64",
                gender == "Total"
              ) %>%
              select(rate)
            )[1, 1], scientific = FALSE, digits = 2),
            "%"
          )),
          p(paste0("of the 16-64 population in ", input$lep1, " have a highest qualification of level 3 or above in 2021")),
        )
      )
    )
  })


  ### KPI 4 ----
  output$qualup.nvq3.2 <- renderValueBox({
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo2",
        div(
          class = "inner",
          h3(paste0(
            format(100. * (C_qualevel3plus_APS1721 %>%
              filter(
                geographic_level == input$GeoType,
                area == input$lep2, year == "2021",
                Level == "Level 3 and above",
                age_band == "16-64",
                gender == "Total"
              ) %>%
              select(rate)
            )[1, 1], scientific = FALSE, digits = 2),
            "%"
          )),
          p(paste0("of the 16-64 population in ", input$lep2, " have a highest qualification of level 3 or above in 2021")),
        )
      )
    )
  })


  # qualifications title
  # output$qualtitle <- renderUI({
  #  paste0(input$qualGroup, " ", "qualification level", ":", " Jan-Dec 2017 to Jan-Dec 2021")
  # })


  ### Qualification line chart ----
  qual_time <- eventReactive(c(input$lep1, input$lep2, input$qualGroup, input$ageGroupQual, input$genGroup), {
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
          "NVQ3"
        },
        age_band == if ("ageGroupQual" %in% names(input)) {
          input$ageGroupQual
        } else {
          "16-64"
        },
        gender == if ("genGroup" %in% names(input) & input$ageGroupQual == "16-64") {
          input$genGroup
        } else {
          "Total"
        }
      ) %>%
      select(area, year, rate, gender)

    # add an extra column so the colours work in ggplot when sorting alphabetically
    qtime$area <- factor(qtime$area,
      levels = c(input$lep1, input$lep2)
    )

    ggplot(qtime, aes(
      x = year, y = rate, group = area, colour = area,
      text = paste0(
        "Year: ", year, "<br>",
        "Area: ", area, "<br>",
        "Percentage: ", scales::percent(round(rate, 2)), "<br>",
        "Gender: ", gender, "<br>",
        "Qualification: ", input$qualGroup, "<br>",
        "Age band: ", input$ageGroupQual, "<br>"
      )
    )) +
      geom_line() +
      theme_minimal() +
      theme(legend.position = "bottom", axis.title.x = element_blank(), axis.title.y = element_blank()) +
      labs(shape = "", colour = "") +
      scale_y_continuous(
        labels = scales::percent_format(accuracy = 1),
        limits = c(0, 0.85)
      ) +
      # scale_y_continuous(label = comma) +
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


  ## 6.2 Downloads----
  # download qualifications indicators
  list_of_datasets4 <- list(
    "4a. Qualification level" = D_qual_APS1721
  )
  output$download_btn4a <- downloadHandler(
    filename = function() {
      "QualificationIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list_of_datasets4, path = file)
    }
  )

  # Download current area indicators
  filtered_data4 <- reactive({
    list(
      "4b. Qualificationlevelbyagegen" = filter(
        D_qual_APS1721, geographic_level == input$GeoType, (area == input$lep1 |
          area == if ("lep2" %in% names(input)) {
            input$lep2
          } else {
            "\nNone"
          }),
        gender == input$genGroup
      )
    )
  })
  output$download_btn4b <- downloadHandler(
    filename = function() {
      "currentQualificationIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(filtered_data4(), path = file)
    }
  )


  # 7. DESTINATIONS  ----

  # turn off comparison boxes if none is selected
  output$dest_comp <- renderUI({
    if ("lep2" %in% names(input)) {
      if (input$lep2 == "\nNone") {
        tagList(
          br(),
          p("")
        )
      } else {
        tagList(
          valueBoxOutput("destup.eduempks4.2"),
          valueBoxOutput("destup.eduempks5.2")
        )
      }
    } else {
    }
  })

  # define page title
  output$page5title <- renderUI({
    paste0(
      "Destinations in ", input$lep1,
      if ("lep2" %in% names(input)) {
        if (input$lep2 == "\nNone") {
        } else {
          paste0(" compared to ", input$lep2)
        }
      }
    )
  })

  # turn off cohort filter
  output$cohort_group_off <- renderUI({
    if (input$datatabset == "Destinations" & input$keystageGroup == "Key Stage 5") {
      selectizeInput("cohortGroup", "Choose qualification level",
        choices = if ("keystageGroup" %in% names(input)) {
          if ("Key Stage 5" %in% input$keystageGroup) {
            c("Total", "Level 2", "Level 3", "All other qualifications")
          } else if ("Key Stage 4" %in% input$keystageGroup) {
            c("Total")
          }
        } else {
          c("Total")
        }
      )
    } else {

    }
  })
  ## 7.1 KPIs/Charts ----
  ### KPI 1 ----
  output$destup.eduempks4 <- renderValueBox({
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo1",
        div(
          class = "inner",
          h3(paste0(
            format(100. * (C_KS4_KS5eduempapp %>%
              filter(
                geographic_level == input$GeoType,
                area == input$lep1, time_period == "202021",
                positive_sust == "Sust edu, emp and app",
                `Cohort Group` == "Total",
                `Key Stage` == "Key Stage 4"
              ) %>%
              select(rate)
            )[1, 1], scientific = FALSE, digits = 2),
            "%"
          )),
          p(paste0("of the Key Stage 4 cohort had a sustained positive destination in 2020/21 in ", input$lep1)),
        )
      )
    )
  })

  ### KPI 2 ----
  output$destup.eduempks4.2 <- renderValueBox({
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo2",
        div(
          class = "inner",
          h3(paste0(
            format(100. * (C_KS4_KS5eduempapp %>%
              filter(
                geographic_level == input$GeoType,
                area == input$lep2, time_period == "202021",
                positive_sust == "Sust edu, emp and app",
                `Cohort Group` == "Total",
                `Key Stage` == "Key Stage 4"
              ) %>%
              select(rate)
            )[1, 1], scientific = FALSE, digits = 2),
            "%"
          )),
          p(paste0("of the Key Stage 4 cohort had a sustained positive destination in 2020/21 in ", input$lep2)),
        )
      )
    )
  })

  ### KPI 3 ----
  output$destup.eduempks5 <- renderValueBox({
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo1",
        div(
          class = "inner",
          h3(paste0(
            format(100. * (C_KS4_KS5eduempapp %>%
              filter(
                geographic_level == input$GeoType,
                area == input$lep1, time_period == "202021",
                positive_sust == "Sust edu, emp and app",
                `Cohort Group` == "Total",
                `Key Stage` == "Key Stage 5"
              ) %>%
              select(rate)
            )[1, 1], scientific = FALSE, digits = 2),
            "%"
          )),
          p(paste0("of the Key Stage 5 cohort had a sustained positive destination in 2020/21 in ", input$lep1)),
        )
      )
    )
  })

  ### KPI 4 ----
  output$destup.eduempks5.2 <- renderValueBox({
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo2",
        div(
          class = "inner",
          h3(paste0(
            format(100. * (C_KS4_KS5eduempapp %>%
              filter(
                geographic_level == input$GeoType,
                area == input$lep2, time_period == "202021",
                positive_sust == "Sust edu, emp and app",
                `Cohort Group` == "Total",
                `Key Stage` == "Key Stage 5"
              ) %>%
              select(rate)
            )[1, 1], scientific = FALSE, digits = 2),
            "%"
          )),
          p(paste0("of the Key Stage 5 cohort had a sustained positive destination in 2020/21 in ", input$lep2)),
        )
      )
    )
  })

  ### KPI 5 ----
  output$destup.eduempks4eng <- renderValueBox({
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo3",
        div(
          class = "inner",
          h3(paste0(
            format(100. * (C_KS4_KS5eduempapp %>%
              filter(
                geographic_level == "COUNTRY",
                area == "England", time_period == "202021",
                positive_sust == "Sust edu, emp and app",
                `Cohort Group` == "Total",
                `Key Stage` == "Key Stage 4"
              ) %>%
              select(rate)
            )[1, 1], scientific = FALSE, digits = 2),
            "%"
          )),
          p(paste0("of the Key Stage 4 cohort had a sustained positive destination in 2020/21 in England")),
        )
      )
    )
  })

  ### KPI 6 ----
  output$destup.eduempks5eng <- renderValueBox({
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo3",
        div(
          class = "inner",
          h3(paste0(
            format(100. * (C_KS4_KS5eduempapp %>%
              filter(
                geographic_level == "COUNTRY",
                area == "England", time_period == "202021",
                positive_sust == "Sust edu, emp and app",
                `Cohort Group` == "Total",
                `Key Stage` == "Key Stage 5"
              ) %>%
              select(rate)
            )[1, 1], scientific = FALSE, digits = 2),
            "%"
          )),
          p(paste0("of the Key Stage 5 cohort had a sustained positive destination in 2020/21 in England")),
        )
      )
    )
  })


  # key stage title
  output$keystagetitle <- renderUI({
    paste0("20/21 destinations of the ", input$keystageGroup, " ", paste0(C_KS4_KS5_2021 %>%
      filter(
        geographic_level == input$GeoType,
        `Cohort Group` == if ("cohortGroup" %in% names(input) & input$keystageGroup == "Key Stage 5") {
          input$cohortGroup
        } else {
          "Total"
        },
        `Key Stage` == if ("keystageGroup" %in% names(input)) {
          input$keystageGroup
        } else {
          "Key Stage 4"
        }
      ) %>%
      select(`Cohort Group`) %>% distinct()), " 19/20 cohort")
  })

  ### key stage bar chart ----
  key_stage_2021 <- eventReactive(c(input$lep1, input$lep2, input$cohortGroup, input$keystageGroup), {
    ks_21 <- C_KS4_KS5_2021 %>%
      filter(
        geographic_level == input$GeoType,
        (area == input$lep1 |
          area == if ("lep2" %in% names(input)) {
            input$lep2
          } else {
            "\nNone"
          }),
        `Cohort Group` == if ("cohortGroup" %in% names(input) & input$keystageGroup == "Key Stage 5") {
          input$cohortGroup
        } else {
          "Total"
        },
        `Key Stage` == if ("keystageGroup" %in% names(input)) {
          input$keystageGroup
        } else {
          "Key Stage 4"
        }
      ) %>%
      select(area, rate, variable, time_period, `Cohort Group`, `Key Stage`)

    # add an extra column so the colours work in ggplot when sorting alphabetically
    ks_21$Area <- factor(ks_21$area,
      levels = c(input$lep1, input$lep2)
    )
    ggplot(ks_21, aes(x = reorder(variable, desc(variable)), y = rate, fill = Area, text = paste0
    (
      "Area: ", Area, "<br>",
      "Key stage group: ", `Key Stage`, "<br>",
      "Cohort group: ", `Cohort Group`, "<br>",
      "Percentage: ", scales::percent(round(rate, 2)), "<br>"
    ))) +
      geom_col(
        position = "dodge"
      ) +
      scale_y_continuous(labels = scales::percent) +
      coord_flip() +
      theme_minimal() +
      labs(fill = "") +
      theme(
        legend.position = "bottom", axis.title.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 7),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
      ) +
      scale_fill_manual(values = chartColors2)
  })

  output$key_stage_2021 <- renderPlotly({
    ggplotly(key_stage_2021(),
      tooltip = c("text"), height = 474
    ) %>%
      layout(
        legend = list(orientation = "h", x = 0, y = -0.1),
        xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })

  ## 7.2 Downloads----
  # download destinations indicators
  list_of_datasets5 <- list(
    "5a. KS4 destinations" = D_KS4destin_1521,
    "5b. KS5 destinations" = D_KS5destin_1721
  )
  output$download_btn5a <- downloadHandler(
    filename = function() {
      "Destinations.xlsx"
    },
    content = function(file) {
      write_xlsx(list_of_datasets5, path = file)
    }
  )

  # Download current area indicators
  filtered_data5 <- reactive({
    list(
      "5a. KS4 destinations" = filter(D_KS4destin_1521, geographic_level == input$GeoType, (area == input$lep1 |
        area == if ("lep2" %in% names(input)) {
          input$lep2
        } else {
          "\nNone"
        })),
      "5b. KS5 destinations" = filter(
        D_KS5destin_1721, geographic_level == input$GeoType, (area == input$lep1 |
          area == if ("lep2" %in% names(input)) {
            input$lep2
          } else {
            "\nNone"
          }),
        `Cohort Group` == input$cohortGroup
      )
    )
  })
  output$download_btn5b <- downloadHandler(
    filename = function() {
      "currentDestinations.xlsx"
    },
    content = function(file) {
      write_xlsx(filtered_data5(), path = file)
    }
  )


  # 8. ENTERPRISE ----

  # turn off comparison boxes if none is selected
  output$ent_comp <- renderUI({
    if ("lep2" %in% names(input)) {
      if (input$lep2 == "\nNone") {
        tagList(
          br(),
          p("")
        )
      } else {
        tagList(
          valueBoxOutput("ent.mic.2"),
          valueBoxOutput("ent.sma.2")
        )
      }
    } else {
    }
  })

  # define page title
  output$page6title <- renderUI({
    paste0(
      "Enterprises in ", input$lep1,
      if ("lep2" %in% names(input)) {
        if (input$lep2 == "\nNone") {
        } else {
          paste0(" compared to ", input$lep2)
        }
      }
    )
  })


  # industry filter
  output$industry_on <- renderUI({
    selectizeInput("industryGroup", "Choose industry",
      choices = C_empentind3_UBC1822 %>%
        distinct(Industry = industry),
      multiple = FALSE, selected = "Total"
    )
  })

  # year filter
  output$ent_on <- renderUI({
    selectizeInput("entGroup", "Choose enterprise size",
      choices = C_empentind3_UBC1822 %>% distinct(Enterprise_size = variable),
      multiple = FALSE, selected = "Micro 0 to 9"
    )
  })

  ## 8.1 KPIs/Charts ----
  # KPI 1
  output$ent.mic <- renderValueBox({
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo1",
        div(
          class = "inner",
          h3(paste0(
            format(100. * (C_empentind3_UBC1822 %>%
              filter(
                geographic_level == input$GeoType,
                area == input$lep1, year == "2022",
                variable == "Micro 0 to 9",
                industry == "Total"
              ) %>%
              select(rate)
            )[1, 1], scientific = FALSE, digits = 2),
            "%"
          )),
          p(paste0("of enterprises in ", input$lep1, " in 2022 are micro (with 0 to 9 employees)")),
        )
      )
    )
  })

  ### KPI 2 ----
  output$ent.mic.2 <- renderValueBox({
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo2",
        div(
          class = "inner",
          h3(paste0(
            format(100. * (C_empentind3_UBC1822 %>%
              filter(
                geographic_level == input$GeoType,
                area == input$lep2, year == "2022",
                variable == "Micro 0 to 9",
                industry == "Total"
              ) %>%
              select(rate)
            )[1, 1], scientific = FALSE, digits = 2),
            "%"
          )),
          p(paste0("of enterprises in ", input$lep2, " in 2022 are micro (with 0 to 9 employees)")),
        )
      )
    )
  })

  ### KPI 3 ----
  output$ent.sma <- renderValueBox({
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo1",
        div(
          class = "inner",
          h3(paste0(
            format(100. * (C_empentind3_UBC1822 %>%
              filter(
                geographic_level == input$GeoType,
                area == input$lep1, year == "2022",
                variable == "Small 10 to 49",
                industry == "Total"
              ) %>%
              select(rate)
            )[1, 1], scientific = FALSE, digits = 2),
            "%"
          )),
          p(paste0("of enterprises in ", input$lep1, " in 2022 are small (with 10 to 49 employees)")),
        )
      )
    )
  })

  ### KPI 4 ----
  output$ent.sma.2 <- renderValueBox({
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo2",
        div(
          class = "inner",
          h3(paste0(
            format(100. * (C_empentind3_UBC1822 %>%
              filter(
                geographic_level == input$GeoType,
                area == input$lep2, year == "2022",
                variable == "Small 10 to 49",
                industry == "Total"
              ) %>%
              select(rate)
            )[1, 1], scientific = FALSE, digits = 2),
            "%"
          )),
          p(paste0("of enterprises in ", input$lep2, " in 2022 are small (with 10 to 49 employees)")),
        )
      )
    )
  })


  ### KPI 5 ----
  output$ent.microeng <- renderValueBox({
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo3",
        div(
          class = "inner",
          h3(paste0(
            format(100. * (C_empentind3_UBC1822 %>%
              filter(
                geographic_level == "COUNTRY",
                area == "England", year == "2022",
                variable == "Micro 0 to 9",
                industry == "Total"
              ) %>%
              select(rate)
            )[1, 1], scientific = FALSE, digits = 2),
            "%"
          )),
          p(paste0("of enterprises in England in 2022 are micro (with 0 to 9 employees)")),
        )
      )
    )
  })

  ### KPI 6 ----
  output$ent.smaeng <- renderValueBox({
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo3",
        div(
          class = "inner",
          h3(paste0(
            format(100. * (C_empentind3_UBC1822 %>%
              filter(
                geographic_level == "COUNTRY",
                area == "England", year == "2022",
                variable == "Small 10 to 49",
                industry == "Total"
              ) %>%
              select(rate)
            )[1, 1], scientific = FALSE, digits = 2),
            "%"
          )),
          p(paste0("of enterprises in England in 2022 are small (with 10 to 49 employees)")),
        )
      )
    )
  })


  ### births and deaths line chart ----
  birth_death_time <- eventReactive(c(input$lep1, input$lep2), {
    bir_dea_demo <- C_enterprise_demo1621 %>%
      filter(
        geographic_level == input$GeoType,
        (area == input$lep1 |
          (area == if ("lep2" %in% names(input)) {
            input$lep2
          } else {
            "\nNone"
          })
        )
      ) %>%
      select(area, year, variable, rate)

    # add an extra column so the colours work in ggplot when sorting alphabetically
    bir_dea_demo$area <- factor(bir_dea_demo$area,
      levels = c(input$lep1, input$lep2)
    )

    ggplot(bir_dea_demo, aes(
      x = year, y = rate, colour = area, linetype = variable, group = interaction(area, variable),
      text = paste0(
        "Year: ", year, "<br>",
        "Area: ", area, "<br>",
        "Percentage: ", scales::percent(round(rate, 2)), "<br>"
      )
    )) +
      geom_line() +
      theme_minimal() +
      theme(legend.position = "bottom", axis.title.x = element_blank(), axis.title.y = element_blank()) +
      labs(shape = "", colour = "") +
      scale_y_continuous(labels = scales::percent) +
      # scale_y_continuous(label = comma) +
      xlab("Year") +
      scale_color_manual(values = chartColors2) +
      scale_linetype(guide = "none")
  })



  output$birth_death_time <- renderPlotly({
    ggplotly(birth_death_time(), tooltip = c("text")) %>%
      layout(
        legend = list(orientation = "h", x = 0, y = -0.1),
        xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })




  ### enterprise bar chart ----
  # key stage bar chart
  enterprisesize <- eventReactive(c(input$lep1, input$lep2), {
    ent_22 <- C_empent2_UBC1822 %>%
      filter(
        geographic_level == input$GeoType,
        (area == input$lep1 |
          area == if ("lep2" %in% names(input)) {
            input$lep2
          } else {
            "\nNone"
          }), year == "2022"
      ) %>%
      select(area, rate, variable)

    # add an extra column so the colours work in ggplot when sorting alphabetically
    ent_22$Area <- factor(ent_22$area,
      levels = c(input$lep1, input$lep2)
    )

    level_order <- c("Micro 0 to 9", "Small 10 to 49", "Medium 50 to 249", "Large 250+")

    ggplot(ent_22, aes(x = factor(variable, level = level_order), y = rate, fill = Area, text = paste0(
      "Area: ", Area, "<br>",
      "Enterprise size: ", variable, "<br>",
      "Percentage: ", scales::percent(round(rate, 2)), "<br>"
    ))) +
      geom_col(
        position = "dodge"
      ) +
      scale_y_continuous(labels = scales::percent_format()) +
      # coord_flip() +
      theme_minimal() +
      labs(fill = "") +
      theme(
        legend.position = "bottom", axis.title.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 7),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
      ) +
      scale_fill_manual(values = chartColors2)
  })

  output$enterprisesize <- renderPlotly({
    ggplotly(enterprisesize(),
      tooltip = c("text"), height = 474
    ) %>%
      layout(
        legend = list(orientation = "h", x = 0, y = -0.1),
        xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })

  # enterprise title
  output$enterprisetitle <- renderUI({
    paste0(input$industryGroup, " enterprises: ", input$entGroup)
  })

  ### enterprise line chart ----
  enterprise <- eventReactive(c(input$lep1, input$lep2, input$industryGroup, input$entGroup), {
    ent <- C_empentind3_UBC1822 %>%
      filter(
        geographic_level == input$GeoType,
        (area == input$lep1 |
          area == if ("lep2" %in% names(input)) {
            input$lep2
          } else {
            "\nNone"
          }),
        industry == if ("industryGroup" %in% names(input)) {
          input$industryGroup
        } else {
          "Total"
        },
        variable == if ("entGroup" %in% names(input)) {
          input$entGroup
        } else {
          "Micro 0 to 9"
        }
      ) %>%
      select(area, rate, variable, industry, year)

    # add an extra column so the colours work in ggplot when sorting alphabetically
    ent$Area <- factor(ent$area,
      levels = c(input$lep1, input$lep2)
    )

    ggplot(ent, aes(
      x = year, y = rate, colour = area, group = area,
      text = paste0(
        "Area: ", Area, "<br>",
        "Industry: ", industry, "<br>",
        "Enterprise size: ", variable, "<br>",
        "Percentage: ", scales::percent(round(rate, 2)), "<br>"
      )
    )) +
      geom_line() +
      theme_minimal() +
      theme(legend.position = "bottom", axis.title.x = element_blank(), axis.title.y = element_blank()) +
      labs(shape = "", colour = "") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
      scale_color_manual(values = chartColors2)
  })

  output$enterprise <- renderPlotly({
    ggplotly(enterprise(),
      tooltip = c("text"), height = 474
    ) %>%
      layout(
        legend = list(orientation = "h", x = 0, y = -0.1),
        xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })

  ## 8.2 Downloads----
  # download destinations indicators
  list_of_datasets6 <- list(
    "6a. Ent by emp sizey" = D_empent_UBC1822,
    "6b. Ent by emp size and industry" = D_empentind_UBC1822,
    "6c. Enterprise demography" = D_enterprise_demo1621
  )
  output$download_btn6a <- downloadHandler(
    filename = function() {
      "Enterprise.xlsx"
    },
    content = function(file) {
      write_xlsx(list_of_datasets6, path = file)
    }
  )

  # Download current area indicators
  filtered_data6 <- reactive({
    list(
      "6a. Ent demo" = filter(D_enterprise_demo1621, geographic_level == input$GeoType, (area == input$lep1 |
        area == if ("lep2" %in% names(input)) {
          input$lep2
        } else {
          "\nNone"
        })),
      "6b. Ent by emp size and ind" = filter(
        D_empentind_UBC1822, geographic_level == input$GeoType, (area == input$lep1 |
          area == if ("lep2" %in% names(input)) {
            input$lep2
          } else {
            "\nNone"
          }),
        industry == input$industryGroup, year == input$entGroup
      )
    )
  })
  output$download_btn6b <- downloadHandler(
    filename = function() {
      "currentEnterprise.xlsx"
    },
    content = function(file) {
      write_xlsx(filtered_data6(), path = file)
    }
  )


  # define page title
  output$OnsProftitle <- renderUI({
    paste0("Online job adverts in ", input$lep1)
  })
  # define time chart title
  output$OnsProfTime <- renderUI({
    paste0(input$profChoice, " online job adverts: Oct 2017 to Dec 2022")
  })
  # define detailed table title
  output$OnsProfDetail <- renderUI({
    paste0(input$profChoice, " online job adverts by detailed profession: Dec 2022")
  })

  # 9. JOB ADVERTS ----
  ## 9.1 KPIs/Charts  ----
  # job advert count
  output$profKpi1 <- renderValueBox({
    # call 2022 and 2021 values for chosen area
    vacancyTotalLatest <- C_OnsProfTime %>%
      filter(
        time_period == "2022-12-01",
        geographic_level == input$GeoType,
        area == input$lep1
      )
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo1",
        div(
          class = "inner",
          h3(paste0(
            format(sum(vacancyTotalLatest$vacancies), big.mark = ",")
          )),
          p(paste0("online job adverts in Dec 2022 in ", input$lep1)),
        )
      )
    )
  })

  ### job adverts for profession ----
  output$profKpiProf1 <- renderValueBox({
    # call 2022 and 2021 values for chosen area
    vacancyTotalLatest <- C_OnsProfTime %>%
      filter(
        time_period == "2022-12-01",
        geographic_level == input$GeoType,
        area == input$lep1,
        if (input$profChoice == "All") {
          TRUE
        } else {
          `Summary Profession Category` == input$profChoice
        }
      )
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo1",
        div(
          class = "inner",
          h3(paste0(
            format(sum(vacancyTotalLatest$vacancies), big.mark = ",")
          )),
          p(paste0("online job adverts in Dec 2022 in ", input$lep1)),
        )
      )
    )
  })

  ### Job advert change ----
  output$profKpi2 <- renderValueBox({
    # chnage
    vacancyTotalLatest <- C_OnsProfTime %>%
      filter(
        time_period == "2022-12-01",
        geographic_level == input$GeoType,
        area == input$lep1
      )
    vacancyTotalLast <-
      C_OnsProfTime %>%
      filter(
        time_period == "2021-12-01",
        geographic_level == input$GeoType,
        area == input$lep1
      )

    vacancyTotalChange <- (sum(vacancyTotalLatest$vacancies) - sum(vacancyTotalLast$vacancies)) / sum(vacancyTotalLast$vacancies)
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo1",
        div(
          class = "inner",
          h3(paste0(
            round(vacancyTotalChange * 100), "%"
          )),
          p(paste0("change in online job adverts since Dec 2021 in ", input$lep1)),
        )
      )
    )
  })

  ### Job advert change ----
  output$profKpiProf2 <- renderValueBox({
    # chnage
    vacancyTotalLatest <- C_OnsProfTime %>%
      filter(
        time_period == "2022-12-01",
        geographic_level == input$GeoType,
        area == input$lep1,
        if (input$profChoice == "All") {
          TRUE
        } else {
          `Summary Profession Category` == input$profChoice
        }
      )
    vacancyTotalLast <-
      C_OnsProfTime %>%
      filter(
        time_period == "2021-12-01",
        geographic_level == input$GeoType,
        area == input$lep1,
        if (input$profChoice == "All") {
          TRUE
        } else {
          `Summary Profession Category` == input$profChoice
        }
      )

    vacancyTotalChange <- (sum(vacancyTotalLatest$vacancies) - sum(vacancyTotalLast$vacancies)) / sum(vacancyTotalLast$vacancies)
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo1",
        div(
          class = "inner",
          h3(paste0(
            round(vacancyTotalChange * 100), "%"
          )),
          p(paste0("change in online job adverts since Dec 2021 in ", input$lep1)),
        )
      )
    )
  })

  ### Job advert change England ----
  output$profKpi2Eng <- renderValueBox({
    # chnage
    vacancyTotalLatest <- C_OnsProfTime %>%
      filter(
        time_period == "2022-12-01",
        geographic_level == "Country"
      )
    vacancyTotalLast <-
      C_OnsProfTime %>%
      filter(
        time_period == "2021-12-01",
        geographic_level == "Country"
      )

    vacancyTotalChange <- (sum(vacancyTotalLatest$vacancies) - sum(vacancyTotalLast$vacancies)) / sum(vacancyTotalLast$vacancies)
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo3",
        div(
          class = "inner",
          h3(paste0(
            round(vacancyTotalChange * 100), "%"
          )),
          p("change in online job adverts since Dec 2021 in England"),
        )
      )
    )
  })

  ### Job advert change England for profession ----
  output$profKpi2ProfEng <- renderValueBox({
    # chnage
    vacancyTotalLatest <- C_OnsProfTime %>%
      filter(
        time_period == "2022-12-01",
        geographic_level == "Country",
        if (input$profChoice == "All") {
          TRUE
        } else {
          `Summary Profession Category` == input$profChoice
        }
      )
    vacancyTotalLast <-
      C_OnsProfTime %>%
      filter(
        time_period == "2021-12-01",
        geographic_level == "Country",
        if (input$profChoice == "All") {
          TRUE
        } else {
          `Summary Profession Category` == input$profChoice
        }
      )

    vacancyTotalChange <- (sum(vacancyTotalLatest$vacancies) - sum(vacancyTotalLast$vacancies)) / sum(vacancyTotalLast$vacancies)
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo3",
        div(
          class = "inner",
          h3(paste0(
            round(vacancyTotalChange * 100), "%"
          )),
          p("change in online job adverts since Dec 2021 in England"),
        )
      )
    )
  })

  # comparison boxes
  output$profComp <- renderUI({
    if ("lep2" %in% names(input)) {
      if (input$lep2 == "\nNone") {
        tagList(
          br(),
          p("")
        )
      } else {
        tagList(
          valueBoxOutput("profKpi1comp"),
          valueBoxOutput("profKpi2comp")
        )
      }
    } else {
    }
  })
  # for profession
  output$profCompProf <- renderUI({
    if ("lep2" %in% names(input)) {
      if (input$lep2 == "\nNone") {
        tagList(
          br(),
          p("")
        )
      } else {
        tagList(
          valueBoxOutput("profKpi1compProf"),
          valueBoxOutput("profKpi2compProf")
        )
      }
    } else {
    }
  })

  # job advert count
  output$profKpi1comp <- renderValueBox({
    # call 2022 and 2021 values for chosen area
    vacancyTotalLatest <- C_OnsProfTime %>%
      filter(
        time_period == "2022-12-01",
        geographic_level == input$GeoType,
        area == input$lep2
      )
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo2",
        div(
          class = "inner",
          h3(paste0(
            format(sum(vacancyTotalLatest$vacancies), big.mark = ",")
          )),
          p(paste0("online job adverts in Dec 2022 in ", input$lep2)),
        )
      )
    )
  })


  # job advert count for profeesion
  output$profKpi1compProf <- renderValueBox({
    # call 2022 and 2021 values for chosen area
    vacancyTotalLatest <- C_OnsProfTime %>%
      filter(
        time_period == "2022-12-01",
        geographic_level == input$GeoType,
        area == input$lep2,
        if (input$profChoice == "All") {
          TRUE
        } else {
          `Summary Profession Category` == input$profChoice
        }
      )
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo2",
        div(
          class = "inner",
          h3(paste0(
            format(sum(vacancyTotalLatest$vacancies), big.mark = ",")
          )),
          p(paste0("online job adverts in Dec 2022 in ", input$lep2)),
        )
      )
    )
  })

  ### Job advert change ----
  output$profKpi2comp <- renderValueBox({
    # chnage
    vacancyTotalLatest <- C_OnsProfTime %>%
      filter(
        time_period == "2022-12-01",
        geographic_level == input$GeoType,
        area == input$lep2
      )
    vacancyTotalLast <-
      C_OnsProfTime %>%
      filter(
        time_period == "2021-12-01",
        geographic_level == input$GeoType,
        area == input$lep2
      )

    vacancyTotalChange <- (sum(vacancyTotalLatest$vacancies) - sum(vacancyTotalLast$vacancies)) / sum(vacancyTotalLast$vacancies)
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo2",
        div(
          class = "inner",
          h3(paste0(
            round(vacancyTotalChange * 100), "%"
          )),
          p(paste0("change in online job adverts since Dec 2021 in ", input$lep2)),
        )
      )
    )
  })

  ### Job advert change for profession ----
  output$profKpi2compProf <- renderValueBox({
    # chnage
    vacancyTotalLatest <- C_OnsProfTime %>%
      filter(
        time_period == "2022-12-01",
        geographic_level == input$GeoType,
        area == input$lep2,
        if (input$profChoice == "All") {
          TRUE
        } else {
          `Summary Profession Category` == input$profChoice
        }
      )
    vacancyTotalLast <-
      C_OnsProfTime %>%
      filter(
        time_period == "2021-12-01",
        geographic_level == input$GeoType,
        area == input$lep2,
        if (input$profChoice == "All") {
          TRUE
        } else {
          `Summary Profession Category` == input$profChoice
        }
      )

    vacancyTotalChange <- (sum(vacancyTotalLatest$vacancies) - sum(vacancyTotalLast$vacancies)) / sum(vacancyTotalLast$vacancies)
    div(
      class = "col-sm-4",
      div(
        class = "small-box bg-geo2",
        div(
          class = "inner",
          h3(paste0(
            round(vacancyTotalChange * 100), "%"
          )),
          p(paste0("change in online job adverts since Dec 2021 in ", input$lep2)),
        )
      )
    )
  })

  ### adverts over time ----
  profTime <- eventReactive(c(input$lep1, input$lep2, input$profChoice), {
    profTimeData <-
      C_OnsProfTime %>%
      filter(
        if (input$profChoice == "All") {
          TRUE
        } else {
          `Summary Profession Category` == input$profChoice
        }
      ) %>%
      select(time_period, area, geographic_level, vacancies) %>%
      filter(
        geographic_level == input$GeoType,
        (area == input$lep1 |
          area == if ("lep2" %in% names(input)) {
            input$lep2
          } else {
            "\nNone"
          })
      ) %>%
      group_by(time_period, area, geographic_level) %>%
      summarise(vacancies = sum(vacancies))

    # add an extra column so the colours work in ggplot when sorting alphabetically
    profTimeData$Areas <- factor(profTimeData$area,
      levels = c(input$lep1, input$lep2)
    )

    ggplot(
      profTimeData,
      aes(
        x = as.Date(time_period), y = vacancies,
        color = Areas, group = Areas,
        text = paste0(
          "Period: ", format(as.Date(time_period), "%b %y"), "<br>",
          "Area: ", Areas, "<br>",
          "Online job adverts: ", round(vacancies), "<br>"
        )
      )
    ) +
      geom_line() +
      theme_minimal() +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "bottom", legend.title = element_blank()) +
      scale_y_continuous(labels = label_number_si(accuracy = 1)) +
      scale_x_date(
        name = "My date axis title", date_breaks = "1 years",
        date_labels = "%Y"
      ) +
      labs(colour = "") +
      scale_color_manual(values = chartColors2)
  })

  output$profTime <- renderPlotly({
    ggplotly(profTime(), tooltip = "text") %>%
      layout(
        legend = list(orientation = "h", x = 0, y = -0.1),
        xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })

  # adverts by profession
  ### Employment by occupation data table ----
  profTable <- eventReactive(c(input$lep1, input$lep2), {
    # create areas chosen table
    profTableArea <- C_OnsProfTime %>%
      # select(-"Detailed Profession Category") %>%
      filter(
        time_period == "2022-12-01",
        geographic_level == input$GeoType,
        (area == input$lep1 |
          area == if ("lep2" %in% names(input)) {
            input$lep2
          } else {
            "\nNone"
          })
      )
    # create england table
    profTableEngland <- C_OnsProfTime %>%
      # select(-"Detailed Profession Category") %>%
      filter(
        time_period == "2022-12-01",
        geographic_level == "Country"
      ) %>%
      mutate(area = "England")
    # combine and reformat
    profTable <- bind_rows(profTableArea, profTableEngland) %>%
      group_by(area, `Summary Profession Category`) %>%
      summarise(vacancies = sum(vacancies)) %>%
      pivot_wider(names_from = area, values_from = vacancies) %>%
      relocate(England, .after = `Summary Profession Category`) %>%
      relocate(input$lep1, .after = England) %>%
      mutate(across(where(is.numeric), ~ round(prop.table(.), 4)))
  })

  vals <- reactiveValues(
    row_priority = c("All", unique(C_OnsProfTime$`Summary Profession Category`)),
    row_color = rep("white", 26)
  )

  observeEvent(input$profChoice, {
    vals$row_priority <-
      c(input$profChoice, vals$row_priority[vals$row_priority != input$profChoice])
    vals$row_color <- c("#F46A25", rep("white", 25))
  })

  output$profTable <- renderDataTable({
    df <- profTable()
    # check where profession is in the rank and expand table if it's low
    tableLength <- (C_OnsProfTime %>% filter(
      time_period == "2022-12-01", geographic_level == input$GeoType,
      area == input$lep1
    ) %>%
      group_by(`Summary Profession Category`) %>%
      summarise(vacancies = sum(vacancies)) %>%
      mutate(vacanciesR = rank(desc(vacancies), ties.method = "first")) %>%
      filter(`Summary Profession Category` == input$profChoice))$vacanciesR
    datatable(df, options = list(
      order = list(2, "desc"), pageLength =
        if (tableLength[1] <= 10 | input$profChoice == "All") {
          10
        } else {
          25
        }
    ), rownames = FALSE) %>%
      formatPercentage(2:ncol(df), 0) %>%
      formatStyle("Summary Profession Category",
        target = "row",
        backgroundColor = styleEqual(vals$row_priority,
          vals$row_color,
          default = "white"
        )
      )
  })

  # adverts by detailed profession
  profDetail <- eventReactive(c(input$lep1, input$lep2, input$profChoice), {
    # create areas chosen table
    profDetailArea <- C_OnsProfDetail %>%
      filter(
        # time_period == "2022-12-01",
        if (input$profChoice == "All") {
          TRUE
        } else {
          `Summary Profession Category` == input$profChoice
        },
        geographic_level == input$GeoType,
        (area == input$lep1 |
          area == if ("lep2" %in% names(input)) {
            input$lep2
          } else {
            "\nNone"
          })
      )
    # create england table
    profDetailEngland <- C_OnsProfDetail %>%
      filter(
        # time_period == "2022-12-01",
        if (input$profChoice == "All") {
          TRUE
        } else {
          `Summary Profession Category` == input$profChoice
        },
        geographic_level == "Country"
      ) %>%
      mutate(area = "England")
    # combine and reformat
    profDetail <- bind_rows(profDetailArea, profDetailEngland) %>%
      group_by(area,`Detailed Profession Category`) %>%
      summarise(vacancies = sum(vacancies, na.rm = T)) %>%
      pivot_wider(names_from = area, values_from = vacancies) %>%
      relocate(England, .after = `Detailed Profession Category`) %>%
      left_join(C_OnsProfDetail%>%distinct(`Detailed Profession Category`,`Summary Profession Category`))%>%
      relocate(`Summary Profession Category`, .after = `Detailed Profession Category`)%>%
      relocate(input$lep1, .after = England) %>%
      mutate_at(c(4), ~ replace(., is.na(.), 0)) %>%
      mutate(across(where(is.numeric), ~ round(prop.table(.), 4)))
  })

  output$profDetail <- renderDataTable({
    df <- profDetail()
    datatable(df, options = list(order = list(3, "desc")), rownames = FALSE) %>%
      formatPercentage(3:ncol(df), 0)
  })

  ## 9.2 Downloads----

  list_of_datasetsONS <- list(
    "2a.Adverts over time" = D_OnsProfTime,
    "2b.Adverts by detailed profession" = D_OnsProfDetail
  )
  output$download_prof1 <- downloadHandler(
    filename = function() {
      "ONS_adverts_by_profession.xlsx"
    },
    content = function(file) {
      write_xlsx(list_of_datasetsONS, path = file)
    }
  )

  # Download current area indicators
  filtered_dataProf <- reactive({
    list(
      "2a.Adverts over time" = D_OnsProfTime %>% filter(
        geographic_level == input$GeoType,
        area == input$lep1
      ),
      "2b.Adverts by detailed profession" = D_OnsProfDetail %>% filter(
        geographic_level == input$GeoType,
        area == input$lep1
      )
    )
  })
  output$download_prof2 <- downloadHandler(
    filename = function() {
      "CurrentONS_adverts_by_profession.xlsx"
    },
    content = function(file) {
      write_xlsx(filtered_dataProf(), path = file)
    }
  )


  # Stop app ---------------------------------------------------------------------------------

  session$onSessionEnded(function() {
    stopApp()
  })
}
