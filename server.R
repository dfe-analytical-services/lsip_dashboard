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
               "#2073BC"
    )
  # for when no England
  chartColors5 <-
    c(
      "#12436D",
               "#28A197",
               "#801650",
               "#F46A25",
               "#A285D1",
               "#2073BC"
    )
  
  # 2 Main page ----
  ## 2.1 Homepage ----
  ### 2.1.1 Make links ----
  # Create link to overview tab
  observeEvent(input$link_to_tabpanel_overview, {
    updateTabsetPanel(session, "navbar", "Overview")
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
    updateTabsetPanel(session, "navbar", "Data infomation")
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
      "Keystage4destinationsIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list("8a.Key Stage 4 destinations" = D_KS4destin_1521),
                 path = file
      )
    }
  )
  output$downloadData11 <- downloadHandler(
    filename = function() {
      "Keystage5destinationsIndicators.xlsx"
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
    paste0("Overview of local landscape in ", input$lep1)
  })
  
  ### 2.2.1 Filters ----
  # alter area dropdown depending if lep or lsip
  output$lep1_geo <- renderUI({
    if (input$GeoType == "LEP") {
      selectInput(
        "lep1",
        "Choose primary LEP area",
        choices = C_LEP2020 %>% filter(geographic_level == "LEP") %>% select(Area),
        selected = input$lep1
      )
    } else if (input$GeoType == "LSIP") {
      selectInput(
        "lep1",
        "Choose primary LSIP area",
        choices = C_LEP2020 %>% filter(geographic_level == "LSIP") %>% select(Area),
        selected = input$lep1
      )
    } else {
      selectInput(
        "lep1",
        "Choose primary MCA area",
        choices = C_LEP2020 %>% filter(geographic_level == "MCA") %>% select(Area),
        selected = input$lep1
      )
    }
  })
  
  ###  2.2.2 Downloads ----
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
        geographic_level == input$GeoType,
        area == input$lep1
      ),
      "1b.Emp rate" = filter(
        D_EmpRate_APS1822,
        geographic_level == input$GeoType,
        area == input$lep1
      ),
      # "2.Vacancies" = filter(C_Vacancy_ONS1722, geographic_level == input$GeoType, area == input$lep1),
      "2a.Adverts over time" = filter(
        D_OnsProfTime,
        geographic_level == input$GeoType,
        area == input$lep1
      ),
      "2b.Adverts by detailed profession" = filter(
        D_OnsProfDetail,
        geographic_level == input$GeoType,
        area == input$lep1
      ),
      "3a.FE achievements SSA" = filter(
        D_Achieve_ILR21,
        geographic_level == input$GeoType,
        area == input$lep1
      ),
      "3b.FE achievements" = filter(
        D_Achieve_ILR1621,
        geographic_level == input$GeoType,
        area == input$lep1
      ),
      "4a.Ent by emp size band" = filter(
        D_empent_UBC1822,
        geographic_level == input$GeoType,
        area == input$lep1
      ),
      "5a.Key Stage 5 destinations" = filter(
        D_KS5destin_1721,
        geographic_level == input$GeoType,
        area == input$lep1
      ),
      "6a.Qual by age and gender" = filter(
        D_qual_APS1721,
        geographic_level == input$GeoType,
        area == input$lep1
      ),
      "7a.Ent by emp size & industry" = filter(
        D_empentind_UBC1822,
        geographic_level == input$GeoType,
        area == input$lep1
      ),
      "8a.Enterprise demography" = filter(
        D_enterprise_demo1621,
        geographic_level == input$GeoType,
        area == input$lep1
      ),
      "9a.Key Stage 4 destinations" = filter(
        D_KS4destin_1521,
        geographic_level == input$GeoType,
        area == input$lep1
      ),
      "10a.Key Stage 5 destinations" = filter(
        D_KS5destin_1721,
        geographic_level == input$GeoType,
        area == input$lep1
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
  #### 2.2.3.1 Employment count ----
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
  
  # Employment count
  output$locland.emplcnt0 <- renderUI({
    # call 2022 and 2021 values for chosen area
    empCnt2022 <- emp2022()$Employment
    empCntChange <- emp2022()$Employment - emp2021()$Employment
    
    # print with formatting
    h4(
      span("Oct-Sep 2022", style = "font-size: 16px;font-weight:normal;"),
      br(),
      format(empCnt2022, big.mark = ","),
      br(),
      span(
        format_pm(empCntChange) # plus-minus and comma sep formatting
        ,
        style = paste0("font-size: 16px;color:", cond_color(empCntChange > 0)) # colour formating
        
        ,
        .noWS = c("before", "after") # remove whitespace
      ),
      br(),
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
    
    ggplot(
      empLine,
      aes(
        x = Year - 1,
        y = Employment,
        group = area,
        text = paste0(
          "Year: Oct-Sep ",
          year,
          "<br>",
          "Employment: ",
          format(Employment, big.mark = ","),
          "<br>"
        )
      )
    ) +
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
    r = 4,
    # increase this margin a bit to prevent the last lable dissapearing
    b = 0,
    t = 0,
    pad = 0
  )
  
  output$empLineChart <- renderPlotly({
    validate(need(input$lep1 != "", "")) # if area not yet loaded don't try to load ch)
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
  output$locland.emplrate0 <- renderUI({
    # call 2021 values and 21-22 change for chosen area
    empRate2022 <- emp2022()$empRate
    empRateChange <- emp2022()$empRate - emp2021()$empRate
    
    # print with formatting
    h4(
      span("Oct-Sep 2022", style = "font-size: 16px;font-weight:normal;"),
      br(),
      paste0(format(100 * empRate2022, digit = 2), "%"),
      br(),
      span(
        paste0(sprintf("%+.0f", 100 * empRateChange), "ppts"),
        style = paste0("font-size: 16px;color:", cond_color(empRateChange > 0)) # colour formating
        ,
        .noWS = c("before", "after") # remove whitespace
      ),
      br(),
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
      filter((geographic_level == input$GeoType &
                area == input$lep1) |
               (geographic_level == "COUNTRY" & area == "England"))
    
    ggplot(
      empRateLine,
      aes(
        x = Year - 1,
        y = empRate,
        group = area,
        text = paste0(
          "Year: Oct-Sep ",
          year,
          "<br>",
          "Area: ",
          area,
          "<br>",
          "Employment rate: ",
          format(100 * empRate, digit = 2),
          "%<br>"
        )
      )
    ) +
      geom_line(data = empRateLine %>% filter(Year <= 21, geographic_level == input$GeoType)) +
      geom_line(
        data = empRateLine %>% filter(geographic_level == "COUNTRY"),
        alpha = 0.5
      ) +
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
    r = 4,
    # increase this margin a bit to prevent the last lable dissapearing
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
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })
  
  # Add link to employment data
  observeEvent(input$link_to_tabpanel_employment2, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateSelectInput(session, "splashMetric",
                      selected = "empRate"
    )
  })
  
  #### 2.2.3.3 Job adverts ----
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
      filter(time_period == "2022-10-01")
  })
  # get 2021 values
  VacLast <- reactive({
    VacArea() %>%
      filter(time_period == "2021-10-01")
  })
  
  # Vacancy kpi
  output$jobad.units <- renderUI({
    ### ONS job advert units change
    VacChange <- VacLatest()$vacancies - VacLast()$vacancies
    
    # print with formatting
    h4(
      span("Oct 2022", style = "font-size: 16px;font-weight:normal;"),
      br(),
      format(VacLatest()$vacancies, big.mark = ","),
      br(),
      span(
        format_pm(VacChange),
        style = paste0("font-size: 16px;color:", cond_color(VacChange > 0)) # colour formating
        ,
        .noWS = c("before", "after") # remove whitespace
      ),
      br(),
      style = "font-size: 21px"
    )
  })
  
  # Vacancy chart
  VacLineChart <- eventReactive(input$lep1, {
    VacLine <- VacArea()
    VacPcChange <- VacLatest()$vacancies - VacLast()$vacancies
    
    ggplot(
      VacLine,
      aes(
        x = as.Date(time_period),
        y = vacancies,
        group = area,
        text = paste0(
          "Period: ",
          format(as.Date(time_period), "%b %y"),
          "<br>",
          "Online job adverts: ",
          format(vacancies, big.mark = ","),
          "<br>"
        )
      )
    ) +
      geom_line(data = VacLine %>% filter(time_period <= as.Date("2021-10-01"))) +
      geom_ribbon(
        data = VacLine %>% filter(time_period >= as.Date("2021-10-01")),
        aes(ymin = min(vacancies), ymax = vacancies),
        fill = ifelse(VacPcChange > 0, "#00703c", "#d4351c"),
        alpha = 0.3
      ) +
      geom_line(
        data = VacLine %>% filter(time_period >= as.Date("2021-10-01")),
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
        name = "My date axis title",
        date_breaks = "1 years",
        date_labels = "%y"
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
  
  output$VacLineChart <- renderPlotly({
    ggplotly(VacLineChart(),
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
  etLineChart <- eventReactive(input$lep1, {
    etLine <- EtLEP()
    etCntChange <- EtLatest()$achievements - EtLast()$achievements
    EtMinMax <- C_Achieve_ILR1621_max_min %>% filter(
      geographic_level == input$GeoType,
      area == input$lep1,
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
  # set margins
  m <- list(
    l = 0,
    r = 4,
    # increase this margin a bit to prevent the last lable dissapearing
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
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })
  
  #### 2.2.3.5 FE app achieve ----
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
  AppLineChart <- eventReactive(input$lep1, {
    AppLine <- AppLEP()
    AppCntChange <-
      AppLatest()$achievements - AppLast()$achievements
    AppMinMax <- C_Achieve_ILR1621_max_min %>% filter(
      geographic_level == input$GeoType,
      area == input$lep1,
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
  # set margins
  m <- list(
    l = 0,
    r = 4,
    # increase this margin a bit to prevent the last label dissapearing
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
  KS5LineChart <- eventReactive(input$lep1, {
    KS5Line <- C_KS4_KS5eduempapp %>% filter(
      (geographic_level == input$GeoType &
         area == input$lep1) |
        (geographic_level == "COUNTRY" &
           area == "England"),
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
      geom_line(data = KS5Line %>% filter(Year <= 19, geographic_level == input$GeoType)) +
      geom_line(
        data = KS5Line %>% filter(geographic_level == "COUNTRY"),
        alpha = 0.5
      ) +
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
    r = 4,
    # increase this margin a bit to prevent the last label dissapearing
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
  UBCLineChart <- eventReactive(input$lep1, {
    EntLine <- C_empentind3_UBC1822 %>% filter(
      (geographic_level == input$GeoType &
         area == input$lep1) |
        (geographic_level == "COUNTRY" &
           area == "England"),
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
      geom_line(data = EntLine %>% filter(Year <= 21, geographic_level == input$GeoType)) +
      geom_line(
        data = EntLine %>% filter(geographic_level == "COUNTRY"),
        alpha = 0.5
      ) +
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
    r = 4,
    # increase this margin a bit to prevent the last label dissapearing
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
  Nvq3plusLineChart <- eventReactive(input$lep1, {
    QualLine <- C_qualevel3plus_APS1721 %>% filter(
      (geographic_level == input$GeoType &
         area == input$lep1) |
        (geographic_level == "COUNTRY" & area == "England"),
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
      geom_line(data = QualLine %>% filter(Year <= 20, geographic_level == input$GeoType)) +
      geom_line(
        data = QualLine %>% filter(geographic_level == "COUNTRY"),
        alpha = 0.5
      ) +
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
    r = 4,
    # increase this margin a bit to prevent the last lable dissapearing
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
  areaClicked <- reactive({
    if ("map_shape_click" %in% names(input)) {
      event <- input$map_shape_click
    } else {
      event <- data.frame(id = c("E37000025"))
    }
    C_Geog$areaName[C_Geog$areaCode == event$id]
  })
  # get current LA
  laClicked <- reactive({
    eventLA <- input$mapLA_shape_click
    C_mapLA$LAD22NM[C_mapLA$LAD22CD == eventLA$id]
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
      filename = paste0(areaClicked(), "-", input$splashMetric, ".png"),
      icon("camera"),
      "Screenshot"
    )
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
  output$geoComp <- renderUI({
    if (input$splashGeoType == "LEP") {
      selectizeInput(
        "geoComps",
        multiple = TRUE,
        label = NULL,
        choices = c(
          C_LEP2020 %>% filter(geographic_level == "LEP", Area != areaClicked()) %>% select(Area)
        ),
        options = list(maxItems = 4, placeholder = "Choose comparison LEPs")
      )
    } else if (input$splashGeoType == "LSIP") {
      selectizeInput(
        "geoComps",
        multiple = TRUE,
        label = NULL,
        choices = c(
          C_LEP2020 %>% filter(geographic_level == "LSIP", Area != areaClicked()) %>% select(Area)
        ),
        options = list(maxItems = 4, placeholder = "Choose comparison LSIPs")
      )
    } else {
      selectizeInput(
        "geoComps",
        multiple = TRUE,
        label = NULL,
        choices = c(
          C_LEP2020 %>% filter(geographic_level == "MCA", Area != areaClicked()) %>% select(Area)
        ),
        options = list(maxItems = 4, placeholder = "Choose comparison MCAs")
      )
    }
  })
  
  ### 2.3.5 National map ----
  #### 2.3.5.1 Title ----
  output$titleMap <- renderUI({
    paste0("Where does ", areaClicked(), " fit in the national picture?")
  })
  
  #### 2.3.5.2 Comment ----
  output$commentMap <- renderUI({
    compareNational <-
      if ((C_Geog %>%
           filter(geog == input$splashGeoType &
                  areaName == areaClicked()))[[input$splashMetric]]
          >
          (englandGeog)[[input$splashMetric]]) {
        "higher"
      } else {
        "lower"
      }
    areaRank <- (geogRank() %>%
                   filter(areaName == areaClicked()))$ranking
    suff <- case_when(
      areaRank %in% c(11, 12, 13) ~ "th",
      areaRank %% 10 == 1 ~ "st",
      areaRank %% 10 == 2 ~ "nd",
      areaRank %% 10 == 3 ~ "rd",
      TRUE ~ "th"
    )
    paste0(
      areaClicked(),
      " has a ",
      compareNational,
      " ",
      currentMetric(),
      " than the national average. It has the ",
      areaRank,
      suff,
      " highest ",
      currentMetric(),
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
          currentMetric(),
          round(mapData[[input$splashMetric]] * 100)
        ) %>% lapply(htmltools::HTML)
      } else {
        sprintf(
          "<strong>%s</strong><br/>%s: %s",
          mapData$areaName,
          currentMetric(),
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
        ),
        popup = labels,
        popupOptions = popupOptions(
          className = "myspecial-popup",
          textsize = "12px",
          direction = "auto",
          closeOnClick = TRUE,
          closeButton = FALSE
        )
      ) %>%
      setView(
        lng = -1.6,
        lat = 52.8,
        zoom = 5.7
      )
  })
  
  ### 2.3.6 LA map ----
  #### 2.3.6.1 Title ----
  output$titleLaMap <- renderUI({
    paste0("What is the variation within ", areaClicked(), "?")
  })
  #### 2.3.6.2 Comment----
  output$commentLA <- renderUI({
    LaHighLow <- C_Geog %>%
      filter(
        geog == "LADU",
        eval(parse(text = input$splashGeoType)) == areaClicked()
      ) %>%
      mutate(ranking = rank(desc(eval(
        parse(text = input$splashMetric)
      )), ties.method = c("first")))
    
    LaHigh <- (LaHighLow %>% filter(ranking == 1))$areaName
    LaLow <-
      (LaHighLow %>% filter(ranking == max(ranking)))$areaName
    if (areaClicked() == "London" &
        currentMetric() == "job adverts") {
      "ONS job adverts in London are not broken down by LA."
    } else {
      paste0(
        str_to_sentence(currentMetric()),
        " is highest in ",
        LaHigh,
        " and lowest in ",
        LaLow,
        "."
      )
    }
  })
  
  #### 2.3.6.3 Map----
  output$mapLA <- renderLeaflet({
    # Filter to those LAs in that region
    mapData <- C_Geog %>%
      filter(
        geog == "LADU",
        eval(parse(text = input$splashGeoType)) == areaClicked()
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
  
  ### 2.3.7 Time chart ----
  
  # create time header
  output$titleTime <- renderUI({
    paste0("How is ", currentMetric(), " changing over time?")
  })
  
  #### 2.3.7.1 Comment ----
  output$commentTime <- renderUI({
    currentArea <- C_time %>%
      filter(
        geographic_level == input$splashGeoType,
        area == areaClicked(),
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
      areaClicked(), "'s ", currentMetric(), " has ",
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
        input$mapLA_shape_click,
        input$geoComps,
        input$splashMetric
      ),
      {
        SplashTime <- C_time %>%
          filter(
            # get lep/lsip/mca areas
            (
              geographic_level == input$splashGeoType &
                (area == areaClicked() |
                   area %in% if ("geoComps" %in% names(input)) {
                     input$geoComps
                   } else {
                     "\nNone"
                   })
            ) |
              # get england for comparison (if a rate)
              (if (str_sub(input$splashMetric, start = -4) %in% c("Rate", "tion")) {
                (geographic_level == "COUNTRY" & area == "England")
              } else {
                area == "\nNone"
              }) |
              # get LA
              if (is.null(input$mapLA_shape_click) == TRUE) {
                area == "\nNone"
              } else {
                (geographic_level == "LADU" & area == laClicked())
              },
            metric == input$splashMetric
          )
        # add an extra column so the colours work in ggplot when sorting alphabetically
        SplashTime$Areas <- factor(SplashTime$area,
                                   levels = c("England", areaClicked(), laClicked(), input$geoComps)
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
                scales::percent(round(value, 2))
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
            scales::percent_format(accuracy = 1)
          } else {
            label_number_si(accuracy = 1)
          }) +
          labs(colour = "") +
          scale_color_manual(values = if (str_sub(input$splashMetric, start = -4) %in% c("Rate", "tion")) {
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
  #### 2.3.8.2 Subgroup filter ----
  distinctSubgroups <- C_breakdown %>%
    distinct(metric, breakdown, subgroups)
  topTenEachBreakdown <- C_breakdown %>%
    group_by(metric, breakdown, area, geographic_level) %>%
    arrange(desc(value)) %>%
    slice(1:10)
  
  output$subgroupFilter <- renderUI({
    validate(
      need(input$barBreakdown != "", ""),
      # if area not yet loaded don't try to load ch
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
              breakdown == input$barBreakdown
            )
        ))$subgroups,
      multiple = TRUE,
      selected = (as.vector(
        topTenEachBreakdown %>%
          filter(
            metric == input$splashMetric,
            breakdown == input$barBreakdown,
            area == areaClicked(),
            geographic_level == input$splashGeoType
          ) %>%
          distinct(subgroups)
      ))$subgroups,
      options = list(`actions-box` = TRUE)
    )
  })
  
  #### 2.3.8.3 Title ----
  output$titleBreakdown <- renderUI({
    validate(need(input$barBreakdown != "", "")) # if area not yet loaded don't try to load ch)
    if (input$barBreakdown == "No breakdowns available") {
      paste0(
        str_to_sentence(currentMetric()),
        " currently has no breakdowns.",
        if (input$splashMetric %in% c(
          "empRate",
          "selfempRate",
          "unempRate",
          "inactiveRate",
          "  Self Employed ",
          "  Unemployed ",
          "  Inactive "
        )) {
          " Switch to Employment volume metric for occupation and industry breakdowns."
        } else {
          ""
        }
      )
    } else {
      paste0(
        "How does ",
        currentMetric(),
        " vary by ",
        tolower(input$barBreakdown),
        "?"
      )
    }
  })
  
  #### 2.3.8.4 Comment ----
  output$commentBreakdown <- renderUI({
    validate(
      need(input$barBreakdown != "", ""),
      # if area not yet loaded don't try to load ch
      need(input$barBreakdown != "No breakdowns available", "")
    )
    breakdownDiff <- C_breakdown %>%
      filter(
        geographic_level == input$splashGeoType |
          geographic_level == "COUNTRY",
        area == areaClicked() | area == "England",
        breakdown == input$barBreakdown,
        metric == input$splashMetric
      ) %>%
      group_by(subgroups) %>%
      mutate(change = (value - lag(value, default = 1)) / value) %>%
      ungroup() %>%
      filter(area == areaClicked()) %>%
      mutate(ranking = rank(desc(abs(change)), ties.method = c("first"))) %>%
      filter(ranking == 1)
    
    breakdownDirection <-
      if (isTRUE(breakdownDiff$change) && breakdownDiff$change > 0) {
        "high"
      } else {
        "low"
      }
    paste0(
      areaClicked(),
      " has a ",
      breakdownDirection,
      " ",
      currentMetric(),
      " in ",
      breakdownDiff$subgroups,
      " than the national average."
    )
  })
  
  #### 2.3.8.3 Bar chart ----
  Splash_pc <- eventReactive(
    c(
      input$map_shape_click,
      input$geoComps,
      # input$barBreakdown,
      input$barSubgroup,
      input$mapLA_shape_click,
      input$splashMetric
    ),
    {
      validate(
        need(input$barBreakdown != "", ""),
        # if area not yet loaded don't try to load ch
        need(input$barSubgroup != "", ""),
        need(input$splashMetric != "", ""),
        need(input$barBreakdown != "No breakdowns available", "")
      )
      Splash_21 <- C_breakdown %>% filter(
        breakdown == input$barBreakdown,
        subgroups %in% input$barSubgroup,
        metric == input$splashMetric,
        (geographic_level == "COUNTRY" & area == "England") |
          ((
            geographic_level == input$splashGeoType &
              (area == areaClicked() |
                 area %in% if ("geoComps" %in% names(input)) {
                   input$geoComps
                 } else {
                   "\nNone"
                 })
          ) |
            if (is.null(input$mapLA_shape_click) == TRUE) {
              area == "\nNone"
            } else {
              (geographic_level == "LADU" & area == laClicked())
            })
      )
      # if no rows (because of filter lag) then don't plot
      if (nrow(Splash_21) == 0) {
        "x"
      } else {
        # add an extra column so the colours work in ggplot when sorting alphabetically
        Splash_21$Area <- factor(Splash_21$area,
                                 levels = c("England", areaClicked(), laClicked(), input$geoComps)
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
                scales::percent(round(value, 2))
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
                                          input$splashMetric == "enterpriseCount") {
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
  
  ## 2.4 DataHub----
  ### 2.4.1 Filters----
  output$hubAreaInput <- renderUI({
    selectizeInput(
      "hubArea",
      choices = C_datahub %>%
        filter(if (is.null(input$hubGeog) == TRUE) {
          TRUE
        } else {
          geographic_level %in% input$hubGeog
        }) %>%
        distinct(Area = area),
      multiple = TRUE,
      label = NULL,
      options = list(placeholder = "Choose areas*")
    )
  })
  output$hubMetricInput <- renderUI({
    selectizeInput(
      "hubMetric",
      choices = C_datahub %>% filter(
        if (is.null(input$hubGeog) == TRUE) {
          TRUE
        } else {
          geographic_level %in% input$hubGeog
        },
        if (is.null(input$hubArea) == TRUE) {
          TRUE
        } else {
          area %in% input$hubArea
        }
      ) %>%
        distinct(Metrics = metric),
      multiple = TRUE,
      label = NULL,
      options = list(placeholder = "Choose metrics*")
    )
  })
  
  output$hubBreakdownInput <- renderUI({
    selectizeInput(
      "hubBreakdowns",
      choices = C_datahub %>% filter(
        if (is.null(input$hubGeog) == TRUE) {
          TRUE
        } else {
          geographic_level %in% input$hubGeog
        },
        if (is.null(input$hubArea) == TRUE) {
          TRUE
        } else {
          area %in% input$hubArea
        },
        if (is.null(input$hubMetric) == TRUE) {
          TRUE
        } else {
          metric %in% input$hubMetric
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
        if (is.null(input$hubGeog) == TRUE) {
          TRUE
        } else {
          geographic_level %in% input$hubGeog
        },
        if (is.null(input$hubArea) == TRUE) {
          TRUE
        } else {
          area %in% input$hubArea
        },
        if (is.null(input$hubMetric) == TRUE) {
          TRUE
        } else {
          metric %in% input$hubMetric
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
        (if (is.null(input$hubGeog) == TRUE) {
          TRUE
        } else {
          {
            geographic_level %in% input$hubGeog
          } |
            (if ("Yes" %in% input$hubLA) {
              geographic_level == "LADU"
            } else {
              geographic_level == "xxx"
            }) |
            (if ("National" %in% input$hubComparators) {
              geographic_level == "COUNTRY"
            } else {
              geographic_level == "xxx"
            })
        }),
        (if (is.null(input$hubArea) == TRUE) {
          TRUE
        } else {
          {
            area %in% input$hubArea
          } |
            (if ("Yes" %in% input$hubLA) {
              area %in% (
                C_Geog %>% filter(geog == "LADU", LEP %in% input$hubArea) %>% distinct(areaName)
              )$areaName
            } else {
              geographic_level == "xxx"
            }) |
            (if ("National" %in% input$hubComparators) {
              area == "England"
            } else {
              geographic_level == "xxx"
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
          metric %in% input$hubMetric
        },
        (if (is.null(input$hubBreakdowns) == TRUE) {
          TRUE
        } else {
          breakdown %in% input$hubBreakdowns
        })
      ) %>%
      select(
        Year = time_period,
        Geography = geographic_level,
        Area = area,
        Data = metric,
        Breakdown = breakdown,
        Splits = subgroups,
        Value = value
      )
  })
  
  output$hubTable <- renderDataTable({
    DT::datatable(datahubDataset())
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
  allOptions <- bind_rows(
    data.frame(
      Choice = c("LEP", "LSIP", "MCA", "LA"),
      filterID = "a"
    ),
    C_datahub %>% distinct(Choice = area) %>% mutate(filterID = "b"),
    data.frame(Choice = c("Yes", "No"), filterID = "c"),
    data.frame(
      Choice = c("National", "Regional (to come)"),
      filterID = "d"
    ),
    C_datahub %>% distinct(Choice = metric) %>% mutate(filterID = "e"),
    C_datahub %>% distinct(Choice = breakdown) %>% mutate(filterID = "f"),
    C_datahub %>% distinct(Choice = as.character(time_period)) %>% mutate(filterID = "g")
  ) %>%
    group_by(filterID) %>%
    mutate(ChoiceNo = row_number()) %>%
    mutate(ChoiceID = paste0(filterID, ChoiceNo)) %>%
    ungroup() %>%
    select(-filterID, -ChoiceNo)
  
  output$uniqueCode <- renderUI({
    allOptions %>%
      mutate(
        chosen = case_when(
          Choice %in% input$hubArea ~ 1,
          Choice %in% input$hubMetric ~ 1,
          Choice %in% input$hubGeog ~ 1,
          Choice %in% input$hubComparators ~ 1,
          Choice %in% input$hubLA ~ 1,
          Choice %in% input$hubBreakdowns ~ 1,
          Choice %in% input$hubYears ~ 1,
          TRUE ~ 0
        )
      ) %>%
      filter(chosen == 1) %>%
      select(ChoiceID) %>%
      summarize(
        strong = str_c(ChoiceID, collapse = ""),
        .groups = "drop"
      )
  })
  
  ## 2.6 FE interventions table----
  output$interventionTable <- DT::renderDataTable({
    DT::datatable(
      I_InterventionTable,
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