server <- function(input, output, session) {
  # Loading screen ---------------------------------------------------------------------------
  # Call initial loading screen

  hide(id = "loading-content", anim = TRUE, animType = "fade")
  show("app-content")

  # Load chart colours:https://analysisfunction.civilservice.gov.uk/policy-store/data-visualisation-colours-in-charts/
  # England, geo1, geo2
  chartColors3 <- c("#BFBFBF", "#12436D", "#28A197")
  # geo1, geo2
  chartColors2 <- c("#12436D", "#28A197")
  chartColors6 <- c("#BFBFBF", "#12436D",  "#28A197", "#801650", "#F46A25", "#A285D1","#2073BC")
  #for when no England
  chartColors5 <- c("#12436D",  "#28A197", "#801650", "#F46A25", "#A285D1","#2073BC")

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
      "EnterprisebyemploymentsizeIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list(
        "4a.Enterprise by emp size" = D_empent_UBC1822
      ), path = file)
    }
  )


  output$downloadData7 <- downloadHandler(
    filename = function() {
      "Keystage4destinationsIndicators.xlsx"
    },
    content = function(file) {
      write_xlsx(list(
        "5a.Key Stage 4 destinations" = D_KS4destin_1521
      ), path = file)
    }
  )

  output$downloadData8 <- downloadHandler(
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
    lapply(1:8, function(i) {
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

  # # OVERVIEW ----
  #
  # # alter area dropdown depending if lep or lsip
  # output$lep1_geo <- renderUI({
  #   if (input$GeoType == "LEP") {
  #     selectInput("lep1", "Choose primary LEP area",
  #       choices = C_LEP2020 %>% filter(geographic_level == "LEP") %>% select(Area),
  #       selected = input$lep1
  #     )
  #   } else if (input$GeoType == "LSIP") {
  #     selectInput("lep1", "Choose primary LSIP area",
  #       choices = C_LEP2020 %>% filter(geographic_level == "LSIP") %>% select(Area),
  #       selected = input$lep1
  #     )
  #   } else {
  #     selectInput("lep1", "Choose primary MCA area",
  #       choices = C_LEP2020 %>% filter(geographic_level == "MCA") %>% select(Area),
  #       selected = input$lep1
  #     )
  #   }
  # })
  #
  # # turn off lep 2 for overview page (as not used here)
  # output$lep2_off <- renderUI({
  #   if (input$datatabset == "Overview") {
  #     p("")
  #   } else {
  #     if (input$GeoType == "LEP") {
  #       selectInput("lep2", "Choose comparison LEP area",
  #         choices = c("\nNone", C_LEP2020 %>% filter(geographic_level == "LEP", Area != input$lep1) %>% select(Area)),
  #         selected = input$lep2
  #       )
  #     } else if (input$GeoType == "LSIP") {
  #       selectInput("lep2", "Choose comparison LSIP area",
  #         choices = c("\nNone", C_LEP2020 %>% filter(geographic_level == "LSIP", Area != input$lep1) %>% select(Area)),
  #         selected = input$lep2
  #       )
  #     } else {
  #       selectInput("lep2", "Choose comparison MCA area",
  #         choices = c("\nNone", C_LEP2020 %>% filter(geographic_level == "MCA", Area != input$lep1) %>% select(Area)),
  #         selected = input$lep2
  #       )
  #     }
  #   }
  # })
  #
  #
  # # turn on extra filters where used
  # output$age_on <- renderUI({
  #   selectInput("ageGroup", "Choose age group",
  #     choices = C_Achieve_ILR1621 %>%
  #       filter(
  #         typeNeat %in% if ("typeGroup" %in% names(input)) {
  #           input$typeGroup
  #         } else {
  #           "Total FE and skills provision"
  #         },
  #         level_or_type %in% if ("levelGroup" %in% names(input)) {
  #           input$levelGroup
  #         } else {
  #           "Further education and skills: Total"
  #         }
  #       ) %>%
  #       distinct(Age = age_group),
  #     multiple = FALSE, selected = "Total"
  #   )
  # })
  #
  # output$type_on <- renderUI({
  #   selectizeInput("typeGroup", "Choose type of training",
  #     choices = C_Achieve_ILR1621 %>% distinct(Type = typeNeat),
  #     multiple = FALSE, selected = "Total FE and skills provision"
  #   )
  # })
  #
  # output$level_on <- renderUI({
  #   selectizeInput("levelGroup", "Choose level of training",
  #     choices = C_Achieve_ILR1621 %>%
  #       filter(typeNeat %in% if ("typeGroup" %in% names(input)) {
  #         input$typeGroup
  #       } else {
  #         "Total FE and skills provision"
  #       }) %>% distinct(Level = level_or_type),
  #     multiple = FALSE, selected = "Further education and skills: Total"
  #   )
  # })
  #
  # output$metric_on <- renderUI({
  #   selectizeInput("metricGroup", "Choose metric",
  #     choices = if ("typeGroup" %in% names(input)) {
  #       if ("Apprenticeships (all ages)" %in% input$typeGroup) {
  #         c("Achievements" = "achievements", "Starts (apprenticeships only)" = "starts", "Participation" = "participation")
  #       } else {
  #         c("Achievements" = "achievements", "Participation" = "participation")
  #       }
  #     } else {
  #       c("Achievements" = "achievements", "Participation" = "participation")
  #     }
  #   )
  # })
  #
  # # define page title
  # output$page0title <- renderUI({
  #   paste0("Overview of local landscape in ", input$lep1)
  # })
  #
  # ### Downloads----
  # # download all indicators
  # list_of_datasets0 <- list(
  #   "1a.Emp by occupation" = D_EmpOcc_APS1721,
  #   "1b.Emp rate" = D_EmpRate_APS1822,
  #   "2.Vacancies" = C_Vacancy_ONS1722,
  #   "3a.FE achievements SSA" = D_Achieve_ILR21,
  #   "3b.FE achievements" = D_Achieve_ILR1621,
  #   "4a.Enterprise by emp size" = D_empent_UBC1822,
  #   "5a.Key Stage 4 destinations" = D_KS4destin_1521,
  #   "6a.Key Stage 5 destinations" = D_KS5destin_1721
  # )
  # output$download_btn0a <- downloadHandler(
  #   filename = function() {
  #     "CoreIndicators.xlsx"
  #   },
  #   content = function(file) {
  #     write_xlsx(list_of_datasets0, path = file)
  #   }
  # )
  #
  # # Download current LEP indicators
  # filtered_data0 <- reactive({
  #   list(
  #     "1a.Emp by occupation" = filter(D_EmpOcc_APS1721, geographic_level == input$GeoType, area == input$lep1),
  #     "1b.Emp rate" = filter(D_EmpRate_APS1822, geographic_level == input$GeoType, area == input$lep1),
  #     "2.Vacancies" = filter(C_Vacancy_ONS1722, geographic_level == input$GeoType, area == input$lep1),
  #     "3a.FE achievements SSA" = filter(D_Achieve_ILR21, geographic_level == input$GeoType, area == input$lep1),
  #     "3b.FE achievements" = filter(D_Achieve_ILR1621, geographic_level == input$GeoType, area == input$lep1)
  #   )
  # })
  # output$download_btn0b <- downloadHandler(
  #   filename = function() {
  #     "CurrentIndicators.xlsx"
  #   },
  #   content = function(file) {
  #     write_xlsx(filtered_data0(), path = file)
  #   }
  # )
  #
  # ## KPIs and charts----
  #
  # # get emp data for current lep
  # empLEP <- eventReactive(input$lep1, {
  #   C_EmpRate_APS1822 %>%
  #     filter(area == input$lep1, geographic_level == input$GeoType)
  # })
  # # get 2022 values
  # emp2022 <- reactive({
  #   empLEP() %>%
  #     filter(year == "2022")
  # })
  # # get 2021 values
  # emp2021 <- reactive({
  #   empLEP() %>%
  #     filter(year == "2021")
  # })
  #
  # #### Employment count ----
  # output$locland.emplcnt0 <- renderUI({
  #   # call 2022 and 2021 values for chosen LEP
  #   empCnt2022 <- emp2022()$Employment
  #   empCntChange <- emp2022()$Employment - emp2021()$Employment
  #
  #   # print with formatting
  #   h4(span("Jul-Jun 2022", style = "font-size: 16px;font-weight:normal;"), br(),
  #     format(empCnt2022, big.mark = ","), br(),
  #     span(
  #       format_pm(empCntChange) # plus-minus and comma sep formatting
  #       ,
  #       style = paste0("font-size: 16px;color:", cond_color(empCntChange > 0)) # colour formating
  #
  #       , .noWS = c("before", "after") # remove whitespace
  #     ), br(),
  #     style = "font-size: 21px"
  #   )
  # })
  #
  # # Emp chart
  # empLineChart <- eventReactive(input$lep1, {
  #   # call 2022 to 2021 change  for chosen LEP
  #   empCntChange <- emp2022()$Employment - emp2021()$Employment
  #   empLine <- empLEP()
  #
  #   # find min and max for lep
  #   empCntMinMax <- C_EmpRate_APS1822_max_min %>%
  #     filter(area == input$lep1)
  #
  #   ggplot(empLine, aes(x = Year - 1, y = Employment, group = area, text = paste0(
  #     "Year: Jul-Jun ", year, "<br>",
  #     "Employment: ", format(Employment, big.mark = ","), "<br>"
  #   ))) +
  #     geom_line(data = empLine %>% filter(Year <= 21)) +
  #     geom_ribbon(
  #       data = empLine %>% filter(Year >= 21),
  #       aes(ymin = min(Employment), ymax = Employment),
  #       fill = ifelse(empCntChange > 0, "#00703c", "#d4351c"),
  #       alpha = 0.3
  #     ) +
  #     geom_line(
  #       data = empLine %>% filter(Year >= 21),
  #       color = ifelse(empCntChange > 0, "#00703c", "#d4351c")
  #     ) +
  #     theme_classic() +
  #     theme(
  #       axis.line = element_blank(),
  #       axis.ticks = element_blank(),
  #       axis.title = element_blank(),
  #       panel.background = element_rect(fill = "#f3f2f1"),
  #       plot.background = element_rect(fill = "#f3f2f1")
  #     ) +
  #     scale_y_continuous(
  #       labels = label_number_si(accuracy = 1),
  #       breaks = c(empCntMinMax$minEmp, empCntMinMax$maxEmp)
  #     )
  # })
  # # set margins
  # m <- list(
  #   l = 0,
  #   r = 4, # increase this margin a bit to prevent the last lable dissapearing
  #   b = 0,
  #   t = 0,
  #   pad = 0
  # )
  #
  # output$empLineChart <- renderPlotly({
  #   validate(
  #     need(input$lep1 != "", "") # if area not yet loaded don't try to load ch
  #   )
  #   ggplotly(empLineChart(),
  #     tooltip = "text",
  #     height = 81
  #   ) %>%
  #     layout(
  #       margin = m,
  #       xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
  #     ) %>% # disable zooming because it's awful on mobile
  #     config(displayModeBar = FALSE)
  # })
  #
  # #### Employment rate -----
  # output$locland.emplrate0 <- renderUI({
  #   # call 2021 values and 21-22 change for chosen LEP
  #   empRate2022 <- emp2022()$empRate
  #   empRateChange <- emp2022()$empRate - emp2021()$empRate
  #
  #   # print with formatting
  #   h4(span("Jul-Jun 2022", style = "font-size: 16px;font-weight:normal;"), br(),
  #     paste0(format(100 * empRate2022, digit = 2), "%"), br(),
  #     span(
  #       paste0(sprintf("%+.0f", 100 * empRateChange), "ppts"),
  #       style = paste0("font-size: 16px;color:", cond_color(empRateChange > 0)) # colour formating
  #       , .noWS = c("before", "after") # remove whitespace
  #     ), br(),
  #     style = "font-size: 21px"
  #   )
  # })
  #
  # # Emp chart
  #
  # # find emp chart y axis min and max
  # EmpRateMin <- C_EmpRate_APS1822 %>%
  #   summarise(min(empRate, na.rm = T), .groups = "drop")
  # EmpRateMax <- C_EmpRate_APS1822 %>%
  #   summarise(max(empRate, na.rm = T), .groups = "drop")
  #
  # empRateLineChart <- eventReactive(input$lep1, {
  #   empRateChange <- emp2022()$empRate - emp2021()$empRate
  #   empRateLine <- C_EmpRate_APS1822 %>%
  #     filter((geographic_level == input$GeoType & area == input$lep1) | (geographic_level == "COUNTRY" & area == "England"))
  #
  #   ggplot(empRateLine, aes(
  #     x = Year - 1, y = empRate,
  #     group = area,
  #     text = paste0(
  #       "Year: Jul-Jun ", year, "<br>",
  #       "Area: ", area, "<br>",
  #       "Employment rate: ", format(100 * empRate, digit = 2), "%<br>"
  #     )
  #   )) +
  #     geom_line(data = empRateLine %>% filter(Year <= 21, geographic_level == input$GeoType)) +
  #     geom_line(data = empRateLine %>% filter(geographic_level == "COUNTRY"), alpha = 0.5) +
  #     geom_ribbon(
  #       data = empRateLine %>% filter(Year >= 21, geographic_level == input$GeoType),
  #       aes(ymin = min(empRate), ymax = empRate),
  #       fill = ifelse(empRateChange > 0, "#00703c", "#d4351c"),
  #       alpha = 0.3
  #     ) +
  #     geom_line(
  #       data = empRateLine %>% filter(Year >= 21, geographic_level == input$GeoType),
  #       color = ifelse(empRateChange > 0, "#00703c", "#d4351c")
  #     ) +
  #     theme_classic() +
  #     theme(
  #       axis.line = element_blank(),
  #       axis.ticks = element_blank(),
  #       axis.title = element_blank(),
  #       panel.background = element_rect(fill = "#f3f2f1"),
  #       plot.background = element_rect(fill = "#f3f2f1")
  #     ) +
  #     scale_y_continuous(
  #       labels = scales::percent_format(accuracy = 1),
  #       breaks = c(EmpRateMin[1, 1], EmpRateMax[1, 1]),
  #       limits = c(EmpRateMin[1, 1], EmpRateMax[1, 1])
  #     )
  # })
  # # set margins
  # m <- list(
  #   l = 0,
  #   r = 4, # increase this margin a bit to prevent the last lable dissapearing
  #   b = 0,
  #   t = 0,
  #   pad = 0
  # )
  #
  # output$empRateLineChart <- renderPlotly({
  #   ggplotly(empRateLineChart(),
  #     tooltip = "text",
  #     height = 81
  #   ) %>%
  #     layout(
  #       margin = m,
  #       xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
  #     ) %>% # disable zooming because it's awful on mobile
  #     config(displayModeBar = FALSE)
  # })
  #
  # # Add link to employment data
  # observeEvent(input$link_to_tabpanel_employment2, {
  #   updateTabsetPanel(session, "navbar", "Dashboard")
  #   updateTabsetPanel(session, "datatabset", "Employment")
  # })
  #
  # #### ONS job advert units  ----
  # # get vac data for current area chosen
  # VacArea <- eventReactive(input$lep1, {
  #   C_Vacancy_England %>%
  #     filter(area == input$lep1, geographic_level == input$GeoType)
  # })
  # # get 2022 values
  # Vac2022 <- reactive({
  #   VacArea() %>%
  #     filter(year == "2022")
  # })
  # # get 2021 values
  # Vac2021 <- reactive({
  #   VacArea() %>%
  #     filter(year == "2021")
  # })
  #
  # # Vacancy kpi
  # output$jobad.units <- renderUI({
  #   ### ONS job advert units change
  #   VacPcChange <- Vac2022()$jobpc - Vac2021()$jobpc
  #
  #   # print with formatting
  #   h4(span("Jan 2022", style = "font-size: 16px;font-weight:normal;"), br(),
  #     paste0(format(100 * Vac2022()$jobpc, digit = 2), "%"), br(),
  #     span(
  #       paste0(sprintf("%+.1f", 100 * VacPcChange), "ppts"),
  #       style = paste0("font-size: 16px;color:", cond_color(VacPcChange > 0)) # colour formating
  #       , .noWS = c("before", "after") # remove whitespace
  #     ), br(),
  #     style = "font-size: 21px"
  #   )
  # })
  #
  # # Vacancy chart
  # VacLineChart <- eventReactive(input$lep1, {
  #   VacLine <- VacArea() %>% filter(year >= 2018)
  #   VacPcChange <- Vac2022()$jobpc - Vac2021()$jobpc
  #
  #   VacMinMax <- C_Vacancy_England_max_min %>% filter(area == input$lep1, geographic_level == input$GeoType)
  #
  #   ggplot(VacLine, aes(x = Year, y = jobpc, group = area, text = paste0(
  #     "Period: Jan ", year, "<br>",
  #     "England vacancy share: ", format(100 * jobpc, digit = 2), "%<br>"
  #   ))) +
  #     geom_line(data = VacLine %>% filter(Year <= 21)) +
  #     geom_ribbon(
  #       data = VacLine %>% filter(Year >= 21),
  #       aes(ymin = min(jobpc), ymax = jobpc),
  #       fill = ifelse(VacPcChange > 0, "#00703c", "#d4351c"),
  #       alpha = 0.3
  #     ) +
  #     geom_line(
  #       data = VacLine %>% filter(Year >= 21),
  #       color = ifelse(VacPcChange > 0, "#00703c", "#d4351c")
  #     ) +
  #     theme_classic() +
  #     theme(
  #       axis.line = element_blank(),
  #       axis.ticks = element_blank(),
  #       axis.title = element_blank(),
  #       panel.background = element_rect(fill = "#f3f2f1"),
  #       plot.background = element_rect(fill = "#f3f2f1")
  #     ) +
  #     scale_y_continuous(
  #       labels = scales::percent_format(accuracy = 0.1),
  #       breaks = c(VacMinMax$minVac, VacMinMax$maxVac)
  #     )
  # })
  # # set margins
  # m <- list(
  #   l = 0,
  #   r = 4, # increase this margin a bit to prevent the last lable dissapearing
  #   b = 0,
  #   t = 0,
  #   pad = 0
  # )
  #
  # output$VacLineChart <- renderPlotly({
  #   ggplotly(VacLineChart(),
  #     tooltip = "text",
  #     height = 81
  #   ) %>%
  #     layout(
  #       margin = m,
  #       xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
  #     ) %>% # disable zooming because it's awful on mobile
  #     config(displayModeBar = FALSE)
  # })
  #
  # # Add link to vacancy data
  # observeEvent(input$link_to_tabpanel_vacancies2, {
  #   updateTabsetPanel(session, "navbar", "Dashboard")
  #   updateTabsetPanel(session, "datatabset", "Vacancies")
  # })
  #
  # #### E&T achievements ----
  #
  # # get EandT data for current lep
  # EtLEP <- eventReactive(input$lep1, {
  #   C_Achieve_ILR1621 %>%
  #     filter(
  #       geographic_level == input$GeoType,
  #       area == input$lep1,
  #       level_or_type == "Education and training: Total",
  #       age_group == "Total"
  #     )
  # })
  # # get 20/21 values
  # Et2021 <- reactive({
  #   EtLEP() %>%
  #     filter(time_period == "202021")
  # })
  # # get 19/20 values
  # Et1920 <- reactive({
  #   EtLEP() %>%
  #     filter(time_period == "201920")
  # })
  #
  # output$skisup.ETach <- renderUI({
  #   ETach <- Et2021()$achievements
  #
  #   # E&T achievements change
  #   ETachChange <- Et2021()$achievements - Et1920()$achievements
  #
  #   # print with formatting
  #   h4(span("2020/21", style = "font-size: 16px;font-weight:normal;"), br(),
  #     format(ETach, big.mark = ","), br(),
  #     span(
  #       format_pm(ETachChange) # plus-minus and comma sep formatting
  #       ,
  #       style = paste0("font-size: 16px;color:", cond_color(ETachChange > 0)) # colour formating
  #       , .noWS = c("before", "after") # remove whitespace
  #     ), br(),
  #     style = "font-size: 21px"
  #   )
  # })
  #
  # # e and t chart
  # etLineChart <- eventReactive(input$lep1, {
  #   etLine <- EtLEP()
  #   etCntChange <- Et2021()$achievements - Et1920()$achievements
  #   EtMinMax <- C_Achieve_ILR1621_max_min %>% filter(
  #     geographic_level == input$GeoType,
  #     area == input$lep1,
  #     level_or_type == "Education and training: Total"
  #   )
  #
  #   ggplot(etLine, aes(x = Year, y = achievements, group = area, text = paste0(
  #     "Academic year: ", time_period, "<br>",
  #     "Achievements: ", format(achievements, big.mark = ","), "<br>"
  #   ))) +
  #     geom_line(data = etLine %>% filter(Year <= 19)) +
  #     geom_ribbon(
  #       data = etLine %>% filter(Year >= 19),
  #       aes(ymin = min(achievements), ymax = achievements),
  #       fill = ifelse(etCntChange > 0, "#00703c", "#d4351c"),
  #       alpha = 0.3
  #     ) +
  #     geom_line(
  #       data = etLine %>% filter(Year >= 19),
  #       color = ifelse(etCntChange > 0, "#00703c", "#d4351c")
  #     ) +
  #     # add a blank line for the formatted tooltip
  #     theme_classic() +
  #     theme(
  #       axis.line = element_blank(),
  #       # axis.text.y = element_blank(),
  #       axis.ticks = element_blank(),
  #       axis.title = element_blank(),
  #       panel.background = element_rect(fill = "#f3f2f1"),
  #       plot.background = element_rect(fill = "#f3f2f1")
  #     ) +
  #     scale_y_continuous(
  #       labels = label_number_si(accuracy = 1),
  #       breaks = c(EtMinMax$minAch, EtMinMax$maxAch)
  #     )
  # })
  # # set margins
  # m <- list(
  #   l = 0,
  #   r = 4, # increase this margin a bit to prevent the last lable dissapearing
  #   b = 0,
  #   t = 0,
  #   pad = 0
  # )
  #
  # output$etLineChart <- renderPlotly({
  #   ggplotly(etLineChart(),
  #     tooltip = "text",
  #     height = 81
  #   ) %>%
  #     layout(
  #       margin = m,
  #       xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
  #     ) %>% # disable zooming because it's awful on mobile
  #     config(displayModeBar = FALSE)
  # })
  #
  # #### App achievements ----
  # # get App data for current lep
  # AppLEP <- eventReactive(input$lep1, {
  #   C_Achieve_ILR1621 %>%
  #     filter(
  #       geographic_level == input$GeoType,
  #       area == input$lep1,
  #       level_or_type == "Apprenticeships: Total",
  #       age_group == "Total"
  #     )
  # })
  # # get 20/21 values
  # App2021 <- reactive({
  #   AppLEP() %>%
  #     filter(time_period == "202021")
  # })
  # # get 19/20 values
  # App1920 <- reactive({
  #   AppLEP() %>%
  #     filter(time_period == "201920")
  # })
  # output$skisup.APPach <- renderUI({
  #   Appach <- App2021()$achievements
  #
  #   # E&T achievements change
  #   AppachChange <- App2021()$achievements - App1920()$achievements
  #
  #   # print with formatting
  #   h4(span("2020/21", style = "font-size: 16px;font-weight:normal;"), br(),
  #     format(Appach, big.mark = ","), br(),
  #     span(
  #       format_pm(AppachChange) # plus-minus and comma sep formatting
  #       ,
  #       style = paste0("font-size: 16px;color:", cond_color(AppachChange > 0)) # colour formating
  #       , .noWS = c("before", "after") # remove whitespace
  #     ), br(),
  #     style = "font-size: 21px"
  #   )
  # })
  #
  # # app chart
  # AppLineChart <- eventReactive(input$lep1, {
  #   AppLine <- AppLEP()
  #   AppCntChange <- App2021()$achievements - App1920()$achievements
  #   AppMinMax <- C_Achieve_ILR1621_max_min %>% filter(
  #     geographic_level == input$GeoType,
  #     area == input$lep1,
  #     level_or_type == "Apprenticeships: Total"
  #   )
  #
  #
  #   ggplot(AppLine, aes(x = Year, y = achievements, group = area, text = paste0(
  #     "Academic year: ", time_period, "<br>",
  #     "Achievements: ", format(achievements, big.mark = ","), "<br>"
  #   ))) +
  #     geom_line(data = AppLine %>% filter(Year <= 19)) +
  #     geom_ribbon(
  #       data = AppLine %>% filter(Year >= 19),
  #       aes(ymin = min(achievements), ymax = achievements),
  #       fill = ifelse(AppCntChange > 0, "#00703c", "#d4351c"),
  #       alpha = 0.3
  #     ) +
  #     geom_line(
  #       data = AppLine %>% filter(Year >= 19),
  #       color = ifelse(AppCntChange > 0, "#00703c", "#d4351c")
  #     ) +
  #     # add a blank line for the formatted tooltip
  #     theme_classic() +
  #     theme(
  #       axis.line = element_blank(),
  #       # axis.text.y = element_blank(),
  #       axis.ticks = element_blank(),
  #       axis.title = element_blank(),
  #       panel.background = element_rect(fill = "#f3f2f1"),
  #       plot.background = element_rect(fill = "#f3f2f1")
  #     ) +
  #     scale_y_continuous(
  #       labels = label_number_si(accuracy = 1),
  #       breaks = c(AppMinMax$minAch, AppMinMax$maxAch)
  #     )
  # })
  # # set margins
  # m <- list(
  #   l = 0,
  #   r = 4, # increase this margin a bit to prevent the last lable dissapearing
  #   b = 0,
  #   t = 0,
  #   pad = 0
  # )
  #
  # output$AppLineChart <- renderPlotly({
  #   ggplotly(AppLineChart(),
  #     tooltip = "text",
  #     height = 81
  #   ) %>%
  #     layout(
  #       margin = m,
  #       xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
  #     ) %>% # disable zooming because it's awful on mobile
  #     config(displayModeBar = FALSE)
  # })
  #
  # # Add link to skills data
  # observeEvent(input$link_to_tabpanel_FE2, {
  #   updateTabsetPanel(session, "navbar", "Dashboard")
  #   updateTabsetPanel(session, "datatabset", "Skills")
  # })
  #
  # # EMPLOYMENT ----
  # # define page title
  # output$page1title <- renderUI({
  #   paste0(
  #     "Employment in ", input$lep1,
  #     if ("lep2" %in% names(input)) {
  #       if (input$lep2 == "\nNone") {
  #       } else {
  #         paste0(" compared to ", input$lep2)
  #       }
  #     }
  #   )
  # })
  #
  # ### Downloads----
  # list_of_datasets1 <- list(
  #   "1a.Emp by occupation" = D_EmpOcc_APS1721,
  #   "1b.Emp rate" = D_EmpRate_APS1822
  # )
  # output$download_btn1a <- downloadHandler(
  #   filename = function() {
  #     "EmploymentIndicators.xlsx"
  #   },
  #   content = function(file) {
  #     write_xlsx(list_of_datasets1, path = file)
  #   }
  # )
  #
  # # Download current LEP indicators
  # filtered_data1 <- reactive({
  #   list(
  #     "1a.Emp by occupation" = filter(D_EmpOcc_APS1721, geographic_level == input$GeoType, area == input$lep1),
  #     "1b.Emp rate" = filter(D_EmpRate_APS1822, geographic_level == input$GeoType, area == input$lep1)
  #   )
  # })
  # output$download_btn1b <- downloadHandler(
  #   filename = function() {
  #     "CurrentEmploymentIndicators.xlsx"
  #   },
  #   content = function(file) {
  #     write_xlsx(filtered_data1(), path = file)
  #   }
  # )
  #
  # ## KPIs ----
  #
  # ### Employment rate -----
  # output$locland.emplrate <- renderValueBox({
  #   div(
  #     class = "col-sm-4",
  #     div(
  #       class = "small-box bg-geo1",
  #       div(
  #         class = "inner",
  #         h3(paste0(
  #           format(100. * emp2022()$empRate, digits = 2),
  #           "%"
  #         )),
  #         p(paste0("employment rate Jul-Jun 2022 in ", input$lep1)),
  #       )
  #     )
  #   )
  # })
  #
  # output$locland.emplrate.2 <- renderValueBox({
  #   div(
  #     class = "col-sm-4",
  #     div(
  #       class = "small-box bg-geo2",
  #       div(
  #         class = "inner",
  #         h3(paste0(
  #           format(100. * (C_EmpRate_APS1822 %>%
  #             filter(
  #               geographic_level == input$GeoType,
  #               area == input$lep2,
  #               year == "2022"
  #             )
  #           )$empRate, digits = 2),
  #           "%"
  #         )),
  #         p(paste0("employment rate Jul-Jun 2022 in ", input$lep2)),
  #       )
  #     )
  #   )
  # })
  # ### Employment count ----
  # output$locland.emplcnt <- renderValueBox({
  #   div(
  #     class = "col-sm-4",
  #     div(
  #       class = "small-box bg-geo1",
  #       div(
  #         class = "inner",
  #         h3(format(emp2022()$Employment,
  #           scientific = FALSE, big.mark = ","
  #         )),
  #         p(paste0("in employment Jul-Jun 2022 in ", input$lep1)),
  #       )
  #     )
  #   )
  # })
  #
  # output$locland.emplcnt.2 <- renderValueBox({
  #   div(
  #     class = "col-sm-4",
  #     div(
  #       class = "small-box bg-geo2",
  #       div(
  #         class = "inner",
  #         h3(format((C_EmpRate_APS1822 %>%
  #           filter(
  #             geographic_level == input$GeoType,
  #             area == input$lep2,
  #             year == "2022"
  #           )
  #         )$Employment,
  #         scientific = FALSE, big.mark = ","
  #         )),
  #         p(paste0("in employment Jul-Jun 2022 in ", input$lep2)),
  #       )
  #     )
  #   )
  # })
  #
  # # turn off comparison boxes if none is selected
  # output$emp_comp <- renderUI({
  #   if ("lep2" %in% names(input)) {
  #     if (input$lep2 == "\nNone") {
  #       tagList(
  #         br(),
  #         p("")
  #       )
  #     } else {
  #       tagList(
  #         valueBoxOutput("locland.emplcnt.2"),
  #         valueBoxOutput("locland.emplrate.2")
  #       )
  #     }
  #   } else {
  #     p("")
  #   }
  # })
  #
  # ## Employment rate over time line graph ----
  # EmpRate_time <- eventReactive(c(input$lep1, input$lep2), {
  #   EmpRateTime <- C_EmpRate_APS1822 %>%
  #     select(year, area, geographic_level, empRate) %>%
  #     filter(
  #       geographic_level == input$GeoType | geographic_level == "COUNTRY",
  #       (area == "England" |
  #         area == input$lep1 |
  #         area == if ("lep2" %in% names(input)) {
  #           input$lep2
  #         } else {
  #           "\nNone"
  #         })
  #     )
  #   # add an extra column so the colours work in ggplot when sorting alphabetically
  #   EmpRateTime$Areas <- factor(EmpRateTime$area,
  #     levels = c("England", input$lep1, input$lep2)
  #   )
  #
  #   ggplot(
  #     EmpRateTime,
  #     aes(
  #       x = year - 1, y = empRate,
  #       color = Areas, group = Areas,
  #       text = paste0(
  #         "Year: Jul-Jun ", year, "<br>",
  #         "Area: ", Areas, "<br>",
  #         "Employment rate: ", scales::percent(round(empRate, 2)), "<br>"
  #       )
  #     )
  #   ) +
  #     geom_line() +
  #     theme_minimal() +
  #     theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "bottom", legend.title = element_blank()) +
  #     scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(.65, .85)) +
  #     labs(colour = "") +
  #     scale_color_manual(values = chartColors3)
  # })
  #
  # output$EmpRate_time <- renderPlotly({
  #   ggplotly(EmpRate_time(), tooltip = "text") %>%
  #     layout(
  #       legend = list(orientation = "h", x = 0, y = -0.1),
  #       xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
  #     ) %>% # disable zooming because it's awful on mobile
  #     config(displayModeBar = FALSE)
  # })
  #
  # ## Employment by occupation data table ----
  # EmpOcc <- eventReactive(c(input$lep1, input$lep2), {
  #   EmpOcc <- C_EmpOcc_APS1721 %>%
  #     filter(
  #       geographic_level == input$GeoType | geographic_level == "COUNTRY",
  #       (area == "England" |
  #         area == input$lep1 |
  #         area == if (("lep2" %in% names(input))) {
  #           input$lep2
  #         } else {
  #           "\nNone"
  #         })
  #     ) %>%
  #     select(-year, -geographic_level) %>%
  #     rename_with(str_to_sentence) %>%
  #     t() %>%
  #     row_to_names(row_number = 1) %>%
  #     as.data.frame() %>%
  #     mutate_if(is.character, as.numeric) %>%
  #     mutate_at(c(2), ~ replace(., is.na(.), 0)) %>%
  #     mutate(across(where(is.numeric), ~ round(prop.table(.), 4))) %>%
  #     filter(.[[2]] > 0) %>%
  #     rownames_to_column("Occupation") %>%
  #     relocate(input$lep1, .after = England)
  #   # %>%
  #   # filter(Occupation != "Alloccsremain")
  # })
  #
  # output$EmpOcc <- renderDataTable({
  #   df <- EmpOcc()
  #   datatable(df, options = list(order = list(2, "desc")), rownames = FALSE) %>%
  #     formatPercentage(2:ncol(df), 0)
  # })
  #
  # # VACANCIES ----
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
  #
  # ### Downloads----
  # # download skills indicators
  # list_of_datasets3 <- list("2.Vacancies" = C_Vacancy_ONS1722)
  # output$download_btn3a <- downloadHandler(
  #   filename = function() {
  #     "VacancyIndicators.xlsx"
  #   },
  #   content = function(file) {
  #     write_xlsx(list_of_datasets3, path = file)
  #   }
  # )
  #
  # # Download current LEP indicators
  # filtered_data3 <- reactive({
  #   list("2.Vacancies" = filter(C_Vacancy_ONS1722, geographic_level == input$GeoType, area == input$lep1))
  # })
  # output$download_btn3b <- downloadHandler(
  #   filename = function() {
  #     "CurrentVacancyIndicators.xlsx"
  #   },
  #   content = function(file) {
  #     write_xlsx(filtered_data3(), path = file)
  #   }
  # )
  #
  # ## KPIs ----
  # ### ONS job advert unit percent of total area 1
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
  #           format(100. *
  #             (C_Vacancy_England %>%
  #               filter(area == input$lep2, geographic_level == input$GeoType, year == "2022"))$jobpc,
  #           digits = 2
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
  #           format(100. * (C_Vacancy_England_change %>%
  #             filter(area == input$lep1, geographic_level == input$GeoType))$Percentage_Change,
  #           digits = 2
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
  #           format(100. * (C_Vacancy_England_change %>%
  #             filter(area == input$lep2, geographic_level == input$GeoType))$Percentage_Change,
  #           digits = 2
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
  # ## Online job vacancy units over time line chart ----
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
  #
  #
  # # FE ----
  # # define page title
  # output$page2title <- renderUI({
  #   paste0(
  #     "Skills training in ", input$lep1,
  #     if ("lep2" %in% names(input)) {
  #       if (input$lep2 == "\nNone") {
  #       } else {
  #         paste0(" compared to ", input$lep2)
  #       }
  #     }
  #   )
  # })
  #
  # ### Downloads----
  # # download skills indicators
  # list_of_datasets2 <- list(
  #   "3a.FE achievements SSA" = D_Achieve_ILR21,
  #   "3b.FE achievements" = D_Achieve_ILR1621
  # )
  # output$download_btn2a <- downloadHandler(
  #   filename = function() {
  #     "SkillIndicators.xlsx"
  #   },
  #   content = function(file) {
  #     write_xlsx(list_of_datasets2, path = file)
  #   }
  # )
  #
  # # Download current LEP indicators
  # filtered_data2 <- reactive({
  #   list(
  #     "3a.FE achievements SSA" = filter(D_Achieve_ILR21, geographic_level == input$GeoType, area == input$lep1),
  #     "3b.FE achievements" = filter(D_Achieve_ILR1621, geographic_level == input$GeoType, area == input$lep1)
  #   )
  # })
  # output$download_btn2b <- downloadHandler(
  #   filename = function() {
  #     "CurrentSkillIndicators.xlsx"
  #   },
  #   content = function(file) {
  #     write_xlsx(filtered_data2(), path = file)
  #   }
  # )
  #
  # ## KPIs ----
  # ### FE achievements -----
  # output$skisup.FEach <- renderValueBox({
  #   div(
  #     class = "col-sm-4",
  #     div(
  #       class = "small-box bg-geo1",
  #       div(
  #         class = "inner",
  #         h3(format((C_Achieve_ILR1621 %>%
  #           filter(
  #             geographic_level == input$GeoType,
  #             area == input$lep1, time_period == "202021",
  #             level_or_type %in% if ("levelGroup" %in% names(input) & !"Further education and skills: Total" %in% input$levelGroup) {
  #               input$levelGroup
  #             } else {
  #               "Further education and skills: Total"
  #             },
  #             age_group %in% if ("ageGroup" %in% names(input) & !"Total" %in% input$ageGroup) {
  #               input$ageGroup
  #             } else {
  #               "Total"
  #             }
  #           ) %>%
  #           select(input$metricGroup)
  #         )[1, 1], scientific = FALSE, big.mark = ",")),
  #         p(paste0(input$metricGroup, " in 2020/21 in ", input$lep1)),
  #       )
  #     )
  #   )
  # })
  #
  # output$skisup.FEach.2 <- renderValueBox({
  #   div(
  #     class = "col-sm-4",
  #     div(
  #       class = "small-box bg-geo2",
  #       div(
  #         class = "inner",
  #         h3(format((C_Achieve_ILR1621 %>%
  #           filter(
  #             geographic_level == input$GeoType,
  #             area == input$lep2, time_period == "202021",
  #             level_or_type %in% if ("levelGroup" %in% names(input) & !"Further education and skills: Total" %in% input$levelGroup) {
  #               input$levelGroup
  #             } else {
  #               "Further education and skills: Total"
  #             },
  #             age_group %in% if ("ageGroup" %in% names(input) & !"Total" %in% input$ageGroup) {
  #               input$ageGroup
  #             } else {
  #               "Total"
  #             }
  #           ) %>%
  #           select(input$metricGroup)
  #         )[1, 1], scientific = FALSE, big.mark = ",")),
  #         p(paste0(input$metricGroup, " in 2020/21 in ", input$lep2)),
  #       )
  #     )
  #   )
  # })
  #
  # ### Apprenticeship achievements ----
  # output$skisup.APach <- renderValueBox({
  #   div(
  #     class = "col-sm-4",
  #     div(
  #       class = "small-box bg-geo1",
  #       div(
  #         class = "inner",
  #         h3(format(round((C_Achieve_ILR1621 %>%
  #           filter(
  #             geographic_level == input$GeoType,
  #             area == input$lep1, time_period == "202021",
  #             level_or_type %in% if ("levelGroup" %in% names(input) & !"Further education and skills: Total" %in% input$levelGroup) {
  #               input$levelGroup
  #             } else {
  #               "Further education and skills: Total"
  #             },
  #             age_group %in% if ("ageGroup" %in% names(input) & !"Total" %in% input$ageGroup) {
  #               input$ageGroup
  #             } else {
  #               "Total"
  #             }
  #           ) %>%
  #           select(if ("metricGroup" %in% names(input)) {
  #             paste0(input$metricGroup, "_rate_per_100000_population")
  #           } else {
  #             "achievements_rate_per_100000_population"
  #           })
  #         )[1, 1], 0), scientific = FALSE, big.mark = ",", nsmall = 0)),
  #         p(paste0(input$metricGroup, " rate per 100,000 in 2020/21 in ", input$lep1)),
  #       )
  #     )
  #   )
  # })
  #
  # output$skisup.APach.2 <- renderValueBox({
  #   div(
  #     class = "col-sm-4",
  #     div(
  #       class = "small-box bg-geo2",
  #       div(
  #         class = "inner",
  #         h3(format(round((C_Achieve_ILR1621 %>%
  #           filter(
  #             geographic_level == input$GeoType,
  #             area == input$lep2, time_period == "202021",
  #             level_or_type == if ("levelGroup" %in% names(input)) {
  #               input$levelGroup
  #             } else {
  #               "Further education and skills: Total"
  #             },
  #             age_group %in% if ("ageGroup" %in% names(input) & !"Total" %in% input$ageGroup) {
  #               input$ageGroup
  #             } else {
  #               "Total"
  #             }
  #           ) %>%
  #           select(if ("metricGroup" %in% names(input)) {
  #             paste0(input$metricGroup, "_rate_per_100000_population")
  #           } else {
  #             "achievements_rate_per_100000_population"
  #           })
  #         )[1, 1], 0), scientific = FALSE, big.mark = ",", nsmall = 0)),
  #         p(paste0(input$metricGroup, " rate per 100,000 in 2020/21 in ", input$lep2)),
  #       )
  #     )
  #   )
  # })
  #
  # output$skisup.APach.national <- renderValueBox({
  #   div(
  #     class = "col-sm-4",
  #     div(
  #       class = "small-box bg-geo3",
  #       div(
  #         class = "inner",
  #         h3(format((C_Achieve_ILR1621 %>%
  #           filter(
  #             geographic_level == "National",
  #             time_period == "202021",
  #             level_or_type == if ("levelGroup" %in% names(input)) {
  #               input$levelGroup
  #             } else {
  #               "Further education and skills: Total"
  #             },
  #             age_group == if ("ageGroup" %in% names(input)) {
  #               input$ageGroup
  #             } else {
  #               "Total"
  #             }
  #           ) %>%
  #           select(if ("metricGroup" %in% names(input)) {
  #             paste0(input$metricGroup, "_rate_per_100000_population")
  #           } else {
  #             "achievements_rate_per_100000_population"
  #           })
  #         )[1, 1], scientific = FALSE, big.mark = ",")),
  #         p(input$metricGroup, " rate per 100,000 in 2020/21 in England"),
  #       )
  #     )
  #   )
  # })
  #
  # # turn off comparison boxes if none is selected
  # output$skill_comp <- renderUI({
  #   if ("lep2" %in% names(input)) {
  #     if (input$lep2 == "\nNone") {
  #     } else {
  #       tagList(
  #         valueBoxOutput("skisup.FEach.2"),
  #         valueBoxOutput("skisup.APach.2")
  #       )
  #     }
  #   } else {
  #   }
  # })
  #
  # ## Achievements over time line chart ----
  # # title
  # output$feLineTitle <- renderUI({
  #   paste0(str_to_sentence(input$metricGroup), ": 2016/17 to 2020/21")
  # })
  #
  # Ach_time <- eventReactive(c(input$lep1, input$lep2, input$levelGroup, input$ageGroup, input$metricGroup), { # , input$splitLine), {
  #   validate(
  #     need(input$ageGroup != "", "") # if area not yet loaded don't try to load ch
  #   )
  #   FETime <- C_Achieve_ILR1621 %>%
  #     filter(
  #       geographic_level == input$GeoType,
  #       (area == input$lep1 |
  #         (area == if ("lep2" %in% names(input)) {
  #           input$lep2
  #         } else {
  #           "\nNone"
  #         })
  #       ),
  #       #      if(input$splitLine=="typeNeat")
  #       #        {typeNeat!="Total FE and skills provision"}
  #       #      else{
  #       typeNeat == if ("typeGroup" %in% names(input)) {
  #         input$typeGroup
  #       } else {
  #         "Total FE and skills provision"
  #         # }
  #       },
  #       #   if(input$splitLine=="level_or_type"){level_or_type!="Total"}else{
  #       level_or_type == if ("levelGroup" %in% names(input)) {
  #         input$levelGroup
  #       } else {
  #         "Further education and skills: Total"
  #         #   }
  #       },
  #       #   if(input$splitLine=="age_group"){age_group!="Total"}else{
  #       age_group == if ("ageGroup" %in% names(input)) {
  #         input$ageGroup
  #       } else {
  #         "Total"
  #         #      }
  #       }
  #     ) %>%
  #     select(area, AY, level_or_type, age_group,
  #       # typeNeat,
  #       metric = if ("metricGroup" %in% names(input)) {
  #         input$metricGroup
  #       } else {
  #         "achievements"
  #       }
  #     )
  #
  #   # add an extra column so the colours work in ggplot when sorting alphabetically
  #   FETime$Area <- factor(FETime$area,
  #     levels = c(input$lep1, input$lep2)
  #   )
  #
  #   ggplot(FETime, aes(
  #     x = AY, y = metric, colour = area,
  #     # linetype=if(input$splitLine=="None"){}else{eval(parse(text = input$splitLine))},
  #     group =
  #     # interaction(
  #       area
  #     # ,if(input$splitLine=="None"){}else{eval(parse(text = input$splitLine))})
  #     ,
  #     text = paste0(
  #       "Academic year: ", AY, "<br>",
  #       "Area: ", Area, "<br>",
  #       str_to_sentence(input$metricGroup), ": ", format(metric, big.mark = ","), "<br>",
  #       "Provision and level: ", level_or_type, "<br>",
  #       "Age: ", age_group, "<br>"
  #     )
  #   )) +
  #     geom_line() +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", axis.title.x = element_blank(), axis.title.y = element_blank()) +
  #     labs(shape = "", colour = "") +
  #     scale_y_continuous(label = comma) +
  #     xlab("Year") +
  #     scale_color_manual(values = chartColors2)
  # })
  #
  # output$Ach_time <- renderPlotly({
  #   ggplotly(Ach_time(), tooltip = c("text")) %>%
  #     layout(
  #       legend = list(orientation = "h", x = 0, y = -0.1),
  #       xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
  #     ) %>% # disable zooming because it's awful on mobile
  #     config(displayModeBar = FALSE)
  # })
  #
  # ## Achievements pc bar chart ----
  # Ach_SSA_pc <- eventReactive(c(input$lep1, input$lep2, input$levelBar, input$sexBar, input$metricBar), {
  #   AchSSA_21 <- C_Achieve_ILR21 %>%
  #     filter(
  #       geographic_level == input$GeoType,
  #       (area == input$lep1 |
  #         area == if ("lep2" %in% names(input)) {
  #           input$lep2
  #         } else {
  #           "\nNone"
  #         }),
  #       Level == input$levelBar,
  #       sex == input$sexBar
  #     ) %>%
  #     mutate(pc = case_when(input$metricBar == "Achievements" ~ pcAch, TRUE ~ pcEnr)) %>%
  #     select(area, SSA, metric = input$metricBar, pc)
  #
  #   # add an extra column so the colours work in ggplot when sorting alphabetically
  #   AchSSA_21$Area <- factor(AchSSA_21$area,
  #     levels = c(input$lep1, input$lep2)
  #   )
  #   ggplot(AchSSA_21, aes(x = reorder(SSA, desc(SSA)), y = pc, fill = Area, text = paste0(
  #     "SSA: ", SSA, "<br>",
  #     "Area: ", Area, "<br>",
  #     "Percentage of ", str_to_lower(input$metricBar), ": ", scales::percent(round(pc, 2)), "<br>",
  #     input$metricBar, ": ", metric, "<br>"
  #   ))) +
  #     geom_col(
  #       position = "dodge"
  #     ) +
  #     scale_y_continuous(labels = scales::percent) +
  #     scale_x_discrete(labels = function(SSA) str_wrap(SSA, width = 26)) +
  #     coord_flip() +
  #     theme_minimal() +
  #     labs(fill = "") +
  #     theme(
  #       legend.position = "bottom", axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_text(size = 7),
  #       panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  #     ) +
  #     scale_fill_manual(values = chartColors2)
  # })
  #
  # output$Ach_SSA_pc <- renderPlotly({
  #   ggplotly(Ach_SSA_pc(),
  #     tooltip = c("text"), height = 474
  #   ) %>%
  #     layout(
  #       legend = list(orientation = "h", x = 0, y = -0.1),
  #       xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
  #     ) %>% # disable zooming because it's awful on mobile
  #     config(displayModeBar = FALSE)
  # })


  #---
  
  # Maps ----

  # get current metric in plain englush
  currentMetric <- reactive({
    sub("fe", "FE", tolower(gsub("^.*\\.", "", names(unlist(metricChoices)[unlist(metricChoices) == input$splashMetric]))))
  })

  # add comparison area option
  output$geoComp <- renderUI({
    if ("map_shape_click" %in% names(input)) {
      event <- input$map_shape_click
    } else {
      event <- data.frame(id = c("E37000025"))
    }
    areaClicked <- C_Geog$areaName[C_Geog$areaCode == event$id]
    if (input$splashGeoType == "LEP") {
      selectizeInput("geoComps",
        multiple = TRUE, label = NULL,
        choices = c(C_LEP2020 %>% filter(geographic_level == "LEP", Area != areaClicked) %>% select(Area)),
        options = list(maxItems = 4, placeholder = "Choose comparison LEPs")
      )
    } else if (input$splashGeoType == "LSIP") {
      selectizeInput("geoComps",
        multiple = TRUE, label = NULL,
        choices = c(C_LEP2020 %>% filter(geographic_level == "LSIP", Area != areaClicked) %>% select(Area)),
        options = list(maxItems = 4, placeholder = "Choose comparison LSIPs")
      )
    } else {
      selectizeInput("geoComps",
        multiple = TRUE, label = NULL,
        choices = c(C_LEP2020 %>% filter(geographic_level == "MCA", Area != areaClicked) %>% select(Area)),
        options = list(maxItems = 4, placeholder = "Choose comparison MCAs")
      )
    }
  })

  # create map header
  output$titleMap <- renderUI({
    # get current area
    if ("map_shape_click" %in% names(input)) {
      event <- input$map_shape_click
    } else {
      event <- data.frame(id = c("E37000025"))
    }
    areaClicked <- C_Geog$areaName[C_Geog$areaCode == event$id]
    paste0("Where does ", areaClicked, " fit in the national picture?")
  })

  # create map comment
  output$commentMap <- renderUI({
    # get current area
    if ("map_shape_click" %in% names(input)) {
      event <- input$map_shape_click
    } else {
      event <- data.frame(id = c("E37000025"))
    }
    areaClicked <- C_Geog$areaName[C_Geog$areaCode == event$id]
    compareNational <-
      if ((C_Geog %>%
        filter(geog == input$splashGeoType & areaName == areaClicked))[[input$splashMetric]]
      >
        (C_Geog %>%
          filter(geog == "COUNTRY" & areaName == "England"))[[input$splashMetric]]
      ) {
        "higher"
      } else {
        "lower"
      }
    areaRank <- (C_Geog %>%
      filter(geog == input$splashGeoType) %>%
      mutate(ranking = rank(desc(eval(parse(text = input$splashMetric))))) %>%
      filter(areaName == areaClicked))$ranking
    suff <- case_when(
      areaRank %in% c(11, 12, 13) ~ "th",
      areaRank %% 10 == 1 ~ "st",
      areaRank %% 10 == 2 ~ "nd",
      areaRank %% 10 == 3 ~ "rd",
      TRUE ~ "th"
    )
    groupCount <- if (input$splashGeoType == "LEP") {
      "38 LEPs."
    } else {
      "10 MCAs."
    }
    paste0(areaClicked, " has a ", compareNational, " ", currentMetric(), " than the national average. It has the ", areaRank, suff, " highest ", currentMetric(), " of the ", groupCount)
  })

  # draw map
  output$map <- renderLeaflet({
    if (input$splashGeoType == "LEP") {
      # if("map_shape_click" %in% names(input)){
      #  baseAreaLEP<-input$map_shape_click$id}
      # else {
      baseAreaLEP <- "E37000025"
    } # }
    # if("map_shape_click" %in% names(input)){
    #  baseAreaMCA<-input$map_shape_click$id}
    else {
      baseAreaMCA <- "E47000010"
    }

    if (input$splashGeoType == "LEP") {
      baseArea <- baseAreaLEP
    } else {
      baseArea <- baseAreaMCA
    }

    mapData <- C_Geog %>% filter(geog == input$splashGeoType)
    pal <- colorNumeric("Blues", mapData[[input$splashMetric]]) ## 6BACE6 c("#FFFFFF", "#12436D")

    labels <-
      if (str_sub(input$splashMetric, start = -4) == "Rate") {
        sprintf(
          "<strong>%s</strong><br/>%s: %s%%",
          mapData$areaName, currentMetric(),
          round(mapData[[input$splashMetric]] * 100)
        ) %>% lapply(htmltools::HTML)
      } else {
        sprintf(
          "<strong>%s</strong><br/>%s: %s",
          mapData$areaName, currentMetric(),
          format(round(mapData[[input$splashMetric]]), big.mark = ",")
        ) %>% lapply(htmltools::HTML)
      }
    # labelsChosen <- if(input$splashMetric=="empRate"){
    #   sprintf(
    #     "<strong>%s</strong><br/>Employment rate: %s%%",
    #     (mapData%>%filter(areaCode==baseArea))$areaName, round((mapData%>%filter(areaCode==baseArea))[[input$splashMetric]]*100)
    #   ) %>% lapply(htmltools::HTML)}
    # else{
    #   sprintf(
    #     "<strong>%s</strong><br/>FE achievements per <br/>100,000: %s",
    #     (mapData%>%filter(areaCode==baseArea))$areaName, round((mapData%>%filter(areaCode==baseArea))[[input$splashMetric]])
    #   ) %>% lapply(htmltools::HTML)
    # }

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
      #   #highlight first polygon chose (Black Country)
      #   addPolygons(
      #     data = mapData%>%filter(areaCode==baseArea),
      #     fillColor = ~ pal(mapData[[input$splashMetric]]),
      #     color = "black",
      #     layerId = ~areaCode,
      #     weight = 3,
      #     highlightOptions = highlightOptions(
      #       weight = 2,
      #       bringToFront = TRUE
      #     ),
      #     label = labelsChosen,
      #     labelOptions = labelOptions(
      #       style = list("font-weight" = "normal", padding = "3px 8px"),
      #       textsize = "12px",
      #       direction = "auto"
      #     ))%>%
      setView(lng = -1.6, lat = 52.8, zoom = 5.7)
  })

  # observeEvent(input$map_shape_click$id,{
  #   mapData<-C_Geog%>%filter(geog==input$splashGeoType)
  #   pal <- colorNumeric("Blues", mapData[[input$splashMetric]]) ## 6BACE6 c("#FFFFFF", "#12436D")
  #
  #   labels <-       if(input$splashMetric=="empRate"){
  #     sprintf(
  #       "<strong>%s</strong><br/>Employment rate: %s%%",
  #       mapData$areaName, round(mapData[[input$splashMetric]]*100)
  #     )%>% lapply(htmltools::HTML) }else{
  #       sprintf(
  #         "<strong>%s</strong><br/>FE achievements per 100,000: %s",
  #         mapData$areaName, round(mapData[[input$splashMetric]])
  #       )%>% lapply(htmltools::HTML)
  #     }
  #   labelsChosen <- if(input$splashMetric=="empRate"){
  #     sprintf(
  #     "<strong>%s</strong><br/>Employment rate: %s%%",
  #     (mapData%>%filter(areaCode==input$map_shape_click$id))$areaName, round((mapData%>%filter(areaCode==input$map_shape_click$id))[[input$splashMetric]]*100)
  #     ) %>% lapply(htmltools::HTML)}
  #   else{
  #     sprintf(
  #       "<strong>%s</strong><br/>FE achievements per <br/>100,000: %s",
  #       (mapData%>%filter(areaCode==input$map_shape_click$id))$areaName, round((mapData%>%filter(areaCode==input$map_shape_click$id))[[input$splashMetric]])
  #     ) %>% lapply(htmltools::HTML)
  #   }
  #
  #   leafletProxy("map") %>%
  #     #removeShape(layerId =input$map_shape_click$id)%>%
  #     addPolygons(
  #       data = mapData,
  #       fillColor = ~ pal(mapData[[input$splashMetric]]),
  #       color = "black",
  #       layerId = ~areaCode,
  #       weight = 1,
  #       highlightOptions = highlightOptions(
  #         weight = 2,
  #         bringToFront = TRUE
  #       ),
  #       label = labels,
  #       labelOptions = labelOptions(
  #         style = list("font-weight" = "normal", padding = "3px 8px"),
  #         textsize = "12px",
  #         direction = "auto"
  #       )
  #     ) %>%
  #     addPolygons(
  #       data = mapData%>%filter(areaCode==input$map_shape_click$id),
  #       fillColor = ~ pal(mapData[[input$splashMetric]]),
  #       color = "black",
  #       layerId = ~areaCode,
  #       weight = 3,
  #       highlightOptions = highlightOptions(
  #         weight = 2,
  #         bringToFront = TRUE
  #       ),
  #       label = labelsChosen,
  #       labelOptions = labelOptions(
  #         style = list("font-weight" = "normal", padding = "3px 8px"),
  #         textsize = "12px",
  #         direction = "auto"
  #       )
  #     )
  # })

  # create LA map comment
  output$commentLA <- renderUI({
    # #get current area
    if ("map_shape_click" %in% names(input)) {
      event <- input$map_shape_click
    } else {
      event <- data.frame(id = c("E37000025"))
    }
    areaClicked <- C_Geog$areaName[C_Geog$areaCode == event$id]

    LaHighLow <- C_Geog %>%
      filter(
        geog == "LADU",
        eval(parse(text = input$splashGeoType)) == areaClicked
      ) %>%
      mutate(ranking = rank(desc(eval(parse(text = input$splashMetric)))))

    LaHigh <- (LaHighLow %>% filter(ranking == 1))$areaName
    LaLow <- (LaHighLow %>% filter(ranking == max(ranking)))$areaName

    paste0(str_to_sentence(currentMetric()), " is highest in ", LaHigh, " and lowest in ", LaLow, ".")
  })

  # make LA map
  output$mapLA <- renderLeaflet({
    # get current region
    if ("map_shape_click" %in% names(input)) {
      event <- input$map_shape_click
    } else {
      event <- data.frame(id = c("E37000025"))
    }
    # Filter to those LAs in that region
    mapData <- C_Geog %>% filter(geog == "LADU", eval(parse(text = input$splashGeoType)) == C_Geog$areaName[C_Geog$areaCode == event$id])
    pal <- colorNumeric("Blues", mapData[[input$splashMetric]]) ## 6BACE6 c("#FFFFFF", "#12436D")

    labels <- if (str_sub(input$splashMetric, start = -4) == "Rate") {
      sprintf(
        "<strong>%s</strong><br/>%s: %s%%",
        mapData$areaName, currentMetric(), round(mapData[[input$splashMetric]] * 100)
      ) %>% lapply(htmltools::HTML)
    } else {
      sprintf(
        "<strong>%s</strong><br/>%s: %s",
        mapData$areaName, currentMetric(), format(mapData[[input$splashMetric]], big.mark = ",")
      ) %>% lapply(htmltools::HTML)
    }

    leaflet(options = leafletOptions(zoomSnap = 0.1)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% # "Stamen.TonerLite"
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

  # time chart

  # create time header
  output$titleTime <- renderUI({
    paste0("How is ", currentMetric(), " changing over time?")
  })

  # create time comment
  output$commentTime <- renderUI({
    # get current area
    if ("map_shape_click" %in% names(input)) {
      event <- input$map_shape_click
    } else {
      event <- data.frame(id = c("E37000025"))
    }
    areaClicked <- C_Geog$areaName[C_Geog$areaCode == event$id]
    dataTimeCompare <- if (input$splashMetric == "empRate") {
      EmpRateData <- C_EmpRate_APS1822 %>%
        filter(geographic_level != "LADU") %>%
        select(year, area, geographic_level, metric = empRate) %>%
        mutate(year = as.numeric(year) - 2017)
    } else {
      FeData <- C_Achieve_ILR1621 %>%
        filter(geographic_level != "Local authority district", level_or_type == "Further education and skills: Total", age_group == "Total") %>%
        select(year = time_period, area, geographic_level, metric = achievements_rate_per_100000_population) %>%
        mutate(geographic_level = case_when(
          geographic_level == "National" ~ "COUNTRY",
          TRUE ~ geographic_level
        )) %>%
        mutate(year = round(year / 100, 0) - 2015)
    }

    compareNational <-
      if (((dataTimeCompare %>%
        filter(year == 5, geographic_level == input$splashGeoType & area == areaClicked))$metric -
        (dataTimeCompare %>%
          filter(year == 1, geographic_level == input$splashGeoType & area == areaClicked))$metric
      ) >
        ((dataTimeCompare %>%
          filter(year == 5, geographic_level == "COUNTRY" & area == "England"))$metric -
          (dataTimeCompare %>%
            filter(year == 1, geographic_level == "COUNTRY" & area == "England"))$metric
        )) {
        "faster"
      } else {
        "slower"
      }
    areaRank <- (dataTimeCompare %>%
      filter(geographic_level == input$splashGeoType, (year == 5 | year == 1)) %>%
      group_by(geographic_level, area) %>%
      mutate(change = metric - lag(metric, default = 0)) %>%
      filter(year == 5) %>%
      ungroup() %>%
      mutate(ranking = rank(desc(change))) %>%
      filter(area == areaClicked))$ranking
    suff <- case_when(
      areaRank %in% c(11, 12, 13) ~ "th",
      areaRank %% 10 == 1 ~ "st",
      areaRank %% 10 == 2 ~ "nd",
      areaRank %% 10 == 3 ~ "rd",
      TRUE ~ "th"
    )
    groupCount <- if (input$splashGeoType == "LEP") {
      "38 LEPs."
    } else {
      "10 MCAs."
    }
    paste0(areaClicked, "'s ", currentMetric(), " has increased ", compareNational, " than the national average in the last five years. It has the ", areaRank, suff, " fastest growing ", currentMetric(), " of the ", groupCount)
  })

  Splash_time <- eventReactive(c(input$map_shape_click, input$mapLA_shape_click, input$geoComps, input$levelBar, input$splashMetric), {
    if ("map_shape_click" %in% names(input)) {
      event <- input$map_shape_click
    } else {
      event <- data.frame(id = c("E37000025"))
    }
    eventLA <- input$mapLA_shape_click
    SplashTime <- C_time %>%
      filter(
        # get lep/lsip/mca areas
        (
          geographic_level == input$splashGeoType &
            (area == C_Geog$areaName[C_Geog$areaCode == event$id] |
              area %in% if ("geoComps" %in% names(input)) {
                input$geoComps
              } else {
                "\nNone"
              })
        ) |
          # get england for comparison (if a rate)
          (if (str_sub(input$splashMetric, start = -4) %in% c("Rate","tion")) {
            (geographic_level == "COUNTRY" & area == "England")
          } else {
            area == "\nNone"
          }) |
          # get LA
          if (is.null(eventLA) == TRUE) {
            area == "\nNone"
          } else {
            (geographic_level == "LADU" & area == C_mapLA$LAD22NM[C_mapLA$LAD22CD == eventLA$id])
          },
        metric == input$splashMetric
      )
    # add an extra column so the colours work in ggplot when sorting alphabetically
    SplashTime$Areas <- factor(SplashTime$area,
      levels = c("England", C_Geog$areaName[C_Geog$areaCode == event$id], C_mapLA$LAD22NM[C_mapLA$LAD22CD == eventLA$id], input$geoComps)
    )

    ggplot(
      SplashTime,
      aes(
        x = chart_year, y = value,
        color = Areas, group = Areas,
        text = paste0(
          "Year: ", chart_year, "<br>",
          "Area: ", Areas, "<br>",
          currentMetric(), ": ", if (str_sub(input$splashMetric, start = -4) == "Rate") {
            scales::percent(round(value, 2))
          } else {
            format(round(value), big.mark = ",")
          }, "<br>"
        )
      )
    ) +
      geom_line() +
      theme_minimal() +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "bottom", legend.title = element_blank()) +
      scale_y_continuous(
        labels = if (str_sub(input$splashMetric, start = -4) == "Rate") {
          scales::percent_format(accuracy = 1)
        } else {
          label_number_si(accuracy = 1)
        }
        # , limits = if(input$splashMetric=="empRate"){c(.65, .85)}else{c(0, 10000)}
      ) +
      labs(colour = "") +
      scale_color_manual(values = if (str_sub(input$splashMetric, start = -4) %in% c("Rate","tion")) {chartColors6}else{chartColors5})
  })

  output$Splash_time <- renderPlotly({
    ggplotly(Splash_time(), tooltip = "text") %>%
      layout(
        legend = list(orientation = "h", x = 0, y = -0.1),
        xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })

  # Breakdown chart
  # breakdown filter
  output$breakdownFilter <- renderUI({
    selectizeInput(
      inputId = "barBreakdown",
      label = NULL,
      choices =
        (as.vector(C_breakdown %>%
          filter(metric == input$splashMetric) %>%
          distinct(breakdown)))$breakdown
    )
  })

  output$subgroupFilter <- renderUI({
    if ("map_shape_click" %in% names(input)) {
      event <- input$map_shape_click
    } else {
      event <- data.frame(id = c("E37000025"))
    }
    pickerInput(
      inputId = "barSubgroup",
      label = NULL,
      choices =
        (as.vector(C_breakdown %>%
          filter(
            metric == input$splashMetric,
            breakdown == input$barBreakdown
          ) %>%
          distinct(subgroups)))$subgroups,
      multiple = TRUE,
      selected = (as.vector(C_breakdown %>%
        filter(
          metric == input$splashMetric,
          breakdown == input$barBreakdown,
          area == C_Geog$areaName[C_Geog$areaCode == event$id],
          geographic_level == input$splashGeoType
        ) %>%
        arrange(desc(value)) %>%
        slice(1:10) %>%
        distinct(subgroups)))$subgroups,
      options = list(`actions-box` = TRUE)
    )
  })

  # create breakdown header
  output$titleBreakdown <- renderUI({
    if (input$barBreakdown == "No breakdowns available") {
      paste0(
        str_to_sentence(currentMetric()), " currently has no breakdowns.",
        if (input$splashMetric %in% c("empRate", "selfempRate", "unempRate", "inactiveRate", "  Self Employed ", "  Unemployed ", "  Inactive ")) {
          " Switch to Employment volume metric for occupation and industry breakdowns."
        } else {
          ""
        }
      )
    } else {
      paste0(
        "How does ", currentMetric(), " vary by ",
        input$barBreakdown, "?"
      )
    }
  })

  # create breakdown comment
  output$commentBreakdown <- renderUI({
    # #get current area
    if ("map_shape_click" %in% names(input)) {
      event <- input$map_shape_click
    } else {
      event <- data.frame(id = c("E37000025"))
    }
    areaClicked <- C_Geog$areaName[C_Geog$areaCode == event$id]

    breakdownDiff <- C_breakdown %>%
      filter(
        geographic_level == input$splashGeoType | geographic_level == "COUNTRY",
        area == areaClicked | area == "England",
        breakdown == input$barBreakdown,
        metric == input$splashMetric
      ) %>%
      group_by(subgroups) %>%
      mutate(change = (value - lag(value, default = 1)) / value) %>%
      ungroup() %>%
      filter(area == areaClicked) %>%
      mutate(ranking = rank(desc(abs(change)))) %>%
      filter(ranking == 1)

    breakdownDirection <- if (breakdownDiff$change > 0) {
      "high"
    } else {
      "low"
    }
    breakdownSubgroup <- breakdownDiff$subgroups
    paste(breakdownDirection, breakdownSubgroup)

    if (input$barBreakdown == "No breakdowns available") {} else {
      paste0(areaClicked, " has a ", breakdownDirection, " ", currentMetric(), " in ", breakdownSubgroup, " than the national average.")
    }
  })

  # create breakdown bar
  Splash_pc <- eventReactive(c(input$map_shape_click, input$geoComps, input$barBreakdown, input$barSubgroup, input$levelBar, input$sexBar, input$metricBar, input$splashBreakdown, input$mapLA_shape_click,input$splashMetric), {
    if ("map_shape_click" %in% names(input)) {
      event <- input$map_shape_click
    } else {
      event <- data.frame(id = c("E37000025"))
    }
    eventLA <- input$mapLA_shape_click
    Splash_21 <- C_breakdown %>% filter(
      breakdown == input$barBreakdown,
      subgroups %in% input$barSubgroup,
      metric == input$splashMetric,
      (geographic_level == "COUNTRY" & area == "England") |
        ((geographic_level == input$splashGeoType &

          (area == C_Geog$areaName[C_Geog$areaCode == event$id] |
            area %in% if ("geoComps" %in% names(input)) {
              input$geoComps
            } else {
              "\nNone"
            })) |
          if (is.null(eventLA) == TRUE) {
            area == "\nNone"
          } else {
            (geographic_level == "LADU" & area == C_mapLA$LAD22NM[C_mapLA$LAD22CD == eventLA$id])
          })
    )

    # add an extra column so the colours work in ggplot when sorting alphabetically
    Splash_21$Area <- factor(Splash_21$area,
      levels = c("England", C_Geog$areaName[C_Geog$areaCode == event$id], C_mapLA$LAD22NM[C_mapLA$LAD22CD == eventLA$id], input$geoComps)
    )
    ggplot(Splash_21, aes(
      x = reorder(subgroups, value, mean), y = value, fill = Area,
      text = paste0( # reorder(input$splashBreakdown, desc(input$splashBreakdown))
        # "Breakdown: ", input$splashBreakdown, "<br>",
        "Area: ", Area, "<br>",
        # "Percentage of ", str_to_lower(input$metricBar), ": ", scales::percent(round(value, 2)), "<br>",
        currentMetric(), ": ", if (str_sub(input$splashMetric, start = -4) == "Rate" | input$splashMetric == "Employment") {
          scales::percent(round(value, 2))
        } else {
          round(value, 0)
        }, "<br>"
      )
    )) +
      geom_col(
        position = "dodge"
      ) +
      scale_y_continuous(labels = if (str_sub(input$splashMetric, start = -4) == "Rate" | input$splashMetric == "Employment") {
        scales::percent
      } else {
        label_number_si(accuracy = 1)
      }) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 26)) +
      coord_flip() +
      theme_minimal() +
      labs(fill = "") +
      theme(
        legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_text(size = 7),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
      ) +
      scale_fill_manual(values = chartColors6)
  })

  output$Splash_pc <- renderPlotly({
    ggplotly(Splash_pc(),
      tooltip = c("text") # , height = 474
    ) %>%
      layout(
        legend = list(orientation = "h", x = 0, y = -0.1),
        xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })

  # create datahub table to show
  
  output$hubAreaInput <- renderUI({
      selectizeInput("hubArea",
                     choices =C_datahub%>%
                       filter(if(is.null(input$hubGeog)==TRUE){TRUE}else{geographic_level %in% input$hubGeog})%>% 
                       distinct(Area=area),
                     multiple = TRUE, label = NULL,
                     options = list(placeholder = "Choose areas*")
      )
  })
  
 allOptions<- bind_rows(
    data.frame(Choice=c("LEP", "LSIP", "MCA", "LA"),filterID="a"),
  C_datahub%>%distinct(Choice=area)%>%mutate(filterID="b"),
  data.frame(Choice=c("Yes", "No"),filterID="c"),
  data.frame(Choice=c("National", "Regional (to come)"),filterID="d"),
  C_datahub%>%distinct(Choice=metric)%>%mutate(filterID="e"),
  C_datahub%>%distinct(Choice=breakdown)%>%mutate(filterID="f"),
  C_datahub%>%distinct(Choice=as.character(time_period))%>%mutate(filterID="g")
  )%>%
   group_by(filterID) %>%
   mutate(ChoiceNo = row_number())%>%
   mutate(ChoiceID=paste0(filterID,ChoiceNo))%>%
   ungroup()%>%
   select(-filterID,-ChoiceNo)
  
  
  output$uniqueCode<-renderUI({
    allOptions%>%
      mutate(chosen=case_when(Choice %in% input$hubArea ~ 1,
                              Choice %in% input$hubMetric ~1,
                              Choice %in% input$hubGeog ~1,
                              Choice %in% input$hubComparators ~1,
                              Choice %in% input$hubLA ~1,
                              Choice %in% input$hubBreakdowns ~1,
                              Choice %in% input$hubYears ~1,
                              TRUE~0))%>%
        filter(chosen==1)%>%
        select(ChoiceID)%>%
        summarize(strong = str_c(ChoiceID, collapse = ''), .groups = 'drop')
  })

  output$hubMetricInput <- renderUI({
  selectizeInput("hubMetric",
                 choices = C_datahub%>%filter(if(is.null(input$hubGeog)==TRUE){TRUE}else{geographic_level %in% input$hubGeog},
                                              if(is.null(input$hubArea)==TRUE){TRUE}else{area %in% input$hubArea})%>%
                   distinct(Metrics=metric),
                                              multiple = TRUE, label = NULL,
                 options = list(placeholder = "Choose metrics*")
  )
  })
  
  output$hubBreakdownInput <- renderUI({
  selectizeInput("hubBreakdowns",
                 choices = C_datahub%>%filter(if(is.null(input$hubGeog)==TRUE){TRUE}else{geographic_level %in% input$hubGeog},
                                              if(is.null(input$hubArea)==TRUE){TRUE}else{area %in% input$hubArea},
                                              if(is.null(input$hubMetric)==TRUE){TRUE}else{metric %in% input$hubMetric}
                 )%>% distinct(Breakdowns=breakdown),
                 multiple = TRUE, label = NULL,
                 options = list(placeholder = "Choose breakdowns")
  )
  })
  
  output$hubYearInput <- renderUI({
  selectizeInput("hubYears",
                 choices = C_datahub%>%filter(if(is.null(input$hubGeog)==TRUE){TRUE}else{geographic_level %in% input$hubGeog},
                                              if(is.null(input$hubArea)==TRUE){TRUE}else{area %in% input$hubArea},
                                              if(is.null(input$hubMetric)==TRUE){TRUE}else{metric %in% input$hubMetric},
                                              if(is.null(input$hubBreakdowns)==TRUE){TRUE}else{breakdown %in% input$hubBreakdowns})%>% 
                   distinct("Time period"=time_period),
                 multiple = TRUE, label = NULL,
                 options = list(placeholder = "Choose years*")
                 
  )
  })

  output$hubTable <- renderDataTable({
    DT::datatable(C_datahub %>%
      filter(
        (
          if(is.null(input$hubGeog)==TRUE){TRUE}else{geographic_level %in% input$hubGeog}
           |(if("Yes" %in% input$hubLA){geographic_level=="LADU"}else{geographic_level=="xxx"})
           |(if("National" %in% input$hubComparators){geographic_level=="COUNTRY"}else{geographic_level=="xxx"})
         ) , 
        (
          if(is.null(input$hubArea)==TRUE){TRUE}else{area %in% input$hubArea}
          |(if("Yes" %in% input$hubLA){area %in% (C_Geog %>% filter(geog == "LADU", LEP %in% input$hubArea)%>%distinct(areaName))$areaName}else{geographic_level=="xxx"})
          |(if("National" %in% input$hubComparators){area=="England"}else{geographic_level=="xxx"})
        ),
        if(is.null(input$hubYears)==TRUE){TRUE}else{time_period %in% input$hubYears},
        if(is.null(input$hubMetric)==TRUE){TRUE}else{metric %in% input$hubMetric},
        (if(is.null(input$hubBreakdowns)==TRUE){TRUE}else{breakdown %in% input$hubBreakdowns})
      ) %>%
      select(
        Year = time_period, Geography = geographic_level, Area = area,
        Data=metric,Breakdown=breakdown,Splits=subgroups,Value=value
      ))
  })

  # create hub code
  output$hubCode <- renderUI({
    round(runif(1, 0, 10) * 1000000, 0)
  })
  
  ##FE interventions table
  output$interventionTable = DT::renderDataTable({
    DT::datatable(I_InterventionTable, escape = FALSE, options = list(dom = "t"), rownames = FALSE)
  })
  
  ##FE sources table
  output$sourcesTable = DT::renderDataTable({
    DT::datatable(I_SourcesTable, escape = FALSE, options = list(dom = "t"), rownames = FALSE)
  })
  
  # Stop app ---------------------------------------------------------------------------------

  session$onSessionEnded(function() {
    stopApp()
  })
}
