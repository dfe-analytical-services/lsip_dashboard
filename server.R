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
  # England, geo1, geo2, then any others
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

  ## 1.3 Set up cookies
  # output if cookie is unspecified
  observeEvent(input$cookies, {
    if (!is.null(input$cookies)) {
      if (!("dfe_analytics" %in% names(input$cookies))) {
        shinyalert(
          inputId = "cookie_consent",
          title = "Cookie consent",
          text = "This site uses cookies to record traffic flow using Google Analytics",
          size = "s",
          closeOnEsc = TRUE,
          closeOnClickOutside = FALSE,
          html = FALSE,
          type = "",
          showConfirmButton = TRUE,
          showCancelButton = TRUE,
          confirmButtonText = "Accept",
          confirmButtonCol = "#AEDEF4",
          timer = 0,
          imageUrl = "",
          animation = TRUE
        )
      } else {
        msg <- list(
          name = "dfe_analytics",
          value = input$cookies$dfe_analytics
        )
        session$sendCustomMessage("analytics-consent", msg)
        if ("cookies" %in% names(input)) {
          if ("dfe_analytics" %in% names(input$cookies)) {
            if (input$cookies$dfe_analytics == "denied") {
              ga_msg <- list(name = paste0("_ga_", google_analytics_key))
              session$sendCustomMessage("cookie-remove", ga_msg)
            }
          }
        }
      }
    }
  })

  observeEvent(input$cookie_consent, {
    msg <- list(
      name = "dfe_analytics",
      value = ifelse(input$cookie_consent, "granted", "denied")
    )
    session$sendCustomMessage("cookie-set", msg)
    session$sendCustomMessage("analytics-consent", msg)
    if ("cookies" %in% names(input)) {
      if ("dfe_analytics" %in% names(input$cookies)) {
        if (input$cookies$dfe_analytics == "denied") {
          ga_msg <- list(name = paste0("_ga_", google_analytics_key))
          session$sendCustomMessage("cookie-remove", ga_msg)
        }
      }
    }
  })

  observeEvent(input$remove, {
    msg <- list(name = "dfe_analytics", value = "denied")
    session$sendCustomMessage("cookie-remove", msg)
    session$sendCustomMessage("analytics-consent", msg)
  })

  cookies_data <- reactive({
    input$cookies
  })

  output$cookie_status <- renderText({
    cookie_text_stem <- "To better understand the reach of our dashboard tools, this site uses cookies to identify numbers of unique users as part of Google Analytics. You have chosen to"
    cookie_text_tail <- "the use of cookies on this website."
    if ("cookies" %in% names(input)) {
      if ("dfe_analytics" %in% names(input$cookies)) {
        if (input$cookies$dfe_analytics == "granted") {
          paste(cookie_text_stem, "accept", cookie_text_tail)
        } else {
          paste(cookie_text_stem, "reject", cookie_text_tail)
        }
      }
    } else {
      paste("Cookies consent has not been confirmed.")
    }
  })

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
      selected = "inemploymentRate"
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
  # Create link to working futures
  observeEvent(input$link_to_tabpanel_wf1, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateSelectInput(session, "splashMetric",
      selected = "employmentProjection"
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
      "EmploymentVolumes.xlsx"
    },
    content = function(file) {
      write_xlsx(list("1b.Employment volumes" = C_datahub %>%
        filter(metric %in% c("all", "inemployment", "selfemployed", "unemployed", "inactive"), Breakdown != "Occupation", Breakdown != "Industry") %>%
        select(-metric, -Breakdown, -Subgroup) %>%
        rename("Volume" = valueText, Metric = metricNeat)), path = file)
    }
  )
  output$downloadData2 <- downloadHandler(
    filename = function() {
      "EmploymentByOccupation.xlsx"
    },
    content = function(file) {
      write_xlsx(list("1a.Employment by occupation" = C_datahub %>%
        filter(metric == "inemployment", Breakdown == "Occupation") %>%
        select(-metric, -metricNeat, -Breakdown) %>%
        rename("Employment volume" = valueText, Occupation = Subgroup)), path = file)
    }
  )
  output$downloadData3 <- downloadHandler(
    filename = function() {
      "EmploymentByIndustry.xlsx"
    },
    content = function(file) {
      write_xlsx(list("1c.Employment by industry" = C_datahub %>%
        filter(metric == "inemployment", Breakdown == "Industry") %>%
        select(-metric, -metricNeat, -Breakdown, Industry = Subgroup) %>%
        rename("Employment volume" = valueText)), path = file)
    }
  )
  output$downloadData4 <- downloadHandler(
    filename = function() {
      "FeAchievementParticipation.xlsx"
    },
    content = function(file) {
      write_xlsx(list("3b.FE achievement&participation" = C_datahub %>%
        filter(metric %in% c("achievements", "participation"), Breakdown != "SSA") %>%
        select(-metric) %>%
        rename(Volume = valueText, Metric = metricNeat)), path = file)
    }
  )
  output$downloadData5 <- downloadHandler(
    filename = function() {
      "FeAchievementBySSA.xlsx"
    },
    content = function(file) {
      write_xlsx(list("3a.FE achievements by SSA" = C_datahub %>%
        filter(metric == "achievements", Breakdown == "SSA") %>%
        select(-metric, -metricNeat, -Breakdown) %>%
        rename(Achievements = valueText, "Sector subject area tier 1" = Subgroup)), path = file)
    }
  )
  output$downloadData6 <- downloadHandler(
    filename = function() {
      "HighestQualification.xlsx"
    },
    content = function(file) {
      write_xlsx(
        list("4.Highest qualification" = C_datahub %>%
          filter(metric %in% c("qualNone", "qualL1", "qualL2", "qualApp", "qualL3", "qualL4", "qualOther")) %>%
          select(-metric) %>%
          rename("16-64 year olds" = valueText, "Highest qualification" = metricNeat)),
        path = file
      )
    }
  )
  output$downloadData7 <- downloadHandler(
    filename = function() {
      "EnterpriseBySize.xlsx"
    },
    content = function(file) {
      write_xlsx(list("5a.Enterprises by size" = C_datahub %>%
        filter(metric == "enterpriseCount", Breakdown != "Industry") %>%
        select(-metric, -metricNeat, -Breakdown) %>%
        rename("Enterprise count" = valueText, "Size band" = Subgroup)), path = file)
    }
  )
  output$downloadData8 <- downloadHandler(
    filename = function() {
      "EnterpriseByIndustry.xlsx"
    },
    content = function(file) {
      write_xlsx(
        list("5b.Enterprises by industry" = C_datahub %>%
          filter(metric == "enterpriseCount", Breakdown != "Size") %>%
          select(-metric, -metricNeat, -Breakdown) %>%
          rename("Enterprise count" = valueText, Industry = Subgroup)),
        path = file
      )
    }
  )
  output$downloadData9 <- downloadHandler(
    filename = function() {
      "EnterpriseDemography.xlsx"
    },
    content = function(file) {
      write_xlsx(
        list("5c.Enterprise demography" = C_datahub %>%
          filter(metric %in% c("births", "deaths", "active")) %>%
          select(-metric, -Breakdown, -Subgroup) %>%
          rename("Enterprise count" = valueText, Metric = metricNeat)),
        path = file
      )
    }
  )
  output$downloadData10 <- downloadHandler(
    filename = function() {
      "KS4Destinations.xlsx"
    },
    content = function(file) {
      write_xlsx(
        list("6a.Key Stage 4 destinations" = C_datahub %>%
          filter(metric == "sustainedPositiveDestinationKS4Rate") %>%
          select(-metric, -metricNeat, -Breakdown) %>%
          rename("KS4 sustained positive destination rate" = valueText, Outcome = Subgroup)),
        path = file
      )
    }
  )
  output$downloadData11 <- downloadHandler(
    filename = function() {
      "KS5Destinations.xlsx"
    },
    content = function(file) {
      write_xlsx(
        list("6b.Key Stage 5 destinations" = C_datahub %>%
          filter(metric == "sustainedPositiveDestinationKS5Rate") %>%
          select(-metric, -metricNeat) %>%
          rename("KS5 sustained positive destination rate" = valueText)),
        path = file
      )
    }
  )
  output$downloadData12 <- downloadHandler(
    filename = function() {
      "OnlineJobAdverts.xlsx"
    },
    content = function(file) {
      write_xlsx(
        list(
          "2a.Job adverts" = C_datahub %>%
            filter(metric == "vacancies", Breakdown == "Total") %>%
            select(-metric, -metricNeat, -Breakdown, -Subgroup) %>%
            rename("Online job adverts" = valueText),
          "2b.Job adverts by profession" = C_datahub %>%
            filter(metric == "vacancies", Breakdown != "Total") %>%
            select(-metric, -metricNeat) %>%
            rename("Online job adverts" = valueText, "Detailed/Summary" = Breakdown, Profession = Subgroup)
        ),
        path = file
      )
    }
  )
  output$downloadData13 <- downloadHandler(
    filename = function() {
      "EmploymentProjection.xlsx"
    },
    content = function(file) {
      write_xlsx(
        list(
          "7.Projected employment" = C_datahub %>%
            filter(metric == "employmentProjection") %>%
            select(-metric, -metricNeat) %>%
            rename("Projected employment" = valueText)
        ),
        path = file
      )
    }
  )

  ### 2.5.2 Create download links ----
  output$hidden_downloads <- renderUI(lapply(1:13, function(i) {
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
  output$download_btn0a <- downloadHandler(
    filename = function() {
      "AllAreasIndicators.xlsx"
    },
    content = function(file) {
      file.copy("Data/AppData/CoreIndicators.xlsx", file)
    }
  )

  # Download current area indicators
  filtered_data0 <- reactive({
    currentGeogconcat <- C_datahub %>%
      filter(Area == input$geoChoiceOver)
    list(
      "1a.Employment by occupation" = currentGeogconcat %>%
        filter(metric == "inemployment", Breakdown == "Occupation") %>%
        select(-metric, -metricNeat, -Breakdown) %>%
        rename("Employment volume" = valueText, Occupation = Subgroup),
      "1b.Employment volumes" = currentGeogconcat %>%
        filter(metric %in% c("all", "inemployment", "selfemployed", "unemployed", "inactive"), Breakdown != "Occupation", Breakdown != "Industry") %>%
        select(-metric, -Breakdown, -Subgroup) %>%
        rename("Volume" = valueText, Metric = metricNeat),
      "1c.Employment by industry" = currentGeogconcat %>%
        filter(metric == "inemployment", Breakdown == "Industry") %>%
        select(-metric, -metricNeat, -Breakdown, Industry = Subgroup) %>%
        rename("Employment volume" = valueText),
      "2a.Job adverts" = currentGeogconcat %>%
        filter(metric == "vacancies", Breakdown == "Total") %>%
        select(-metric, -metricNeat, -Breakdown, -Subgroup) %>%
        rename("Online job adverts" = valueText),
      "2b.Job adverts by profession" = currentGeogconcat %>%
        filter(metric == "vacancies", Breakdown != "Total") %>%
        select(-metric, -metricNeat) %>%
        rename("Online job adverts" = valueText, "Detailed/Summary" = Breakdown, Profession = Subgroup),
      "3a.FE achievements by SSA" = currentGeogconcat %>%
        filter(metric == "achievements", Breakdown == "SSA") %>%
        select(-metric, -metricNeat, -Breakdown) %>%
        rename(Achievements = valueText, "Sector subject area tier 1" = Subgroup),
      "3b.FE achievement&participation" = currentGeogconcat %>%
        filter(metric %in% c("achievements", "participation"), Breakdown != "SSA") %>%
        select(-metric) %>%
        rename(Volume = valueText, Metric = metricNeat),
      "4.Highest qualification" = currentGeogconcat %>%
        filter(metric %in% c("qualNone", "qualL1", "qualL2", "qualApp", "qualL3", "qualL4", "qualOther")) %>%
        select(-metric) %>%
        rename("16-64 year olds" = valueText, "Highest qualification" = metricNeat),
      "5a.Enterprises by size" = currentGeogconcat %>%
        filter(metric == "enterpriseCount", Breakdown != "Industry") %>%
        select(-metric, -metricNeat, -Breakdown) %>%
        rename("Enterprise count" = valueText, "Size band" = Subgroup),
      "5b.Enterprises by industry" = currentGeogconcat %>%
        filter(metric == "enterpriseCount", Breakdown != "Size") %>%
        select(-metric, -metricNeat, -Breakdown) %>%
        rename("Enterprise count" = valueText, Industry = Subgroup),
      "5c.Enterprise demography" = currentGeogconcat %>%
        filter(metric %in% c("births", "deaths", "active")) %>%
        select(-metric, -Breakdown, -Subgroup) %>%
        rename("Enterprise count" = valueText, Metric = metricNeat),
      "6a.Key Stage 4 destinations" = currentGeogconcat %>%
        filter(metric == "sustainedPositiveDestinationKS4Rate") %>%
        select(-metric, -metricNeat, -Breakdown) %>%
        rename("KS4 sustained positive destination rate" = valueText, Outcome = Subgroup),
      "6b.Key Stage 5 destinations" = currentGeogconcat %>%
        filter(metric == "sustainedPositiveDestinationKS5Rate") %>%
        select(-metric, -metricNeat) %>%
        rename("KS5 sustained positive destination rate" = valueText),
      "7.Projected employment" = currentGeogconcat %>%
        filter(metric == "employmentProjection") %>%
        select(-metric, -metricNeat) %>%
        rename("Projected employment" = valueText)
    )
  })
  output$download_btn0b <- downloadHandler(
    filename = function() {
      paste0(input$geoChoiceOver, " Indicators.xlsx")
    },
    content = function(file) {
      write_xlsx(filtered_data0(), path = file)
    }
  )

  ### 2.2.3 KPIs and charts----
  currentGeogTime <- eventReactive(input$geoChoiceOver, {
    C_time %>%
      filter(geogConcat == input$geoChoiceOver)
  })
  englandTime <- C_time %>%
    filter(geogConcat == "England")

  # create a function to build the overview KPIs
  createOverviewKPI <- function(metricName, format) {
    # "format" can either be "percent" or "number"
    # set metric
    currentGeogTimeMetric <- currentGeogTime() %>% filter(metric == metricName)
    latest <- (currentGeogTimeMetric %>% filter(latest == 1))$value
    change <- latest - (currentGeogTimeMetric %>% filter(latest == -1))$value

    # print with formatting
    h4(
      span((currentGeogTimeMetric %>% filter(latest == 1))$chartPeriod, style = "font-size: 16px;font-weight:normal;"),
      br(),
      if (format == "percent") {
        paste0(format(100 * latest, digit = 2), "%")
      } else {
        format(latest, big.mark = ",")
      },
      br(),
      span(
        if (format == "percent") {
          paste0(sprintf("%+.0f", 100 * change), "ppts")
        } else {
          format_pm(change)
        }, # plus-minus and comma sep formatting
        style = paste0("font-size: 16px;color:", cond_color(change > 0)), # colour formating
        .noWS = c("before", "after") # remove whitespace
      ),
      br(),
      style = "font-size: 21px"
    )
  }

  # create a function to build the overview charts
  createOverviewChart <- function(metricName, format, chartLabel) {
    # set metric
    currentGeogTimeMetric <- currentGeogTime() %>% filter(metric == metricName)
    change <- (currentGeogTimeMetric %>% filter(latest == 1))$value -
      (currentGeogTimeMetric %>% filter(latest == -1))$value
    line <- if (format == "percent") {
      bind_rows(
        currentGeogTimeMetric,
        englandTime %>% filter(metric == metricName)
      )
    } else {
      currentGeogTimeMetric
    }
    timeChop <- (currentGeogTimeMetric %>% filter(latest == -1))$timePeriod # The point at which to apply the red/green colouring

    ggplot(
      line,
      aes(
        x = as.Date(timePeriod),
        y = value,
        group = geogConcat,
        text = paste0(
          geogConcat, "<br>",
          chartPeriod, "<br>",
          chartLabel, ": ",
          if (format == "percent") {
            paste0(format(100 * value, digit = 2), "%")
          } else {
            format(value, big.mark = ",")
          },
          "<br>"
        )
      )
    ) +
      geom_line(data = line %>% filter(timePeriod <= timeChop, geogConcat == input$geoChoiceOver)) +
      geom_ribbon(
        data = line %>% filter(timePeriod >= timeChop, geogConcat == input$geoChoiceOver),
        aes(ymin = min(value), ymax = value),
        fill = ifelse(change > 0, "#00703c", "#d4351c"),
        alpha = 0.3
      ) +
      geom_line(
        data = line %>% filter(timePeriod >= timeChop, geogConcat == input$geoChoiceOver),
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
        labels =
          if (format == "percent") {
            scales::percent_format(accuracy = 1)
          } else {
            label_number(accuracy = 1, scale_cut = cut_short_scale())
          },
        breaks =
          if (format == "percent") {
            c((C_axisMinMax %>% filter(metric == metricName))$minAxis, (C_axisMinMax %>% filter(metric == metricName))$maxAxis)
          } else {
            c(min(line$value), max(line$value))
          },
        limits =
          if (format == "percent") {
            c((C_axisMinMax %>% filter(metric == metricName))$minAxis - 0.001, (C_axisMinMax %>% filter(metric == metricName))$maxAxis)
          } else {
            c(min(line$value), max(line$value))
          }
      ) +
      scale_x_date(
        name = "My date axis title",
        date_breaks = "1 years",
        date_labels = "%Y"
      ) +
      if (format == "percent") {
        geom_line(
          data = line %>% filter(geogConcat == "England"),
          alpha = 0.5
        )
      } else {}
  }

  # create a function to render the overview charts
  renderOverviewChart <- function(chartData) {
    ggplotly(chartData(),
      tooltip = "text",
      height = 81
    ) %>%
      layout(
        margin = list(
          l = 0,
          r = 4,
          # increase this margin a bit to prevent the last lable dissapearing
          b = 0,
          t = 0,
          pad = 0
        ),
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  }

  #### 2.2.3.1 Employment count ----
  # Employment count KPI
  output$overviewEmpCntKPI <- renderUI({
    validate(need(input$geoChoiceOver != "", ""))
    createOverviewKPI("inemployment", "number")
  })

  # create Emp chart
  empLineChart <- eventReactive(input$geoChoiceOver, {
    createOverviewChart("inemployment", "number", "In employment")
  })

  # render empchart
  output$empLineChart <- renderPlotly({
    validate(need(input$geoChoiceOver != "", "")) # if area not yet loaded don't try to load
    renderOverviewChart(empLineChart)
  })

  #### 2.2.3.2 Employment rate ----
  output$overviewEmpRateKPI <- renderUI({
    validate(need(input$geoChoiceOver != "", ""))
    createOverviewKPI("inemploymentRate", "percent")
  })

  empRateLineChart <- eventReactive(input$geoChoiceOver, {
    createOverviewChart("inemploymentRate", "percent", "Employment rate")
  })

  output$empRateLineChart <- renderPlotly({
    validate(need(input$geoChoiceOver != "", ""))
    renderOverviewChart(empRateLineChart)
  })
  # Add link to employment rate
  observeEvent(input$link_to_tabpanel_empRate, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateSelectInput(session, "splashMetric",
      selected = "inemploymentRate"
    )
  })

  #### 2.2.3.3 Job adverts ----
  # Vacancy kpi
  output$overviewJobKPI <- renderUI({
    validate(need(input$geoChoiceOver != "", ""))
    createOverviewKPI("vacancies", "number")
  })

  # Vacancy chart
  jobLineChart <- eventReactive(input$geoChoiceOver, {
    createOverviewChart("vacancies", "number", "Online job adverts")
  })

  output$jobLineChart <- renderPlotly({
    validate(need(input$geoChoiceOver != "", ""))
    renderOverviewChart(jobLineChart)
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
  output$skisup.ETach <- renderUI({
    validate(need(input$geoChoiceOver != "", ""))
    createOverviewKPI("achievements Education and training", "number")
  })

  # e and t chart
  etLineChart <- eventReactive(input$geoChoiceOver, {
    createOverviewChart("achievements Education and training", "number", "Education and training achievements")
  })

  output$etLineChart <- renderPlotly({
    validate(need(input$geoChoiceOver != "", ""))
    renderOverviewChart(etLineChart)
  })

  #### 2.2.3.5 FE app achieve ----
  # get App data for current area
  output$skisup.APPach <- renderUI({
    validate(need(input$geoChoiceOver != "", ""))
    createOverviewKPI("achievements Apprenticeships", "number")
  })

  # app chart
  AppLineChart <- eventReactive(input$geoChoiceOver, {
    createOverviewChart("achievements Apprenticeships", "number", "Apprenticeship achievements")
  })

  output$AppLineChart <- renderPlotly({
    validate(need(input$geoChoiceOver != "", ""))
    renderOverviewChart(AppLineChart)
  })

  # Add link to skills data
  observeEvent(input$link_to_tabpanel_FE2, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateSelectInput(session, "splashMetric",
      selected = "achievements_rate_per_100000_population"
    )
  })

  #### 2.2.3.6 KS5 sustained positive destination rate ----
  # destinations overview KPI
  output$dest.ks5over <- renderUI({
    validate(need(input$geoChoiceOver != "", ""))
    createOverviewKPI("sustainedPositiveDestinationKS5Rate", "percent")
  })

  # KS5 destinations chart
  KS5LineChart <- eventReactive(input$geoChoiceOver, {
    createOverviewChart("sustainedPositiveDestinationKS5Rate", "percent", "KS5 sustained positive destination rate")
  })

  output$KS5LineChart <- renderPlotly({
    validate(need(input$geoChoiceOver != "", ""))
    renderOverviewChart(KS5LineChart)
  })

  # add link to destinations
  observeEvent(input$link_to_tabpanel_destinations2, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateSelectInput(session, "splashMetric",
      selected = "sustainedPositiveDestinationKS4Rate"
    )
  })

  #### 2.2.3.7 Micro enterprise ----
  # enterprise overview KPI
  output$UBC.micro <- renderUI({
    validate(need(input$geoChoiceOver != "", ""))
    createOverviewKPI("enterprisePctMicro", "percent")
  })

  # micro enterprise chart
  UBCLineChart <- eventReactive(input$geoChoiceOver, {
    createOverviewChart("enterprisePctMicro", "percent", "Share of businesses with 0-9 employees (micro)")
  })

  output$UBCLineChart <- renderPlotly({
    validate(need(input$geoChoiceOver != "", ""))
    renderOverviewChart(UBCLineChart)
  })

  # add link to enterprise
  observeEvent(input$link_to_tabpanel_enterprise2, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateSelectInput(session, "splashMetric",
      selected = "enterpriseCount"
    )
  })

  #### 2.2.3.8 Qualifications NVQ ----
  # NVQ3 or above overview KPI
  output$APS.nvq3plus <- renderUI({
    validate(need(input$geoChoiceOver != "", ""))
    createOverviewKPI("L3PlusRate", "percent")
  })

  # qualification chart
  Nvq3plusLineChart <- eventReactive(input$geoChoiceOver, {
    createOverviewChart("L3PlusRate", "percent", "People with a qualification at level 3 or above")
  })

  output$Nvq3plusLineChart <- renderPlotly({
    validate(need(input$geoChoiceOver != "", ""))
    renderOverviewChart(Nvq3plusLineChart)
  })

  # add link to qualification level
  observeEvent(input$link_to_tabpanel_qualification2, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateSelectInput(session, "splashMetric",
      selected = "level3AndAboveRate"
    )
  })

  #### 2.2.3.9 Working futures ----
  # This is in a slightly different format so the functions aren't used
  output$wfOverviewKpi <- renderUI({
    validate(need(input$geoChoiceOver != "", ""))
    change <- (C_Geog %>%
      filter(
        geogConcat == input$geoChoiceOver
      ))$employmentProjection

    # print with formatting
    h4(
      paste0(format(100 * change, digit = 1), "%"),
      br(),
      span(
        "growth 2023 to 2035",
        style = paste0("font-size: 16px;color:", cond_color(change > 0)) # colour formating
        ,
        .noWS = c("before", "after") # remove whitespace
      ),
      br(),
      style = "font-size: 21px"
    )
  })

  # qualification chart
  wfLineChart <- eventReactive(input$geoChoiceOver, {
    wfgeo <- C_time %>%
      filter(
        (geogConcat == input$geoChoiceOver | geogConcat == "England"),
        metric == "employmentProjection"
      )

    ggplot(wfgeo, aes(
      x = substr(chartPeriod, 3, 4),
      y = value,
      group = geogConcat,
      text = paste0(
        "Year: ",
        chartPeriod,
        "<br>",
        "Area: ",
        geogConcat,
        "<br>",
        "Year on year growth: ",
        scales::percent(round(value, 3)),
        "<br>"
      )
    )) +
      geom_line(data = wfgeo %>% filter(geogConcat == input$geoChoiceOver)) +
      geom_line(
        data = wfgeo %>% filter(geogConcat == "England"),
        alpha = 0.5
      ) +
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
        labels = scales::percent_format(accuracy = 2),
        breaks = c((C_axisMinMax %>% filter(metric == "employmentProjection"))$minAxis, (C_axisMinMax %>% filter(metric == "employmentProjection"))$maxAxis),
        limits = c((C_axisMinMax %>% filter(metric == "employmentProjection"))$minAxis - 0.001, (C_axisMinMax %>% filter(metric == "employmentProjection"))$maxAxis)
      ) +
      scale_x_discrete(breaks = c("23", "25", "27", "29", "31", "33", "35"))
  })

  output$wfOverviewChart <- renderPlotly({
    validate(need(input$geoChoiceOver != "", ""))
    renderOverviewChart(wfLineChart)
  })

  # add link to qualification level
  observeEvent(input$link_to_tabpanel_wf, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateSelectInput(session, "splashMetric",
      selected = "employmentProjection"
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

  # get current LA
  laClicked <- reactive({
    eventLA <- input$mapLA_shape_click
    C_Geog$areaName[C_Geog$areaCode == eventLA$id]
  })
  # ranking for each geog
  geogRank <- reactive({
    validate(need(input$splashGeoType != "", ""))
    C_Geog %>%
      filter(geog == input$splashGeoType) %>%
      mutate(ranking = rank(desc(eval(
        parse(text = input$splashMetric)
      )), ties.method = c("first")))
  })
  # count of areas
  groupCount <- reactive({
    validate(need(input$splashGeoType != "", ""))
    if (input$splashGeoType == "LEP") {
      "38 LEPs."
    } else {
      if (input$splashGeoType == "MCA") {
        "11 MCAs."
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
    validate(
      need("geoChoice" %in% names(input), ""),
      need(input$geoChoice != "", "")
    )
    paste0(
      (I_DataText %>% filter(metric == input$splashMetric))$subheading,
      if (input$splashMetric %in% c("vacancies", "employmentProjection")) {
        if (input$geoChoice == "Dorset LSIP") {
          " The data presented here for Dorset LSIP is correct, however for the Skills Imperative and Job Advert data, it does not match the published data. We are working to update the published data. "
        } else if (input$geoChoice == "Enterprise M3 LEP (including all of Surrey) LSIP") {
          " The published data for Skills Imperative and Job Advert for Enterprise M3 LEP (including all of Surrey) LSIP is incorrect due to the wrong LAs being included in the area. Presented here is an estimate for this LSIP compiled from other LEP and LSIP regions. As such there may be some rounding issues. We are working to update the published data. "
        } else {
          ""
        }
      } else {
        ""
      }
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
    HTML((I_DataText %>% filter(metric == input$splashMetric))$dataText)
  })
  # create data caveat
  output$dataCaveat <- renderUI({
    HTML((I_DataText %>% filter(metric == input$splashMetric))$caveatText)
  })

  ### 2.3.5 Comparison filter----
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
    validate(
      need("geoChoice" %in% names(input), ""),
      need(input$geoChoice != "", "")
    )
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
      if (str_sub(input$splashMetric, start = -4) == "Rate" | input$splashMetric == "employmentProjection" | str_sub(input$splashMetric, start = -10) == "population") {
        paste0(
          " is ",
          compareNational,
          " than the national average. It"
        )
      } else {},
      " is ranked ",
      areaRank,
      suff,
      " of the ",
      groupCount()
    )
  })

  #### 2.3.5.3 Map ----
  output$map <- renderLeaflet({
    validate(
      need(input$geoChoice != "", ""),
      need(input$splashGeoType != "", "")
    )
    mapData <- C_Geog %>% filter(geog == input$splashGeoType)
    pal <- colorNumeric("Blues", mapData[[input$splashMetric]])
    labels <-
      # if a percentage then format as %, else big number
      if (str_sub(input$splashMetric, start = -4) == "Rate" | input$splashMetric == "employmentProjection") {
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
      if (str_sub(input$splashMetric, start = -4) == "Rate" | input$splashMetric == "employmentProjection") {
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
      (I_DataText %>% filter(metric == input$splashMetric))$LatestPeriod, ". Click an area to update dashboard."
    )
  })

  ### 2.3.6 LA map ----
  #### 2.3.6.1 Title ----
  output$titleLaMap <- renderUI({
    paste0("What is the variation within ", input$geoChoice, "?")
  })
  #### 2.3.6.2 Comment----
  output$commentLA <- renderUI({
    validate(
      need("geoChoice" %in% names(input), ""),
      need(input$geoChoice != "", "")
    )
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
    if ((input$geoChoice %in% c("London LEP", "Greater London LSIP", "Greater London Authority MCA") &
      currentMetric() == "online job adverts") | (input$splashMetric == "employmentProjection")) {
      "Data is not available at LA level."
    } else {
      if (nrow(LaHighLow) == 1) {
        ""
      } # Blank if only one LA
      else {
        paste0(
          (I_DataText %>% filter(metric == input$splashMetric))$LaComment,
          " highest in ",
          LaHigh,
          " and lowest in ",
          LaLow,
          "."
        )
      }
    }
  })

  #### 2.3.6.3 Map----
  output$mapLA <- renderLeaflet({
    validate(
      need(!((input$geoChoice %in% c("London LEP", "Greater London LSIP", "Greater London Authority MCA") &
        currentMetric() == "online job adverts") | (input$splashMetric == "employmentProjection")), ""),
      need(input$geoChoice != "", "")
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
          textsize = "15px",
          direction = "auto"
        )
      )
  })

  #### 2.3.6.4 Map footnote ----
  output$mapLaFoot <- renderUI({
    validate(
      need("geoChoice" %in% names(input), ""),
      need(input$geoChoice != "", "")
    )
    if ((input$geoChoice %in% c("London LEP", "Greater London LSIP", "Greater London Authority MCA") &
      currentMetric() == "online job adverts") | (input$splashMetric == "employmentProjection")) {
      ""
    } else {
      paste0(
        (I_DataText %>% filter(metric == input$splashMetric))$LatestPeriod, ". Click an area to update other charts with LA data."
      )
    }
  })

  ### 2.3.7 Time chart ----

  # create time header
  output$titleTime <- renderUI({
    paste0("How ", (I_DataText %>% filter(metric == input$splashMetric))$timeTitle, " over time?")
  })

  #### 2.3.7.1 Comment ----
  output$commentTime <- renderUI({
    validate(
      need("geoChoice" %in% names(input), ""),
      need(input$geoChoice != "", "")
    )
    currentArea <- C_time %>%
      filter(
        geogConcat == input$geoChoice,
        metric == input$splashMetric
      )
    englandArea <- C_time %>%
      filter(
        geogConcat == "England",
        metric == input$splashMetric
      )
    currentChange <- (currentArea %>%
      filter(latest == 1))$value -
      (currentArea %>%
        filter(timePeriod == min(timePeriod)))$value
    englandChange <- (englandArea %>%
      filter(latest == 1))$value -
      (englandArea %>%
        filter(timePeriod == min(timePeriod)))$value
    paste0(
      (I_DataText %>% filter(metric == input$splashMetric))$timeComment, " ",
      input$geoChoice, if (input$splashMetric == "employmentProjection") {
        " is projected to increase "
      } else {
        if (currentChange > 0) {
          " has increased "
        } else {
          " has decreased "
        }
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
        if (input$splashMetric == "employmentProjection") {
          " to 2035."
        } else {
          " in the last five years."
        }
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
              (if (str_sub(input$splashMetric, start = -4) == "Rate" | str_sub(input$splashMetric, start = -10) == "population" | input$splashMetric == "employmentProjection") {
                (geogConcat == "England")
              } else {
                geogConcat == "\nNone"
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
            x = as.Date(timePeriod),
            y = value,
            color = Areas,
            group = Areas,
            text = paste0(
              "Period: ",
              chartPeriod,
              "<br>",
              "Area: ",
              Areas,
              "<br>",
              currentMetric(),
              ": ",
              if (str_sub(input$splashMetric, start = -4) == "Rate" | input$splashMetric == "employmentProjection") {
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
          scale_y_continuous(labels = if (str_sub(input$splashMetric, start = -4) == "Rate" | input$splashMetric == "employmentProjection") {
            scales::percent
          } else {
            label_number(accuracy = 1, scale_cut = cut_short_scale())
          }) +
          labs(colour = "") +
          scale_color_manual(values = if (str_sub(input$splashMetric, start = -4) == "Rate" | str_sub(input$splashMetric, start = -10) == "population" | input$splashMetric == "employmentProjection") {
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
    validate(
      need("geoChoice" %in% names(input), ""),
      need(input$geoChoice != "", "")
    )
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
    distinct(metric, breakdown, subgroup)
  distinctBreakdowns <- C_breakdown %>%
    distinct(metric, breakdown)
  output$breakdownFilter <- renderUI({
    validate(
      need(input$splashMetric %in% distinctBreakdowns$metric, "")
    )
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
  ))$subgroup)
  output$professionFilter <- renderUI({
    validate(
      need(input$barBreakdown != "", ""),
      need(input$barBreakdown == "Detailed Profession Category", ""),
      need(input$splashMetric %in% distinctBreakdowns$metric, "")
    )
    selectizeInput(
      inputId = "summaryProfession",
      label = "Limit to particular summary profession",
      choices = summaryCategories
    )
  })

  #### 2.3.8.2 Subgroup filter ----
  output$subgroupFilter <- renderUI({
    validate(
      need(input$barBreakdown != "", ""),
      need(input$splashMetric %in% distinctBreakdowns$metric, "")
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
                subgroup %in%
                  (C_detailLookup %>% filter(`Summary Profession Category` == input$summaryProfession))$`Detailed Profession Category`
              } else {
                TRUE
              }
            )
        ))$subgroup,
      multiple = TRUE,
      selected = (as.vector(
        C_topTenEachBreakdown %>%
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
      ))$subgroup,
      options = list(`actions-box` = TRUE, `live-search` = TRUE)
    )
  })

  #### 2.3.8.3 Title ----
  output$titleBreakdown <- renderUI({
    validate(
      need(input$barBreakdown != "", ""),
      need(input$splashMetric %in% distinctBreakdowns$metric, "")
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
      need("barBreakdown" %in% names(input) | !input$splashMetric %in% distinctBreakdowns$metric, ""),
      need(input$geoChoice != "", "")
    )

    if (!input$splashMetric %in% distinctBreakdowns$metric) {
      paste0(
        str_to_sentence(currentMetric()),
        " currently has no breakdowns.",
        if (input$splashMetric %in% c(
          "inemploymentRate",
          "selfemployedRate",
          "unemployedRate",
          "inactiveRate",
          "selfemployed",
          "unemployed",
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
        group_by(subgroup) %>%
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
        breakdownDiff$subgroup,
        " than the national average. ",
        if (nrow(C_breakdown %>%
          filter(breakdown == input$barBreakdown) %>%
          distinct(subgroup)) > 10) {
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
        subgroup %in% input$barSubgroup,
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
            x = reorder(subgroup, value, mean),
            y = value,
            fill = Area,
            text = paste0(
              "Area: ",
              Area,
              "<br>",
              currentMetric(),
              ": ",
              if (str_sub(input$splashMetric, start = -4) == "Rate" |
                input$splashMetric == "inemployment" |
                input$splashMetric == "vacancies" |
                input$splashMetric == "enterpriseCount" |
                input$splashMetric == "achievements" |
                input$splashMetric == "participation" |
                input$splashMetric == "employmentProjection" |
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
            input$splashMetric == "inemployment" |
            input$splashMetric == "vacancies" |
            input$splashMetric == "enterpriseCount" |
            input$splashMetric == "achievements" |
            input$splashMetric == "participation" |
            input$splashMetric == "employmentProjection" |
            input$splashMetric == "starts") {
            scales::percent
          } else {
            label_number(accuracy = 1, scale_cut = cut_short_scale())
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

  output$breadownPlot <- renderUI({
    if ("barBreakdown" %in% names(input) && input$barBreakdown == "No breakdowns available") {} else {
      withSpinner(plotlyOutput("Splash_pc"))
    }
  })

  #### 2.3.8.6 Bar footnote ----
  output$breakdownFoot <- renderUI({
    validate(
      need(input$barBreakdown != "", ""),
      need(input$barBreakdown != "No breakdowns available", "")
    )
    if (input$splashMetric == "inemployment" & input$barBreakdown == "Occupation") {
      "Jan 2021-Dec 2021 data."
    } # Occupation data is older because of the SOC issue
    else {
      paste0(
        (I_DataText %>% filter(metric == input$splashMetric))$LatestPeriod, "."
      )
    }
  })

  ### 2.3.9 Downloads local skills ----
  # all areas
  listDownloadV1All <- reactive({
    list(
      "AllArea" = filter(C_time, metric == input$splashMetric) %>%
        mutate(metric = case_when(metric == "employmentProjection" ~ "ProjectedYearOnYearEmploymentGrowth", TRUE ~ metric)) %>%
        select(-latest, -valueText, -timePeriod) %>%
        rename(Area = geogConcat, Period = chartPeriod, Metric = metric, Value = value),
      "AllAreaBreakdown" = filter(C_breakdown, metric == input$splashMetric) %>%
        mutate(metric = case_when(metric == "employmentProjection" ~ "ProjectedEmploymentGrowthTo2035", TRUE ~ metric)) %>%
        select(-valueText) %>%
        rename(Area = geogConcat, Metric = metric, Value = value, Breakdown = breakdown, Subgroup = subgroup)
    )
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
    list(
      "CurrentArea" = filter(
        C_time,
        metric == input$splashMetric,
        (geogConcat == input$geoChoice |
          geogConcat %in% input$geoComps |
          geogConcat == "England")
      ) %>%
        mutate(metric = case_when(metric == "employmentProjection" ~ "ProjectedYearOnYearEmploymentGrowth", TRUE ~ metric)) %>%
        select(-latest, -valueText, -timePeriod) %>%
        rename(Area = geogConcat, Period = chartPeriod, Metric = metric, Value = value),
      "CurrentAreaBreakdown" = filter(
        C_breakdown,
        metric == input$splashMetric,
        (geogConcat == input$geoChoice |
          geogConcat %in% input$geoComps |
          geogConcat == "England")
      ) %>%
        mutate(metric = case_when(metric == "employmentProjection" ~ "ProjectedEmploymentGrowthTo2035", TRUE ~ metric)) %>%
        select(-valueText) %>%
        rename(Area = geogConcat, Metric = metric, Value = value, Breakdown = breakdown, Subgroup = subgroup)
    )
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
          Area %in% input$hubArea
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
          Area %in% input$hubArea
        },
        if (is.null(input$hubMetric) == TRUE) {
          TRUE
        } else {
          metricNeat %in% input$hubMetric
        }
      ) %>% distinct(Breakdown),
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
          Area %in% input$hubArea
        },
        if (is.null(input$hubMetric) == TRUE) {
          TRUE
        } else {
          metricNeat %in% input$hubMetric
        },
        if (is.null(input$hubBreakdowns) == TRUE) {
          TRUE
        } else {
          Breakdown %in% input$hubBreakdowns
        }
      ) %>%
        distinct("Time period" = Period),
      multiple = TRUE,
      label = NULL,
      options = list(placeholder = "Choose period*")
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
            Area %in% input$hubArea
          } |
            (if ("Yes" %in% input$hubLA) {
              Area %in% (
                C_Geog %>% filter(geog == "LADU", (LEP %in% input$hubArea | LSIP %in% input$hubArea | MCA %in% input$hubArea))
                  %>% distinct(geogConcat)
              )$geogConcat
            } else {
              Area == "xxx"
            }) |
            (if ("National" %in% input$hubComparators) {
              Area == "England"
            } else {
              Area == "xxx"
            })
        }),
        if (is.null(input$hubYears) == TRUE) {
          TRUE
        } else {
          Period %in% input$hubYears
        },
        if (is.null(input$hubMetric) == TRUE) {
          TRUE
        } else {
          metricNeat %in% input$hubMetric
        },
        (if (is.null(input$hubBreakdowns) == TRUE) {
          TRUE
        } else {
          Breakdown %in% input$hubBreakdowns
        })
      ) %>%
      select(
        Period = Period,
        Area,
        Data = metricNeat,
        Breakdown,
        Subgroup,
        Value = valueText
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
      "LocalSkillsDataset.xlsx"
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
