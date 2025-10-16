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

  # We use bookmarking to store input choices in the url.
  # We exclude these inputs:
  observe({
    setBookmarkExclude(c(
      "link_to_tabpanel_accessibility", "link_to_tabpanel_supportandfeedback", "cookies",
      ".clientValue-default-plotlyCrosstalkOpts", "plotly_afterplot-A",
      "plotly_relayout-A", "plotly_hover-A", "plotly_click-A", "DataTbl_rows_selected",
      "DataTbl_columns_selected", "DataTbl_cells_selected", "DataTbl_rows_current",
      "DataTbl_rows_all", "DataTbl_state", "DataTbl_search", "DataTbl_cell_clicked",
      "DataTbl_row_last_clicked", "mapLA_bounds", "mapLA_center", "mapLA_zoom", "map_bounds",
      "map_center", "map_zoom", "mapLA_shape_click", "mapLA_click",
      "link_to_tabpanel_data", "link_to_tabpanel_dataSources", "link_to_tabpanel_furtherresources", "link_to_tabpanel_localskills2",
      "link_to_tabpanel_localskills", "link_to_tabpanel_overview", "cookies_banner-cookies_reject",
      "cookies_banner-cookies_accept", "cookies_banner-cookies_link", "link_to_tabpanel_employment",
      "link_to_tabpanel_vacancies", "link_to_tabpanel_enterprise", "link_to_tabpanel_FE", "link_to_tabpanel_destinations",
      "link_to_tabpanel_wf1", "link_to_tabpanel_empRate", "link_to_tabpanel_vacancies2", "link_to_tabpanel_FE2",
      "link_to_tabpanel_wf", "link_to_tabpanel_enterprise2", "link_to_tabpanel_LS", "cookies_panel-submit_btn",
      "cookies_panel-cookies_analytics", "summaryTopProjectedListTable_rows_selected", "summaryTopProjectedListTable_columns_selected",
      "summaryTopProjectedListTable_cells_selected", "summaryTopProjectedListTable_rows_current",
      "summaryTopProjectedListTable_rows_all", "summaryTopProjectedListTable_state", "summaryTopProjectedListTable_search",
      "summaryTopProjectedListTable_cell_clicked", "summaryTopProjectedListTable_row_last_clicked",
      "map_shape_mouseover", "map_shape_mouseout", "map_shape_click", "map_click", "barSubgroup_open",
      "mapLA_shape_mouseover", "mapLA_shape_mouseout", "hubTable_rows_selected", "hubTable_columns_selected",
      "hubTable_cells_selected", "hubTable_rows_current", "hubTable_rows_all", "hubTable_state",
      "hubTable_search", "hubTable_cell_clicked", "hubTable_row_last_clicked", "reportsTable_rows_selected",
      "reportsTable_columns_selected", "reportsTable_cells_selected", "reportsTable_rows_current",
      "reportsTable_rows_all", "reportsTable_state", "reportsTable_search", "reportsTable_cell_clicked",
      "sourcesTable_rows_selected", "sourcesTable_columns_selected", "sourcesTable_cells_selected",
      "sourcesTable_rows_current", "sourcesTable_rows_all", "sourcesTable_state", "sourcesTable_search",
      "sourcesTable_cell_clicked", "toolsTable_rows_selected", "toolsTable_columns_selected",
      "toolsTable_cells_selected", "toolsTable_rows_current", "toolsTable_rows_all", "toolsTable_state",
      "toolsTable_search", "toolsTable_cell_clicked", "reportsTable_row_last_clicked", "sourcesTable_row_last_clicked",
      "toolsTable_row_last_clicked", "barSubgroup", "geoComps", "breakdownPage",
      "subgroupPage", "barBreakdown", "barProfession", "hubLA", "hubComparators", "hubYears", "hubBreakdowns",
      "hubMetric", "hubArea", "accessibility_footer_link", "cookies_footer_link", "support_footer_link"
    ))
  })

  observe({
    # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })

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

  ## 1.3 Set up cookies ----
  dfeshiny::cookies_banner_server(
    input_cookies = reactive(input$cookies),
    parent_session = session,
    google_analytics_key = google_analytics_key,
    cookies_link_panel = "cookies_panel"
  )
  dfeshiny::cookies_panel_server(
    id = "cookies_panel",
    input_cookies = reactive(input$cookies),
    google_analytics_key = google_analytics_key
  )

  # Manage page naviogation
  observeEvent(input$summary, bslib::nav_select("navbar", "summary"))
  observeEvent(input$local_skills_data, bslib::nav_select("navbar", "local_skills_data"))
  observeEvent(input$data_download, bslib::nav_select("navbar", "data_download"))
  observeEvent(input$user_guide, bslib::nav_select("navbar", "user_guide"))

  observeEvent(input$go_summary, {
    bslib::nav_select("navbar", "summary")
  })
  observeEvent(input$go_skills, {
    bslib::nav_select("navbar", "local_skills_data")
  })
  observeEvent(input$go_download, {
    bslib::nav_select("navbar", "data_download")
  })

  # 2 User guide ----
  ## 2.1 Make links ----
  # Create link to overview tab from user guide
  observeEvent(input$link_to_tabpanel_overview, {
    bslib::nav_select("navbar", "summary")
  })

  # Create links to local skills tab from user guide
  observeEvent(input$link_to_tabpanel_localskills, {
    bslib::nav_select("navbar", "local_skills_data")
  })
  observeEvent(input$link_to_tabpanel_localskills2, {
    bslib::nav_select("navbar", "local_skills_data")
  })

  # Create link to further resources tab
  observeEvent(input$link_to_tabpanel_furtherresources, {
    bslib::nav_select("navbar", "further_resources")
  })

  # Create link to accessibility tab
  observeEvent(input$link_to_tabpanel_accessibility, {
    bslib::nav_select("navbar", "accessibility")
  })

  # Create link to support and feedback tab
  observeEvent(input$link_to_tabpanel_supportandfeedback, {
    bslib::nav_select("navbar", "support_and_feedback")
  })

  # Create link to employment data
  observeEvent(input$link_to_tabpanel_employment, {
    bslib::nav_select("navbar", "local_skills_data")
    updateSelectInput(session, "splashMetric",
      selected = "inemploymentRate"
    )
  })

  # Create link to job advert
  observeEvent(input$link_to_tabpanel_vacancies, {
    bslib::nav_select("navbar", "local_skills_data")
    updateSelectInput(session, "splashMetric",
      selected = "vacancies"
    )
  })
  # Create link to skills data tab
  observeEvent(input$link_to_tabpanel_FE, {
    bslib::nav_select("navbar", "local_skills_data")
    updateSelectInput(session, "splashMetric",
      selected = "achievements_rate_per_100000_population"
    )
  })
  # Create link to enterprises from user guide
  observeEvent(input$link_to_tabpanel_enterprise, {
    bslib::nav_select("navbar", "local_skills_data")
    updateSelectInput(session, "splashMetric",
      selected = "enterpriseCount"
    )
  })
  # Create link to enterprises from summary
  observeEvent(input$link_to_tabpanel_enterprise2, {
    bslib::nav_select("navbar", "local_skills_data")
    updateSelectInput(session, "splashMetric",
      selected = "enterpriseCount"
    )
  })
  # Create link to qualification
  observeEvent(input$link_to_tabpanel_qualification, {
    bslib::nav_select("navbar", "local_skills_data")
    updateSelectInput(session, "splashMetric",
      selected = "L3PlusRate"
    )
  })
  # Create link to destinations
  observeEvent(input$link_to_tabpanel_destinations, {
    bslib::nav_select("navbar", "local_skills_data")
    updateSelectInput(session, "splashMetric",
      selected = "sustainedPositiveDestinationKS4Rate"
    )
  })
  # Create link to working futures user guide
  observeEvent(input$link_to_tabpanel_wf1, {
    bslib::nav_select("navbar", "local_skills_data")
    updateSelectInput(session, "splashMetric",
      selected = "employmentProjection"
    )
  })
  # Create link to working futures from Summary
  observeEvent(input$link_to_tabpanel_wf, {
    bslib::nav_select("navbar", "local_skills_data")
    updateSelectInput(session, "splashMetric",
      selected = "employmentProjection"
    )
  })
  # Create link to data tab
  observeEvent(input$link_to_tabpanel_data, {
    bslib::nav_select("navbar", "data_download")
  })
  # Create link to data tab
  observeEvent(input$link_to_tabpanel_dataSources, {
    bslib::nav_select("navbar", "data_sources")
  })
  # 3 Data sources ----
  ## 3.1 Data table download links----
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
        filter(metric == "inemployment", Breakdown == "Occupation (SOC2020 Sub-Major Group)") %>%
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
        filter(metric == "achievementsAims", Breakdown == "SSA") %>%
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

  # Create download links
  output$hidden_downloads <- renderUI(lapply(1:13, function(i) {
    downloadLink(paste0("downloadData", i), "download", class = "hiddenLink")
  }))

  ## 3.1 Data table ----
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

  # 4 Summary ----

  observeEvent(input$nav_click, {
    req(input$nav_click)
    # select the matching nav_panel in the navset with id = "navbar"
    bslib::nav_select(id = "navbar", selected = input$nav_click, session = session)
  })

  # Add link to local skills data
  observeEvent(input$link_to_tabpanel_LS, {
    bslib::nav_select("navbar", "local_skills_data")
  })

  # define page title
  output$page0title <- renderUI({
    paste0(input$geoChoiceOver, " headline data")
  })

  # create subheading
  output$subheadingSummary <- renderUI({
    req(input$geoChoiceOver)
    paste0(
      if (input$geoChoiceOver %in% c(
        "Greater Devon LSIP", "Greater Lincolnshire LSIP",
        "Hampshire and the Solent LSIP", "Leicester, Leicestershire and Rutland LSIP", "North East LSIP",
        "Somerset LSIP", "Surrey LSIP",
        "Warwickshire LSIP", "West Midlands LSIP"
      )) {
        " The boundaries for this LSIP area were updated in September 2025. See data sources page for more information."
      } else if (input$geoChoiceOver %in% c(
        "Central London Forward LSIP", "Local London LSIP", "South London Partnership LSIP",
        "West London Alliance LSIP"
      )) {
        " London wide data is available by choosing the Greater London Authority under Combined authorities."
      } else if (input$geoChoiceOver == "Greater London Authority CA") {
        " Data is available for 4 sub London areas as well. These can be chosen from the area filter under LSIPs."
      }
    )
  })

  ## 4.1 Filter ----
  # alter area dropdown if changed on other tab
  observeEvent(input$geoChoice, {
    if (input$geoChoiceOver != input$geoChoice) {
      updateSelectInput(session, "geoChoiceOver",
        selected = input$geoChoice
      )
    }
  })

  ## 4.2 Screenshot----
  output$screenshotOverview <- renderUI({
    capture::capture(
      selector = "body",
      filename = paste0(input$geoChoiceOver, "-overview", ".png"),
      icon("camera"),
      "Screenshot"
    )
  })

  ##  4.3 Downloads ----
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
        filter(metric == "inemployment", Breakdown == "Occupation (SOC2020 Sub-Major Group)") %>%
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
        filter(metric == "achievementsAims", Breakdown == "SSA") %>%
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

  ## 4.4 KPIs and charts----
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
    paste0(
      # print with formatting
      if (format == "percent") {
        paste0(format(100 * latest, digit = 2), "%")
      } else {
        format(latest, big.mark = ",")
      },
      br(),
      span( # plus-minus and comma sep formatting
        paste(
          if (format == "percent") {
            paste0(sprintf("%+.0f", 100 * change), "ppts")
          } else {
            format_pm(change)
          },
          " since ",
          (currentGeogTime() %>% filter(metric == metricName) %>%
            filter(latest == -1))$chartPeriod
        ),
        style = paste0("font-size: 16px;color:", cond_color(change)), # colour formating
        .noWS = c("before", "after") # remove whitespace
      )
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
        fill = ifelse(change > 0.0005, "#00703c", ifelse(change < -0.0005, "#d4351c", "#000")),
        alpha = 0.3
      ) +
      geom_line(
        data = line %>% filter(timePeriod >= timeChop, geogConcat == input$geoChoiceOver),
        color = ifelse(change > 0.0005, "#00703c", ifelse(change < -0.0005, "#d4351c", "#000"))
      ) +
      theme_classic() +
      theme(
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()
      ) +
      scale_y_continuous(
        labels =
          if (format == "percent") {
            scales::percent_format(accuracy = 1)
          } else {
            label_number(accuracy = 1, scale_cut = append(scales::cut_short_scale(), 1, 1))
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
    ggplotly(chartData,
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

  createOverviewTitle <- function(metricName) {
    span(
      (currentGeogTime() %>% filter(metric == metricName) %>%
        filter(latest == 1))$chartPeriod,
      style = "color:grey"
    )
  }

  ### 4.4.1 Employment rate ----
  # Create title
  output$overviewEmpRateTitle <- renderText({
    paste0(
      p("Workforce: employment rate", style = "font-weight: bold; font-size: 18px;"),
      createOverviewTitle("inemploymentRate")
    )
  })
  # Get value
  output$overviewEmpRateKPI <- renderText({
    createOverviewKPI("inemploymentRate", "percent")
  })
  # Create chart
  output$empRateLineChart <- renderPlotly({
    renderOverviewChart(createOverviewChart("inemploymentRate", "percent", "Employment rate"))
  })
  # Add link to employment rate
  observeEvent(input$link_to_tabpanel_empRate, {
    bslib::nav_select("navbar", "local_skills_data")
    updateSelectInput(session, "splashMetric",
      selected = "inemploymentRate"
    )
  })

  ### 4.4.2 Job adverts ----
  # Create title
  output$overviewJobTitle <- renderText({
    paste0(
      p("Demand: online job adverts", style = "font-weight: bold; font-size: 18px;"),
      if (input$geoChoiceOver %in% c(
        "Central London Forward LSIP", "Local London LSIP", "South London Partnership LSIP", "West London Alliance LSIP"
      )) {
        "There is no London LA level job advert data so this LSIP cannot be calculated."
      } else {
        createOverviewTitle("vacancies")
      }
    )
  })

  # Vacancy kpi
  output$overviewJobKPI <- renderText({
    req(!input$geoChoiceOver %in% c(
      "Central London Forward LSIP", "Local London LSIP", "South London Partnership LSIP", "West London Alliance LSIP"
    ))
    createOverviewKPI("vacancies", "number")
  })

  # Vacancy chart
  output$jobLineChart <- renderPlotly({
    req(!input$geoChoiceOver %in% c(
      "Central London Forward LSIP", "Local London LSIP", "South London Partnership LSIP", "West London Alliance LSIP"
    ))
    renderOverviewChart(createOverviewChart("vacancies", "number", "Online job adverts"))
  })

  # Add link to vacancy data
  observeEvent(input$link_to_tabpanel_vacancies2, {
    bslib::nav_select("navbar", "local_skills_data")
    updateSelectInput(session, "splashMetric",
      selected = "vacancies"
    )
  })

  ### 4.4.3 FE app achieve ----
  # Create title
  output$overviewAppTitle <- renderText({
    paste0(
      p("Skills supply: apprenticeship achievements", style = "font-weight: bold; font-size: 18px;"),
      createOverviewTitle("achievementsProvisionApprenticeships")
    )
  })

  # get App data for current area
  output$overviewAppKPI <- renderText({
    createOverviewKPI("achievementsProvisionApprenticeships", "number")
  })

  # app chart
  output$AppLineChart <- renderPlotly({
    renderOverviewChart(createOverviewChart("achievementsProvisionApprenticeships", "number", "Apprenticeship achievements"))
  })

  # Add link to skills data
  observeEvent(input$link_to_tabpanel_FE2, {
    bslib::nav_select("navbar", "local_skills_data")
    updateSelectInput(session, "splashMetric",
      selected = "achievements_rate_per_100000_population"
    )
  })

  ## 4.5 Skills projections ----
  # Top projected jobs sentence
  output$summaryTopProjected <- renderUI({
    if (input$geoChoiceOver %in% c(
      "Central London Forward LSIP", "Greater Devon LSIP", "Greater Lincolnshire LSIP",
      "Hampshire and the Solent LSIP", "Leicester, Leicestershire and Rutland LSIP", "North East LSIP",
      "Local London LSIP", "Somerset LSIP", "South London Partnership LSIP", "Surrey LSIP",
      "Warwickshire LSIP", "West London Alliance LSIP", "West Midlands LSIP"
    )) {
      "The boundaries of this LSIP have changed since this data was published so there is no associated employment projection data."
    } else if (input$geoChoiceOver %in% c("Devon and Torbay CA", "Greater Lincolnshire CA", "Hull and East Yorkshire CA", "Lancashire CA")) {
      "This CA was created after this data was published so there is no associated employment projection data."
    } else {
      paste0(
        "From 2024 to 2035 ", input$geoChoiceOver, " is projected to grow ",
        format((C_Geog %>%
          filter(
            geogConcat == input$geoChoiceOver
          ))$employmentProjection * 100, digit = 1),
        "%",
        ifelse(input$geoChoiceOver == "England", "", paste0(
          " (compared to ",
          format((C_Geog %>%
            filter(
              geogConcat == "England"
            ))$employmentProjection * 100, digit = 1),
          "% nationally)"
        )),
        ". These are the top projected growth occupations in the area:"
      )
    }
  })

  # get top growing occupations
  summaryTopProjectedList <- reactive({
    C_breakdown %>%
      dplyr::filter(
        geogConcat == input$geoChoiceOver,
        metric == "employmentProjection",
        breakdown == "Occupation (SOC2020 Sub-Major Group)"
      ) %>%
      dplyr::arrange(dplyr::desc(value)) %>%
      dplyr::slice_head(n = 5) %>%
      select(subgroup, value) %>%
      left_join(C_breakdown %>%
        dplyr::filter(
          geogConcat == "England",
          metric == "employmentProjection",
          breakdown == "Occupation (SOC2020 Sub-Major Group)"
        ) %>%
        rename(National = value), by = join_by(subgroup)) %>%
      mutate(
        value = label_percent(accuracy = 1)(value),
        National = label_percent(accuracy = 1)(National),
        subgroup = gsub("[[:digit:]]+", "", gsub(" - ", "", subgroup))
      ) %>%
      select(Occupation = subgroup, `Local growth` = value, `National growth` = National)
  })

  output$summaryTopProjectedListTable <- renderDataTable({
    req(!input$geoChoiceOver %in% c(
      "Central London Forward LSIP", "Greater Devon LSIP", "Greater Lincolnshire LSIP",
      "Hampshire and the Solent LSIP", "Leicester, Leicestershire and Rutland LSIP", "North East LSIP",
      "Local London LSIP", "Somerset LSIP", "South London Partnership LSIP", "Surrey LSIP",
      "Warwickshire LSIP", "West London Alliance LSIP", "West Midlands LSIP",
      "Devon and Torbay CA", "Greater Lincolnshire CA", "Hull and East Yorkshire CA", "Lancashire CA"
    ))
    DT::datatable(summaryTopProjectedList(),
      options = list(
        info = FALSE,
        paging = FALSE,
        searching = FALSE,
        rownames = FALSE
      )
    )
  })

  ## 4.6 Businesses ----

  # Get data
  chartData <- reactive({
    areaBreakdownData <- C_breakdown %>%
      dplyr::filter(geogConcat %in% c(input$geoChoiceOver, "England"))

    lsipData <- areaBreakdownData %>%
      dplyr::filter(
        metric == "enterpriseCount",
        breakdown == "Industry",
        geogConcat == input$geoChoiceOver
      ) %>%
      left_join(areaBreakdownData %>%
        dplyr::filter(
          metric == "enterpriseCount",
          breakdown == "Industry",
          geogConcat == "England"
        ) %>%
        select(subgroup, valueEngland = value), by = join_by(subgroup)) %>%
      mutate(diff = value - valueEngland) %>%
      mutate(extremes = case_when(
        diff == max(diff) ~ "top",
        diff == min(diff) ~ "bottom",
        TRUE ~ subgroup
      )) %>%
      dplyr::mutate(subgroup = gsub("[[:digit:]]+ - ", "", subgroup)) %>%
      dplyr::mutate(geogOrder = tail(strsplit(geogConcat, split = " ")[[1]], 1))

    # get England
    englandData <- areaBreakdownData %>%
      dplyr::filter(
        metric == "enterpriseCount",
        breakdown == "Industry",
        geogConcat == "England"
      ) %>%
      left_join(lsipData %>% select(subgroup, extremes), by = join_by(subgroup)) %>%
      dplyr::mutate(geogOrder = " England")

    bind_rows(lsipData, englandData) %>%
      filter(extremes %in% c("top", "bottom"))
  })

  # Businesses sentence
  output$summaryBusinesses <- renderText({
    currentArea <- currentGeogTime() %>%
      filter(
        metric == "enterpriseCount"
      )
    englandArea <- englandTime %>%
      filter(
        metric == "enterpriseCount"
      )
    currentChange <- (currentArea %>%
      filter(latest == 1))$value -
      (currentArea %>%
        filter(timePeriod == min(timePeriod)))$value
    englandChange <- (englandArea %>%
      filter(latest == 1))$value -
      (englandArea %>%
        filter(timePeriod == min(currentArea$timePeriod)))$value # match with the area data

    paste0(
      "In ",
      (currentArea %>%
        dplyr::filter(latest == 1)
      )$chartPeriod,
      ", ",
      format((currentArea %>%
        dplyr::filter(latest == 1)
      )$value, big.mark = ","),
      " businesses were active in ",
      input$geoChoiceOver,
      ". The number of businesses has ",
      ifelse(currentChange > 0.0005, "grown ", ifelse(currentChange < -0.0005, "fallen ", "remained broadly stable (less than 0.1% change)")),
      ifelse(abs(currentChange) < 0.0005, "", label_percent(accuracy = 0.1)(currentChange / (currentArea %>%
        filter(timePeriod == min(timePeriod)))$value)),
      " in the last four years",
      ifelse(input$geoChoiceOver == "England", ".",
        paste0(
          ", while across England businesses have ",
          ifelse(englandChange > 0.0005, "grown ", ifelse(englandChange < -0.0005, "fallen ", "remained broadly stable (less than 0.1% change)")),
          ifelse(abs(currentChange) < 0.0005, "", label_percent(accuracy = 0.1)(englandChange / (englandArea %>%
            filter(timePeriod == min(currentArea$timePeriod)))$value)),
          ". Across this area, there is a higher proportion of ",
          (chartData() %>% filter(extremes == "top"))$subgroup[1],
          " businesses than across England, and a lower proportion of ",
          (chartData() %>% filter(extremes == "bottom"))$subgroup[1],
          " businesses. "
        )
      )
    )
  })

  # Businesses title top
  output$summaryBusinessesTop <- renderText({
    req(input$geoChoiceOver != "England")
    paste0(
      "Percentage of businesses in ",
      (chartData() %>% filter(extremes == "top"))$subgroup[1]
    )
  })

  # Businesses title bottom
  output$summaryBusinessesBottom <- renderText({
    req(input$geoChoiceOver != "England")
    paste0(
      "Percentage of businesses in ",
      (chartData() %>% filter(extremes == "bottom"))$subgroup[1]
    )
  })

  summaryBusinessesPlotTop <- eventReactive(input$geoChoiceOver, {
    chartData <- chartData() %>% filter(extremes == "top")
    chartData$extremes <- reorder(chartData$extremes, chartData$value, sum)
    # plot
    ggplot2::ggplot(
      chartData,
      ggplot2::aes(
        x = geogOrder,
        y = value,
        fill = geogOrder,
        text = paste0(
          geogConcat,
          "<br>",
          "Percentage of businesses in ",
          subgroup,
          ": ",
          label_percent(accuracy = 1)(value)
        )
      )
    ) +
      ggplot2::geom_col() +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal() +
      ggplot2::labs(fill = "") +
      ggplot2::theme(
        legend.position = "none",
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      ) +
      ggplot2::scale_fill_manual(values = c(
        "lightgrey", "#12436D"
      ))
  })

  output$summaryBusinessesChartTop <- renderPlotly({
    req(input$geoChoiceOver != "England")
    ggplotly(summaryBusinessesPlotTop(), tooltip = "text") %>%
      layout(
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })

  summaryBusinessesPlotBottom <- eventReactive(input$geoChoiceOver, {
    chartData <- chartData() %>% filter(extremes == "bottom")
    chartData$extremes <- reorder(chartData$extremes, chartData$value, sum)
    # plot
    ggplot2::ggplot(
      chartData,
      ggplot2::aes(
        x = geogOrder,
        y = value,
        fill = geogOrder,
        text = paste0(
          geogConcat,
          "<br>",
          "Percentage of businesses in ",
          subgroup,
          ": ",
          label_percent(accuracy = 1)(value)
        )
      )
    ) +
      ggplot2::geom_col() +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal() +
      ggplot2::labs(fill = "") +
      ggplot2::theme(
        legend.position = "none",
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      ) +
      ggplot2::scale_fill_manual(values = c(
        "lightgrey", "#12436D"
      ))
  })

  output$summaryBusinessesChartBottom <- renderPlotly({
    req(input$geoChoiceOver != "England")
    ggplotly(summaryBusinessesPlotBottom(), tooltip = "text") %>%
      layout(
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })

  # 5 Local skills----

  ## 5.1 Reusable variables----
  # get current metric in plain English
  currentMetricClean <- reactive({
    sub("fe", "FE", tolower(gsub("^.*\\.", "", names(
      unlist(metricChoices)[unlist(metricChoices) == input$splashMetric]
    ))))
  })

  # filter for just england
  englandGeog <- C_Geog %>%
    filter(geog == "England" & areaName == "England")

  # flag for if there is no data and write error message
  noData <- reactive({
    if (input$splashMetric == "vacancies" & grepl("London", input$geoChoice)) {
      "There is no LA level job advert data available for London, so this LSIP cannot be calculated."
    } else if (input$splashMetric == "employmentProjection" & input$geoChoice %in% c(
      "Central London Forward LSIP", "Greater Devon LSIP", "Greater Lincolnshire LSIP",
      "Hampshire and the Solent LSIP", "Leicester, Leicestershire and Rutland LSIP", "North East LSIP",
      "Local London LSIP", "Somerset LSIP", "South London Partnership LSIP", "Surrey LSIP",
      "Warwickshire LSIP", "West London Alliance LSIP", "West Midlands LSIP"
    )) {
      "The geography of this LSIP has changed since the Skills Imperative data was published, so no data is available here."
    } else if (input$splashMetric == "employmentProjection" & input$geoChoice %in% c(
      "Devon and Torbay CA", "Greater Lincolnshire CA", "Hull and East Yorkshire CA", "Lancashire CA"
    )) {
      "This CA was created after the Skills Imperative data was published, so no data is available here."
    } else {
      ""
    }
  })

  ## 5.2 Screenshot----
  output$screenshotFile <- renderUI({
    capture::capture(
      selector = "body",
      filename = paste0(input$geoChoice, "-", input$splashMetric, ".png"),
      button_class = "btn btn-default btn-block",
      icon("camera"),
      "Screenshot"
    )
  })

  ## 5.3 Dynamic text----
  # create subheading
  output$subheading <- renderUI({
    req(input$geoChoice)
    paste0(
      (I_DataText %>% filter(metric == input$splashMetric))$subheading,
      if (input$geoChoice == "Dorset LSIP" & input$splashMetric == "employmentProjection") {
        " The data presented here for Dorset LSIP is correct, however for the Skills Imperative data, it does not match the published data. We are working to update the published data. "
      } else if (input$geoChoice %in% c(
        "Greater Devon LSIP", "Greater Lincolnshire LSIP",
        "Hampshire and the Solent LSIP", "Leicester, Leicestershire and Rutland LSIP", "North East LSIP",
        "Somerset LSIP", "Surrey LSIP",
        "Warwickshire LSIP", "West Midlands LSIP"
      )) {
        " The boundaries for this LSIP area were updated in September 2025. See data sources page for more information."
      } else if (input$geoChoice %in% c(
        "Central London Forward LSIP", "Local London LSIP", "South London Partnership LSIP",
        "West London Alliance LSIP"
      )) {
        " London wide data is available by choosing the Greater London Authority under Combined authorities."
      } else if (input$geoChoice == "Greater London Authority CA") {
        " Data is available for 4 sub London areas as well. These can be chosen from the area filter under LSIPs."
      }
    )
  })

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

  ## 5.4 Filters----
  # Update if summary tab filter changed
  observeEvent(input$geoChoiceOver, {
    if (input$geoChoice != input$geoChoiceOver) {
      updateSelectizeInput(session, "geoChoice",
        selected = input$geoChoiceOver
      )
    }
  })

  # List the breakdown and the subgroups
  distinctSubgroups <- C_breakdown %>%
    distinct(metric, breakdown, subgroup)
  distinctBreakdowns <- C_breakdown %>%
    distinct(metric, breakdown)

  # set current metric (and a versions with spaces for printing) up as reactive var - it will change as users activate filters
  currentMetric <- reactiveVal(NULL)
  currentMetricSpaces <- reactiveVal(NULL)

  # Updates that occur when the metric changes
  observeEvent(input$splashMetric,
    {
      types <- distinctBreakdowns$breakdown[distinctBreakdowns$metric == input$splashMetric] # find subgroups in metric
      # If there are no subgroups for the metric then hide all the breakdown filters
      if (length(types) == 0) {
        shinyjs::hide("breakdownPage_wrapper")
        updateSelectInput(session, "breakdownPage", choices = "", selected = "")
        shinyjs::hide("subgroupPage_wrapper")
        updateSelectInput(session, "subgroupPage", choices = "", selected = "")
        shinyjs::hide("breakdownBar_wrapper")
        updateSelectInput(session, "barBreakdown", choices = "", selected = "")
        shinyjs::hide("professionBar_wrapper")
        updateSelectInput(session, "barProfession", choices = "", selected = "")
        shinyjs::hide("subgroupBar_wrapper")
        updatePickerInput(inputId = "barSubgroup", choices = "", selected = "")
      } else { # if there are subgroups then show the subgroup type filters
        shinyjs::show("breakdownPage_wrapper")
        updateSelectInput(session, "breakdownPage", choices = c("All", types), selected = "All")
        shinyjs::show("breakdownBar_wrapper")
        updateSelectInput(session, "barBreakdown", choices = types, selected = types[1])
      }
      # Update currentMetric to just the main metric (no subgroups are chosen when a user changes metric)
      currentMetric(input$splashMetric)
    },
    ignoreInit = FALSE # <-- ensure it runs at startup
  )

  # Updates that occur when the top breakdown filter changes
  observeEvent(input$breakdownPage,
    {
      # if breakdown doesn't exists or is all, then hide the top subgroup filter but show the barchart filter
      if (is.null(input$breakdownPage) || input$breakdownPage == "" || input$breakdownPage == "All") {
        shinyjs::hide("subgroupPage_wrapper")
        updateSelectInput(session, "subgroupPage", choices = "", selected = "")
        # Update metric to just the main metric as type is set to All
        currentMetric(input$splashMetric)
        # also turn on the bar chart filter to allow the option from there
        shinyjs::show("breakdownBar_wrapper")
      } else {
        # if breakdown option is chosen then show relevant subgroups in top subgroup filter
        shinyjs::show("subgroupPage_wrapper")
        subgroups <- distinctSubgroups$subgroup[distinctSubgroups$metric == input$splashMetric & distinctSubgroups$breakdown == input$breakdownPage]
        updateSelectInput(session, "subgroupPage", choices = subgroups, selected = subgroups[1])
        # also turn off the bar chart subgroup type filter as it is already selected above, but set it to the same as breakdownPage
        shinyjs::hide("breakdownBar_wrapper")
        updateSelectInput(session, "barBreakdown", selected = input$breakdownPage)
      }
    },
    ignoreInit = TRUE
  )

  # Updates that occur when the main subgroup filter changes
  observeEvent(input$subgroupPage, {
    # Update metric to subgroup
    if (input$subgroupPage != "") {
      currentMetric(paste0(input$splashMetric, input$breakdownPage, input$subgroupPage))
      currentMetricSpaces(paste0(input$breakdownPage, ": ", input$subgroupPage))
    }
  })

  # Updates that occur when the bar chart breakdown changes
  observeEvent(input$barBreakdown,
    {
      # if occupation is chosen then show the major group filter above the bar chart
      if (input$barBreakdown == "Occupation (SOC2020 Sub-Major Group)") {
        updateSelectInput(session, "barProfession", choices = "", selected = "")
        shinyjs::show("professionBar_wrapper")
        updateSelectInput(session, "barProfession", choices = c("All", unique(distinctSubgroups$subgroup[distinctSubgroups$breakdown == "Occupation (SOC2020 Major Group)"])))
      } else if (input$barBreakdown != "") {
        shinyjs::hide("professionBar_wrapper")
        # show only show the top ten
        shinyjs::show("subgroupBar_wrapper")
        subgroups <- distinctSubgroups$subgroup[distinctSubgroups$metric == input$splashMetric & distinctSubgroups$breakdown == input$barBreakdown]
        subgroupsBar <- (as.vector(
          C_topTenEachBreakdown %>%
            filter(
              metric == input$splashMetric,
              breakdown == input$barBreakdown,
              geogConcat == input$geoChoice,
              `Occupation (SOC2020 Major Group)` == "All"
            )
        ))$subgroup
        updatePickerInput(session = session, inputId = "barSubgroup", choices = subgroups, selected = subgroupsBar)
      }
    },
    ignoreInit = TRUE
  )

  # Updates that occur when the bar chart submajor group filter changes
  observeEvent(input$barProfession,
    {
      if (input$barProfession != "") {
        # Show the subgroups for that major profession
        shinyjs::show("subgroupBar_wrapper")
        subgroups <- distinctSubgroups$subgroup[distinctSubgroups$metric == input$splashMetric & distinctSubgroups$breakdown == input$barBreakdown]
        subgroupsBar <- (as.vector(
          C_topTenEachBreakdown %>%
            filter(
              metric == input$splashMetric,
              breakdown == input$barBreakdown,
              geogConcat == input$geoChoice,
              `Occupation (SOC2020 Major Group)` == input$barProfession
            )
        ))$subgroup
        updatePickerInput(session = session, inputId = "barSubgroup", choices = subgroups, selected = subgroupsBar)
      }
    },
    ignoreInit = TRUE
  )

  # update the comparison filter when an LA is clicked
  observeEvent(input$mapLA_shape_click, {
    updateSelectizeInput(session, "geoComps",
      selected = c(input$geoComps, paste0(C_Geog$areaName[C_Geog$areaCode == input$mapLA_shape_click$id], " LADU")), options = list()
    )
  })

  ## Update area select when map clicked
  observeEvent(input$map_shape_click, {
    updateSelectizeInput(session, "geoChoice",
      selected = C_Geog$geogConcat[C_Geog$areaCode == input$map_shape_click$id]
    )
  })

  # update the comparison filter to exclude the current choice
  observeEvent(input$geoChoice, {
    updateSelectizeInput(session, "geoComps",
      choices = lapply(areaChoices, function(x) x[x != input$geoChoice])
    )
  })

  ## 5.5 National map ----

  # If map changes to a different geography, then change area to first in that list
  observeEvent(input$splashGeoType, {
    if (tail(strsplit(input$geoChoice, split = " ")[[1]], 1) != input$splashGeoType) {
      updateSelectizeInput(session, "geoChoice",
        selected = C_Geog$geogConcat[C_Geog$geog == input$splashGeoType][1]
      )
    }
  })
  # If area in a different geography, update the map
  observeEvent(input$geoChoice, {
    if (tail(strsplit(input$geoChoice, split = " ")[[1]], 1) != input$splashGeoType) {
      updateRadioGroupButtons(session, "splashGeoType",
        selected = tail(strsplit(input$geoChoice, split = " ")[[1]], 1)
      )
    }
  })
  ### 5.5.1 Title ----
  output$titleMap <- renderUI({
    if (input$geoChoice == "England") {
      paste0("Latest England data")
    } else {
      paste0("Where does ", input$geoChoice, " fit in the national picture?")
    }
  })

  ### 5.5.2 Map data----
  currentMapData <- reactive({
    C_Geog %>%
      filter(geog == input$splashGeoType | geog == "England") %>%
      select(areaName, areaCode, geogConcat, geog,
        value = currentMetric()
      )
  })

  ### 5.5.3 Comment ----
  output$commentMap <- renderUI({
    # Ensure currentMapData has been updated to the current area
    req(input$geoChoice %in% currentMapData()$geogConcat)
    # Ensure there is data in the latest year (sometimes not the case in small subgroups)
    req(is.na((currentMapData() %>% filter(geogConcat == input$geoChoice))$value) == FALSE)
    # No need for sentence for England
    req(input$geoChoice != "England")
    compareNational <-
      if ((currentMapData() %>% filter(geogConcat == input$geoChoice))$value
      >
        (currentMapData() %>% filter(geogConcat == "England"))$value) {
        "higher"
      } else {
        "lower"
      }
    areaRank <- (currentMapData() %>% filter(geog == input$splashGeoType) %>%
      mutate(ranking = rank(desc(value), ties.method = c("first"))) %>%
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
      if (str_sub(input$geoChoice, start = -2) == "CA") {
        "16 CAs (and GLA)."
      } else {
        "42 areas (38 LSIPS and 4 sub-London LSIP areas)."
      }
    )
  })

  ### 5.5.4 Map ----
  output$map <- renderLeaflet({
    mapData <- currentMapData() %>% filter(geog == input$splashGeoType)
    pal <- colorNumeric("Blues", mapData$value)
    labels <-
      # if a percentage then format as %, else big number
      if (str_sub(input$splashMetric, start = -4) == "Rate" | input$splashMetric == "employmentProjection") {
        sprintf(
          "<strong>%s</strong><br/>%s<br/>%s: %s%%",
          mapData$areaName,
          if (currentMetric() %in% unname(unlist(lapply(metricChoices, unlist)))) {
            "Total"
          } else {
            currentMetricSpaces()
          },
          (I_DataText %>% filter(metric == input$splashMetric))$mapPop,
          round(mapData$value * 100)
        ) %>% lapply(htmltools::HTML)
      } else {
        sprintf(
          "<strong>%s</strong><br/>%s<br/>%s: %s",
          mapData$areaName,
          if (currentMetric() %in% unname(unlist(lapply(metricChoices, unlist)))) {
            "Total"
          } else {
            currentMetricSpaces()
          },
          (I_DataText %>% filter(metric == input$splashMetric))$mapPop,
          format(round(mapData$value), big.mark = ",")
        ) %>% lapply(htmltools::HTML)
      }
    mapDataPopup <- currentMapData() %>%
      filter(geogConcat == input$geoChoice)

    popupLabel <-
      # if a percentage then format as %, else big number
      if (str_sub(input$splashMetric, start = -4) == "Rate" | input$splashMetric == "employmentProjection") {
        sprintf(
          "<strong>%s</strong><br/>%s<br/>%s: %s%%",
          mapDataPopup$areaName,
          if (currentMetric() %in% unname(unlist(lapply(metricChoices, unlist)))) {
            "Total"
          } else {
            currentMetricSpaces()
          },
          (I_DataText %>% filter(metric == input$splashMetric))$mapPop,
          round(mapDataPopup$value * 100)
        ) %>% lapply(htmltools::HTML)
      } else {
        sprintf(
          "<strong>%s</strong><br/>%s<br/>%s: %s",
          mapDataPopup$areaName,
          if (currentMetric() %in% unname(unlist(lapply(metricChoices, unlist)))) {
            "Total"
          } else {
            currentMetricSpaces()
          },
          (I_DataText %>% filter(metric == input$splashMetric))$mapPop,
          format(round(mapDataPopup$value), big.mark = ",")
        ) %>% lapply(htmltools::HTML)
      }

    # Create map
    leaflet(options = leafletOptions(zoomSnap = 0.1)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(
        lng = -1.6,
        lat = 52.8,
        zoom = 5.7
      ) %>%
      addPolygons(
        data = mapData,
        fillColor = ~ pal(mapData$value),
        fillOpacity = 1,
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
      ) %>%
      addPopups(
        # proxy,
        lng = C_Geog$LONG[C_Geog$geogConcat == input$geoChoice],
        lat = C_Geog$LAT[C_Geog$geogConcat == input$geoChoice],
        popup = popupLabel,
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

  ### 5.4.5 Map footnote ----
  output$mapFoot <- renderUI({
    paste0(
      (I_DataText %>% filter(metric == input$splashMetric))$LatestPeriod, ". Click an area to update dashboard."
    )
  })

  ## 5.6 LA map ----
  ### 5.6.1 Title ----
  output$titleLaMap <- renderUI({
    paste0("What is the variation within ", input$geoChoice, "?")
  })
  ### 5.6.2 Comment----
  output$commentLA <- renderUI({
    LaHighLow <- # Filter to those LAs in that region
      C_Geog %>%
      filter(
        geog == "LADU",
        eval(parse(text = tail(strsplit(input$geoChoice, split = " ")[[1]], 1))) == input$geoChoice
      ) %>%
      select(areaName, areaCode,
        value = currentMetric()
      ) %>%
      filter(is.na(value) == FALSE) %>% # remove NAs from ranking
      mutate(ranking = rank(desc(value), ties.method = c("first")))

    if (nrow(LaHighLow) > 1) { # if more than 1 LA
      LaHigh <- (LaHighLow %>% filter(ranking == 1))$areaName
      LaLow <- (LaHighLow %>% filter(ranking == max(ranking)))$areaName
      paste0(
        (I_DataText %>% filter(metric == input$splashMetric))$LaComment,
        " highest in ",
        LaHigh,
        " and lowest in ",
        LaLow,
        "."
      )
    } else {
      ""
    }
  })

  ### 5.6.3 Map----
  # Update LA map when interaction
  output$mapLA <- renderLeaflet({
    # If there is no data in any of the LAs (as is the case for some of the subgroups and for London in online job ads) then do not plot
    shiny::validate(need(
      all(is.na((C_Geog %>%
        filter(
          geog == "LADU",
          eval(parse(text = tail(strsplit(input$geoChoice, split = " ")[[1]], 1))) == input$geoChoice
        ) %>%
        select(areaName, areaCode, geometry,
          value = currentMetric()
        ))$value)) == FALSE, "No data is available at LA level."
    ))

    # Filter to those LAs in that region
    mapData <- C_Geog %>%
      filter(
        geog == "LADU",
        eval(parse(text = tail(strsplit(input$geoChoice, split = " ")[[1]], 1))) == input$geoChoice
      ) %>%
      select(areaName, areaCode, geometry,
        value = currentMetric()
      )

    pal <- colorNumeric("Blues", mapData$value)

    labels <-
      # if a percentage then format as %, else big number
      if (str_sub(input$splashMetric, start = -4) == "Rate" | input$splashMetric == "employmentProjection") {
        sprintf(
          "<strong>%s</strong><br/>%s<br/>%s: %s%%",
          mapData$areaName,
          if (currentMetric() %in% unname(unlist(lapply(metricChoices, unlist)))) {
            "Total"
          } else {
            currentMetricSpaces()
          },
          (I_DataText %>% filter(metric == input$splashMetric))$mapPop,
          round(mapData$value * 100)
        ) %>% lapply(htmltools::HTML)
      } else {
        sprintf(
          "<strong>%s</strong><br/>%s<br/>%s: %s",
          mapData$areaName,
          if (currentMetric() %in% unname(unlist(lapply(metricChoices, unlist)))) {
            "Total"
          } else {
            currentMetricSpaces()
          },
          (I_DataText %>% filter(metric == input$splashMetric))$mapPop,
          format(round(mapData$value), big.mark = ",")
        ) %>% lapply(htmltools::HTML)
      }
    # Find bounds
    bbox <- sf::st_bbox(mapData$geometry)
    bbox_list <- as.list(bbox)

    leaflet(options = leafletOptions(zoomSnap = 0.1)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data = mapData,
        fillColor = ~ pal(mapData$value),
        fillOpacity = 1,
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
      fitBounds(
        lng1 = bbox_list$xmin,
        lat1 = bbox_list$ymin,
        lng2 = bbox_list$xmax,
        lat2 = bbox_list$ymax
      )
    # }
  })

  ### 5.6.4 Map footnote ----
  output$mapLaFoot <- renderUI({
    req(noData() == "")
    req(input$splashMetric != "employmentProjection") # No LA level for skills imp
    paste0(
      (I_DataText %>% filter(metric == input$splashMetric))$LatestPeriod, ". Click an area to update other charts with LA data."
    )
  })

  ## 5.7 Time chart ----
  # create time header
  output$titleTime <- renderUI({
    paste0("How ", (I_DataText %>% filter(metric == input$splashMetric))$timeTitle, " over time?")
  })

  ### 5.7.1 Comment ----
  output$commentTime <- renderUI({
    # No need for sentence for England
    req(input$geoChoice != "England")
    # ensure latest data is available for comparison
    req(is.na((C_time %>%
      filter(
        geogConcat == input$geoChoice,
        metric == currentMetric(),
        latest == 1
      ))$value) == FALSE)
    # ensure there are at least two data points for comparison
    req(nrow(C_time %>%
      filter(
        geogConcat == input$geoChoice,
        metric == currentMetric(),
        is.na(value) == FALSE # remove any rows with no data
      )) >= 2)

    currentArea <- C_time %>%
      filter(
        geogConcat == input$geoChoice,
        metric == currentMetric(),
        is.na(value) == FALSE # remove any rows with no data
      )
    englandArea <- C_time %>%
      filter(
        geogConcat == "England",
        metric == currentMetric()
      )
    currentChange <- (currentArea %>%
      filter(latest == 1))$value -
      (currentArea %>%
        filter(timePeriod == min(timePeriod)))$value
    englandChange <- (englandArea %>%
      filter(latest == 1))$value -
      (englandArea %>%
        filter(timePeriod == min(currentArea$timePeriod)))$value # match with the area data
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
      if (input$splashMetric == "employmentProjection") {
        " to 2035."
      } else {
        paste0(
          " in the last ",
          if (input$splashMetric == "vacancies") {
            round(((nrow(currentArea) - 1) / 12))
          } else {
            nrow(currentArea) - 1
          }, # count number of years of change
          " years."
        )
      }
    )
  })

  ### 5.7.2 Chart ----
  output$Splash_time <- renderPlotly({
    validate(need(noData() == "", noData()))

    df <- C_time %>%
      filter(
        # get lsip/ca areas
        (geogConcat == input$geoChoice | geogConcat %in% if ("geoComps" %in% names(input)) {
          input$geoComps
        } else {
          "\nNone"
        }) |
          # get england for comparison (if a rate and not already chosen)
          (if ((str_sub(input$splashMetric, start = -4) == "Rate" | str_sub(input$splashMetric, start = -10) == "population" | input$splashMetric == "employmentProjection") & input$geoChoice != "England" & !("England" %in% input$geoComps)) {
            (geogConcat == "England")
          } else {
            geogConcat == "\nNone"
          })
      )

    SplashTime <- df %>% filter(metric == currentMetric())
    # add an extra column so the colours work in ggplot when sorting alphabetically
    SplashTime$Areas <- factor(
      SplashTime$geogConcat,
      if (input$geoChoice == "England") {
        levels <- c(input$geoChoice, input$geoComps)
      } else {
        if ("England" %in% input$geoComps) {
          levels <- c("England", input$geoChoice, input$geoComps[!grepl("England", input$geoComps)])
        } else {
          levels <- c("England", input$geoChoice, input$geoComps)
        }
      }
    )

    timePlot <- ggplot(
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
          if (currentMetric() %in% unname(unlist(lapply(metricChoices, unlist)))) {
            "Total"
          } else {
            currentMetricSpaces()
          }, " ",
          ifelse(input$splashMetric == "employmentProjection", "Projected annual employment growth", (I_DataText %>% filter(metric == input$splashMetric))$mapPop),
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
      geom_point() +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()
      ) +
      scale_y_continuous(
        # Style labels. If a rate style as %, If bigger than 1m style as x.xxM, else use cut_short_scale to append a k for bigger than 1000
        labels = if (str_sub(input$splashMetric, start = -4) == "Rate" | input$splashMetric == "employmentProjection") {
          scales::percent
        } else if ((max(SplashTime$value, na.rm = TRUE) >= 1000000 & (max(SplashTime$value, na.rm = TRUE) - min(SplashTime$value, na.rm = TRUE)) < 600000) | (max(SplashTime$value, na.rm = TRUE) >= 1000 & (max(SplashTime$value, na.rm = TRUE) - min(SplashTime$value, na.rm = TRUE)) < 600)) {
          label_number(accuracy = 0.01, scale_cut = append(scales::cut_short_scale(), 1, 1))
        } else if ((max(SplashTime$value, na.rm = TRUE) >= 1000000 & (max(SplashTime$value, na.rm = TRUE) - min(SplashTime$value, na.rm = TRUE)) < 6000000) | (max(SplashTime$value, na.rm = TRUE) >= 1000 & (max(SplashTime$value, na.rm = TRUE) - min(SplashTime$value, na.rm = TRUE)) < 6000)) {
          label_number(accuracy = 0.1, scale_cut = append(scales::cut_short_scale(), 1, 1))
        } else {
          label_number(accuracy = 1, scale_cut = append(scales::cut_short_scale(), 1, 1))
        }
      ) +
      labs(colour = "") +
      scale_color_manual(values = if (str_sub(input$splashMetric, start = -4) == "Rate" | str_sub(input$splashMetric, start = -10) == "population" | input$splashMetric == "employmentProjection" | input$geoChoice == "England" | "England" %in% input$geoComps) {
        chartColors6
      } else {
        chartColors5
      }) +
      scale_x_date(
        name = "My date axis title",
        date_breaks = "1 years",
        date_labels = "%Y"
      )
    ggplotly(timePlot, tooltip = "text") %>%
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

  ### 5.7.3 Time footnote ----
  output$timeFoot <- renderUI({
    req(noData() == "")
    if (input$splashMetric == "sustainedPositiveDestinationKS5Rate") {
      "The definition of when a student is at the end of 16 to 18 study has changed last year and comparisons to previous cohorts should be treated with caution. See footnote below. Also NB non-zero axis."
    } else {
      if (input$splashMetric %in% c("L3PlusRate", "L4PlusRate")) {
        "Figures from 2022 onwards are not directly comparable to previous years due to survey changes. Also NB non-zero axis."
      } else {
        "NB non-zero axis."
      }
    }
  })

  ## 5.8 Breakdown chart ----
  ### 5.8.1 Title ----
  output$titleBreakdown <- renderUI({
    if (input$splashMetric %in% distinctBreakdowns$metric) {
      paste0(
        "How do ",
        (I_DataText %>% filter(metric == input$splashMetric))$breakdownTitle,
        " vary by ",
        tolower(gsub("SOC2020 ", "", ifelse(input$breakdownPage == "All", input$barBreakdown, input$breakdownPage))),
        "?"
      )
    }
  })

  ### 5.8.2 Comment ----
  output$commentBreakdown <- renderUI({
    validate(need(noData() == "", noData()))
    if (!input$splashMetric %in% distinctBreakdowns$metric) {
      paste0(
        str_to_sentence(currentMetricClean()),
        " currently has no breakdowns.",
        if (input$splashMetric %in% c(
          "inemploymentRate",
          "selfemployedRate",
          "unemployedRate",
          "inactiveRate",
          "selfemployed",
          "unemployed",
          "inactive"
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
        arrange(desc(geogConcat == "England")) %>% # force england to the top
        group_by(subgroup) %>%
        mutate(change = (value - lag(value, default = 1)) / value) %>%
        ungroup() %>%
        filter(geogConcat == input$geoChoice) %>%
        mutate(ranking = rank(desc(abs(change)), ties.method = c("first"))) %>%
        filter(ranking == 1)

      breakdownDirection <-
        if (exists("breakdownDiff") == TRUE && length(breakdownDiff$change) > 0 && breakdownDiff$change > 0) {
          "higher"
        } else {
          "lower"
        }
      paste0(
        if (input$geoChoice != "England") {
          paste0(
            input$geoChoice,
            " has a ",
            breakdownDirection,
            " ",
            (I_DataText %>% filter(metric == input$splashMetric))$breakdownComment,
            " in ",
            breakdownDiff$subgroup,
            " than the national average. "
          )
        },
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

  ### 5.8.3 Bar chart ----
  Splash_pc <- reactive({
    req(noData() == "")
    Splash_21 <- C_breakdown %>% filter(
      breakdown == input$barBreakdown,
      subgroup %in% input$barSubgroup,
      metric == input$splashMetric,
      # get lsip/ca areas
      (geogConcat == input$geoChoice | geogConcat %in% if ("geoComps" %in% names(input)) {
        input$geoComps
      } else {
        "\nNone"
      }) |
        # get england for comparison
        if (!("England" %in% input$geoComps)) {
          (geogConcat == "England")
        } else {
          geogConcat == "\nNone"
        }
    )
    # bold the category chosen
    # if using a subgroup
    if (!currentMetric() %in% unname(unlist(lapply(metricChoices, unlist)))) {
      Splash_21 <- Splash_21 %>% mutate(subgroup = case_when(
        subgroup == input$subgroupPage ~ paste0("<b>", subgroup, "</b>"),
        TRUE ~ subgroup
      ))
    } else {
      "x"
    }
    # get rid of soc codes
    Splash_21 <- Splash_21 %>% mutate(subgroup = gsub("[0-9]+ - ", "", subgroup))

    # if no rows (because of filter lag) then don't plot
    if (nrow(Splash_21) == 0) {
      "x"
    } else {
      # add an extra column so the colours work in ggplot when sorting alphabetically
      Splash_21$Area <- factor(
        Splash_21$geogConcat,
        if (input$geoChoice == "England" | "England" %in% input$geoComps) {
          levels <- c(input$geoChoice, input$geoComps)
        } else {
          levels <- c("England", input$geoChoice, input$geoComps)
        }
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
            currentMetricClean(),
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
          label_number(accuracy = 1, scale_cut = append(scales::cut_short_scale(), 1, 1))
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
  })

  output$Splash_pc <- renderPlotly({
    # check it exists
    req(Splash_pc() != "x")
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
    withSpinner(plotlyOutput("Splash_pc"))
    # }
  })

  ### 5.8.4 Bar footnote ----
  output$breakdownFoot <- renderUI({
    req(noData() == "")
    if (input$splashMetric %in% distinctBreakdowns$metric) {
      paste0(
        (I_DataText %>% filter(metric == input$splashMetric))$LatestPeriod, ".",
        if (input$splashMetric %in% c("achievements", "participation")) {
          if (input$barBreakdown == "Provision") {
            " Splits based on learner achievement volumes. The apprenticeship and community learning volumes include all age apprentices and community learners (including under 19) and so the denominator of the provision split is the sum of all age apprentices, all age community learners and 19+ education and training learners."
          } else {
            if (input$barBreakdown == "SSA") {
              " SSA splits are based on Education and Training achievement aims (not all FE learners as in other splits)."
            } else {
              if (input$barBreakdown == "Level") {
                " Splits are based on learner volumes. Learners can appear in multiple categories if they take multiple courses and as such percentages may add up to more than 100%.  Full level 2 and Full level 3 are shown for interest but are a subset of Level 2 and Level 3."
              }
            }
          }
        }
      )
    }
  })

  ## 5.9 Downloads local skills ----
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
    paste0(currentMetricClean(), "-all areas.xlsx")
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
    paste0(currentMetricClean(), "-", input$geoChoice, ".xlsx")
  })
  output$downloadV1Current <- downloadHandler(
    filename = function() {
      nameDownloadV1Current()
    },
    content = function(file) {
      write_xlsx(listDownloadV1Current(), path = file)
    }
  )

  # 6 DataHub----
  ## 6.1 Filters----
  output$hubAreaInput <- renderUI({
    selectizeInput(
      "hubArea",
      multiple = TRUE,
      label = NULL,
      options = list(placeholder = "Choose LSIPs, CAs, LAs*"),
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
      choices = (C_datahub %>% filter(
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
      ) %>% distinct(Breakdown))$Breakdown,
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

  ## 6.2 Table----
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
                C_Geog %>% filter(geog == "LADU", (LSIP %in% input$hubArea | CA %in% input$hubArea))
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

  # 7 Resource tables----
  ## 7.1 FE tools table----
  output$toolsTable <- DT::renderDataTable({
    DT::datatable(
      I_ToolsTable,
      escape = FALSE,
      options = list(dom = "t", pageLength = 50),
      rownames = FALSE
    )
  })

  ## 7.2 FE sources table----
  output$sourcesTable <- DT::renderDataTable({
    DT::datatable(
      I_SourcesTable,
      escape = FALSE,
      options = list(dom = "t", pageLength = 50),
      rownames = FALSE
    )
  })

  ## 7.3 FE reports table----
  output$reportsTable <- DT::renderDataTable({
    DT::datatable(
      I_ReportsTable,
      escape = FALSE,
      options = list(dom = "t", pageLength = 50),
      rownames = FALSE
    )
  })

  # 8 Footer----
  # Create link to accessibility tab
  observeEvent(input$accessibility_footer_link, {
    bslib::nav_select("navbar", "accessibility")
  })

  # Create link to support and feedback tab
  observeEvent(input$cookies_footer_link, {
    bslib::nav_select("navbar", "cookie_information")
  })
  # Create link to support and feedback tab
  observeEvent(input$support_footer_link, {
    bslib::nav_select("navbar", "support_and_feedback")
  })
}
