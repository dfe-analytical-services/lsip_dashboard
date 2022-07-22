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
# application  by clicking 'Run App' above.
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

  # Define page titles ----
  output$page0title <- renderUI({
    paste0(input$lep0a, ": overview of local landscape")
  })

  output$page1title <- renderUI({
    paste0(input$lep1, " employment trends")
  })

  output$page2title <- renderUI({
    paste0(input$lep3, " FE skill supply trends")
  })

  output$page3title <- renderUI({
    paste0(input$lep5, " vacancy trends")
  })

  output$page4title <- renderUI({
    paste0(input$lep7, ": Overview of earnings")
  })

  output$page5title <- renderUI({
    paste0(input$lep9, ": Overview of HE")
  })

  output$page6title <- renderUI({
    paste0(input$lepOver2, ": overview of local landscape")
  })

  output$page7title <- renderUI({
    paste0(input$lepAppa, ": overview of apprenticeships")
  })

  ### Conditional functions----
  # Returns arrow-up icon on true (if true_direction is 'up')
  cond_icon <- function(condition, true_direction = "up") {
    if (true_direction == "up") {
      return(icon(ifelse(condition, "arrow-up", "arrow-down"), "fa-2x fa-beat"))
    }
    return(icon(ifelse(condition, "arrow-down", "arrow-up"), "fa-2x fa-beat"))
  }

  # Conditional color for widget
  # Returns 'green' on true, 'red' on false, e.g. api usage % change > 0
  #                                               load time % change < 0
  cond_color <- function(condition, true_color = "green") {
    if (is.na(condition)) {
      return("black")
    }
    colours <- c("green", "red")
    return(ifelse(condition, true_color, colours[!colours == true_color]))
  }

  # HOMEPAGE ----

  # Add link to overview
  observeEvent(input$link_to_tabpanel_overview, {
    updateTabsetPanel(session, "navbar", "Overview")
  })
  # Add link to employment data
  observeEvent(input$link_to_tabpanel_employment, {
    updateTabsetPanel(session, "navbar", "Employment")
  })
  # Add link to vacancy data
  observeEvent(input$link_to_tabpanel_vacancies, {
    updateTabsetPanel(session, "navbar", "Vacancies")
  })
  # Add link to salary data
  observeEvent(input$link_to_tabpanel_earnings, {
    updateTabsetPanel(session, "navbar", "Earnings")
  })
  # Add link to skills data
  observeEvent(input$link_to_tabpanel_FE, {
    updateTabsetPanel(session, "navbar", "FE")
  })
  # Add link to HE data
  observeEvent(input$link_to_tabpanel_HE, {
    updateTabsetPanel(session, "navbar", "HE")
  })

  # OVERVIEW ----

  ### Downloads----
  # download all indicators
  list_of_datasets0 <- list(
    "2.Emp by occupation" = C_EmpOcc_APS1721,
    "5.Emp rate" = C_EmpRate_APS1721,
    "12a.FE achievements SSA" = C_Achieve_ILR21,
    "12b.FE achievements" = C_Achieve_ILR1621,
    "22.Vacancies" = C_Vacancy_ONS1722
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
      "2.Emp by occupation" = filter(C_EmpOcc_APS1721, geographic_level == "lep", area == input$lep0a),
      "5.Emp rate" = filter(C_EmpRate_APS1721, geographic_level == "lep", area == input$lep0a),
      "12a.FE achievements SSA" = filter(C_Achieve_ILR21, LEP == input$lep0a),
      "12b.FE achievements" = filter(C_Achieve_ILR1621, LEP == input$lep0a),
      "22.Vacancies" = filter(C_Vacancy_ONS1722, LEP == input$lep0a)
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

  ## KPIs ----

  ### Employment count ----
  output$locland.emplcnt0 <- renderValueBox({
    valueBox(
      format((C_EmpRate_APS1721 %>%
        filter(
          geographic_level == "lep", # cleans up for London which is included as lep and gor
          area == input$lep0a,
          year == "2021"
        )
      )$"28  in employment ",
      scientific = FALSE, big.mark = ","
      ),
      "people employed in 2021",
      width = 12
    )
  })

  ### Employment change ----
  output$locland.emplcntchange0 <- renderValueBox({
    # get value
    x <- ((C_EmpRate_APS1721 %>%
      filter(
        geographic_level == "lep", # cleans up for London which is included as lep and gor
        area == input$lep0a,
        year == "2021"
      )
    )$"28  in employment "
      - (C_EmpRate_APS1721 %>%
        filter(
          geographic_level == "lep", # cleans up for London which is included as lep and gor
          area == input$lep0a,
          year == "2020"
        )
      )$"28  in employment ")
    # build box
    valueBox(
      format_pm(x),
      subtitle = NULL,
      width = 12,
      icon = cond_icon(x > 0),
      color = cond_color(x > 0)
    )
  })

  ### Employment rate -----
  output$locland.emplrate0 <- renderValueBox({
    valueBox(
      paste0(
        format(100. * (C_EmpRate_APS1721 %>%
          filter(
            geographic_level == "lep", # cleans up for London which is included as lep and gor
            area == input$lep0a,
            year == "2021"
          )
        )$empRate, digits = 2),
        "%"
      ),
      paste0(
        "employment rate in 2021 (compared with ",
        format(100. * (C_EmpRate_APS1721 %>%
          filter(
            geographic_level == "country", # cleans up for London which is included as lep and gor
            year == "2021"
          )
        )$empRate, digits = 2),
        "% for England)"
      ),
      width = 12
    )
  })

  ### Employment rate change -----
  output$locland.emplchange0 <- renderValueBox({
    x <- (100. * ((C_EmpRate_APS1721 %>%
      filter(
        geographic_level == "lep", # cleans up for London which is included as lep and gor
        area == input$lep0a,
        year == "2021"
      ))$empRate -
      (C_EmpRate_APS1721 %>%
        filter(
          geographic_level == "lep", # cleans up for London which is included as lep and gor
          area == input$lep0a,
          year == "2020"
        ))$empRate))
    valueBox(
      paste(
        sprintf("%+.0f", x),
        "ppts"
      ),
      subtitle = NULL,
      width = 12,
      icon = cond_icon(x > 0),
      color = cond_color(x > 0)
    )
  })

  # Add button to link to employment data
  observeEvent(input$link_to_tabpanel_employment2, {
    updateTabsetPanel(session, "navbar", "Employment")
  })

  ### ONS job advert units  ----
  output$jobad.units <- renderValueBox({
    # valueBox(C_Vacancy_England %>%
    #   filter(
    #     year == "2022",
    #     LEP == input$lep0a
    #   ) %>%
    #   summarise(job.unit = sum(vacancy_unit)),
    # "job vacancy units (Jan 22)",
    # width = 12
    # )
    valueBox(
      paste0(
        format(100. * (C_Vacancy_England %>%
          filter(
            year == "2022",
            LEP == input$lep0a
          ) %>%
          summarise(job.pc = sum(pc_total))), digits = 2),
        "%"
      ),
      paste0("of online vacancies in England (Jan '22)"),
      width = 12
    )
  })

  ### ONS job advert units change  ----
  output$jobad.change <- renderValueBox({
    x <- (C_Vacancy_England %>%
      filter(
        year == "2022",
        LEP == input$lep0a
      ) %>%
      summarise(job.pc = sum(pc_total))
      - (C_Vacancy_England %>%
        filter(
          year == "2021",
          LEP == input$lep0a
        ) %>%
        summarise(job.pc = sum(pc_total))))
    valueBox(
      paste(
        sprintf("%+.2f", x),
        "ppts"
      ),
      subtitle = NULL,
      width = 12,
      icon = cond_icon(x > 0),
      color = cond_color(x > 0)
    )
  })

  # Add button to link to vacancy data
  observeEvent(input$link_to_tabpanel_vacancies2, {
    updateTabsetPanel(session, "navbar", "Vacancies")
  })

  # Add button to link to skills data
  observeEvent(input$link_to_tabpanel_FE2, {
    updateTabsetPanel(session, "navbar", "FE")
  })


  ### E&T achievements -----
  output$skisup.ETach <- renderValueBox({
    valueBox(
      format((C_Achieve_ILR1621 %>%
        filter(
          time_period == "202021",
          LEP == input$lep0a,
          level_or_type == "Education and training: Total"
        ) %>%
        summarise(App_ach = sum(achievements))), scientific = FALSE, big.mark = ","),
      "adult education and training achievements (AY20/21)",
      width = 12
    )
  })

  ### E&T achievements change -----
  output$skisup.ETachChange <- renderValueBox({
    x <- ((C_Achieve_ILR1621 %>%
      filter(
        time_period == "202021",
        LEP == input$lep0a,
        level_or_type == "Education and training: Total"
      ) %>%
      summarise(App_ach = sum(achievements)))
    - (C_Achieve_ILR1621 %>%
        filter(
          time_period == "201920",
          LEP == input$lep0a,
          level_or_type == "Education and training: Total"
        ) %>%
        summarise(App_ach = sum(achievements))))
    valueBox(
      format_pm(x),
      subtitle = NULL,
      width = 12,
      icon = cond_icon(x > 0),
      color = cond_color(x > 0)
    )
  })

  ### App achievements ----
  output$skisup.APPach <- renderValueBox({
    valueBox(
      format((C_Achieve_ILR1621 %>%
        filter(
          time_period == "202021",
          LEP == input$lep0a,
          level_or_type == "Apprenticeships: Total"
        ) %>%
        dplyr::summarise(App_ach = sum(achievements))), scientific = FALSE, big.mark = ","),
      "apprenticeship achievements (AY20/21)",
      width = 12
    )
  })

  ### App achievements change ----
  output$skisup.APPachChange <- renderValueBox({
    x <- ((C_Achieve_ILR1621 %>%
      filter(
        time_period == "202021",
        LEP == input$lep0a,
        level_or_type == "Apprenticeships: Total"
      ) %>%
      summarise(App_ach = sum(achievements)))
    - (C_Achieve_ILR1621 %>%
        filter(
          time_period == "201920",
          LEP == input$lep0a,
          level_or_type == "Apprenticeships: Total"
        ) %>%
        summarise(App_ach = sum(achievements))))
    valueBox(
      format_pm(x),
      subtitle = NULL,
      width = 12,
      icon = cond_icon(x > 0),
      color = cond_color(x > 0)
    )
  })

  # EMPLOYMENT ----
  ### Downloads----
  # download employment indicators
  list_of_datasets1 <- list(
    "2.Emp by occupation" = C_EmpOcc_APS1721,
    "5.Emp rate" = C_EmpRate_APS1721
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
      "2.Emp by occupation" = filter(C_EmpOcc_APS1721, geographic_level == "lep", area == input$lep1),
      "5.Emp rate" = filter(C_EmpRate_APS1721, geographic_level == "lep", area == input$lep1)
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
        format(100. * (C_EmpRate_APS1721 %>%
          filter(
            geographic_level == "lep", # cleans up for London which is included as lep and gor
            area == input$lep1,
            year == "2021"
          )
        )$empRate, digits = 2),
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
            geographic_level == "lep", # cleans up for London which is included as lep and gor
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
      format((C_EmpRate_APS1721 %>%
        filter(
          geographic_level == "lep", # cleans up for London which is included as lep and gor
          area == input$lep1,
          year == "2021"
        )
      )$"28  in employment ",
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
          geographic_level == "lep", # cleans up for London which is included as lep and gor
          area == input$lep2,
          year == "2021"
        )
      )$"28  in employment ",
      scientific = FALSE, big.mark = ","
      ),
      paste0("in employment in 2021 in ", input$lep2),
      color = "orange"
    )
  })
  # turn off comparison boxes if none is selected
  output$emp_comp <- renderUI({
    print(input$lep2)
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
  })

  ## Employment rate over time line graph ----
  EmpRate_time <- reactive({
    EmpRateTime <- C_EmpRate_APS1721 %>%
      select(year, area, geographic_level, empRate) %>%
      filter(
        geographic_level == "lep" |
          geographic_level == "country", # cleans up for London and South East which is included as lep and gor
        area == "England" |
          area == input$lep1 |
          area == input$lep2
      )
    # add an extra column so the colours work in ggplot when sorting alphabetically
    EmpRateTime$Areas <- factor(EmpRateTime$area,
      levels = c("England", input$lep1, input$lep2)
    )
    ggplot(EmpRateTime, aes(x = year, y = empRate, color = Areas, group = Areas)) +
      geom_line(aes(text = paste0(
        "AY: ", year, "<br>",
        "Area: ", Areas, "<br>",
        "Employment rate: ", scales::percent(round(empRate, 2)), "<br>"
      ))) +
      theme_minimal() +
      expand_limits(y = 0.6) +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "bottom", legend.title = element_blank()) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(colour = "") +
      scale_color_manual(values = c("#28a197", "#1d70b8", "#F46A25"))
  })

  output$EmpRate_time <- renderPlotly({
    ggplotly(EmpRate_time(), tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.1)) %>%
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
          area == input$lep1 | # "South East"|
          area == input$lep2 # "London"
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

  ### Downloads----
  # download skills indicators
  list_of_datasets3 <- list("22.Vacancies" = C_Vacancy_ONS1722)
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
    list("22.Vacancies" = filter(C_Vacancy_ONS1722, LEP == input$lep5))
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
            LEP == input$lep5
          ) %>%
          summarise(job.pc = sum(pc_total))), digits = 3),
        "%"
      ),
      paste0("of online vacancies in England (Jan '22) were in ", input$lep5),
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
            LEP == input$lep6
          ) %>%
          summarise(job.pc = sum(pc_total))), digits = 3),
        "%"
      ),
      paste0("of online vacancies in England (Jan '22) were in ", input$lep6),
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
            LEP == input$lep5
          ) %>%
          group_by(year) %>%
          dplyr::summarise(job.cnt = sum(vacancy_unit)) %>%
          dplyr::mutate(Row = 1:n()) %>%
          mutate(Percentage_Change = job.cnt / lag(job.cnt)) %>%
          ungroup() %>%
          filter(year == "2022") %>%
          select(Percentage_Change)), digits = 3),
        "%"
      ),
      paste0("change in online job vacancies in ", input$lep5, " from Jan '21 to Jan '22"),
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
            LEP == input$lep6
          ) %>%
          group_by(year) %>%
          dplyr::summarise(job.cnt = sum(vacancy_unit)) %>%
          dplyr::mutate(Row = 1:n()) %>%
          mutate(Percentage_Change = job.cnt / lag(job.cnt)) %>%
          ungroup() %>%
          filter(year == "2022") %>%
          select(Percentage_Change)), digits = 3),
        "%"
      ),
      paste0("change in online job vacancies in ", input$lep6, " from Jan '21 to Jan '22"),
      color = "orange"
    )
  })

  # turn off comparison boxes if none is selected
  output$vac_comp <- renderUI({
    print(input$lep6)
    if (input$lep6 == "\nNone") {
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
  })

  ## Online job vacancy units over time line chart ----
  jobad.time <- reactive({
    JobTime <- C_Vacancy_England %>%
      filter(LEP == input$lep5 |
        LEP == input$lep6) %>%
      select(-LA, -pc_total, -region, -England) %>%
      group_by(year, LEP) %>%
      summarise(total = sum(vacancy_unit))
    # add an extra column so the colours work in ggplot when sorting alphabetically
    JobTime$Areas <- factor(JobTime$LEP,
      levels = c(input$lep5, input$lep6)
    )
    ggplot(JobTime, aes(x = year, y = total, colour = Areas, group = Areas)) +
      geom_line(aes(text = paste0(
        "Year: ", year, "<br>",
        "Area: ", Areas, "<br>",
        "Job vacancy units: ", round(total, 0), "<br>"
      ))) +
      theme_minimal() +
      labs(colour = "LEP") +
      theme(legend.position = "bottom", axis.title.x = element_blank(), axis.title.y = element_blank()) +
      labs(shape = "", colour = "") +
      scale_color_manual(values = c("#1d70b8", "#F46A25"))
  })

  output$jobad.time <- renderPlotly({
    ggplotly(jobad.time(), tooltip = c("text")) %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.1)) %>%
      config(displayModeBar = FALSE)
  })


  # FE ----
  ### Downloads----
  # download skills indicators
  list_of_datasets2 <- list(
    "12a.FE achievements SSA" = C_Achieve_ILR21,
    "12b.FE achievements" = C_Achieve_ILR1621
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
      "12a.FE achievements SSA" = filter(C_Achieve_ILR21, LEP == input$lep3),
      "12b.FE achievements" = filter(C_Achieve_ILR1621, LEP == input$lep3)
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
          LEP == input$lep3,
          level_or_type == "Education and training: Total"
        ) %>%
        summarise(App_ach = sum(achievements))), scientific = FALSE, big.mark = ","),
      paste0("20/21 adult education and training achievements in ", input$lep3),
      color = "blue"
    )
  })

  output$skisup.FEach.2 <- renderValueBox({
    # Put value into box to plug into app
    valueBox(
      format((C_Achieve_ILR1621 %>%
        filter(
          time_period == "202021",
          LEP == input$lep4,
          level_or_type == "Education and training: Total"
        ) %>%
        summarise(App_ach = sum(achievements))), scientific = FALSE, big.mark = ","),
      paste0("20/21 adult education and training achievements in ", input$lep4),
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
          LEP == input$lep3,
          level_or_type == "Apprenticeships: Total"
        ) %>%
        summarise(App_ach = sum(achievements))), scientific = FALSE, big.mark = ","),
      paste0("20/21 apprenticeship achievements in ", input$lep3),
      color = "blue"
    )
  })

  output$skisup.APach.2 <- renderValueBox({
    # Put value into box to plug into app
    valueBox(
      format((C_Achieve_ILR1621 %>%
        filter(
          time_period == "202021",
          LEP == input$lep4,
          level_or_type == "Apprenticeships: Total"
        ) %>%
        summarise(App_ach = sum(achievements))), scientific = FALSE, big.mark = ","),
      paste0("20/21 apprenticeship achievements in ", input$lep4),
      color = "orange"
    )
  })

  # turn off comparison boxes if none is selected
  output$skill_comp <- renderUI({
    print(input$lep4)
    if (input$lep4 == "\nNone") {
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
  })

  ## Achievements over time line chart ----
  Ach_time <- reactive({
    FETime <- C_Achieve_ILR1621 %>%
      select(time_period, area, LEP, level_or_type, achievements) %>%
      mutate(level_or_type = case_when(
        level_or_type == "Further education and skills: Total" ~ "Total FE and Apps provision",
        level_or_type == "Education and training: Total" ~ "Education and training (adults only)",
        level_or_type == "Community learning: Total" ~ "Community learning (adults only)",
        level_or_type == "Apprenticeships: Total" ~ "Apprenticeships (all ages)",
        TRUE ~ level_or_type
      )) %>%
      filter(
        # area == "England" |
        LEP == input$lep3 |
          LEP == input$lep4,
        level_or_type == input$skill_line,
        time_period != 202122
      ) %>%
      mutate(AY = paste(substr(time_period, 3, 4), "/", substr(time_period, 5, 6), sep = "")) %>%
      group_by(AY, LEP, level_or_type) %>%
      summarise(Achievements = sum(achievements))
    # add an extra column so the colours work in ggplot when sorting alphabetically
    FETime$Area <- factor(FETime$LEP,
      levels = c(input$lep3, input$lep4)
    )
    ggplot(FETime, aes(
      x = AY, y = Achievements, colour = Area,
      group = interaction(level_or_type, LEP)
    )) +
      geom_line(aes(text = paste0(
        "AY: ", AY, "<br>",
        "Area: ", Area, "<br>",
        "Achievements: ", format(Achievements, big.mark = ","), "<br>",
        "Provision: ", level_or_type, "<br>"
      ))) +
      theme_minimal() +
      theme(legend.position = "bottom", axis.title.x = element_blank(), axis.title.y = element_blank()) +
      labs(shape = "", colour = "") +
      scale_y_continuous(label = comma) +
      xlab("Year") +
      scale_color_manual(values = c("#1d70b8", "#F46A25"))
  })

  output$Ach_time <- renderPlotly({
    ggplotly(Ach_time(), tooltip = c("text")) %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.1)) %>%
      config(displayModeBar = FALSE)
  })

  ## Achievements pc bar chart ----
  Ach_SSA_pc <- reactive({
    AchSSA_21 <- C_Achieve_ILR21 %>%
      filter(
        time_period == "202122",
        LEP == input$lep3 |
          LEP == input$lep4
      ) %>%
      select(LEP, SSA = ssa_t1_desc, Achievements = achievements) %>%
      group_by(LEP, SSA) %>%
      summarise(Achievements = sum(Achievements)) %>%
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
      levels = c(input$lep3, input$lep4)
    )
    Ach_SSA_pc <- ggplot(FEBar, aes(x = reorder(SSA, desc(SSA)), y = pc, fill = Area)) +
      geom_col(
        position = "dodge",
        aes(text = paste0(
          "SSA: ", SSA, "<br>",
          "Area: ", Area, "<br>",
          "Percentage of achievements: ", scales::percent(round(pc, 2)), "<br>",
          "Achievements: ", Achievements, "<br>"
        ))
      ) +
      scale_y_continuous(labels = scales::percent) +
      # scale_x_discrete(label = function(SSA) stringr::str_trunc(SSA, 12)) + # truncate labels because they can be very long
      scale_x_discrete(labels = function(SSA) str_wrap(SSA, width = 26)) +
      coord_flip() +
      theme_minimal() +
      labs(fill = "") +
      theme(legend.position = "bottom", axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_text(size = 7),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      scale_fill_manual(values = c("#1d70b8", "#F46A25"))
  })

  output$Ach_SSA_pc <- renderPlotly({
    ggplotly(Ach_SSA_pc(), tooltip = c("text"), height = 474) %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.1)) %>%
      config(displayModeBar = FALSE)
  })

  # OVERVIEW V2 ----

  ## KPIs ----

  ### Employment count ----
  output$locland.emplcntOver2 <- renderValueBox({
    valueBox(
      format((C_EmpRate_APS1721 %>%
        filter(
          geographic_level == "lep", # cleans up for London which is included as lep and gor
          area == input$lepOver2,
          year == "2021"
        )
      )$"28  in employment ",
      scientific = FALSE, big.mark = ","
      ),
      "people were employed in 2021",
      width = 12
    )
  })

  ### Employment change ----
  output$locland.emplcntchangeOver2 <- renderValueBox({
    # get value
    x <- ((C_EmpRate_APS1721 %>%
      filter(
        geographic_level == "lep", # cleans up for London which is included as lep and gor
        area == input$lepOver2,
        year == "2021"
      )
    )$"28  in employment "
      - (C_EmpRate_APS1721 %>%
        filter(
          geographic_level == "lep", # cleans up for London which is included as lep and gor
          area == input$lepOver2,
          year == "2020"
        )
      )$"28  in employment ")
    # build box
    valueBox(
      sprintf("%+.0f", x),
      subtitle = NULL,
      width = 12,
      icon = cond_icon(x > 0),
      color = cond_color(x > 0)
    )
  })

  ### Employment rate -----
  output$locland.emplrateOver2 <- renderValueBox({
    valueBox(
      # take input number
      paste(
        format(100. * (C_EmpRate_APS1721 %>%
          filter(
            geographic_level == "lep", # cleans up for London which is included as lep and gor
            area == input$lepOver2,
            year == "2021"
          )
        )$empRate, digits = 2),
        "%"
      ),
      paste(
        "employment rate in 2021 (compared with ",
        format(100. * (C_EmpRate_APS1721 %>%
          filter(
            geographic_level == "country", # cleans up for London which is included as lep and gor
            year == "2021"
          )
        )$empRate, digits = 2),
        "% for England)"
      ),
      width = 12
    )
  })

  ### Employment rate change -----
  output$locland.emplchangeOver2 <- renderValueBox({
    x <- (100. * ((C_EmpRate_APS1721 %>%
      filter(
        geographic_level == "lep", # cleans up for London which is included as lep and gor
        area == input$lepOver2,
        year == "2021"
      ))$empRate -
      (C_EmpRate_APS1721 %>%
        filter(
          geographic_level == "lep", # cleans up for London which is included as lep and gor
          area == input$lepOver2,
          year == "2020"
        ))$empRate))
    valueBox(
      # take input number
      paste(
        sprintf("%+.0f", x),
        "ppts"
      ),
      subtitle = NULL,
      width = 12,
      icon = cond_icon(x > 0),
      color = cond_color(x > 0)
    )
  })

  # Add button to link to employment data
  observeEvent(input$link_to_tabpanel_employment3, {
    updateTabsetPanel(session, "navbar", "Employment")
  })

  ### ONS job advert units  ----
  output$jobad.unitsOver2 <- renderValueBox({
    valueBox(C_Vacancy_England %>%
      filter(
        year == "2022",
        LEP == input$lepOver2
      ) %>%
      summarise(job.unit = sum(vacancy_unit)),
    "job vacancy units (Jan 22)",
    width = 12
    )
  })

  ### ONS job advert units change  ----
  output$jobad.changeOver2 <- renderValueBox({
    x <- ((C_Vacancy_England %>%
      filter(
        year == "2022",
        LEP == input$lepOver2
      ) %>%
      summarise(job.unit = sum(vacancy_unit)))
    - (C_Vacancy_England %>%
        filter(
          year == "2021",
          LEP == input$lepOver2
        ) %>%
        summarise(job.unit = sum(vacancy_unit))))
    valueBox(
      sprintf("%+.0f", x),
      subtitle = NULL,
      width = 12,
      icon = cond_icon(x > 0),
      color = cond_color(x > 0)
    )
  })

  # Add button to link to vacancy data
  observeEvent(input$link_to_tabpanel_vacancies3, {
    updateTabsetPanel(session, "navbar", "Vacancies")
  })

  ### Average salary  ----
  output$earn.avgOver2 <- renderValueBox({
    valueBox("In V2",
      "average salary (compared with £x for England)",
      width = 12
    )
  })

  ### Average salary change  ----
  output$earn.changeOver2 <- renderValueBox({
    x <- (0)
    valueBox(
      sprintf("%+.0f", x),
      subtitle = NULL,
      width = 12,
      icon = cond_icon(x > 0),
      color = cond_color(x > 0)
    )
  })

  # Add button to link to salary data
  observeEvent(input$link_to_tabpanel_earnings3, {
    updateTabsetPanel(session, "navbar", "Earnings")
  })

  ### L4+  ----
  output$skills.l4Over2 <- renderValueBox({
    valueBox("In V2",
      "qualified at level 4+ (compared with x% for England)",
      width = 12
    )
  })

  ### L4+ change  ----
  output$skills.l4changeOver2 <- renderValueBox({
    x <- (0)
    valueBox(
      sprintf("%+.0f", x),
      subtitle = NULL,
      width = 12,
      icon = cond_icon(x > 0),
      color = cond_color(x > 0)
    )
  })

  # Add button to link to skills data
  observeEvent(input$link_to_tabpanel_FE3, {
    updateTabsetPanel(session, "navbar", "FE")
  })


  ### E&T achievements -----
  output$skisup.ETachOver2 <- renderValueBox({
    valueBox(
      format((C_Achieve_ILR1621 %>%
        filter(
          time_period == "202021",
          LEP == input$lepOver2,
          level_or_type == "Education and training: Total"
        ) %>%
        summarise(App_ach = sum(achievements))), scientific = FALSE, big.mark = ","),
      "Education and training achievements (AY20/21)",
      width = 12
    )
  })

  ### E&T achievements change -----
  output$skisup.ETachChangeOver2 <- renderValueBox({
    x <- ((C_Achieve_ILR1621 %>%
      filter(
        time_period == "202021",
        LEP == input$lepOver2,
        level_or_type == "Education and training: Total"
      ) %>%
      summarise(App_ach = sum(achievements)))
    - (C_Achieve_ILR1621 %>%
        filter(
          time_period == "201920",
          LEP == input$lepOver2,
          level_or_type == "Education and training: Total"
        ) %>%
        summarise(App_ach = sum(achievements))))
    valueBox(
      sprintf("%+.0f", x),
      subtitle = NULL,
      width = 12,
      icon = cond_icon(x > 0),
      color = cond_color(x > 0)
    )
  })

  ### App achievements ----
  output$skisup.APPachOver2 <- renderValueBox({
    valueBox(
      format((C_Achieve_ILR1621 %>%
        filter(
          time_period == "202021",
          LEP == input$lepOver2,
          level_or_type == "Apprenticeships: Total"
        ) %>%
        summarise(App_ach = sum(achievements))), scientific = FALSE, big.mark = ","),
      "Apprenticeship achievements (AY20/21)",
      width = 12
    )
  })

  ### App achievements change ----
  output$skisup.APPachChangeOver2 <- renderValueBox({
    x <- ((C_Achieve_ILR1621 %>%
      filter(
        time_period == "202021",
        LEP == input$lepOver2,
        level_or_type == "Apprenticeships: Total"
      ) %>%
      summarise(App_ach = sum(achievements)))
    - (C_Achieve_ILR1621 %>%
        filter(
          time_period == "201920",
          LEP == input$lepOver2,
          level_or_type == "Apprenticeships: Total"
        ) %>%
        summarise(App_ach = sum(achievements))))
    valueBox(
      sprintf("%+.0f", x),
      subtitle = NULL,
      width = 12,
      icon = cond_icon(x > 0),
      color = cond_color(x > 0)
    )
  })

  ### HE entrants  ----
  output$he.entrantsOver2 <- renderValueBox({
    valueBox("In V2",
      "HE entrants",
      width = 12
    )
  })

  ### HE+ change  ----
  output$he.entrantschangeOver2 <- renderValueBox({
    x <- (0)
    valueBox(
      sprintf("%+.0f", x),
      subtitle = NULL,
      width = 12,
      icon = cond_icon(x > 0),
      color = cond_color(x > 0)
    )
  })

  # Add button to link to HE data
  observeEvent(input$link_to_tabpanel_HE3, {
    updateTabsetPanel(session, "navbar", "HE")
  })

  # Stop app ---------------------------------------------------------------------------------

  session$onSessionEnded(function() {
    stopApp()
  })
}
