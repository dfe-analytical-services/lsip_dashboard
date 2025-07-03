summaryTab <- function() {
  tabPanel(
    "Summary",
    # 1 Filters ----
    fluidRow(
      column(
        12,
        br(),
        div(
          class = "filterRow",
          fluidRow(
            column(
              width = 9,
              p("Choose an LSIP, MCA or England"),
              selectizeInput(
                "geoChoiceOver",
                multiple = FALSE,
                label = NULL,
                choices = areaChoices[1:3]
              )
            ),
            column(
              3,
              uiOutput("screenshotOverview")
            )
          )
        ),
        br(),
      )
    ), # end of filters row

    h1(uiOutput("page0title")),
    h2("Headline data"),
    fluidRow(
      column(
        4,
        value_box(
          title = span(tags$p("Workforce:", style = "font-weight: bold;"), p("Employment rate")),
          value = uiOutput("summaryEmployment"),
          # "England 76%",
          showcase = plotlyOutput("sparklineEmployment"),
          showcase_layout = "bottom",
          # theme = "primary"
        ),
        # third row - link to emp tab
        fluidRow(
          class = "rightAlignLinks",
          actionLink("link_to_tabpanel_empRate", "More about employment rates")
        )
      ),
      column(
        4,
        value_box(
          title = span(tags$p("Demand:", style = "font-weight: bold;"), p("Online job adverts")),
          value = uiOutput("summaryAdverts"),
          showcase = plotlyOutput("sparklineAdverts"),
          showcase_layout = "bottom",
          # theme = "primary"
        ),
        fluidRow(
          class = "rightAlignLinks",
          actionLink("link_to_tabpanel_vacancies2", "More about online job adverts")
        )
      ),
      column(
        4,
        value_box(
          title = span(tags$p("Skills supply:", style = "font-weight: bold;"), p("Apprenticeship achievements")),
          value = uiOutput("summaryAppAchievements"),
          showcase = plotlyOutput("sparklineAppAchievements"),
          showcase_layout = "bottom",
          # theme = "primary"
        ),
        fluidRow(
          class = "rightAlignLinks",
          actionLink("link_to_tabpanel_FE2", "More about achievements")
        )
      )
    ),
    p("Change quoted is since last year."),
    br(),
    # Employment projections
    fluidRow(
      style = "padding-left: 15px;padding-right: 15px;", # indent slightly so box aligns
      column(12,
        class = "chartBox",
        h3("Top projected growth occupations"),
        p(uiOutput("summaryTopProjected")),
        dataTableOutput("summaryTopProjectedListTable"),
        fluidRow(
          class = "rightAlignLinks",
          actionLink("link_to_tabpanel_wf", "More about employment projections")
        )
      )
    ),
    p(""),
    fluidRow(
      column(
        12,
        h3("Businesses"),
        p(uiOutput("summaryBusinesses")),
        withSpinner(plotlyOutput("summaryBusinessesChartCurrent")),
        fluidRow(
          class = "rightAlignLinks",
          actionLink("link_to_tabpanel_enterprise2", "More about businesses")
        )
      )
    ),
    fluidRow(
      h4(actionLink("link_to_tabpanel_LS", "Explore more data and metrics on the Local skills data page."))
    ),
    ### Downloads-------------
    br(),
    fluidRow(
      column(
        width = 3,
        downloadButton(
          outputId = "download_btn0a",
          label = "All data   ",
          icon = shiny::icon("download"),
          class = "downloadButton"
        )
      ),
      column(
        width = 9,
        "Download all data for all geographies (LEPs, LSIP, MCA areas, LAs, regions and England)",
      )
    ),
    fluidRow(
      column(
        width = 3,
        downloadButton(
          outputId = "download_btn0b",
          label = "Current geographic area",
          icon = shiny::icon("download"),
          class = "downloadButton"
        )
      ),
      column(width = 9, "Download all data for the selected geographic area")
    ),
    column(width = 12, br(""))
  )
}
