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
    fluidRow(
      h2("Headline data"),
      column(
        4,
        value_box(
          title = "Working: Employment rate",
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
          title = "Demand: Online job adverts",
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
          title = "Skills supply: Apprenticeship achievements",
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
