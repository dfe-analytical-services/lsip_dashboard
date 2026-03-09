jobAdTab <- function() {
  bslib::nav_panel(
    "Online job adverts",
    value = "job_ad_tab",
    br(),
    ### 2.3.1 Filters ----
    div(
      class = "filterRow",
      fluidRow(
        column(
          4,
          selectizeInput(
            "jobGeoChoice",
            multiple = FALSE,
            label = "Choose area(s)",
            choices = areaChoices[1:3],
            options = list(
              persist = TRUE, # keep selected value
              create = FALSE, # disallow new values
              onDelete = I("function(values) { return false; }")
            )
          ),
          selectizeInput(
            "jobComparisonChoice",
            multiple = TRUE,
            label = "Choose to combine or compare",
            choices = areaChoices,
            options = list(
              maxItems = 7,
              placeholder = "Choose to combine or compare"
            )
          )
        ),
        column(
          4,
          selectizeInput(
            inputId = "jobOccupationChoice",
            choices = c("Add occupation list"),
            multiple = FALSE,
            label = "Choose occupation(s)",
            options = list(
              persist = TRUE, # keep selected value
              create = FALSE, # disallow new values
              onDelete = I("function(values) { return false; }")
            )
          ),
        ),
        column(
          4,
          selectizeInput(
            inputId = "jobMetricChoice",
            choices = c("Volume", "Growth rate", "Per population", "Per job"),
            multiple = FALSE,
            label = "Choose a metric",
            options = list(
              persist = TRUE, # keep selected value
              create = FALSE, # disallow new values
              onDelete = I("function(values) { return false; }")
            )
          ),
        )
      )
    ),

    ### 2.3.2 Visuals row 1 ----
    fluidRow(
      column(
        6,
        p(uiOutput("jobMapComment")),
        radioGroupButtons(
          inputId = "jobMapSwitch",
          choices = c("Map", "List")
        ),
        withSpinner(leafletOutput("jobMap"))
      ),
      column(
        6,
        p(uiOutput("jobTimeComment")),
        withSpinner(plotlyOutput("jobTime"))
      )
    ),
    br(),
    ### 2.3.3 Visuals row 2 ----
    fluidRow(
      column(
        6,
        p(uiOutput("jobBarComment")),
        radioGroupButtons(
          inputId = "jobBarSwitch",
          choices = c("Bar Chart", "Table")
        ),
        withSpinner(uiOutput("jobBar"))
      ),
      column(
        6,
        p(uiOutput("jobTableComment")),
        withSpinner(uiOutput("jobTable"))
      )
    ),
    ### 2.3.3 Downloads ----
    fluidRow(
      column(
        width = 3,
        downloadButton(
          outputId = "jobAdDownload",
          label = "All areas   ",
          icon = shiny::icon("download"),
          class = "downloadButton"
        )
      ),
      column(
        width = 9,
        "Download metric data for all geographies (LSIP, CA areas, LAs, regions and England)",
      )
    ),
    fluidRow(
      column(
        width = 3,
        downloadButton(
          outputId = "jobAdDownload",
          label = "Current geographic areas",
          icon = shiny::icon("download"),
          class = "downloadButton"
        )
      ),
      column(width = 9, "Download metric data for the selected geographic areas")
    ),
    ### 2.3.3 Data notes ----
    fluidRow(column(
      12,
      h2("Data notes"),
      p("XXX."),
    )),
    br()
  )
}
