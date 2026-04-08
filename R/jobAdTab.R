jobAdTab <- function() {
  bslib::nav_panel(
    "Online job adverts",
    value = "job_ad_tab",
    # Add banner note for feedback
    shinyGovstyle::banner(
      "update banner",
      "Note",
      paste0(
        "This page is in development and will change. Please contact us at ",
        "<a href='mailto:skillsengland.analysisandinsight@dwp.gov.uk'>skillsengland.analysisandinsight@dwp.gov.uk</a>",
        " to provide feedback."
      )
    ),
    # CSS for banner
    tags$style(HTML("
/* --- Minimise margin --- */
    .govuk-phase-banner {
      margin-left: 0px;
      margin-top: 10px;
      background-color: #c5cdd7;
      text-align: left;
    }

    .govuk-phase-banner__content {
        text-align: left;
        display: flex;
        align-items: center;
    }

    .govuk-phase-banner__content__tag {
        background-color: #774b99;
        color: white;
        margin-left: 10px;
    }
")),
    # Bespoke code to disable 'Area' dropdown and hide arrow - this will be removed once regions are added in
    tags$head(
      tags$style(HTML("
        /* Disable clicking on the #jobGeoChoice dropdown */
        #jobGeoChoice + .selectize-control {
          pointer-events: none;                /* block mouse events (no open) */
        }
        /* Make the field look read-only */
        #jobGeoChoice + .selectize-control .selectize-input {
          background-color: #f5f5f5;           /* subtle grey background */
          cursor: default;
        }
        /* Hide the arrow */
        #jobGeoChoice + .selectize-control .selectize-input:after {
          display: none !important;
        }
        /* Ensure no dropdown menu appears */
        #jobGeoChoice + .selectize-control .selectize-dropdown {
          display: none !important;
        }
      "))
    ),
    # End of bespoke code to disable 'Area' dropdown
    # Set the font within the search box to normal (i.e. not bold as is the default)
    tags$head(
      tags$style(HTML("
    .dataTables_filter input {
      font-weight: normal;
    }
  "))
    ),
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
            label = "Area",
            choices = c("England"),
            selected = "England", # Default to England but this will be updated when regions are added
            options = list(
              persist = TRUE, # keep selected value
              create = FALSE, # disallow new values
              onDelete = I("function(values) { return false; }")
            )
          ),
          # selectizeInput(
          #   "jobComparisonChoice",
          #   multiple = TRUE,
          #   label = "Choose to combine or compare",
          #   choices = areaChoices,
          #   options = list(
          #     maxItems = 7,
          #     placeholder = "Choose to combine or compare"
          #   )
          # )
        ),
        # column(
        #   4,
        #   selectizeInput(
        #     inputId = "jobOccupationChoice",
        #     choices = c("Add occupation list"),
        #     multiple = FALSE,
        #     label = "Choose occupation(s)",
        #     options = list(
        #       persist = TRUE, # keep selected value
        #       create = FALSE, # disallow new values
        #       onDelete = I("function(values) { return false; }")
        #     )
        #   ),
        # ),
        column(
          4,
          selectizeInput(
            inputId = "jobMetric",
            choices = jobMetricChoices,
            multiple = FALSE,
            label = "Choose a metric for the time series",
            options = list(
              persist = TRUE, # keep selected value
              create = FALSE, # disallow new values
              onDelete = I("function(values) { return false; }")
            )
          ),
        )
      )
    ),
    fluidRow(
      column(
        12,
        p(uiOutput("jobCaveatText")),
        hr(),
        p(uiOutput("jobDynamicText"))
      )
    ),
    ### 2.3.2 Visuals row 1 ----
    fluidRow(
      column(
        6,
        h3("Where are new job adverts focussed across England?"),
        p(uiOutput("jobMapComment")),
        radioGroupButtons(
          inputId = "jobMapSwitch",
          choices = c("Map", "List")
        ),
        withSpinner(uiOutput("jobMapUI")),
        p(uiOutput("jobMapFooter"))
      ),
      column(
        6,
        h3(uiOutput("jobTimeHeading")),
        p(uiOutput("jobTimeComment")),
        withSpinner(plotlyOutput("jobTime"))
      )
    ),
    br(),
    ### 2.3.3 Visuals row 2 ----
    fluidRow(
      column(
        6,
        h3("Which occupations have the highest volumes of online job adverts?"),
        p(uiOutput("jobRankComment")),
        withSpinner(DT::dataTableOutput("jobRankTable")),
        br(),
        p(uiOutput("jobRankFooter"))
      ),
      column(
        6,
        h3(uiOutput("jobDemandHeading")),
        p(uiOutput("jobDemandComment")),
        div(
          style = "text-align: right;",
          radioGroupButtons(
            inputId = "jobTableSwitch",
            choices = c("Emerging Demand", "Constant Demand")
          )
        ),
        withSpinner(uiOutput("jobDemandTable")),
        br(),
        p(uiOutput("jobDemandFooter"))
      )
    ),
    br(),
    ### 2.3.3 Downloads ----
    # fluidRow(
    #   column(
    #     width = 3,
    #     downloadButton(
    #       outputId = "jobAdDownload",
    #       label = "All areas   ",
    #       icon = shiny::icon("download"),
    #       class = "downloadButton"
    #     )
    #   ),
    #   column(
    #     width = 9,
    #     "Download metric data for all geographies (LSIP, CA areas, LAs, regions and England)",
    #   )
    # ),
    # fluidRow(
    #   column(
    #     width = 3,
    #     downloadButton(
    #       outputId = "jobAdDownload",
    #       label = "Current geographic areas",
    #       icon = shiny::icon("download"),
    #       class = "downloadButton"
    #     )
    #   ),
    #   column(width = 9, "Download metric data for the selected geographic areas")
    # ),
    ### 2.3.3 Data notes ----
    fluidRow(column(
      12,
      h2("Data notes"),
      p("Any NAs or missing data in the charts or maps are due to supressed data."),
      p(uiOutput("jobDataSource")),
      p("Caveats:"),
      p(uiOutput("jobDataCaveat"))
    )),
    br()
  )
}
