panel_overview <- function() {
  tabPanel(
    "Overview",
    # 1 Filters ----
    fluidRow(
      column(
        12,
        br(),
        div(
          class = "filterRow",
          fluidRow(
            # column(
            #   width = 4,
            #   selectInput("GeoType", "Choose geography",
            #     choices = c(
            #       "Local Enterprise Partnership (LEP)" = "LEP",
            #       "Local Skills Improvement Plan (LSIP)" = "LSIP",
            #       "Mayoral Combined Authority (MCA)" = "MCA"
            #     ),
            #     selected = "LEP"
            #   )
            # ),
            column(
              width = 4,
              p("Choose a LEP, LSIP or MCA"),
              uiOutput("geoChoiceOver")
            ),
            column(5),
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
    p("Change metrics are measured against the same period in the previous year. NB non-zero axes."),
    fluidRow(
      style = "padding-left: 15px;padding-right: 15px;", # indent slightly so box aligns
      # left column
      column(
        width = 6,
        class = "chartBox",
        h2("Labour market"),
        h3("People employed"),
        fluidRow(
          column(
            width = 4,
            div( # need a div to add hover over title
              title = "Source: APS. Oct-Sep 2022",
              uiOutput("overviewEmpCntKPI"),
            )
          ),
          column(
            width = 8,
            withSpinner(plotlyOutput("empLineChart", height = 81))
          )
        ),
        # third row - link to emp tab
        fluidRow(
          class = "rightAlignLinks",
          actionLink("link_to_tabpanel_employment2", "Find out more about employment volumes")
        ),
        h3("Employment rate"),
        fluidRow(
          column(
            width = 4,
            div(
              title = "Source: APS. Oct-Sep 2022",
              uiOutput("overviewEmpRateKPI"),
            )
          ),
          column(
            width = 8,
            withSpinner(plotlyOutput("empRateLineChart", height = 81))
          )
        ),
        # third row - link to emp tab
        fluidRow(
          class = "rightAlignLinks",
          actionLink("link_to_tabpanel_empRate", "Find out more about employment rates")
        ),
        # fourth row - vacancies
        h3("Online job adverts (experimental)"),
        fluidRow(
          column(
            width = 4,
            div(
              title = "Source: ONS (Textkernel). Oct 2022. Online job adverts.",
              uiOutput("overviewJobKPI"),
            )
          ),
          column(
            width = 8,
            withSpinner(plotlyOutput("jobLineChart", height = 81))
          )
        ),
        fluidRow(
          class = "rightAlignLinks",
          actionLink("link_to_tabpanel_vacancies2", "Find out more about online job adverts")
        ),

        # sixth row - enterprise
        h3("Share of businesses with 0-9 employees (micro)"),
        fluidRow(
          column(
            width = 4,
            div(
              title = "Source: UBC. 2022 calendar year",
              uiOutput("UBC.micro"),
            )
          ),
          column(
            width = 8,
            withSpinner(plotlyOutput("UBCLineChart", height = 81))
          )
        ),
        fluidRow(
          class = "rightAlignLinks",
          actionLink("link_to_tabpanel_enterprise2", "Find out more about businesses")
        ),

        # 7th row - working futures
        h3("Year on year projected employment growth"),
        fluidRow(
          column(
            width = 4,
            div(
              title = "Source: Skills Imperative 2035.",
              uiOutput("wfOverviewKpi"),
            )
          ),
          column(
            width = 8,
            withSpinner(plotlyOutput("wfOverviewChart", height = 81))
          )
        ),
        fluidRow(
          class = "rightAlignLinks",
          actionLink("link_to_tabpanel_wf", "Find out more about employment projections")
        ),
        br()
      ),
      # right column
      column(
        width = 6,
        class = "chartBox",
        h2("Skills"),
        h3("Education and training achievements"),
        fluidRow(
          column(
            width = 4,
            div(
              title = "Source: ILR AY21/22",
              uiOutput("skisup.ETach"),
            )
          ),
          column(
            width = 8,
            withSpinner(plotlyOutput("etLineChart", height = 81))
          )
        ),
        h3("Apprenticeship achievements"),
        fluidRow(
          column(
            width = 4,
            div(
              title = "Source: ILR AY21/22",
              uiOutput("skisup.APPach"),
            )
          ),
          column(
            width = 8,
            withSpinner(plotlyOutput("AppLineChart", height = 81))
          )
        ),
        fluidRow(
          class = "rightAlignLinks",
          actionLink("link_to_tabpanel_FE2", "Find out more about skills")
        ),
        # fifth row - destinations
        h3("Key Stage 5 positive destination rate"),
        fluidRow(
          column(
            width = 4,
            div(
              title = "Source: NPD. 2021 academic year",
              uiOutput("dest.ks5over"),
            )
          ),
          column(
            width = 8,
            withSpinner(plotlyOutput("KS5LineChart", height = 81))
          )
        ),
        fluidRow(
          class = "rightAlignLinks",
          actionLink("link_to_tabpanel_destinations2", "Find out more about destinations")
        ),
        # 6th row - link to app data
        h3("People with a qualification at level 3 or above"),
        fluidRow(
          column(
            width = 4,
            div(
              title = "Source: APS. 2021 calendar year",
              uiOutput("APS.nvq3plus"),
            )
          ),
          column(
            width = 8,
            withSpinner(plotlyOutput("Nvq3plusLineChart", height = 81))
          )
        ),
        # third row - link to emp tab
        fluidRow(
          class = "rightAlignLinks",
          actionLink("link_to_tabpanel_qualification2", "Find out more about qualification level")
        ),
        br()
      ) # end of right column
    ), # end of data row
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
