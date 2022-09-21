panel_overview <- function(){
  tabPanel(
    "Overview",
    ## Main panel ----
      column(
        width = 12,
        uiOutput("page0title", style = "font-size: 24px;"),
        div("Change metrics are measured since the same period the year before.", style = "font-size: 16px; font-style: italic;"),
        br(),
        gov_row(
          # left column
          column(
            width = 5,
            style = "background-color:#f3f2f1;",
            h2("Labour market"),
            h3("People employed"),
            gov_row(
              column(
                width = 4,
                div(
                  title = "Source: APS. 2021 calendar year",
                  uiOutput("locland.emplcnt0"),
                )
              ),
              column(
                width = 7,
                plotlyOutput("empLineChart", height = 81)
              )
            ),
            h3("Employment rate"),
            gov_row(
              column(
                width = 4,
                div(
                  title = "Source: APS. 2021 calendar year",
                  uiOutput("locland.emplrate0"),
                )
              ),
              column(
                width = 7,
                plotlyOutput("empRateLineChart", height = 81)
              )
            ),
            # third row - link to emp tab
            box(
              width = 12,
              actionLink("link_to_tabpanel_employment2", "Find out more about employment"),
              align = "right"
            ),
            box(
              width = 12,
              p(" ")
            ),
            # fourth row - vacancies
            h3("Job vacancy share"),
            gov_row(
              column(
                width = 4,
                div(
                  title = "Source: ONS (Adzuna). Jan 2022. Share of job vacancies in England.",
                  uiOutput("jobad.units"),
                )
              ),
              column(
                width = 7,
                plotlyOutput("VacLineChart", height = 81)
              )
            ),
            box(
              width = 12,
              actionLink("link_to_tabpanel_vacancies2", "Find out more about vacancies"),
              align = "right"
            )
          ),
          column(width = 1), # column split
          # right column
          column(
            width = 5,
            style = "background-color:#f3f2f1;",
            h2("Skills landscape"),
            h3("Education and training achievements"),
            gov_row(
              column(
                width = 4,
                div(
                  title = "Source: ILR AY20/21",
                  uiOutput("skisup.ETach"),
                )
              ),
              column(
                width = 7,
                plotlyOutput("etLineChart", height = 81)
              )
            ),
            h3("Apprenticeship achievements"),
            gov_row(
              column(
                width = 4,
                div(
                  title = "Source: ILR AY20/21",
                  uiOutput("skisup.APPach"),
                )
              ),
              column(
                width = 7,
                plotlyOutput("AppLineChart", height = 81)
              )
            ),
            # 6th row - link to app data
            box(
              width = 12,
              actionLink("link_to_tabpanel_FE2", "Find out more about skills"),
              align = "right"
            ),
          ) # end of right column
        ), # end of data row
        ### Downloads-------------
        br(),
        gov_row(
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
            "Download all available indicators for all geographies (LEPs, LAs, Regions and England)",
          )
        ),
        gov_row(
          column(
            width = 3,
            downloadButton(
              outputId = "download_btn0b",
              label = "Current LEP",
              icon = shiny::icon("download"),
              class = "downloadButton"
            )
          ),
          column(width = 9, "Or just for the currently chosen LEP")
        )
      ),
      column(width=12,br(""))
    )

}

panel_employment <- function(){
  tabPanel(
    "Employment",
    ## Main panel ----
      column(
        width = 12,
        uiOutput("page1title", style = "font-size: 24px;"),
        div("Data is from the Annual Population Survey. Years represent calendar years. All figures are for 16-64 except the occupation split table which is all ages.", style = "font-size: 16px; font-style: italic;"),
        br(),
        
        ### KPI boxes ----
        column(
          width = 12,
          valueBoxOutput("locland.emplcnt"),
          valueBoxOutput("locland.emplrate")
        ),
        column(
          width = 12,
          uiOutput("emp_comp")
        ),
        column(
          width = 12,
          p(" ")
        ),
        ### Employment rate over time line chart ----
        column(
          width = 6,
          p("Employment rate trend", style = "font-size:20px;"),
          plotlyOutput("EmpRate_time")
        ),
        ### Employment percentage by occupation data table ----
        column(
          width = 6,
          p("Employment percentage by occupation (sub-major SOC group)", style = "font-size:20px;"),
          dataTableOutput("EmpOcc")
        ),
        # br(),
        # column(
        #   width = 12,
        #   p("Employment rates", style = "font-size:20px;"),
        #   plotlyOutput("EmpRate_dot")
        # ),
        
        ### Downloads-------------
        column(
          width = 12,
          p(" ")
        ),
        gov_row(
          column(
            width = 3,
            downloadButton(
              outputId = "download_btn1a",
              label = "All data   ",
              icon = shiny::icon("download"),
              class = "downloadButton"
            )
          ),
          column(
            width = 9,
            "Download employment indicators for all geographies (LEPs, LAs, Regions and England)",
          )
        ), # end of row
        gov_row(
          column(
            width = 3,
            downloadButton(
              outputId = "download_btn1b",
              label = "Current LEP",
              icon = shiny::icon("download"),
              class = "downloadButton"
            )
          ),
          column(width = 9, p("Or just for the currently chosen LEP"))
        )
      ),# end of row
      column(width = 12, br(""))
  ) # end of Local Landscape tab
}
