panel_overview <- function(){
  tabPanel(
    "Overview",
      column(width=12,br("")),
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
    column(width=12,br("")),
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

panel_vacancies <- function(){
  tabPanel(
    "Vacancies",
    column(width=12,br("")),
    column(
      width = 12,
      uiOutput("page3title", style = "font-size: 24px;"),
      div("Data is from ONS using Adzuna online job adverts. Data for each year is the average of vacancies across January of that year.", style = "font-size: 16px; font-style: italic;"),
      br(),
      ### KPI boxes ----
      column(
        width = 12,
        valueBoxOutput("jobad.pc"),
        valueBoxOutput("jobad.ch"),
      ), # end of box
      column(
        width = 12,
        uiOutput("vac_comp")
      ),
      column(
        width = 12,
        p(" ")
      ),
      column(
        width = 12,
        ### Online job vacancy units over time line chart ----
        p("Online job vacancy unit trend", style = "font-size:20px;"),
        plotlyOutput("jobad.time")
      ), # end of box
      details(
        inputId = "SubsLev",
        label = "Chart information",
        help_text = "Each time point in the series covers a monthly average of the volume of online job adverts in the month of January for the years 2017 to 2022.
              The monthly average is derived from weekly snapshots in January. The volume of online job adverts is presented as a standardised unit measure. The unit measure is derived by dividing the actual monthly average count of job adverts by a single set value. The job vacancy units can therefore be used to compare between LEPS and over time, but do not represent true job vacancy volumes."
      ),
      ### Downloads-------------
      br(),
      gov_row(
        column(
          width = 3,
          downloadButton(
            outputId = "download_btn3a",
            label = "All data   ",
            icon = shiny::icon("download"),
            class = "downloadButton"
          )
        ),
        column(
          width = 9,
          "Download vacancy indicators for all geographies (LEPs, LAs, Regions and England)",
        )
      ), # end of row
      gov_row(
        column(
          width = 3,
          downloadButton(
            outputId = "download_btn3b",
            label = "Current LEP",
            icon = shiny::icon("download"),
            class = "downloadButton"
          )
        ),
        column(width = 9, p("Or just for the currently chosen LEP")),
      ), # end of row
      column(width=12,br(""))
    ) # end of main panel
  ) # end of Skills Supply tab
}


panel_skills <- function(){
  tabPanel(
    "Skills",
    column(width=12,br("")),
    column(
      width = 12,
      uiOutput("page2title", style = "font-size: 24px;"),
      div("Data from Individualised Learner Records for FE and skills learners. Years shown are academic years.", style = "font-size: 16px; font-style: italic;"),
      br(),
      ### KPI boxes ----
      column(
        width = 12,
        valueBoxOutput("skisup.FEach"),
        valueBoxOutput("skisup.APach")
      ),
      column(
        width = 12,
        uiOutput("skill_comp")
      ),
      tabsetPanel(
        ### Achievements over time line chart ----
        tabPanel(
          "Achievements over time",
        column(
          width = 12,
          p("FE and skills learner achievement trend", style = "font-size:20px;"),
          div(
            class = "well",
            style = "min-height: 100%; height: 100%; overflow-y: visible",
            p("Choose provision group:"),
            selectizeInput("skill_line", NULL,
                           choices = c("Apprenticeships (all ages)", "Education and training (adults only)", "Community learning (adults only)", "Total FE and skills provision")
            )
          ),
          
          plotlyOutput("Ach_time"),
          p("Total achievements are the count of learners that achieved at any point during the stated academic period. Learners achieving more than one course will appear only once in the grand total.", style = "font-style: italic;")
        )
        ),
        ### FE achievements by SSA----
        tabPanel(
          "Achievements by SSA",
          column(
          width = 12,
          p("All FE and skills learner achievements by SSA tier 1 (AY21/22 Aug to Jan provisional data)", style = "font-size:20px;"),
          plotlyOutput("Ach_SSA_pc")
        )
        )
      ), # end of charts box
      ### FE definitions----
      details(
        inputId = "FEdefs",
        label = "FE definitions",
        tags$ul(
          tags$li("FE and skills include all age apprenticeships and wider adult (19+) FE learning, such as community learning and education and training."),
          tags$li("Further Education covers publicly-funded learning delivered by an FE institution, a training provider or within a local community. It also includes apprenticeships delivered in the workplace. It does not include higher education, unless delivered as part of an apprenticeship programme."),
          tags$li("Apprenticeships are paid jobs that incorporate on-the-job and off-the-job training leading to nationally recognised qualifications."),
          tags$li("Community learning funds a wide range of non-formal courses (e.g. IT or employability skills) and activity targeted at deprived areas or disadvantaged groups. They can be offered by local authorities, colleges, community groups."),
          tags$li("Education and training is mainly classroom-based adult FE that is not an apprenticeship or community learning."),
          tags$li("Achievements are the number of learners who successfully complete an individual aim in an academic year. ")
        )
      )
    ), 
    column(width = 12, br("")),
    gov_row(
      column(
          width = 3,
          downloadButton(
            outputId = "download_btn2a",
            label = "All data   ",
            icon = shiny::icon("download"),
            class = "downloadButton"
          )
        ),
        column(
          width = 9,
          "Download FE and skills indicators for all geographies (LEPs, LAs, Regions and England)"
        )
      ), # end of row
      gov_row(
        column(
          width = 3,
          downloadButton(
            outputId = "download_btn2b",
            label = "Current LEP",
            icon = shiny::icon("download"),
            class = "downloadButton"
          )
        ),
        column(width = 9, p("Or just for the currently chosen LEP"))
      ), # end of row
      column(width = 12, br(""))
  ) # sills panel
}