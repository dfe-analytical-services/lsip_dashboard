panel_overview <- function() {
  tabPanel(
    "Overview",
      h1(uiOutput("page0title")),
      p("Change metrics are measured since the same period the year before."),
      gov_row(style="padding-left: 15px;padding-right: 15px;",#indent slightly so box aligns
        # left column
        column(
          width = 6,
          style = "background-color:#f3f2f1;border-right: 5px solid white;",
          h2("Labour market"),
          h3("People employed"),
          gov_row(
            column(
              width = 4,
              div(#need a div to add hover over title
                title = "Source: APS. 2021 calendar year",
                uiOutput("locland.emplcnt0"),
              )
            ),
            column(
              width = 8,
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
              width = 8,
              plotlyOutput("empRateLineChart", height = 81)
            )
          ),
          # third row - link to emp tab 
          fluidRow(style="text-align: right;padding-right:15px;padding-top:15px;",
            actionLink("link_to_tabpanel_employment2", "Find out more about employment")),
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
              width = 8,
              plotlyOutput("VacLineChart", height = 81)
            )
          ),
          fluidRow(style="text-align: right;padding-right:15px;padding-top:15px;",
                   actionLink("link_to_tabpanel_vacancies2", "Find out more about vacancies")),
          br()
        ),
        # right column
        column(
          width = 6,
          style = "background-color:#f3f2f1;border-right: 5px solid white;",
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
              width = 8,
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
              width = 8,
              plotlyOutput("AppLineChart", height = 81)
            )
          ),
          # 6th row - link to app data
          fluidRow(style="text-align: right;padding-right:15px;padding-top:15px;",
                   actionLink("link_to_tabpanel_FE2", "Find out more about skills")),
br()
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
      ),
    column(width = 12, br(""))
  )
}

panel_employment <- function() {
  tabPanel(
    "Employment",
      h1(uiOutput("page1title")),
      p("Data is from the Annual Population Survey. Years represent calendar years. All figures are for 16-64 except the occupation split table which is all ages."),
      ### KPI boxes ----
    fluidRow(
        valueBoxOutput("locland.emplcnt"),
        valueBoxOutput("locland.emplrate")
      ),
    fluidRow(
        uiOutput("emp_comp")
      ),
      ### Employment rate over time line chart ----
    fluidRow(column(
        width = 6,
        p("Employment rate trend", style = "font-size:20px;"),
        plotlyOutput("EmpRate_time")
      ),
      ### Employment percentage by occupation data table ----
      column(
        width = 6,
        p("Employment percentage by occupation (sub-major SOC group)", style = "font-size:20px;"),
        dataTableOutput("EmpOcc")
      )),

      ### Downloads-------------
    br(),
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
    ), # end of row
    column(width = 12, br(""))
  ) # end of Local Landscape tab
}

panel_vacancies <- function() {
  tabPanel(
    "Vacancies",
      h1(uiOutput("page3title")),
      p("Data is from ONS using Adzuna online job adverts. Data for each year is the average of vacancies across January of that year."),
      ### KPI boxes ----
    fluidRow(
        valueBoxOutput("jobad.pc"),
        valueBoxOutput("jobad.ch"),
      ), # end of box
    fluidRow(
        uiOutput("vac_comp")
      ),
    fluidRow(style="padding-left: 15px;padding-right: 15px",#indent slightly so box aligns
        ### Online job vacancy units over time line chart ----
        p("Online job vacancy unit trend", style = "font-size:20px;"),
        plotlyOutput("jobad.time")
      ), # end of box
    fluidRow(style="padding-left: 15px",#indent slightly so box aligns
      details(
        inputId = "SubsLev",
        label = "Chart information",
        help_text = "Each time point in the series covers a monthly average of the volume of online job adverts in the month of January for the years 2017 to 2022.
              The monthly average is derived from weekly snapshots in January. The volume of online job adverts is presented as a standardised unit measure. The unit measure is derived by dividing the actual monthly average count of job adverts by a single set value. The job vacancy units can therefore be used to compare between LEPS and over time, but do not represent true job vacancy volumes."
      )),
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
      column(width = 12, br(""))
  ) # end of Skills Supply tab
}


panel_skills <- function() {
  tabPanel(
    "Skills",
      h1(uiOutput("page2title")),
      p("Data from Individualised Learner Records for FE and skills learners. Years shown are academic years."),
      ### KPI boxes ----
    fluidRow(
        valueBoxOutput("skisup.FEach"),
        valueBoxOutput("skisup.APach")
      ),
    fluidRow(
        uiOutput("skill_comp")
      ),
        ### Achievements over time line chart ----
    fluidRow(style="padding-left: 15px;padding-right: 15px;",#indent slightly so box aligns
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
          ),
        ### FE achievements by SSA----
    fluidRow(style="padding-left: 15px",#indent slightly so box aligns
            p("All FE and skills learner achievements by SSA tier 1 (AY21/22 Aug to Jan provisional data)", style = "font-size:20px;"),
            plotlyOutput("Ach_SSA_pc")
          ),
    column(width = 12, br("")),#put in to push below the fixed height chart
    column(width = 12, br("")),
    ### FE definitions----
    fluidRow(style="padding-left: 15px",#indent slightly so box aligns
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
  ) # skills panel
}
