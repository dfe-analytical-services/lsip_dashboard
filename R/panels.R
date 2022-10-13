panel_overview <- function() {
  tabPanel(
    "Overview",
    h1(uiOutput("page0title")),
    p("Change metrics are measured since the same period the year before."),
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
        fluidRow(
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
        fluidRow(
          class = "rightAlignLinks",
          actionLink("link_to_tabpanel_employment2", "Find out more about employment")
        ),
        # fourth row - vacancies
        h3("Job vacancy share (experimental)"),
        fluidRow(
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
        fluidRow(
          class = "rightAlignLinks",
          actionLink("link_to_tabpanel_vacancies2", "Find out more about vacancies")
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
        fluidRow(
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
        fluidRow(
          class = "rightAlignLinks",
          actionLink("link_to_tabpanel_FE2", "Find out more about skills")
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
        "Download all data for all geographies (LEPs, LISP areas, LAs, regions and England)",
      )
    ),
    fluidRow(
      column(
        width = 3,
        downloadButton(
          outputId = "download_btn0b",
          label = "Current LEP/LSIP",
          icon = shiny::icon("download"),
          class = "downloadButton"
        )
      ),
      column(width = 9, "Download all data for the selected LEP/LSIP area")
    ),
    column(width = 12, br(""))
  )
}

panel_employment <- function() {
  tabPanel(
    "Employment",
    h1(uiOutput("page1title")),
    ### KPI boxes ----
    fluidRow(
      valueBoxOutput("locland.emplcnt"),
      valueBoxOutput("locland.emplrate")
    ),
    fluidRow(
      uiOutput("emp_comp")
    ),
    details(label="Data notes",
            inputId = "empKPISource",
            p("Source: Annual Population Survey",
              tags$ul(
                tags$li("1. Figures are for 16-64 year olds.")
              )
            )
    ),
    ### Employment rate over time line chart ----
    fluidRow(
      column(
        width = 6,
        h2("Employment rates: 2017 to 2022"),
        plotlyOutput("EmpRate_time"),
        details(label="Data notes",
          inputId = "empRateSource",
          p("Source: Annual Population Survey",
          tags$ul(
            tags$li("1. Figures are for 16-64 year olds."),
            tags$li("2. Years represent Jul-Jun period. So 2017 is the Jul 2016 – June 2017 period.")
                  )
          )
        )
      ),
      ### Employment percentage by occupation data table ----
      column(
        width = 6,
        h2("Employment share by occupation: Jan-Dec 2021"),
        dataTableOutput("EmpOcc"),
        details(label="Data notes",
                inputId = "empOccSource",
                p("Source: Annual Population Survey",
                  tags$ul(
                    tags$li("1. Figures are for all age groups.")
                  )
                )
        )
      )
    ),

    ### Downloads-------------
    br(),
    fluidRow(
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
        "Download employment data for all geographies (LEPs, LISP areas, LAs, regions and England)",
      )
    ), # end of row
    fluidRow(
      column(
        width = 3,
        downloadButton(
          outputId = "download_btn1b",
          label = "Current LEP/LSIP",
          icon = shiny::icon("download"),
          class = "downloadButton"
        )
      ),
      column(width = 9, p("Download employment data for the selected LEP/LSIP area"))
    ), # end of row
    column(width = 12, br(""))
  ) # end of Local Landscape tab
}

panel_vacancies <- function() {
  tabPanel(
    "Vacancies",
    h1(uiOutput("page3title")),
    p("The data on this page uses experimental online job advert estimates produced by ONS using Adzuna data. "),
    ### KPI boxes ----
    fluidRow(
      valueBoxOutput("jobad.pc"),
      valueBoxOutput("jobad.ch"),
    ), # end of box
    fluidRow(
      uiOutput("vac_comp")
    ),
    details(label="Data notes",
            inputId = "vacKPISource",
            p("Source: Adzuna",
              tags$ul(
                tags$li("1. Figures refer to the average estimates over the month of January in each year.")
              )
            )
    ),
    fluidRow(
      column(
        12,
        ### Online job vacancy units over time line chart ----
        h2("Monthly average units of job adverts: Jan 2017 to Jan 2022"),
        plotlyOutput("jobad.time")
      )
    ), # end of box
    details(label="Data notes",
            inputId = "vacKPISource",
            p("Source: Adzuna",
              tags$ul(
                tags$li("1. Figures refer to the average estimates over the month of January in each year.")
              )
            )
    ),
    fluidRow(
      column(
        12,
        details(
          inputId = "SubsLev",
          label = "Job vacancy data information",
          help_text = "Each time point in the series covers a monthly average of the volume of online job adverts in the month of January for the years 2017 to 2022.
              The monthly average is derived from weekly snapshots in January. The volume of online job adverts is presented as a standardised unit measure. The unit measure is derived by dividing the actual monthly average count of job adverts by a single set value. The job vacancy units can therefore be used to compare between LEPs/LSIPs and over time, but do not represent true job vacancy volumes."
        )
      )
    ),
    ### Downloads-------------
    br(),
    fluidRow(
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
        "Download vacancies data for all geographies (LEPs, LISP areas, LAs, regions and England)",
      )
    ), # end of row
    fluidRow(
      column(
        width = 3,
        downloadButton(
          outputId = "download_btn3b",
          label = "Current LEP/LSIP",
          icon = shiny::icon("download"),
          class = "downloadButton"
        )
      ),
      column(width = 9, p("Download vacancies data for the selected LEP/LSIP area")),
    ), # end of row
    column(width = 12, br(""))
  ) # end of Skills Supply tab
}


panel_skills <- function() {
  tabPanel(
    "Skills",
    h1(uiOutput("page2title")),
    ### KPI boxes ----
    fluidRow(
      valueBoxOutput("skisup.FEach"),
      valueBoxOutput("skisup.APach")
    ),
    fluidRow(
      uiOutput("skill_comp")
    ),
    details(label="Data notes",
            inputId = "FEKPISource",
            p("Source: Individualised Learner Record",
              tags$ul(
                tags$li("1. Total achievements are the count of learners that achieved at any point duding the stated academic period."),
                tags$li("2. Learners achieving more than one course will appear only once in the grand total."),
                tags$li("3. Years shown represent academic years.")
                )
            )
    ),
    ### Achievements over time line chart ----
    fluidRow(
      column(
        12,
        h2("Further education and skills achievements: 2016/17 to 2020/21"),
        div(
          class = "filterRow",
          selectizeInput("skill_line", "Choose type of training",
            choices = c("Apprenticeships (all ages)", "Education and training (adults only)", "Community learning (adults only)", "Total FE and skills provision")
          ),
          br()
        ),
        plotlyOutput("Ach_time"),
        p("Total achievements are the count of learners that achieved at any point during the stated academic period. Learners achieving more than one course will appear only once in the grand total."),
        details(label="Data notes",
                inputId = "FEKPISource",
                p("Source: Individualised Learner Record",
                  tags$ul(
                    tags$li("1. Total achievements are the count of learners that achieved at any point duding the stated academic period."),
                    tags$li("2. Learners achieving more than one course will appear only once in the grand total."),
                    tags$li("3. Years shown represent academic years.")
                  )
                )
        ),
      )
    ),
    ### FE achievements by SSA----
    fluidRow(
      column(
        12,
        h1("Further education and skills achievements by sector subject area: Aug 2021 to Apr 2022 (provisional)"),
        plotlyOutput("Ach_SSA_pc")
      )
    ),
    column(width = 12, br("")), # put in to push below the fixed height chart
    column(width = 12, br("")),
    details(label="Data notes",
            inputId = "FEKPISource",
            p("Source: Individualised Learner Record",
              tags$ul(
                tags$li("1. Total achievements are the count of learners that achieved at any point duding the stated academic period."),
                tags$li("2. Learners achieving more than one course will appear only once in the grand total."),
                tags$li("3. Years shown represent academic years.")
              )
            )
    ),
    ### FE definitions----
    fluidRow(
      column(
        12,
        details(
          inputId = "FEdefs",
          label = "Further education and skills definitions",
          tags$ul(
            tags$li("FE and skills include all age apprenticeships and wider adult (19+) FE learning, such as community learning and education and training."),
            tags$li("Further Education covers publicly-funded learning delivered by an FE institution, a training provider or within a local community. It also includes apprenticeships delivered in the workplace. It does not include higher education, unless delivered as part of an apprenticeship programme."),
            tags$li("Apprenticeships are paid jobs that incorporate on-the-job and off-the-job training leading to nationally recognised qualifications."),
            tags$li("Community learning funds a wide range of non-formal courses (e.g. IT or employability skills) and activity targeted at deprived areas or disadvantaged groups. They can be offered by local authorities, colleges, community groups."),
            tags$li("Education and training is mainly classroom-based adult FE that is not an apprenticeship or community learning."),
            tags$li("Achievements are the number of learners who successfully complete an individual aim in an academic year. ")
          )
        )
      )
    ),
    fluidRow(
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
        "Download skills data for all geographies (LEPs, LISP areas, LAs, regions and England)"
      )
    ), # end of row
    fluidRow(
      column(
        width = 3,
        downloadButton(
          outputId = "download_btn2b",
          label = "Current LEP/LSIP",
          icon = shiny::icon("download"),
          class = "downloadButton"
        )
      ),
      column(width = 9, p("Download skills data for the selected LEP/LSIP area"))
    ), # end of row
    column(width = 12, br(""))
  ) # skills panel
}
