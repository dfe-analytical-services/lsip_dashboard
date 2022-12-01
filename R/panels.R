panel_overview <- function() {
  tabPanel(
    "Overview",
    h1(uiOutput("page0title")),
    p("Change metrics are measured against the same period in the previous year."),
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
              title = "Source: ILR AY21/22",
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
              title = "Source: ILR AY21/22",
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
        "Download all data for all geographies (LEPs, LSIP, MCA areas, LAs, regions and England)",
      )
    ),
    fluidRow(
      column(
        width = 3,
        downloadButton(
          outputId = "download_btn0b",
          label = "Current LEP/LSIP/MCA",
          icon = shiny::icon("download"),
          class = "downloadButton"
        )
      ),
      column(width = 9, "Download all data for the selected geographic area")
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
    details(
      label = "Source: Annual Population Survey",
      inputId = "empKPISource",
      tags$ol(
        tags$li("Figures are for 16-64 year olds.")
      )
    ),
    ### Employment rate over time line chart ----
    fluidRow(
      column(
        width = 6,
        h2("Employment rates: Jul-Jun 2018 to Jul-Jun 2022"),
        plotlyOutput("EmpRate_time"),
        details(
          label = "Source: Annual Population Survey",
          inputId = "empRateSource",
          tags$ol(
            # Is this correct (16-64 year olds)? Should be the same as for occupation table
            tags$li("Figures are for 16-64 year olds."),
            tags$li("Years represent Jul-Jun period. So 2017 is the Jul 2017 – June 2018 period.")
          )
        )
      ),
      ### Employment percentage by occupation data table ----
      column(
        width = 6,
        h2("Employment share by occupation: Jan-Dec 2021"),
        dataTableOutput("EmpOcc"),
        br(),
        details(
          label = "Source: Annual Population Survey",
          inputId = "empOccSource",
          tags$ol(
            # IS this correct (all age groups)? Should be same as employment rate overtime
            tags$li("Figures are for all age groups."),
            tags$li(
              "Standard Occupational Classification 2010 (SOC2010).
                    The ONS have announced that, due to a coding error, their occupational data should be used with caution.
                    For more information see this ONS ",
              a(
                href = "https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/articles/theimpactofmiscodingofoccupationaldatainofficefornationalstatisticssocialsurveysuk/2022-09-26",
                "article",
                .noWS = c("after")
              ), "."
            )
          )
        )
      )
    ),
    fluidRow(
      column(
        12,
        ### employment by industry bar chart ----
        h2("Employment share by industry: Jul 2021 to Jun 2022"),
        plotlyOutput("empind")
      )
    ),
    column(width = 12, br("")), # put in to push below the fixed height chart
    column(width = 12, br("")),
    details(
      label = "Source: Annual Population Survey",
      inputId = "empindSource",
      tags$ol(
        tags$li("Figures are for all age groups."),
        tags$li("Percentages are based on proportion of total employment."),
        tags$li("Employment split by broad industry group Standard Industrial Classification: SIC 2007.")
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
        "Download employment data for all geographies (LEPs, LSIP, MCA areas, LAs, regions and England)",
      )
    ), # end of row
    fluidRow(
      column(
        width = 3,
        downloadButton(
          outputId = "download_btn1b",
          label = "Current LEP/LSIP/MCA",
          icon = shiny::icon("download"),
          class = "downloadButton"
        )
      ),
      column(width = 9, p("Download employment data for the selected geographic area"))
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
    details(
      label = "Source: Adzuna",
      inputId = "vacKPISource",
      tags$ol(
        tags$li("Figures refer to the average estimates over the month of January in each year.")
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
    details(
      label = "Source: Adzuna",
      inputId = "vacKPISource",
      tags$ol(
        tags$li("Figures refer to the average estimates over the month of January in each year.")
      )
    ),
    fluidRow(
      column(
        12,
        details(
          inputId = "SubsLev",
          label = "Job vacancy data information",
          help_text = "Each time point in the series covers a monthly average of the volume of online job adverts in the month of January for the years 2017 to 2022.
              The monthly average is derived from weekly snapshots in January. The volume of online job adverts is presented as a standardised unit measure. The unit measure is derived by dividing the actual monthly average count of job adverts by a single set value. The job vacancy units can therefore be used to compare between areas and over time, but do not represent true job vacancy volumes."
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
        "Download vacancies data for all geographies (LEPs, LSIP, MCA areas, LAs, regions and England)",
      )
    ), # end of row
    fluidRow(
      column(
        width = 3,
        downloadButton(
          outputId = "download_btn3b",
          label = "Current LEP/LSIP/MCA",
          icon = shiny::icon("download"),
          class = "downloadButton"
        )
      ),
      column(width = 9, p("Download vacancies data for the selected geographic area")),
    ), # end of row
    column(width = 12, br(""))
  ) # end of Skills Supply tab
}


panel_skills <- function() {
  tabPanel(
    "Skills",
    h1(uiOutput("page2title")),
    fluidRow(
      column(
        12,
        div(
          class = "filterRow",
          fluidRow(
            column(
              width = 4,
              uiOutput("type_on")
            ),
            column(
              width = 4,
              uiOutput("level_on")
            ),
            column(
              width = 4,
              uiOutput("age_on")
            )
          ),
          fluidRow(
            column(
              width = 4,
              uiOutput("metric_on")
            )
          )
        )
      )
    ),
    br(),
    ### KPI boxes ----
    fluidRow(
      valueBoxOutput("skisup.FEach"),
      valueBoxOutput("skisup.APach"),
      valueBoxOutput("skisup.APach.national")
    ),
    fluidRow(
      uiOutput("skill_comp")
    ),
    details(
      label = "Source: Individualised Learner Record",
      inputId = "FEKPISource",
      tags$ol(
        tags$li("Total achievements is the count of learners that completed their qualification at any point during the stated academic period."),
        tags$li("Learners achieving more than one course will appear only once in the grand total."),
        tags$li("Years shown represent academic years.")
      )
    ),
    ### Achievements over time line chart ----
    # fluidRow(
    #   column(
    #     12,
    #     div(
    #       class = "filterRow",
    #       fluidRow(
    #         column(
    #           4,
    #           selectizeInput("splitLine", "Choose split",
    #                          choices = c("None", "Provision"="typeNeat", "Level"="level_or_type", "Age"="age_group")
    #           )
    #         )
    #       )
    #     )
    #   )
    # ),
    fluidRow(
      column(
        12,
        h2(uiOutput("feLineTitle")),
        plotlyOutput("Ach_time"),
        details(
          label = "Source: Individualised Learner Record",
          inputId = "FEKPISource",
          tags$ol(
            tags$li("Total achievements is the count of learners that completed their qualification at any point during the stated academic period."),
            tags$li("Learners achieving more than one course will appear only once in the grand total."),
            tags$li("Years shown represent academic years.")
          )
        ),
      )
    ),
    ### FE achievements by SSA----
    fluidRow(
      column(
        12,
        div(
          class = "filterRow",
          fluidRow(
            column(
              4,
              selectizeInput("levelBar", "Choose level of training",
                choices = c("Total", "1", "2", "3", "4+", "E", "Not Assigned")
              )
            ),
            column(
              4,
              selectizeInput("sexBar", "Choose gender",
                choices = c("Total", "Female", "Male")
              )
            ),
            column(
              4,
              selectizeInput("metricBar", "Choose metric",
                choices = c("Achievements", "Enrolments")
              )
            )
          )
        )
      )
    ),
    fluidRow(
      column(
        12,
        h2("Further education and skills training by sector subject area: Aug 2021 to Jul 2022"),
      )
    ),
    fluidRow(column(
      12,
      plotlyOutput("Ach_SSA_pc")
    )),
    column(width = 12, br("")), # put in to push below the fixed height chart
    column(width = 12, br("")),
    details(
      label = "Source: Individualised Learner Record",
      inputId = "FEKPISource",
      tags$ol(
        tags$li("'Not Applicable/Not Known' levels includes aims where a qualification either has no level or may be taken at several levels."),
        tags$li("‘Aim enrolments’ are a count of learners starting a course (including component courses) for each academic period.
                Learners are counted for each aim they are studying and so can be counted more than once."),
        tags$li("‘Aim achievements’ are a count of learners completing (passing or certification) a course (including a component course) for each academic year.
                Learners will be counted for each aim they achieve and so can be counted more than once."),
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
            tags$li("Further education and skills include all age apprenticeships and publicly-funded adult (19+) learning, including community learning, delivered by an FE institution, a training provider or within a local community."),
            tags$li("FE and skills does not includer higher education, unless delivered as part of an apprenticeship programme."),
            tags$li("Apprenticeships are paid jobs that incorporate on-the-job and off-the-job training leading to nationally recognised qualifications."),
            tags$li("Community learning funds a wide range of non-formal courses (e.g. IT or employability skills) and activity targeted at deprived areas or disadvantaged groups. They can be offered by local authorities, colleges, community groups."),
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
        "Download skills data for all geographies (LEPs, LSIP, MCA areas, LAs, regions and England)"
      )
    ), # end of row
    fluidRow(
      column(
        width = 3,
        downloadButton(
          outputId = "download_btn2b",
          label = "Current LEP/LSIP/MCA",
          icon = shiny::icon("download"),
          class = "downloadButton"
        )
      ),
      column(width = 9, p("Download skills data for the selected geographic area"))
    ), # end of row
    column(width = 12, br(""))
  ) # skills panel
}

# Qualification level
panel_qualification_level <- function() {
  tabPanel(
    "Qualification level",
    h1(uiOutput("page4title")),
    p("Highest level of qualification by working age population and gender."),
    
    fluidRow(
      column(
        12,
        div(
          class = "filterRow",
          fluidRow(
            column(
              width = 4,
              uiOutput("age_on2")
            ),
            column(
              width = 4,
              uiOutput("qual_on")
            ),
            column(
              width = 4,
              uiOutput("gen_off")
            )
          )
        )
      )
    ),
    
    br(),
    
    ### KPI boxes ----
    fluidRow(
      valueBoxOutput("qualup.nvq3"),
      valueBoxOutput("qualup.nvq4")
    ),
    fluidRow(
      uiOutput("qual_comp")), 
      details(
        label = "Source: Annual Population Survey",
        inputId = "QualSource",
        tags$ol(
          tags$li("Figures are for 16-64 year olds."),
          tags$li("Years represent Jul-Jun period. So 2018 is the Jul 2017 – June 2018 period.")
        )
      ),
    
    
    
    fluidRow(
      column(
        12,
        h2("Qualification Level: 2017 to 2021"),
        plotlyOutput("qual_time"),
        details(
          label = "Source: Annual Population Survey",
          inputId = "QualSource",
          tags$ol(
            tags$li("Figures are for 16-64 year olds."),
            tags$li("Years represent Jul-Jun period. So 2018 is the Jul 2017 – June 2018 period.")
          )
        )
      )
    ),
    
    ### NVQ qualifications definitions----
    fluidRow(
      column(
        12,
        details(
          inputId = "NVQdefs",
          label = "NVQ qualification definitions",
          tags$ul(
            tags$li("NVQ1: 1-4 O Levels/CSE/GCSEs (any grades), Entry Level, Foundation Diploma, NVQ level 1, Foundation GNVQ, Basic/Essential Skills"),
            tags$li("NVQ2: 5+ O Level (Passes)/CSEs (Grade 1)/GCSEs (Grades A*-C), School Certificate, 1 A Level/ 2-3 AS Levels/VCEs, Intermediate/Higher Diploma, Welsh Baccalaureate Intermediate Diploma, NVQ level 2, Intermediate GNVQ, City and Guilds Craft, BTEC First/General Diploma, RSA Diploma"),
            tags$li("Trade Apprenticeships: Apprenticeships"),
            tags$li("NVQ3: 2+ A Levels/VCEs, 4+ AS Levels, Higher School Certificate, Progression/Advanced Diploma, Welsh Baccalaureate Advanced Diploma, NVQ Level 3; Advanced GNVQ, City and Guilds Advanced Craft, ONC, OND, BTEC National, RSA Advanced Diploma"),
            tags$li("NVQ4 or above: Degree (for example BA, BSc), Higher Degree (for example MA, PhD, PGCE), NVQ Level 4-5, HNC, HND, RSA Higher Diploma, BTEC Higher level, Foundation degree (NI), Professional qualifications (for example teaching, nursing, accountancy)"),
            tags$li("Other qualifications: Other vocational/work-related Qualifications, Foreign Qualifications (Not Stated / level unknown)"),
            tags$li("None: No qualifications")
          )
        )
      )
    ),
    
    fluidRow(
      column(
        width = 3,
        downloadButton(
          outputId = "download_btn4a",
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
          outputId = "download_btn4b",
          label = "Current LEP/LSIP",
          icon = shiny::icon("download"),
          class = "downloadButton"
        )
      ),
      column(width = 9, p("Download skills data for the selected LEP/LSIP area"))
    ), # end of row
    column(width = 12, br(""))
    
  ) # end of qualification tab
}  
  
  # destinations
  panel_destinations <- function() {
    tabPanel(
      "Destinations",
      h1(uiOutput("page5title")),
      p("Destination measures show the percentage of students going to or remaining in an education, apprenticeship or employment destination in the academic year after completing 14 to 16 studies (finishing year 11, usually aged 16) and 16 to 18 studies (finishing year 13, usually aged 18). "),
      
      fluidRow(
        column(
          12,
          div(
            class = "filterRow",
            fluidRow(
              column(
                width = 4,
                uiOutput("key_stage_on")
              ),
              column(
                width = 4,
                uiOutput("cohort_group_on")
              )
              )
            )
          )
      ),
      
      br(),
      
      ### KPI boxes ----
      fluidRow(
        valueBoxOutput("destup.ks4edu"),
        valueBoxOutput("destup.ks5edu"),
        valueBoxOutput("destup.ks5emp")),
      
      fluidRow(
        uiOutput("dest_comp")),
      
      details(
        label = "Source: National Pupil Database",
        inputId = "destSource",
        tags$ol(
          tags$li("Add in")
        )
      ),
      
      br(),
      
      #Key stage bar chart
      fluidRow(
        column(
          12,
          h2(uiOutput("keystagetitle")),
          plotlyOutput("key_stage_2021"))),
      column(width = 12, br("")), # put in to push below the fixed height chart
      column(width = 12, br("")),
      details(
        label = "Source: Nationl Pupil Database",
        inputId = "Key stage source",
        tags$ol(
          tags$li("Add in")
        )
      ),
      
      ### sustained destination definitions----
      fluidRow(
        column(
          12,
          details(
            inputId = "sustdef",
            label = "sustained destination definitions",
            tags$ul(
              tags$li("A sustained destination is a count of young people recorded as having sustained participation (education and employment) for a 6 month period in the destination year."),
              tags$li("This means attending for all of the first two terms of the academic year (October 2020 to March 2021) at one or more education providers; spending 5 of the 6 months in employment or a combination of the two."),
              tags$li("A sustained apprenticeship is recorded when 6 months continuous participation is recorded at any point in the destination year (between August 2020 and July 2021).")
            )
          )
        )
      ),
      
      fluidRow(
        column(
          width = 3,
          downloadButton(
            outputId = "download_btn5a",
            label = "All data   ",
            icon = shiny::icon("download"),
            class = "downloadButton"
          )
        ),
        column(
          width = 9,
          "Download skills data for all geographies (LEPs, LSIP areas, MCA areas LAs, regions and England)"
        )
      ), # end of row
      fluidRow(
        column(
          width = 3,
          downloadButton(
            outputId = "download_btn5b",
            label = "Current LEP/LSIP/MCA",
            icon = shiny::icon("download"),
            class = "downloadButton"
          )
        ),
        column(width = 9, p("Download skills data for the selected LEP/LSIP area"))
      ), # end of row
      column(width = 12, br(""))
    ) #end of destinations tab
}

