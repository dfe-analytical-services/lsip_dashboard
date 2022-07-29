fluidPage(
  shinyjs::useShinyjs(),
  includeCSS("www/dfe_shiny_gov_style.css"),
  title = "DfE Local Skills Dashboard",
  # use_tota11y(), # accessibility layer for local testing

  # Set metadata for browser ==================================================

  tags$html(lang = "en"),
  # meta_general(
  #   application_name = "DfE Analytical Services R-Shiny Template",
  #   description = "R-Shiny template for use by DfE external data dashboards",
  #   robots = "index,follow",
  #   generator = "R-Shiny",
  #   subject = "Education data dashboards",
  #   rating = "General",
  #   referrer = "no-referrer"
  # ),

  # Set title for search engines
  HTML("<title>Local Skills Dashboard</title>"),

  # Navbar ====================================================================
  navbarPage("",
    id = "navbar",

    # HOMEPAGE ============================================================

    tabPanel(
      "Homepage",
      fluidPage(
        fluidRow(
          column(
            12,
            h1("Local Skills Dashboard"),
            p("This dashboard brings together published data on local employment and skills. It provides a consistent baseline assessment of local skills landscapes for use by local actors."),
            p("It aims to support employer bodies in developing Local Skills Improvement Plans."),
            p("The dashboard displays published data from a variety of sources (APS, ILR and ONS online job vacancies) at Local Enterprise Partnership (LEP) geography. Trends can be compared between different LEPs and over time for some indicators. The underlying data contains national, regional, LEP and LA data and can be downloaded directly from the dashboard."),
            br(),
            "This dashboard has been produced to support the aims of the ",
            a(
              href = "https://www.gov.uk/government/groups/unit-for-future-skills",
              "Unit for Future Skills",
              .noWS = c("after")
            ),
            ".",
            box(
              width = 12,
              p(" ")
            )
          ),

          ## Left panel -------------------------------------------------------
          column(
            6,
            div(
              div(
                class = "panel panel-info",
                div(
                  class = "panel-heading",
                  style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;",
                  h2("Contents")
                ),
                div(
                  class = "panel-body",
                  tags$div(
                    h2("Dashboard"),
                    p("This page contains the different dashboard tabs."),
                    h3(actionLink("link_to_tabpanel_overview", "Overview")),
                    p("This tab provides a summary of employment and skills metrics at LEP level. It displays employment volume, employment rate, proportion of online vacancies, adult FE and apprenticeship achievement volumes. It shows year-on-year change for each indicator."),
                    p("The download buttons download all indicators for the selected LEP or for all available geographies (England, region, LEP, LA)."),
                    # h2("Labour market"),
                    h3(actionLink("link_to_tabpanel_employment", "Employment")),
                    p("This tab contains employment indicators at LEP level. These can be compared to England and a comparator LEP."),
                    p("The line chart shows employment rate over time for the chosen LEP. The table displays employment distribution by occupation (sub-major SOC group)."),
                    p("The download buttons download employment indicators for the selected LEP or for all available geographies (England, region, LEP, LA)."),
                    h3(actionLink("link_to_tabpanel_vacancies", "Vacancies")),
                    p("This tab contains online job vacancies indicators at LEP level. These can be compared to England and a comparator LEP."),
                    p("The line chart shows change in online job vacancy units over time. Units are not real volumes but represent a fixed number of job adverts to be used for comparisons over time and between areas."),
                    p("The download buttons download vacancy indicators for the selected LEP or for all available geographies (England, region, LEP, LA)."),
                    # h2("Skills landscape"),
                    h3(actionLink("link_to_tabpanel_FE", "FE")),
                    p("This tab provides a summary of further education (FE) and skills statistics at LEP level. These can be compared to another LEP."),
                    p("The line chart shows achievement volumes over time for apprenticeships, education & training and community learning."),
                    p("The bar chart displays distribution of FE and apprenticeships achievements by SSA for the latest available period."),
                    p("The download buttons download FE and skills indicators for the selected LEP or for all available geographies (England, region, LEP, LA)."),
                    h3("Future development"),
                    p("This page summarises the improvements we plan to make for the next release (October 2022). These include:"),
                    tags$li("Integrating more data sources (e.g. ASHE earnings data, BRES enterprise data, HE learner data, OBSM learner outcomes);"),
                    tags$li("Providing more data breakdowns (e.g. employment by 4-digit SOC, qualification by subject and level, apprenticeships by enterprise size and sector);"),
                    tags$li("Adding new features, like the ability to choose the geography (LA, LEP, Region) and view data for one or multiple areas, combined or side-by-side."),
                    br(),
                    "If you have any feedback or suggestions for improvement, please contact us at ",
                    a(href = "mailto:ufs.contact@education.gov.uk", "ufs.contact@education.gov.uk",.noWS = c("after")), "."
                  ),
                  br()
                )
              )
            ),
          ), # end of left panel

          ## Right panel ------------------------------------------------------
          column(
            6,
            div(
              div(
                class = "panel panel-info",
                div(
                  class = "panel-heading",
                  style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;",
                  h2("Data sources")
                ),
                div(
                  class = "panel-body",
                  h2("Labour market"),
                  h3("Annual Population Survey (APS)"),
                  p("A continuous household survey covering the UK.
                             The dashboard uses employment volumes and rates for each LEP and split by sub-major SOC 2010 grouping.
                             The data are for interviews conducted over the calendar year (2017-2021 are shown).
                             "),
                  a(
                    href = "https://www.nomisweb.co.uk/datasets/apsnew",
                    "APS data on Nomis",
                    .noWS = c("after")
                  ),
                  br(),
                  h3("ONS Online Job Adverts"),
                  p("These data tables are created based upon online job adverts data provided by Adzuna.
                             Each time point in the series covers a monthly average of the volume of online job adverts in the month of January for the years 2017 to 2022. The monthly average is derived from weekly snapshots in January.
                             The dashboard shows job 'units' which is the number of job adverts divided a set value for all regions. It is therefore not an indication of the real volume of job adverts, but can be used in comparisons across regions or to follow trends over time."),
                  a(
                    href = "https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/datasets/onlinejobadvertsbyitl1regionandlocalauthority",
                    "Job adverts data on ONS",
                    .noWS = c("after")
                  ),
                  br(),
                  h2("Skills landscape"),
                  h3("Individualised Learner Records (ILR)"),
                  p("The ILR is an on-going collection of data about learners from training providers in the Further Education (FE) and Skills sector in England.
                             The dashboard uses FE learner achievements over time (AY1617-21/22 (temporary data to October for 21/22)) split by apprenticeships, community learning, education and training.
                             The data is taken from the FE and Skills official statistics."),
                  a(
                    href = "https://explore-education-statistics.service.gov.uk/find-statistics/further-education-and-skills",
                    "ILR data on EES",
                    .noWS = c("after","before")
                  )
                )
              )
            )
          ), # end of right panel
        ) # end of FluidRow
      ) # end of FluidPage
    ), # end of Tab Panel

    # APP ----
    tabPanel(
      "Dashboard",
      # choice row
      sidebarLayout(
        sidebarPanel(
          width = 2,
          selectInput("lep1", "Choose primary LEP area",
            choices = C_LEP2020
          ),
          # lep 2 is reactve to lep 1 so is populated in the server
          selectInput("lep2", "Choose comparison LEP area",
            choices = NULL
          )
        ),

        # next row is the data tabs
        mainPanel(
          width = 10,
          tabsetPanel(
            id = "datatabset",

            # OVERVIEW ----
            tabPanel(
              "Overview",
              ## Main panel ----
              box(
                width = 12,
                uiOutput("page0title", style = "font-size: 24px;"),
                div("Change metrics are measured since the same period the year before.", style = "font-size: 16px; font-style: italic;"),
                br(),
                fluidRow(
                  # left column
                  column(
                    width = 5,
                    style = "background-color:#f3f2f1;",
                    h2("Labour market"),
                    div(
                      title = "Source: APS. 2021 calendar year",
                      uiOutput("locland.emplcnt0"),
                      p("people employed in 2021")
                    ),
                    box(
                      width = 12,
                      p(" ")
                    ),
                    # second row - emp rate
                    div(
                      title = "Source: APS. 2021 calendar year",
                      uiOutput("locland.emplrate0"),
                      uiOutput("locland.emplRateSub") # subtitle compared to England
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
                    div(
                      title = "Source: ONS (Adzuna). Jan 2022",
                      uiOutput("jobad.units"),
                      p("of online vacancies in England (Jan 2022)")
                    ),
                    # fifth row - link to vacancy data
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
                    div(
                      title = "Source: ILR AY20/21", # tooltip
                      uiOutput("skisup.ETach"),
                      p("education and training achievements (AY20/21)")
                    ),
                    box(
                      width = 12,
                      p(" ")
                    ),
                    # 5th row - apps
                    div(
                      title = "Source: ILR AY20/21",
                      uiOutput("skisup.APPach"),
                      p("apprenticeship achievements (AY20/21)")
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
                    "Download all available indicators for all geographies (LEPs, LAs, Regions and England)",
                  )
                ),
                fluidRow(
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
                box(
                  width = 12,
                  p(" ")
                )
              ) # end of main pane box
            ), # end of Overview tab

            # EMPLOYMENT  ----
            tabPanel(
              "Employment",
              ## Main panel ----
              box(
                width = 12,
                uiOutput("page1title", style = "font-size: 24px;"),
                div("Data is from the Annual Population Survey. Years represent calendar years.", style = "font-size: 16px; font-style: italic;"),
                br(),

                ### KPI boxes ----
                box(
                  width = 12,
                  valueBoxOutput("locland.emplcnt"),
                  valueBoxOutput("locland.emplrate")
                ),
                box(
                  width = 12,
                  uiOutput("emp_comp")
                ),
                box(
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
                ### Downloads-------------
                box(
                  width = 12,
                  p(" ")
                ),
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
                    "Download employment indicators for all geographies (LEPs, LAs, Regions and England)",
                  )
                ), # end of row
                fluidRow(
                  column(
                    width = 3,
                    downloadButton(
                      outputId = "download_btn1b",
                      label = "Current LEP",
                      icon = shiny::icon("download"),
                      class = "downloadButton"
                    )
                  ),
                  column(width = 9, p("Or just for the currently chosen LEP")),
                  box(
                    width = 12,
                    p(" ")
                  )
                ) # end of row
              ) # end of main panel
            ), # end of Local Landscape tab

            # VACANCIES ---------------
            tabPanel(
              "Vacancies",
              ## Main panel ----
              box(
                width = 12,
                uiOutput("page3title", style = "font-size: 24px;"),
                div("Data is from ONS using Adzuna online job adverts. Data for each year is the average of vacancies across January of that year.", style = "font-size: 16px; font-style: italic;"),
                br(),
                ### KPI boxes ----
                box(
                  width = 12,
                  valueBoxOutput("jobad.pc"),
                  valueBoxOutput("jobad.ch"),
                ), # end of box
                box(
                  width = 12,
                  uiOutput("vac_comp")
                ),
                box(
                  width = 12,
                  p(" ")
                ),
                box(
                  width = 12,
                  ### Online job vacancy units over time line chart ----
                  column(
                    width = 12,
                    p("Online job vacancy unit trend", style = "font-size:20px;"),
                    plotlyOutput("jobad.time")
                  ),
                ), # end of box
                details(
                  inputId = "SubsLev",
                  label = "Chart information",
                  help_text = "Each time point in the series covers a monthly average of the volume of online job adverts in the month of January for the years 2017 to 2022.
              The monthly average is derived from weekly snapshots in January. The volume of online job adverts is presented as a standardised unit measure. The unit measure is derived by dividing the actual monthly average count of job adverts by a single set value. The job vacancy units can therefore be used to compare between LEPS and over time, but do not represent true job vacancy volumes."
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
                    "Download vacancy indicators for all geographies (LEPs, LAs, Regions and England)",
                  )
                ), # end of row
                fluidRow(
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
                  box(
                    width = 12,
                    p(" ")
                  )
                ) # end of row
              ) # end of main panel
            ), # end of Skills Supply tab
            # FE ----
            tabPanel(
              "FE",
              # Main panel
              box(
                width = 12,
                uiOutput("page2title", style = "font-size: 24px;"),
                div("Data from Individualised Learner Records for FE learners. Years shown are academic years. All figures are for 16-64 except the occupation split bar chart which is all ages.", style = "font-size: 16px; font-style: italic;"),
                br(),
                ### KPI boxes ----
                box(
                  width = 12,
                  valueBoxOutput("skisup.FEach"),
                  valueBoxOutput("skisup.APach"),
                ),
                box(
                  width = 12,
                  uiOutput("skill_comp")
                ),
                box(
                  width = 12,
                  p(" ")
                ),
                box(
                  width = 12,
                  ### Achievements over time line chart ----
                  column(
                    width = 6,
                    p("FE and apprenticehips learner achievement trend", style = "font-size:20px;"),
                    p("Choose provision group:"),
                    selectizeInput("skill_line", NULL,
                      choices = c("Apprenticeships (all ages)", "Education and training (adults only)", "Community learning (adults only)", "Total FE and Apps provision")
                    ),
                    plotlyOutput("Ach_time"),
                    p("Total achievements are the count of learners that achieved at any point during the stated academic period. Learners achieving more than one course will appear only once in the grand total.", style = "font-style: italic;")
                  ),
                  ### FE achievements by SSA----
                  column(
                    width = 6,
                    p("All FE learner achievements by SSA tier 1 (AY21/22 Aug to Jan)", style = "font-size:20px;"),
                    plotlyOutput("Ach_SSA_pc")
                  )
                ), # end of charts box
                ### FE definitions----
                details(
                      inputId = "FEdefs",
                      label = "FE definitions",
                    p(
                      tags$li("FE and skills include all age apprenticeships and wider adult (19+) FE learning, such as community learning and education and training."),
                    tags$li("Further Education covers publicly-funded learning delivered by an FE institution, a training provider or within a local community. It also includes apprenticeships delivered in the workplace. It does not include higher education, unless delivered as part of an apprenticeship programme."),
                    tags$li("Apprenticeships are paid jobs that incorporate on-the-job and off-the-job training leading to nationally recognised qualifications."),
                    tags$li("Community learning funds a wide range of non-formal courses (e.g. IT or employability skills) and activity targeted at deprived areas or disadvantaged groups. They can be offered by local authorities, colleges, community groups."),
                    tags$li("Education and training   is mainly classroom-based adult FE that is not an apprenticeship or community learning."),
                    tags$li("Achievements are the number of learners who successfully complete an individual aim in an academic year. "),
                    )
                ),
                ### Downloads-------------
                box(
                  width = 12,
                  p(" ")
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
                    "Download FE indicators for all geographies (LEPs, LAs, Regions and England)",
                  )
                ), # end of row
                fluidRow(
                  column(
                    width = 3,
                    downloadButton(
                      outputId = "download_btn2b",
                      label = "Current LEP",
                      icon = shiny::icon("download"),
                      class = "downloadButton"
                    )
                  ),
                  column(width = 9, p("Or just for the currently chosen LEP")),
                  box(
                    width = 12,
                    p(" ")
                  )
                ) # end of row
              ) # end of main panel
            ) # FE panel
          ) # end of dashboard tabset panel
        ) # end of dashboard navbar
      ) # end of app data row
    ), # end of app tab panel
    
    # Create the accessibility statement-----------------
    tabPanel(
      "Accessibility",
      h2("Accessibility statement"),
      br("This accessibility statement applies to the Local Skills dashboard.
            This dashboard is run by the Department for Education. We want as many people as possible to be able to use this application,
            and have actively developed this dashboard with accessibilty in mind."),
      h3("WCAG 2.1 compliance"),
      br("We follow the reccomendations of the ", a(href = "https://www.w3.org/TR/WCAG21/", "WCAG 2.1 requirements. "), "This application has been checked using the ", a(href = "https://github.com/ewenme/shinya11y", "Shinya11y tool "), ", which did not detect accessibility issues.
             This dashboard also fully passes the accessibility audits checked by the ", a(href = "https://developers.google.com/web/tools/lighthouse", "Google Developer Lighthouse tool"), ". This means that this dashboard:"),
      tags$div(tags$ul(
        tags$li("uses colours that have sufficient contrast"),
        tags$li("allows you to zoom in up to 300% without the text spilling off the screen"),
        tags$li("has its performance regularly monitored, with a team working on any feedback to improve accessibility for all users")
      )),
      h3("Limitations"),
      br("We recognise that there are still potential issues with accessibility in this dashboard, but we will continue
             to review updates to technology available to us to keep improving accessibility for all of our users. For example, these
            are known issues that we will continue to monitor and improve:"),
      tags$div(tags$ul(
        tags$li("List"),
        tags$li("known"),
        tags$li("limitations, e.g."),
        tags$li("Alternative text in interactive charts is limited to titles and could be more descriptive (although this data is available in csv format)")
      )),
      h3("Feedback"),
      br(
        "If you have any feedback on how we could further improve the accessibility of this dashboard, please contact us at",
        a(href = "mailto:statistics.development@education.gov.uk", "statistics.development@education.gov.uk")
      ),
      br()
    ), # End of accessibility tab
    # Support links ===========================================================
    
    tabPanel(
      "Support and feedback",
      support_links() # defined in R/supporting_links.R
    ),
    
    # Footer ====================================================================
    
    shinyGovstyle::footer(TRUE)
  ) # End of navBarPage
)
    
    # FUTURE DEVELOPMENT ---------------
    #   navbarMenu(
    #    "Future development",
    # tabPanel(
    #   "Future development",
    #   mainPanel(
    #     width = 12,
    #     p("We will be seeking feedback on this version 1 of the local skills dashboard to inform our version 2.
    #         We do have a number of improvements we will implement in version 2:  "),
    #     br(),
    #     p("Overview page", style = "font-size:20px;"),
    #     p("V2 will include a number of key indicators to this page including: average salary, FE skill level, and HE data. "),
    #     p("V2 will also include the functionality to filter the indicators by sector or occupation. "),
    #     p("V2 overview page mockup:", style = "font-style: italic"),
    #     img(src = "OverviewV2.png"),
    #     br(),
    #     p("Apprenticeships page", style = "font-size:20px;"),
    #     p("V2 will show:"),
    #     p("Starts and achievements timeseries."),
    #     p("Distribution by level, SSA, enterprise size, and enterprise sector."),
    #     p("Most common apprenticeships within selected provision, level, SSA, sector."),
    #     p("Information about which providers/employers deliver these apprenticeships."),
    #     br(),
    #     p("HE page", style = "font-size:20px;"),
    #     p("V2 will show HE participants and qualifiers by subject."),
    #     br(),
    #     p("FE page", style = "font-size:20px;"),
    #     p("V2 will show learner outcomes by type of provision."),
    #   )
    # ), # enedx of future development
    #   # Overview ---------------
    #   tabPanel(
    #     "Overview v2",
    #     sidebarLayout(
    #       ## Side panel ----
    #       sidebarPanel(
    #         width = 2,
    #         br(),
    #         ### LEP 1 input ---------------
    #         selectizeInput("lepOver2",
    #           "Choose the area(s) you want to look at",
    #           choices = C_LEP2020,
    #           selected = "England" # ,multiple=TRUE
    #         ),
    #         radioButtons("OccSect",
    #           "Do you want to look at a particular sector or occupation group?",
    #           choices = list(
    #             "Occupation" = 1,
    #             "Sector" = 2
    #           ),
    #           selected = 1
    #         ),
    #         selectizeInput("Occ-Sector",
    #           "Choose sector/occupation",
    #           choices = c("Sector 1", "Sector 2", "...")
    #         ),
    #         ### Help text --------------------
    #         helpText("Download all available indicators for all geographies (LEPs, LAs, Regions and England):",
    #           style = "font-style: italic;"
    #         ),
    #         ### Download button -------------
    #         downloadButton(
    #           outputId = "download_btnOver2a",
    #           label = "All data",
    #           icon = icon("download")
    #         ),
    #         helpText("Or just for the currently chosen LEP:",
    #           style = "font-style: italic;"
    #         ),
    #         downloadButton(
    #           outputId = "download_btnOver2b",
    #           label = "Current LEP",
    #           icon = icon("download")
    #         )
    #       ), # end of side panel
    #       ## Main panel ----
    #       mainPanel(
    #         width = 10,
    #         ### Title ----
    #         uiOutput("page6title", style = "font-size: 24px;"),
    #         div("Change measured since the same time year", style = "font-size: 16px; font-style: italic;"),
    #         br(),
    #         fluidRow(
    #           # left column
    #           column(
    #             width = 6,
    #             div(
    #               div(
    #                 class = "panel panel-info",
    #                 div(
    #                   class = "panel-heading",
    #                   style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;text-align:center",
    #                   h2("Labour market")
    #                 ),
    #                 div(
    #                   class = "panel-body",
    #                   # first row - emp vol
    #                   tags$div(
    #                     title = "Source: APS. 2021 calendar year",
    #                     valueBoxOutput("locland.emplcntOver2", width = 8),
    #                     valueBoxOutput("locland.emplcntchangeOver2", width = 4)
    #                   ),
    #                   box(
    #                     width = 12,
    #                     p(" ")
    #                   ),
    #                   # second row - emp rate
    #                   tags$div(
    #                     title = "Source: APS. 2021 calendar year",
    #                     valueBoxOutput("locland.emplrateOver2", width = 8),
    #                     valueBoxOutput("locland.emplchangeOver2", width = 4)
    #                   ),
    #                   box(
    #                     width = 12,
    #                     actionLink("link_to_tabpanel_employment3", "Find out more about employment"),
    #                     align = "right"
    #                   ),
    #                   # third row - link to emp tab
    #                   # fourth row - vacancies
    #                   tags$div(
    #                     title = "Source: ONS (Adzuna). Jan 2022",
    #                     valueBoxOutput("jobad.unitsOver2", width = 8),
    #                     valueBoxOutput("jobad.changeOver2", width = 4)
    #                   ),
    #                   # fifth row - link to vacancy data
    #                   box(
    #                     width = 12,
    #                     actionLink("link_to_tabpanel_vacancies3", "Find out more about vacancies"),
    #                     align = "right"
    #                   ),
    #                   tags$div(
    #                     # title="Source: ?????",
    #                     valueBoxOutput("earn.avgOver2", width = 8),
    #                     valueBoxOutput("earn.changeOver2", width = 4)
    #                   ),
    #                   box(
    #                     width = 12,
    #                     actionLink("link_to_tabpanel_earnings3", "Find out more about earnings"),
    #                     align = "right"
    #                   )
    #                 )
    #               )
    #             ),
    #           ),
    #           # right column
    #           column(
    #             width = 6,
    #             div(
    #               div(
    #                 class = "panel panel-info",
    #                 div(
    #                   class = "panel-heading",
    #                   style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;text-align:center",
    #                   h2("Skills landscape")
    #                 ),
    #                 div(
    #                   class = "panel-body",
    #                   # first row - level 4 starts
    #                   tags$div(
    #                     # title="Source:?",
    #                     valueBoxOutput("skills.l4Over2", width = 8),
    #                     valueBoxOutput("skills.l4changeOver2", width = 4)
    #                   ),
    #                   box(
    #                     width = 12,
    #                     p(" ")
    #                   ),
    #                   # 3rd row - E&T
    #                   tags$div(
    #                     title = "Source:ILR AY20/21",
    #                     valueBoxOutput("skisup.ETachOver2", width = 8),
    #                     valueBoxOutput("skisup.ETachChangeOver2", width = 4)
    #                   ),
    #                   box(
    #                     width = 12,
    #                     p(" ")
    #                   ),
    #                   # 5th row - apps
    #                   tags$div(
    #                     title = "Source:ILR AY20/21",
    #                     valueBoxOutput("skisup.APPachOver2", width = 8),
    #                     valueBoxOutput("skisup.APPachChangeOver2", width = 4)
    #                   ),
    #                   # 6th row - link to app data
    #                   box(
    #                     width = 12,
    #                     actionLink("link_to_tabpanel_FE3", "Find out more about skills"),
    #                     align = "right"
    #                   ),
    #                   # 7th row - HE
    #                   tags$div(
    #                     # title="Source:?",
    #                     valueBoxOutput("he.entrantsOver2", width = 8),
    #                     valueBoxOutput("he.entrantschangeOver2", width = 4)
    #                   ),
    #                   # 8th row - link to HE data
    #                   box(
    #                     width = 12,
    #                     actionLink("link_to_tabpanel_HE3", "Find out more about HE"),
    #                     align = "right"
    #                   ),
    #                 )
    #               )
    #             ),
    #           ),
    #         )
    #       )
    #     ) # end of side bar layout
    #   ), # end of Overview tab
    #
    #   # APPS ---------------
    #   tabPanel(
    #     "Apprenticeships",
    #     sidebarLayout(
    #       ## Side panel ----
    #       sidebarPanel(
    #         width = 2,
    #         ### Help text --------------------
    #         helpText("Choose a Local Area to view outcome trends",
    #           style = "font-style: italic;"
    #         ),
    #         br(),
    #         ### LEP 7 input ---------------
    #         selectizeInput("lepAppa",
    #           "Choose a primary LEP:",
    #           choices = C_LEP2020,
    #           selected = "England",
    #         ),
    #         ### LEP 8 input ------------
    #         selectizeInput("lepAppb", # Make no selection an option
    #           "Choose a comparison LEP (optional):",
    #           choices = c("\nNone", unique(C_LEP2020))
    #         ),
    #         ### Help text --------------------
    #         helpText("Download apprenticeship indicators for all geographies (LEPs, LAs, Regions and England):",
    #           style = "font-style: italic;"
    #         ),
    #         ### Download buttons -------------
    #         downloadButton(
    #           outputId = "download_btnAppa",
    #           label = "All data",
    #           icon = icon("download")
    #         ),
    #         helpText("Or just for the currently chosen LEP:",
    #           style = "font-style: italic;"
    #         ),
    #         downloadButton(
    #           outputId = "download_btnAppb",
    #           label = "Current LEP",
    #           icon = icon("download")
    #         ),
    #       ), # end of side panel
    #       ## Main panel ----
    #       mainPanel(
    #         width = 10,
    #         ### Title ----
    #         uiOutput("page7title", style = "font-size: 24px;"),
    #         br(),
    #         p("We will show:"), br(),
    #         p("Starts and achievements timeseries."), br(),
    #         p("Distribution by level, SSA, enterprise size, and enterprise sector."), br(),
    #         p("Most common apprenticeships within selected provision, level, SSA, sector."), br(),
    #         p("Information about which providers/employers deliver these apprenticeships."),
    #       ) # end of main panel
    #     ) # end of side bar layout
    #   ), # end of earnings tab
    #
    #   # HE ---------------
    #   tabPanel(
    #     "HE",
    #     sidebarLayout(
    #       ## Side panel ----
    #       sidebarPanel(
    #         width = 2,
    #         ### Help text --------------------
    #         helpText("Choose a Local Area to view skill demand trends",
    #           style = "font-style: italic;"
    #         ),
    #         br(),
    #         ### LEP 9 input ---------------
    #         selectizeInput("lep9",
    #           "Choose a primary LEP:",
    #           choices = C_LEP2020,
    #           selected = "England",
    #         ),
    #         ### LEP 10 input ------------
    #         selectizeInput("lep10", # Make no selection an option
    #           "Choose a comparison LEP (optional):",
    #           choices = c("\nNone", unique(C_LEP2020))
    #         ),
    #         ### Help text --------------------
    #         helpText("Download HE indicators for all geographies (LEPs, LAs, Regions and England):",
    #           style = "font-style: italic;"
    #         ),
    #         ### Download buttons -------------
    #         downloadButton(
    #           outputId = "download_btn5a",
    #           label = "All data",
    #           icon = icon("download")
    #         ),
    #         helpText("Or just for the currently chosen LEP:",
    #           style = "font-style: italic;"
    #         ),
    #         downloadButton(
    #           outputId = "download_btn5b",
    #           label = "Current LEP",
    #           icon = icon("download")
    #         ),
    #       ), # end of side panel
    #       ## Main panel ----
    #       # Show a plot of the generated distribution
    #       mainPanel(
    #         width = 10,
    #         ### Title ----
    #         uiOutput("page5title", style = "font-size: 24px;"),
    #         br(),
    #         p("Page will show HE participants and qualifiers by subject"),
    #       ) # end of main panel
    #     ) # end of side bar layout
    #   ), # end of HE tab
    #   # OUTCOMES ---------------
    #   tabPanel(
    #     "Outcomes",
    #     sidebarLayout(
    #       ## Side panel ----
    #       sidebarPanel(
    #         width = 2,
    #         ### Help text --------------------
    #         helpText("Choose a Local Area to view outcome trends",
    #           style = "font-style: italic;"
    #         ),
    #         br(),
    #         ### LEP 7 input ---------------
    #         selectizeInput("lep7",
    #           "Choose a primary LEP:",
    #           choices = C_LEP2020,
    #           selected = "England",
    #         ),
    #         ### LEP 8 input ------------
    #         selectizeInput("lep8", # Make no selection an option
    #           "Choose a comparison LEP (optional):",
    #           choices = c("\nNone", unique(C_LEP2020))
    #         ),
    #         ### Help text --------------------
    #         helpText("Download outcome indicators for all geographies (LEPs, LAs, Regions and England):",
    #           style = "font-style: italic;"
    #         ),
    #         ### Download buttons -------------
    #         downloadButton(
    #           outputId = "download_btn4a",
    #           label = "All data",
    #           icon = icon("download")
    #         ),
    #         helpText("Or just for the currently chosen LEP:",
    #           style = "font-style: italic;"
    #         ),
    #         downloadButton(
    #           outputId = "download_btn4b",
    #           label = "Current LEP",
    #           icon = icon("download")
    #         ),
    #       ), # end of side panel
    #       ## Main panel ----
    #       mainPanel(
    #         width = 10,
    #         ### Title ----
    #         uiOutput("page4title", style = "font-size: 24px;"), br(),
    #         p("Page will show learner outcomes by type of provision"),
    #       ) # end of main panel
    #     ) # end of side bar layout
    #   ), # end of earnings tab
    # ), # end of future development nav bar


  
