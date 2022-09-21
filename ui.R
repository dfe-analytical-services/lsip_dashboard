fluidPage(
  title = tags$head(tags$link(
    rel = "shortcut icon",
    href = "dfefavicon.png"
  )),
  shinyjs::useShinyjs(),
  useShinydashboard(),
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "dfe_shiny_gov_style.css"
    )
  ),
  # use_tota11y(), # accessibility layer for local testing

  # Set metadata for browser ==================================================

  tags$html(lang = "en"),
  tags$head(
    tags$meta(name="application_name", content="Unit for Future Skills - Local Skills Dashboard"),
    tags$meta(name="description", content="Data dashboard presenting Local Skills data from the Unit for Future Skills in the Department for Education."),
    tags$meta(name="subject", content="Education data dashboards.")
  ),
  # Set title for search engines
  HTML("<title>Local Skills Dashboard</title>"),
  tags$head(includeHTML(("google-analytics.html"))),
  shinyGovstyle::header(
    main_text = "DfE",
    main_link = "https://www.gov.uk/government/organisations/department-for-education",
    secondary_text = "Unit for Future Skills - Local Skills Dashboard",
    logo = "images/DfE_logo.png"
  ),
  shinyGovstyle::banner(
    "beta banner",
    "beta",
    paste0(
      "Please be aware that you may experience performance issues and the dashboard may require a reload. We are working to fix this."
    )
  ),
  
  # Navbar ====================================================================
  navlistPanel(
    id = "navbar",
    widths = c(2, 8),
    well = FALSE,
    
    # HOMEPAGE ============================================================

    tabPanel(
      "Homepage",
          column(
            12,
            h1("Local Skills Dashboard"),
            p("Prototype dashboard showing statistics on local employment and skills in England, to support local skills planning and delivery (including Local Skills Improvement Plans)."),
            p("The prototype dashboard shows a subset of employment and skills statistics at Local Enterprise Partnership (LEP) level, including:"),
            tags$ul(
              tags$li("Employment rates and employment distribution by occupation (source: ONS Annual Population Survey), ILR and ONS online job vacancies)"),
              tags$li("Online job vacancy units (source: ONS Online Job Adverts)"),
              tags$li("Further Education aim achievement volumes and achievements by sector subject area (source: DfE FE and Skills National Statistics)"),
            ),
            p("Trends can be compared between different LEPs and over time for some indicators. The underlying data contains national, regional, LEP and LA data and can be downloaded directly from the dashboard."),
            br(),
            "This dashboard has been produced to support the aims of the ",
            a(
              href = "https://www.gov.uk/government/groups/unit-for-future-skills",
              "Unit for Future Skills",
              .noWS = c("after")
            ),
            ".",
            column(width = 12, br(" "))
          ),

          ## Left panel -------------------------------------------------------
          column(
            6,
            # div(
            div(
              class = "panel panel-info",
              div(
                class = "panel-heading",
                style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;",
                h2("Contents")
              ),
              div(
                class = "panel-body",
                h2("Dashboard"),
                p("This page contains the different dashboard tabs."),
                h3(actionLink("link_to_tabpanel_overview", "Overview")),
                p("This tab provides a summary of employment and skills metrics at LEP level. It displays employment volume, employment rate, proportion of online vacancies, Further Education (FE) and skills achievement volumes. It shows year-on-year change for each indicator."),
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
                h3(actionLink("link_to_tabpanel_FE", "Skills")),
                p("This tab provides a summary of further education (FE) and skills statistics at LEP level. These can be compared to another LEP."),
                p("The line chart shows achievement volumes over time for apprenticeships, education & training and community learning."),
                p("The bar chart displays distribution of FE and skills achievements by SSA for the latest available period."),
                p("The download buttons download FE and skills indicators for the selected LEP or for all available geographies (England, region, LEP, LA)."),
              )
            )
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
                             The dashboard uses FE and skills learner achievements over time (AY1617-21/22 (provisional data to January for 21/22)) split by apprenticeships, community learning, education and training.
                             The data is taken from the FE and Skills official statistics."),
                  a(
                    href = "https://explore-education-statistics.service.gov.uk/find-statistics/further-education-and-skills",
                    "ILR data on EES",
                    .noWS = c("after", "before")
                  )
                )
              )
            ),
            div(
              class = "panel panel-info",
              div(
                class = "panel-heading",
                style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;",
                h2("Version control")
              ),
              div(
                class = "panel-body",
                p("Version 0.1. August 2022."),
                h3("Future development"),
                p("This dashboard will be developed with additional data and features in consultation with users. An updated dashboard will be released in Autumn 2022. This will include:"),
                tags$ul(
                  tags$li("A wider range of statistics on employment and skills;"),
                  tags$li("Additional data breakdowns;"),
                  tags$li("New features, including the ability to view data at different geography levels (e.g. Local Authority District)."),
                ),
                "If you have any feedback or suggestions for improvement, please contact us at ",
                a(href = "mailto:ufs.contact@education.gov.uk", "ufs.contact@education.gov.uk", .noWS = c("after")), "."
              )
            )
          ), # end of right panel
    ), # end of Tab Panel

    # APP ----

    tabPanel(
      "Local Skills",
      gov_main_layout(
      gov_row(
        column(
          width=12,
          div(
            class = "well",
            style = "min-height: 100%; height: 100%; overflow-y: visible",
            gov_row(
              column(
                width=6,
              selectInput("lep1", "Choose primary LEP area",
            choices = C_LEP2020
          )
          ),
          column(
            width=6,
            uiOutput("lep2_off")
          )
          # # lep 2 is reactve to lep 1 so is populated in the server
          # selectInput("lep2", "Choose comparison LEP area",
          #   choices = NULL
          # )
            )
        )
        )
        ),

        # next row is the data tabs
        column(
          width = 12,
          tabsetPanel(
            id = "datatabset",
            panel_overview(),
            panel_employment(),
            panel_vacancies(),
            panel_skills()
          ) # end of dashboard tabset panel
        ) # end of dashboard navbar
      ) # end of app data row
    ), # end of app tab panel

    # Create the accessibility statement-----------------
    tabPanel(
      "Accessibility",
      column(
        width=12,
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
        tags$li("Keyboard navigation through the interactive charts is currently limited, and some features are unavailable for keyboard only users"),
        tags$li("Alternative text in interactive charts is limited to titles and could be more descriptive (although this data is available in csv format)")
      )),
      h3("Feedback"),
      br(
        "If you have any feedback on how we could further improve the accessibility of this dashboard, please contact us at",
        a(href = "mailto:statistics.development@education.gov.uk", "statistics.development@education.gov.uk")
      ),
      br()
      )
    ), # End of accessibility tab
    # Support links ===========================================================

    tabPanel(
      "Support and feedback",
      support_links() # defined in R/supporting_links.R
    )
  ), # End of navBarPage
  # Footer ====================================================================

  shinyGovstyle::footer(TRUE)
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
#         gov_row(
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
