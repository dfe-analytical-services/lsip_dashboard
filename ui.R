# ---------------------------------------------------------
# This is the ui file.
# Use it to call elements created in your server file into the app, and define where they are placed.
# Also use this file to define inputs.
#
# Every UI file should contain:
# - A title for the app
# - A call to a CSS file to define the styling
# - An accessibility statement
# - Contact information
#
# Other elements like charts, navigation bars etc. are completely up to you to decide what goes in.
# However, every element should meet accessibility requirements and user needs.
#
# This fil e uses a slider input, but other inputs are available like date selections, multiple choice dropdowns etc.
# Use the shiny cheatsheet to explore more options: https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
#
# Likewise, this template uses the navbar layout.
# We have used this as it meets accessibility requirements, but you are free to use another layout if it does too.
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# ---------------------------------------------------------

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
            p("The aim of this app is to provide local data to local actors in an accessible format. The app will
                     support the implementation of LSIPs,
provide a baseline of local skills data which ERBs can build on with qualitative employer information, and
benefit secondary local actors (including Ofsted) who already use these local indicators to inform their work.
The app displays published data from a variety of sources (APS, ILR and ONS online job vacancies) to compare between different LEPS and over time for some indicators. "),
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
                    h3(actionLink("link_to_tabpanel_overview", "Overview")),
                    p("Key labour and skills metrics for each LEP."),
                    h2("Labour market"),
                    h3(actionLink("link_to_tabpanel_employment", "Employment")),
                    p("Employment volumes and rates over time and by occupation."),
                    h3(actionLink("link_to_tabpanel_vacancies", "Vacancies")),
                    p("Online job vacancies over time."),
                    h3(actionLink("link_to_tabpanel_earnings", "Earnings")),
                    p("To be completed in V2."),
                    h2("Skills landscape"),
                    h3(actionLink("link_to_tabpanel_FE", "FE")),
                    p("FE achievements over time and by SSA."),
                    h3(actionLink("link_to_tabpanel_HE", "HE")),
                    p("To be completed in V2."),
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
                             The app uses employment volumes and rates for each LEP and split by sub-major SOC 2020 grouping.
                             The data are for interviews conducted over the calendar year (2017-2021 are shown).
                             "),
                  a(
                    href = "https://www.nomisweb.co.uk/datasets/apsnew",
                    "APS data on Nomis",
                    .noWS = c("after")
                  ),
                  br(),
                  h2("Skills landscape"),
                  h3("Individualised Learner Records (ILR)"),
                  p("The ILR is an on-going collection of data about learners from training providers in the Further Education (FE) and Skills sector in England.
                             The app uses adult FE achievements over time (AY1617-21/22 (temporary data to October for 21/22)) split by apprenticeships, community learning, education and training.
                             The app also shows adult FE achievements split by sector subject area (tier 1) for the latest AY21/22 reported to January."),
                  a(
                    href = "https://explore-education-statistics.service.gov.uk/find-statistics/further-education-and-skills",
                    "ILR data on EES",
                    .noWS = c("after")
                  ),
                  br(),
                  h3("ONS Online Job Adverts"),
                  p("These data tables are created based upon online job adverts data provided by Adzuna.
                             Each time point in the series covers a monthly average of the volume of online job adverts in the month of January for the years 2017 to 2022. The monthly average is derived from weekly snapshots in January.
                             The app shows job 'units' which is the number of job adverts divided a set value for all regions. It is therefore not an indication of the real volume of job adverts, but can be used in comparisons across regions or to follow trends over time."),
                  a(
                    href = "https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/datasets/onlinejobadvertsbyitl1regionandlocalauthority",
                    "Job adverts data on ONS",
                    .noWS = c("after")
                  )
                )
              )
            )
          ), # end of right panel
        ) # end of FluidRow
      ) # end of FluidPage
    ), # end of Tab Panel

    # OVERVIEW ----
    tabPanel(
      "Overview",
      sidebarLayout(
        ## Side panel ----
        sidebarPanel(
          width = 2,
          br(),
          ### LEP 1 input ---------------
          selectizeInput("lep0a",
            "Choose a LEP:",
            choices = C_LEP2020,
            selected = "England" # ,multiple=TRUE
          ),
          ### Help text --------------------
          helpText("Download all available indicators for all geographies (LEPs, LAs, Regions and England):",
            style = "font-style: italic;"
          ),
          ### Download button -------------
          downloadButton(
            outputId = "download_btn0a",
            label = "All data",
            icon = shiny::icon("download")
          ),
          helpText("Or just for the currently chosen LEP:",
            style = "font-style: italic;"
          ),
          downloadButton(
            outputId = "download_btn0b",
            label = "Current LEP",
            icon = shiny::icon("download")
          )
        ), # end of side panel
        ## Main panel ----
        mainPanel(
          width = 10,
          ### Title ----
          uiOutput("page0title", style = "font-size: 24px;"),
          div("Change metrics are measured since the same period the year before.", style = "font-size: 16px; font-style: italic;"),
          br(),
          fluidRow(
            # left column
            column(
              width = 6,
              div(
                div(
                  class = "panel panel-info",
                  div(
                    class = "panel-heading",
                    style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;text-align:center",
                    h2("Labour market")
                  ),
                  div(
                    class = "panel-body",
                    # first row - emp vol
                    tags$div(
                      title = "Source: APS. 2021 calendar year",
                      valueBoxOutput("locland.emplcnt0", width = 8),
                      valueBoxOutput("locland.emplcntchange0", width = 4)
                    ),
                    box(
                      width = 12,
                      p(" ")
                    ),
                    # second row - emp rate
                    tags$div(
                      title = "Source: APS. 2021 calendar year",
                      valueBoxOutput("locland.emplrate0", width = 8),
                      valueBoxOutput("locland.emplchange0", width = 4)
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
                    tags$div(
                      title = "Source: ONS (Adzuna). Jan 2022",
                      valueBoxOutput("jobad.units", width = 8),
                      valueBoxOutput("jobad.change", width = 4)
                    ),
                    # fifth row - link to vacancy data
                    box(
                      width = 12,
                      actionLink("link_to_tabpanel_vacancies2", "Find out more about vacancies"),
                      align = "right"
                    ),
                  )
                )
              ),
            ),
            # right column
            column(
              width = 6,
              div(
                div(
                  class = "panel panel-info",
                  div(
                    class = "panel-heading",
                    style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;text-align:center",
                    h2("Skills landscape")
                  ),
                  div(
                    class = "panel-body",
                    # 3rd row - E&T
                    tags$div(
                      title = "Source:ILR AY20/21",
                      valueBoxOutput("skisup.ETach", width = 8),
                      valueBoxOutput("skisup.ETachChange", width = 4)
                    ),
                    box(
                      width = 12,
                      p(" ")
                    ),
                    # 5th row - apps
                    tags$div(
                      title = "Source:ILR AY20/21",
                      valueBoxOutput("skisup.APPach", width = 8),
                      valueBoxOutput("skisup.APPachChange", width = 4)
                    ),
                    # 6th row - link to app data
                    box(
                      width = 12,
                      actionLink("link_to_tabpanel_FE2", "Find out more about skills"),
                      align = "right"
                    ),
                  )
                )
              ),
            ),
          )
        )
      ) # end of side bar layout
    ), # end of Overview tab

    navbarMenu(
      "Labour market",

      # EMPLOYMENT ----
      tabPanel(
        "Employment",

        # Sidebar
        sidebarLayout(
          ## Side panel ----
          sidebarPanel(
            width = 2,

            ### LEP 1 input ---------------
            selectizeInput("lep1",
              "Choose a LEP:",
              choices = C_LEP2020,
              selected = "England",
            ),
            ### LEP 2 input ------------
            selectizeInput("lep2",
              "Choose a comparison LEP (optional):",
              choices = c("\nNone", unique(C_LEP2020)),
              multiple = F
            ),

            ### Help text --------------------
            helpText("Download employment indicators for all geographies (LEPs, LAs, Regions and England):",
              style = "font-style: italic;"
            ),
            ### Download buttons -------------
            downloadButton(
              outputId = "download_btn1a",
              label = "All data",
              icon = icon("download")
            ),
            helpText("Or just for the currently chosen LEP:",
              style = "font-style: italic;"
            ),
            downloadButton(
              outputId = "download_btn1b",
              label = "Current LEP",
              icon = icon("download")
            ),
          ), # end of side panel
          ## Main panel ----
          # Show a plot of the generated distribution
          mainPanel(
            width = 10,
            ### Title ----
            uiOutput("page1title", style = "font-size: 24px;"),
            div("Data is from the Annual Population Survey. Years represent calendar years.", style = "font-size: 16px; font-style: italic;"),
            # br(),

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
            )
          ) # end of main panel
        ) # end of side bar layout
      ), # end of Local Landscape tab

      # VACANCIES ---------------
      tabPanel(
        "Vacancies",
        sidebarLayout(
          ## Side panel ----
          sidebarPanel(
            width = 2,
            ### LEP 5 input ---------------
            selectizeInput("lep5",
              "Choose a LEP:",
              choices = C_LEP2020,
              selected = "England",
            ),
            ### LEP 6 input ------------
            selectizeInput("lep6", # Make no selection an option
              "Choose a comparison LEP (optional):",
              choices = c("\nNone", unique(C_LEP2020))
            ),
            ### Help text --------------------
            helpText("Download vacancy indicators for all geographies (LEPs, LAs, Regions and England):",
              style = "font-style: italic;"
            ),
            ### Download buttons -------------
            downloadButton(
              outputId = "download_btn3a",
              label = "All data",
              icon = icon("download")
            ),
            helpText("Or just for the currently chosen LEP:",
              style = "font-style: italic;"
            ),
            downloadButton(
              outputId = "download_btn3b",
              label = "Current LEP",
              icon = icon("download")
            ),
          ), # end of side panel
          ## Main panel ----
          # Show a plot of the generated distribution
          mainPanel(
            width = 10,
            ### Title ----
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
              ### ONS job advert information ----
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
              The monthly average is derived from weekly snapshots in January. The volume of online job adverts is presented as a unit measure. The unit measure is derived by dividing the monthly average count of job adverts by a set value."
            ),
          ) # end of main panel
        ) # end of side bar layout
      ), # end of Skills Supply tab
    ), # end of labour navbar

    navbarMenu(
      "Skills landscape",
      # FE ----
      tabPanel(
        "FE",
        sidebarLayout(
          ## Side panel ----
          sidebarPanel(
            width = 2,
            ### LEP 3 input ---------------
            selectizeInput("lep3",
              "Choose a LEP:",
              choices = C_LEP2020,
              selected = "England",
            ),
            ### LEP 4 input ------------
            selectizeInput("lep4", # Make no selection an option
              "Choose a comparison LEP (optional):",
              choices = c("\nNone", unique(C_LEP2020))
            ),
            ### Help text --------------------
            helpText("Download FE indicators for all geographies (LEPs, LAs, Regions and England):",
              style = "font-style: italic;"
            ),
            ### Download buttons -------------
            downloadButton(
              outputId = "download_btn2a",
              label = "All data",
              icon = icon("download")
            ),
            helpText("Or just for the currently chosen LEP:",
              style = "font-style: italic;"
            ),
            downloadButton(
              outputId = "download_btn2b",
              label = "Current LEP",
              icon = icon("download")
            ),
          ), # end of side panel
          ## Main panel ----
          mainPanel(
            width = 10,
            ### Title ----
            uiOutput("page2title", style = "font-size: 24px;"),
            div("Data from Individualised Learner Records for adult FE learners. Years shown are academic years.", style = "font-size: 16px; font-style: italic;"),
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
              # height=500,
              ### Achievements over time line chart ----
              column(
                width = 6,
                ### LEP 4 input ------------
                p("FE and apps achievement trend", style = "font-size:20px;"),
                p("Choose provision group"),
                selectizeInput("skill_line", "",
                  choices = c("Apprenticeships (all ages)", "Education and training (adults only)", "Community learning (adults only)", "Total FE and Apps provision")
                ),
                plotlyOutput("Ach_time")
              ),

              ### FE acheivements ----
              column(
                width = 6,
                p("All adult FE achievements by SSA tier 1 (AY21/22 Aug to Jan)", style = "font-size:20px;"),
                plotlyOutput("Ach_SSA_pc")
              )
            ) # end of box
          ) # end of main panel
        ) # end of side bar layout
      ), # end of Skills Supply tab
    ), # end of skills navbar

    # FUTURE DEVELOPMENT ---------------
    #   navbarMenu(
    #    "Future development",
    tabPanel(
      "Future development",
      mainPanel(
        width = 12,
        p("We will be seeking feedback on this version 1 of the local skills dashboard to inform our version 2.
            We do have a number of improvements we will implement in version 2:  "),
        br(),
        p("Overview page", style = "font-size:20px;"),
        p("V2 will include a number of key indicators to this page including: average salary, FE skill level, and HE data. "),
        p("V2 will also include the functionality to filter the indicators by sector or occupation. "),
        p("V2 overview page mockup:", style = "font-style: italic"),
        img(src = "OverviewV2.png"),
        br(),
        p("Apprenticeships page", style = "font-size:20px;"),
        p("V2 will show:"),
        p("Starts and achievements timeseries."),
        p("Distribution by level, SSA, enterprise size, and enterprise sector."),
        p("Most common apprenticeships within selected provision, level, SSA, sector."),
        p("Information about which providers/employers deliver these apprenticeships."),
        br(),
        p("HE page", style = "font-size:20px;"),
        p("V2 will show HE participants and qualifiers by subject."),
        br(),
        p("Overview page", style = "font-size:20px;"),
        p("V2 will show learner outcomes by type of provision."),
      )
    ), # enedx of future development
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


    # Create the accessibility statement-----------------
    tabPanel(
      "Accessibility",
      h2("Accessibility statement"),
      br("This accessibility statement applies to the **application name**.
            This application is run by the Department for Education. We want as many people as possible to be able to use this application,
            and have actively developed this application with accessibilty in mind."),
      h3("WCAG 2.1 compliance"),
      br("We follow the reccomendations of the ", a(href = "https://www.w3.org/TR/WCAG21/", "WCAG 2.1 requirements. "), "This application has been checked using the ", a(href = "https://github.com/ewenme/shinya11y", "Shinya11y tool "), ", which did not detect accessibility issues.
             This application also fully passes the accessibility audits checked by the ", a(href = "https://developers.google.com/web/tools/lighthouse", "Google Developer Lighthouse tool"), ". This means that this application:"),
      tags$div(tags$ul(
        tags$li("uses colours that have sufficient contrast"),
        tags$li("allows you to zoom in up to 300% without the text spilling off the screen"),
        tags$li("has its performance regularly monitored, with a team working on any feedback to improve accessibility for all users")
      )),
      h3("Limitations"),
      br("We recognise that there are still potential issues with accessibility in this application, but we will continue
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
        "If you have any feedback on how we could further improve the accessibility of this application, please contact us at",
        a(href = "mailto:email@education.gov.uk", "email@education.gov.uk")
      )
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
