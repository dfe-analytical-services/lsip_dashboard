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
    tags$meta(name = "application_name", content = "Unit for Future Skills - Local Skills Dashboard"),
    tags$meta(name = "description", content = "Data dashboard presenting Local skills data from the Unit for Future Skills in the Department for Education."),
    tags$meta(name = "subject", content = "Education data dashboards.")
  ),
  # Set title for search engines

  HTML("<title>Local Skills Dashboard</title>"),
  tags$head(includeHTML(("google-analytics.html"))),

  # internal CSS
  # remove the max width of the main panel so spreads across screen
  tags$head(
    tags$style(HTML("
.govuk-width-container {
    max-width: 100%;
}

.filterRow{
background-color: #f3f2f1; /*#1d70b8*/
border-radius: 4px;
padding: 15px 15px 0px 15px;
}
/*filter labels*/
.control-label {
    color: #000;
}

.chartBox{
background-color:#f3f2f1;
border-right: 5px solid white;
}

.rightAlignLinks{
text-align: right;
padding-right:15px;
padding-top:15px
}

.hiddenLink {
  visibility: hidden;
}

/* styles for menu button - move to stylesheet */

    #menuButton {
      display: none;
      width: auto;
    }

    .menuBtn {
      color: #fff;
      float: left;
      padding: 10px;
    }

        .menuBtn:focus {
      color: #fff;
      background-color: #000;
    }


    @media (max-width: 767px) {
      .nav-stacked {
        display: none;
      }
      .nav-stacked.active {
        display: block;
      }

      #menuButton {
        display: block;
      }

      .menuBtn.active {
        background-color: #fff;
        color: #000;
      }

      /* end of styles for menu button */

    }

    /* style KPI boxes
    #12436D - govt analytical function blue
    #AAE3F0 - dfe corp turquoise 40% tint
    #28A197 - govt analytical function turquoise
    #DAEEBF - dfe corp lime 40% tint
    #718ea7 - govt analytical function blue 40% tint
    #7ec7c1 - govt analytical function turquoise 40% tint
    */

.small-box.bg-geo1{
    background-color: #12436D !important;
    color: #fff;
    padding: 2px 2px 4px 12px;
}

.small-box.bg-geo2{
    background-color: #28A197 !important;
    color: #fff;
    padding: 2px 2px 4px 12px;
}

.small-box.bg-geo3{
    background-color: #BFBFBF !important;
    color: #fff;
    padding: 2px 2px 4px 12px;
}

")),
    tags$script(HTML(
      '
    /* javascript function for menu button */
    function collapseMenu() {
      var x = document.getElementById("navbar");
      x.classList.toggle("active");

      var x = document.getElementById("menuButton");
      x.classList.toggle("active");
    }
    '
    ))
  ),

  # Force the top nav bar to left allign and centre the title
  HTML('<header class="govuk-header" role="banner">
    <div class="govuk-header__container">
    <div class="govuk-header__logo" style="width: 15%; margin-left: 15px;float:left;">
    <a href="https://www.gov.uk/government/organisations/department-for-education" class="govuk-header__link govuk-header__link--homepage">
    <span class="govuk-header__logotype">
    <img src="images/DfE_logo_landscape.png" height=32 width=146/>
    <span class="govuk-header__logotype-text"></span>
    </span>
    </a>
    </div>
    <div class="govuk-header__content" style="width: 70%; text-align: center;float:left;">
    <a href="https://www.gov.uk/government/groups/unit-for-future-skills" class="govuk-header__link govuk-header__link--service-name" style="font-size: 24px;">Unit for Future Skills - Local Skills Dashboard</a>
    </div>
        <a href="javascript:void(0);" id="menuButton" class="menuBtn" onclick="collapseMenu()">
    <i class="fa fa-bars" style="font-size:24px;"></i></a>
    </div>
    </header>'),
  # This is the old code if we need it
  # shinyGovstyle::header(
  #   main_text = "DfE",
  #   main_link = "https://www.gov.uk/government/organisations/department-for-education",
  #   secondary_text = "Unit for Future Skills - Local Skills Dashboard",
  #   logo = "images/DfE_logo.png"
  # ),

  HTML('<div class="govuk-phase-banner govuk-width-container govuk-main-wrapper" id="beta banner" style="margin-left:0px;margin-right:0px">
  <p class="govuk-phase-banner__content">
    <strong class="govuk-tag govuk-phase-banner__content__tag ">beta</strong>
    <span class="govuk-phase-banner__text">We are aware of performance issues that require some users to reload the page. We are working to fix this.
</span>
  </p>
</div>'),
  # shinyGovstyle::banner(
  #   "beta banner",
  #   "beta",
  #   paste0(
  #     "Please be aware that you may experience performance issues and the dashboard may require a reload. We are working to fix this."
  #   )
  # ),

  # Navbar ====================================================================
  navlistPanel(
    id = "navbar",
    widths = c(2, 10),
    well = FALSE,
    selected = "Local skills v2",

    # HOMEPAGE ============================================================

    tabPanel(
      "Homepage",
      fluidRow(
        column(
          12,
          h1("Local Skills Dashboard"),
          p("This prototype dashboard shows statistics on local employment and skills in England, to support local skills planning and delivery.
            It includes a subset of employment and skills statistics that can be viewed for three geographic areas: Local Enterprise Partnership (LEP), Local Skills Improvement Plan (LSIP) and Mayoral Combined Authority (MCA) areas.
            The underlying data can also contains breakdowns by Local Authority and region and can be downloaded using the links on each page or directly from the downloads page."),
          p("The dashboard currently uses data that is published by the Office for National Statistics (ONS) and the Department for Education (DfE).
            It currently only includes a small number of sources and will be gradually updated to include further sources and functionality."),
          p(
            "This dashboard does not currently include any information on the labour market outcomes of training.
            Separate tools are available that have been developed to help understand these data through the links below, or from the ",
            a(
              href = "https://www.gov.uk/government/groups/unit-for-future-skills",
              "Unit for Future Skills",
              .noWS = c("after")
            ), " webpage."
          ),
          tags$ul(
            tags$li(
              "Graduate outcomes (",
              a(
                href = "https://explore-education-statistics.service.gov.uk/find-statistics/graduate-outcomes-leo",
                "official statistics",
                .noWS = c("after", "before")
              ),
              ")(",
              a(
                href = "https://department-for-education.shinyapps.io/leo-graduate-industry-dashboard",
                "dashboard",
                .noWS = c("after", "before")
              ),
              ")"
            ),
            tags$li(
              "Outcome based success measures (",
              a(
                href = "https://explore-education-statistics.service.gov.uk/find-statistics/further-education-outcome-based-success-measures",
                "official statistics",
                .noWS = c("after", "before")
              ),
              ")(",
              a(
                href = "https://app.powerbi.com/view?r=eyJrIjoiOGYxYmU5ODktN2U1NC00ZjU4LWIwMTgtZDAzMDljMzVlNTE0IiwidCI6ImZhZDI3N2M5LWM2MGEtNGRhMS1iNWYzLWIzYjhiMzRhODJmOSIsImMiOjh9&pageName=ReportSection836f307d5071a434f3a2",
                "dashboard",
                .noWS = c("after", "before")
              ),
              ")"
            ),
          ),
        )
      ), # end intro text row

      ## dashboard contents -------------------------------------------------------
      fluidRow(
        column(
          12,
          div(
            class = "panel panel-info",
            div(
              class = "panel-heading",
              style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;",
              h2("User Guide")
            ),
            div(
              class = "panel-body",
              h2("Dashboard pages"),
              p("There are four pages which focus on different parts of the jobs and skills market.
                Each page includes options to download all of the indicators shown for either just the selected geographic area or for all available geographies.
                Where published figures are not available, totals for LEP/LSIP/MCA areas are calculated by adding up the relevant local authorities.
                As local authority volumes are rounded and small volumes are suppressed, there may be rounding errors in the LEP/LSIP/MCA area totals."),
              h3(actionLink("link_to_tabpanel_overview", "Overview")),
              p("This page provides a summary of labour market and skills information for the selected geographic area.
                It includes a time series of data on employment, online job adverts, and further education and skills achievements."),
              p(
                "ONS have announced that there is an issue with the collection of their occupational data surveys.
            As such please see this data with caution.
                    For more information see this ONS  ",
                a(
                  href = "https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/articles/theimpactofmiscodingofoccupationaldatainofficefornationalstatisticssocialsurveysuk/2022-09-26",
                  "article",
                  .noWS = c("after")
                ), "."
              ),
              h3(actionLink("link_to_tabpanel_employment", "Employment")),
              p("This page includes information on employment for the selected geographic area, and the option to compare against another area. It includes data on employment rates over time, and the share of employment for occupations."),
              h3(actionLink("link_to_tabpanel_vacancies", "Vacancies")),
              p("This page includes experimental data on the share of online job adverts for the selected geographic area, and the option to compare against another area.
                The data has been included temporarily, and will be replaced when ONS release further, more detailed, experimental data on online job adverts (currently planned before the end of 2022).
                LEP/LSIP/MCA area totals are calculated by adding up the relevant local authorities."),
              h3(actionLink("link_to_tabpanel_FE", "Skills")),
              p("This page includes information on training activity for the selected geographic area, and the option to compare against another area.
                It includes data on achievements for further education and skills training, with breakdowns for type of training over time and subject area for the latest time period.
                LEP/LSIP/MCA area totals are calculated by adding up the relevant local authorities."),
              h2("Data download page"),
              p("The", actionLink("link_to_tabpanel_data", "data download page"), " includes details on the sources of data used in this dashboard, and includes options to download some or all of the data."),
            )
          )
        )
      ), # end of dashboard contents row

      ## Version control ------------------------------------------------------
      fluidRow(
        column(
          12,
          div(
            class = "panel panel-info",
            div(
              class = "panel-heading",
              style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;",
              h2("Update history")
            ),
            div(
              class = "panel-body",
              h2("Latest update"),
              p("29 Nov 2022 (0.2.3)"),
              tags$ul(
                tags$li("Included MCA areas."),
                tags$li("Employment data for LEPs, LSIPs (and MCAs) taken directly from the APS data (previously aggregated from LAs)."),
                tags$li("Characteristic and course breakdowns added to the Skills page."),
                tags$li("Updated ILR data to the final AY21/22 data.")
              ),
              details(
                label = "Previous updates",
                inputId = "PreviousUpdate",
                p(
                  p("25 Oct 2022 (0.2.2)"),
                  p("Applied fixes and included new datasets to the data sources page."),
                  br(),
                  p("18 Oct 2022 (0.2.1)"),
                  p("Added option to view LSIP geographies, included new data download page, and updated dashboard styling."),
                  br(),
                  p("16 Sep 2022 (0.1.2)"),
                  p("Applied fixes to improve background performance."),
                  br(),
                  p("8 Aug 2022 (0.1.1)"),
                  p("Release initial prototype.")
                )
              ),
              h2("Future development"),
              p("The dashboard will be kept up to date with the latest data shortly after it is released – check the data downloads page for dates when new data is published."),
              p(
                "We will be regularly adding more data and visualisations to the dashboard based on the user feedback we have received.  If there are further data or dashboard features that you would find useful please contact us at ",
                a(href = "mailto:ufs.contact@education.gov.uk", "ufs.contact@education.gov.uk", .noWS = c("after")), "."
              )
            )
          )
        )
      ) # end of version control row
    ), # end of Tab Panel

    # # APP ----
    # 
    # tabPanel(
    #   "Local skills",
    #   fluidRow(
    #     column(
    #       12,
    #       br(),
    #       div(
    #         class = "filterRow",
    #         fluidRow(
    #           column(
    #             width = 4,
    #             selectInput("GeoType", "Choose geography",
    #               choices = c(
    #                 "Local Enterprise Partnership (LEP)" = "LEP",
    #                 "Local Skills Improvement Plan (LSIP)" = "LSIP",
    #                 "Mayoral Combined Authority (MCA)" = "MCA"
    #               ),
    #               selected = "LEP"
    #             )
    #           ),
    #           column(
    #             width = 4,
    #             uiOutput("lep1_geo"),
    #           ),
    #           column(
    #             width = 4,
    #             uiOutput("lep2_off")
    #           )
    #         )
    #       ),
    #       br(),
    #     )
    #   ), # end of filters row
    # 
    #   # next row is the data tabs
    #   fluidRow(
    #     column(
    #       12,
    #       tabsetPanel(
    #         id = "datatabset",
    #         panel_overview(),
    #         panel_employment(),
    #         panel_vacancies(),
    #         panel_skills()
    #       ) # end of dashboard tabset panel
    #     ) # end of dashboard navbar
    #   )
    # 
    #   #  ) # end of app data row
    # ), # end of app tab panel

    tabPanel(
      "Data & downloads",
      fluidRow(
        column(
          12,
          h1("Data sources"),
          dataTableOutput("DataTbl"),
          uiOutput("hidden_downloads")
        )
      ), # end of data table row
      br(),
      fluidRow(
        column(
          12,
          h2("Data information"),
          h3("Annual Population Survey"),
          p("The Annual Population Survey (APS) is a continuous household survey, covering the UK.
            The topics covered include employment and unemployment, as well as housing, ethnicity, religion, health and education.
            The dashboard shows employment volumes and rates for each geographic area and split by sub-major Standard Occupational Classification (SOC) 2010 grouping."),
          p(
            "ONS have announced that there is an issue with the collection of their occupational data surveys.
            As such please see this data with caution.
                    For more information see this ONS  ",
            a(
              href = "https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/articles/theimpactofmiscodingofoccupationaldatainofficefornationalstatisticssocialsurveysuk/2022-09-26",
              "article",
              .noWS = c("after")
            ), "."
          ),
          h3("ONS online job adverts"),
          p("These data tables are created based upon online job adverts data provided by Adzuna.
            Each time point in the series covers a monthly average of the volume of online job adverts in the month of January for the years 2017 to 2022.
            The dashboard shows job 'units' which is the number of job adverts divided a set value for all regions.
            It is therefore not an indication of the real volume of job adverts, but can be used in comparisons across areas or to follow trends over time.
            Totals for LEP/LSIP/MCA areas are calculated by adding up the relevant local authorities (LAs).
            Since the published LA volumes are rounded, there may be a small rounding error in the LEP/LSIP/MCA totals."),
          h3("Individualised Learner Record"),
          p("The Individualised Learner Record (ILR) is an on-going collection of data about learners from training providers in the further education and skills sector in England.
          The dashboard shows further education and skills learner achievements over time split by apprenticeships, community learning, education and training.
          Totals for LEP/LSIP/MCA areas are calculated by adding up the relevant local authorities (LAs).
            Since the published LA volumes are rounded, there may be a small rounding error in the LEP/LSIP/MCA totals."),
          h3("KS4 and KS5 destinations"),
          p("Statistics compiled from the National Pupil Database (NPD) showing the number of young people going into education and,
            employment or apprenticeship in the year following completion of their qualification:"),
          tags$ul(
            tags$li("Key Stage 4 (year 10 and 11 students) includes GCSEs and equivalent qualifications"),
            tags$li("Key Stage 5 (students 16-18 years) includes A Levels, T levels and other 2-year vocational education programmes.")
          ),
          p(" Totals for LEP/LSIP/MCA areas are calculated by adding up the relevant local authorities (LAs).
            Since the published LA volumes are rounded, there may be a small rounding error in the LEP/LSIP/MCA totals."),
          h3("UK Business Count (UBC)"),
          p("A record of the number of enterprises from the Inter Departmental Business Register (IDBR), which is broken down by employment size band, detailed
            industry (5 digit SIC2007) and legal status.
             Totals for LEP/LSIP/MCA areas are calculated by adding up the relevant local authorities (LAs).
            Since the published LA volumes are rounded, there may be a small rounding error in the LEP/LSIP/MCA totals."),
          br()
        )
      ) # end of data information row
    ), # end of data tab

    # Create the accessibility statement-----------------
    tabPanel(
      "Accessibility",
      fluidRow(
        column(
          width = 12,
          h1("Accessibility statement"),
          p("This accessibility statement applies to the Local skills dashboard.
            This dashboard is run by the Department for Education. We want as many people as possible to be able to use this application,
            and have actively developed this dashboard with accessibilty in mind."),
          h2("WCAG 2.1 compliance"),
          p("We follow the reccomendations of the ", a(href = "https://www.w3.org/TR/WCAG21/", "WCAG 2.1 requirements. "), "This application has been checked using the ", a(href = "https://github.com/ewenme/shinya11y", "Shinya11y tool "), ", which did not detect accessibility issues.
             This dashboard also fully passes the accessibility audits checked by the ", a(href = "https://developers.google.com/web/tools/lighthouse", "Google Developer Lighthouse tool"), ". This means that this dashboard:"),
          tags$div(tags$ul(
            tags$li("uses colours that have sufficient contrast"),
            tags$li("allows you to zoom in up to 300% without the text spilling off the screen"),
            tags$li("has its performance regularly monitored, with a team working on any feedback to improve accessibility for all users")
          )),
          h2("Limitations"),
          p("We recognise that there are still potential issues with accessibility in this dashboard, but we will continue
             to review updates to technology available to us to keep improving accessibility for all of our users. For example, these
            are known issues that we will continue to monitor and improve:"),
          tags$div(tags$ul(
            tags$li("Keyboard navigation through the interactive charts is currently limited, and some features are unavailable for keyboard only users"),
            tags$li("Alternative text in interactive charts is limited to titles and could be more descriptive (although this data is available in csv format)")
          )),
          h2("Feedback"),
          p(
            "If you have any feedback on how we could further improve the accessibility of this dashboard, please contact us at",
            a(href = "mailto:statistics.development@education.gov.uk", "statistics.development@education.gov.uk")
          ),
          br()
        )
      )
    ), # End of accessibility tab
    # Support links ===========================================================

    tabPanel(
      "Support and feedback",
      support_links() # defined in R/supporting_links.R
    ),

    # mapsplash ======
    tabPanel(
      "Local skills v2",
      br(),
      fluidRow(column(
        12,
        radioGroupButtons(
          inputId = "splashMetric",
          # label = "Focus on",
          choices = c("Employment rate"="empRate", "Vacancies"="vacancies", "FE achievements"="achievements_rate_per_100000_population", "Supply vs demand"="mismatch")
        )
      )),
      fluidRow(
        column(
          6,
          h3(uiOutput("titleMap")),
          radioGroupButtons(
            inputId = "splashGeoType",
            choices = c("LEP", "LSIP", "MCA")
          ),
          p(uiOutput("commentMap")),
          leafletOutput("map") # ,
          # leafletOutput("mymap"),
          # plotlyOutput("mapSplashGG")
        ),
        column(
          6,
          h3(uiOutput("titleTime")),
          p(uiOutput("commentTime")),
          uiOutput("geoComp"),
          plotlyOutput("Splash_time")
        )
      ),
      br(),
      fluidRow(
        column(
          6,
          h3(uiOutput("titleBreakdown")),
          uiOutput("breakdownFilter"),
          p(uiOutput("commentBreakdown")),
          plotlyOutput("Splash_pc")
        ),
        column(
          6,
          h3("What is the variation with the LEP?"),
          p("FE achievement rate is highest in Sunderland and lowest in Northumberland"),
          leafletOutput("mapLA")
        )
      )
    )
  ), # End of navBarPage
  # Footer ====================================================================

  shinyGovstyle::footer(TRUE)
)
