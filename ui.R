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
    tags$meta(name = "description", content = "Data dashboard presenting Local Skills data from the Unit for Future Skills in the Department for Education."),
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
background-color: #1d70b8;
border-radius: 4px;
padding: 15px 15px 0px 15px;
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
    <img src="images/DfE_logo.png" class="govuk-header__logotype-crown-fallback-image"/>
    <span class="govuk-header__logotype-text">DfE</span>
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
    <span class="govuk-phase-banner__text">Please be aware that you may experience performance issues and the dashboard may require a reload. We are working to fix this.</span>
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

    # HOMEPAGE ============================================================

    tabPanel(
      "Homepage",
      fluidRow(
        column(
          12,
          h1("Local Skills Dashboard"),
          p("Prototype dashboard showing statistics on local employment and skills in England, to support local skills planning and delivery (including Local Skills Improvement Plans)."),
          p("The prototype dashboard shows a subset of employment and skills statistics at Local Enterprise Partnership (LEP) and local skills improvement plan area (LSIP) level, including:"),
          tags$ul(
            tags$li("Employment rates and employment distribution by occupation (source: ONS Annual Population Survey), ILR and ONS online job vacancies)"),
            tags$li("Online job vacancy units (source: ONS Online Job Adverts)"),
            tags$li("Further Education aim achievement volumes and achievements by sector subject area (source: DfE FE and Skills National Statistics)"),
          ),
          p("Trends can be compared between different LEPs/LSIPs and over time for some indicators. The underlying data contains national, regional, LEP, LSIP and LA data and can be downloaded directly from the dashboard."),
          p(
            "This dashboard has been produced to support the aims of the ",
            a(
              href = "https://www.gov.uk/government/groups/unit-for-future-skills",
              "Unit for Future Skills",
              .noWS = c("after")
            ), "."
          )
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
              h2("Dashboard contents")
            ),
            div(
              class = "panel-body",
              h3(actionLink("link_to_tabpanel_overview", "Overview")),
              p("This tab provides a summary of employment and skills metrics at LEP/LSIP level. It displays employment volume, employment rate, proportion of online vacancies, Further Education (FE) and skills achievement volumes. It shows year-on-year change for each indicator."),
              p("The download buttons download all indicators for the selected LEP/LSIP or for all available geographies (England, region, LEP, LSIP, LA)."),
              h3(actionLink("link_to_tabpanel_employment", "Employment")),
              p("This tab contains employment indicators at LEP/LSIP level. These can be compared to England and a comparator LEP/LSIP."),
              p("The line chart shows employment rate over time for the chosen LEP/LSIP. The table displays employment distribution by occupation (sub-major SOC group)."),
              p("The download buttons download employment indicators for the selected LEP/LSIP or for all available geographies (England, region, LEP, , LA)."),
              h3(actionLink("link_to_tabpanel_vacancies", "Vacancies")),
              p("This tab contains online job vacancies indicators at LEP/LSIP level. These can be compared to England and a comparator LEP/LSIP."),
              p("The line chart shows change in online job vacancy units over time. Units are not real volumes but represent a fixed number of job adverts to be used for comparisons over time and between areas."),
              p("The download buttons download vacancy indicators for the selected LEP/LSIP or for all available geographies (England, region, LEP, LSIP, LA)."),
              h3(actionLink("link_to_tabpanel_FE", "Skills")),
              p("This tab provides a summary of further education (FE) and skills statistics at LEP/LSIP level. These can be compared to another LEP/LSIP."),
              p("The line chart shows achievement volumes over time for apprenticeships, education & training and community learning."),
              p("The bar chart displays distribution of FE and skills achievements by SSA for the latest available period."),
              p("The download buttons download FE and skills indicators for the selected LEP/LSIP or for all available geographies (England, region, LEP, LSIP, LA)."),
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
              p(
                "If you have any feedback or suggestions for improvement, please contact us at ",
                a(href = "mailto:ufs.contact@education.gov.uk", "ufs.contact@education.gov.uk", .noWS = c("after")), "."
              )
            )
          )
        )
      ) # end of version control row
    ), # end of Tab Panel

    tabPanel(
      "Data",
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
          h3("Annual Population Survey (APS)"),
          p("A continuous household survey covering the UK.
                             The dashboard uses employment volumes and rates for each LEP/LSIP and split by sub-major SOC 2010 grouping.
                             The data are for interviews conducted over the calendar year (2017-2021 are shown).
                             Totals for LSIPs are calculated by adding up the relevant LAs. Since the published LA volumes are rounded, there may be a small rounding error in the LSIP total.
                             "),
          h3("ONS Online Job Adverts"),
          p("These data tables are created based upon online job adverts data provided by Adzuna.
                             Each time point in the series covers a monthly average of the volume of online job adverts in the month of January for the years 2017 to 2022. The monthly average is derived from weekly snapshots in January.
                             The dashboard shows job 'units' which is the number of job adverts divided a set value for all regions. It is therefore not an indication of the real volume of job adverts, but can be used in comparisons across regions or to follow trends over time."),
          h3("Individualised Learner Records (ILR)"),
          p("The ILR is an on-going collection of data about learners from training providers in the Further Education (FE) and Skills sector in England.
                             The dashboard uses FE and skills learner achievements over time (AY1617-21/22 (provisional data to April for 21/22)) split by apprenticeships, community learning, education and training.
                             The data is taken from the FE and Skills official statistics.
                             Totals for LSIPs and LEPs are calculated by adding up the relevant LAs. Since the published LA volumes are rounded, there may be a small rounding error in the LSIP and LEP totals.
"),
        )
      ) # end of data information row
    ), # end of data tab
    # APP ----

    tabPanel(
      "Local Skills",
      fluidRow(
        column(
          12,
          br(),
          div(
            class = "filterRow",
            fluidRow(
              column(
                style = "color:#fff",
                width = 4,
                selectInput("GeoType", "Choose geography level:",
                  choices = list("LEP", "LSIP"),
                  selected = "LEP"
                )
              ),
              column(
                width = 4,
                uiOutput("lep1_geo"),
              ),
              column(
                width = 4,
                uiOutput("lep2_off")
              )
            )
          ),
          br(),
        )
      ), # end of filters row

      # next row is the data tabs
      fluidRow(
        column(
          12,
          tabsetPanel(
            id = "datatabset",
            panel_overview(),
            panel_employment(),
            panel_vacancies(),
            panel_skills()
          ) # end of dashboard tabset panel
        ) # end of dashboard navbar
      )

      #  ) # end of app data row
    ), # end of app tab panel

    # Create the accessibility statement-----------------
    tabPanel(
      "Accessibility",
      fluidRow(
        column(
          width = 12,
          h1("Accessibility statement"),
          p("This accessibility statement applies to the Local Skills dashboard.
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
    )
  ), # End of navBarPage
  # Footer ====================================================================

  shinyGovstyle::footer(TRUE)
)
