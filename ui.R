# 1. Set up page, header, js and css ----
## 1.1 Set up page and browser info ----
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

  # Set metadata for browser
  tags$html(lang = "en"),
  tags$head(
    tags$meta(name = "application_name", content = "Unit for Future Skills - Local Skills Dashboard"),
    tags$meta(name = "description", content = "Data dashboard presenting Local skills data from the Unit for Future Skills in the Department for Education."),
    tags$meta(name = "subject", content = "Education data dashboards.")
  ),

  # Set title for search engines
  HTML("<title>Local Skills Dashboard</title>"),
  tags$head(includeHTML((
    "google-analytics.html"
  ))),

  ## 1.2. Internal CSS ----
  tags$head(
    tags$style(
      HTML(
        "

    /* remove the max width of the main panel so spreads across screen*/
.govuk-width-container {
    max-width: 100%;
}

/*style filter row grey background*/
.filterRow{
background-color: #f3f2f1; /*#1d70b8*/
border-radius: 4px;
padding: 15px 15px 0px 15px;
}

/*filter labels*/
.control-label {
    color: #000;
}

/*add white border*/
.chartBox{
background-color:#f3f2f1;
border-right: 5px solid white;
}

/*right allign links*/
.rightAlignLinks{
text-align: right;
padding-right:15px;
padding-top:15px
}

/*on the data table hide links to overlay with clicakble links*/
.hiddenLink {
  visibility: hidden;
}

/* styles for menu button*/
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

/* for mobile*/
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
    }

    /* style KPI boxes. Here are some colour options for easy access
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

/* map popup styling*/
div.myspecial-popup div.leaflet-popup-content-wrapper {
          padding: 0px 0px 1px 0px;

        }

"
      )
    ),
    ## 1.3. Javascript and HTML banner----
    # Collapsible menu js
    tags$script(
      HTML(
        '
    /* javascript function for menu button */
    function collapseMenu() {
      var x = document.getElementById("navbar");
      x.classList.toggle("active");

      var x = document.getElementById("menuButton");
      x.classList.toggle("active");
    }
    '
      )
    )
  ),

  # Force the top nav bar to left align and centre the title
  HTML(
    '<header class="govuk-header" role="banner">
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
    </header>'
  ),

  # Add bug header
  HTML(
    '<div class="govuk-phase-banner govuk-width-container govuk-main-wrapper" id="beta banner" style="margin-left:0px;margin-right:0px">
  <p class="govuk-phase-banner__content">
    <strong class="govuk-tag govuk-phase-banner__content__tag ">beta</strong>
    <span class="govuk-phase-banner__text">We are aware of performance issues that require some users to reload the page. We are working to fix this.
</span>
  </p>
</div>'
  ),

  # Force page to scroll to top when links clicked
  tags$script(
    " $(document).ready(function () {
         $('#navbar a[data-toggle=\"tab\"]').on('click', function (e) {
          window.scrollTo(0, 0)
               });
               });"
  ),
  tags$script(
    " $(document).ready(function () {
                $('#link_to_tabpanel_overview').on('click', function (e) {
                window.scrollTo(0, 0)
                });
                });"
  ),
  tags$script(
    " $(document).ready(function () {
                $('#link_to_tabpanel_employment').on('click', function (e) {
                window.scrollTo(0, 0)
                });
                });"
  ),
  tags$script(
    " $(document).ready(function () {
                $('#link_to_tabpanel_vacancies').on('click', function (e) {
                window.scrollTo(0, 0)
                });
                });"
  ),
  tags$script(
    " $(document).ready(function () {
                $('#link_to_tabpanel_enterprise').on('click', function (e) {
                window.scrollTo(0, 0)
                });
                });"
  ),
  tags$script(
    " $(document).ready(function () {
                $('#link_to_tabpanel_FE').on('click', function (e) {
                window.scrollTo(0, 0)
                });
                });"
  ),
  tags$script(
    " $(document).ready(function () {
                $('#link_to_tabpanel_qualification').on('click', function (e) {
                window.scrollTo(0, 0)
                });
                });"
  ),
  tags$script(
    " $(document).ready(function () {
                $('#link_to_tabpanel_destinations').on('click', function (e) {
                window.scrollTo(0, 0)
                });
                });"
  ),
  tags$script(
    " $(document).ready(function () {
                $('#link_to_tabpanel_data').on('click', function (e) {
                window.scrollTo(0, 0)
                });
                });"
  ),
  tags$script(
    " $(document).ready(function () {
                $('#link_to_tabpanel_employment2').on('click', function (e) {
                window.scrollTo(0, 0)
                });
                });"
  ),
  tags$script(
    " $(document).ready(function () {
                $('#link_to_tabpanel_vacancies2').on('click', function (e) {
                window.scrollTo(0, 0)
                });
                });"
  ),
  tags$script(
    " $(document).ready(function () {
                $('#link_to_tabpanel_destinations2').on('click', function (e) {
                window.scrollTo(0, 0)
                });
                });"
  ),
  tags$script(
    " $(document).ready(function () {
                $('#link_to_tabpanel_enterprise2').on('click', function (e) {
                window.scrollTo(0, 0)
                });
                });"
  ),
  tags$script(
    " $(document).ready(function () {
                $('#link_to_tabpanel_FE2').on('click', function (e) {
                window.scrollTo(0, 0)
                });
                });"
  ),
  tags$script(
    " $(document).ready(function () {
                $('#link_to_tabpanel_qualification2').on('click', function (e) {
                window.scrollTo(0, 0)
                });
                });"
  ),

  # 2 Main page ----
  navlistPanel(
    id = "navbar",
    widths = c(2, 10),
    well = FALSE,
    selected = "Local skills",

    ## 2.1 Homepage ----

    tabPanel(
      "Homepage",
      ### 2.1.1 Intro ----
      fluidRow(column(
        12,
        h1("Local Skills Dashboard"),
        p(
          "This dashboard brings together published statistics on local employment and skills in England, to support local skills planning and delivery."
        ),
        p(
          "It includes a subset of employment and skills statistics that can be viewed for three geographic areas: Local Enterprise Partnership (LEP), Local Skills Improvement Plan (LSIP) and Mayoral Combined Authority (MCA) areas. The underlying data can be downloaded using the links on each page or directly from the downloads page and contains breakdowns by Local Authority (LA) and region."
        ),
        p(
          "The dashboard currently uses data published by the Office for National Statistics (ONS) and the Department for Education (DfE). The sources currently included in the dashboard will be added to alongside additional functionality in response to user feedback and new sources being published."
        ),
        p(
          "To access the additional dashboards developed to help users further understand the labour market outcomes of training use the links below, or from the ",
          a(
            href = "https://www.gov.uk/government/groups/unit-for-future-skills",
            "Unit for Future Skills",
            .noWS = c("after")
          ),
          " webpage."
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
          )
        )
      )),
      # end intro text row

      ### 2.1.2 Contents ----
      fluidRow(column(
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
            p(
              "The tabs along the top of Local Skills page focus on different parts of the jobs and skills market. Each page includes options to download all of the indicators shown, either just for the selected geographic area or for all available geographies."
            ),
            p(
              "Where published figures are not available, area totals for LEP, LSIP or MCA are calculated by adding up the relevant local authorities - rounding errors may be present in these geographic areas where local authority total volumes are rounded and small volumes are suppressed."
            ),
            p(
              "The ONS have announced that, due to a coding error, their occupational data should be used with caution. For more information see this ONS ",
              a(
                href = "https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/articles/theimpactofmiscodingofoccupationaldatainofficefornationalstatisticssocialsurveysuk/2022-09-26",
                "article",
                .noWS = c("after")
              ),
              "."
            ),
            h3(actionLink("link_to_tabpanel_overview", "Overview")),
            p(
              "This tab provides a summary of labour market and skills information for the selected geographic area. It includes a time series of data on employment, online job adverts, and further education and skills achievements."
            ),
            h3(actionLink(
              "link_to_tabpanel_employment", "Employment"
            )),
            p(
              "This tab provides information on employment for the selected geographic area including data on employment rates over time, the share of employment by occupation, and the share of employment by industry. There is an option to compare against another area in England at the same geographic level."
            ),
            p(
              "SOC2020 data is available for the latest period via NOMIS but is not included here due to ongoing ONS coding issues."
            ),
            h3(
              actionLink("link_to_tabpanel_vacancies", "Online job adverts")
            ),
            p(
              "This tab provides experimental online job advert data, split by profession, for the selected geographic area, and the option to compare against another area in England at the same geographic level. "
            ),
            h3(actionLink(
              "link_to_tabpanel_enterprise", "Enterprises"
            )),
            p(
              "This tab provides data on the count of new and no longer trading enterprises and count of enterprises by employment size and industry for the selected geographic area, and the option to compare against another area in England at the same geographic level."
            ),
            h3(actionLink("link_to_tabpanel_FE", "Skills")),
            p(
              "This tab provides information on training activity for the selected geographic area including data on achievements for further education and skills training, with breakdowns for type of training over time and subject area for the latest time period. There is an option to compare against another area in England at the same geographic level."
            ),
            p(
              "LEP, LSIP and MCA area totals are calculated by aggregating the relevant local authorities."
            ),
            h3(
              actionLink("link_to_tabpanel_qualification", "Qualification level")
            ),
            p(
              "This page includes information on the highest qualification level for working age individuals (16-64) for the selected geographic area, and the option to compare against another area.
                It includes data on qualification level, with breakdowns by age band and gender."
            ),
            h3(actionLink(
              "link_to_tabpanel_destinations", "Destinations"
            )),
            p(
              "This page includes information on the destinations of young people after KS4 and KS5 education for the selected geographic area, and the option to compare against another area.
                It includes data on destinations, with breakdowns by level and key stage group."
            ),
            h2("Data download page"),
            p(
              "The",
              actionLink("link_to_tabpanel_data", "data download page"),
              " includes details on the sources of data used in this dashboard, and includes options to download some or all of the data."
            ),
          )
        )
      )),
      # end of dashboard contents row

      ### 2.1.3 Version control ----
      fluidRow(column(
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
            p("25 Jan 2023 (0.4.2)"),
            p(
              "Updated employment rates and employment by industry figures with Oct-Sep 2022 Annual Population Survey data."
            ),
            details(
              label = "Previous updates",
              inputId = "PreviousUpdate",
              p(
                p("12 Jan 2023 (0.4.1)"),
                p("Included ONS-Textkernel online job adverts by profession data."),
                p("20 Dec 2022 (0.3.1)"),
                p("Addition of three new data sources:"),
                tags$ul(
                  tags$li("Highest qualification by age and gender"),
                  tags$li("KS4 and KS5 destinations"),
                  tags$li("Enterprise count by employment size and industry")
                ),
                p("29 Nov 2022 (0.2.3)"),
                p("Applied various improvements and updates, including:"),
                tags$ul(
                  tags$li("Included MCA areas."),
                  tags$li(
                    "Employment data for LEPs, LSIPs and MCAs taken directly from the APS data (previously aggregated from LAs)."
                  ),
                  tags$li("Industry (SIC 2007) breakdown added to the Employment page."),
                  tags$li("Characteristic and course breakdowns added to the Skills page."),
                  tags$li("Updated ILR data to the final AY21/22 data.")
                ),
                p("25 Oct 2022 (0.2.2)"),
                p("Applied fixes and included new datasets to the data sources page."),
                br(),
                p("18 Oct 2022 (0.2.1)"),
                p(
                  "Added option to view LSIP geographies, included new data download page, and updated dashboard styling."
                ),
                br(),
                p("16 Sep 2022 (0.1.2)"),
                p("Applied fixes to improve background performance."),
                br(),
                p("8 Aug 2022 (0.1.1)"),
                p("Release initial prototype.")
              )
            ),
            h2("Future development"),
            p(
              "The dashboard will be kept up to date with the latest data shortly after it is released – check the data downloads page for dates when new data is published."
            ),
            p(
              "We will be regularly adding more data and visualisations to the dashboard based on the user feedback we have received.  If there are further data or dashboard features that you would find useful please contact us at ",
              a(
                href = "mailto:ufs.contact@education.gov.uk",
                "ufs.contact@education.gov.uk",
                .noWS = c("after")
              ),
              "."
            )
          )
        )
      )) # end of version control row
    ),
    # end of homepage Panel

    ## 2.2 Overview ----
    panel_overview(),

    ## 2.3 Local skills ----
    tabPanel(
      "Local skills",
      br(),
      ### 2.3.1 Filters ----
      fluidRow(
        column(
          9,
          h3("What are you interested in?"),
          pickerInput(
            inputId = "splashMetric",
            choices = metricChoices,
            multiple = FALSE,
            choicesOpt = list(
              disabled = unlist(metricChoices) %in% c("workingFutures"),
              style = ifelse(
                unlist(metricChoices) %in% c("workingFutures"),
                yes = "color: rgba(119, 119, 119, 0.5);",
                no = ""
              )
            )
          )
        ),
        column(
          3,
          uiOutput("screenshotFile")
        )
      ),
      fluidRow(
        column(
          12,
          p(uiOutput("subheading"))
        )
      ),
      ### 2.3.2 Visuals row 1 ----
      fluidRow(
        column(
          6,
          h3(uiOutput("titleMap")),
          radioGroupButtons(
            inputId = "splashGeoType",
            choices = c("LEP", "LSIP", "MCA")
          ),
          p(uiOutput("commentMap")),
          leafletOutput("map")
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
      ### 2.3.3 Visuals row 2 ----
      fluidRow(
        column(
          6,
          h3(uiOutput("titleBreakdown")),
          uiOutput("breakdownFilter"),
          uiOutput("professionFilter"),
          uiOutput("subgroupFilter"),
          p(uiOutput("commentBreakdown")),
          plotlyOutput("Splash_pc")
        ),
        column(
          6,
          h3(uiOutput("titleLaMap")),
          p(uiOutput("commentLA")),
          leafletOutput("mapLA")
        )
      ),
      ### 2.3.3 Data notes ----
      fluidRow(column(
        12,
        h2("Data notes"),
        p(uiOutput("dataSource")),
        p(uiOutput("dataNote")),
        p("Caveats:"),
        p(uiOutput("dataCaveat"))
      )),
      br()
    ),

    ## 2.4 Data info and download hub ----
    tabPanel(
      "Data info and download",
      fluidRow(column(
        12,
        h1("Data download"),
        p(
          "Use the filters to create a bespoke dataset. * fields are mandatory. Once you have set your filters, make a note of your unique code and you can recreate your filters whenever you want."
        )
      )),
      fluidRow(column(
        12,
        p("If you have a query code, enter it here:"),
        textInput("hubCode", label = NULL),
      )),
      ### 2.4.1 Datahub filters ----
      fluidRow(column(12, h2("Geography"))),
      fluidRow(
        column(
          4,
          selectizeInput(
            "hubGeog",
            choices = c("LEP", "LSIP", "MCA", "LA"),
            multiple = TRUE,
            label = NULL,
            options = list(placeholder = "Choose geographies*")
          )
        ),
        column(
          4,
          uiOutput("hubAreaInput")
        )
      ),
      fluidRow(
        column(
          4,
          selectizeInput(
            "hubLA",
            choices = c("Yes", "No"),
            label = NULL,
            multiple = TRUE,
            options = list(maxItems = 1, placeholder = "Include LA level data?")
          )
        ),
        column(
          4,
          selectizeInput(
            "hubComparators",
            label = NULL,
            choices = c("National", "Regional (to come)"),
            multiple = TRUE,
            options = list(placeholder = "Include national/regional data?")
          )
        )
      ),
      fluidRow(column(12, h2("Data"))),
      fluidRow(
        column(
          4,
          uiOutput("hubMetricInput")
        ),
        column(
          4,
          uiOutput("hubBreakdownInput")
        ),
        column(
          4,
          uiOutput("hubYearInput")
        )
      ),
      fluidRow(column(12, h2("Output"))),
      fluidRow(column(
        12,
        p("If you want to reuse this query, use this code:"),
        p(uiOutput("uniqueCode")),
      )),
      fluidRow(column(
        3,
        downloadButton(
          outputId = "hubDownload",
          label = "Download this data",
          icon = shiny::icon("download"),
          class = "downloadButton"
        )
      )),
      br(),
      ### 2.4.2 Datahub table ----
      fluidRow(column(
        12,
        dataTableOutput("hubTable")
      )),
      br(),
      ## 2.5 Data infomation ----
      ### 2.5.1 Data sources table ----
      fluidRow(column(
        12,
        h1("Data sources"),
        dataTableOutput("DataTbl"),
        uiOutput("hidden_downloads")
      )),
      # end of data table row
      ### 2.5.2 Data details text ----
      fluidRow(
        column(
          12,
          h2("Data information"),
          h3("Annual Population Survey"),
          p(
            "The Annual Population Survey (APS) is a continuous household survey covering the UK.
          Topics included cover employment and unemployment, and education as well as housing, ethnicity and religion.
            This dashboard currently shows employment volumes and rates for each geographic area and by occupation (SOC2010) and industry (SIC 2007)."
          ),
          p(
            "The ONS have announced that, due to a coding error, their occupational data should be used with caution. For more information see this ONS  ",
            a(
              href = "https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/articles/theimpactofmiscodingofoccupationaldatainofficefornationalstatisticssocialsurveysuk/2022-09-26",
              "article",
              .noWS = c("after")
            ),
            "."
          ),
          p(
            "SOC2020 data is available for the latest period via NOMIS but is not included here due to ongoing ONS coding issues."
          ),
          h3("ONS-Textkernel online job adverts"),
          p(
            "These data tables are based on experimental data based on Textkernel online job adverts. Textkernel data is web-scraped from job advert information from approximately 90,000 job boards and recruitment pages.
            The dashboard shows the monthly average number of live adverts from 2017 to 2022."
          ),
          p(
            "Advert volumes are shown split by profession. Textkernel have derived these professions from the job advert job title. These professions do not align directly to the Standard Occupation Classification (SOC2020). ONS are working on using SOC coding in future releases of this data."
          ),
          p(
            "Counts have been rounded to the nearest 5 and so totals may not add due to this rounding. The scope of online job adverts does not fully capture the scope of UK economic activity because of differing advertising methods, for example, casual work may be advertised by word-of-mouth or in shop windows as opposed to online."
          ),
          p(
            "As this data is experimental, there are some quality issues with the data. The ONS dataset has a full rundown on its cover sheet (link on downloads page). In brief:"
          ),
          tags$ul(
            tags$li(
              "There are methodological changes throughout the time series (classification of profession and location) that may result in step-changes. "
            ),
            tags$li(
              "When job location information is limited, the centroid of the region is used. This may lead to clustering of job counts."
            ),
          ),
          h3("Individualised Learner Record"),
          p(
            "The Individualised Learner Record (ILR) is an on-going collection of data about learners from training providers in the further education and skills sector in England.
          The dashboard shows further education and skills learner achievements over time split by apprenticeships, community learning, education and training."
          ),
          p(
            "LEP, LSIP and MCA area totals are calculated by adding up the relevant local authorities,
            rounding errors may be present in these geographic areas as local authority total volumes are rounded and small volumes are suppressed.
Per 100,000 figures for LEP/LSIP/MCA areas are based on subgroup populations calculated from the ILR dataset."
          ),
          h3("KS4 and KS5 destinations"),
          p(
            "Statistics compiled from the National Pupil Database (NPD) showing the number of young people going into education, employment or an apprenticeship
            in the academic year following completion of their qualification:"
          ),
          tags$ul(
            tags$li(
              "Key Stage 4 (year 10 and 11 students) includes GCSEs and equivalent qualifications"
            ),
            tags$li(
              "Key Stage 5 (students 16-18 years) includes A Levels, T levels and other 2-year vocational education programmes."
            )
          ),
          p(
            "LEP, LSIP and MCA area totals are calculated by adding up the relevant local authorities,
            rounding errors may be present in these geographic areas as local authority total volumes are rounded and small volumes are suppressed.
            Some new local authorities due to boundary changes are not included due to these changes not being updated in data publications."
          ),
          p(
            "The dashboard currently shows the number of young people going into education and, employment or apprenticeship in the year following completion of their qualification by level, KS4 and KS5."
          ),
          h3("UK Business Counts (UBC)"),
          p(
            "The UK Business Counts (UBC) is a record of the number of enterprises from the Inter Departmental Business Register (IDBR). Topics included cover employment size band, detailed industry (5 digit SIC 2007) and legal status.
            The dashboard currently shows the count of enterprises by employment size and industry."
          ),
          h3("ONS Business Demography 2021"),
          p(
            "The business demography data comes from the IDBR and the main administrative sources for the IDBR are VAT trader and PAYE employer information passed to the ONS by HM Revenue & Customs under the Value Added Tax Act 1994 for VAT traders and the Finance Act 1969 for PAYE employers; details of incorporated businesses are also passed to ONS by Companies House."
          ),
          p(
            "The dashboard currently shows the number of new and no longer trading enterprises from 2016 to 2021. Business demography measures businesses that were active throughout the reference year.
            The reference period is December, and therefore the 2021 publication measures businesses that were active between December 2020 and December 2021."
          ),
          p(
            "LEP, LSIP and MCA area totals are calculated by adding up the relevant local authorities,
            rounding errors may be present in these geographic areas as local authority total volumes are rounded and small volumes are suppressed."
          ),
          br()
        )
      )
    ),

    ## 2.6 FE interventions and sources ----
    tabPanel(
      "FE sources and interventions",
      fluidRow(
        column(
          12,
          h1("FE data sources"),
          p(
            "In the below table you can find a list of other data sources which cover Further Education and local skills."
          ),
          DT::dataTableOutput("sourcesTable"),
          br()
        )
      ),
      fluidRow(
        column(
          12,
          h1("FE interventions"),
          p(
            "There have been several interventions and policies in recent years which have affected FE. Below provides a list of interventions in FE which hopefully provide some context around the data and trends within this dashboard."
          ),
          h3(2021),
          tags$div(tags$ul(
            tags$li(
              "The Department for Education White Paper created the policy for Local Skills Improvement Plans and the involvement of employer bodies."
            ),
            tags$li(
              "Skills for Jobs: Lifelong Learning for Opportunity and Growth policy paper suggests the Lifetime Loan Entitlement and increased IfATEs role in technical qualifications."
            ),
            tags$li(
              "The Skills and Post-16 education bill confirmed the introduction of the Lifelong Loan Entitlement."
            )
          )),
          h3(2020),
          tags$div(tags$ul(
            tags$li(
              "The Chancellor announced 'Plan for Jobs' in July 2020, to help combat the impact of the pandemic on the education system. There was a particular focus on apprenticeships, with £2,000 for each new apprentice under 25, later raised to £3,000."
            ),
            tags$li(
              "In September 2020 as part of 'Build Back Better', the Lifetime Skills Guarantee was introduced to give adults without a full Level 3 qualification the chance to take free college courses.
                      SKills Bootcamps were also introduced  offering flexible courses of 12-16 weeks to build up digital skills (later expanded in 2021 and 022 to cover construction, engineering and green skills)."
            ),
            tags$li(
              "Independent Commissions on the College of the Future report was published. This called for long-term education and skills reforms, and extra investment to address current and future skilsl gaps."
            )
          )),
          h3(2019),
          tags$div(tags$ul(
            tags$li("The Adult Education Budget (AEB) devolved to six MCAs and the GLA."),
            tags$li(
              "The Auger report, and independent panel report to the Reveiw of Post-18 Education and Funding was published.
                      This called for a major overhaul of the sturcture and funding system for HE and FE, including rationalising the FE college ‘network’, first free full level 2 and 3 qualification for all learners, simplifying the ESFA funding rules, investing in the FE workforce as a ‘priority' and introducing maintenance support for level 4 and 5 qualifications."
            )
          )),
          h3(2018),
          tags$div(tags$ul(
            tags$li(
              "The Technical and Further Education Act extended the remite of IfA to cover college-based technical education from 2018 onwards, creating IfATE."
            )
          )),
          h3(2017),
          tags$div(tags$ul(
            tags$li(
              "Plans for establishing 'Institutes of Technology' were published by DfE. £170 million fund to ‘establish high quality institutions which specialised in delivering high-level technical skills that employers need across all regions of England’. First 12 successful bids were announced in 2019."
            ),
            tags$li(
              "The Education Funding Agency and the SFA were merged to create the Education and Skills Funding Agency (ESFA) that sits within DfE."
            ),
            tags$li(
              "IfA formally became operational alongside the new apprenticeship levy in April 2017."
            ),
            tags$li("UKCES was closed following withdrawal of government funding.")
          )),
          h3(2016),
          tags$div(tags$ul(
            tags$li(
              "Lord Sainsbury report was published, a report of the Independent Panel on Technical Education. This called for new approach to technical education, including: a technical pathway for learners including employment-based (apprenticeship) and college-based training, a single framework of standards for both apprenticeships and college-based provision, a common framework of 15 routes for levels 2 to 5."
            ),
            tags$li(
              "Post-16 skills plan published alongside to deliver recommendations."
            )
          ))
        )
      ),
      br()
    ),
    ## 2.8 Accessibility ----
    tabPanel(
      "Accessibility",
      fluidRow(
        column(
          width = 12,
          h1("Accessibility statement"),
          p(
            "This accessibility statement applies to the Local skills dashboard.
            This dashboard is run by the Department for Education. We want as many people as possible to be able to use this application,
            and have actively developed this dashboard with accessibilty in mind."
          ),
          h2("WCAG 2.1 compliance"),
          p(
            "We follow the reccomendations of the ",
            a(href = "https://www.w3.org/TR/WCAG21/", "WCAG 2.1 requirements. "),
            "This application has been checked using the ",
            a(href = "https://github.com/ewenme/shinya11y", "Shinya11y tool "),
            ", which did not detect accessibility issues.
             This dashboard also fully passes the accessibility audits checked by the ",
            a(href = "https://developers.google.com/web/tools/lighthouse", "Google Developer Lighthouse tool"),
            ". This means that this dashboard:"
          ),
          tags$div(tags$ul(
            tags$li("uses colours that have sufficient contrast"),
            tags$li(
              "allows you to zoom in up to 300% without the text spilling off the screen"
            ),
            tags$li(
              "has its performance regularly monitored, with a team working on any feedback to improve accessibility for all users"
            )
          )),
          h2("Limitations"),
          p(
            "We recognise that there are still potential issues with accessibility in this dashboard, but we will continue
             to review updates to technology available to us to keep improving accessibility for all of our users. For example, these
            are known issues that we will continue to monitor and improve:"
          ),
          tags$div(tags$ul(
            tags$li(
              "Keyboard navigation through the interactive charts is currently limited, and some features are unavailable for keyboard only users"
            ),
            tags$li(
              "Alternative text in interactive charts is limited to titles and could be more descriptive (although this data is available in csv format)"
            )
          )),
          h2("Feedback"),
          p(
            "If you have any feedback on how we could further improve the accessibility of this dashboard, please contact us at",
            a(href = "mailto:statistics.development@education.gov.uk", "statistics.development@education.gov.uk")
          ),
          br()
        )
      )
    ),
    # End of accessibility tab

    ## 2.9 Support ----
    tabPanel(
      "Support and feedback",
      support_links() # defined in R/supporting_links.R))
    )
  ),
  # End of navBarPage
  # 3 Footer ----

  shinyGovstyle::footer(TRUE)
)
