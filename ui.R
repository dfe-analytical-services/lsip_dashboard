# 1. Set up page, header, js and css ----
## 1.1 Set up page and browser info ----
fluidPage(
  title = tags$head(tags$link(
    rel = "shortcut icon",
    href = "dfefavicon.png"
  )),
  shinyjs::useShinyjs(),
  dfeshiny::custom_disconnect_message(dashboard_title = site_title),
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "dfe_shiny_gov_style.css"
    )
  ),
  dfeshiny::dfe_cookies_script(),
  dfeshiny::cookies_banner_ui(name = site_title),

  # Set metadata for browser
  tags$html(lang = "en"),
  tags$head(
    tags$meta(name = "application_name", content = "Local Skills Dashboard"),
    tags$meta(name = "description", content = "Data dashboard presenting local skills data from Skills England."),
    tags$meta(name = "subject", content = "Education data dashboards.")
  ),

  # Set title for search engines
  HTML("<title>Local Skills Dashboard</title>"),
  # Setting up cookie consent based on a cookie recording the consent:
  # https://book.javascript-for-r.com/shiny-cookies.html
  tags$head(
    tags$script(
      src = paste0(
        "https://cdn.jsdelivr.net/npm/js-cookie@rc/",
        "dist/js.cookie.min.js"
      )
    ),
    tags$script(src = "cookie-consent.js")
  ),
  tags$head(includeHTML(("google-analytics.html"))),

  ## 1.2. Internal CSS ----
  tags$head(
    tags$style(
      HTML(
        "
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

 /* increase banner size*/
.feedback-banner {
    background-color: #d53880;
    font-size: 16px;
    line-height: 1.5;
    padding: 5px 8px 4px;
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
    <div class="govuk-header__logo" style="margin-left: 15px">
    <a href="https://www.gov.uk/government/organisations/department-for-education" class="govuk-header__link govuk-header__link--homepage">
    <span class="govuk-header__logotype">
     <img src="images/DfE_logo_landscape.png" class="govuk-header__logotype-crown-fallback-image" width="133px"/>
    </span>
    </a>
    </div>
    <div class="govuk-header__content" style="width: 70%; text-align: center;float:left;">
    <a href="https://www.gov.uk/government/collections/skills-england" class="govuk-header__link govuk-header__link--service-name" style="font-size: 24px;">Local Skills Dashboard</a>
    </div>
        <a href="javascript:void(0);" id="menuButton" class="menuBtn" onclick="collapseMenu()">
    <i class="fa fa-bars" style="font-size:24px;"></i></a>
    </div>
    </header>'
  ),

  # Add banner note around new LSIPs
  HTML(
    '<div class="feedback-banner" id="feedback banner" >
    <p style="color: #fff;">Some LSIP boundaries have been updated in line with the next cycle of LSIP development beginning October 2025, and new combined authorities have been added. See data sources page for more information.</p>
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
  tags$script(
    " $(document).ready(function () {
                $('#link_to_tabpanel_localskills').on('click', function (e) {
                window.scrollTo(0, 0)
                });
                });"
  ),
  tags$script(
    " $(document).ready(function () {
                $('#link_to_tabpanel_localskills2').on('click', function (e) {
                window.scrollTo(0, 0)
                });
                });"
  ),
  tags$script(
    " $(document).ready(function () {
                $('#link_to_tabpanel_furtherresources').on('click', function (e) {
                window.scrollTo(0, 0)
                });
                });"
  ),
  tags$script(
    " $(document).ready(function () {
                $('#link_to_tabpanel_accessibility').on('click', function (e) {
                window.scrollTo(0, 0)
                });
                });"
  ),
  tags$script(
    " $(document).ready(function () {
                $('#link_to_tabpanel_wf').on('click', function (e) {
                window.scrollTo(0, 0)
                });
                });"
  ),
  tags$script(
    " $(document).ready(function () {
                $('#link_to_tabpanel_wf1').on('click', function (e) {
                window.scrollTo(0, 0)
                });
                });"
  ),
  tags$script(
    " $(document).ready(function () {
                $('#link_to_tabpanel_LS').on('click', function (e) {
                window.scrollTo(0, 0)
                });
                });"
  ),

  # 2 Main page ----
  navlistPanel(
    id = "navbar",
    widths = c(2, 10),
    well = FALSE,
    selected = "Summary",

    ## 2.1 User guide ----

    tabPanel(
      "User guide",
      ### 2.1.1 Intro ----
      fluidRow(column(
        12,
        h1("Local Skills Dashboard"),
        p(
          "The Local Skills dashboard provides published local data from a variety of sources in an easy to navigate format. To support local skills planning, the dashboard covers topics such as employment, qualifications, and education outcomes across England."
        ),
        p(
          "Data is available to view and download for various geographies, including: local authority (LA), local skills improvement plan (LSIP) area, Mayoral Combined Authority (MCA) and national."
        ),
        p(
          "This dashboard is produced by ",
          a(
            href = "https://www.gov.uk/government/collections/skills-england",
            "Skills England",
            .noWS = c("after")
          ),
          ", a body that brings together key partners to meet the skills needs of the next decade across all regions. For more information on Skills England's aims and our work, visit our",
          a(
            href = "https://www.gov.uk/government/collections/skills-england",
            "webpage.",
            .noWS = c("after")
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
            h2("How to use this dashboard")
          ),
          div(
            class = "panel-body",
            p("Use the navigation bar on the left to select the tab you want to view."),
            h2("Dashboard structure"),
            tags$ul(
              tags$li(actionLink("link_to_tabpanel_overview", "Summary"), " - this tab provides a summary of some of the key metrics for the selected geographic area."),
              tags$li(actionLink("link_to_tabpanel_localskills", "Local skills data"), " - the Local skills data tab provides additional metrics and breakdowns for the selected geographic area."),
              tags$li(actionLink("link_to_tabpanel_data", "Data sources and Data download"), "- the sources tab includes details on the sources of data used in this dashboard, and the download tab includes options to download some or all of the data."),
              tags$li(actionLink("link_to_tabpanel_furtherresources", "Further resources"), " - provides information and links to additional data sources and cross-government tools for exploration of local labour market and education system."),
              tags$li(actionLink("link_to_tabpanel_accessibility", "Accessibility"), "- provides the Local Skills dashboard accessibility statement, compliance requirmeents, limitations and opportunity to feedback on accessibility of the dashboard."),
              tags$li(actionLink("link_to_tabpanel_supportandfeedback", "Support and feedback"), " - provides links to the Skills England and Department for Education Statistics Development inboxes for feedback and if you hve any questions about the dashboard or the data it contains. There is also a link to the GitHub repository if you wish to view the dashboard source code.")
            ),
            h2("Local skills metrics"),
            p(
              "Where published figures are not available, area totals for LSIP or MCA are either taken from a matching geographical area or are calculated by adding up the relevant local authorities - rounding errors may be present in these geographic areas where local authority total volumes are rounded and small volumes are suppressed."
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
            h3(actionLink(
              "link_to_tabpanel_localskills2", "Local skills data"
            )),
            p(
              "The Local skills data tab provides additional metrics and breakdowns for the selected geographic area. Using the various filters you can select the metric of interest, primary area from the England map, and multiple comparison areas alongside the default England comparator. The chart in the bottom left provides additional breakdowns specific to the metric, for example:"
            ),
            tags$ul(
              tags$li("Occupation and industry for employment volumes"),
              tags$li("Size and industry for business count"),
              tags$li("Learner age, level, provision and sector subject area for further education achievement rates")
            ),
            p(""),
            h3(actionLink(
              "link_to_tabpanel_employment", "Employment"
            )),
            p(
              "This group of metrics provide information on employment, self-employment and inactivity for the selected geographic area including data on employment rates over time, the share of employment by occupation, and the share of employment by industry."
            ),
            h3(
              actionLink("link_to_tabpanel_vacancies", "Jobs")
            ),
            p(
              "This is an experimental metric of online job advert data, split by SOC2020 Sub-Major groups, for the selected geographic area, and the option to compare against another area in England at the same geographic level. "
            ),
            h3(actionLink(
              "link_to_tabpanel_enterprise", "Businesses"
            )),
            p(
              "This group of metrics provide data on the count of new and no longer trading enterprises and count of enterprises by employment size and industry for the selected geographic area, and the option to compare against another area in England at the same geographic level."
            ),
            h3(actionLink("link_to_tabpanel_FE", "Skills")),
            p(
              "This group of metrics provide information on training activity for the selected geographic area including data on achievements for further education and skills training, with breakdowns for type of training over time and subject area for the latest time period."
            ),
            h3(actionLink(
              "link_to_tabpanel_destinations", "Destinations"
            )),
            p(
              "These two metrics provide information on the destinations of young people after Key Stage 4 and Key Stage 5 education for the selected geographic area.
                It includes data on destinations, with breakdowns by level and key stage group."
            ),
            h3(actionLink(
              "link_to_tabpanel_wf1", "Employment projections"
            )),
            p(
              "Projected employment growth until 2035. Sector, industry, occupation and qualification projected growths are available. LA level data is not available for this dataset."
            )
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
            p("16 Sep 2025 (1.6.0)"),
            tags$ul(
              tags$li("Update to latest LSIP geographies."),
              tags$li("Add new MCAs.")
            ),
            details(
              label = "Previous updates",
              inputId = "PreviousUpdate",
              p(
                p("02 Sep 2025 (1.5.2)"),
                tags$ul(
                  tags$li("Update to latest online job advert data."),
                  tags$li("Reorganise the filter methodology to speed up the dashboard."),
                  tags$li("Small bug fixes.")
                ),
                p("22 Aug 2025 (1.5.1)"),
                tags$ul(
                  tags$li("Refresh of the summary page."),
                  tags$li("Remove LEP maps and add in National picture."),
                  tags$li("Add in page wide subgroup filters to the Local Skills data page."),
                  tags$li("Update Skills Imperative projection long term growth metric to 2024 to 2035."),
                  tags$li("Utilise the populations published in FE data to calculate grouped FE per 100k statistics.")
                ),
                p("5 Aug 2025 (1.4.17)"),
                tags$ul(
                  tags$li("Update to latest online job advert data.")
                ),
                p("28 Jul 2025 (1.4.16)"),
                tags$ul(
                  tags$li("Add in North Yorkshire LAD to the comparison drop down list."),
                  tags$li("Correction to Enterprise M3 employment projections to 2035. Previous versions used the incorrect published figures. See Data information tab for more information on the correction.")
                ),
                p("18 Jul 2025 (1.4.15)"),
                tags$ul(
                  tags$li("Update to latest APS data.")
                ),
                p("11 Jul 2025 (1.4.14)"),
                tags$ul(
                  tags$li("Update to latest online job advert data.")
                ),
                p("14 May 2025 (1.4.13)"),
                tags$ul(
                  tags$li("Update to latest online job advert data."),
                  tags$li("Update to latest qualification data.")
                ),
                p("25 Apr 2025 (1.4.12)"),
                tags$ul(
                  tags$li("Updated APS employment to latest data."),
                  tags$li("Update to latest online job advert data.")
                ),
                p("27 Mar 2025 (1.4.11)"),
                tags$ul(
                  tags$li("Update to latest revised destination data."),
                  tags$li("Update to latest online job advert data.")
                ),
                p("17 Feb 2025 (1.4.10)"),
                tags$ul(
                  tags$li("Update to latest online job advert data.")
                ),
                p("6 Feb 2025 (1.4.9)"),
                tags$ul(
                  tags$li("Updated the MCA map to include the new combined authorities (East Midlands, York and North Yorkshire) and the combination of North East MCA and North of Tyne MCA into North East MCA.")
                ),
                p("30 Jan 2025 (1.4.8)"),
                tags$ul(
                  tags$li("Updated business demography to latest data."),
                  tags$li("Updated further education enrolments and achievements to latest data."),
                  tags$li("Updated APS employment to latest data.")
                ),
                p("12 Dec 2024 (1.4.7)"),
                tags$ul(
                  tags$li("Updated job adverts to latest, Nov24"),
                  tags$li("Added comment that explains North of Tyne and North East MCA have been temporarily
                      removed")
                ),
                p("18 Oct 2024 (1.4.5)"),
                tags$ul(
                  tags$li("Updated Annual Population Survey data to Jul 2023 - Jun 2024."),
                  tags$li("Update to reflect the dashboard is now run by Skills England.")
                ),
                p("5 Aug 2024 (1.4.4)"),
                tags$ul(
                  tags$li(
                    "Revised Skills Imperative employment projections using corrected data published by the Office for National Statistics (ONS). ",
                    a(
                      href = "https://www.nfer.ac.uk/key-topics-expertise/education-to-employment/the-skills-imperative-2035/publications",
                      "Revised employment and skills projections for the Skills Imperative 2035",
                      .noWS = c("after", "before")
                    ),
                    " from the National Foundation for Educational Research (NFER) has information about the impact of the corrected data. In brief, the LFS coding error affects the projections of employment by occupation but not the projections of employment by industry. The errors have a very small effect on which skills are projected to be most intensively utilised across the labour market in 2035. The errors do affect how much each occupation is expected to grow or decline as a share of all employment, between 2021 and 2035. The degree to which this share differs between the new and old projections varies by occupational group."
                  )
                ),
                p("22 Jul 2024 (1.4.3)"),
                tags$ul(
                  tags$li("Updated Annual Population Survey data to Apr 2023 - Mar 2024.")
                ),
                p("10 Jul 2024 (1.4.2)"),
                tags$ul(
                  tags$li("Updated job adverts data to latest version including data up to December 2023.")
                ),
                p("20 Jun 2024 (1.4.1)"),
                tags$ul(
                  tags$li("Fixed package error which stopped charts plotting.")
                ),
                p("17 May 2024 (1.4.0)"),
                tags$ul(
                  tags$li("Updated the job adverts data to the latest online job adverts from Textkernel, now split by SOC2020 Sub-Major groups (previously TextKernel's own profession split was used)."),
                  tags$li("Made a correction to the achievement split by SSA to use total aims as the denominator, and the provision split to include under 19 apprenticeships and community learning in the denominator."),
                  tags$li("Add SOC codes to employment data SOC labels."),
                  tags$li("Updated cookie banner."),
                  tags$li("Updated APS employment rates, employment by industry and occupation data to the latest quarter and amended age ranges and base populations to be in line with ONS publication of this data."),
                  tags$li("Updated highest qualification data to use National Qualifications Framework allowing more recent data."),
                  tags$li("Added in highest qualification at level four plus metric."),
                  tags$li("Added a feedback survey."),
                  tags$li("Added new resources to the Further Resources tab."),
                ),
                p("25 March 2024 (1.3.9)"),
                tags$ul(
                  tags$li("Calculate the bar chart splits based on percentage of the whole area (previously percentage of subgroups with data present)."),
                ),
                p("25 January 2024 (1.3.8)"),
                tags$ul(
                  tags$li("Update to latest APS employment data.")
                ),
                p("12 January 2024 (1.3.7)"),
                tags$ul(
                  tags$li("Update to latest ILR FE data data. This includes using some data now published at LEP and LSIP level."),
                  tags$li("Update geographies to latest LEPs and LAs. This includes the closure of Black Country and Coventry & Warwickshire LEPs.")
                ),
                p("19 December 2023 (1.3.6)"),
                tags$ul(
                  tags$li("Update to latest business demographies data.")
                ),
                p("25 October 2023 (1.3.5)"),
                tags$ul(
                  tags$li("Update to latest KS4 and KS5 destinations data."),
                  tags$li("Update to latest APS employment data.")
                ),
                p("19 October 2023 (1.3.4)"),
                tags$ul(
                  tags$li("Update to latest business count data.")
                ),
                p("12 September 2023 (1.3.3)"),
                tags$ul(
                  tags$li("Updated FE sources table."),
                  tags$li("Updated map colours for clarity."),
                  tags$li("Corrected the breakdown table comment text.")
                ),
                p("23 August 2023 (1.3.2)"),
                tags$ul(
                  tags$li("Updated Nomis employment rates, employment by industry and occupation data to the latest quarter."),
                  tags$li("Started using SOC2020 data (replacing SOC2010 data) following the ONS correction to the data."),
                ),
                p("23 June 2023 (1.3.1)"),
                p("Corrected the Worcestershire LEP and Stoke-on-Trent and Staffordshire LEP LA level map to include all areas."),
                p("16 June 2023 (1.3.0)"),
                p(
                  "Started using the Nomis API to get NOMIS data. More information on the API ",
                  a(
                    href = "https://www.nomisweb.co.uk/api/v01/help",
                    "here",
                    .noWS = c("after", "before")
                  ),
                  "."
                ),
                p("10 May 2023 (1.2.2)"),
                tags$ul(
                  tags$li("Updated Nomis employment rates and employment by industry data to the latest quarter."),
                  tags$li("Updated ILR data with the latest revision (time period covered has stayed the same)"),
                  tags$li("Corrections to Dorset LSIP, Enterprise M3 LEP (including all of Surrey) LSIP, Stoke-on-Trent and Staffordshire LEP, and Worcestershire LEP. In the case of of the Enterprise M3 LEP (including all of Surrey) LSIP we have used a temporary fix where we compile the Skills Imperative and job advert data from other LEPs and LSIPs. There may therefore be some rounding issues with this LSIP. We are working on republishing that data with the correction. All other data shown in the dashboard is accurate."),
                  tags$li("Correction to include data for some LAs which used discontinued LA naming."),
                  tags$li("Code refresh and tidy."),
                ),
                p("21 Mar 2023 (1.2.1)"),
                p(
                  "Added Skills Imperative 2035 data to the dashboard. This projects employment to 2035 with sector, industry, qualification and occupation splits."
                ),
                p("28 Feb 2023 (1.1.1)"),
                p(
                  "Dashboard redesign including:"
                ),
                tags$ul(
                  tags$li("More granular breakdowns within geographic areas"),
                  tags$li("More area comparison options"),
                  tags$li("Bespoke download option including geographies and all published breakdowns"),
                  tags$li("Expanding on role as ‘local skills hub,’ hosting links to local skills sources and tools")
                ),
                p("9 Feb 2023 (0.4.3)"),
                p("Updated destinations data with the February revision."),
                p("25 Jan 2023 (0.4.2)"),
                p("Updated employment rates and employment by industry figures with Oct-Sep 2022 Annual Population Survey data."),
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
              "The dashboard will be kept up to date with the latest data shortly after it is released – check the data sources page for dates when new data is published. If there are further data or dashboard features that you would find useful please contact us at ",
              a(
                href = "mailto:skills.england@education.gov.uk",
                "skills.england@education.gov.uk",
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
    summaryTab(),

    ## 2.3 Local skills ----
    tabPanel(
      "Local skills data",
      br(),
      ### 2.3.1 Filters ----
      fluidRow(
        column(
          5,
          selectizeInput(
            "geoChoice",
            multiple = FALSE,
            label = "Choose an LSIP, MCA or England",
            choices = areaChoices[1:3],
            options = list(
              persist = TRUE, # keep selected value
              create = FALSE, # disallow new values
              onDelete = I("function(values) { return false; }")
            )
          ),
          selectizeInput(
            "geoComps",
            multiple = TRUE,
            label = NULL,
            choices = areaChoices,
            options = list(
              maxItems = 7,
              placeholder = "Comparison areas (LA/LSIP/MCA/National"
            )
          )
        ),
        column(
          5,
          selectizeInput(
            inputId = "splashMetric",
            choices = metricChoices,
            multiple = FALSE,
            label = "What are you interested in?",
            options = list(
              persist = TRUE, # keep selected value
              create = FALSE, # disallow new values
              onDelete = I("function(values) { return false; }")
            )
          ),
          hidden(
            div(
              id = "breakdownPage_wrapper",
              selectInput("breakdownPage", "Limit to subgroup:", choices = "")
            )
          ),
          hidden(
            div(
              id = "subgroupPage_wrapper",
              selectInput("subgroupPage", "Subgroup:", choices = "")
            )
          )
        ),
        column(
          2,
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
            choices = c("LSIP", "MCA", "England")
          ),
          p(uiOutput("commentMap")),
          withSpinner(leafletOutput("map")),
          p(uiOutput("mapFoot"))
        ),
        column(
          6,
          h3(uiOutput("titleTime")),
          p(uiOutput("commentTime")),
          withSpinner(plotlyOutput("Splash_time")),
          p(uiOutput("timeFoot"))
        )
      ),
      br(),
      ### 2.3.3 Visuals row 2 ----
      fluidRow(
        column(
          6,
          h3(uiOutput("titleBreakdown")),
          hidden(
            div(
              id = "breakdownBar_wrapper",
              selectInput("barBreakdown", "Subgroup type", choices = "")
            )
          ),
          hidden(
            div(
              id = "professionBar_wrapper",
              selectInput("barProfession", "Limit to particular SOC2020 Major group", choices = "")
            )
          ),
          hidden(
            div(
              id = "subgroupBar_wrapper",
              pickerInput(
                inputId = "barSubgroup", label = NULL, choices = "",
                multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE)
              )
            )
          ),
          p(uiOutput("commentBreakdown")),
          uiOutput("breadownPlot"),
          p(uiOutput("breakdownFoot"))
        ),
        column(
          6,
          h3(uiOutput("titleLaMap")),
          p(uiOutput("commentLA")),
          withSpinner(leafletOutput("mapLA")),
          p(uiOutput("mapLaFoot"))
        )
      ),
      ### 2.3.3 Downloads ----
      fluidRow(
        column(
          width = 3,
          downloadButton(
            outputId = "downloadV1All",
            label = "All areas   ",
            icon = shiny::icon("download"),
            class = "downloadButton"
          )
        ),
        column(
          width = 9,
          "Download metric data for all geographies (LSIP, MCA areas, LAs, regions and England)",
        )
      ),
      fluidRow(
        column(
          width = 3,
          downloadButton(
            outputId = "downloadV1Current",
            label = "Current geographic areas",
            icon = shiny::icon("download"),
            class = "downloadButton"
          )
        ),
        column(width = 9, "Download metric data for the selected geographic areas")
      ),
      ### 2.3.3 Data notes ----
      fluidRow(column(
        12,
        h2("Data notes"),
        p("Any NAs or missing data in the charts or maps are due to supressed data."),
        p(uiOutput("dataSource")),
        p(uiOutput("dataNote")),
        p("Caveats:"),
        p(uiOutput("dataCaveat"))
      )),
      br()
    ),

    ## 2.5 Data information ----
    tabPanel(
      "Data sources",
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
          h3("Boundary changes"),
          p(
            "Some LSIP boundaries have changed in advance of the next cycle of LSIP development, beginning October 2025. These boundaries were updated on the dashboard on 19/09/25 affecting the following LSIPs:"
          ),
          tags$ul(
            tags$li("Greater Devon"),
            tags$li("Greater Lincolnshire"),
            tags$li("Hampshire and the Solent"),
            tags$li("Leicester, Leicestershire and Rutland"),
            tags$li("North East"),
            tags$li("Somerset"),
            tags$li("Surrey"),
            tags$li("Warwickshire"),
            tags$li("West Midlands")
          ),
          p(
            "More information on boundary changes is available on the Local skills improvement plans and designated employer representative bodies ",
            a(
              href = "https://www.gov.uk/government/publications/designated-employer-representative-bodies/notice-of-designated-employer-representative-bodies",
              "page on gov.uk",
              .noWS = c("after")
            ),
            "."
          ),
          p("During this update the following changes were also made: London sub-areas were added as individual LSIP areas. Combined London data remains available as a Combined Authority area; the following newly created combined authorities were added:"),
          tags$ul(
            tags$li("Devon and Torbay"),
            tags$li("Greater Lincolnshire"),
            tags$li("Hull and East Yorkshire"),
            tags$li("Lancashire")
          ),
          h3("Annual Population Survey"),
          p(
            "The Annual Population Survey (APS) is a continuous household survey covering the UK.
          Topics included cover employment and unemployment, and education as well as housing, ethnicity and religion.
            This dashboard currently shows employment volumes and rates for each geographic area and by occupation (SOC2020) and industry (SIC 2007)."
          ),
          p(
            "The ONS previously identified a coding error in the collection of SOC2020, although this occupational data has been revised, it is advised to continue to use caution when interpreting these data particularly when looking at year-on-year changes as they may continue to be impacted by this coding issue as well as wider factors such as Covid-19, the UK's exit from the EU, and changes to the economy. For more information see this ONS  ",
            a(
              href = "https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/articles/revisionofmiscodedoccupationaldataintheonslabourforcesurveyuk/january2021toseptember2022",
              "article",
              .noWS = c("after")
            ),
            "."
          ),
          p(
            "Each estimate from the Annual Population Survey carries a margin of error.
            These are available in the original data via NOMIS. Large margins of error are usually associated with groups with only a small number of respondents.
            Therefore, please take caution when interpreting data from small subgroups."
          ),
          p("In Q1 2022 (January to March 2022) the highest qualification variable (HIQUAL) of the LFS was revised. New qualifications have been added after a review identifying gaps in the LFS questionnaire. Therefore figures published after this are not directly comparable to previous years. From 2022 on qualification level is defined by the National Qualifications Framework (NQF). Before that National Vocational Qualifications (NVQ) is used."),
          p(
            "The Labour Force Survey (LFS) supplies data for the APS. The ONS have highlighted increased uncertainty around the LFS estimates due to falling response rates. As a result, ONS have begun to offer alternative sources of employment data, but these are yet to be published in the form used in this dashboard. For more information see this  ",
            a(
              href = "https://blog.ons.gov.uk/2023/10/24/creating-the-best-picture-of-changes-in-the-labour-market-an-update/",
              "article",
              .noWS = c("after")
            ),
            "."
          ),
          h4("Annual Population Survey accreditation"),
          p(
            "ONS published a ",
            a(
              href = "https://osr.statisticsauthority.gov.uk/correspondence/michael-keoghan-to-siobhan-tuohy-smith-request-to-suspend-aps-accreditation/",
              "response to OSR",
              .noWS = c("after")
            ),
            " about the current quality of Annual Population Survey (APS) (and Labour Force Survey) outputs. ONS asked OSR to temporarily suspend accreditation of all APS-based ONS outputs. There has since been a ",
            a(
              href = "https://osr.statisticsauthority.gov.uk/correspondence/ed-humpherson-to-michael-keoghan-suspension-of-the-accredited-official-statistics-status-for-the-estimates-ons-produces-from-the-annual-population-survey/",
              "response letter from OSR",
              .noWS = c("after")
            ),
            "."
          ),
          p("Overall, ONS’ view on the quality of the APS is that while it is robust for national and headline regional estimates, there are concerns with the quality of estimates for smaller segments of the population, such as local authority geographies. ONS will publish an explanatory note later this year providing guidance to users on the quality of current APS and will be used to inform further work ONS is undertaking to improve quality of the survey."),
          h3("ONS-Textkernel online job adverts"),
          p(
            "These data tables are based on experimental data based on Textkernel online job adverts. Textkernel data is web-scraped from job advert information from approximately 90,000 job boards and recruitment pages.
            The dashboard shows the monthly average number of live adverts."
          ),
          p(
            "Advert volumes are shown split by SOC2020 Major and Sub-Major groups. Textkernel have derived these codes from the job advert job title."
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
          p("
            For LSIPs, only the most recent year's data is published. We therefore compile the statistics from LAs for all years except the most recent."),
          p("Rounding errors may be present in these geographic areas as local authority total volumes are rounded and small volumes are suppressed.
Per 100,000 figures for LSIP/MCA areas are based on subgroup populations calculated from the ILR dataset."),
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
            "LSIP and MCA area totals are calculated by adding up the relevant local authorities,
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
            "LSIP and MCA area totals are calculated by adding up the relevant local authorities,
            rounding errors may be present in these geographic areas as local authority total volumes are rounded and small volumes are suppressed."
          ),
          h3("Skills Imperative 2035 employment projections"),
          p(
            "Skills Imperative 2035 projects the future size and shape of the labour market by considering employment prospects by industry, occupation, qualification level.
            The dashboard shows the year on year growth of employment as well as the long term growth from 2024 to 2035.
            The employment volumes are available in the data downloads.
            "
          ),
          p("The projections are calculated from a number of different data sources, and as such precise margin errors have not been assigned.
            Care should be taken when using projections with small volumes of individuals (see Skills Imperative 2035 datasets for more detail). "),
          p("There is an error in the data for Enterprise M3 LSIP which has prevented it from being included in the published data. This is due to the incorrect mapping of LAs. For this LSIP we have estimated the value by looking the broader region and calculating the value of the Enterprise LSIP having removed other LSIPs in the region. This will come with some rounding issues."),
          br()
        )
      )
    ),
    ## 2.4 Download hub ----
    tabPanel(
      "Data download (experimental)",
      fluidRow(column(
        12,
        h1("Data download"),
        p(
          "Use the filters to create a bespoke dataset. * fields are mandatory."
          # Once you have set your filters, make a note of your unique code and you can recreate your filters whenever you want."
        )
      )),
      ### 2.4.1 Datahub filters ----
      fluidRow(column(12, h2("Geography"))),
      fluidRow(
        column(
          4,
          uiOutput("hubAreaInput")
        ),
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
            choices = c("National"),
            multiple = TRUE,
            options = list(placeholder = "Include national data?")
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
      br()
    ),

    ## 2.6 FE interventions and sources ----
    tabPanel(
      "Further resources",
      fluidRow(
        column(
          12,
          h1("Reports"),
          p(
            "Published reports related to local skills."
          ),
          DT::dataTableOutput("reportsTable"),
          br()
        )
      ),
      fluidRow(
        column(
          12,
          h1("Data sources"),
          p(
            "Other data sources which cover local skills and jobs."
          ),
          DT::dataTableOutput("sourcesTable"),
          br()
        )
      ),
      fluidRow(
        column(
          12,
          h1("Data tools"),
          p(
            "Publicly available online tools relevant to local skills."
          ),
          DT::dataTableOutput("toolsTable"),
          br()
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
      "Cookie information",
      dfeshiny::cookies_panel_ui(
        id = "cookies_panel",
        google_analytics_key = google_analytics_key
      )
    ),
    ## 2.10 Support ----
    tabPanel(
      "Support and feedback",
      dfeshiny::support_panel(
        team_email = "skills.england@education.gov.uk",
        repo_name = "https://github.com/dfe-analytical-services/lsip_dashboard",
        form_url = "https://forms.office.com/pages/responsepage.aspx?id=yXfS-grGoU2187O4s0qC-fk-uIY5X_9Grwm9UK_gdoJUNzhPRjkxRUE1MFZXS0lZNDdZVkpUM0Y0Wi4u&route=shorturl",
        custom_data_info = "This dashboard collates data from a wide range of sources. Full details of all sources are provided on the data sources page of this dashboard."
      )
    )
  ),
  # End of navBarPage
  # 3 Footer ----

  shinyGovstyle::footer(TRUE)
)
