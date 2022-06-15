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
# This file uses a slider input, but other inputs are available like date selections, multiple choice dropdowns etc.
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
  title = "DfE Analytical Services R-Shiny Template",
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
  HTML("<title>Local Skils Dashboard</title>"),
  
# Navbar ====================================================================
  
  # This CSS sets the 4th item on the navbar to the right
  tagList(
    tags$head(tags$style(HTML("
                           .navbar-nav {
                           float: none !important;
                           }
                           .navbar-nav > li:nth-child(5) {
                           float: right;
                           }
                           ")))
  ),
  navbarPage("",
             id = "navbar",
             
# Homepage tab ============================================================

             tabPanel(
               "Homepage",
               fluidPage(
                 fluidRow(
                   column(
                     12,
                     img(src='UFS logo.png', align="right"),
                     h1("Local Skills Dashboard"),
                     h3("Welcome"),
                     br(),
                     p("This app XXX"),
                     br(),
                     br()
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
                         #  tags$div(
                        #     title = "This section is useful if you want to understand how well different industries retain graduates.",
                       #      h3(actionLink("link_to_industryFlow_tab", "App Content"))
                       #    ),
                           br()
                         )
                       )
                     ),
                   ),

 ## Right panel ------------------------------------------------------

                   column(
                     6,
                     div(
                       div(
                         class = "panel panel-info",
                         div(
                           class = "panel-heading",
                           style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;",
                           h2("Data sources and background")
                         ),
                         div(
                           class = "panel-body",
                           h2("ONS Annual Population Survey (APS)"),
                           p("XXX"),
                           br(),
                           h2("DfE Individualised Learner Record (ILR)"),
                           p("XXX"),
                           br(),
                           h2("ONS Online job adverts"),
                           p("XXX"),
                           br()
                         )
                       )
                     )
                   )
                 )
               )
             ),
# Local Landscape tab ----
             tabPanel(
               "Local Landscape",
               
               # Define UI for application that draws a histogram
               
               # Sidebar with a slider input for number of bins
               sidebarLayout(
 ## Side panel ----
                 sidebarPanel(
                   width = 2,
  ### Help text --------------------
                   helpText("Choose a Local Area to view employment trends" 
                            ,style = "font-style: italic;"
                   ),
                   br(),
  ### LEP 1 input ---------------
                   selectizeInput("lep1",
                                  "Choose a primary LEP:",
                                  choices=C_LEP2020,
                                  selected="England",
                   ),
  ### LEP 2 input ------------
                   selectizeInput("lep2", 
                                  "Choose a comparison LEP (optional):",
                                  choices=C_LEP2020,
                                  selected="England",
                   ),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
  ### Download button -------------
                   downloadButton(
                     outputId = "download_btn1",
                     label = "Download",
                     icon = shiny::icon("download")
                   ),
                   
                 ), # end of side panel
 ## Main panel ----
                 # Show a plot of the generated distribution
                 mainPanel(
                   width=10,
  ### Title ----
                   box(width=12,
                       uiOutput("page1title", style="font-size: 24px;"),
                   br(),
                   div("Data for employees aged 25-30 in sustained employment in the 2018-19 tax year", style = "font-size: 16px; font-style: italic;"),
                   br(),
                   ),

  ### KPI boxes ----
                   box(width=12,
                       valueBoxOutput("locland.emplrate"),
                       valueBoxOutput("locland.emplcnt"), 
                           ), # end of box
  ### Employment rate over time line chart ----
  box(
    width=12,
  column(width=6,
                              plotlyOutput("EmpRate_time")),
  ### Employment by occupation data table ----
                       column(width=6,
                              dataTableOutput("EmpOcc"))
  )
                 ) # end of main panel
               ) # end of side bar layout
             ), # end of Local Landscape tab
# Skill Supply tab ----
tabPanel(
  "Skill Supply",

  # Define UI for application that draws a histogram

  # Sidebar with a slider input for number of bins
  sidebarLayout(
 ## Side panel ----
    sidebarPanel(
      width = 2,
  ### Help text --------------------
      helpText("Choose a Local Area to view skill supply trends"
               ,style = "font-style: italic;"
      ),
      br(),
  ### LEP 1 input ---------------
      selectizeInput("lep3",
                     "Choose a primary LEP:",
                     choices=C_LEP2020,
                     selected="England",
      ),
  ### LEP 2 input ------------
      selectizeInput("lep4", # Make no selection an option
                     "Choose a comparison LEP (optional):",
                     choices=C_LEP2020,
                     selected="England",
      ),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
  ### Download button -------------
      downloadButton(
        outputId = "download_btn2",
        label = "Download",
        icon = shiny::icon("download")
      ),

    ), # end of side panel
 ## Main panel ----
    # Show a plot of the generated distribution
    mainPanel(
      width=10,
  ### Title ----
      box(width=12,
          uiOutput("page2title", style="font-size: 24px;"),
      br(),
      div("XXX", style = "font-size: 16px; font-style: italic;"),
      br(),
      ), # end of box
  ### KPI boxes ----
      box(width=12,
          valueBoxOutput("skisup.FEach"),
          valueBoxOutput("skisup.APach"), 
    ), # end of box
   
 
      box(width=12,
  ### Employment rate over time line chart ----
  
          column(width=6,
                 p("Place holder for Achievement counts 2017-21")),
                 #plotlyOutput("EmpRate_time")),
  
  ### Employment by occupation data table ----
  
          column(width=6,
                 p("Place holder for Achievements by Sector Subject Area 2021"))
                 #dataTableOutput("EmpOcc"))
    ) # end of box

    ) # end of main panel
  ) # end of side bar layout
), # end of Skills Supply tab

# SKILL DEMAND ----
tabPanel(
  "Skill Demand",
  
  # Define UI for application that draws a histogram
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
 ## Side panel ----
    sidebarPanel(
      width = 2,
  ### Help text --------------------
      helpText("Choose a Local Area to view skill demand trends"
               ,style = "font-style: italic;"
      ),
      br(),
  ### LEP 5 input ---------------
      selectizeInput("lep5",
                     "Choose a primary LEP:",
                     choices=C_LEP2020,
                     selected="England",
      ),
  ### LEP 6 input ------------
      selectizeInput("lep6", # Make no selection an option
                     "Choose a comparison LEP (optional):",
                     choices=C_LEP2020,
                     selected="England",
      ),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
  ### Download button -------------
      downloadButton(
        outputId = "download_btn3",
        label = "Download",
        icon = shiny::icon("download")
      ),
      
    ), # end of side panel
 ## Main panel ----
    # Show a plot of the generated distribution
    mainPanel(
      width=10,
  ### Title ----
      box(width=12,
          uiOutput("page3title", style="font-size: 24px;"),
      br(),
      div("XXX", style = "font-size: 16px; font-style: italic;"),
      br(),
      ), #end of box
  ### KPI boxes ----
      box(width=12,
          # title = NULL,
          # status = "primary",
          # solidheader=T,
          # column(
          #   id="second",
          #   width=4,
          #   align="left",
          #   style="height:15vh; min-height:96px; padding:5px; word-wrap: break-word;",
            valueBoxOutput("jobad.cnt"), 
          # style="color:white"
          # ),
          # column(
          #   id="third",
          #   width=1
          # ),
          # column(width=4,
          #        id="second",
          #        align="left",
          #        style="height:15vh; min-height:96px; padding:5px; word-wrap: break-word;",
                 valueBoxOutput("jobad.oth"), 
                 # style="color:white"
          #)
      ), # end of box


      box(width=12,
          ### Online job advert information box ----
          
          column(width=4,
                 h3("Information"),
                 br(),
                 p("Each time point in the series covers a monthly average of the volume of online job adverts in the month of January for the years 2017 to 2022."),
                 br(),
                 p("The monthly average is derived from weekly snapshots in January. The volume of online job adverts is presented as a unit measure. The unit measure is derived by dividing the monthly average count of job adverts by a set value.")),
          #plotlyOutput("EmpRate_time")),
          
          ### Employment by occupation data table ----
          
          column(width=8,
                 p("Place holder for Achievements by Sector Subject Area 2021"))
          #dataTableOutput("EmpOcc"))
      ) # end of box
      
    ) # end of main panel
  ) # end of side bar layout
), # end of Skill demand tab

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