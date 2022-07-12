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
  HTML("<title>Local Skills Dashboard</title>"),
  
# Navbar ====================================================================
  navbarPage("",
             id = "navbar",
             
# Homepage tab ============================================================

             tabPanel(
               "Homepage",
               fluidPage(
                 fluidRow(
                   column(
                     12,
                     h1("Local Skills Dashboard"),

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
                           tags$div(
                             title = "This section is useful if you want to understand how well different industries retain graduates.",
                             h3(actionLink("link_to_industryFlow_tab", "App Content"))
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
                           h2("Background Info")
                         ),
                         div(
                           class = "panel-body",
                           h2("Local Landscape"),
                           h3("Annual Population Survey (APS)"),
                           p("XXX"),
                           br(),
                           h2("Skill Supply"),
                           h3("Individualised Learner Records (ILR)"),
                           p("XXX"),
                           br(),
                           h2("Skill Demand"),
                           h3("ONS Online Job Adverts"),
                           p("XXX")
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
                     "Choose the area(s) you want to look at",
                     choices=C_LEP2020,
                     selected="England"#,multiple=TRUE
      ),
      radioButtons("OccSect", 
                         "Do you want to look at a particular sector or occupation group?", 
                         choices = list("Occupation" = 1, 
                                        "Sector" = 2),
                         selected = 1),
      ### Help text --------------------
      helpText("Download all available indicators for:"
               ,style = "font-style: italic;"
      ),
      ### Download button -------------
      downloadButton(
        outputId = "download_btn0a",
        label = "All LEPs",
        icon = shiny::icon("download")
      ),
      downloadButton(
        outputId = "download_btn0b",
        label = "Current LEP",
        icon = shiny::icon("download")
      )
    ), # end of side panel
    ## Main panel ----
    mainPanel(
      width=10,
         ### Title ----
         uiOutput("page0title", style="font-size: 24px;"),
         div("Change measured since the same time year", style = "font-size: 16px; font-style: italic;"),
         br(),
      fluidRow(
        #left column
        column(width = 6,
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
             #first row - emp vol
             tags$div(
               title="Source: APS. 2021 calendar year",
               valueBoxOutput("locland.emplcnt0", width=8),
               valueBoxOutput("locland.emplcntchange0",width=4)
             ),
             box(
               width=12,
               p(" ")
             ),
             #second row - emp rate
             tags$div(
               title="Source: APS. 2021 calendar year",
               valueBoxOutput("locland.emplrate0",width=8),
               valueBoxOutput("locland.emplchange0",width=4)
             ),
             box(
               width=12,
               actionLink("link_to_tabpanel_employment", "Find out more about employment")
             ),
             #third row - link to emp tab
             #fourth row - vacancies
             tags$div(
               title="Source: ONS (Adzuna). Jan 2022",
               valueBoxOutput("jobad.units", width=8),
               valueBoxOutput("jobad.change",width=4)
             ),
             #fifth row - link to vacancy data
             box(
               width=12,
               actionLink("link_to_tabpanel_skill_demand", "Find out more about vacancies")
             ),
             tags$div(
               title="Source: ?????",
               valueBoxOutput("earn.avg", width=8),
               valueBoxOutput("earn.change",width=4)
             ),
             box(
             actionLink("link_to_tabpanel_earnings", "Find out more about earnings")
             )
                   )
                 )
               ),
        ),
        #right column
        column(width = 6,
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
                     #first row - level 4 starts
                     tags$div(
                       title="Source:?",
                       valueBoxOutput("skills.l4", width=8),
                       valueBoxOutput("skills.l4change",width=4)
                     ),
                     #2nd row - link to skills population data
                     box(
                       width = 12, 
                       actionLink("link_to_tabpanel_skills", "Find out more about skills")
                     ),
                     #3rd row - E&T 
                     tags$div(
                       title="Source:ILR AY20/21",
                       valueBoxOutput("skisup.ETach", width=8),
                       valueBoxOutput("skisup.ETachChange", width=4)
                     ),
                     #4th row - link to E&T data
                     box(
                       width = 12, 
                       actionLink("link_to_tabpanel_skills", "Find out more about skills")
                     ),
                     #5th row - apps
                     tags$div(
                       title="Source:ILR AY20/21",
                       valueBoxOutput("skisup.APPach", width=8),
                       valueBoxOutput("skisup.APPachChange", width=4)
                     ),
                     #6th row - link to app data
                     box(
                       width = 12, 
                       actionLink("link_to_tabpanel_skills", "Find out more about skills")
                     ),
                     #7th row - HE
                     tags$div(
                       title="Source:?",
                       valueBoxOutput("he.entrants", width=8),
                       valueBoxOutput("he.entrantschange",width=4)
                     ),
                     #8th row - link to HE data
                     box(
                       width = 12, 
                       actionLink("link_to_tabpanel_HE", "Find out more about HE")
                     ),
                   )
                 )
               ),
        ),
        
      )
    )

  ) # end of side bar layout
), # end of Overview tab

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
                                  choices=C_LEP2020,
                                  selected="England",
                   ),
  ### LEP 2 input ------------
                   selectizeInput("lep2",
                                  "Choose a comparison LEP (optional):",
                                  choices=c("\nNone", unique(C_LEP2020)),
                                  multiple = F
                   ),

  ### Help text --------------------
  helpText("Download employment indicators for:"
           ,style = "font-style: italic;"
  ),                   
  ### Download buttons -------------
                   downloadButton(
                     outputId = "download_btn1a",
                     label = "All LEPs",
                     icon = icon("download")
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
                   width=10,
  ### Title ----
                   uiOutput("page1title", style="font-size: 24px;"),
                   div("Data for employees aged 25-30 in sustained employment in the 2018-19 tax year", style = "font-size: 16px; font-style: italic;"),
                   br(),

  ### KPI boxes ----
                   box(width=12,
                       valueBoxOutput("locland.emplrate"),
                       valueBoxOutput("locland.emplcnt")
                      ),
  box(
    width=12,
    uiOutput("emp_comp")
  ),
  br(),
  ### Employment rate over time line chart ----
                       column(width=6,
                              p("Employment rate",style = "font-size:20px;"),
                              plotlyOutput("EmpRate_time")
                              ),
  ### Employment by occupation data table ----
                       column(width=6,
                              p("In employment in each occupation",style = "font-size:20px;"),
                              dataTableOutput("EmpOcc")
                              )

                 ) # end of main panel
               ) # end of side bar layout
             ), # end of Local Landscape tab

#SKILLS ----
tabPanel(
  "Skills",
  sidebarLayout(
 ## Side panel ----
    sidebarPanel(
      width = 2,
  ### LEP 3 input ---------------
      selectizeInput("lep3",
                     "Choose a primary LEP:",
                     choices=C_LEP2020,
                     selected="England",
      ),
  ### LEP 4 input ------------
      selectizeInput("lep4", # Make no selection an option
                     "Choose a comparison LEP (optional):",
                     choices=c("\nNone", unique(C_LEP2020))
      ),
  ### Help text --------------------
  helpText("Download skills indicators for:"
           ,style = "font-style: italic;"
  ),                   
  ### Download buttons -------------
  downloadButton(
    outputId = "download_btn2a",
    label = "All LEPs",
    icon = icon("download")
  ),
  downloadButton(
    outputId = "download_btn2b",
    label = "Current LEP",
    icon = icon("download")
  ),

    ), # end of side panel
 ## Main panel ----
    mainPanel(
      width=10,
  ### Title ----
      uiOutput("page2title", style="font-size: 24px;"),
      div("xxx", style = "font-size: 16px; font-style: italic;"),
      br(),
  ### KPI boxes ----
      box(width=12,
          valueBoxOutput("skisup.FEach"),
          valueBoxOutput("skisup.APach"),
      ), 
    box(
    width=12,
    uiOutput("skill_comp")
    ),
      box(width=12,
          
  ### Achievements over time line chart ----
          column(width=6,
                 ### LEP 4 input ------------
                 p("FE and apprenticeship achievements",style = "font-size:20px;"),
                 selectizeInput("skill_line", # Make no selection an option
                                "Choose a learner group",
                                choices=c("Apprenticeships: Total", "Education and training: Total","Community learning: Total","Further education and skills: Total")),
                 plotlyOutput("Ach_time")),

  ### Employment by occupation data table ----
          column(width=6,
                 p("All achievements by SSA tier 1 (AY20/21)",style = "font-size:20px;"),
                 plotlyOutput("Ach_SSA_pc"))
    ) # end of box

    ) # end of main panel
  ) # end of side bar layout
), # end of Skills Supply tab

# VACANCIES ---------------
tabPanel(
  "Vacancies",
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    ## Side panel ----
    sidebarPanel(
      width = 2,
      ### Help text --------------------
      helpText("Choose a Local Area to view online vacancy trends"
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
                     choices=c("\nNone", unique(C_LEP2020))
      ),
      ### Help text --------------------
      helpText("Download vacancy indicators for:"
               ,style = "font-style: italic;"
      ),                   
      ### Download buttons -------------
      downloadButton(
        outputId = "download_btn3a",
        label = "All LEPs",
        icon = icon("download")
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
      width=10,
      ### Title ----
      uiOutput("page3title", style="font-size: 24px;"),
      div("XXX", style = "font-size: 16px; font-style: italic;"),
      br(),
      ### KPI boxes ----
      box(width=12,
          valueBoxOutput("jobad.pc"),
          valueBoxOutput("jobad.ch"),
      ), # end of box
      box(
        width=12,
        uiOutput("vac_comp")
      ),
      box(width=12,
          ### ONS job advert information ----
          
          column(width=4,
                 p("Information",style = "font-size:20px;"),
                 br(),
                 p("Each time point in the series covers a monthly average of the volume of online job adverts in the month of January for the years 2017 to 2022. 
"),
                 br(),
                 p("The monthly average is derived from weekly snapshots in January. The volume of online job adverts is presented as a unit measure. The unit measure is derived by dividing the monthly average count of job adverts by a set value.
")
          ),
    
          
          ### Online job vacancy units over time line chart ----
          column(width=8,
                 p("Online job vacancy units",style = "font-size:20px;"),
                 plotlyOutput("jobad.time")),
      ) # end of box
      
    ) # end of main panel
  ) # end of side bar layout
), # end of Skills Supply tab

# EARNINGS ---------------
tabPanel(
  "Earnings",
  
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
      ### LEP 7 input ---------------
      selectizeInput("lep7",
                     "Choose a primary LEP:",
                     choices=C_LEP2020,
                     selected="England",
      ),
      ### LEP 8 input ------------
      selectizeInput("lep8", # Make no selection an option
                     "Choose a comparison LEP (optional):",
                     choices=c("\nNone", unique(C_LEP2020))
      ),
      ### Help text --------------------
      helpText("Download earnings indicators for:"
               ,style = "font-style: italic;"
      ),                   
      ### Download buttons -------------
      downloadButton(
        outputId = "download_btn4a",
        label = "All LEPs",
        icon = icon("download")
      ),
      downloadButton(
        outputId = "download_btn4b",
        label = "Current LEP",
        icon = icon("download")
      ),
      
    ), # end of side panel
    ## Main panel ----
    # Show a plot of the generated distribution
    mainPanel(
      width=10,
      ### Title ----
      uiOutput("page4title", style="font-size: 24px;"),
      div("XXX", style = "font-size: 16px; font-style: italic;"),
      br(),
      ### KPI boxes ----
      box(width=12,"blank"
         # valueBoxOutput("jobad.pc"),
         # valueBoxOutput("jobad.ch"),
      ), # end of box
    ) # end of main panel
  ) # end of side bar layout
), # end of earnings tab

# HE ---------------
tabPanel(
  "HE",
  
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
      ### LEP 9 input ---------------
      selectizeInput("lep9",
                     "Choose a primary LEP:",
                     choices=C_LEP2020,
                     selected="England",
      ),
      ### LEP 10 input ------------
      selectizeInput("lep10", # Make no selection an option
                     "Choose a comparison LEP (optional):",
                     choices=c("\nNone", unique(C_LEP2020))
      ),
      ### Help text --------------------
      helpText("Download HE indicators for:"
               ,style = "font-style: italic;"
      ),                   
      ### Download buttons -------------
      downloadButton(
        outputId = "download_btn5a",
        label = "All LEPs",
        icon = icon("download")
      ),
      downloadButton(
        outputId = "download_btn5b",
        label = "Current LEP",
        icon = icon("download")
      ),
      
    ), # end of side panel
    ## Main panel ----
    # Show a plot of the generated distribution
    mainPanel(
      width=10,
      ### Title ----
      uiOutput("page5title", style="font-size: 24px;"),
      div("XXX", style = "font-size: 16px; font-style: italic;"),
      br(),
      ### KPI boxes ----
      box(width=12,"blank"
          # valueBoxOutput("jobad.pc"),
          # valueBoxOutput("jobad.ch"),
      ), # end of box
    ) # end of main panel
  ) # end of side bar layout
), # end of HE tab

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