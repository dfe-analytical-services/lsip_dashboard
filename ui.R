# ---------------------------------------------------------
# File name: ui.R
# Date created: 06/06/2022
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
  HTML("<title>DfE Analytical Services R-Shiny Template</title>"),

  # Navbar ====================================================================

  # This CSS sets the 4th item on the navbar to the right
  tagList(
    tags$head(tags$style(HTML("
                           .navbar-nav {
                           float: none !important;
                           }
                           .navbar-nav > li:nth-child(6) {
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
                     h1("LSIP dashboard: Local Supply and Demand Data"),

                     p("This app provides a comprehensive picture of local supply and demand data."),
                     br(),
                     br()
                   ),

                   ## Left panel ---------------------

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
                   ),

                   ## Right panel -----------------------------

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
                         )
                       )
                     )
                   )
                 )
               )
             ), # End of Homepage
    # Local landscape tab ----------------
    tabPanel(
      "Local landscape",
      
      # Sidebar =======================
      sidebarLayout(
        sidebarPanel(
          width = 2,
        ### Help text 
          helpText("Choose a primary LEP area to view employment data."),
          br(),
        ### Primary LEP input 
          selectInput("LEP",
 #                        options = list(create=TRUE),
                         label = "Choose a primary LEP:",
                         choices = C_LEP2020,
                         multiple = F
          ),
        
        ### Comparison LEP input 
          selectInput("LEP2",
  #                       options = list(create=TRUE),
                         label = "Choose a comparison LEP (optional):",
                         choices = C_LEP2020,
                         multiple = F
          ),

        ### Reset button 
        actionButton("reset", "Reset",
                     style = "color: #0b0c0c;
                                             font-size: 12px;
                                             font-weight: bold;
                                             background-color: #ffffff"
        ),
        br(),
        br(),
        ### Download button 
        downloadButton(
          outputId = "download_btn1",
          label = "Download",
          icon = shiny::icon("download")
        ),
        br(),
        br()
      ), # end of sidebar
 
      # Main panel =========
        mainPanel(
          width = 10,
        
          ## Title -----------
          h1("Primary LEP: Overview of Local Landscape", style = "font-size: 24px;"), #Make reactive title from primary LEP
          br(),
          p("Data for working age population in 2021", style = "font-size: 16px; font-style: italic;"),
          br(),
          ## Sections ------------------------------------------------------------------------------
          
          #### KPIs ----------------------------------------------------------------------------
          
          box( # set colour of boxes
            title = NULL,
            width = 600,
            status = "primary",
            solidHeader = T,
            column(
              id = "second",
              align = "left",
              width = 3,
              style = "height:15vh; min-height:96px; padding:5px; word-wrap: break-word;",
                div("759,100", style = "font-size: 20px;"), # Make reactive to Primary LEP
                div("in employment in Primary LEP", style = "font-size: 12px;")
            ),
            column(
              id = "third",
              width = 1
            ),
            column(
              id = "first",
              align = "left",
              width = 3,
              style = "height:15vh; min-height:96px; padding:5px; word-wrap: break-word;",
                div("80%", style = "font-size: 20px;"), # Make reactive to Primary LEP
                div("employment rate in Primary LEP", style = "font-size: 12px;")
      #        uiOutput("median_in_sector"), # Make reactive to Primary LEP
      #        uiOutput("kpiEarn")
            ),
     #       column(
     #         id = "third",
     #         width = 1
      #      ),
      #      column(
      #        id = "second",
      #        align = "left",
      #        width = 3,
      #        style = "height:15vh; min-height:96px; padding:5px; word-wrap: break-word;",
      #          div("15%", style = "font-size: 20px;"), # Make reactive to Primary LEP
      #          div("self-employment rate in Primary LEP", style = "font-size: 12px;")
      #        uiOutput("directionSector"), # Make reactive to Primary LEP
     #         uiOutput("kpiChange")
       #     ),
      #      column(
      #        id = "third",
      #        width = 1
     #       )
          ),
          box(
            title = NULL,
            width = 600,
            status = "primary",
            solidHeader = T,
            column(
              id = "third", width = 12,
              style = "height:3vh; padding:0px;"
            )
          ), # end of box
     ## Left panel ------------
     column(
       width = 6,
       plotlyOutput("empRate"),
       br(),
       valueBoxOutput("box_info", width = 6)
     )
     ## Right panel -------------
          
     
        ) # end of main panel
      )
    ), # End of Local landscape tab
    # Skill supply tab ============================================================
    tabPanel(
      "Skills supply",

      # Sidebar =======
      sidebarLayout(
        sidebarPanel(
          width = 2,
          ### Help text 
          helpText("Choose a primary LEP area to view employment data."),
          br(),
          ### Primary LEP input 
          selectInput("LEP",
                      #                        options = list(create=TRUE),
                      label = "Choose a primary LEP:",
                      choices = C_LEP2020,
                      multiple = F
          ),
          
          ### Comparison LEP input 
          selectInput("LEP2",
                      #                       options = list(create=TRUE),
                      label = "Choose a comparison LEP (optional):",
                      choices = C_LEP2020,
                      multiple = F
          ),
          
          ### Region button 
     #     radioButtons("showMedian",
     #                  selected = "England",
    #                   label = div(
     #                    style = "white-space: nowrap; ",
     #                    "Choose a metric to show:"
      #                 ),
     #                  choices = c("England", "Region"),
     #                  inline = F,
      #                 width = "50%"
      #    ),
          #     uiOutput("SubjQualInputPanel"),
      #    br(),
          ### Rest button 
          actionButton("reset", "Reset",
                       style = "color: #0b0c0c;
                                             font-size: 12px;
                                             font-weight: bold;
                                             background-color: #ffffff"
          ),
          br(),
          br(),
          ### Download button 
          downloadButton(
            outputId = "download_btn1",
            label = "Download",
            icon = shiny::icon("download")
          ),
          br(),
          br()
        ), # end of sidebar
        
        # Main panel ====
        mainPanel(
          width = 10,
          ### Title -----------
          div("Overview of skill supply", style = "font-size: 24px; font-weight: bold;"), #Make dynamic with LEP title
          br(),
          div("XXX", style = "font-size: 16px; font-style: italic;"),
          br(),
          # add box to show user input
        ) # end of main panel
      )
    ), # End of Skill supply tab
    # Skill demand tab -------------------
    tabPanel(
      "Skill demand",
      
      # Sidebar =======
      sidebarLayout(
        sidebarPanel(
          width = 2,
          ### Help text 
          helpText("Choose a primary LEP area to view employment data."),
          br(),
          ### Primary LEP input 
          selectInput("LEP",
                      #                        options = list(create=TRUE),
                      label = "Choose a primary LEP:",
                      choices = C_LEP2020,
                      multiple = F
          ),
          
          ### Comparison LEP input 
          selectInput("LEP2",
                      #                       options = list(create=TRUE),
                      label = "Choose a comparison LEP (optional):",
                      choices = C_LEP2020,
                      multiple = F
          ),
          
          ### Region button 
      #    radioButtons("showMedian",
      #                 selected = "England",
      #                 label = div(
      #                   style = "white-space: nowrap; ",
      #                   "Choose a metric to show:"
      #                 ),
       #                choices = c("England", "Region"),
       #                inline = F,
       #                width = "50%"
      #    ),
          #     uiOutput("SubjQualInputPanel"),
      #    br(),
          ### Rest button 
          actionButton("reset", "Reset",
                       style = "color: #0b0c0c;
                                             font-size: 12px;
                                             font-weight: bold;
                                             background-color: #ffffff"
          ),
          br(),
          br(),
          ### Download button 
          downloadButton(
            outputId = "download_btn1",
            label = "Download",
            icon = shiny::icon("download")
          ),
          br(),
          br()
        ), # end of sidebar
        
        # Main panel ========
        mainPanel(
          width = 10,
          div("Overview of skill demand", style = "font-size: 24px; font-weight: bold;"), #Make dynamic with LEP title
          br(),
          div("XXX", style = "font-size: 16px; font-style: italic;"),
          br(),
          # add box to show user input
        )
      )
    ), #End of Skill demand tab
    # Mapping supply and demand tab -----------------
#    tabPanel(
#      "Mapping supply and demand",
      
      # Sidebar =======
  #    sidebarLayout(
  #      sidebarPanel(
  #        width = 2,
          ### Help text 
   #       helpText("Choose a primary LEP area to view employment data."),
    #      br(),
          ### Primary LEP input 
#          selectInput("LEP",
 #                     #                        options = list(create=TRUE),
  #                    label = "Choose a primary LEP:",
   #                   choices = C_LEP2020,
    #                  multiple = F
     #     ),
          
          ### Comparison LEP input 
#          selectInput("LEP2",
                      #                       options = list(create=TRUE),
#                      label = "Choose a comparison LEP (optional):",
#                      choices = C_LEP2020,
#                      multiple = F
#          ),
          
          ### Region button 
    #      radioButtons("showMedian",
    #                   selected = "England",
    #                   label = div(
    #                     style = "white-space: nowrap; ",
    #                     "Choose a metric to show:"
    #                   ),
   #                    choices = c("England", "Region"),
   #                    inline = F,
   #                    width = "50%"
    #      ),
          #     uiOutput("SubjQualInputPanel"),
   #       br(),
          ### Reset button 
#          actionButton("reset", "Reset",
#                       style = "color: #0b0c0c;
#                                             font-size: 12px;
#                                             font-weight: bold;
#                                             background-color: #ffffff"
#          ),
#          br(),
#          br(),
          ### Download button 
#          downloadButton(
#            outputId = "download_btn1",
#            label = "Download",
#            icon = shiny::icon("download")
#          ),
#          br(),
#          br()
#        ), # end of sidebar
        
        # Main panel ======
 #       mainPanel(
#          width = 10,
#          div("Overview of mapping supply and demand", style = "font-size: 24px; font-weight: bold;"), #Make dynamic with LEP title
#          br(),
#          div("XXX", style = "font-size: 16px; font-style: italic;"),
#          br(),
#          # add box to show user input
 #       )
#      )
#    ), # End of Mapping supply and demand tab
    # Create the accessibility statement-----------------
    tabPanel(
      "Accessibility",
      accessibility() # as defined in R/accessibility.R
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
