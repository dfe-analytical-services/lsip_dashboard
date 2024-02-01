panel_question <- function() {
  tabPanel(
    "Ask away",
    # 1 Filters ----
    fluidRow(
      column(
        12,
        br(),
        div(
          class = "filterRow",
          fluidRow(
            column(
              width = 10,
                textInput("questionGuess", "Ask me anything")
            ),
            column(2
                   ,br()
                  ,actionButton("questionButton", "Ask")
              )
          )
        ),
        br(),
      )
    ), # end of filters row
    
    #textOutput("questionGuessAreaName"),
    #textOutput("questionGuessMetric"),
    textOutput("answerMainQuestion"),
    
    h3(textOutput("questionHeading")),
    fluidRow(uiOutput("employedBox2"),
           uiOutput("inemploymentRateBox2"),
          uiOutput("vacanciesBox2"),
          uiOutput("enterpriseCountBox2"),
          uiOutput("employmentProjectionBox2"),
    uiOutput("achievements_Education_and_trainingBox2"),
          uiOutput("achievements_ApprenticeshipsBox2"),
          uiOutput("sustainedPositiveDestinationKS4RateBox2"),
          uiOutput("L3PlusRateBox2")),
    br(),
    h3(textOutput("questionMapHeading")),
    htmlOutput("commentMapAiQuestion"),
     withSpinner(leafletOutput("mapOverview2")),
    br()
  #   h1(uiOutput("page0title")),
  #   fluidRow(
  #     column(
  #       12,
  #       htmlOutput("summaryArea")
  #     )
  #   ),
  #   br(),
  #   fluidRow(column(12
  #                   ,textOutput("answerDoc")
  #                   ,h3('Based on your question, here are some relevant charts')
  #   )
  #   ),
  #   br(),
  #   
  #   
  #   p("Change metrics are measured against the same period in the previous year. NB non-zero axes."),
  #   fluidRow(
  #     style = "padding-left: 15px;padding-right: 15px;", # indent slightly so box aligns
  #     # left column
  #     column(
  #       width = 6,
  #       class = "chartBox",
  #       uiOutput("employedBox"),
  #       uiOutput("inemploymentRateBox"),
  #       # fourth row - vacancies
  #       uiOutput("vacanciesBox"),
  #       
  #       # sixth row - enterprise
  #       uiOutput("enterpriseCountBox"),
  #       
  #       # 7th row - working futures
  #       uiOutput("employmentProjectionBox"),
  #       br()
  #     ),
  #     # right column
  #     column(
  #       width = 6,
  #       class = "chartBox",
  #       uiOutput("achievements_Education_and_trainingBox"),
  #       uiOutput("achievements_ApprenticeshipsBox"),
  #       # fifth row - destinations
  #       uiOutput("sustainedPositiveDestinationKS4RateBox"),
  #       # 6th row - link to app data
  #       uiOutput("L3PlusRateBox"),
  #       br()
  #     ) # end of right column
  #   ), # end of data row
  #   withSpinner(leafletOutput("mapOverview")),
  #   ### Downloads-------------
  #   br(),
  #   fluidRow(
  #     column(
  #       width = 3,
  #       downloadButton(
  #         outputId = "download_btn0a",
  #         label = "All data   ",
  #         icon = shiny::icon("download"),
  #         class = "downloadButton"
  #       )
  #     ),
  #     column(
  #       width = 9,
  #       "Download all data for all geographies (LEPs, LSIP, MCA areas, LAs, regions and England)",
  #     )
  #   ),
  #   fluidRow(
  #     column(
  #       width = 3,
  #       downloadButton(
  #         outputId = "download_btn0b",
  #         label = "Current geographic area",
  #         icon = shiny::icon("download"),
  #         class = "downloadButton"
  #       )
  #     ),
  #     column(width = 9, "Download all data for the selected geographic area")
  #   ),
  #   column(width = 12, br(""))
   )
}
