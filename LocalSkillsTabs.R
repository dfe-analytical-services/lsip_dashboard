ui <-
  fluidPage(
    tabsetPanel(
      tabPanel(
        "Main",
        "Lots of text here"
      ),
      tabPanel(
        "App",
        # choice row
        fluidRow(
          column(
            4,
            selectInput("id1", "Choose primary LEP",
              choices = c("a", "b", "c"),
              selected = "a"
            )
          ),
          column(
            4,
            selectInput("id2", "Choose comparison LEP",
              choices = c("a", "b", "c"),
              selected = "a"
            )
          )
        ),
        # next row is the data tabs
        fluidRow(
          tabsetPanel(
            tabPanel(
              "Overview",
              verbatimTextOutput("choice1")
            ),
            tabPanel(
              "Employment",
              "Charts and stuff"
            ),
            tabPanel(
              "Vacancies",
              "Charts and stuff"
            ),
            tabPanel(
              "FE",
              verbatimTextOutput("choice2")
            )
          ) # end data tabs
        ) # end data tabs row
      ), # end apps tabs
      tabPanel(
        "Accessibility",
        "Lots of text here"
      ),
      tabPanel(
        "Support and feedback",
        "Lots of text here"
      )
    ) # end overall tabs
  )

server <- function(input, output, session) {
  output$choice1 <- renderPrint(input$id1)
  output$choice2 <- renderPrint({
    paste("The choice is:", input$id1)
  })
}

shinyApp(ui, server)
