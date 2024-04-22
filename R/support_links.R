support_links <- function() {
  fluidRow(
    column(
      width = 12,
      h1("Give us feedback"),
      p(
        "This dashboard is a new service that we are developing. If you have any feedback or suggestions for improvements, please contact us at ",
        a(href = "mailto:ufs.contact@education.gov.uk", "ufs.contact@education.gov.uk", .noWS = c("after")), "."
      ),
      p(
        "If you spot any errors or bugs while using this dashboard, please screenshot and email them to ",
        a(href = "mailto:statistics.development@education.gov.uk", "statistics.development@education.gov.uk", .noWS = c("after")), "."
      ),
      h2("Contact us"),
      p(
        "If you have questions about the dashboard or data within it, please contact us at ",
        a(href = "mailto:ufs.contact@education.gov.uk", "ufs.contact@education.gov.uk", .noWS = c("after")), "."
      ),
      h2("See the source code"),
      p(
        "The source code for this dashboard is available in our ",
        a(href = "https://github.com/dfe-analytical-services/lsip_dashboard", "GitHub repository", .noWS = c("after")),
        "."
      )
    ),
    column(
      12,
      h2("Use of cookies"),
      textOutput("cookie_status"),
      actionButton("remove", "Reset cookie consent"),
      br(),
      br()
    )
  )
}
