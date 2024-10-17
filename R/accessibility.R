accessibility <- function() {
  div(
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
      tags$li("Keyboard navigation through the interactive charts is currently limited, and some features are unavailable for keyboard only users"),
      tags$li("Alternative text in interactive charts is limited to titles and could be more descriptive (although this data is available in csv format)")
    )),
    h3("Feedback"),
    br(
      "If you have any feedback on how we could further improve the accessibility of this application, please contact us at",
      a(href = "mailto:skills.england@education.gov.uk", "skills.england@education.gov.uk"),
      br(),
      br(),
      br(),
      br()
    )
  )
}
