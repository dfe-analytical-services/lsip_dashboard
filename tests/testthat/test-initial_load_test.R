library(shinytest2)

test_that("Migrated shinytest test: initial_load_test.R", {
  run_set_shinytests <- function(dfinputs, outstring, listrecords) {
    for (i in 1:nrow(dfinputs)) {
        file <- paste0(outstring, "_", i - 1, ".json")
        message(paste(i, dfinputs$field[i], dfinputs$value[i]))
        eval(parse(text = paste0("app$setInputs(", dfinputs$field[i],
            "=\"", dfinputs$value[i], "\", timeout_ = 3.6e+4, values_ = FALSE)")))
        structure(list(app$expect_values(), app$expect_screenshot(name = file)), names = c("",
        ""))
    }
  }

  app <- AppDriver$new(load_timeout = 6e+05)

  app$expect_values()
  app$expect_screenshot()


  dfTestInputs <- data.frame(
    field = c(
      "navbar", "geoChoiceOver",
      "navbar"
    ),
    value = c(
      "Overview", "Cheshire and Warrington",
      "User guide"
    )
  )


  message("Looping through input list")
  run_set_shinytests(dfTestInputs, "localskills", "")
})
