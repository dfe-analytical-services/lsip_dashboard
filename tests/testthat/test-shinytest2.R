library(shinytest2)

inputs <- c(    
  "geoChoice",
  "geoChoiceOver",
  "hubComparators",
  "hubLA",
  "navbar",
  "splashGeoType",
  "splashMetric"
)

outputs <- c(
  "UBC.micro",
  "dest.ks5over",
  "overviewEmpCntKPI",
  "overviewEmpRateKPI",
  "overviewJobKPI",
  "page0title",
  "screenshotOverview",
  "skisup.APPach",
  "skisup.ETach",
  "wfOverviewKpi"
)

app <- AppDriver$new(load_timeout = 6e+05)

test_that("Initial load", {
  app$expect_values(
    input = inputs,
    output = outputs
  )
})

test_that("User guide", {
  app$set_inputs(navbar = "User guide")
  app$expect_values(
    input = inputs,
    output = outputs
  )
})

test_that("Local skills", {
  app$set_inputs(navbar = "Local skills")
  app$expect_values(
    input = inputs,
    output = outputs
    )
})

test_that("Data sources", {
  app$set_inputs(navbar = "Data sources")
  app$expect_values(
    input = inputs,
    output = outputs
  )
})



