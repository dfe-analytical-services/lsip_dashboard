# ---------------------------------------------------------
# File name: server.R
# Date created: 06/06/2022
#
# ---------------------------------------------------------

# Library calls ---------------------------------------------------------------------------------
shhh <- suppressPackageStartupMessages # It's a library, so shhh!

shhh(library(shiny))
shhh(library(shinyjs))
shhh(library(tools))
shhh(library(testthat))
shhh(library(shinytest2))
shhh(library(diffviewer))
shhh(library(shinyWidgets))
shhh(library(shinyGovstyle))
shhh(library(janitor))
shhh(library(openxlsx))
shhh(library(tidyverse)) # various data manipulation
shhh(library(scales)) # formatting numbers
shhh(library(plotly)) # interactive plots
shhh(library(DT)) # create datatables
shhh(library(writexl)) # write data to xls for download
shhh(library(leaflet)) # create maps
shhh(library(sf)) # load map data
shhh(library(capture)) # screenshots
shhh(library(shinyalert)) # cookie pop up
shhh(library(shinycssloaders)) # spinners
shhh(library(RColorBrewer)) # map colours
shhh(library(quarto)) # map colours
shhh(library(dfeshiny)) # map colours

site_title <- "Local skills dashboard"
google_analytics_key <- "MMB6NG2FE1"

area_select <- c("Coast to Capital", "Greater Manchester", "England")

metricChoices <- list(
  "Employment" = list(
    "Employment rate" = "inemploymentRate",
    "Self-employment rate" = "selfemployedRate",
    "Unemployment rate" = "unemployedRate",
    "Inactive rate" = "inactiveRate",
    "Employment" = "inemployment",
    "Self-employment" = "selfemployed",
    "Unemployed" = "unemployed",
    "Inactive" = "inactive"
  ),
  "Jobs" = list(
    "Online job adverts" = "vacancies",
    "Employment projections (Skills imperative 2035)" = "employmentProjection"
  ),
  "Businesses" = list(
    "Business count" = "enterpriseCount",
    "Business birth rate" = "birthRate",
    "Business death rate" = "deathRate"
  ),
  "Skills" = list(
    "FE achievement" = "achievements",
    "FE participation" = "participation",
    # "FE starts" = "starts",
    "FE achievement rate per 100,000" = "achievements_rate_per_100000_population",
    "FE participation rate per 100,000" = "participation_rate_per_100000_population",
    # "FE start rate" = "starts_rate_per_100000_population",
    "Qualified at Level 3 or above" = "L3PlusRate",
    "Qualified at Level 4 or above" = "L4PlusRate"
  ),
  "Destinations" = list(
    "KS4 sustained positive destination rate" = "sustainedPositiveDestinationKS4Rate",
    "KS5 sustained positive destination rate" = "sustainedPositiveDestinationKS5Rate"
  )
  # ,
  # "Mismatch" = list("Supply vs demand TO COME" = "mismatch")
)

# Functions ---------------------------------------------------------------------------------

# Here's an example function for simplifying the code needed to commas separate numbers:

# cs_num ----------------------------------------------------------------------------
# Comma separating function

cs_num <- function(value) {
  format(value, big.mark = ",", trim = TRUE)
}

# tidy_code_function -------------------------------------------------------------------------------
# Code to tidy up the scripts.

tidy_code_function <- function() {
  message("----------------------------------------")
  message("App scripts")
  message("----------------------------------------")
  app_scripts <- eval(styler::style_dir(recursive = FALSE)$changed)
  R_scripts <- eval(styler::style_dir("R/", filetype = "r")$changed)
  message("Test scripts")
  message("----------------------------------------")
  test_scripts <- eval(styler::style_dir("tests/", filetype = "r")$changed)
  script_changes <- c(app_scripts, test_scripts)
  return(script_changes)
}

# Source scripts ---------------------------------------------------------------------------------

# Source any scripts here. Scripts may be needed to process data before it gets to the server file.
# It's best to do this here instead of the server file, to improve performance.

# source("R/filename.r")


# appLoadingCSS ----------------------------------------------------------------------------
# Set up loading screen

appLoadingCSS <- "
#loading-content {
  position: absolute;
  background: #000000;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
"
