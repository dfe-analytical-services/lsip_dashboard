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
shhh(library(shinytest))
shhh(library(shinydashboard))
shhh(library(shinyWidgets))
shhh(library(shinyGovstyle))
shhh(library(dplyr))
shhh(library(data.table))
shhh(library(tidyverse))
shhh(library(plotly))
shhh(library(openxlsx))
shhh(library(janitor))
shhh(library(DT))
shhh(library(writexl))
shhh(library(scales)) # for comma 1,000s
# shhh(library(shinyBS))
shhh(library(leaflet))
shhh(library(lubridate))
shhh(library(sf))

# renv::snapshot()

area_select <- c("Coast to Capital", "Greater Manchester", "England")

metricChoices <- list(
  "Employment" = list(
    "Employment rate" = "empRate",
    "Self-employment rate" = "selfempRate",
    "Unemployment rate" = "unempRate",
    "Inactive rate" = "inactiveRate",
    "Employment volume" = "Employment",
    "Self-employment volume" = "  Self Employed ",
    "Unemployed volume" = "  Unemployed ",
    "Inactive volume" = "  Inactive ",
    "Online job adverts" = "vacancies",
    "Job projections - TO COME" = "workingFutures"
  ),
  "Enterprises" = list(
    "Enterprise count" = "enterpriseCount",
    "Enterprise birth rate" = "birthRate",
    "Enterprise death rate" = "deathRate"
  ),
  "FE and skills" = list(
    "FE achievement rate" = "achievements_rate_per_100000_population",
    "FE participation rate" = "participation_rate_per_100000_population",
    #"FE start rate" = "starts_rate_per_100000_population",
    "FE achievement volumes" = "achievements",
    "FE participation volumes" = "participation",
    #"FE starts" = "starts",
    "Qualified at Level 3 or above" = "level3AndAboveRate",
    "KS4 completers sustained positive destination rate" = "sustainedPositiveDestinationKS4Rate",
    "KS5 completers sustained positive destination rate" = "sustainedPositiveDestinationKS5Rate"
  )
  #,
  #"Mismatch" = list("Supply vs demand TO COME" = "mismatch")
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
