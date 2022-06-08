# ---------------------------------------------------------
# File name: server.R
# Date created: 06/06/2022
#
# ---------------------------------------------------------


# Library calls ---------------------------------------------------------------------------------


library(shiny)
library(shinyjs)
library(tools)
library(testthat)
library(shinytest)
library(shinydashboard)
library(shinyWidgets)
library(shinyGovstyle)
library(dplyr)
library(data.table)
library(tidyverse)
library(plotly)
#renv::snapshot()

source("R/Loading Core Indicators.R")

area_select <- c("Coast to Capital", "Greater Manchester", "England")
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

source("R/support_links.R")
source("R/accessibility.R")
