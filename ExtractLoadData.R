####
# Title: LSIP dashboard data load and cleanse
# Author: Paul James
# Date: 30th Aug 2022
# Last updated: 12th Jun 2024
# Aim: Loads, cleans and reformats the original data sources used within the dashboard.
# Use: To update any datasets, delete the relevant file within the relevant folder, and paste in the new data file. Rerun this file.
# The dataframes are saved in ./Data/AppData to be read by the dashboard app.
# Sources: See the dashboard page Data sources for links to the datasets
# Notes: Ensure there is *only* the file(s) that will be used in each of the numbered folders in ./Data/
# Running time: ~40mins
###

# Load libraries ----
library(dplyr)
library(data.table) # use %like%

# For QA purposes we compare the old data with any updated data. This is done here before any data changes so we can compare the data as it was to the updated data. This is used with QAdataload.R
# C_timeOld <- readr::read_csv("Data/AppData/C_time.csv")

# 1. Load functions ----
source("~/RProjects/lsip_dashboard/R/functions.R", echo = TRUE)

# 2. Load lookups ----
source("~/RProjects/lsip_dashboard/importData/importLookups.R", echo = TRUE)

# 3. Load geographical boundaries ----
source("~/RProjects/lsip_dashboard/importData/importBoundaries.R", echo = TRUE)

# 4. Datasets ----
## 4.1 Nomis datasets ----
source("~/RProjects/lsip_dashboard/importData/importNomis.R", echo = TRUE)

## 4.2 EES datasets----
source("~/RProjects/lsip_dashboard/importData/importEES.R", echo = TRUE)

## 4.3 ONS datasets ----
source("~/RProjects/lsip_dashboard/importData/importONS.R", echo = TRUE)

## 4.4 Skills imperative----
source("~/RProjects/lsip_dashboard/importData/importSkillsImp.R", echo = TRUE)

# 5 Dashboard text----
source("~/RProjects/lsip_dashboard/importData/importDashboardText.R", echo = TRUE)

# 6 Combine data ----
source("~/RProjects/lsip_dashboard/importData/combineData.R", echo = TRUE)
