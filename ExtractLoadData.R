####
# Title: LSIP dashboard data load
# Author: Paul James
# Date: 30th Aug 2022
# Last updated: 28th Mar 2023
# Aim: Loads the original data sources used within the dashboard.
# Use: To update any datasets, delete the relevant file within the relevant folder, and paste in the new data file. Rerun this file and then run TransformData.R
# Sources: See the dashboard page Data sources for links to the datasets
# Notes: Ensure there is *only* the file(s) that will be used in each of the numbered folders in ./Data/
# Running time: ~20mins
# NB 2.2.2 has a clause to ignore the latest partial year of data (since we only work with full years). You may need to amend this.
###

# Load libraries ----
library(openxlsx) # use read.xlsx, read.csv
library(sf) # use st_read
library(tidyverse) # use map_df, mutate

# 1.Geography and mapping tables ----
## 1.1 LEPs 2020 and LA-LSIP lookup ----
folder <- "1-1_GeogLkup"
sheetNum <- "LAD21 - LSIP - LEP21"
I_LEP2020 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

## 1.2 Missing LAD-LEP lookup----
# This happens because we have some old LADs in the ILR (and other) data that have since been made inactive. These do not feature in the most recent LAD-LEP matching. We have manually mapped these LADs to the latest LEPS (2021)
folder <- "1-2_LEPmissing"
sheetNum <- 2
I_missingLAD <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

## 1.3 MCA lookup ----
folder <- "1-3_MCA_lookup"
sheetNum <- 1
I_mcalookup <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

## 1.4 LA 2011 to 2021 lookup ----
folder <- "1-4_LaLookup"
I_LaLookup <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))

## 1.5 LEP boundary----
folder <- "1-5_LEPBoundary"
I_mapLEP <- sf::st_read(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))),
  stringsAsFactors = F
)

## 1.6 LA boundary----
folder <- "1-6_LABoundary"
I_mapLA <- sf::st_read(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))),
  stringsAsFactors = F
)

## 1.7 MCA boundary----
folder <- "1-7_MCABoundary"
I_mapMCA <- sf::st_read(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))),
  stringsAsFactors = F
)

# 2. Datasets ----
## 2.1 Nomis datasets ----
### 2.1.1 Employment by occupation ----
# Query data
# Geography: England, LEPs, regions, LADs (as of April 2021)
# Date: 12 months to Dec 2017-2021
# Cell: T09a Employment by occupation (SOC2010) sub-major group and full-time/part-time; All people/ All people
folder <- "2-1_APSempOcc"
sheetNum <- 1
I_empOcc <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

### 2.1.2 Employment level and rate ------------
# Geog and date as above
# Cell: T01 Economic activity by age Aged 16-64/ All people
folder <- "2-2_APSempRate"
sheetNum <- 1
I_emp <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

### 2.1.3 UK Business Count----
# Enterprise by employment size
# Query data
# Geography: England, LEPs, regions, LADs (as of April 2022)
# Date: 12 months to Dec 2018-2022
# Cell: UK Business Counts - enterprises by industry and employment size band
folder <- "2-3_UBCempent"
sheetNum <- 1
I_entSize <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

# Enterprise by employment size and industry
folder <- "2-4_UBCempentind"
sheetNum <- 1
I_entInd <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

### 2.1.4 Skill by age gender ------------
# Geog and date as above
# Cell: T19	Qualification by age and gender - NVQ. All people aged 16-64. Only data up to Dec 21 available
folder <- "2-5_APSqualagegen"
sheetNum <- 1
I_qualAgeGender <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

### 2.1.5 Employment by industry------------
# Geog and date as above
# Cell: T13a	Employment by industry (SIC 2007) and flexibility
folder <- "2-6_APSempind"
sheetNum <- 1
I_empInd <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

## 2.2 EES datasets----
### 2.2.1 Achievements by SSAt1, LAD, gender, level------------
folder <- "2-7_ILRachSSA"
I_FeSsa <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))

### 2.2.2 Achievements/starts/part by LAD and provision, level and age------------
## Download "Further education and skills geography - detailed summary " from https://explore-education-statistics.service.gov.uk/data-catalogue/further-education-and-skills/2021-22
folder <- "2-8_ILRach"
I_FeProvLevelAge <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder)))) %>%
  filter(time_period != 202223) # ignore since only a partial year

### 2.2.3 KS4 destinations----
# National pupil database
folder <- "2-9_KS4destin"
I_KS4 <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))

### 2.2.4 KS5 destinations----
folder <- "2-10_KS5destin"
I_KS5 <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))

## 2.3 ONS datasets ----
### 2.3.1 Business demography, UK ----
# Number of enterprise births, deaths and active
# Geography: England and LADS (as of April 2021)
folder <- "2-11_bussdemo"
firstRow <- 4

# births
sheet <- "Table 1.1a"
I_births_ONS1618 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)
sheet <- "Table 1.1b"
I_births_ONS19 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)
sheet <- "Table 1.1c"
I_births_ONS20 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)
sheet <- "Table 1.1d"
I_births_ONS21 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)

# deaths
sheet <- "Table 2.1a"
I_deaths_ONS1618 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)
sheet <- "Table 2.1b"
I_deaths_ONS19 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)
sheet <- "Table 2.1c"
I_deaths_ONS20 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)
sheet <- "Table 2.1d"
I_deaths_ONS21 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)

# active
sheet <- "Table 3.1a"
I_active_ONS1618 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)
sheet <- "Table 3.1b"
I_active_ONS19 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)
sheet <- "Table 3.1c"
I_active_ONS20 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)
sheet <- "Table 3.1d"
I_active_ONS21 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)

### 2.3.2 ONS job adverts by profession ----
folder <- "2-12_OnsProf"
sheet <- "Table 10"
I_OnsProfDetailEng <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 11"
I_OnsProfDetailRegion <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 12"
I_OnsProfDetailLA <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 13"
I_OnsProfDetailLep <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 14"
I_OnsProfDetailLsip <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 15"
I_OnsProfDetailMca <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)

sheet <- "Table 4"
I_OnsProfEng <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 5"
I_OnsProfRegion <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 6"
I_OnsProfLA <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 7"
I_OnsProfLep <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 8"
I_OnsProfLsip <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 9"
I_OnsProfMca <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)

## 2.4 Skills imperative----
# These take a long time to run ~20mins
folder <- "2-13_skillsImperative2035"
dir_path <- paste0("./Data/", folder, "/")
skillsImpFileList <- list.files(dir_path)

read_dir <- function(dir_path, file_name, sheet_name, row_nos) {
  read.xlsx(paste0(dir_path, file_name), sheet = sheet_name, skipEmptyRows = T, rows = row_nos) %>%
    mutate(file_name = file_name)
}

# Industry future
I_wfIndF2 <-
  skillsImpFileList %>%
  map_df(~ read_dir(dir_path, .x, "Ind F2", 4:38))
# Occupation future
I_wfOccF2 <-
  skillsImpFileList %>%
  map_df(~ read_dir(dir_path, .x, "Occ F2", 4:49))
# Qualification future
I_wfQualF1 <-
  skillsImpFileList %>%
  map_df(~ read_dir(dir_path, .x, "Qual F1", 4:14))
# Area names
I_wfAreaName <-
  skillsImpFileList %>%
  map_df(~ read_dir(dir_path, .x, "Info", 2:5)) %>%
  filter(grepl("name", Scenario, fixed = TRUE))

# 3 Dashboard text----
## 3.1 Data sources ----
folder <- "3-1_DataTable"
sheetNum <- 1
I_DataTable <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

## 3.2 Data notes and caveats ----
folder <- "3-2_dataText"
sheetNum <- 1
I_DataText <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

## 3.3 FE Interventions table (not currently used)----
# folder <- "3-3_FeInterventions"
# sheetNum <- 1
# I_InterventionTable <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

## 3.4 Load FE sources and tools tables ----
folder <- "3-4_FeSources"
sheetNum <- "Tools"
I_ToolsTable <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)
sheetNum <- "Sources"
I_SourcesTable <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)
