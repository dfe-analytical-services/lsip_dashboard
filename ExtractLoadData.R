####
# Title: LSIP dashboard data load
# Author: Paul James
# Date: 30th Aug 2022
# Last updated: 30th Aug 2022
# Loads the data required for the dashboard.
# Ensure there is *only* the file that will be used in each of the numbered folders in ./Data/
###

# Load libraries ----
library(openxlsx)
library(sf)
library(rgdal)

# 1.Geography and mapping tables ----
## 1.1 LEPs 2020 and LA-LSIP lookup ----
folder <- "1_GeogLkup"
sheetNum <- "LAD21 - LSIP - LEP21"
I_LEP2020 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

## 1.2 Missing LAD-LEP lookup----
#This happens because we have some old LADs in the ILR data that have since been made inactive. These do not feature in the most recent LAD-LEP matching. We have manually mapped these LADs to the latest LEPS (2021)
folder <- "2_LEPmissing"
sheetNum <- 2
I_missingLAD <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

## 1.3 MCA lookup ----
folder <- "12_MCA_lookup"
sheetNum <- 1
I_mcalookup <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

## 1.4 LA 2011 to 2021 lookup ----
#https://geoportal.statistics.gov.uk/datasets/ons::local-authority-district-2011-to-local-authority-district-2021-lookup-for-england-and-wales/explore
folder <- "20_LaLookup"
I_LaLookup <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))

## 1.5 LEP boundary----
folder <- "13_LEPBoundary"
I_mapLEP <- sf::st_read(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))),
                        stringsAsFactors = F
)

## 1.6 LA boundary----
folder <- "14_LABoundary"
I_mapLA <- sf::st_read(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))),
                       stringsAsFactors = F
)

## 1.7 MCA boundary----
folder <- "15_MCABoundary"
I_mapMCA <- sf::st_read(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))),
                        stringsAsFactors = F
)

## 1.8 LA shapefile ----
folder <- "14_LABoundary"
LAs <- readOGR(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))

# 2. Datasets ----
## 2.1 Nomis datasets ----
### 2.1.1 Employment by occupation ----
# Download from https://www.nomisweb.co.uk/datasets/apsnew
# Query data
# Geography: England, LEPs, regions, LADs (as of April 2021)
# Date: 12 months to Dec 2017-2021
# Cell: T09a Employment by occupation (SOC2010) sub-major group and full-time/part-time; All people/ All people
folder <- "3_APSempOcc"
sheetNum <- 1
I_empOcc <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

### 2.1.2 Employment level and rate ------------
# Geog and date as above
# Cell: T01 Economic activity by age Aged 16-64/ All people
folder <- "4_APSempRate"
sheetNum <- 1
I_emp<- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

### 2.1.3 UK Business Count----
# Enterprise by employment size
# Download from https://www.nomisweb.co.uk/query/construct/summary.asp?mode=construct&version=0&dataset=142
# Query data
# Geography: England, LEPs, regions, LADs (as of April 2022)
# Date: 12 months to Dec 2018-2022
# Cell: UK Business Counts - enterprises by industry and employment size band
folder <- "9_UBCempent"
sheetNum <- 1
I_entSize <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

### 2.1.4 Skill by age gender ------------
# Geog and date as above
# Cell: T19	Qualification by age and gender - NVQ. All people aged 16-64. Only data up to Dec 21 available
folder <- "14_APSqualagegen"
sheetNum <- 1
I_qualAgeGender <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

### 2.1.5 Employment by industry------------
# Geog and date as above
# Cell: T13a	Employment by industry (SIC 2007) and flexibility
folder <- "13_APSempind"
sheetNum <- 1
I_empInd <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

## 2.2 EES datasets----
### 2.2.1 Achievements by SSAt1, LAD, gender, level------------
## Permalink: https://explore-education-statistics.service.gov.uk/data-tables/permalink/2be7f950-b9ff-4f22-d77a-08dabcdedabe
folder <- "5_ILRachSSA"
I_FeSsa <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))

### 2.2.2 Achievements/starts/part by LAD and provision, level and age------------
## Download "Further education and skills geography - detailed summary " from https://explore-education-statistics.service.gov.uk/data-catalogue/further-education-and-skills/2021-22
folder <- "6_ILRach"
I_FeProvLevelAge <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))

### 2.2.3 KS4 destinations----
# National pupil database
# Permalink:https://explore-education-statistics.service.gov.uk/data-tables/permalink/75e2be32-3c51-4790-2c28-08dab0fa305d
folder <- "10_KS4destin"
I_KS4 <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))

### 2.2.4 KS5 destinations----
# Permalink:https://explore-education-statistics.service.gov.uk/data-tables/permalink/62b04091-a13b-40e9-52d9-08dab0fd4449
folder <- "11_KS5destin"
I_KS5 <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))

## 2.3 ONS datasets ----
### 2.3.1 Business demography, UK ----
# Number of enterprise births, deaths and active
# Download from https://www.ons.gov.uk/businessindustryandtrade/business/activitysizeandlocation/datasets/businessdemographyreferencetable
# Georgraphy: England and LADS (as of April 2021)
folder <- "16_bussdemo"
sheet <- "Table 1.1a"
firstRow <- 4

# births
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
folder <- "17_OnsProf"
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
folder <- "21_skillsImperative2035"
dir_path <- paste0("./Data/", folder, "/")
skillsImpFileList<-list.files(dir_path)

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
  skillsImpFileList%>%
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
folder <- "8_DataTable"
sheetNum <- 1
I_DataTable <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

## 3.2 Data notes and caveats ----
folder <- "19_dataText"
sheetNum <- 1
I_DataText <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

# ## 3.3 FE Interventions table (not currently used)----
# folder <- "17_FeInterventions"
# sheetNum <- 1
# I_InterventionTable <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

## 3.4 Load FE sources and tools tables ----
folder <- "18_FeSources"
sheetNum <- "Tools"
I_ToolsTable <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)
sheetNum <- "Sources"
I_SourcesTable <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

