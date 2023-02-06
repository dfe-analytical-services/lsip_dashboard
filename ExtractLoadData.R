####
# Title: LSIP dashboard data load
# Author: Paul James
# Date: 30th Aug 2022
# Last updated: 30th Aug 2022
# Loads the data required for the dashboard.
# Ensure there is *only* the file that will be used in each of the numbered folders in ./Data/
###

# 0. Load libraries ----
library(openxlsx)
# library(geojsonio)

## Load list of LEPs 2020 and LA-LSIP lookup ----
folder <- "1_GeogLkup"
sheetNum <- "LAD21 - LSIP - LEP21"
I_LEP2020 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

# Load missing LAD-LEP lookup. This happens because we have some old LADs in the ILR data that have since been made inactive. These do not feature in the most recent LAD-LEP matching. We have manually mapped these LADs to the latest LEPS (2021)
folder <- "2_LEPmissing"
sheetNum <- 2
I_missingLAD <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

# Load MCA lookup
folder <- "12_MCA_lookup"
sheetNum <- 1
I_mcalookup <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)


# 1. APS ----
### 1.1Employment by occupation ----
# Download from https://www.nomisweb.co.uk/datasets/apsnew
# Query data
# Geography: England, LEPs, regions, LADs (as of April 2021)
# Date: 12 months to Dec 2017-2021
# Cell: T09a Employment by occupation (SOC2010) sub-major group and full-time/part-time; All people/ All people.
# Currently only available up to Dec 2021
folder <- "3_APSempOcc"
sheetNum <- 1
I_EmpOcc_APS1721 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)


### 1.2 Employment level and rate ------------
# Geog and date as above
# Cell: T01 Economic activity by age Aged 16-64/ All people. Most recent data available (Sept 22)
folder <- "18_APS_jan23_update"
sheetNum <- 1
I_EmpRate_APS1822 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", "T01.xlsx"), sheet = sheetNum, skipEmptyRows = T)


### 1.3 employment by industry------------
# Geog and date as above
# Cell: T13a	Employment by industry (SIC 2007) and flexibility. Most recent data available (Sept 2022)
folder <- "18_APS_jan23_update"
sheetNum <- 1
I_empind_APS1822 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", "T13a.xlsx"), sheet = sheetNum, skipEmptyRows = T)

### 1.4 Skill by age gender ------------
# Geog and date as above
# Cell: T19	Qualification by age and gender - NVQ. All people aged 16-64. Only data up to Dec 21 available
folder <- "14_APSqualagegen"
sheetNum <- 1
I_qual_APS1721 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)


# 2. ILR ----
### 2.1 AY21/22 achievements by SSAt1, LAD, gender, level------------
## Permalink: https://explore-education-statistics.service.gov.uk/data-tables/permalink/2be7f950-b9ff-4f22-d77a-08dabcdedabe
folder <- "5_ILRachSSA"
I_Achieve_ILR21 <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))

### 2.2 AY16/17-20/21 achievements/starts/part by LAD and provision, level and age------------
## Download "Further education and skills geography - detailed summary " from https://explore-education-statistics.service.gov.uk/data-catalogue/further-education-and-skills/2021-22
folder <- "6_ILRach"
I_Achieve_ILR1621 <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))

# 3.ONS ----
### 3.1 Vacancy by year and LAD ------------
# ## Download link: https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/datasets/onlinejobadvertsbyitl1regionandlocalauthority
# folder <- "7_ONSvacancy"
# sheetNum <- "1"
# firstRow <- 4
# I_Vacancy_ONS1722 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T, startRow = firstRow)
#

### 3.1 UK Business Count ----
# Enterprise by employment size
# Download from https://www.nomisweb.co.uk/query/construct/summary.asp?mode=construct&version=0&dataset=142
# Query data
# Geography: England, LEPs, regions, LADs (as of April 2022)
# Date: 12 months to Dec 2018-2022
# Cell: UK Business Counts - enterprises by industry and employment size band
folder <- "9_UBCempent"
sheetNum <- 1
I_EmpEnt_APS1822 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

## 3.2 Enterprise by employment size and industry ----
folder <- "15_UBCempentind"
sheetNum <- 1
I_EmpEntind_APS1822 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)


## 3.3 Business demography, UK ----
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

## 3.4 Load ONS job vacancies by profession data ----
folder <- "17_OnsProf"
sheet <- "Table 10"
I_OnsProfDetailEng <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 11"
I_OnsProfDetailRegion <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
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
sheet <- "Table 7"
I_OnsProfLep <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 8"
I_OnsProfLsip <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 9"
I_OnsProfMca <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)


# 4.National pupil database ----
# KS4 AY15/16 - 20/21 - KS4 destinations
# Permalink:https://explore-education-statistics.service.gov.uk/data-tables/permalink/75e2be32-3c51-4790-2c28-08dab0fa305d
folder <- "10_KS4destin"
I_KS4destin_1521 <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))

## 4.1 KS5 AY17/18 - 20/21 - KS5 destinations ----
# Permalink:https://explore-education-statistics.service.gov.uk/data-tables/permalink/62b04091-a13b-40e9-52d9-08dab0fd4449
folder <- "11_KS5destin"
I_KS5destin_1721 <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))

# 5.Load data table ----
folder <- "8_DataTable"
sheetNum <- 1
I_DataTable <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

## Load lep boundary----
folder <- "13_LEPBoundary"
I_mapLEP <- sf::st_read(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))),
  stringsAsFactors = F
)

## Load la boundary----
folder <- "14_LABoundary"
I_mapLA <- sf::st_read(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))),
  stringsAsFactors = F
)

## Load MCA boundary----
folder <- "15_MCABoundary"
I_mapMCA <- sf::st_read(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))),
  stringsAsFactors = F
)

### employment by industry------------
# Geog and date as above
# Cell: T13a	Employment by industry (SIC 2007) and flexibility
folder <- "13_APSempind"
sheetNum <- 1
I_empind_APS1822 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

## Load Fe Interventions table ----
folder <- "17_FeInterventions"
sheetNum <- 1
I_InterventionTable <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

## Load Fe sources table ----
folder <- "18_FeSources"
sheetNum <- 1
I_SourcesTable <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)


### Skill by age gender ------------
# Geog and date as above
# Cell: T19	Qualification by age and gender - NVQ. All people aged 16-64. Only data up to Dec 21 available
folder <- "14_APSqualagegen"
sheetNum <- 1
I_qual_APS1721 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)


### UK Business Count
# Enterprise by employment size
# Download from https://www.nomisweb.co.uk/query/construct/summary.asp?mode=construct&version=0&dataset=142
# Query data
# Geography: England, LEPs, regions, LADs (as of April 2022)
# Date: 12 months to Dec 2018-2022
# Cell: UK Business Counts - enterprises by industry and employment size band
folder <- "9_UBCempent"
sheetNum <- 1
I_EmpEnt_APS1822 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

# Enterprise by employment size and industry
folder <- "15_UBCempentind"
sheetNum <- 1
I_EmpEntind_APS1822 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)


# National pupil database
# KS4 AY15/16 - 20/21 - KS4 destinations
# Permalink:https://explore-education-statistics.service.gov.uk/data-tables/permalink/75e2be32-3c51-4790-2c28-08dab0fa305d
folder <- "10_KS4destin"
I_KS4destin_1521 <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))

# KS5 AY17/18 - 20/21 - KS5 destinations
# Permalink:https://explore-education-statistics.service.gov.uk/data-tables/permalink/62b04091-a13b-40e9-52d9-08dab0fd4449
folder <- "11_KS5destin"
I_KS5destin_1721 <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))

# Business demography, UK
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


## Load ONS job vacancies by profession data
folder <- "17_OnsProf"
sheet <- "Table 9"
I_OnsProfLA <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 10"
I_OnsProfLep <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 11"
I_OnsProfLsip <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 12"
I_OnsProfMca <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
# sheet <- "Table 1"
# I_OnsProfEng <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 7"
I_OnsProfDetailEng <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
