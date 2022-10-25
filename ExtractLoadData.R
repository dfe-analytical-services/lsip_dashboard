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

## Load list of LEPs 2020 and LA-LSIP lookup ----
folder <- "1_GeogLkup"
sheetNum <- "LAD21 - LSIP - LEP21"
I_LEP2020 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

# Load missing LAD-LEP lookup. This happens because we have some old LADs in the ILR data that have since been made inactive. These do not feature in the most recent LAD-LEP matching. We have manually mapped these LADs to the latest LEPS (2021)
folder <- "2_LEPmissing"
sheetNum <- 2
I_missingLAD <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

## APS ----
### Employment by occupation ----
# Download from https://www.nomisweb.co.uk/datasets/apsnew
# Query data
# Geography: England, LEPs, regions, LADs (as of April 2021)
# Date: 12 months to Dec 2017-2021
# Cell: T09a Employment by occupation (SOC2010) sub-major group and full-time/part-time; All people/ All people
folder <- "3_APSempOcc"
sheetNum <- 1
I_EmpOcc_APS1721 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)


### Employment level and rate ------------
# Geog and date as above
# Cell: T01 Economic activity by age Aged 16-64/ All people
folder <- "4_APSempRate"
sheetNum <- 1
I_EmpRate_APS1822 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

## ILR
### AY21/22 achievements by SSAt1 and LAD ------------
## Permalink: https://explore-education-statistics.service.gov.uk/data-tables/permalink/c390bb3f-8577-40f1-869c-fc8a8195516e
folder <- "5_ILRachSSA"
I_Achieve_ILR21 <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))

### AY16/17-20/21 achievements by LAD and provision------------
## Permalink: https://explore-education-statistics.service.gov.uk/data-tables/permalink/3960ad0f-fd8a-49bb-91d7-f3ca1181b93f
folder <- "6_ILRach"
I_Achieve_ILR1621 <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))

## ONS
### Vacancy by year and LAD ------------
## Download link: https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/datasets/onlinejobadvertsbyitl1regionandlocalauthority
folder <- "7_ONSvacancy"
sheetNum <- "1"
firstRow <- 4
I_Vacancy_ONS1722 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T, startRow = firstRow)


### UK Business Count
# Enterprise by employment size
# Download from https://www.nomisweb.co.uk/query/construct/summary.asp?mode=construct&version=0&dataset=142
# Query data
# Geography: England, LEPs, regions, LADs (as of April 2021)
# Date: 12 months to Dec 2017-2021
# Cell: UK Business Counts - enterprises by industry and employment size band 
folder <- "9_UBCempent"
sheetNum <- 1
I_EmpEnt_APS1721 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)


#National pupil database 
#KS4 AY15/16 - 20/21 - KS4 destinations
# Permalink:https://explore-education-statistics.service.gov.uk/data-tables/permalink/75e2be32-3c51-4790-2c28-08dab0fa305d
folder <- "10_KS4destin"
I_KS4destin_1521 <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))

#KS5 AY17/18 - 20/21 - KS5 destinations
# Permalink:https://explore-education-statistics.service.gov.uk/data-tables/permalink/62b04091-a13b-40e9-52d9-08dab0fd4449
folder <- "11_KS5destin"
I_KS5destin_1721 <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))


## Load data table ----
folder <- "8_DataTable"
sheetNum <- 1
I_DataTable <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)
