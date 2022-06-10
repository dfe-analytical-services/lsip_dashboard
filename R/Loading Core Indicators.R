####
# Title: LSIP dashboard - proof of concept
# Author: Hannah Cox
# Date: 18th May 2022
# Last updated: 8th June 2022
###

# Load libraries ----
library(dplyr)
library(data.table)
library(tidyverse)
library(stringr)
library(eeptools)
library(odbc)
library(ggplot2)
library(openxlsx)
library(janitor)

# Load data ----
## LEP 2020 ----
I_LEP2020 <- read.xlsx(xlsxFile="./Data/2020 LEP.xlsx", sheet=1, skipEmptyRows=T)
C_LEP2020 <- I_LEP2020 %>%
  select(ERNM)

## APS ----
### Employment by occupation ----
# Download from https://www.nomisweb.co.uk/datasets/apsnew
#Query data
#Geography: England, LEPs (as of April 2021)
#Date: 12 months to Dec 2017-2021
#Cell: T09b Employment by occupation (SOC2010) sub-major group and full-time/part-time; All people/ All people

I_EmpOcc_APS1721 <- read.xlsx(xlsxFile="./Data/nomis_2022_06_08_092847.xlsx", sheet=1, skipEmptyRows=T)

### Employment level and rate ------------
#Cell: T01 Economic activity by age Aged 16-64/ All people

I_EmpRate_APS1721 <- read.xlsx(xlsxFile="./Data/nomis_2022_06_08_111841.xlsx", sheet=1, skipEmptyRows=T)



# Functions ----
format.EmpOcc.APS <- function(x) { # need to clean up colnames
  x %>% mutate(year = ifelse(annual.population.survey == "date", X2, NA))%>% # tag time periods
    fill(year)%>% # fill time periods for all rows
    row_to_names(row_number=4) %>% # set col names
    clean_names()%>%
    select(-starts_with('na'))%>% # remove na columns (flags and confidence)
    mutate(check = ifelse(grepl(":", area), 1, 0))%>% # remove anything but LEP and Country
    filter(check ==1)%>%
    filter(!grepl("nomisweb", area))%>%
    select(year = jan_2017_dec_2017, area, everything(), - check) %>%# reorder and remove
    mutate(area = gsub(".*:", "", area)) %>% # Tidy up Area names
    mutate_at(c(3:27),as.numeric) # Convert to numeric
}

format.EmpRate.APS <- function(x) { # need to clean up colnames
  x %>% mutate(year = ifelse(annual.population.survey == "date", substr(X2,nchar(X2)-4+1, nchar(X2)), NA))%>% # tag time periods
    fill(year)%>% # fill time periods for all rows
    row_to_names(row_number=4) %>% # set col names
    clean_names()%>%
    select(-starts_with('na'))%>% # remove na columns (flags and confidence)
    mutate(check = ifelse(grepl(":", area), 1, 0))%>% # remove anything but LEP and Country
    filter(check ==1)%>%
    filter(!grepl("nomisweb", area))%>%
    mutate(area = gsub(".*:", "", area))%>% # Tidy up Area names
    mutate_at(c(2:9),as.numeric)%>% # Convert to numeric
    select(year=x2017, area, everything(), - check)%>%# reorder and remove
    mutate(empRate = .[[5]]/.[[3]])
}

# Indicators ----
## Employment by occupation ----
C_EmpOcc_APS1721 <- format.EmpOcc.APS(I_EmpOcc_APS1721)

## Employment level and rate ----
C_EmpRate_APS1721 <- format.EmpRate.APS(I_EmpRate_APS1721)


# Save to SQL ----
#con <- dbConnect(odbc(), Driver = "SQL Server Native Client 11.0", 

#                 Server = "T1PRANMSQL\SQLPROD,60125", 

#                Database = "MA_UFS_S_DATA",

#                Trusted_Connection = "yes")



#and then I think you can use dbWriteTable to save the table 
#(haven't used it before so you may need to tweak the code below). I'm calling X your R table.


#con %>% dbWriteTable("MA_UFS_S_DATA.LSIP.dashboard_data", X)

# Combine into single workbook ----



