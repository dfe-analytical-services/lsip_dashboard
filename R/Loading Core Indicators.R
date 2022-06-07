# Title: LSIP dashboard - proof of concept
# Author: Hannah Cox
# Date: 18th May 2022
# Last updated: 30th May 2022



## ---- Load libraries ----
library(dplyr)
library(data.table)
library(tidyverse)
library(stringr)
library(eeptools)
library(odbc)
library(ggplot2)

## ---- Load data ----
### ---- LEP 2020
I_LEP2020 <- fread('./Data/2020 LEP.csv')
C_LEP2020 <- I_LEP2020 %>%
  select(ERNM)

### ---- APS
I_APS2021 <- fread('./Data/Employment by occupation - Dec 2021 - SOC2010 - APS.csv', data.table=FALSE) 
I_APS2020 <- fread('./Data/Employment by occupation - Dec 2020 - SOC2010 - APS.csv', data.table=FALSE)
I_APS2019 <- fread('./Data/Employment by occupation - Dec 2019 - SOC2010 - APS.csv', data.table=FALSE)
I_APS2018 <- fread('./Data/Employment by occupation - Dec 2018 - SOC2010 - APS.csv', data.table=FALSE)
I_APS2017 <- fread('./Data/Employment by occupation - Dec 2017 - SOC2010 - APS.csv', data.table=FALSE)
I_APS2016 <- fread('./Data/Employment by occupation - Dec 2016 - SOC2010 - APS.csv', data.table=FALSE)
I_APS2015 <- fread('./Data/Employment by occupation - Dec 2015 - SOC2010 - APS.csv', data.table=FALSE)

# BRES - need to find out how to remove commas from numbers
I_BRES2020 <- fread('./Data/Employment by sector - BRES 2020.csv') #, data.table=FALSE)
I_BRES2019 <- fread('./Data/Employment by sector - BRES 2019.csv') #, data.table=FALSE)
I_BRES2018 <- fread('./Data/Employment by sector - BRES 2018.csv') #, data.table=FALSE)
I_BRES2017 <- fread('./Data/Employment by sector - BRES 2017.csv') #, data.table=FALSE)
I_BRES2016 <- fread('./Data/Employment by sector - BRES 2016.csv') #, data.table=FALSE)
I_BRES2015 <- fread('./Data/Employment by sector - BRES 2015.csv') #, data.table=FALSE)

  # Transpose 

# ESS
I_ESS2019 <- fread('./Data/ESS_2019.csv')

  # Save tabs as csv to import

# KS4 Destinations
I_KS4 <- fread('./Data/KS4 destinations - 201011 - 2021920 .csv')

## ---- Functions ----
format.APS <- function(x) {
  x %>% discard(~all(is.na(.)))%>%
    mutate(Male = ifelse(grepl("Males", Cell), "Male", ""), # Tag for males
           Female = ifelse(grepl("Females", Cell), "Female", ""), # Tag for females
           Total = ifelse(Female == "" & Male == "", "Total", ""), #Tag for total
           Gender = trimws(paste(Male, Female, Total)))%>% # Make new Gender column from Cell
    select(-Male, -Female, -Total)%>%
    mutate(FT = ifelse(grepl("Full-time", Cell), "Full-time",""), # Tag for FT
           PT = ifelse(grepl("Part-time", Cell), "Part-time", ""), # Tag for PT
           Total = ifelse(FT == "" & PT == "", "Total", ""), # Tag for total
           WorkPattern = trimws(paste(FT, PT, Total)))%>% # Make new WorkPattern column from Cell
    select(-FT, -PT, -Total)%>%
    mutate(SOC2010title = gsub(".*- (.+) :.*", "\\1",Cell))%>% # Clean up and rename 
    select(SOC2010title, Gender, WorkPattern, everything(), -Cell, -starts_with('Conf'))%>% # Remove conf
    mutate_all(funs(replace(.,.=="!"|.=="~",NA)))%>% # Replace ! and ~ with NA
    mutate_at(c(4:210),as.numeric)%>% # Convert to numeric
    select(1:42) # remove nuts 3 for now
}

format.BRES <- function(x) {
  x %>%
    discard(~all(is.na(.)))%>%
    slice(-1)%>%
    mutate_all(funs(replace(.,.=="" | .== "*",NA)))%>% # Replace "" and * with NA
    discard(~all(is.na(.)))%>% # Remove empty columns
    mutate_at(c(2:40),as.numeric)%>%
    filter(!grepl("nuts", Area)) %>% # remove nuts 3 for now
    mutate(Area = gsub(".*:", "", Area)) # Clean up LEP names
}

formant.ESS <- function(x) {
  
}

## --- Employment by occupation ---
C_APS2015 <- format.APS(I_APS2015)%>%
  mutate(Year = 2015)
C_APS2016 <- format.APS(I_APS2016)%>%
  mutate(Year = 2016)
C_APS2017 <- format.APS(I_APS2017)%>%
  mutate(Year = 2017)
C_APS2018 <- format.APS(I_APS2018)%>%
  mutate(Year = 2018)
C_APS2019 <- format.APS(I_APS2019)%>%
  mutate(Year = 2019)
C_APS2020 <- format.APS(I_APS2020)%>%
  mutate(Year = 2020)
C_APS2021 <- format.APS(I_APS2021)%>%
  mutate(Year = 2021)

C_APS1521 <- rbind(C_APS2015, C_APS2016, C_APS2017, C_APS2018, C_APS2019, C_APS2020, C_APS2021)

rm(C_APS2015,
   C_APS2016,
   C_APS2017,
   C_APS2018,
   C_APS2019,
   C_APS2020,
   C_APS2021,
   I_APS2015,
   I_APS2016,
   I_APS2017,
   I_APS2018,
   I_APS2019,
   I_APS2020,
   I_APS2021)

## ---- Employment by sector ----
C_BRES2020 <- format.BRES(I_BRES2020)%>%
  mutate(Year = 2020)
C_BRES2019 <- format.BRES(I_BRES2019)%>%
  mutate(Year = 2019)
C_BRES2018 <- format.BRES(I_BRES2018)%>%
  mutate(Year = 2018)
C_BRES2017 <- format.BRES(I_BRES2017)%>%
  mutate(Year = 2017)
C_BRES2016 <- format.BRES(I_BRES2016)%>%
  mutate(Year = 2016)
C_BRES2015 <- format.BRES(I_BRES2015)%>%
  mutate(Year = 2015)

C_BRES1520 <- rbind(C_BRES2015, C_BRES2016, C_BRES2017, C_BRES2018, C_BRES2019, C_BRES2020)

rm(C_BRES2015,
   C_BRES2016,
   C_BRES2017,
   C_BRES2018,
   C_BRES2019,
   C_BRES2020,
   I_BRES2015,
   I_BRES2016,
   I_BRES2017,
   I_BRES2018,
   I_BRES2019,
   I_BRES2020)

## ---- Employer reported skills that will need developing ----

# ---- Proficiency of workforce ----

# ---- Summary of vacancies (skills shortage and hard to fill) ----



## ---- Save to SQL ----
#con <- dbConnect(odbc(), Driver = "SQL Server Native Client 11.0", 
                 
#                 Server = "T1PRANMSQL\SQLPROD,60125", 
                 
 #                Database = "MA_UFS_S_DATA",
                 
 #                Trusted_Connection = "yes")



#and then I think you can use dbWriteTable to save the table 
#(haven't used it before so you may need to tweak the code below). I'm calling X your R table.
                                                             
                                                             
#con %>% dbWriteTable("MA_UFS_S_DATA.LSIP.dashboard_data", X)
