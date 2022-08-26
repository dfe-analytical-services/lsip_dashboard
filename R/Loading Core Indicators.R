####
# Title: LSIP dashboard - proof of concept
# Author: Hannah Cox/Paul James
# Date: 18th May 2022
# Last updated: 15th June 2022
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
I_LEP2020 <- read.xlsx(xlsxFile = "./Data/OA11_LAD21_LSOA11_MSOA11_LEP21_EN_v3.xlsx", sheet = 1, skipEmptyRows = T)
# list leps for dropdowns
C_LEP2020 <- I_LEP2020 %>%
  distinct(LEP = LEP21NM1) %>%
  arrange(LEP)
# Load missing LAD-LEP lookup. This happens because we have some old LADs in the ILR data that have since been made inactive. These do not feature in the most recent LAD-LEP matching. We have manually mapped these LADs to the latest LEPS (2021)
I_missingLAD <- read.xlsx(xlsxFile = "./Data/missing_leps.xlsx", sheet = 2, skipEmptyRows = T)
# Create LAD-LEP lookup table
C_LADLEP2020 <- distinct(I_LEP2020, LAD21CD, LAD21NM, LEP = LEP21NM1) %>%
  bind_rows(I_missingLAD %>% filter(LAD21CD != "z") %>% select(LAD21CD, LEP = `LEP21.(manually.mapped)`)) %>%
  bind_rows(distinct(I_LEP2020 %>% filter(is.na(LEP21NM2) == FALSE), LAD21CD, LAD21NM, LEP = LEP21NM2))

## APS ----
### Core indicator 2: Employment by occupation ----
# Download from https://www.nomisweb.co.uk/datasets/apsnew
# Query data
# Geography: England, LEPs, regions, LADs (as of April 2021)
# Date: 12 months to Dec 2017-2021
# Cell: T09a Employment by occupation (SOC2010) sub-major group and full-time/part-time; All people/ All people
I_EmpOcc_APS1721 <- read.xlsx(xlsxFile = "./Data/nomis_2022_06_14_092401.xlsx", skipEmptyRows = T)

### Core indicator 5: Employment level and rate ------------
# Geog and date as above
# Cell: T01 Economic activity by age Aged 16-64/ All people
I_EmpRate_APS1721 <- read.xlsx(xlsxFile = "./Data/nomis_2022_06_14_095314.xlsx", sheet = 1, skipEmptyRows = T)

## ILR
### Core indicator 12: AY21/22 achievements by SSAt1 and LAD ------------
## Permalink: https://explore-education-statistics.service.gov.uk/data-tables/permalink/c390bb3f-8577-40f1-869c-fc8a8195516e
I_Achieve_ILR21 <- read.csv(file = "./Data/permalink-c390bb3f-8577-40f1-869c-fc8a8195516e.csv")

### Core indicator 13: AY16/17-20/21 achievements by LAD and provision------------
## Permalink: https://explore-education-statistics.service.gov.uk/data-tables/permalink/3960ad0f-fd8a-49bb-91d7-f3ca1181b93f
I_Achieve_ILR1621 <- read.csv(file = "./Data/permalink-3960ad0f-fd8a-49bb-91d7-f3ca1181b93f.csv")

## ONS
### Core indicator 22: Vacancy by year and LAD ------------
## Download link: https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/datasets/onlinejobadvertsbyitl1regionandlocalauthority
I_Vacancy_ONS1722 <- read.xlsx(xlsxFile = "./Data/referencetablesupdated.xlsx", sheet = "1", skipEmptyRows = T, startRow = 4)

# Data cleaning functions ----
## Employment by occupation ----
format.EmpOcc.APS <- function(x) {
  x %>%
    mutate(year = ifelse(annual.population.survey == "date", X2, NA)) %>% # tag time periods
    fill(year) %>% # fill time periods for all rows
    row_to_names(row_number = 4) %>% # set col names
    clean_names() %>%
    select(-starts_with("na")) %>% # remove na columns (flags and confidence)
    mutate(check = ifelse(grepl(":", area), 1, 0)) %>% # remove anything but LEP and Country
    filter(check == 1) %>%
    filter(!grepl("nomisweb", area)) %>%
    select(year = jan_2017_dec_2017, area, everything(), -check) %>% # reorder and remove
    mutate(geographic_level = gsub(":.*", "", area)) %>% # Get geog type
    mutate(area = gsub(".*:", "", area)) %>% # Tidy up Area names
    mutate(area = case_when(
      area == "Hull and East Riding" ~ "Hull and East Yorkshire",
      area == "Buckinghamshire Thames Valley" ~ "Buckinghamshire",
      TRUE ~ area
    )) %>% # Rename so matches official name
    relocate(geographic_level, .after = area) %>%
    mutate(year = as.numeric(substr(year, 5, 8))) %>%
    rename_with(
      .fn = ~ str_replace_all(.x, c("t09a_" = "", "all_people" = "", "soc2010" = "", "_" = " ")),
      .cols = starts_with("t09a_")
    ) %>%
    rename_with(~ gsub("[[:digit:]]+", "", .)) # remove numbers from occupations since they don't match the ONS ones
}
# format data
F_EmpOcc_APS1721 <- format.EmpOcc.APS(I_EmpOcc_APS1721)
# create downloadable version with new suppression rules
D_EmpOcc_APS1721 <- F_EmpOcc_APS1721 %>%
  mutate_at(vars(-year, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "c", "\\*" = "u", "~" = "low", "-" = "x")))
# create version to use in dashboard
C_EmpOcc_APS1721 <- F_EmpOcc_APS1721 %>%
  mutate_at(vars(-year, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = ""))) %>% # convert to blank to avoid error msg
  mutate_at(c(4:28), as.numeric) %>% # Convert to numeric
  filter(
    year == "2021",
    geographic_level == "lep" |
      geographic_level == "country" # cleans up for London and South East which is included as lep and gor
  ) %>%
  select(-year, -geographic_level) %>%
  rename_with(str_to_sentence) %>% # capitalise column titles
  t() %>%
  row_to_names(row_number = 1) %>%
  as.data.frame() %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(across(where(is.numeric), ~ round(prop.table(.), 4)))


## Employment level and rate ----
format.EmpRate.APS <- function(x) {
  x %>%
    mutate(year = ifelse(annual.population.survey == "date", substr(X2, nchar(X2) - 4 + 1, nchar(X2)), NA)) %>% # tag time periods
    fill(year) %>% # fill time periods for all rows
    row_to_names(row_number = 4) %>% # set col names
    clean_names() %>%
    select(-starts_with("na")) %>% # remove na columns (flags and confidence)
    mutate(check = ifelse(grepl(":", area), 1, 0)) %>% # remove anything but LEP and Country
    filter(check == 1) %>%
    filter(!grepl("nomisweb", area)) %>%
    mutate(geographic_level = gsub(":.*", "", area)) %>% # Get geog type
    mutate(area = gsub(".*:", "", area)) %>% # Tidy up Area names
    mutate(area = case_when(
      area == "Hull and East Riding" ~ "Hull and East Yorkshire",
      area == "Buckinghamshire Thames Valley" ~ "Buckinghamshire",
      TRUE ~ area
    )) %>% # Rename so matches official name
    select(year = x2017, area, everything(), -check) %>% # reorder and remove
    relocate(geographic_level, .after = area) %>%
    rename_with(
      .fn = ~ str_replace_all(.x, c("t01_" = "", "all_people" = "", "aged_16_64" = "", "_" = " ")),
      .cols = starts_with("t01")
    )
}

# format data
F_EmpRate_APS1721 <- format.EmpRate.APS(I_EmpRate_APS1721)
# create downloadable version with new suppression rules
D_EmpRate_APS1721 <- F_EmpRate_APS1721 %>%
  mutate_at(vars(-year, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "c", "\\*" = "u", "~" = "low", "-" = "x")))
# create version to use in dashboard
C_EmpRate_APS1721 <- F_EmpRate_APS1721 %>%
  mutate_at(vars(-year, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = ""))) %>% # convert to blank to avoid error msg
  mutate_at(c(4:10), as.numeric) %>% # Convert to numeric
  mutate(empRate = .[[6]] / .[[4]]) %>%
  filter(
    geographic_level == "lep" # cleans up for London which is included as lep and gor
    | area == "England" # for use as comparison
  ) %>%
  mutate(Year = as.numeric(substr(year, 3, 4))) %>% # for use in charts
  rename(Employment = `28  in employment `) # for use in charts

# create max and min emp count and rate by LEP for use in setting axis
C_EmpRate_APS1721_max_min <- C_EmpRate_APS1721 %>%
  group_by(area) %>%
  summarise(minEmp = min(Employment), maxEmp = max(Employment))

# Clean ILR column names, reorder and reformat
format.AchieveSSA.ILR <- function(x) { # need to clean up colnames
  colnames(x)[1] <- "area"
  x %>%
    left_join(select(C_LADLEP2020, -LAD21NM), by = c("location_code" = "LAD21CD")) %>%
    relocate(LEP, .after = geographic_level) %>%
    relocate(time_period, .before = area) %>%
    rename_all(recode, e_and_t_aims_ach = "achievements")
}

## format achievements
F_Achieve_ILR1621 <- format.AchieveSSA.ILR(I_Achieve_ILR1621)
# create downloadable version with new suppression rules
D_Achieve_ILR1621 <- F_Achieve_ILR1621 %>%
  mutate_at(vars(achievements), function(x) str_replace_all(x, c("!" = "c", "\\*" = "u", "~" = "low", "-" = "x")))
# create version to use in dashboard
C_Achieve_ILR1621 <- F_Achieve_ILR1621 %>%
  mutate_at(vars(achievements), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = ""))) %>% # convert to blank to avoid error msg
  mutate(achievements = as.numeric(achievements)) %>%
  mutate(Year = as.numeric(substr(time_period, 3, 4))) %>% # add year name for charts
  filter(time_period != "202122") %>% # ignore temporary data in the latest year
  group_by(Year, time_period, LEP, level_or_type) %>%
  summarise(Ach = sum(achievements), .groups = "drop") %>%
  mutate(level_or_typeNeat = case_when(
    level_or_type == "Further education and skills: Total" ~ "Total FE and skills provision",
    level_or_type == "Education and training: Total" ~ "Education and training (adults only)",
    level_or_type == "Community learning: Total" ~ "Community learning (adults only)",
    level_or_type == "Apprenticeships: Total" ~ "Apprenticeships (all ages)",
    TRUE ~ level_or_type
  )) %>%
  mutate(AY = paste(substr(time_period, 3, 4), "/", substr(time_period, 5, 6), sep = ""))

# create max and min vacancy pc by LEP for use in setting axis
C_Achieve_ILR1621_max_min <- C_Achieve_ILR1621 %>%
  group_by(LEP, level_or_type) %>%
  summarise(minAch = min(Ach), maxAch = max(Ach), .groups = "drop")

## format achievements by SSA
F_Achieve_ILR21 <- format.AchieveSSA.ILR(I_Achieve_ILR21)
# create downloadable version with new suppression rules
D_Achieve_ILR21 <- F_Achieve_ILR21 %>%
  mutate_at(vars(achievements), function(x) str_replace_all(x, c("!" = "c", "\\*" = "u", "~" = "low", "-" = "x")))
# create version to use in dashboard
# group by ssa and lep
SSA_LEP_Achieve_ILR21 <- F_Achieve_ILR21 %>%
  mutate_at(vars(achievements), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = ""))) %>% # convert to blank to avoid error msg
  mutate(achievements = as.numeric(achievements)) %>%
  filter(time_period == "202122") %>%
  select(LEP, SSA = ssa_t1_desc, Achievements = achievements) %>%
  group_by(LEP, SSA) %>%
  summarise(Achievements = sum(Achievements, na.rm = T), .groups = "drop") %>%
  ungroup()
# group by ssa to get ssa totals
Ach_pc_Achieve_ILR21 <- SSA_LEP_Achieve_ILR21 %>%
  filter(SSA == "Total") %>%
  select(LEP, Total = SSA, Total_ach = Achievements)
# calculate ssa %s
C_Achieve_ILR21 <- SSA_LEP_Achieve_ILR21 %>%
  left_join(Ach_pc_Achieve_ILR21, by = "LEP") %>%
  group_by(LEP) %>%
  mutate(pc = Achievements / Total_ach) %>%
  filter(SSA != "Total")

# Reshape vacancy data to long, rename and reorder and reformat some columns
format.Vacancy.ONS <- function(x) { # need to clean up colnames
  x %>%
    gather(year, vacancy_unit, 3:8) %>%
    rename(LA = "Local.authority.[note.1]", region = "Region.[note.2]") %>%
    left_join(select(C_LADLEP2020, -LAD21CD), by = c("LA" = "LAD21NM")) %>%
    relocate(LEP, .after = region) %>%
    relocate(year, .before = LA) %>%
    mutate(year = as.numeric(year))
}

# vacancy
C_Vacancy_ONS1722 <- format.Vacancy.ONS(I_Vacancy_ONS1722)

# vacancy data to use in dashboard
C_Vacancy_England <- C_Vacancy_ONS1722 %>%
  filter(!region %in% c("Wales", "Scotland", "Northern Ireland")) %>%
  group_by(year) %>%
  summarise(England = sum(vacancy_unit)) %>%
  right_join(C_Vacancy_ONS1722, by = "year") %>%
  mutate(pc_total = vacancy_unit / England) %>%
  mutate(Year = as.numeric(substr(year, 3, 4))) %>%
  group_by(LEP, year, Year) %>%
  summarise(jobpc = sum(pc_total), jobcnt = sum(vacancy_unit), .groups = "drop")

# create max and min vacancy pc by LEP for use in setting axis
C_Vacancy_England_max_min <- C_Vacancy_England %>%
  group_by(LEP) %>%
  summarise(minVac = min(jobpc), maxVac = max(jobpc))

# create change vacancy pc by LEP
C_Vacancy_England_change <- C_Vacancy_England %>%
  filter(year == "2022" | year == "2021") %>%
  mutate(Row = 1:n()) %>%
  mutate(Percentage_Change = (jobcnt / lag(jobcnt)) - 1) %>%
  filter(year == "2022") %>%
  select(LEP, Percentage_Change)
