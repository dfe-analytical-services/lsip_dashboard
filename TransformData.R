####
# Title: LSIP dashboard data transform
# Author: Hannah Cox/Paul James
# Date: 18th May 2022
# Last updated: 30th Aug 2022
# Transforms the raw data into the dataframes used in the dashboard.
# The dataframes are then saved as .csv in ./Data/AppData to be read by the dashboard app
###

# Load libraries ----
library(dplyr)
library(data.table)
library(tidyverse)
library(stringr)
library(eeptools)
library(odbc)
library(janitor)
library(openxlsx)

# list leps for dropdowns
C_LEP2020 <- I_LEP2020 %>%
  distinct(LEP = LEP21NM1) %>%
  arrange(LEP)
write.csv(C_LEP2020, file = "Data\\AppData\\C_LEP2020.csv", row.names = FALSE)

# Create LAD-LEP lookup table
C_LADLEP2020 <- distinct(I_LEP2020, LAD21CD, LAD21NM, LEP = LEP21NM1) %>%
  bind_rows(I_missingLAD %>% filter(LAD21CD != "z") %>% select(LAD21CD, LEP = `LEP21.(manually.mapped)`)) %>%
  bind_rows(distinct(I_LEP2020 %>% filter(is.na(LEP21NM2) == FALSE), LAD21CD, LAD21NM, LEP = LEP21NM2))

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
write.csv(D_EmpOcc_APS1721, file = "Data\\AppData\\D_EmpOcc_APS1721.csv", row.names = FALSE)
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
  mutate(across(where(is.numeric), ~ round(prop.table(.), 4))) %>%
  rownames_to_column("Occupation")

write.csv(C_EmpOcc_APS1721, file = "Data\\AppData\\C_EmpOcc_APS1721.csv", row.names = FALSE)

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

write.csv(D_EmpRate_APS1721, file = "Data\\AppData\\D_EmpRate_APS1721.csv", row.names = FALSE)

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
write.csv(C_EmpRate_APS1721, file = "Data\\AppData\\C_EmpRate_APS1721.csv", row.names = FALSE)

# create max and min emp count and rate by LEP for use in setting axis
C_EmpRate_APS1721_max_min <- C_EmpRate_APS1721 %>%
  group_by(area) %>%
  summarise(minEmp = min(Employment), maxEmp = max(Employment))
write.csv(C_EmpRate_APS1721_max_min, file = "Data\\AppData\\C_EmpRate_APS1721_max_min.csv", row.names = FALSE)

# FE data cleaning
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
write.csv(D_Achieve_ILR1621, file = "Data\\AppData\\D_Achieve_ILR1621.csv", row.names = FALSE)

# create version to use in dashboard
C_Achieve_ILR1621 <- F_Achieve_ILR1621 %>%
  mutate_at(vars(achievements), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = ""))) %>% # convert to blank to avoid error msg
  mutate(achievements = as.numeric(achievements)) %>%
  mutate(Year = as.numeric(substr(time_period, 3, 4))) %>% # add year name for charts
  filter(time_period != "202122") %>% # ignore temporary data in the latest year
  group_by(Year, time_period, LEP, level_or_type) %>%
  summarise(Ach = sum(achievements, na.rm = T), .groups = "drop") %>%
  mutate(level_or_typeNeat = case_when(
    level_or_type == "Further education and skills: Total" ~ "Total FE and skills provision",
    level_or_type == "Education and training: Total" ~ "Education and training (adults only)",
    level_or_type == "Community learning: Total" ~ "Community learning (adults only)",
    level_or_type == "Apprenticeships: Total" ~ "Apprenticeships (all ages)",
    TRUE ~ level_or_type
  )) %>%
  mutate(AY = paste(substr(time_period, 3, 4), "/", substr(time_period, 5, 6), sep = ""))
write.csv(C_Achieve_ILR1621, file = "Data\\AppData\\C_Achieve_ILR1621.csv", row.names = FALSE)

# create max and min vacancy pc by LEP for use in setting axis
C_Achieve_ILR1621_max_min <- C_Achieve_ILR1621 %>%
  group_by(LEP, level_or_type) %>%
  summarise(minAch = min(Ach), maxAch = max(Ach), .groups = "drop")
write.csv(C_Achieve_ILR1621_max_min, file = "Data\\AppData\\C_Achieve_ILR1621_max_min.csv", row.names = FALSE)

## format achievements by SSA
F_Achieve_ILR21 <- format.AchieveSSA.ILR(I_Achieve_ILR21)
# create downloadable version with new suppression rules
D_Achieve_ILR21 <- F_Achieve_ILR21 %>%
  mutate_at(vars(achievements), function(x) str_replace_all(x, c("!" = "c", "\\*" = "u", "~" = "low", "-" = "x")))
write.csv(D_Achieve_ILR21, file = "Data\\AppData\\D_Achieve_ILR21.csv", row.names = FALSE)

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
write.csv(C_Achieve_ILR21, file = "Data\\AppData\\C_Achieve_ILR21.csv", row.names = FALSE)

## Vacancy data
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

# vacancy (for use in downloads)
C_Vacancy_ONS1722 <- format.Vacancy.ONS(I_Vacancy_ONS1722)
write.csv(C_Vacancy_ONS1722, file = "Data\\AppData\\C_Vacancy_ONS1722.csv", row.names = FALSE)

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
write.csv(C_Vacancy_England, file = "Data\\AppData\\C_Vacancy_England.csv", row.names = FALSE)

# create max and min vacancy pc by LEP for use in setting axis
C_Vacancy_England_max_min <- C_Vacancy_England %>%
  group_by(LEP) %>%
  summarise(minVac = min(jobpc), maxVac = max(jobpc))
write.csv(C_Vacancy_England_max_min, file = "Data\\AppData\\C_Vacancy_England_max_min.csv", row.names = FALSE)

# create change vacancy pc by LEP
C_Vacancy_England_change <- C_Vacancy_England %>%
  filter(year == "2022" | year == "2021") %>%
  mutate(Row = 1:n()) %>%
  mutate(Percentage_Change = (jobcnt / lag(jobcnt)) - 1) %>%
  filter(year == "2022") %>%
  select(LEP, Percentage_Change)
write.csv(C_Vacancy_England_change, file = "Data\\AppData\\C_Vacancy_England_change.csv", row.names = FALSE)
