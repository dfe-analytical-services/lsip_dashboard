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

# list leps and LSIPs for dropdowns
C_LEP2020 <- I_LEP2020 %>%
  distinct(Area = LEP21NM1) %>%
  arrange(Area) %>%
  mutate(geographic_level = "LEP") %>%
  bind_rows(
    I_LEP2020 %>%
      distinct(Area = LSIP) %>%
      arrange(Area) %>%
      mutate(geographic_level = "LSIP")
  )
write.csv(C_LEP2020, file = "Data\\AppData\\C_LEP2020.csv", row.names = FALSE)

# Create LAD-LEP lookup table
C_LADLEP2020 <- distinct(I_LEP2020, LAD21CD, LAD21NM, LEP = LEP21NM1) %>%
  bind_rows(I_missingLAD %>% filter(LAD21CD != "z") %>% select(LAD21CD, LEP = `LEP21.(manually.mapped)`)) %>%
  bind_rows(distinct(I_LEP2020 %>% filter(LEP21NM2 != 0), LAD21CD, LAD21NM, LEP = LEP21NM2))

# Create LAD-LSIP lookup table
C_LADLSIP2020 <- distinct(I_LEP2020, LAD21CD, LAD21NM, LSIP) %>%
  bind_rows(I_missingLAD %>% filter(LAD21CD != "z") %>% select(LAD21CD, LAD21NM = area, LSIP = `LSIP21.(manually.mapped)`))

# Data cleaning functions ----
## Employment by occupation ----
format.EmpOcc.APS <- function(x) {
  reformat <- x %>%
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
    rename_with(~ gsub("[[:digit:]]+", "", .)) %>% # remove numbers from occupations since they don't match the ONS ones
    mutate(geographic_level = toupper(geographic_level))

  # create lsip file
  addlsip <- reformat %>%
    filter(geographic_level == "LADU") %>%
    left_join(select(C_LADLSIP2020, -LAD21CD), by = c("area" = "LAD21NM")) %>%
    filter(is.na(LSIP) == FALSE) %>% # remove non-english
    select(-area) %>% # get rid of ladu area
    mutate(geographic_level = "LSIP") %>% # rename as lsip
    rename(area = LSIP) %>%
    relocate(area, .before = geographic_level) %>%
    mutate_at(vars(-year, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = ""))) %>% # convert to blank to avoid error msg
    mutate_at(c(4:28), as.numeric) %>% # Convert to numeric
    group_by(year, area, geographic_level) %>% # sum for each LSIP
    summarise(across(everything(), list(sum), na.rm = T)) %>%
    rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
    mutate_at(c(4:28), as.character) # Convert to sring to bind

  # join together
  bind_rows(reformat, addlsip)
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
    geographic_level != "LADU" &
      geographic_level != "GOR" # cleans up for London and South East which is included as lep and gor
  )
write.csv(C_EmpOcc_APS1721, file = "Data\\AppData\\C_EmpOcc_APS1721.csv", row.names = FALSE)

## Employment level and rate ----
format.EmpRate.APS <- function(x) {
  reformat <- x %>%
    mutate(year = ifelse(annual.population.survey == "date", substr(X2, nchar(X2) - 4 + 1, nchar(X2)), NA)) %>% # tag time periods
    fill(year) %>% # fill time periods for all rows
    row_to_names(row_number = 4) %>% # set col names
    clean_names() %>%
    select(-starts_with("na")) %>% # remove na columns (flags and confidence)
    mutate(check = ifelse(grepl(":", area), 1, 0)) %>% # remove anything but LEP and Country
    filter(check == 1) %>%
    filter(!grepl("nomisweb", area)) %>%
    mutate(geographic_level = gsub(":.*", "", area)) %>% # Get geog type
    mutate(geographic_level = toupper(geographic_level)) %>%
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
  # create lsip file
  addlsip <- reformat %>%
    filter(geographic_level == "LADU") %>%
    left_join(select(C_LADLSIP2020, -LAD21CD), by = c("area" = "LAD21NM")) %>%
    filter(is.na(LSIP) == FALSE) %>% # remove non-english
    select(-area) %>% # get rid of ladu area
    mutate(geographic_level = "LSIP") %>% # rename as lsip
    rename(area = LSIP) %>%
    relocate(area, .before = geographic_level) %>%
    mutate_at(vars(-year, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = ""))) %>% # convert to blank to avoid error msg
    mutate_at(c(4:10), as.numeric) %>% # Convert to numeric
    group_by(year, area, geographic_level) %>% # sum for each LSIP
    summarise(across(everything(), list(sum), na.rm = T)) %>%
    rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
    mutate_at(c(4:10), as.character) # Convert to sring to bind

  # join together
  bind_rows(reformat, addlsip)
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
    geographic_level != "LADU" # not needed for the dashboard currently
    & geographic_level != "GOR"
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
format.Achieve.ILR <- function(x) {
  colnames(x)[1] <- "area"

  # create lep file
  addLEP <- x %>%
    filter(geographic_level == "localAuthorityDistrict") %>%
    left_join(select(C_LADLEP2020, -LAD21NM), by = c("location_code" = "LAD21CD")) %>%
    filter(is.na(LEP) == FALSE) %>% # remove non-english
    select(-area, -location_code, -age_group) %>% # get rid of ladu area
    mutate(geographic_level = "LEP") %>% # rename as lsip
    rename(area = LEP) %>%
    relocate(area, .before = geographic_level) %>%
    mutate_at(vars(-time_period, -area, -geographic_level, -level_or_type), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = ""))) %>% # convert to blank to avoid error msg
    mutate_at(c(5:5), as.numeric) %>% # Convert to numeric
    group_by(time_period, area, geographic_level, level_or_type) %>% # sum for each LSIP
    summarise(across(everything(), list(sum), na.rm = T)) %>%
    rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
    mutate_at(c(5:5), as.character) # Convert to sring to bind
  # rename_all(recode, e_and_t_aims_ach = "achievements")

  # create lsip file
  addLSIP <- x %>%
    filter(geographic_level == "localAuthorityDistrict") %>%
    left_join(select(C_LADLSIP2020, -LAD21CD), by = c("area" = "LAD21NM")) %>%
    filter(is.na(LSIP) == FALSE) %>% # remove non-english
    select(-area, -location_code, -age_group) %>% # get rid of ladu area
    mutate(geographic_level = "LSIP") %>% # rename as lsip
    rename(area = LSIP) %>%
    relocate(area, .before = geographic_level) %>%
    mutate_at(vars(-time_period, -area, -geographic_level, -level_or_type), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = ""))) %>% # convert to blank to avoid error msg
    mutate_at(c(5:5), as.numeric) %>% # Convert to numeric
    group_by(time_period, area, geographic_level, level_or_type) %>% # sum for each LSIP
    summarise(across(everything(), list(sum), na.rm = T)) %>%
    rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
    mutate_at(c(5:5), as.character) # Convert to string to bind

  # join together
  bind_rows(x %>% select(-location_code), addLEP, addLSIP)
}

## format achievements
F_Achieve_ILR1621 <- format.Achieve.ILR(I_Achieve_ILR1621)
# create downloadable version with new suppression rules
D_Achieve_ILR1621 <- F_Achieve_ILR1621 %>%
  mutate_at(vars(achievements), function(x) str_replace_all(x, c("!" = "c", "\\*" = "u", "~" = "low", "-" = "x")))
write.csv(D_Achieve_ILR1621, file = "Data\\AppData\\D_Achieve_ILR1621.csv", row.names = FALSE)

# create version to use in dashboard
C_Achieve_ILR1621 <- F_Achieve_ILR1621 %>%
  filter(geographic_level != "localAuthorityDistrict", geographic_level != "region", geographic_level != "country") %>%
  mutate_at(vars(achievements), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = ""))) %>% # convert to blank to avoid error msg
  mutate(achievements = as.numeric(achievements)) %>%
  mutate(Year = as.numeric(substr(time_period, 3, 4))) %>% # add year name for charts
  filter(time_period != "202122") %>% # ignore temporary data in the latest year
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
  group_by(geographic_level, area, level_or_type) %>%
  summarise(minAch = min(achievements), maxAch = max(achievements), .groups = "drop")
write.csv(C_Achieve_ILR1621_max_min, file = "Data\\AppData\\C_Achieve_ILR1621_max_min.csv", row.names = FALSE)

## format achievements by SSA
format.AchieveSSA.ILR <- function(x) {
  colnames(x)[1] <- "area"

  # create lep file
  addLEP <- x %>%
    filter(geographic_level == "localAuthorityDistrict") %>%
    left_join(select(C_LADLEP2020, -LAD21NM), by = c("location_code" = "LAD21CD")) %>%
    filter(is.na(LEP) == FALSE) %>% # remove non-english
    select(-area, -location_code, -notional_nvq_level, -ethnicity_group, -gender) %>% # get rid of ladu area
    mutate(geographic_level = "LEP") %>% # rename as lsip
    rename(area = LEP) %>%
    relocate(area, .before = geographic_level) %>%
    mutate_at(vars(-time_period, -area, -geographic_level, -ssa_t1_desc), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = ""))) %>% # convert to blank to avoid error msg
    mutate_at(c(5:5), as.numeric) %>% # Convert to numeric
    group_by(time_period, area, geographic_level, ssa_t1_desc) %>% # sum for each LSIP
    summarise(across(everything(), list(sum), na.rm = T)) %>%
    rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
    mutate_at(c(5:5), as.character)  # Convert to sring to bind
    
  # create lsip file
  addLSIP <- x %>%
    filter(geographic_level == "localAuthorityDistrict") %>%
    left_join(select(C_LADLSIP2020, -LAD21CD), by = c("area" = "LAD21NM")) %>%
    filter(is.na(LSIP) == FALSE) %>% # remove non-english
    select(-area, -location_code, -notional_nvq_level, -ethnicity_group, -gender) %>% # get rid of ladu area
    mutate(geographic_level = "LSIP") %>% # rename as lsip
    rename(area = LSIP) %>%
    relocate(area, .before = geographic_level) %>%
    mutate_at(vars(-time_period, -area, -geographic_level, -ssa_t1_desc), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = ""))) %>% # convert to blank to avoid error msg
    mutate_at(c(5:5), as.numeric) %>% # Convert to numeric
    group_by(time_period, area, geographic_level, ssa_t1_desc) %>% # sum for each LSIP
    summarise(across(everything(), list(sum), na.rm = T)) %>%
    rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
    mutate_at(c(5:5), as.character)  # Convert to sring to bind

  # join together
  bind_rows(x %>% select(-location_code, -notional_nvq_level, -ethnicity_group, -gender), addLEP, addLSIP)%>%
    rename_all(recode, e_and_t_aims_ach = "achievements")
}


F_Achieve_ILR21 <- format.AchieveSSA.ILR(I_Achieve_ILR21)
# create downloadable version with new suppression rules
D_Achieve_ILR21 <- F_Achieve_ILR21 %>%
  mutate_at(vars(achievements), function(x) str_replace_all(x, c("!" = "c", "\\*" = "u", "~" = "low", "-" = "x")))
write.csv(D_Achieve_ILR21, file = "Data\\AppData\\D_Achieve_ILR21.csv", row.names = FALSE)

# create version to use in dashboard
# group by ssa and lep
SSA_LEP_Achieve_ILR21 <- F_Achieve_ILR21 %>%
  filter(geographic_level != "localAuthorityDistrict", geographic_level != "region", geographic_level != "country") %>%
  mutate_at(vars(achievements), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = ""))) %>% # convert to blank to avoid error msg
  mutate(Achievements = as.numeric(achievements)) %>%
  filter(time_period == "202122") %>%
  select(geographic_level, area, SSA = ssa_t1_desc, Achievements)
# group by ssa to get ssa totals
Ach_pc_Achieve_ILR21 <- SSA_LEP_Achieve_ILR21 %>%
  filter(SSA == "Total") %>%
  select(geographic_level, area, Total = SSA, Total_ach = Achievements)
# calculate ssa %s
C_Achieve_ILR21 <- SSA_LEP_Achieve_ILR21 %>%
  left_join(Ach_pc_Achieve_ILR21, by = c("geographic_level", "area")) %>%
  group_by(geographic_level, area) %>%
  mutate(pc = Achievements / Total_ach) %>%
  filter(SSA != "Total")
write.csv(C_Achieve_ILR21, file = "Data\\AppData\\C_Achieve_ILR21.csv", row.names = FALSE)

## Vacancy data
# Reshape vacancy data to long, rename and reorder and reformat some columns
format.Vacancy.ONS <- function(x) { # need to clean up colnames
  reformat <- x %>%
    gather(year, vacancy_unit, 3:8) %>%
    rename(LA = "Local.authority.[note.1]", region = "Region.[note.2]") %>%
    relocate(year, .before = LA) %>%
    mutate(year = as.numeric(year))

  # create LA file
  addLA <- reformat %>%
    select(-region) %>%
    mutate(geographic_level = "LADU") %>% # rename as lsip
    rename(area = LA) %>%
    relocate(vacancy_unit, .after = geographic_level) %>%
    mutate_at(vars(-year, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = ""))) %>% # convert to blank to avoid error msg
    mutate_at(c(4:4), as.numeric) %>% # Convert to numeric
    group_by(year, area, geographic_level) %>% # sum for each LSIP
    summarise(across(everything(), list(sum), na.rm = T)) %>%
    rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
    mutate_at(c(4:4), as.character) # Convert to sring to bind

  # create region file
  addRegion <- reformat %>%
    select(-LA) %>% # get rid of ladu
    mutate(geographic_level = "GOR") %>% # rename as lsip
    rename(area = region) %>%
    relocate(vacancy_unit, .after = geographic_level) %>%
    mutate_at(vars(-year, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = ""))) %>% # convert to blank to avoid error msg
    mutate_at(c(4:4), as.numeric) %>% # Convert to numeric
    group_by(year, area, geographic_level) %>% # sum for each LSIP
    summarise(across(everything(), list(sum), na.rm = T)) %>%
    rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
    mutate_at(c(4:4), as.character) # Convert to sring to bind

  # create lep file
  addLEP <- reformat %>%
    left_join(select(C_LADLEP2020, -LAD21CD), by = c("LA" = "LAD21NM")) %>%
    filter(is.na(LEP) == FALSE) %>% # remove non-english
    select(-LA, -region) %>% # get rid of ladu area and region
    mutate(geographic_level = "LEP") %>% # rename as lsip
    rename(area = LEP) %>%
    relocate(vacancy_unit, .after = geographic_level) %>%
    mutate_at(vars(-year, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = ""))) %>% # convert to blank to avoid error msg
    mutate_at(c(4:4), as.numeric) %>% # Convert to numeric
    group_by(year, area, geographic_level) %>% # sum for each LSIP
    summarise(across(everything(), list(sum), na.rm = T)) %>%
    rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
    mutate_at(c(4:4), as.character) # Convert to sring to bind


  # create lsip file
  addLSIP <- reformat %>%
    left_join(select(C_LADLSIP2020, -LAD21CD), by = c("LA" = "LAD21NM")) %>%
    filter(is.na(LSIP) == FALSE) %>% # remove non-english
    select(-LA, -region) %>% # get rid of ladu area and region
    mutate(geographic_level = "LSIP") %>% # rename as lsip
    rename(area = LSIP) %>%
    relocate(vacancy_unit, .after = geographic_level) %>%
    mutate_at(vars(-year, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = ""))) %>% # convert to blank to avoid error msg
    mutate_at(c(4:4), as.numeric) %>% # Convert to numeric
    group_by(year, area, geographic_level) %>% # sum for each LSIP
    summarise(across(everything(), list(sum), na.rm = T)) %>%
    rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
    mutate_at(c(4:4), as.character) # Convert to sring to bind

  # join together
  bind_rows(addLA, addRegion, addLSIP, addLEP)
}

# vacancy (for use in downloads)
C_Vacancy_ONS1722 <- format.Vacancy.ONS(I_Vacancy_ONS1722)
write.csv(C_Vacancy_ONS1722, file = "Data\\AppData\\C_Vacancy_ONS1722.csv", row.names = FALSE)

# vacancy data to use in dashboard
C_Vacancy_England <-
  # work with original file to utilise the relationship between LA and region (to get to only England)
  I_Vacancy_ONS1722 %>%
  gather(year, vacancy_unit, 3:8) %>%
  rename(LA = "Local.authority.[note.1]", region = "Region.[note.2]") %>%
  relocate(year, .before = LA) %>%
  mutate(year = as.numeric(year), vacancy_unit = as.numeric(vacancy_unit)) %>%
  filter(!region %in% c("Wales", "Scotland", "Northern Ireland")) %>%
  group_by(year) %>%
  summarise(England = sum(vacancy_unit)) %>%
  right_join(C_Vacancy_ONS1722 %>%
    filter(
      geographic_level != "LADU" # not needed for the dashboard currently
      & geographic_level != "GOR"
    ) %>%
    mutate_at(c(4:4), as.numeric), by = "year") %>%
  mutate(pc_total = vacancy_unit / England) %>%
  mutate(Year = as.numeric(substr(year, 3, 4))) %>%
  group_by(area, geographic_level, year, Year) %>%
  summarise(jobpc = sum(pc_total), jobcnt = sum(vacancy_unit), .groups = "drop")
write.csv(C_Vacancy_England, file = "Data\\AppData\\C_Vacancy_England.csv", row.names = FALSE)

# create max and min vacancy pc by LEP for use in setting axis
C_Vacancy_England_max_min <- C_Vacancy_England %>%
  group_by(geographic_level, area) %>%
  summarise(minVac = min(jobpc), maxVac = max(jobpc))
write.csv(C_Vacancy_England_max_min, file = "Data\\AppData\\C_Vacancy_England_max_min.csv", row.names = FALSE)

# create change vacancy pc by LEP
C_Vacancy_England_change <- C_Vacancy_England %>%
  filter(year == "2022" | year == "2021") %>%
  mutate(Row = 1:n()) %>%
  mutate(Percentage_Change = (jobcnt / lag(jobcnt)) - 1) %>%
  filter(year == "2022") %>%
  select(geographic_level, area, Percentage_Change)
write.csv(C_Vacancy_England_change, file = "Data\\AppData\\C_Vacancy_England_change.csv", row.names = FALSE)

## APS - employment by industry data
# Reshape data to long, rename and reorder and reformat some columns

format.empind.APS <- function(x) {
  reformat <- x %>%
    mutate(year = ifelse(annual.population.survey == "date", X2, NA)) %>% # tag time periods
    fill(year) %>% # fill time periods for all rows
    row_to_names(row_number = 4) %>% # set col names
    clean_names() %>%
    select(-starts_with("na")) %>% # remove na columns (flags and confidence)
    mutate(check = ifelse(grepl(":", area), 1, 0)) %>% # remove anything but LEP and Country
    filter(check == 1) %>%
    filter(!grepl("nomisweb", area)) %>%
    select(year = jul_2017_jun_2018, area, everything(), -check) %>% # reorder and remove
    mutate(geographic_level = gsub(":.*", "", area)) %>% # Get geog type
    mutate(area = gsub(".*:", "", area)) %>% # Tidy up Area names
    mutate(area = case_when(
      area == "Hull and East Riding" ~ "Hull and East Yorkshire",
      area == "Buckinghamshire Thames Valley" ~ "Buckinghamshire",
      TRUE ~ area
    )) %>% # Rename so matches official name
    relocate(geographic_level, year, .before = area) %>%
    mutate(year = as.numeric(substr(year, 5, 8))) %>%
    rename_with(
      .fn = ~ str_replace_all(.x, c("t13a_" = "", "all_people" = "", "(sic 2007)" = "", "_" = " ", "sic" = "")),
      .cols = starts_with("t13a_")
    ) %>%
    rename_with(~ gsub("[[:digit:]]+", "", .)) %>% # remove numbers from occupations since they don't match the ONS ones
    mutate(geographic_level = toupper(geographic_level))
  
  # create lsip file
  addlsip <- reformat %>%
    filter(geographic_level == "LADU") %>%
    left_join(select(C_LADLSIP2020, -LAD21CD), by = c("area" = "LAD21NM")) %>%
    filter(is.na(LSIP) == FALSE) %>% # remove non-english
    select(-area) %>% # get rid of ladu area
    mutate(geographic_level = "LSIP") %>% # rename as lsip
    rename(area = LSIP) %>%
    relocate(area, .before = geographic_level) %>%
    mutate_at(vars(-year, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = ""))) %>% # convert to blank to avoid error msg
    mutate_at(c(4:12), as.numeric) %>% # Convert to numeric
    group_by(year, area, geographic_level) %>% # sum for each LSIP
    summarise(across(everything(), list(sum), na.rm = T)) %>%
    mutate_at(vars(-year, -area, -geographic_level), ~ replace(., which(.==0), "!")) %>%
    rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
    mutate_at(c(4:12), as.character) # Convert to sring to bind
  
  # join together and rename columns
  LEP_LSIP <- bind_rows(reformat, addlsip) %>%
    rename("Agriculture and Fishing" = " a agricuture fishing   ",
           "Energy and Water" = " b d e energy water   ",
           "Manufacturing" = " c manufacturing   ",
           "Construction" = " f construction   ",
           "Distribution, Hotel and Restaurants" = " g i distribution hotels restaurants   ",
           "Transport and Communication" = " h j transport communication   ",
           "Banking, Finance and Insurance" = " k n banking finance insurance etc   ",
           "Public Administration, Education and Health" = " o q public admin education health   ",
           "Other Services" = " r u other services   ") %>%
    filter(geographic_level %in%  c("LSIP", "LEP", "LADU", "COUNTRY")) %>%
    relocate(area, .before = geographic_level) %>%
    relocate(year, .before = area)
  
  
}

## format employment by industry
F_empind_APS1722 <- format.empind.APS(I_EmpInd_APS1721)



#mutate(across(c(-year, -area, -geographic_level), ~replace(., . == 0, !))) %>%
mutate_at(vars(-year, -area, -geographic_level), ~ replace(., which(.==0), !)) %>%
#Downloadable version
D_empind_APS1721 <- F_empind_APS1722 %>%
  mutate_at(vars(-year, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "c", "\\*" = "u", "~" = "low", "-" = "x")))
#Write the data to folder

write.csv(D_empind_APS1721, file = "Data\\AppData\\D_empind_APS1721.csv", row.names = FALSE)

## APS - Skill level of the working age population (% 16-64 year olds qualified to Level 2, level 3, level 4+)
# Reshape data to long, rename and reorder and reformat some columns

format.skills.APS <- function(x) {
  reformat <- x %>%
    mutate(year = ifelse(annual.population.survey == "date", X2, NA)) %>% # tag time periods
    fill(year)%>% # fill time periods for all rows
    row_to_names(row_number = 4)%>% # set col names
    clean_names()%>%
    select(-starts_with("na"))%>% # remove na columns (flags and confidence)
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
    relocate(geographic_level, year, .after = area) %>%
    mutate(year = as.numeric(substr(year, 5, 8))) %>%
    rename_with(
      .fn = ~ str_replace_all(.x, c("t19_" = "", "_" = " ", "all people" = "")),
      .cols = starts_with("t19_")
    ) %>%
    # rename_with(~ gsub("[[:digit:]]+", "", .)) %>% # remove numbers from occupations since they don't match the ONS ones
    mutate(geographic_level = toupper(geographic_level))
  
  
  # create lsip file
  addlsip <- reformat %>%
    filter(geographic_level == "LADU") %>%
    left_join(select(C_LADLSIP2020, -LAD21CD), by = c("area" = "LAD21NM")) %>%
    filter(is.na(LSIP) == FALSE) %>% # remove non-english
    select(-area) %>% # get rid of ladu area
    mutate(geographic_level = "LSIP") %>% # rename as lsip
    rename(area = LSIP) %>%
    relocate(area, .before = geographic_level) %>%
    mutate_at(vars(-year, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = ""))) %>% # convert to blank to avoid error msg
    mutate_at(c(4:10), as.numeric) %>% # Convert to numeric
    group_by(year, area, geographic_level) %>% # sum for each LSIP
    summarise(across(everything(), list(sum), na.rm = T)) %>%
    rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
    mutate_at(c(4:10), as.character) # Convert to sring to bind
  
  # join together and rename columns
  LEP_LSIP <- bind_rows(reformat, addlsip)%>%
    rename("16-64 None" = "1 total  aged 16 64 none",
           "16-64 NVQ1" = "2 total  aged 16 64 nvq1",
           "16-64 NVQ2" = "3 total  aged 16 64 nvq2",
           "16-64 trade apprenticeships"  = "4 total  aged 16 64 trade apprenticeships",
           "16-64 NVQ3"= "5 total  aged 16 64 nvq3",
           "16-64 NVQ4" = "6 total  aged 16 64 nvq4",
           "16-64 Other Qualifications" = "7 total  aged 16 64 other qualifications") %>%
    filter(geographic_level %in%  c("LSIP", "LEP","LADU", "COUNTRY")) %>%
    relocate(area, .before = geographic_level) %>%
    relocate(year, .before = area)
  
}

## format skills
F_skillsnvq_APS1721 <- format.skills.APS(I_Skillsnvq_APS1721)


#Downloadable version
D_skillsnvq_APS1721 <- F_skillsnvq_APS1721 %>%
  mutate_at(vars(-year, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "c", "\\*" = "u", "~" = "low", "-" = "x")))
#write the data to folder
write.csv(D_skillsnvq_APS1721, file = "Data\\AppData\\D_skillsnvq_APS1721.csv", row.names = FALSE)


## UK Business Count - Enterprise by employment size
# Reshape data to long, rename and reorder and reformat some columns

format.empent.UBC <- function(x) {
  reformat <- x %>%
    rename(business_counts = "UK.Business.Counts.-.enterprises.by.industry.and.employment.size.band") %>%
    mutate(year = ifelse(business_counts == "date", X2, NA)) %>% # tag time periods
    fill(year) %>%
    row_to_names(row_number = 5) %>% # set col names
    clean_names() %>%
    rename(year = "x2018") %>%
    mutate(check = ifelse(grepl(":", area), 1, 0)) %>% # remove anything but LEP and Country
    filter(check == 1) %>%
    filter(!grepl("nomisweb", area)) %>%
    select(area, everything(), -check) %>% # reorder and remove
    mutate(geographic_level = gsub(":.*", "", area)) %>% # Get geog type
    mutate(area = gsub(".*:", "", area)) %>% # Tidy up Area names
    mutate(area = case_when(
      area == "Hull and East Riding" ~ "Hull and East Yorkshire",
      area == "Buckinghamshire Thames Valley" ~ "Buckinghamshire",
      TRUE ~ area
    )) %>%
    relocate(geographic_level, year, .after = area) %>%
    mutate(geographic_level = toupper(geographic_level))
  
  # create lsip file
  addlsip <- reformat %>%
    filter(geographic_level == "LADU") %>%
    left_join(select(C_LADLSIP2020, -LAD21CD), by = c("area" = "LAD21NM")) %>%
    filter(is.na(LSIP) == FALSE) %>% # remove non-english
    select(-area) %>% # get rid of ladu area
    mutate(geographic_level = "LSIP") %>% # rename as lsip
    rename(area = LSIP) %>%
    relocate(area, .before = geographic_level) %>%
    mutate_at(vars(-year, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = ""))) %>% # convert to blank to avoid error msg
    mutate_at(c(4:8), as.numeric) %>% # Convert to numeric
    group_by(year, area, geographic_level) %>% # sum for each LSIP
    summarise(across(everything(), list(sum), na.rm = T)) %>%
    rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
    rename(small_10_to_49 = "small0_to_49") %>%
    mutate_at(c(4:8), as.character) # Convert to sring to bind
  
  # join together and rename columns
  LEP_LSIP <- bind_rows(reformat, addlsip) %>%
    rename("Total Enterprises" = "total",
           "Total Enterprises Micro 0-9" = "micro_0_to_9",
           "Total Enterprises Small 10-49" = "small_10_to_49",
           "Total Enterprises Medium 50-249" = "medium_sized_50_to_249",
           "Total Enterprises Large 250" = "large_250") %>%
    filter(geographic_level %in%  c("LSIP", "LEP","LADU", "COUNTRY")) %>%
    relocate(area, .before = geographic_level) %>%
    relocate(year, .before = area)
}

## format UBC
F_empent_UBC1822 <- format.empent.UBC(I_EmpEnt_APS1721)

#Downloadable version
D_empent_UBC1822 <- F_empent_UBC1822 %>%
  mutate_at(vars(-year, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "c", "\\*" = "u", "~" = "low", "-" = "x")))
#write the data to folder
write.csv(D_empent_UBC1822, file = "Data\\AppData\\D_empent_UBC1822.csv", row.names = FALSE)


## National Pupil Database - Key Stage 4 Destinations
# Reshape data to long, rename and reorder and reformat some columns

format.ks4 <- function(x) {
  colnames(x)[1] <- "area"
  
  addladu <- x %>%
    select(-location_code, -characteristic, -data_type, -institution_group, -level_methodology) %>%
    mutate(geographic_level = replace(geographic_level, geographic_level == "localAuthorityDistrict", "LADU")) %>%
    mutate_at(c(4:9), as.character) # Convert to string to bind

  
  # create lep file
  addLEP <- x %>%
    left_join(select(C_LADLEP2020, -LAD21NM), by = c("location_code" = "LAD21CD")) %>%
    filter(is.na(LEP) == FALSE) %>% # remove non-english
    select(-area, -location_code) %>% # get rid of ladu area
    mutate(geographic_level = "LEP") %>% # rename as lsip
    rename(area = LEP) %>%
    relocate(area, .before = geographic_level) %>%
    mutate_at(vars(-time_period, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = ""))) %>% # convert to blank to avoid error msg
    mutate_at(c(8:13), as.numeric) %>% # Convert to numeric
    select(-data_type, -institution_group, -level_methodology, -characteristic) %>%
    group_by(area, geographic_level, time_period) %>% # sum for each LSIP
    summarise(across(everything(), list(sum), na.rm = T)) %>%
    rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
    mutate_at(c(4:9), as.character) # Convert to sring to bind
  
  # create lsip file
  addLSIP <- x %>%
    left_join(select(C_LADLSIP2020, -LAD21CD), by = c("area" = "LAD21NM")) %>%
    filter(is.na(LSIP) == FALSE) %>% # remove non-english
    select(-area, -location_code) %>% # get rid of ladu area
    mutate(geographic_level = "LSIP") %>% # rename as lsip
    rename(area = LSIP) %>%
    relocate(area, .before = geographic_level) %>%
    mutate_at(vars(-time_period, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = ""))) %>% # convert to blank to avoid error msg
    mutate_at(c(8:13), as.numeric) %>% # Convert to numeric
    select(-data_type, -institution_group, -level_methodology, -characteristic) %>%
    group_by(area, geographic_level, time_period) %>% # sum for each LSIP
    summarise(across(everything(), list(sum), na.rm = T)) %>%
    rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
    mutate_at(c(4:9), as.character) # Convert to string to bind
  
  # join together and rename columns
  LEP_LSIP <- bind_rows(addladu, addLEP, addLSIP) %>%
    rename("Total Completed Key Stage 4" = "cohort",
           "Unknown" = "all_unknown",
           "Not Recorded as a Sustained Destination" = "all_notsust",
           "Sustained Education" = "education",
           "Sustained Employment" = "all_work",
           "Sustained Apprenticeships" = "appren") %>%
    relocate('Total Completed Key Stage 4', .before = 'Unknown') %>%
    relocate(area, .before = geographic_level) %>%
    relocate(time_period, .before = area)
  
}    

## format KS4
F_KS4destin_1520<- format.ks4(I_KS4destin_1520)  

#Downloadable data
D_KS4destin_1520 <- F_KS4destin_1520 %>%
  mutate_at(vars(-time_period, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "c", "\\*" = "u", "~" = "low", "-" = "x")))
#write data to folder
write.csv(D_KS4destin_1520, file = "Data\\AppData\\D_KS4destin_1520.csv", row.names = FALSE)



## National Pupil Database  - Key Stage 5 Destinations
# Reshape data to long, rename and reorder and reformat some columns

format.ks5 <- function(x) {
  colnames(x)[1] <- "area"
  
  
  addladu <- x %>%
    select(-location_code, -characteristic, -data_type, -institution_group, -level_methodology) %>%
    mutate(geographic_level = replace(geographic_level, geographic_level == "localAuthority", "LADU")) %>%
    mutate_at(c(5:10), as.character) # Convert to string to bind
  
  
  # create lep file
  addLEP <- x %>%
    left_join(select(C_LADLEP2020, -LAD21NM), by = c("location_code" = "LAD21CD")) %>%
    filter(is.na(LEP) == FALSE) %>% # remove non-english
    select(-area, -location_code) %>% # get rid of ladu area
    mutate(geographic_level = "LEP") %>% # rename as lsip
    rename(area = LEP) %>%
    relocate(area, .before = geographic_level) %>%
    mutate_at(vars(-time_period, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = ""))) %>% # convert to blank to avoid error msg
    mutate_at(c(9:14), as.numeric) %>% # Convert to numeric
    select(-data_type, -institution_group, -level_methodology, -characteristic) %>%
    group_by(area, geographic_level, time_period, cohort_level_group) %>% # sum for each LSIP
    summarise(across(everything(), list(sum), na.rm = T)) %>%
    rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
    mutate_at(c(5:10), as.character) # Convert to sring to bind
  
  # create lsip file
  addLSIP <- x %>%
    left_join(select(C_LADLSIP2020, -LAD21CD), by = c("area" = "LAD21NM")) %>%
    filter(is.na(LSIP) == FALSE) %>% # remove non-english
    select(-area, -location_code) %>% # get rid of ladu area
    mutate(geographic_level = "LSIP") %>% # rename as lsip
    rename(area = LSIP) %>%
    relocate(area, .before = geographic_level) %>%
    mutate_at(vars(-time_period, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = ""))) %>% # convert to blank to avoid error msg
    mutate_at(c(9:14), as.numeric) %>% # Convert to numeric
    select(-data_type, -institution_group, -level_methodology, -characteristic) %>%
    group_by(area, geographic_level, time_period, cohort_level_group) %>% # sum for each LSIP
    summarise(across(everything(), list(sum), na.rm = T)) %>%
    rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
    mutate_at(c(5:10), as.character) # Convert to string to bind
  
  # join together and rename columns
  LEP_LSIP <- bind_rows(addladu, addLEP, addLSIP) %>%
    rename("Total Completed Key Stage 4" = "cohort",
           "Unknown" = "all_unknown",
           "Not Recorded as a Sustained Destination" = "all_notsust",
           "Sustained Education" = "education",
           "Sustained Employment" = "all_work",
           "Sustained Apprenticeships" = "appren",
           "Cohort Group" = "cohort_level_group") %>%
    relocate('Total Completed Key Stage 4',  .before = "Cohort Group") %>%
    relocate(area, .before = geographic_level) %>%
    relocate(time_period, .before = area)
  
} 

## format KS5
F_KS5destin_1720 <- format.ks5(I_KS5destin_1720)

#downloadable version
D_KS5destin_1720 <- F_KS5destin_1720 %>%
  mutate_at(vars(-time_period, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "c", "\\*" = "u", "~" = "low", "-" = "x")))
#write data to folder - todo
write.csv(D_KS5destin_1720, file = "Data\\AppData\\D_KS5destin_1720.csv", row.names = FALSE)



# Tidy up data table
names(I_DataTable) <- gsub(".", " ", names(I_DataTable), fixed = TRUE)
write.csv(I_DataTable, file = "Data\\AppData\\I_DataTable.csv", row.names = FALSE)

