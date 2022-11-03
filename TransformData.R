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
    mutate_at(c(4:29), as.numeric) %>% # Convert to numeric
    group_by(year, area, geographic_level) %>% # sum for each LSIP
    summarise(across(everything(), list(sum), na.rm = T)) %>%
    rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
    pivot_longer(-c(year, area, geographic_level), names_to = "occupation", values_to = "frequency") %>%
    group_by(year, area, geographic_level) %>%
    slice_max(order_by = frequency, n = 6) %>% # find top 5
    pivot_wider(names_from = occupation, values_from = frequency, values_fill = 9999999) %>% # unpivot
    mutate_at(c(4:19), as.character) # Convert to sring to bind

  # join together
  bind <- bind_rows(reformat, addlsip) %>%
    rename(allOccs = t__all_aged__over_in_employment_all_people) # get total for LSIP
  bind[is.na(bind)] <- "9999999" # reaplce nas with error code
  bind
}
# format data
F_EmpOcc_APS1721 <- format.EmpOcc.APS(I_EmpOcc_APS1721)
# create downloadable version with new suppression rules
D_EmpOcc_APS1721 <- F_EmpOcc_APS1721 %>%
  select(-allOccs) %>%
  mutate_at(vars(-year, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "c", "\\*" = "u", "~" = "low", "-" = "x", "9999999" = "x")))
write.csv(D_EmpOcc_APS1721, file = "Data\\AppData\\D_EmpOcc_APS1721.csv", row.names = FALSE)
# create version to use in dashboard
C_EmpOcc_APS1721 <- F_EmpOcc_APS1721 %>%
  filter(
    year == "2021",
    geographic_level != "LADU" &
      geographic_level != "GOR" # cleans up for London and South East which is included as lep and gor
  ) %>%
  mutate_at(vars(-year, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = "", "9999999" = ""))) %>% # convert to blank to avoid error msg
  mutate_at(c(4:29), as.numeric) %>% # Convert to numeric
  mutate(allOccsRemain = allOccs -
    rowSums(select(., -allOccs, -year, -area, -geographic_level), na.rm = TRUE)) %>%
  mutate(allOccsRemain = case_when(allOccsRemain <= 0 ~ 0, TRUE ~ allOccsRemain)) %>%
  select(-allOccs)

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
    select(year = x2018, area, everything(), -check) %>% # reorder and remove
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
F_EmpRate_APS1822 <- format.EmpRate.APS(I_EmpRate_APS1822)
# create downloadable version with new suppression rules
D_EmpRate_APS1822 <- F_EmpRate_APS1822 %>%
  mutate_at(vars(-year, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "c", "\\*" = "u", "~" = "low", "-" = "x")))

write.csv(D_EmpRate_APS1822, file = "Data\\AppData\\D_EmpRate_APS1822.csv", row.names = FALSE)

# create version to use in dashboard
C_EmpRate_APS1822 <- F_EmpRate_APS1822 %>%
  mutate_at(vars(-year, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = ""))) %>% # convert to blank to avoid error msg
  mutate_at(c(4:10), as.numeric) %>% # Convert to numeric
  mutate(empRate = .[[6]] / .[[4]]) %>%
  filter(
    geographic_level != "LADU" # not needed for the dashboard currently
    & geographic_level != "GOR"
  ) %>%
  mutate(Year = as.numeric(substr(year, 3, 4))) %>% # for use in charts
  rename(Employment = `28  in employment `) # for use in charts
write.csv(C_EmpRate_APS1822, file = "Data\\AppData\\C_EmpRate_APS1822.csv", row.names = FALSE)

# create max and min emp count and rate by LEP for use in setting axis
C_EmpRate_APS1822_max_min <- C_EmpRate_APS1822 %>%
  group_by(area) %>%
  summarise(minEmp = min(Employment), maxEmp = max(Employment))
write.csv(C_EmpRate_APS1822_max_min, file = "Data\\AppData\\C_EmpRate_APS1822_max_min.csv", row.names = FALSE)

# FE data cleaning
# Clean ILR column names, reorder and reformat
format.Achieve.ILR <- function(x) {
  addLA <- x %>%
    filter(geographic_level == "Local authority district") %>%
    select(
      -time_identifier, -country_code, -country_name, -region_code, -region_name, -old_la_code,
      -lad_code, -pcon_code, -pcon_name, -new_la_code, -la_name
    ) %>% # get rid of ladu area
    rename(area = lad_name)

  popLA <- addLA %>%
    filter(time_period == 202021) %>%
    filter(level_or_type == "Further education and skills: Total" | (level_or_type == "Apprenticeships: Total" & (age_group == "Under 19" | age_group == "Total"))) %>%
    mutate_at(vars(starts, participation, achievements, starts_rate_per_100000_population, participation_rate_per_100000_population, achievements_rate_per_100000_population), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = "", "z" = "", "low" = ""))) %>% # convert to blank to avoid error msg
    mutate_at(vars(starts, participation, achievements, starts_rate_per_100000_population, participation_rate_per_100000_population, achievements_rate_per_100000_population), as.numeric) %>% # Convert to numeric
    mutate(pop = 100000 * participation / participation_rate_per_100000_population) %>%
    mutate(popGroup = case_when(
      apprenticeships_or_further_education == "Apprenticeships" ~ paste(apprenticeships_or_further_education, age_group),
      TRUE ~ age_group
    )) %>%
    select(area, popGroup, pop)

  addCountry <- x %>%
    filter(geographic_level == "National") %>%
    select(
      -time_identifier, -country_code, -region_code, -region_name, -old_la_code,
      -lad_code, -pcon_code, -pcon_name, -new_la_code, -la_name, -lad_name
    ) %>% # get rid of ladu area
    rename(area = country_name)

  # create lep file
  addLEP <- x %>%
    filter(geographic_level == "Local authority district") %>%
    # add population
    mutate(popGroup = case_when(
      apprenticeships_or_further_education == "Apprenticeships" & (age_group == "Total" | age_group == "Under 19") ~ paste(apprenticeships_or_further_education, age_group),
      TRUE ~ age_group
    )) %>%
    left_join(popLA, by = c("lad_name" = "area", "popGroup" = "popGroup")) %>%
    # addLEPS
    left_join(select(C_LADLEP2020, -LAD21NM), by = c("lad_code" = "LAD21CD")) %>%
    filter(is.na(LEP) == FALSE) %>% # remove non-english
    select(
      -time_identifier, -country_code, -country_name, -region_code, -region_name, -old_la_code,
      -lad_code, -pcon_code, -pcon_name, -new_la_code, -la_name, -lad_name, -starts_rate_per_100000_population, -participation_rate_per_100000_population, -achievements_rate_per_100000_population, -popGroup
    ) %>% # get rid of ladu area
    mutate(geographic_level = "LEP") %>% # rename as lsip
    rename(area = LEP) %>%
    relocate(area, .before = geographic_level) %>%
    mutate_at(vars(starts, participation, achievements), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = "", "z" = "", "low" = ""))) %>% # convert to blank to avoid error msg
    mutate_at(vars(starts, participation, achievements), as.numeric) %>% # Convert to numeric
    group_by(time_period, area, geographic_level, apprenticeships_or_further_education, level_or_type, age_group) %>% # sum for each LEP
    summarise(across(everything(), list(sum), na.rm = T)) %>%
    rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
    # calc per 100000 values
    mutate(
      starts_rate_per_100000_population = 100000 * starts / pop,
      participation_rate_per_100000_population = 100000 * participation / pop,
      achievements_rate_per_100000_population = 100000 * achievements / pop
    ) %>%
    select(-pop) %>%
    mutate_at(vars(starts, participation, achievements, starts_rate_per_100000_population, participation_rate_per_100000_population, achievements_rate_per_100000_population), as.character) # Convert to string to bind

  # create lsip file
  addLSIP <- x %>%
    filter(geographic_level == "Local authority district") %>%
    # add population
    mutate(popGroup = case_when(
      apprenticeships_or_further_education == "Apprenticeships" & (age_group == "Total" | age_group == "Under 19") ~ paste(apprenticeships_or_further_education, age_group),
      TRUE ~ age_group
    )) %>%
    left_join(popLA, by = c("lad_name" = "area", "popGroup" = "popGroup")) %>%
    # addLSIPS
    left_join(select(C_LADLSIP2020, -LAD21NM), by = c("lad_code" = "LAD21CD")) %>%
    filter(is.na(LSIP) == FALSE) %>% # remove non-english
    select(
      -time_identifier, -country_code, -country_name, -region_code, -region_name, -old_la_code,
      -lad_code, -pcon_code, -pcon_name, -new_la_code, -la_name, -lad_name, -starts_rate_per_100000_population, -participation_rate_per_100000_population, -achievements_rate_per_100000_population, -popGroup
    ) %>% # get rid of ladu area
    mutate(geographic_level = "LSIP") %>% # rename as lsip
    rename(area = LSIP) %>%
    relocate(area, .before = geographic_level) %>%
    mutate_at(vars(starts, participation, achievements), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = "", "z" = "", "low" = ""))) %>% # convert to blank to avoid error msg
    mutate_at(vars(starts, participation, achievements), as.numeric) %>% # Convert to numeric
    group_by(time_period, area, geographic_level, apprenticeships_or_further_education, level_or_type, age_group) %>% # sum for each LEP
    summarise(across(everything(), list(sum), na.rm = T)) %>%
    rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
    # calc per 100000 values
    mutate(
      starts_rate_per_100000_population = 100000 * starts / pop,
      participation_rate_per_100000_population = 100000 * participation / pop,
      achievements_rate_per_100000_population = 100000 * achievements / pop
    ) %>%
    select(-pop) %>%
    mutate_at(vars(starts, participation, achievements, starts_rate_per_100000_population, participation_rate_per_100000_population, achievements_rate_per_100000_population), as.character) # Convert to string to bind

  # join together
  bind_rows(addLA, addCountry, addLEP, addLSIP)
}

## format achievements
F_Achieve_ILR1621 <- format.Achieve.ILR(I_Achieve_ILR1621)
# create downloadable version with new suppression rules
D_Achieve_ILR1621 <- F_Achieve_ILR1621 %>%
  mutate_at(vars(achievements), function(x) str_replace_all(x, c("!" = "c", "\\*" = "u", "~" = "low", "-" = "x")))
write.csv(D_Achieve_ILR1621, file = "Data\\AppData\\D_Achieve_ILR1621.csv", row.names = FALSE)

# create version to use in dashboard
C_Achieve_ILR1621 <- F_Achieve_ILR1621 %>%
  filter(geographic_level == "LEP" | geographic_level == "LSIP" | geographic_level == "National") %>%
  mutate_at(vars(starts, participation, achievements, starts_rate_per_100000_population, participation_rate_per_100000_population, achievements_rate_per_100000_population), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = "", "z" = "", "low" = ""))) %>% # convert to blank to avoid error msg
  mutate_at(vars(starts, participation, achievements, starts_rate_per_100000_population, participation_rate_per_100000_population, achievements_rate_per_100000_population), as.numeric) %>% # Convert to numeric
  mutate(Year = as.numeric(substr(time_period, 3, 4))) %>% # add year name for charts
  filter(time_period != "202122") %>% # ignore temporary data in the latest year
  mutate(typeNeat = case_when(
    apprenticeships_or_further_education == "Further education and skills" ~ "Total FE and skills provision",
    apprenticeships_or_further_education == "Education and training" ~ "Education and training (adults only)",
    apprenticeships_or_further_education == "Community Learning" ~ "Community learning (adults only)",
    apprenticeships_or_further_education == "Apprenticeships" ~ "Apprenticeships (all ages)",
    TRUE ~ apprenticeships_or_further_education
  )) %>%
  mutate(AY = paste(substr(time_period, 3, 4), "/", substr(time_period, 5, 6), sep = "")) %>%
  arrange(fct_relevel(level_or_type, "Education and training: Total", "Community learning: Total", "Apprenticeships: Total", "Further education and skills: Total")) %>%
  arrange(fct_relevel(age_group, "Total", "Under 19", "19-24", "25+")) %>%
  arrange(fct_relevel(typeNeat, "Total FE and skills provision"))

write.csv(C_Achieve_ILR1621, file = "Data\\AppData\\C_Achieve_ILR1621.csv", row.names = FALSE)

# create max and min vacancy pc by LEP for use in setting axis
C_Achieve_ILR1621_max_min <- C_Achieve_ILR1621 %>%
  group_by(geographic_level, area, level_or_type) %>%
  summarise(minAch = min(achievements), maxAch = max(achievements), .groups = "drop")
write.csv(C_Achieve_ILR1621_max_min, file = "Data\\AppData\\C_Achieve_ILR1621_max_min.csv", row.names = FALSE)

## format achievements by SSA
format.AchieveSSA.ILR <- function(x) {
  # create lep file
  addLEP <- x %>%
    filter(geographic_level == "localAuthorityDistrict") %>%
    left_join(select(C_LADLEP2020, -LAD21NM), by = c("location_code" = "LAD21CD")) %>%
    filter(is.na(LEP) == FALSE) %>% # remove non-english
    select(-location, -location_code, -ethnicity_group) %>% # get rid of ladu area
    mutate(geographic_level = "LEP") %>% # rename as lsip
    rename(area = LEP) %>%
    relocate(area, .before = geographic_level) %>%
    mutate_at(vars(e_and_t_aims_ach, e_and_t_aims_enrolments), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = "", "low" = ""))) %>% # convert to blank to avoid error msg
    mutate_at(vars(e_and_t_aims_ach, e_and_t_aims_enrolments), as.numeric) %>% # Convert to numeric
    group_by(time_period, area, geographic_level, ssa_t1_desc, notional_nvq_level, sex) %>% # sum for each LSIP
    summarise(across(everything(), list(sum), na.rm = T)) %>%
    rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
    mutate_at(vars(e_and_t_aims_ach, e_and_t_aims_enrolments), as.character) # Convert to sring to bind

  # create lsip file
  addLSIP <- x %>%
    filter(geographic_level == "localAuthorityDistrict") %>%
    left_join(select(C_LADLSIP2020, -LAD21CD), by = c("location" = "LAD21NM")) %>%
    filter(is.na(LSIP) == FALSE) %>% # remove non-english
    select(-location, -location_code, -ethnicity_group) %>% # get rid of ladu area
    mutate(geographic_level = "LSIP") %>% # rename as lsip
    rename(area = LSIP) %>%
    relocate(area, .before = geographic_level) %>%
    mutate_at(vars(e_and_t_aims_ach, e_and_t_aims_enrolments), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = "", "low" = ""))) %>% # convert to blank to avoid error msg
    mutate_at(vars(e_and_t_aims_ach, e_and_t_aims_enrolments), as.numeric) %>% # Convert to numeric
    group_by(time_period, area, geographic_level, ssa_t1_desc, notional_nvq_level, sex) %>% # sum for each LSIP
    summarise(across(everything(), list(sum), na.rm = T)) %>%
    rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
    mutate_at(vars(e_and_t_aims_ach, e_and_t_aims_enrolments), as.character) # Convert to sring to bind

  # join together
  bind_rows(x %>% select(-location_code, -ethnicity_group) %>% rename(area = location), addLEP, addLSIP) %>%
    mutate(e_and_t_aims_ach = case_when(e_and_t_aims_ach == "0" ~ "low", TRUE ~ e_and_t_aims_ach)) %>% # recode missing numbers as low
    mutate(e_and_t_aims_enrolments = case_when(e_and_t_aims_enrolments == "0" ~ "low", TRUE ~ e_and_t_aims_enrolments)) %>% # recode missing numbers as low
    rename_all(recode, e_and_t_aims_ach = "achievements", e_and_t_aims_enrolments = "enrolments")
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
  mutate_at(vars(achievements, enrolments), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = "", "low" = ""))) %>% # convert to blank to avoid error msg
  mutate(Achievements = as.numeric(achievements), Enrolments = as.numeric(enrolments)) %>%
  filter(time_period == "202122") %>%
  select(geographic_level, area, SSA = ssa_t1_desc, Level = notional_nvq_level, sex, Achievements, Enrolments)
# group by ssa to get ssa totals
Ach_pc_Achieve_ILR21 <- SSA_LEP_Achieve_ILR21 %>%
  filter(SSA == "Total") %>%
  select(geographic_level, area, Level, sex, Total = SSA, Total_ach = Achievements, Total_enr = Enrolments)
# calculate ssa %s
C_Achieve_ILR21 <- SSA_LEP_Achieve_ILR21 %>%
  left_join(Ach_pc_Achieve_ILR21, by = c("geographic_level", "area", "Level", "sex")) %>%
  group_by(geographic_level, area, Level, sex) %>%
  mutate(pcAch = Achievements / Total_ach, pcEnr = Enrolments / Total_enr) %>%
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
  filter(year >= 2018) %>% # only showing past 5 years in chart
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

# Tidy up data table
names(I_DataTable) <- gsub(".", " ", names(I_DataTable), fixed = TRUE)
write.csv(I_DataTable, file = "Data\\AppData\\I_DataTable.csv", row.names = FALSE)
