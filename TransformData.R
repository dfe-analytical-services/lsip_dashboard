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
library(reshape2)
library(broom)
library(mapproj)
library(leaflet)
library(rgdal)
library(rgeos)
library(sf)

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
  ) %>%
  bind_rows(
    I_mcalookup %>%
      distinct(Area = CAUTH21NM) %>%
      arrange(Area) %>%
      mutate(geographic_level = "MCA")
  ) %>%
  mutate(Area = trimws(Area, which = c("right")))


write.csv(C_LEP2020, file = "Data\\AppData\\C_LEP2020.csv", row.names = FALSE)

# Create LAD-LEP lookup table
C_LADLEP2020 <- distinct(I_LEP2020, LAD21CD, LAD21NM, LEP = LEP21NM1) %>%
  bind_rows(I_missingLAD %>% filter(LAD21CD != "z") %>% select(LAD21CD, LEP = `LEP21.(manually.mapped)`)) %>%
  bind_rows(distinct(I_LEP2020 %>% filter(LEP21NM2 != 0), LAD21CD, LAD21NM, LEP = LEP21NM2))


# Create LAD-LSIP lookup table
C_LADLSIP2020 <- distinct(I_LEP2020, LAD21CD, LAD21NM, LSIP) %>%
  bind_rows(I_missingLAD %>% filter(LAD21CD != "z") %>% select(LAD21CD, LAD21NM = area, LSIP = `LSIP21.(manually.mapped)`)) %>%
  mutate(LSIP = trimws(LSIP, which = c("right")))

# create mca lookup
C_mcalookup <- I_mcalookup


# Data cleaning functions ----
## Employment by occupation ----
format.EmpOcc.APS <- function(x) {
  reformat <- x %>%
    mutate(year = ifelse(annual.population.survey == "date", substr(X2, nchar(X2) - 4 + 1, nchar(X2)), NA)) %>% # tag time periods
    fill(year) %>% # fill time periods for all rows
    row_to_names(row_number = 4) %>% # set col names
    clean_names() %>%
    select(-starts_with("na")) %>% # remove na columns (flags and confidence)
    mutate(check = ifelse(grepl(":", area), 1, 0)) %>% # remove anything but LEP and Country
    filter(check == 1) %>%
    filter(!grepl("nomisweb", area)) %>%
    select(year = x2017, area, everything(), -check) %>% # reorder and remove
    mutate(area2 = gsub(".*-", "", area)) %>%
    mutate(geographic_level = gsub(":.*", "", area)) %>% # Get geog type
    mutate(area = gsub(".*:", "", area)) %>%
    mutate(area = gsub("-.*", "", area)) %>%
    mutate(area = case_when(
      area == "Hull and East Riding" ~ "Hull and East Yorkshire",
      area == "Buckinghamshire Thames Valley" ~ "Buckinghamshire",
      area == "Heart of the South" ~ "Heart of the South-West",
      area == "Essex, Southend" ~ "Essex, Southend-on-Sea and Thurrock",
      area == "Stoke" ~ "Stoke-on-Trent and Staffordshire",
      TRUE ~ area
    )) %>%
    mutate(geographic_level = ifelse(geographic_level == "User Defined Geography", area2, geographic_level)) %>%
    select(area, everything(), -area2) %>%
    relocate(geographic_level, year, .after = area) %>%
    # mutate(year = as.numeric(substr(year, 5, 8))) %>%
    rename_with(
      .fn = ~ str_replace_all(.x, c("t09a_" = "", "_" = " ", "soc2010 all people" = "", "all people" = "")),
      .cols = starts_with("t09a_")
    ) %>%
    rename_with(~ gsub("^[0-9]", "", .)) %>%
    rename_with(~ gsub("^[0-9]", "", .)) %>%
    mutate(geographic_level = toupper(geographic_level)) %>%
    rename_with(str_to_title, c(4:28)) %>% # capitalise column names
    mutate(geographic_level = toupper(geographic_level)) %>%
    filter(geographic_level %in% c("LSIP", "LEP", "LADU", "COUNTRY", "MCA"))
}
# format data
F_EmpOcc_APS1721 <- format.EmpOcc.APS(I_EmpOcc_APS1721)
# create downloadable version with new suppression rules
D_EmpOcc_APS1721 <- F_EmpOcc_APS1721 %>%
  # select(-allOccs) %>%
  mutate_at(vars(-year, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "c", "\\*" = "u", "~" = "low", "-" = "x", "9999999" = "x")))
write.csv(D_EmpOcc_APS1721, file = "Data\\AppData\\D_EmpOcc_APS1721.csv", row.names = FALSE)
# create version to use in dashboard
C_EmpOcc_APS1721 <- F_EmpOcc_APS1721 %>%
  filter(
    year == "2021",
    # geographic_level != "LADU" &
    geographic_level != "GOR" # cleans up for London and South East which is included as lep and gor
  ) %>%
  mutate_at(vars(-year, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = "", "9999999" = ""))) %>% # convert to blank to avoid error msg
  mutate_at(c(4:28), as.numeric) # %>% # Convert to numeric
# mutate(allOccsRemain = allOccs -
#        rowSums(select(., -allOccs, -year, -area, -geographic_level), na.rm = TRUE)) %>%
# mutate(allOccsRemain = case_when(allOccsRemain <= 0 ~ 0, TRUE ~ allOccsRemain)) %>%
# select(-allOccs)

## Employment level and rate ----
format.EmpRate.APS <- function(x) {
  reformat <- x %>%
    mutate(year = ifelse(str_like(annual.population.survey, "%date%"), substr(X2, nchar(X2) - 4 + 1, nchar(X2)), NA)) %>% # tag time periods
    fill(year) %>% # fill time periods for all rows
    row_to_names(row_number = 4) %>% # set col names
    clean_names() %>%
    select(-starts_with("na")) %>% # remove na columns (flags and confidence)
    mutate(check = ifelse(grepl(":", area), 1, 0)) %>% # remove anything but LEP and Country
    filter(check == 1) %>%
    filter(!grepl("nomisweb", area)) %>%
    select(year = x2018, area, everything(), -check) %>%
    mutate(area = gsub(" - from dn81838", "", area)) %>%
    mutate(area2 = gsub(".*-", "", area)) %>%
    mutate(geographic_level = gsub(":.*", "", area)) %>% # Get geog type
    mutate(area = gsub(".*:", "", area)) %>%
    mutate(area = gsub("-.*", "", area)) %>%
    mutate(area = case_when(
      area == "Hull and East Riding" ~ "Hull and East Yorkshire",
      area == "Buckinghamshire Thames Valley" ~ "Buckinghamshire",
      area == "Heart of the South" ~ "Heart of the South-West",
      area == "Essex, Southend" ~ "Essex, Southend-on-Sea and Thurrock",
      area == "Stoke" ~ "Stoke-on-Trent and Staffordshire",
      area == "Brighton and Hove, East Sussex, Wes" ~ "Brighton and Hove, East Sussex, West Sussex",
      area == "Enterprise M3 LEP (including all of" ~ "Enterprise M3 LEP (including all of Surrey)",
      TRUE ~ area
    )) %>%
    mutate(geographic_level = ifelse(geographic_level == "User Defined Geography", area2, geographic_level)) %>%
    select(area, everything(), -area2) %>%
    relocate(geographic_level, year, .after = area) %>%
    # mutate(year = as.numeric(substr(year, 5, 8))) %>%
    rename_with(
      .fn = ~ str_replace_all(.x, c("t01_" = "", "_" = " ", "aged 16 64" = "", "all people" = "")),
      .cols = starts_with("t01_")
    ) %>%
    rename_with(~ gsub("[[:digit:]]+", "", .)) %>%
    rename_with(str_to_title, c(4:10)) %>% # capitalise column names
    mutate(geographic_level = toupper(geographic_level)) %>%
    mutate(geographic_level = case_when(
      geographic_level == "LSI" ~ "LSIP",
      geographic_level == "LS" ~ "LSIP",
      geographic_level == "USER DEFINED GEOGRAPHY:BRIGHTON AND HOVE, EAST SUSSEX, WES" ~ "LSIP",
      geographic_level == "USER DEFINED GEOGRAPHY:ENTERPRISE M3 LEP (INCLUDING ALL OF" ~ "LSIP",
      geographic_level == "SEA AND THURROCK" ~ "LSIP",
      area == "West of England and North Somerset" ~ "LSIP",
      TRUE ~ geographic_level
    )) %>%
    filter(geographic_level %in% c("LSIP", "LEP", "LADU", "COUNTRY", "MCA"))
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
  mutate(
    empRate = `  In Employment ` / `  All `,
    selfempRate = `  Self Employed ` / `  All `,
    unempRate = `  Unemployed ` / `  All `,
    inactiveRate = `  Inactive ` / `  All `
  ) %>%
  filter(
    # geographic_level != "LADU" # not needed for the dashboard currently
    geographic_level != "GOR"
  ) %>%
  mutate(Year = as.numeric(substr(year, 3, 4))) %>% # for use in charts
  rename(Employment = `  In Employment `) # for use in charts
write.csv(C_EmpRate_APS1822, file = "Data\\AppData\\C_EmpRate_APS1822.csv", row.names = FALSE)

# create max and min emp count and rate by LEP for use in setting axis
C_EmpRate_APS1822_max_min <- C_EmpRate_APS1822 %>%
  group_by(area) %>%
  summarise(minEmp = min(Employment), maxEmp = max(Employment))
write.csv(C_EmpRate_APS1822_max_min, file = "Data\\AppData\\C_EmpRate_APS1822_max_min.csv", row.names = FALSE)

# FE data cleaning
# Clean ILR column names, reorder and reformat
addCountry <- I_Achieve_ILR1621 %>%
  filter(geographic_level == "National") %>%
  select(
    -time_identifier, -country_code, -region_code, -region_name, -old_la_code,
    -lad_code, -pcon_code, -pcon_name, -new_la_code, -la_name, -lad_name
  ) %>% # get rid of ladu area
  rename(area = country_name)

# calculate population for each LA
popNewLA <- I_Achieve_ILR1621 %>%
  filter(geographic_level == "Local authority district") %>%
  select(
    -time_identifier, -country_code, -country_name, -region_code, -region_name, -old_la_code,
    -lad_code, -pcon_code, -pcon_name, -new_la_code, -la_name
  ) %>% # get rid of ladu area
  rename(area = lad_name) %>%
  # filter(time_period == 202021) %>%
  filter(level_or_type == "Further education and skills: Total" | (level_or_type == "Apprenticeships: Total" & (age_group == "Under 19" | age_group == "Total"))) %>%
  mutate_at(vars(starts, participation, achievements, starts_rate_per_100000_population, participation_rate_per_100000_population, achievements_rate_per_100000_population), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = "", "z" = "", "low" = ""))) %>% # convert to blank to avoid error msg
  mutate_at(vars(starts, participation, achievements, starts_rate_per_100000_population, participation_rate_per_100000_population, achievements_rate_per_100000_population), as.numeric) %>% # Convert to numeric
  mutate(pop = 100000 * participation / participation_rate_per_100000_population) %>%
  mutate(popGroup = case_when(
    apprenticeships_or_further_education == "Apprenticeships" ~ paste(apprenticeships_or_further_education, age_group),
    TRUE ~ age_group
  )) %>%
  select(area, popGroup, time_period, pop) %>%
  # Use new LA names
  left_join(I_LaLookup %>% select(LAD11NM, LAD21NM), by = c("area" = "LAD11NM")) %>% # make new LAs
  mutate(area = case_when(
    is.na(LAD21NM) == FALSE ~ LAD21NM,
    TRUE ~ area
  )) %>%
  select(-LAD21NM) %>%
  group_by(area, popGroup, time_period) %>%
  summarise(pop = sum(pop))

# create LA file (have to do this because of the old leps)
newLA <- I_Achieve_ILR1621 %>%
  filter(geographic_level == "Local authority district") %>%
  # Use new LA names
  left_join(I_LaLookup %>% select(LAD11CD, LAD21NM, LAD21CD), by = c("lad_code" = "LAD11CD")) %>% # make new LAs
  mutate(
    area = case_when(
      is.na(LAD21NM) == FALSE ~ LAD21NM,
      TRUE ~ lad_name
    ),
    areaCode = case_when(
      is.na(LAD21CD) == FALSE ~ LAD21CD,
      TRUE ~ lad_code
    )
  ) %>%
  select(-LAD21NM, -LAD21CD) %>%
  # add population
  mutate(popGroup = case_when(
    apprenticeships_or_further_education == "Apprenticeships" & (age_group == "Total" | age_group == "Under 19") ~ paste(apprenticeships_or_further_education, age_group),
    TRUE ~ age_group
  ))

addNewLA <- newLA %>%
  select(
    -time_identifier, -country_code, -country_name, -region_code, -region_name, -old_la_code,
    -lad_code, -pcon_code, -pcon_name, -new_la_code,
    -la_name, -lad_name, -areaCode,
    -starts_rate_per_100000_population, -participation_rate_per_100000_population, -achievements_rate_per_100000_population
  ) %>%
  # mutate(geographic_level = "LADU") %>% # rename as ladu
  # rename(area = lad_name) %>%
  relocate(area, .before = geographic_level) %>%
  mutate_at(vars(starts, participation, achievements), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = "", "z" = "", "low" = ""))) %>% # convert to blank to avoid error msg
  mutate_at(vars(starts, participation, achievements), as.numeric) %>% # Convert to numeric
  group_by(time_period, area, geographic_level, apprenticeships_or_further_education, level_or_type, age_group, popGroup) %>% # sum for each LEP
  summarise(across(everything(), list(sum), na.rm = T)) %>%
  rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
  left_join(popNewLA, by = c("area" = "area", "popGroup" = "popGroup", "time_period" = "time_period")) %>%
  select(-popGroup) %>%
  # calc per 100000 values
  mutate(
    starts_rate_per_100000_population = 100000 * starts / pop,
    participation_rate_per_100000_population = 100000 * participation / pop,
    achievements_rate_per_100000_population = 100000 * achievements / pop
  ) %>%
  select(-pop) %>%
  mutate_at(vars(starts, participation, achievements, starts_rate_per_100000_population, participation_rate_per_100000_population, achievements_rate_per_100000_population), as.character) # Convert to string to bind

addLA <- I_Achieve_ILR1621 %>%
  filter(geographic_level == "Local authority district") %>%
  select(
    -time_identifier, -country_code, -country_name, -region_code, -region_name, -old_la_code,
    -lad_code, -pcon_code, -pcon_name, -new_la_code, -la_name
  ) %>% # get rid of ladu area
  rename(area = lad_name)

popLA <- addLA %>%
  # filter(time_period == 202021) %>%
  filter(level_or_type == "Further education and skills: Total" | (level_or_type == "Apprenticeships: Total" & (age_group == "Under 19" | age_group == "Total"))) %>%
  mutate_at(vars(starts, participation, achievements, starts_rate_per_100000_population, participation_rate_per_100000_population, achievements_rate_per_100000_population), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = "", "z" = "", "low" = ""))) %>% # convert to blank to avoid error msg
  mutate_at(vars(starts, participation, achievements, starts_rate_per_100000_population, participation_rate_per_100000_population, achievements_rate_per_100000_population), as.numeric) %>% # Convert to numeric
  mutate(pop = 100000 * participation / participation_rate_per_100000_population) %>%
  mutate(popGroup = case_when(
    apprenticeships_or_further_education == "Apprenticeships" ~ paste(apprenticeships_or_further_education, age_group),
    TRUE ~ age_group
  )) %>%
  select(area, popGroup, time_period, pop)

# create lep file
# create lep file
addLEP <- I_Achieve_ILR1621 %>%
  filter(geographic_level == "Local authority district") %>%
  # add population
  mutate(popGroup = case_when(
    apprenticeships_or_further_education == "Apprenticeships" & (age_group == "Total" | age_group == "Under 19") ~ paste(apprenticeships_or_further_education, age_group),
    TRUE ~ age_group
  )) %>%
  left_join(popLA, by = c("lad_name" = "area", "popGroup" = "popGroup", "time_period" = "time_period")) %>%
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
addLSIP <- I_Achieve_ILR1621 %>%
  filter(geographic_level == "Local authority district") %>%
  # add population
  mutate(popGroup = case_when(
    apprenticeships_or_further_education == "Apprenticeships" & (age_group == "Total" | age_group == "Under 19") ~ paste(apprenticeships_or_further_education, age_group),
    TRUE ~ age_group
  )) %>%
  left_join(popLA, by = c("lad_name" = "area", "popGroup" = "popGroup", "time_period" = "time_period")) %>%
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

# create lsip file
addMCA <- I_Achieve_ILR1621 %>%
  filter(geographic_level == "Local authority district") %>%
  # add population
  mutate(popGroup = case_when(
    apprenticeships_or_further_education == "Apprenticeships" & (age_group == "Total" | age_group == "Under 19") ~ paste(apprenticeships_or_further_education, age_group),
    TRUE ~ age_group
  )) %>%
  left_join(popLA, by = c("lad_name" = "area", "popGroup" = "popGroup", "time_period" = "time_period")) %>%
  # addMCA
  left_join(select(C_mcalookup, -CAUTH21CD, -LAD21NM), by = c("lad_code" = "LAD21CD")) %>%
  select(
    -time_identifier, -country_code, -country_name, -region_code, -region_name, -old_la_code,
    -lad_code, -pcon_code, -pcon_name, -new_la_code, -la_name, -lad_name, -starts_rate_per_100000_population,
    -participation_rate_per_100000_population, -achievements_rate_per_100000_population, -popGroup
  ) %>% # get rid of ladu area
  mutate(geographic_level = "MCA") %>% # rename as MCA
  rename(area = CAUTH21NM) %>%
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
  mutate_at(vars(starts, participation, achievements, starts_rate_per_100000_population, participation_rate_per_100000_population, achievements_rate_per_100000_population), as.character) %>% # Convert to string to bind
  filter(!is.na(area))
# join together
F_Achieve_ILR1621 <- bind_rows(addNewLA, addCountry, addLEP, addLSIP, addMCA)

# create downloadable version with new suppression rules
D_Achieve_ILR1621 <- F_Achieve_ILR1621 %>%
  mutate_at(vars(achievements), function(x) str_replace_all(x, c("!" = "c", "\\*" = "u", "~" = "low", "-" = "x")))
write.csv(D_Achieve_ILR1621, file = "Data\\AppData\\D_Achieve_ILR1621.csv", row.names = FALSE)

# create version to use in dashboard
C_Achieve_ILR1621 <- F_Achieve_ILR1621 %>%
  ungroup() %>%
  filter(geographic_level == "LEP" | geographic_level == "LSIP" | geographic_level == "National" | geographic_level == "MCA" | geographic_level == "Local authority district") %>%
  mutate_at(vars(starts, participation, achievements, starts_rate_per_100000_population, participation_rate_per_100000_population, achievements_rate_per_100000_population), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = "", "z" = "", "low" = ""))) %>% # convert to blank to avoid error msg
  mutate_at(vars(starts, participation, achievements, starts_rate_per_100000_population, participation_rate_per_100000_population, achievements_rate_per_100000_population), as.numeric) %>% # Convert to numeric
  mutate(Year = as.numeric(substr(time_period, 3, 4))) %>% # add year name for charts
  # filter(time_period != "202122") %>% # ignore temporary data in the latest year
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

  # create MCA file
  addMCA <- x %>%
    filter(geographic_level == "localAuthorityDistrict") %>%
    left_join(select(C_mcalookup, -LAD21CD, -CAUTH21CD), by = c("location" = "LAD21NM")) %>%
    select(-location, -location_code, -ethnicity_group) %>% # get rid of ladu area
    mutate(geographic_level = "MCA") %>% # rename as MCA
    rename(area = CAUTH21NM) %>%
    relocate(area, .before = geographic_level) %>%
    mutate_at(vars(e_and_t_aims_ach, e_and_t_aims_enrolments), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = "", "low" = ""))) %>% # convert to blank to avoid error msg
    mutate_at(vars(e_and_t_aims_ach, e_and_t_aims_enrolments), as.numeric) %>% # Convert to numeric
    group_by(time_period, area, geographic_level, ssa_t1_desc, notional_nvq_level, sex) %>% # sum for each LSIP
    summarise(across(everything(), list(sum), na.rm = T)) %>%
    rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
    mutate_at(vars(e_and_t_aims_ach, e_and_t_aims_enrolments), as.character) %>% # Convert to sring to bind
    filter(!is.na(area))

  # join together
  bind_rows(x %>% select(-location_code, -ethnicity_group) %>% rename(area = location), addLEP, addLSIP, addMCA) %>%
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
  filter(geographic_level != "region") %>%
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
  filter(SSA != "Total") %>%
  mutate(geographic_level = case_when(
    geographic_level == "localAuthorityDistrict" ~ "LADU",
    geographic_level == "country" ~ "COUNTRY",
    TRUE ~ geographic_level
  ))

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

  addMCA <- reformat %>%
    left_join(select(C_mcalookup, -LAD21CD, -CAUTH21CD), by = c("LA" = "LAD21NM")) %>%
    select(-LA, -region) %>% # get rid of MCA area
    mutate(geographic_level = "MCA") %>% # rename as MCA
    rename(area = CAUTH21NM) %>%
    relocate(vacancy_unit, .after = geographic_level) %>%
    mutate_at(c(4:4), as.numeric) %>% # Convert to numeric
    group_by(year, area, geographic_level) %>% # sum for each LSIP
    summarise(across(everything(), list(sum), na.rm = T)) %>%
    rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
    mutate_at(c(4:4), as.character) %>% # Convert to sring to bind
    filter(!is.na(area))

  # join together
  bind_rows(addLA, addRegion, addLSIP, addLEP, addMCA)
}

## UK Business Count - Enterprise by employment size
# Reshape data to long, rename and reorder and reformat some columns

format.empent.UBC <- function(x) {
  reformat <- x %>%
    rename(business_counts = "UK.Business.Counts.-.enterprises.by.industry.and.employment.size.band") %>%
    mutate(year = ifelse(business_counts == "date", substr(X2, nchar(X2) - 4 + 1, nchar(X2)), NA)) %>% # tag time periods
    fill(year) %>%
    row_to_names(row_number = 5) %>% # set col names
    clean_names() %>%
    rename("year" = !!names(.[7])) %>%
    mutate(check = ifelse(grepl(":", area), 1, 0)) %>% # remove anything but LEP and Country
    filter(check == 1) %>%
    filter(!grepl("nomisweb", area)) %>%
    select(area, everything(), -check) %>% # reorder and remove
    mutate(area2 = gsub(".*-", "", area)) %>%
    mutate(geographic_level = gsub(":.*", "", area)) %>% # Get geog type
    mutate(area = gsub(".*:", "", area)) %>%
    mutate(area = gsub("-.*", "", area)) %>%
    mutate(area = case_when(
      area == "Hull and East Riding" ~ "Hull and East Yorkshire",
      area == "Buckinghamshire Thames Valley" ~ "Buckinghamshire",
      area == "Heart of the South" ~ "Heart of the South-West",
      area == "Essex, Southend" ~ "Essex, Southend-on-Sea and Thurrock",
      area == "Stoke" ~ "Stoke-on-Trent and Staffordshire",
      TRUE ~ area
    )) %>%
    mutate(geographic_level = ifelse(geographic_level == "User Defined Geography", area2, geographic_level)) %>%
    select(area, everything(), -area2) %>%
    relocate(geographic_level, year, .after = area) %>%
    mutate(geographic_level = toupper(geographic_level)) %>%
    melt(id.vars = c("year", "geographic_level", "area")) %>%
    mutate(variable = sub("(.)", "\\U\\1", variable, perl = TRUE)) %>% # capitalise first letter
    filter(geographic_level %in% c("LSIP", "LEP", "LADU", "COUNTRY", "MCA"))
}

## format UBC
F_empent_UBC1822 <- format.empent.UBC(I_EmpEnt_APS1822)

# Downloadable version
D_empent_UBC1822 <- F_empent_UBC1822 %>%
  mutate_at(vars(-year, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "c", "\\*" = "u", "~" = "low", "-" = "x")))
# write the data to folder
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

  addMCA <- x %>%
    left_join(select(C_mcalookup, -LAD21NM, -CAUTH21CD), by = c("location_code" = "LAD21CD")) %>%
    select(-area, -location_code) %>% # get rid of MCA area
    mutate(geographic_level = "MCA") %>% # rename as MCA
    rename(area = CAUTH21NM) %>%
    relocate(area, .before = geographic_level) %>%
    mutate_at(vars(-time_period, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = ""))) %>% # convert to blank to avoid error msg
    mutate_at(c(8:13), as.numeric) %>% # Convert to numeric
    select(-data_type, -institution_group, -level_methodology, -characteristic) %>%
    group_by(area, geographic_level, time_period) %>% # sum for each LSIP
    summarise(across(everything(), list(sum), na.rm = T)) %>%
    rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
    mutate_at(c(4:9), as.character) %>% # Convert to string to bind
    filter(!is.na(area))

  # join together and rename columns
  LEP_LSIP <- bind_rows(addladu, addLEP, addLSIP, addMCA) %>%
    rename(
      "Total" = "cohort",
      "Unknown" = "all_unknown",
      "Not Recorded" = "all_notsust",
      "Sustained Education" = "education",
      "Sustained Employment" = "all_work",
      "Sustained Apprenticeships" = "appren"
    ) %>%
    relocate("Total", .after = "Unknown") %>%
    relocate(area, .before = geographic_level) %>%
    relocate(time_period, .before = area)
}

## format KS4
F_KS4destin_1521 <- format.ks4(I_KS4destin_1521)

# Downloadable data
D_KS4destin_1521 <- F_KS4destin_1521 %>%
  mutate_at(vars(-time_period, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "c", "\\*" = "u", "~" = "low", "-" = "x")))
# write data to folder
write.csv(D_KS4destin_1521, file = "Data\\AppData\\D_KS4destin_1521.csv", row.names = FALSE)



## National Pupil Database  - Key Stage 5 Destinations
# Reshape data to long, rename and reorder and reformat some columns

format.ks5 <- function(x) {
  colnames(x)[1] <- "area"


  addladu <- x %>%
    select(-location_code, -characteristic, -data_type, -institution_group, -level_methodology) %>%
    mutate(geographic_level = replace(geographic_level, geographic_level == "localAuthorityDistrict", "LADU")) %>%
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

  addMCA <- x %>%
    left_join(select(C_mcalookup, -LAD21NM, -CAUTH21CD), by = c("location_code" = "LAD21CD")) %>%
    select(-area, -location_code) %>% # get rid of MCA area
    mutate(geographic_level = "MCA") %>% # rename as MCA
    rename(area = CAUTH21NM) %>%
    relocate(area, .before = geographic_level) %>%
    mutate_at(vars(-time_period, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = ""))) %>% # convert to blank to avoid error msg
    mutate_at(c(9:14), as.numeric) %>% # Convert to numeric
    select(-data_type, -institution_group, -level_methodology, -characteristic) %>%
    group_by(area, geographic_level, time_period, cohort_level_group) %>% # sum for each LSIP
    summarise(across(everything(), list(sum), na.rm = T)) %>%
    rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
    mutate_at(c(5:10), as.character) %>% # Convert to string to bind
    filter(!is.na(area))


  # join together and rename columns
  LEP_LSIP <- bind_rows(addladu, addLEP, addLSIP, addMCA) %>%
    rename(
      "Total" = "cohort",
      "Unknown" = "all_unknown",
      "Not Recorded" = "all_notsust",
      "Sustained Education" = "education",
      "Sustained Employment" = "all_work",
      "Sustained Apprenticeships" = "appren",
      "Cohort Group" = "cohort_level_group"
    ) %>%
    relocate("Total", .after = "Unknown") %>%
    relocate(area, .before = geographic_level) %>%
    relocate(time_period, .before = area)
}

## format KS5
F_KS5destin_1721 <- format.ks5(I_KS5destin_1721)

# downloadable version
D_KS5destin_1721 <- F_KS5destin_1721 %>%
  mutate_at(vars(-time_period, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "c", "\\*" = "u", "~" = "low", "-" = "x")))
# write data to folder - todo
write.csv(D_KS5destin_1721, file = "Data\\AppData\\D_KS5destin_1721.csv", row.names = FALSE)

# employment by industry
format.EmpInd.APS <- function(x) {
  reformat <- x %>%
    mutate(year = ifelse(annual.population.survey == "date", substr(X2, nchar(X2) - 4 + 1, nchar(X2)), NA)) %>% # tag time periods
    fill(year) %>% # fill time periods for all rows
    row_to_names(row_number = 4) %>% # set col names
    clean_names() %>%
    select(-starts_with("na")) %>% # remove na columns (flags and confidence)
    mutate(check = ifelse(grepl(":", area), 1, 0)) %>% # remove anything but LEP and Country
    filter(check == 1) %>%
    filter(!grepl("nomisweb", area)) %>%
    select(year = x2018, area, everything(), -check) %>%
    mutate(area2 = gsub(".*-", "", area)) %>%
    mutate(geographic_level = gsub(":.*", "", area)) %>% # Get geog type
    mutate(area = gsub(".*:", "", area)) %>%
    mutate(area = gsub("-.*", "", area)) %>%
    mutate(area = case_when(
      area == "Hull and East Riding" ~ "Hull and East Yorkshire",
      area == "Buckinghamshire Thames Valley" ~ "Buckinghamshire",
      area == "Heart of the South" ~ "Heart of the South-West",
      area == "Essex, Southend" ~ "Essex, Southend-on-Sea and Thurrock",
      area == "Stoke" ~ "Stoke-on-Trent and Staffordshire",
      TRUE ~ area
    )) %>%
    mutate(geographic_level = ifelse(geographic_level == "User Defined Geography", area2, geographic_level)) %>%
    select(area, everything(), -area2) %>%
    relocate(geographic_level, year, .after = area) %>%
    # mutate(year = as.numeric(substr(year, 5, 8))) %>%
    rename_with(
      .fn = ~ str_replace_all(.x, c("t13a_" = "", "_" = " ", "sic 2007 all people" = "")),
      .cols = starts_with("t13a_")
    ) %>%
    rename_with(~ gsub("[[:digit:]]+", "", .)) %>%
    rename(
      "Agriculture and Fishing" = " a agricuture fishing ",
      "Energy and Water" = " b d e energy water ",
      "Manufacturing" = " c manufacturing ",
      "Construction" = " f construction ",
      "Distribution, Hotels and Restaurants" = " g i distribution hotels restaurants ",
      "Transport and Communication" = " h j transport communication ",
      "Banking, Finance and Insurance" = " k n banking finance insurance etc ",
      "Public Administration, Education and Health" = " o q public admin education health ",
      "Other Services" = " r u other services "
    ) %>%
    mutate(geographic_level = toupper(geographic_level)) %>%
    filter(geographic_level %in% c("LSIP", "LEP", "LADU", "COUNTRY", "MCA"))
}

# format data
F_EmpInd_APS1822 <- format.EmpInd.APS(I_empind_APS1822) %>%
  mutate_at(vars(c(4:12)), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = "")))

# dashboard data
C_EmpInd_APS1822 <- F_EmpInd_APS1822 %>%
  mutate_at(c(4:12), as.numeric) %>%
  melt(id.vars = c("year", "geographic_level", "area")) %>%
  mutate_at(c("value"), ~ replace_na(., 0)) %>%
  group_by(area, year, geographic_level) %>%
  summarise(Total = sum(`value`))

C_EmpInd2_APS1822 <- F_EmpInd_APS1822 %>%
  mutate_at(c(4:12), as.numeric) %>%
  melt(id.vars = c("year", "geographic_level", "area")) %>%
  mutate_at(c("value"), ~ replace_na(., 0)) %>%
  left_join(C_EmpInd_APS1822, by = c(
    "area" = "area", "year" = "year",
    "geographic_level" = "geographic_level"
  )) %>%
  mutate(rate = value / Total)

# Tidy up data table
names(I_DataTable) <- gsub(".", " ", names(I_DataTable), fixed = TRUE)
write.csv(I_DataTable, file = "Data\\AppData\\I_DataTable.csv", row.names = FALSE)


# Tidy up intervention table
names(I_InterventionTable) <- gsub(".", " ", names(I_InterventionTable), fixed = TRUE)
write.csv(I_InterventionTable, file = "Data\\AppData\\I_InterventionTable.csv", row.names = FALSE)

# Tidy up sources table
names(I_SourcesTable) <- gsub(".", " ", names(I_SourcesTable), fixed = TRUE)
write.csv(I_SourcesTable, file = "Data\\AppData\\I_SourcesTable.csv", row.names = FALSE)
names(I_ToolsTable) <- gsub(".", " ", names(I_ToolsTable), fixed = TRUE)
write.csv(I_ToolsTable, file = "Data\\AppData\\I_ToolsTable.csv", row.names = FALSE)

#### Qualification level by age and gender ####
format.qual.APS <- function(x) {
  reformat <- x %>%
    mutate(year = ifelse(str_like(annual.population.survey, "%date%"), substr(X2, nchar(X2) - 4 + 1, nchar(X2)), NA)) %>% # tag time periods
    fill(year) %>% # fill time periods for all rows
    row_to_names(row_number = 4) %>% # set col names
    clean_names() %>%
    select(-starts_with("na")) %>% # remove na columns (flags and confidence)
    mutate(check = ifelse(grepl(":", area), 1, 0)) %>% # remove anything but LEP and Country
    filter(check == 1) %>%
    filter(!grepl("nomisweb", area)) %>%
    select(year = x2017, area, everything(), -check) %>% # reorder and remove
    mutate(area2 = gsub(".*-", "", area)) %>%
    mutate(geographic_level = gsub(":.*", "", area)) %>% # Get geog type
    mutate(area = gsub(".*:", "", area)) %>%
    mutate(area = gsub("-.*", "", area)) %>%
    mutate(area = case_when(
      area == "Hull and East Riding" ~ "Hull and East Yorkshire",
      area == "Buckinghamshire Thames Valley" ~ "Buckinghamshire",
      area == "Heart of the South" ~ "Heart of the South-West",
      area == "Essex, Southend" ~ "Essex, Southend-on-Sea and Thurrock",
      area == "Stoke" ~ "Stoke-on-Trent and Staffordshire",
      TRUE ~ area
    )) %>%
    mutate(geographic_level = ifelse(geographic_level == "User Defined Geography", area2, geographic_level)) %>%
    select(area, everything(), -area2) %>%
    relocate(geographic_level, year, .after = area) %>%
    # mutate(year = as.numeric(substr(year, 5, 8))) %>%
    rename_with(
      .fn = ~ str_replace_all(.x, c("t19_" = "", "_" = " ")),
      .cols = starts_with("t19_")
    ) %>%
    rename_with(~ gsub("^[0-9]", "", .)) %>% # get rid of numbers at begining of column names
    rename_with(~ gsub("^[0-9]", "", .)) %>%
    mutate(geographic_level = toupper(geographic_level)) %>%
    melt(id.vars = c("year", "geographic_level", "area")) %>%
    mutate(group = case_when(
      grepl("all people", variable) ~ "Total",
      grepl("females", variable) ~ "Female",
      grepl("males", variable) ~ "Male"
    )) %>%
    mutate(variable = trimws(variable)) %>% # get rid white space at begining
    mutate(variable = sub("(.)", "\\U\\1", variable, perl = TRUE)) %>%
    mutate(Level = case_when(
      grepl("none", variable) ~ "None",
      grepl("nvq1", variable) ~ "NVQ1",
      grepl("nvq2", variable) ~ "NVQ2",
      grepl("trade apprenticeships", variable) ~ "Trade Apprenticeships",
      grepl("nvq3", variable) ~ "NVQ3",
      grepl("nvq4", variable) ~ "NVQ4",
      grepl("other qualifications", variable) ~ "Other Qualifications"
    )) %>%
    mutate(age_band = case_when(
      grepl("aged 16 64", variable) ~ "16-64",
      grepl("aged 16 19", variable) ~ "16-19",
      grepl("aged 20 24", variable) ~ "20-24",
      grepl("aged 25 29", variable) ~ "25-29",
      grepl("aged 30 39", variable) ~ "30-39",
      grepl("aged 40 49", variable) ~ "40-49",
      grepl("aged 50 64", variable) ~ "50-64"
    )) %>%
    select(area, everything(), -variable) %>%
    filter(geographic_level %in% c("LSIP", "LEP", "LADU", "COUNTRY", "MCA")) %>%
    rename(gender = group)
}

# format data
F_qual_APS1721 <- format.qual.APS(I_qual_APS1721) %>%
  mutate_at(vars(value), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = ""))) %>%
  mutate_at(c(4:4), as.numeric) %>%
  mutate(value = ifelse(is.na(value), 0, value))

# dashboard data formatting
C_qual_APS1721 <- F_qual_APS1721 %>%
  group_by(area, year, geographic_level, gender, age_band) %>%
  summarise(Total = sum(`value`))

#### Level 3 plus ####
C_qualevel3plus_APS1721 <- F_qual_APS1721 %>%
  filter(Level %in% c("NVQ3", "NVQ4")) %>%
  group_by(area, year, geographic_level, gender, age_band) %>%
  summarise(value2 = sum(`value`)) %>%
  mutate(Level = "Level 3 and above") %>%
  left_join(C_qual_APS1721, by = c(
    "area" = "area", "year" = "year",
    "geographic_level" = "geographic_level",
    "gender" = "gender", "age_band" = "age_band"
  )) %>%
  mutate(rate = value2 / Total) %>%
  select(-Total) %>%
  mutate(Year = as.numeric(substr(year, 3, 4))) # add year name for charts

# write data to folder
write.csv(C_qualevel3plus_APS1721, file = "Data\\AppData\\C_qualevel3plus_APS1721.csv", row.names = FALSE)

# get in new format
C_level3Plus <- C_qualevel3plus_APS1721 %>%
  rename(time_period = year, value = rate) %>%
  mutate(metric = "level3AndAboveRate") %>%
  mutate(breakdown = case_when(
    gender %in% c("Male", "Female") ~ "Gender",
    (gender == "Total" & age_band == "16-64") ~ "Total",
    TRUE ~ "Age"
  )) %>%
  mutate(subgroups = case_when(
    breakdown == "Gender" ~ gender,
    breakdown == "Age" ~ age_band,
    TRUE ~ "Total"
  )) %>%
  mutate(time_period = as.numeric(time_period)) %>%
  ungroup() %>%
  select(-Year, -Level, -value2, -gender, -age_band)

# create max and min for use in setting axis
# C_qual_max_min <- C_qual2_APS1721 %>%
# filter(
#  geographic_level != "LADU", Level %in% c("NVQ3", "NVQ4"),
#  age_band == "16-64",
#  gender == "Total"
# ) %>%
# summarise(minnvq4 = min(rate), maxnvq4 = max(rate))

# write data to folder
# write.csv(C_qual_max_min, file = "Data\\AppData\\C_qual_max_min.csv", row.names = FALSE)


#### Qualification data ####
# write data to folder
D_qual_APS1721 <- format.qual.APS(I_qual_APS1721) %>%
  mutate_at(vars(value), function(x) str_replace_all(x, c("!" = "c", "\\*" = "u", "~" = "low", "-" = "x"))) %>%
  rename("qualification count" = value) %>%
  relocate("qualification count", .after = age_band)


# write the data to folder
write.csv(D_qual_APS1721, file = "Data\\AppData\\D_qual_APS1721.csv", row.names = FALSE)



#### Employment by industry ####
format.EmpInd.APS <- function(x) {
  reformat <- x %>%
    mutate(year = ifelse(annual.population.survey == "date", substr(X2, nchar(X2) - 4 + 1, nchar(X2)), NA)) %>% # tag time periods
    fill(year) %>% # fill time periods for all rows
    row_to_names(row_number = 4) %>% # set col names
    clean_names() %>%
    select(-starts_with("na")) %>% # remove na columns (flags and confidence)
    mutate(check = ifelse(grepl(":", area), 1, 0)) %>% # remove anything but LEP and Country
    filter(check == 1) %>%
    filter(!grepl("nomisweb", area)) %>%
    select(year = x2018, area, everything(), -check) %>%
    mutate(area = gsub(" - from dn81838", "", area)) %>%
    mutate(area2 = gsub(".*-", "", area)) %>%
    mutate(geographic_level = gsub(":.*", "", area)) %>% # Get geog type
    mutate(area = gsub(".*:", "", area)) %>%
    mutate(area = gsub("-.*", "", area)) %>%
    mutate(area = case_when(
      area == "Hull and East Riding" ~ "Hull and East Yorkshire",
      area == "Buckinghamshire Thames Valley" ~ "Buckinghamshire",
      area == "Heart of the South" ~ "Heart of the South-West",
      area == "Essex, Southend" ~ "Essex, Southend-on-Sea and Thurrock",
      area == "Stoke" ~ "Stoke-on-Trent and Staffordshire",
      area == "Brighton and Hove, East Sussex, Wes" ~ "Brighton and Hove, East Sussex, West Sussex",
      area == "Enterprise M3 LEP (including all of" ~ "Enterprise M3 LEP (including all of Surrey)",
      TRUE ~ area
    )) %>%
    mutate(geographic_level = ifelse(geographic_level == "User Defined Geography", area2, geographic_level)) %>%
    select(area, everything(), -area2) %>%
    relocate(geographic_level, year, .after = area) %>%
    # mutate(year = as.numeric(substr(year, 5, 8))) %>%
    rename_with(
      .fn = ~ str_replace_all(.x, c("t13a_" = "", "_" = " ", "sic 2007 all people" = "")),
      .cols = starts_with("t13a_")
    ) %>%
    rename_with(~ gsub("[[:digit:]]+", "", .)) %>%
    rename(
      "Agriculture and Fishing" = " a agricuture fishing ",
      "Energy and Water" = " b d e energy water ",
      "Manufacturing" = " c manufacturing ",
      "Construction" = " f construction ",
      "Distribution, Hotels and Restaurants" = " g i distribution hotels restaurants ",
      "Transport and Communication" = " h j transport communication ",
      "Banking, Finance and Insurance" = " k n banking finance insurance etc ",
      "Public Administration, Education and Health" = " o q public admin education health ",
      "Other Services" = " r u other services "
    ) %>%
    mutate(geographic_level = toupper(geographic_level)) %>%
    mutate(geographic_level = case_when(
      geographic_level == "LSI" ~ "LSIP",
      geographic_level == "LS" ~ "LSIP",
      geographic_level == "USER DEFINED GEOGRAPHY:BRIGHTON AND HOVE, EAST SUSSEX, WES" ~ "LSIP",
      geographic_level == "USER DEFINED GEOGRAPHY:ENTERPRISE M3 LEP (INCLUDING ALL OF" ~ "LSIP",
      geographic_level == "SEA AND THURROCK" ~ "LSIP",
      TRUE ~ geographic_level
    )) %>%
    filter(geographic_level %in% c("LSIP", "LEP", "LADU", "COUNTRY", "MCA"))
}

# format data
F_EmpInd_APS1822 <- format.EmpInd.APS(I_empind_APS1822) %>%
  mutate_at(vars(c(4:12)), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = "")))

# dashboard data
C_EmpInd_APS1822 <- F_EmpInd_APS1822 %>%
  mutate_at(c(4:12), as.numeric) %>%
  melt(id.vars = c("year", "geographic_level", "area")) %>%
  mutate_at(c("value"), ~ replace_na(., 0)) %>%
  group_by(area, year, geographic_level) %>%
  summarise(Total = sum(`value`))

C_EmpInd2_APS1822 <- F_EmpInd_APS1822 %>%
  mutate_at(c(4:12), as.numeric) %>%
  melt(id.vars = c("year", "geographic_level", "area")) %>%
  mutate_at(c("value"), ~ replace_na(., 0)) %>%
  left_join(C_EmpInd_APS1822, by = c(
    "area" = "area", "year" = "year",
    "geographic_level" = "geographic_level"
  )) %>%
  mutate(rate = value / Total)

# write to data folder
write.csv(C_EmpInd2_APS1822, file = "Data\\AppData\\C_EmpInd2_APS1822.csv", row.names = FALSE)

# downloadable version
D_EmpInd_APS1822 <- format.EmpInd.APS(I_empind_APS1822) %>%
  mutate_at(vars(c(4:12)), function(x) str_replace_all(x, c("!" = "c", "\\*" = "u", "~" = "low", "-" = "x")))

# write the data to folder
write.csv(D_EmpInd_APS1822, file = "Data\\AppData\\D_EmpInd_APS1822.csv", row.names = FALSE)


#### 3.3 UK Business Count ####
##### Enterprise by employment size ####
# Reshape data to long, rename and reorder and reformat some columns

format.empent.UBC <- function(x) {
  reformat <- x %>%
    rename(business_counts = "UK.Business.Counts.-.enterprises.by.industry.and.employment.size.band") %>%
    mutate(year = ifelse(business_counts == "date", substr(X2, nchar(X2) - 4 + 1, nchar(X2)), NA)) %>% # tag time periods
    fill(year) %>%
    row_to_names(row_number = 5) %>% # set col names
    clean_names() %>%
    rename("year" = !!names(.[7])) %>%
    mutate(check = ifelse(grepl(":", area), 1, 0)) %>% # remove anything but LEP and Country
    filter(check == 1) %>%
    filter(!grepl("nomisweb", area)) %>%
    select(area, everything(), -check) %>% # reorder and remove
    mutate(area2 = gsub(".*-", "", area)) %>%
    mutate(geographic_level = gsub(":.*", "", area)) %>% # Get geog type
    mutate(area = gsub(".*:", "", area)) %>%
    mutate(area = gsub("-.*", "", area)) %>%
    mutate(area = case_when(
      area == "Hull and East Riding" ~ "Hull and East Yorkshire",
      area == "Buckinghamshire Thames Valley" ~ "Buckinghamshire",
      area == "Heart of the South" ~ "Heart of the South-West",
      area == "Essex, Southend" ~ "Essex, Southend-on-Sea and Thurrock",
      area == "Stoke" ~ "Stoke-on-Trent and Staffordshire",
      TRUE ~ area
    )) %>%
    mutate(geographic_level = ifelse(geographic_level == "User Defined Geography", area2, geographic_level)) %>%
    select(area, everything(), -area2) %>%
    relocate(geographic_level, year, .after = area) %>%
    mutate(geographic_level = toupper(geographic_level)) %>%
    melt(id.vars = c("year", "geographic_level", "area")) %>%
    mutate(variable = sub("(.)", "\\U\\1", variable, perl = TRUE)) %>% # capitalise first letter
    filter(geographic_level %in% c("LSIP", "LEP", "LADU", "COUNTRY", "MCA"))
}

## format UBC
F_empent_UBC1822 <- format.empent.UBC(I_EmpEnt_APS1822)

# dashboard version to combine witn industry total
# get total by enterprise size
C_empent_UBC1822 <- F_empent_UBC1822 %>%
  mutate_at(c(5), as.numeric) %>%
  mutate_at(c(5), ~ replace_na(., 0)) %>%
  filter(variable != "Total") %>%
  group_by(area, year, geographic_level) %>%
  summarise(Total = sum(`value`))

# get overall total and merge onto C_empentind_UBC1822 to calculate proportions by enterprise size
C_empent2_UBC1822 <- F_empent_UBC1822 %>%
  mutate_at(c(5), as.numeric) %>%
  mutate_at(c(5), ~ replace_na(., 0)) %>%
  filter(variable != "Total") %>%
  left_join(C_empent_UBC1822, by = c(
    "area" = "area", "year" = "year",
    "geographic_level" = "geographic_level"
  )) %>%
  mutate(rate = value / Total, industry = "Total") %>%
  select(-Total) %>%
  mutate(variable = case_when(
    variable == "Micro_0_to_9" ~ "Micro 0 to 9",
    variable == "Small_10_to_49" ~ "Small 10 to 49",
    variable == "Medium_sized_50_to_249" ~ "Medium 50 to 249",
    variable == "Large_250" ~ "Large 250+",
    TRUE ~ variable
  ))

# Downloadable version
D_empent_UBC1822 <- F_empent_UBC1822 %>%
  mutate_at(vars(-year, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "c", "\\*" = "u", "~" = "low", "-" = "x"))) %>%
  rename("enterprise size" = variable, "enterprise count" = value)

# write the data to folder
write.csv(D_empent_UBC1822, file = "Data\\AppData\\D_empent_UBC1822.csv", row.names = FALSE)


#### Enterprise by employment size and industry ####
# Reshape data to long, rename and reorder and reformat some columns

format.empentind.UBC <- function(x) {
  reformat <- x %>%
    rename(business_counts = "UK.Business.Counts.-.enterprises.by.industry.and.employment.size.band") %>%
    mutate(year = ifelse(business_counts == "date", substr(X2, nchar(X2) - 4 + 1, nchar(X2)), NA)) %>% # tag time periods
    fill(year) %>%
    mutate(industry = ifelse(business_counts == "industry", X2, NA)) %>% # tag time periods
    fill(industry) %>%
    row_to_names(row_number = 5) %>% # set col names
    clean_names() %>%
    rename(
      "industry" = !!names(.[8]),
      "year" = !!names(.[7])
    ) %>%
    mutate(check = ifelse(grepl(":", area), 1, 0)) %>% # remove anything but LEP and Country
    filter(check == 1) %>%
    filter(!grepl("nomisweb", area)) %>%
    select(area, everything(), -check) %>% # reorder and remove
    mutate(area2 = gsub(".*-", "", area)) %>%
    mutate(geographic_level = gsub(":.*", "", area)) %>% # Get geog type
    mutate(area = gsub(".*:", "", area)) %>%
    mutate(area = gsub("-.*", "", area)) %>%
    mutate(industry = gsub(".*:", "", industry)) %>%
    mutate(area = case_when(
      area == "Hull and East Riding" ~ "Hull and East Yorkshire",
      area == "Buckinghamshire Thames Valley" ~ "Buckinghamshire",
      area == "Heart of the South" ~ "Heart of the South-West",
      area == "Essex, Southend" ~ "Essex, Southend-on-Sea and Thurrock",
      area == "Stoke" ~ "Stoke-on-Trent and Staffordshire",
      TRUE ~ area
    )) %>%
    mutate(geographic_level = ifelse(geographic_level == "User Defined Geography", area2, geographic_level)) %>%
    select(area, everything(), -area2) %>%
    relocate(geographic_level, year, industry, .after = area) %>%
    mutate(geographic_level = toupper(geographic_level)) %>%
    melt(id.vars = c("year", "geographic_level", "industry", "area")) %>%
    mutate(variable = sub("(.)", "\\U\\1", variable, perl = TRUE)) %>% # capitalise first letter
    filter(geographic_level %in% c("LSIP", "LEP", "LADU", "COUNTRY", "MCA"))
}


# format data
F_empentind_UBC1822 <- format.empentind.UBC(I_EmpEntind_APS1822) %>%
  mutate_at(vars(value), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = "")))

# dashboard data
# merge totals from enterprise by emp size - C_empent_UCC1822
C_empentind_UBC1822 <- F_empentind_UBC1822 %>%
  mutate_at(c(6), as.numeric) %>%
  mutate_at(c(6), ~ replace_na(., 0)) %>%
  filter(variable != "Total") %>%
  group_by(area, year, geographic_level, variable) %>%
  summarise(Total = sum(`value`))

C_empentind2_UBC1822 <- F_empentind_UBC1822 %>%
  mutate_at(c(6), as.numeric) %>%
  mutate_at(c(6), ~ replace_na(., 0)) %>%
  filter(variable != "Total") %>%
  left_join(C_empentind_UBC1822, by = c(
    "area" = "area", "year" = "year",
    "geographic_level" = "geographic_level", "variable" = "variable"
  )) %>%
  mutate(rate = value / Total) %>%
  select(-Total) %>%
  rbind(C_empent2_UBC1822)


C_empentind3_UBC1822 <- C_empentind2_UBC1822 %>%
  select(-value) %>%
  mutate(industry = trimws(industry, which = c("left"))) %>%
  mutate(variable = case_when(
    variable == "Micro_0_to_9" ~ "Micro 0 to 9",
    variable == "Small_10_to_49" ~ "Small 10 to 49",
    variable == "Medium_sized_50_to_249" ~ "Medium 50 to 249",
    variable == "Large_250" ~ "Large 250+",
    TRUE ~ variable
  )) %>%
  mutate(Year = as.numeric(substr(year, 3, 4))) # add year name for charts

# write data to folder
write.csv(C_empentind3_UBC1822, file = "Data\\AppData\\C_empentind3_UBC1822.csv", row.names = FALSE)

# create new format version
C_enterpriseSizeIndustry <- bind_rows(
  F_empent_UBC1822 %>%
    rename(time_period = year, subgroups = variable) %>%
    mutate(
      time_period = as.numeric(time_period),
      metric = "enterpriseCount", breakdown = "Size"
    ),
  F_empentind_UBC1822 %>%
    filter(variable == "Total") %>%
    select(-variable) %>%
    rename(time_period = year, subgroups = industry) %>%
    mutate(
      time_period = as.numeric(time_period),
      metric = "enterpriseCount", breakdown = "Industry"
    )
) %>%
  mutate(value = as.numeric(value)) %>%
  mutate(value = replace_na(value, 0)) %>%
  mutate(subgroups = case_when(
    subgroups == "Micro_0_to_9" ~ "Micro 0 to 9",
    subgroups == "Small_10_to_49" ~ "Small 10 to 49",
    subgroups == "Medium_sized_50_to_249" ~ "Medium 50 to 249",
    subgroups == "Large_250" ~ "Large 250+",
    TRUE ~ subgroups
  ))

# create max and min for use in setting axis
C_empentind_max_min <- C_empentind3_UBC1822 %>%
  filter(geographic_level != "LADU", variable == "Micro 0 to 9", industry == "Total") %>%
  summarise(minmic = min(rate), maxmic = max(rate))

# write data to folder
write.csv(C_empentind_max_min, file = "Data\\AppData\\C_empentind_max_min.csv", row.names = FALSE)

# Downloadable version
D_empentind_UBC1822 <- C_empentind2_UBC1822 %>%
  select(-rate) %>%
  mutate_at(vars(value), function(x) str_replace_all(x, c("!" = "c", "\\*" = "u", "~" = "low", "-" = "x"))) %>%
  rename("enterprise size" = variable, "enterprise count" = value)

# write the data to folder
write.csv(D_empentind_UBC1822, file = "Data\\AppData\\D_empentind_UBC1822.csv", row.names = FALSE)

#### Enterprise births, deaths and active ####
format.bussdemo.ONS <- function(w, x, y, z) {
  w <- w %>%
    rename(X1 = 1) %>%
    mutate(X1 = trimws(X1, which = c("both"))) %>%
    select(-X2)

  x <- x %>%
    mutate(X1 = trimws(X1, which = c("both"))) %>%
    select(-X2)

  y <- y %>%
    mutate(X1 = trimws(X1, which = c("both"))) %>%
    select(-X2)

  z <- z %>%
    mutate(X1 = trimws(X1, which = c("both"))) %>%
    mutate(X2 = trimws(X2, which = c("both")))

  df_list <- list(z, y, x, w)
  df <- df_list %>%
    reduce(full_join, by = "X1")

  England <- df %>%
    filter(X2 == "ENGLAND") %>%
    select(-c(1:2)) %>%
    mutate(area = "England") %>%
    mutate(geographic_level = "COUNTRY") %>%
    relocate(area, geographic_level, .before = `2021`) %>%
    mutate_at(c(3:8), as.character)

  LADU <- C_LADLEP2020 %>%
    select("LAD21CD", "LAD21NM") %>%
    distinct()

  addLADU <- LADU %>%
    left_join(select(df, -X2), by = c("LAD21CD" = "X1")) %>%
    rename(area = LAD21NM) %>%
    # select(-c(LAD21CD)) %>%
    mutate(geographic_level = "LADU") %>% # rename as LADU
    relocate(geographic_level, .after = area) %>%
    mutate_at(c(4:9), as.character)

  addLEP <- addLADU %>%
    left_join(select(C_LADLEP2020, -LAD21NM), by = c("LAD21CD" = "LAD21CD")) %>%
    filter(is.na(LEP) == FALSE) %>% # remove non-english
    select(-c(1:3)) %>%
    rename(area = LEP) %>%
    mutate(geographic_level = "LEP") %>% # rename as lep
    mutate_at(vars(-area, -geographic_level), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = ""))) %>% # convert to blank to avoid error msg
    relocate(area, geographic_level, .before = "2021") %>%
    mutate_at(c(3:8), as.numeric) %>% # Convert to numeric
    group_by(area, geographic_level) %>% # sum for each LSIP
    summarise(across(everything(), list(sum), na.rm = T)) %>%
    rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
    mutate_at(c(3:8), as.character) # Convert to sring to bind

  addLSIP <- addLADU %>%
    left_join(select(C_LADLSIP2020, -LAD21NM), by = c("LAD21CD" = "LAD21CD")) %>%
    filter(is.na(LSIP) == FALSE) %>% # remove non-english
    select(-c(1:3)) %>%
    rename(area = LSIP) %>%
    mutate(geographic_level = "LSIP") %>% # rename as lsip
    mutate_at(vars(-area, -geographic_level), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = ""))) %>% # convert to blank to avoid error msg
    relocate(area, geographic_level, .before = "2021") %>%
    mutate_at(c(3:8), as.numeric) %>% # Convert to numeric
    group_by(area, geographic_level) %>% # sum for each LSIP
    summarise(across(everything(), list(sum), na.rm = T)) %>%
    rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
    mutate_at(c(3:8), as.character) # Convert to sring to bind

  addMCA <- addLADU %>%
    left_join(select(C_mcalookup, -LAD21NM, -CAUTH21CD), by = c("LAD21CD" = "LAD21CD")) %>%
    select(-c(1:3)) %>%
    rename(area = CAUTH21NM) %>%
    mutate(geographic_level = "MCA") %>% # rename as mca
    mutate_at(vars(-area, -geographic_level), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = ""))) %>% # convert to blank to avoid error msg
    relocate(area, geographic_level, .before = "2021") %>%
    mutate_at(c(3:8), as.numeric) %>% # Convert to numeric
    group_by(area, geographic_level) %>% # sum for each MCA
    summarise(across(everything(), list(sum), na.rm = T)) %>%
    rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
    mutate_at(c(3:8), as.character) # Convert to sring to bind

  # get into new LAs
  addNewLADU <- addLADU %>%
    # Use new LA names
    left_join(I_LaLookup %>% select(LAD11CD, LAD21NM, LAD21CD), by = c("LAD21CD" = "LAD11CD")) %>% # make new LAs
    mutate(area = case_when(
      is.na(LAD21NM) == FALSE ~ LAD21NM,
      TRUE ~ area
    )) %>%
    select(-LAD21NM, -LAD21CD, -LAD21CD.y) %>%
    mutate_at(c(3:8), as.numeric) %>%
    group_by(area, geographic_level) %>%
    summarise(across(everything(), list(sum), na.rm = T)) %>%
    rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
    mutate_at(c(3:8), as.character) # Convert to sring to bind

  combined <- bind_rows(addNewLADU, addLEP, addLSIP, addMCA, England) %>%
    melt(id.vars = c("geographic_level", "area")) %>%
    rename(year = variable) %>%
    filter(!is.na(area)) %>%
    relocate(geographic_level, .after = area) %>%
    mutate_at(c(3), as.character) # Convert to sring to bind
}

# format data
F_births_ONS1621 <- format.bussdemo.ONS(I_births_ONS1618, I_births_ONS19, I_births_ONS20, I_births_ONS21) %>%
  rename(births = value)
F_deaths_ONS1621 <- format.bussdemo.ONS(I_deaths_ONS1618, I_deaths_ONS19, I_deaths_ONS20, I_deaths_ONS21) %>%
  rename(deaths = value)
F_active_ONS1621 <- format.bussdemo.ONS(I_active_ONS1618, I_active_ONS19, I_active_ONS20, I_active_ONS21) %>%
  rename(active = value)

# combine dataframes
df_list <- list(F_births_ONS1621, F_deaths_ONS1621, F_active_ONS1621)

D_enterprise_demo1621 <- df_list %>%
  reduce(full_join, by = c("year", "area", "geographic_level"))

# write data to download
write.csv(D_enterprise_demo1621, file = "Data\\AppData\\D_enterprise_demo1621.csv", row.names = FALSE)

# dashboard data
C_enterprise_demo1621 <- df_list %>%
  reduce(full_join, by = c("year", "area", "geographic_level")) %>%
  mutate_at(c(4:6), as.numeric) %>%
  replace(is.na(.), 0) %>%
  mutate(total = rowSums(.[4:6]), births_prop = births / total, active_prop = active / total, deaths_prop = deaths / total) %>%
  select(-c(4:7)) %>%
  rename(births = births_prop, deaths = deaths_prop) %>%
  select(-active_prop) %>%
  melt(id.vars = c("geographic_level", "area", "year")) %>%
  rename(rate = value)

# Put into new format
C_enterpriseBirthDeath <- C_enterprise_demo1621 %>%
  rename(time_period = year, value = rate, metric = variable) %>%
  mutate(
    breakdown = "No breakdowns available", subgroups = "Total",
    metric = case_when(
      metric == "births" ~ "birthRate",
      metric == "deaths" ~ "deathRate"
    ),
    time_period = as.numeric(time_period)
  )


#### 3.4 National Pupil Database ####
#### - Key Stage 4 Destinations ####
# Reshape data to long, rename and reorder and reformat some columns

format.ks4 <- function(x) {
  colnames(x)[1] <- "area"

  addcountry <- x %>%
    select(-location_code, -characteristic, -data_type, -institution_group, -level_methodology) %>%
    select(-c(1:2)) %>%
    mutate_at(c(2:7), as.numeric) %>%
    group_by(time_period) %>% # sum
    summarise(across(everything(), list(sum), na.rm = T)) %>%
    rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
    mutate_at(c(2:7), as.character) %>% # Convert to sring to bind
    mutate(area = "England") %>%
    mutate(geographic_level = "COUNTRY") %>%
    relocate(area, geographic_level, .before = time_period)

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
    group_by(area, geographic_level, time_period) %>% # sum for each LeP
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

  addMCA <- x %>%
    left_join(select(C_mcalookup, -LAD21NM, -CAUTH21CD), by = c("location_code" = "LAD21CD")) %>%
    select(-area, -location_code) %>% # get rid of MCA area
    mutate(geographic_level = "MCA") %>% # rename as MCA
    rename(area = CAUTH21NM) %>%
    relocate(area, .before = geographic_level) %>%
    mutate_at(vars(-time_period, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = ""))) %>% # convert to blank to avoid error msg
    mutate_at(c(8:13), as.numeric) %>% # Convert to numeric
    select(-data_type, -institution_group, -level_methodology, -characteristic) %>%
    group_by(area, geographic_level, time_period) %>% # sum for each LSIP
    summarise(across(everything(), list(sum), na.rm = T)) %>%
    rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
    mutate_at(c(4:9), as.character) %>% # Convert to string to bind
    filter(!is.na(area))

  # join together and rename columns
  LEP_LSIP <- bind_rows(addcountry, addladu, addLEP, addLSIP, addMCA) %>%
    rename(
      "Total" = "cohort",
      "Unknown" = "all_unknown",
      "Not Recorded as a sustained destination" = "all_notsust",
      "Sustained Education" = "education",
      "Sustained Employment" = "all_work",
      "Sustained Apprenticeships" = "appren"
    ) %>%
    relocate("Total", .after = "Unknown") %>%
    relocate(area, .before = geographic_level) %>%
    relocate(time_period, .before = area)
}

## format KS4
F_KS4destin_1521 <- format.ks4(I_KS4destin_1521)

# Downloadable data
D_KS4destin_1521 <- F_KS4destin_1521 %>%
  mutate_at(vars(-time_period, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "c", "\\*" = "u", "~" = "low", "-" = "x")))
# write data to folder
write.csv(D_KS4destin_1521, file = "Data\\AppData\\D_KS4destin_1521.csv", row.names = FALSE)



## National Pupil Database
#### - Key Stage 5 Destinations ####
# Reshape data to long, rename and reorder and reformat some columns

format.ks5 <- function(x) {
  colnames(x)[1] <- "area"

  addcountry <- x %>%
    select(-location_code, -characteristic, -data_type, -institution_group, -level_methodology) %>%
    select(-c(1:2)) %>%
    mutate_at(c(3:8), as.numeric) %>%
    group_by(time_period, cohort_level_group) %>% # sum
    summarise(across(everything(), list(sum), na.rm = T)) %>%
    rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
    mutate_at(c(3:8), as.character) %>% # Convert to sring to bind
    mutate(area = "England") %>%
    mutate(geographic_level = "COUNTRY") %>%
    relocate(area, geographic_level, .before = time_period)


  addladu <- x %>%
    select(-location_code, -characteristic, -data_type, -institution_group, -level_methodology) %>%
    mutate(geographic_level = replace(geographic_level, geographic_level == "localAuthorityDistrict", "LADU")) %>%
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

  addMCA <- x %>%
    left_join(select(C_mcalookup, -LAD21NM, -CAUTH21CD), by = c("location_code" = "LAD21CD")) %>%
    select(-area, -location_code) %>% # get rid of MCA area
    mutate(geographic_level = "MCA") %>% # rename as MCA
    rename(area = CAUTH21NM) %>%
    relocate(area, .before = geographic_level) %>%
    mutate_at(vars(-time_period, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "", "\\*" = "", "~" = "", "-" = ""))) %>% # convert to blank to avoid error msg
    mutate_at(c(9:14), as.numeric) %>% # Convert to numeric
    select(-data_type, -institution_group, -level_methodology, -characteristic) %>%
    group_by(area, geographic_level, time_period, cohort_level_group) %>% # sum for each LSIP
    summarise(across(everything(), list(sum), na.rm = T)) %>%
    rename_with(~ gsub("_1", "", .)) %>% # remove numbers cretaed by the summarise function
    mutate_at(c(5:10), as.character) %>% # Convert to string to bind
    filter(!is.na(area))


  # join together and rename columns
  LEP_LSIP <- bind_rows(addcountry, addladu, addLEP, addLSIP, addMCA) %>%
    rename(
      "Total" = "cohort",
      "Unknown" = "all_unknown",
      "Not Recorded as a sustained destination" = "all_notsust",
      "Sustained Education" = "education",
      "Sustained Employment" = "all_work",
      "Sustained Apprenticeships" = "appren",
      "Cohort Group" = "cohort_level_group"
    ) %>%
    relocate("Total", .after = "Unknown") %>%
    relocate(area, .before = geographic_level) %>%
    relocate(time_period, .before = area)
}

## format KS5
F_KS5destin_1721 <- format.ks5(I_KS5destin_1721)

# downloadable version
D_KS5destin_1721 <- F_KS5destin_1721 %>%
  mutate_at(vars(-time_period, -area, -geographic_level), function(x) str_replace_all(x, c("!" = "c", "\\*" = "u", "~" = "low", "-" = "x")))

# write data to folder
write.csv(D_KS5destin_1721, file = "Data\\AppData\\D_KS5destin_1721.csv", row.names = FALSE)

#### Employment and education ####
C_KS4_KS5eduempapp <- F_KS4destin_1521 %>%
  mutate(`Cohort Group` = "Total") %>%
  mutate(`Key Stage` = "Key Stage 4") %>%
  # filter(time_period == "202021") %>%
  bind_rows(
    F_KS5destin_1721 %>%
      #  filter(time_period == "202021") %>%
      mutate(`Key Stage` = "Key Stage 5")
  ) %>%
  select(-"Unknown", -"Not Recorded as a sustained destination") %>%
  mutate_at(c(4:7), as.numeric) %>%
  mutate(positive_sust_count = `Sustained Employment` + `Sustained Education` + `Sustained Apprenticeships`) %>%
  # add on the new la names
  left_join(I_LaLookup %>% select(LAD11NM, LAD21NM), by = c("area" = "LAD11NM")) %>% # make new LAs
  mutate(area = case_when(
    geographic_level == "LADU" & is.na(LAD21NM) == FALSE ~ LAD21NM,
    TRUE ~ area
  )) %>%
  select(-LAD21NM) %>%
  # summarise for the new LAs
  group_by(time_period, geographic_level, area, `Cohort Group`, `Key Stage`) %>%
  summarise(
    `Sustained Education` = sum(`Sustained Education`),
    `Sustained Apprenticeships` = sum(`Sustained Apprenticeships`),
    `Sustained Employment` = sum(`Sustained Employment`),
    positive_sust_count = sum(positive_sust_count),
    Total = sum(Total)
  ) %>%
  ungroup() %>%
  mutate(rate = positive_sust_count / Total) %>%
  mutate(positive_sust = "Sust edu, emp and app") %>%
  mutate(AY = paste(substr(time_period, 3, 4), "/", substr(time_period, 5, 6), sep = "")) %>%
  mutate(Year = as.numeric(substr(time_period, 3, 4))) # add year name for charts

# write data to folder
write.csv(C_KS4_KS5eduempapp, file = "Data\\AppData\\C_KS4_KS5eduempapp.csv", row.names = FALSE)

# reformat to new format
C_destinationsPreStep1 <- C_KS4_KS5eduempapp %>%
  mutate(metric = case_when(
    `Key Stage` == "Key Stage 4" ~ "sustainedPositiveDestinationKS4Rate",
    `Key Stage` == "Key Stage 5" ~ "sustainedPositiveDestinationKS5Rate",
    TRUE ~ "Error"
  )) %>%
  mutate(breakdown = case_when(
    `Cohort Group` == "Total" ~ "Total",
    TRUE ~ "Level"
  )) %>%
  rename(value = rate, subgroups = `Cohort Group`) %>%
  mutate(
    sustEdu = `Sustained Education` / Total,
    sustApp = `Sustained Apprenticeships` / Total,
    sustEmp = `Sustained Employment` / Total
  ) %>%
  select(-positive_sust, -AY, -Year, -positive_sust_count, -`Sustained Education`, -`Sustained Apprenticeships`, -`Sustained Employment`, -`Key Stage`, -Total)

# add in outcome subgrousps
C_destinations <- bind_rows(
  C_destinationsPreStep1 %>% select(-sustEdu, -sustApp, -sustEmp),
  C_destinationsPreStep1 %>% filter(subgroups == "Total") %>% mutate(breakdown = "Outcome", subgroups = "Sustained education") %>% select(-sustApp, -sustEmp, -value) %>% rename(value = sustEdu),
  C_destinationsPreStep1 %>% filter(subgroups == "Total") %>% mutate(breakdown = "Outcome", subgroups = "Sustained apprenticeship") %>% select(-sustEdu, -sustEmp, -value) %>% rename(value = sustApp),
  C_destinationsPreStep1 %>% filter(subgroups == "Total") %>% mutate(breakdown = "Outcome", subgroups = "Sustained employment") %>% select(-sustApp, -sustEdu, -value) %>% rename(value = sustEmp)
)

# create max and KS5 positive sustained destination  rate by area for use in setting axis
C_KS5_eduempapp_max_min <- C_KS4_KS5eduempapp %>%
  filter(
    `Cohort Group` == "Total",
    `Key Stage` == "Key Stage 5"
  ) %>%
  filter(geographic_level != "LADU") %>%
  # group_by(area) %>%
  summarise(minks5 = min(rate), maxks5 = max(rate))

# write data to folder
write.csv(C_KS5_eduempapp_max_min, file = "Data\\AppData\\C_KS5_eduempapp_max_min.csv", row.names = FALSE)

#### 3.5 ONS by profession ####
#### Employment by occupation ####
format.OnsProf <- function(x) {
  # Tidy
  reformat <- x %>%
    row_to_names(row_number = 4) # set columns
  geogLevel <- colnames(reformat)[1] # get geog level
  reformat <- reformat %>%
    rename(area = geogLevel) %>% # rename to match other datafiles
    mutate(geographic_level = geogLevel) %>% # set to the current geographic type
    # mutate(geographic_level=gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1',geographic_level,perl = TRUE))%>%#get first letter of geog level
    relocate(geographic_level, .before = area) %>%
    rename(
      `Summary Profession Category` = `Summary profession category`,
      `Detailed Profession Category` = `Detailed profession category`
    )

  # Make long
  reformat %>%
    pivot_longer(!c("geographic_level", "area", "Summary Profession Category", "Detailed Profession Category"),
      names_to = "time_period", values_to = "vacancies"
    ) %>%
    mutate(area = case_when(
      area == "Cambridge and Peterborough" ~ "Cambridgeshire and Peterborough",
      area == "Buckinghamshire " ~ "Buckinghamshire",
      area == "North East*" ~ "North East",
      area == "Norfolk and Suffolk " ~ "Norfolk and Suffolk",
      TRUE ~ area
    ))
}

# format data
D_OnsProf <- bind_rows(
  format.OnsProf(I_OnsProfDetailLep), # remove code since it doesn't exist in other geogs
  format.OnsProf(I_OnsProfDetailLsip),
  format.OnsProf(I_OnsProfDetailMca),
  format.OnsProf(I_OnsProfDetailLA %>% rename(region = 1) %>% filter(!(region %in% c("Scotland", "Wales", "Northern Ireland", "Unknown"))) %>% select(-region)), # remove region code
  format.OnsProf(I_OnsProfDetailEng) %>% filter(area == "England"),
  format.OnsProf(I_OnsProfDetailRegion) %>% filter(area == "London") %>% mutate(geographic_level = "LSIP", area = "Greater London"),
  format.OnsProf(I_OnsProfDetailRegion) %>% filter(area == "London") %>% mutate(geographic_level = "LEP"),
  format.OnsProf(I_OnsProfLep %>% mutate(`Detailed Profession Category` = "Detailed profession category")) %>% mutate(`Detailed Profession Category` = "None"),
  format.OnsProf(I_OnsProfLsip %>% mutate(`Detailed Profession Category` = "Detailed profession category")) %>% mutate(`Detailed Profession Category` = "None"),
  format.OnsProf(I_OnsProfMca %>% mutate(`Detailed Profession Category` = "Detailed profession category")) %>% mutate(`Detailed Profession Category` = "None"),
  format.OnsProf(I_OnsProfLA %>% rename(region = 1) %>% filter(!(region %in% c("Scotland", "Wales", "Northern Ireland", "Unknown"))) %>% select(-region) %>% mutate(`Detailed Profession Category` = "Detailed profession category")) %>% mutate(`Detailed Profession Category` = "None"),
  format.OnsProf(I_OnsProfEng %>% mutate(`Detailed Profession Category` = "Detailed profession category")) %>% mutate(`Detailed Profession Category` = "None") %>% filter(area == "England"),
  format.OnsProf(I_OnsProfRegion %>% mutate(`Detailed Profession Category` = "Detailed profession category")) %>% mutate(`Detailed Profession Category` = "None") %>% filter(area == "London") %>% mutate(geographic_level = "LSIP", area = "Greater London"),
  format.OnsProf(I_OnsProfRegion %>% mutate(`Detailed Profession Category` = "Detailed profession category")) %>% mutate(`Detailed Profession Category` = "None") %>% filter(area == "London") %>% mutate(geographic_level = "LEP")
) %>%
  # change lep naming to match other datafiles
  mutate(geographic_level = case_when(
    geographic_level == "Local Authority District" ~ "LADU",
    geographic_level == "Local Enterprise Partnership" ~ "LEP",
    geographic_level == "Local Skills Improvement Plan" ~ "LSIP",
    geographic_level == "Mayoral Combined Authorities" ~ "MCA",
    TRUE ~ geographic_level
  )) %>%
  mutate(geographic_level = toupper(geographic_level))

# make suppressed data zero to use in dashboard
C_OnsProf <- D_OnsProf %>%
  mutate(vacancies = gsub("\\[x\\]", "0", vacancies)) %>%
  mutate(vacancies = gsub("\\[X\\]", "0", vacancies)) %>%
  mutate(vacancies = as.numeric(vacancies))

# make summary profession over time data
C_OnsProfTime <- C_OnsProf %>%
  filter(`Detailed Profession Category` == "None") %>% # limit to only summary data
  group_by(geographic_level, area, `Summary Profession Category`, time_period) %>%
  summarise(vacancies = sum(vacancies)) %>%
  mutate(time_period = as.Date(paste("01 ", time_period, sep = ""), "%d %b %y"))
# write.csv(C_OnsProfTime, file = "Data\\AppData\\C_OnsProfTime.csv", row.names = FALSE)
# make download version
D_OnsProfTime <- C_OnsProfTime %>%
  mutate(vacancies = as.character(vacancies))
D_OnsProfTime["vacancies"][D_OnsProfTime["vacancies"] == 0] <- "[x]"
write.csv(D_OnsProfTime, file = "Data\\AppData\\D_OnsProfTime.csv", row.names = FALSE)

# make download version
D_OnsProfDetail <- D_OnsProf %>%
  filter(time_period == "Dec 22", `Detailed Profession Category` != "None")
write.csv(D_OnsProfDetail, file = "Data\\AppData\\D_OnsProfDetail.csv", row.names = FALSE)

# Working future data ----
C_wf <-
  # bind_rows(
  bind_rows(
    I_wfIndF2 %>%
      select(-"0") %>% # get rid of row caused by formula error in spreadsheet
      rename(subgroup = X1) %>%
      mutate(
        metric = "wfEmployment",
        breakdown = case_when(
          subgroup %in% c("Primary sector and utilities", "Manufacturing", "Construction", "Trade, accomod. and transport", "Business and other services", "Non-marketed services") ~ "Broad sector",
          subgroup == "All industries" ~ "Total",
          TRUE ~ "Industry"
        ),
        subgroup = case_when(
          subgroup == "All industries" ~ "Total",
          TRUE ~ subgroup
        )
      ) %>%
      mutate_at(vars(`2010`:`2035`),
        .funs = funs(. * 1000)
      ), # put in unit numbers
    I_wfOccF2 %>%
      rename(subgroup = thousands) %>%
      mutate(
        metric = "wfEmployment",
        breakdown = case_when(
          grepl("[0-9]", subgroup) == TRUE ~ "Occupation (SOC2020 submajor)",
          subgroup == "All occupations" ~ "Total",
          TRUE ~ "Occupation (SOC2020 major)"
        ),
        subgroup = case_when(
          subgroup == "All occupations" ~ "Total",
          grepl("[0-9]", subgroup) == TRUE ~ gsub("^[0-9]", "", subgroup),
          TRUE ~ subgroup
        )
      ) %>%
      mutate_at(vars(`2010`:`2035`),
        .funs = funs(. * 1000)
      ) %>% # put in unit numbers
      filter(breakdown != "Total"), # remove this total since we already have it fro industry
    I_wfQualF1 %>%
      mutate(metric = "wfEmployment", breakdown = "Qualification") %>%
      rename(subgroup = X1) %>%
      mutate_at(vars(`2010`:`2035`),
        .funs = funs(. * 1000)
      ), # put in unit numbers
  ) %>%
  left_join(I_wfAreaName) %>%
  mutate(geogConcat = paste0(trimws(`.Main`, "both"), " ", sub(" name", "", Scenario))) %>% # get name
  mutate(geogConcat = case_when(
    file_name == "BH_LSIP_MainTables.Main.xlsm" ~ "Brighton and Hove, East Sussex, West Sussex LSIP",
    trimws(`.Main`, "both") == "England" ~ "England",
    TRUE ~ geogConcat
  )) %>% # correct error in file calling this a LEP instead of LSIP and getting name wrong
  select(-`.Main`, -Scenario, -file_name) %>%
  pivot_longer(!c("geogConcat", "breakdown", "subgroup", "metric"),
    names_to = "timePeriod",
    values_to = "value"
  ) %>%
  mutate(geogConcat = case_when(
    geogConcat == "Buckinghamshire Thames Valley LEP" ~ "Buckinghamshire LEP",
    geogConcat == "Cambridge and Peterborough MCA" ~ "Cambridgeshire and Peterborough MCA",
    geogConcat == "London Enterprise Panel LEP" ~ "London LEP",
    geogConcat == "York, North Yorkshire and East Riding LEP" ~ "York and North Yorkshire LEP",
    geogConcat == "Humber LEP" ~ "Hull and East Yorkshire LEP",
    geogConcat == "Essex Southend-on-Sea and Thurrock LSIP" ~ "Essex, Southend-on-Sea and Thurrock LSIP",
    TRUE ~ geogConcat
  )) # correct different spellings

# correct the missing summaries of a few major groups
correctedSubgroups <- C_wf %>%
  # filter(subgroup%in%c("Sales and customer service occupations","Process, plant and machine operatives","Elementary occupations")%>%
  mutate(majorGroup = case_when(
    subgroup %in% c("71 Sales occupations", "72 Customer service occupations") ~ "Sales and customer service occupations",
    subgroup %in% c("81 Process, plant and machine operatives", "82 Transport and mobile machine drivers and operatives") ~ "Process, plant and machine operatives",
    subgroup %in% c("91 Elementary trades and related occupations", "92 Elementary administration and service occupations") ~ "Elementary occupations",
    TRUE ~ "Ignore"
  )) %>%
  filter(majorGroup != "Ignore") %>%
  group_by(majorGroup, metric, geogConcat, timePeriod) %>%
  summarise(value = sum(value)) %>%
  rename(subgroup = majorGroup) %>%
  mutate(breakdown = "Occupation (SOC2020 major)")

# add back on
C_wf <- bind_rows(
  C_wf %>%
    filter(!subgroup %in% c("Sales and customer service occupations", "Process, plant and machine operatives", "Elementary occupations")),
  correctedSubgroups
) %>%
  mutate(subgroup = trimws(gsub("[[:digit:]]+", "", subgroup))) # remove numbers from soc codes for presentation

# Neaten geog files
neatLA <- I_mapLA %>%
  mutate(geog = "LADU") %>% # add geog type
  rename(areaCode = LAD22CD, areaName = LAD22NM) %>% # consistent naming
  # add on lsip, lep and mca groupings
  left_join(I_LEP2020 %>% mutate(LEP = paste0(LEP21NM1, " LEP"), LEP2 = paste0(LEP21NM2, " LEP"), LSIP = paste0(LSIP, " LSIP")) %>% select(LAD21CD, LSIP, LEP, LEP2), by = c("areaCode" = "LAD21CD")) %>%
  left_join(C_mcalookup %>% mutate(MCA = paste0(CAUTH21NM, " MCA")) %>% select(LAD21CD, MCA), by = c("areaCode" = "LAD21CD")) %>%
  filter(is.na(LSIP) == FALSE) # remove non England

neatMCA <- I_mapMCA %>%
  mutate(geog = "MCA") %>% # add geog type
  rename(areaCode = CAUTH21CD, areaName = CAUTH21NM) # consistent naming

neatLEP <- I_mapLEP %>%
  mutate(geog = "LEP") %>% # add geog type
  rename(areaCode = LEP21CD, areaName = LEP21NM) # consistent naming

addEngland <- data.frame(
  areaName = "England", areaCode = "x",
  geog = "COUNTRY"
)

# read in LA shapefile
LAs <- readOGR("./Data/14_LABoundary/Local_Authority_Districts_(May_2022)_UK_BGC_V3.geojson")
# add on LSIPs
LasLsip <- merge(LAs, I_LEP2020 %>% select(LAD21CD, LSIP, LEP = LEP21NM1, LEP2 = LEP21NM2), by.x = "LAD22CD", by.y = "LAD21CD")
# dissolve the LSIP LAs
LSIPsh <- gUnaryUnion(LasLsip, id = LasLsip@data$LSIP)
# turn into GoeJson
LSIPgeojson <- st_as_sf(LSIPsh)
# add on LSIP names
LSIPmap <- bind_cols(LSIPgeojson, C_LEP2020 %>% filter(geographic_level == "LSIP"))
# neaten
neatLSIP <- LSIPmap %>%
  rename(areaName = Area, geog = geographic_level) %>%
  mutate(areaCode = paste0("LSIP", row_number())) %>%
  mutate(
    LONG = map_dbl(geometry, ~ st_centroid(.x)[[1]]),
    LAT = map_dbl(geometry, ~ st_centroid(.x)[[2]])
  )

neatGeog <- bind_rows(neatMCA, neatLEP, addEngland, neatLA, neatLSIP)
# add on data
C_Geog <- neatGeog %>%
  # add employment rate
  left_join(C_EmpRate_APS1822 %>% filter(year == 2022), by = c("areaName" = "area", "geog" = "geographic_level")) %>%
  select(-Year, -year) %>%
  # add achievment rate
  left_join(
    C_Achieve_ILR1621 %>%
      filter(time_period == 202122, level_or_type == "Further education and skills: Total", age_group == "Total") %>%
      select(-Year, -AY, -typeNeat, -time_period) %>%
      mutate(geographic_level = case_when(geographic_level == "National" ~ "COUNTRY", TRUE ~ geographic_level)) %>%
      mutate(geographic_level = case_when(geographic_level == "Local authority district" ~ "LADU", TRUE ~ geographic_level)),
    by = c("areaName" = "area", "geog" = "geographic_level")
  ) %>%
  left_join(
    C_OnsProfTime %>% filter(time_period == "2022-12-01") %>%
      group_by(area, geographic_level) %>%
      summarise(vacancies = sum(vacancies)),
    by = c("areaName" = "area", "geog" = "geographic_level")
  ) %>%
  # add death of entreprises
  left_join(
    C_enterpriseBirthDeath %>% filter(time_period == "2021", metric == "deathRate") %>% select(area, geographic_level, deathRate = value),
    by = c("areaName" = "area", "geog" = "geographic_level")
  ) %>%
  # add birth of entreprises
  left_join(
    C_enterpriseBirthDeath %>% filter(time_period == "2021", metric == "birthRate") %>% select(area, geographic_level, birthRate = value),
    by = c("areaName" = "area", "geog" = "geographic_level")
  ) %>%
  # add enterprise count
  left_join(
    C_enterpriseSizeIndustry %>% filter(time_period == "2022", subgroups == "Total") %>% select(area, geographic_level, enterpriseCount = value),
    by = c("areaName" = "area", "geog" = "geographic_level")
  ) %>%
  # add over level 3
  left_join(
    C_level3Plus %>% filter(time_period == "2021", subgroups == "Total") %>% select(area, geographic_level, level3AndAboveRate = value),
    by = c("areaName" = "area", "geog" = "geographic_level")
  ) %>%
  # add KS4 sustained positive outcome
  left_join(
    C_destinations %>% filter(time_period == "202021", subgroups == "Total", metric == "sustainedPositiveDestinationKS4Rate") %>% select(area, geographic_level, sustainedPositiveDestinationKS4Rate = value),
    by = c("areaName" = "area", "geog" = "geographic_level")
  ) %>%
  # add KS5 sustained positive outcome
  left_join(
    C_destinations %>% filter(time_period == "202021", subgroups == "Total", metric == "sustainedPositiveDestinationKS5Rate") %>% select(area, geographic_level, sustainedPositiveDestinationKS5Rate = value),
    by = c("areaName" = "area", "geog" = "geographic_level")
  ) %>%
  rename_all(~ str_replace_all(., "\\s+", "")) %>%
  mutate(geogConcat = paste0(areaName, " ", geog)) %>%
  mutate(geogConcat = case_when(
    geogConcat == "England COUNTRY" ~ "England",
    TRUE ~ geogConcat
  )) %>%
  left_join(C_wf %>% filter(timePeriod %in% c(2023,2035), breakdown == "Total") %>% 
              select(geogConcat, timePeriod,value)%>%
              # get growth
              arrange(timePeriod) %>%
              group_by(geogConcat) %>%
              mutate(value = (value - lag(value)) / lag(value)) %>%
              filter(timePeriod != 2023)%>%
              select(geogConcat, wfEmployment = value))

save(C_Geog, file = "Data\\AppData\\C_Geog.rdata")

# create neat over time chart
# geographic_level,area,time_period,metric
C_time <- bind_rows(
  # employment data
  C_EmpRate_APS1822 %>%
    mutate(time_period = paste0("Oct-Sep ", year)) %>%
    mutate(chart_year = as.Date(ISOdate(as.numeric(year) - 1, 1, 1))) %>% # shift to start of year
    select(-year) %>%
    pivot_longer(!c("geographic_level", "area", "time_period", "chart_year"),
      names_to = "metric",
      values_to = "value"
    ),
  # ILR data
  C_Achieve_ILR1621 %>%
    mutate(chart_year = as.Date(ISOdate(str_sub(time_period, 1, 4), 1, 1))) %>%
    mutate(time_period = paste0("AY", AY)) %>%
    mutate_at(c("time_period"), as.character) %>%
    filter(level_or_type == "Further education and skills: Total", age_group == "Total") %>%
    select(-apprenticeships_or_further_education, -level_or_type, -age_group, -typeNeat, -AY, -Year) %>%
    mutate(geographic_level = case_when(
      geographic_level == "National" ~ "COUNTRY",
      geographic_level == "Local authority district" ~ "LADU",
      TRUE ~ geographic_level
    )) %>%
    pivot_longer(!c("geographic_level", "area", "time_period", "chart_year"),
      names_to = "metric",
      values_to = "value"
    ),
  C_OnsProfTime %>%
    mutate(time_period = format(as.Date(time_period))) %>%
    mutate(chart_year = as.Date(time_period)) %>%
    mutate(time_period = format(as.POSIXct(time_period), "%b %Y")) %>%
    group_by(area, geographic_level, time_period, chart_year) %>%
    summarise(vacancies = sum(vacancies)) %>%
    mutate(metric = "vacancies") %>%
    rename(value = vacancies),
  # add enterprise birth and deaths
  C_enterpriseBirthDeath %>%
    select(-breakdown, -subgroups) %>%
    mutate(
      chart_year = as.Date(ISOdate(time_period, 1, 1)),
      time_period = paste0("Dec ", time_period - 1, " - Dec ", time_period)
    ),
  # add enterprise count
  C_enterpriseSizeIndustry %>%
    filter(subgroups == "Total") %>% # just get total
    select(-breakdown, -subgroups) %>%
    mutate(
      chart_year = as.Date(ISOdate(time_period, 3, 1)),
      time_period = paste0("Mar ", time_period)
    ),
  # add level 3 + rate
  C_level3Plus %>%
    filter(subgroups == "Total") %>% # just get total
    select(-breakdown, -subgroups) %>%
    mutate(
      chart_year = as.Date(ISOdate(time_period, 1, 1)),
      time_period = as.character(time_period)
    ),
  # add ks4 destinations
  C_destinations %>%
    filter(subgroups == "Total", metric == "sustainedPositiveDestinationKS4Rate") %>% # just get total
    select(-breakdown, -subgroups) %>%
    mutate(
      chart_year = as.Date(ISOdate(str_sub(time_period, 1, 4), 1, 1)),
      time_period = paste0("AY", str_sub(time_period, 3, 4), "/", str_sub(time_period, 5, 6))
    ),
  # add ks5 destinations
  C_destinations %>%
    filter(subgroups == "Total", metric == "sustainedPositiveDestinationKS5Rate") %>% # just get total
    select(-breakdown, -subgroups) %>%
    mutate(
      chart_year = as.Date(ISOdate(str_sub(time_period, 1, 4), 1, 1)),
      time_period = paste0("AY", str_sub(time_period, 3, 4), "/", str_sub(time_period, 5, 6))
    )
) %>%
  mutate(metric = gsub(" ", "", metric)) %>%
  mutate(geogConcat = paste0(area, " ", geographic_level)) %>%
  mutate(geogConcat = case_when(
    geogConcat == "England COUNTRY" ~ "England",
    TRUE ~ geogConcat
  )) %>%
  bind_rows(
    # add working futires
    test <- C_wf %>%
      filter(breakdown == "Total", timePeriod >= 2022) %>%
      mutate(
        chart_year = as.Date(ISOdate(timePeriod, 1, 1)),
        time_period = timePeriod
      ) %>%
      select(geogConcat, value, chart_year, time_period, metric) %>%
      # get growth
      arrange(chart_year, time_period) %>%
      group_by(geogConcat, metric) %>%
      mutate(value = (value - lag(value)) / lag(value)) %>%
      filter(time_period != 2022)
  )

write.csv(C_time, file = "Data\\AppData\\C_time.csv", row.names = FALSE)

# create neat breakdown file
# geographic_level,area,time_period,breakdown,subgroups,metric,value,
D_breakdown <- bind_rows(
  # employment data
  C_EmpRate_APS1822 %>%
    rename(time_period = year) %>%
    filter(time_period == 2022) %>%
    mutate_at(c("time_period"), as.integer) %>%
    select(-Year) %>%
    mutate(breakdown = "No breakdowns available", subgroups = "Total") %>%
    pivot_longer(!c("geographic_level", "area", "time_period", "breakdown", "subgroups"),
      names_to = "metric",
      values_to = "value"
    ) %>%
    filter(metric != "Employment"),
  C_EmpOcc_APS1721 %>%
    rename(time_period = year) %>%
    mutate_at(c("time_period"), as.integer) %>%
    pivot_longer(!c("geographic_level", "area", "time_period"),
      names_to = "subgroups",
      values_to = "value"
    ) %>%
    mutate(breakdown = "Occupation", metric = "Employment") %>%
    mutate_all(~ replace(., is.na(.), 0)) %>%
    group_by(across(c(-value, -subgroups))) %>%
    mutate(across(value, ~ round(prop.table(.), 3))),
  # employment by industry
  C_EmpInd2_APS1822 %>%
    filter(year == "2022") %>%
    rename(time_period = year) %>%
    mutate_at(c("time_period"), as.integer) %>%
    mutate(breakdown = "Industry", metric = "Employment") %>%
    rename(subgroups = variable) %>%
    select(-Total, -rate) %>%
    group_by(across(c(-value, -subgroups))) %>%
    mutate(across(value, ~ round(prop.table(.), 3))),

  # ILR rate data
  C_Achieve_ILR1621 %>%
    filter(time_period == 202021) %>%
    select(-Year, -typeNeat, -AY) %>%
    mutate(geographic_level = case_when(
      geographic_level == "National" ~ "COUNTRY",
      geographic_level == "Local authority district" ~ "LADU",
      TRUE ~ geographic_level
    )) %>%
    pivot_longer(!c("geographic_level", "area", "time_period", "apprenticeships_or_further_education", "level_or_type", "age_group"),
      names_to = "metric",
      values_to = "value"
    ) %>%
    # get subgroups when the other fields are total
    mutate(subgroups = case_when(
      apprenticeships_or_further_education == "Further education and skills" &
        level_or_type == "Further education and skills: Total" ~ age_group,
      apprenticeships_or_further_education == "Further education and skills" &
        age_group == "Total" ~ level_or_type,
      str_sub(level_or_type, -5, -1) == "Total" &
        age_group == "Total" ~ apprenticeships_or_further_education,
      TRUE ~ "NA"
    )) %>%
    # get breakdown when other fields are total
    mutate(breakdown = case_when(
      apprenticeships_or_further_education == "Further education and skills" &
        level_or_type == "Further education and skills: Total" ~ "Age",
      apprenticeships_or_further_education == "Further education and skills" &
        age_group == "Total" ~ "Level",
      str_sub(level_or_type, -5, -1) == "Total" &
        age_group == "Total" ~ "Provision",
      TRUE ~ "NA"
    )) %>%
    filter(breakdown != "NA") %>%
    select(-apprenticeships_or_further_education, -level_or_type, -age_group) %>%
    filter(str_sub(metric, -10, -1) == "population"),
  # get volume FE
  C_Achieve_ILR1621 %>%
    filter(time_period == 202021) %>%
    select(-Year, -typeNeat, -AY) %>%
    mutate(geographic_level = case_when(
      geographic_level == "National" ~ "COUNTRY",
      geographic_level == "Local authority district" ~ "LADU",
      TRUE ~ geographic_level
    )) %>%
    pivot_longer(!c("geographic_level", "area", "time_period", "apprenticeships_or_further_education", "level_or_type", "age_group"),
      names_to = "metric",
      values_to = "value"
    ) %>%
    # get subgroups when the other fields are total
    mutate(subgroups = case_when(
      apprenticeships_or_further_education == "Further education and skills" &
        level_or_type == "Further education and skills: Total" ~ age_group,
      apprenticeships_or_further_education == "Further education and skills" &
        age_group == "Total" ~ level_or_type,
      str_sub(level_or_type, -5, -1) == "Total" &
        age_group == "Total" ~ apprenticeships_or_further_education,
      TRUE ~ "NA"
    )) %>%
    # get breakdown when other fields are total
    mutate(breakdown = case_when(
      apprenticeships_or_further_education == "Further education and skills" &
        level_or_type == "Further education and skills: Total" ~ "Age",
      apprenticeships_or_further_education == "Further education and skills" &
        age_group == "Total" ~ "Level",
      str_sub(level_or_type, -5, -1) == "Total" &
        age_group == "Total" ~ "Provision",
      TRUE ~ "NA"
    )) %>%
    filter(breakdown != "NA") %>%
    select(-apprenticeships_or_further_education, -level_or_type, -age_group) %>%
    filter(str_sub(metric, -10, -1) != "population") %>%
    filter(subgroups != "Total") %>%
    group_by(across(c(-value, -subgroups))) %>%
    mutate(across(value, ~ round(prop.table(.), 3))),
  C_Achieve_ILR21 %>%
    filter(Level == "Total", sex == "Total") %>%
    ungroup() %>%
    select(-Level, -sex, -Enrolments, -Total, -Total_ach, -Total_enr, -pcAch, -pcEnr) %>%
    mutate(metric = "achievements", time_period = 202122, breakdown = "SSA") %>%
    rename(subgroups = SSA, value = Achievements) %>%
    mutate_all(~ replace(., is.na(.), 0)) %>%
    group_by(across(c(-value, -subgroups))) %>%
    mutate(across(value, ~ round(prop.table(.), 3))),
  C_OnsProf %>%
    filter(time_period == "Dec 22", `Detailed Profession Category` == "None") %>%
    mutate(time_period = 122022) %>%
    group_by(geographic_level, area, time_period, `Summary Profession Category`) %>%
    summarise(value = sum(vacancies)) %>%
    mutate(breakdown = "Summary Profession Category", metric = "vacancies") %>%
    rename(subgroups = `Summary Profession Category`) %>%
    group_by(across(c(-value, -subgroups))) %>%
    mutate(across(value, ~ round(prop.table(.), 3))),
  C_OnsProf %>%
    filter(time_period == "Dec 22", `Detailed Profession Category` != "None") %>%
    mutate(time_period = 122022) %>%
    group_by(geographic_level, area, time_period, `Detailed Profession Category`) %>%
    summarise(value = sum(vacancies)) %>%
    mutate(breakdown = "Detailed Profession Category", metric = "vacancies") %>%
    rename(subgroups = `Detailed Profession Category`) %>%
    group_by(across(c(-value, -subgroups))) %>%
    mutate(across(value, ~ round(prop.table(.), 3))),
  # add enterprise birth and deaths
  C_enterpriseBirthDeath %>%
    filter(time_period == 2021),
  # add enterprise count
  C_enterpriseSizeIndustry %>%
    filter(time_period == 2022, subgroups != "Total") %>%
    group_by(across(c(-value, -subgroups))) %>%
    mutate(across(value, ~ round(prop.table(.), 3))),
  # add level 3 + rate
  C_level3Plus %>%
    filter(time_period == 2021, subgroups != "Total"),
  # add ks4 destinations
  C_destinations %>%
    filter(time_period == 202021, subgroups != "Total", metric == "sustainedPositiveDestinationKS4Rate"),
  # add ks5 destinations
  C_destinations %>%
    filter(time_period == 202021, subgroups != "Total", metric == "sustainedPositiveDestinationKS5Rate")
) %>%
  mutate(metric = gsub(" ", "", metric)) %>%
  mutate(geogConcat = paste0(area, " ", geographic_level)) %>%
  mutate(geogConcat = case_when(
    geogConcat == "England COUNTRY" ~ "England",
    TRUE ~ geogConcat
  )) %>%
  # add working futires
  bind_rows(
    C_wf %>% filter(breakdown != "Total",timePeriod %in% c(2023,2035)) %>%
      mutate(area = "none", geographic_level = "none", timePeriod = as.integer(timePeriod)) %>%
      # get growth
      arrange(timePeriod,) %>%
      group_by(geogConcat,subgroup,metric,breakdown) %>%
      mutate(value = (value - lag(value)) / lag(value)) %>%
      filter(timePeriod != 2023)%>%
      rename(subgroups = subgroup, time_period = timePeriod) 
  )

# Create dataHub dataset
# create neat breakdown file for download so keeping symbols
C_datahub <- bind_rows(
  # employment data
  D_EmpRate_APS1822 %>%
    rename(time_period = year) %>%
    mutate(breakdown = "Total", subgroups = "Total", time_period = as.character(time_period)) %>%
    pivot_longer(!c("geographic_level", "area", "time_period", "breakdown", "subgroups"),
      names_to = "metric",
      values_to = "value"
    ),
  D_EmpOcc_APS1721 %>%
    rename(time_period = year) %>%
    pivot_longer(!c("geographic_level", "area", "time_period"),
      names_to = "subgroups",
      values_to = "value"
    ) %>%
    mutate(breakdown = "Occupation", metric = "Employment", time_period = as.character(time_period)),
  # employment by industry
  D_EmpInd_APS1822 %>%
    rename(time_period = year) %>%
    mutate_at(c("time_period"), as.integer) %>%
    pivot_longer(!c("geographic_level", "area", "time_period"),
      names_to = "subgroups",
      values_to = "value"
    ) %>%
    mutate(breakdown = "Industry", metric = "Employment", time_period = as.character(time_period)),
  # ILR rate data
  D_Achieve_ILR1621 %>%
    mutate(geographic_level = case_when(
      geographic_level == "National" ~ "COUNTRY",
      geographic_level == "Local authority district" ~ "LADU",
      TRUE ~ geographic_level
    )) %>%
    pivot_longer(!c("geographic_level", "area", "time_period", "apprenticeships_or_further_education", "level_or_type", "age_group"),
      names_to = "metric",
      values_to = "value"
    ) %>%
    # get subgroups when the other fields are total
    mutate(subgroups = case_when(
      apprenticeships_or_further_education == "Further education and skills" &
        level_or_type == "Further education and skills: Total" ~ age_group,
      apprenticeships_or_further_education == "Further education and skills" &
        age_group == "Total" ~ level_or_type,
      str_sub(level_or_type, -5, -1) == "Total" &
        age_group == "Total" ~ apprenticeships_or_further_education,
      TRUE ~ "NA"
    )) %>%
    # get breakdown when other fields are total
    mutate(breakdown = case_when(
      apprenticeships_or_further_education == "Further education and skills" &
        level_or_type == "Further education and skills: Total" ~ "Age",
      apprenticeships_or_further_education == "Further education and skills" &
        age_group == "Total" ~ "Level",
      str_sub(level_or_type, -5, -1) == "Total" &
        age_group == "Total" ~ "Provision",
      TRUE ~ "NA"
    )) %>%
    filter(breakdown != "NA") %>%
    select(-apprenticeships_or_further_education, -level_or_type, -age_group) %>%
    filter(str_sub(metric, -10, -1) == "population") %>%
    mutate(
      time_period = as.character(time_period),
      breakdown = case_when(
        subgroups == "Total" ~ "Total",
        TRUE ~ breakdown
      )
    ),
  # ssa breakdown
  D_Achieve_ILR21 %>%
    filter(notional_nvq_level == "Total", sex == "Total", ssa_t1_desc != "Total") %>%
    select(-notional_nvq_level, -sex) %>%
    pivot_longer(!c("geographic_level", "area", "time_period", "ssa_t1_desc"),
      names_to = "metric",
      values_to = "value"
    ) %>%
    rename(subgroups = ssa_t1_desc) %>%
    mutate(breakdown = "SSAtier1", time_period = as.character(time_period)),
  # level breakdown
  D_Achieve_ILR21 %>%
    filter(notional_nvq_level != "Total", sex == "Total", ssa_t1_desc == "Total") %>%
    select(-ssa_t1_desc, -sex) %>%
    pivot_longer(!c("geographic_level", "area", "time_period", "notional_nvq_level"),
      names_to = "metric",
      values_to = "value"
    ) %>%
    rename(subgroups = notional_nvq_level) %>%
    mutate(breakdown = "NVQlevel", time_period = as.character(time_period)),
  # gender breakdown
  D_Achieve_ILR21 %>%
    filter(notional_nvq_level == "Total", sex != "Total", ssa_t1_desc == "Total") %>%
    select(-notional_nvq_level, -ssa_t1_desc) %>%
    pivot_longer(!c("geographic_level", "area", "time_period", "sex"),
      names_to = "metric",
      values_to = "value"
    ) %>%
    rename(subgroups = sex) %>%
    mutate(breakdown = "Sex", time_period = as.character(time_period)),
  # totals breakdown
  D_Achieve_ILR21 %>%
    filter(notional_nvq_level == "Total", sex == "Total", ssa_t1_desc == "Total") %>%
    select(-notional_nvq_level, -ssa_t1_desc, -sex) %>%
    pivot_longer(!c("geographic_level", "area", "time_period"),
      names_to = "metric",
      values_to = "value"
    ) %>%
    mutate(breakdown = "Total", subgroups = "Total", time_period = as.character(time_period)),
  C_OnsProf %>% # get totals over time
    filter(`Detailed Profession Category` == "None") %>%
    group_by(area, geographic_level, time_period) %>%
    summarise(vacancies = sum(vacancies)) %>%
    mutate(metric = "vacancies", breakdown = "Total", subgroups = "Total", vacancies = as.character(vacancies)) %>%
    rename(value = vacancies),
  D_OnsProf %>%
    filter(`Detailed Profession Category` == "None", substr(time_period, 1, 3) == "Dec", vacancies != "[x]") %>%
    select(-`Detailed Profession Category`) %>%
    mutate(breakdown = "Summary Profession Category", metric = "vacancies") %>%
    rename(subgroups = `Summary Profession Category`, value = vacancies),
  D_OnsProf %>%
    filter(`Detailed Profession Category` != "None", substr(time_period, 1, 3) == "Dec", vacancies != "[x]") %>%
    select(-`Summary Profession Category`) %>%
    mutate(breakdown = "Detailed Profession Category", metric = "vacancies") %>%
    rename(subgroups = `Detailed Profession Category`, value = vacancies),
  # add enterprise birth and deaths
  D_enterprise_demo1621 %>%
    rename(time_period = year) %>%
    pivot_longer(!c("geographic_level", "area", "time_period"),
      names_to = "metric",
      values_to = "value"
    ) %>%
    mutate(breakdown = "Total", subgroups = "Total", time_period = as.character(time_period), value = as.character(value)),
  # add enterprise count
  C_enterpriseSizeIndustry %>% mutate(
    time_period = as.character(time_period), value = as.character(value),
    breakdown = case_when(
      subgroups == "Total" ~ "Total",
      TRUE ~ breakdown
    )
  ),
  # add level 3 + rate
  C_level3Plus %>% mutate(time_period = as.character(time_period), value = as.character(value)),
  # add ks4 destinations
  C_destinations %>% mutate(time_period = as.character(time_period), value = as.character(value))
) %>%
  mutate(metric = gsub(" ", "", metric)) %>%
  mutate(geogConcat = paste0(area, " ", geographic_level)) %>%
  mutate(geogConcat = case_when(
    geogConcat == "England COUNTRY" ~ "England",
    TRUE ~ geogConcat
  )) %>%
  # add working futires
  bind_rows(
    C_wf %>%
      mutate(time_period = as.character(timePeriod), value = as.character(value)) %>%
      select(-timePeriod) %>%
      rename(subgroups = subgroup)
  ) %>%
  filter(metric != "starts_rate_per_100000_population") %>% # very bad data coverage
  # rename some of the elements so they make sense here
  mutate(metricNeat = case_when(
    metric == "All" ~ "Population volume",
    metric == "empRate" ~ "Employment rate",
    metric == "selfempRate" ~ "Self-employment rate",
    metric == "unempRate" ~ "Unemployment rate",
    metric == "inactiveRate" ~ "Inactive rate",
    metric == "Employment" ~ "Employment volume",
    metric == "InEmployment" ~ "Employment volume",
    metric == "SelfEmployed" ~ "Self-employment volume",
    metric == "Unemployed" ~ "Unemployed volume",
    metric == "Inactive" ~ "Inactive volume",
    metric == "EconomicallyActive" ~ "Economically active volume",
    metric == "Employees" ~ "Employees volume",
    metric == "achievements_rate_per_100000_population" ~ "FE achievement rate per 100,000 population",
    metric == "participation_rate_per_100000_population" ~ "FE participation rate per 100,000 population",
    # metric == "starts_rate_per_100000_population" ~ "FE start rate per 100k",
    metric == "achievements" ~ "FE achievements volume",
    metric == "participation" ~ "FE participation volume",
    metric == "enrolments" ~ "FE enrolled volume",
    metric == "starts" ~ "FE starts volume",
    metric == "births" ~ "Enterprise births",
    metric == "deaths" ~ "Enterprise deaths",
    metric == "active" ~ "Enterprises active",
    metric == "enterpriseCount" ~ "Enterprise count",
    metric == "level3AndAboveRate" ~ "Qualified at Level 3 or above",
    metric == "sustainedPositiveDestinationKS4Rate" ~ "KS4 sustained positive detination rate",
    metric == "sustainedPositiveDestinationKS5Rate" ~ "KS5 sustained positive detination rate",
    metric == "vacancies" ~ "Online job adverts",
    metric == "wfEmployment" ~ "Employment projections (Skills imperative 2035)",
    TRUE ~ metric
  )) %>%
  mutate(breakdown = case_when(
    breakdown == "Occupation" ~ "Occupation split over geography",
    breakdown == "Industry" ~ "Industry split over geography",
    TRUE ~ breakdown
  )) %>%
  select(-area, -geographic_level)
# split in two because too big
index <- seq.int(nrow(C_datahub) / 2)
C_datahubPt1 <- C_datahub[index, ]
C_datahubPt2 <- C_datahub[-index, ]
write.csv(C_datahubPt1, file = "Data\\AppData\\C_datahubPt1.csv", row.names = FALSE)
write.csv(C_datahubPt2, file = "Data\\AppData\\C_datahubPt2.csv", row.names = FALSE)

# Find top ten for each breakdown (these are chosen in the filter)
detailLookup <- D_OnsProfDetail %>% distinct(`Summary Profession Category`, `Detailed Profession Category`)
topTenEachBreakdown <- bind_rows(
  D_breakdown %>%
    filter(geographic_level != "LADU") %>%
    group_by(metric, breakdown, geogConcat) %>%
    arrange(desc(value)) %>%
    slice(1:10) %>%
    mutate(`Summary Profession Category` = "All"),
  D_breakdown %>%
    filter(breakdown == "Detailed Profession Category", geographic_level != "LADU") %>%
    left_join(detailLookup, by = c("subgroups" = "Detailed Profession Category")) %>%
    group_by(metric, breakdown, area, geographic_level, `Summary Profession Category`) %>%
    arrange(desc(value)) %>%
    slice(1:10)
) %>%
  select(metric, breakdown, geogConcat, subgroups, `Summary Profession Category`)
write.csv(topTenEachBreakdown, file = "Data\\AppData\\topTenEachBreakdown.csv", row.names = FALSE)

# Limit to just those with breakdowns, but keep category
C_breakdown <- bind_rows(
  D_breakdown %>%
    filter(breakdown != "No breakdowns available"),
  D_breakdown %>%
    filter(breakdown == "No breakdowns available") %>%
    distinct(metric, breakdown, subgroups)
) %>%
  select(-geographic_level, -area, -time_period)

write.csv(C_breakdown, file = "Data\\AppData\\C_breakdown.csv", row.names = FALSE)

# Tidy up data text table
names(I_DataText) <- gsub(".", " ", names(I_DataText), fixed = TRUE)
write.csv(I_DataText, file = "Data\\AppData\\I_DataText.csv", row.names = FALSE)
