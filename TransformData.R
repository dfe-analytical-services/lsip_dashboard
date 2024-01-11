###
# Title: LSIP dashboard data transform
# Author: Hannah Cox/Paul James
# Date: 18th May 2022
# Last updated: 28th March 2023
# Use: Transforms/format the raw data into the dataframes used in the dashboard.
# The dataframes are then saved as .csv in ./Data/AppData to be read by the dashboard app.
# Sources: The files created by running ExtractLoadData.R
# Running time: ~20mins
###

# Load libraries ----
library(tidyverse) # mostly dplyr
library(janitor) # use clean_names()
library(sf) # use st_as_sf st_union sf_use_s2
library(lubridate) # use years
library(writexl) # use write_xlsx

# 1 Geographical data ----
# Clean LSIP names
F_LEP2020 <- I_LEP2020 %>%
  mutate(LSIP23NM = trimws(LSIP23NM))

# Create LAD-LEP lookup table
C_LADLEP2020 <- distinct(F_LEP2020, LAD23CD, LAD23NM, LEP23NM1) %>% # get latest LAD and LEP lookup
  bind_rows(distinct(F_LEP2020 %>% filter(LEP23NM2 != 0), LAD23CD, LAD23NM, LEP23NM2)) # %>%#add on areas which are in the LEP overlaps

# Create LAD-LSIP lookup table
C_LADLSIP2020 <- distinct(F_LEP2020, LAD23CD, LAD23NM, LSIP23NM)

# create LAD-MCA lookup
C_mcalookup <- I_mcalookup %>% select(-ObjectId)

# Neaten geog files
neatLA <- I_mapLA %>%
  select(-LAD23NMW) %>% # remove extra welsh column
  mutate(geog = "LADU") %>% # add geog type
  rename(OBJECTID = FID) %>% # consistent naming
  # add on lsip, lep and mca groupings
  left_join(F_LEP2020 %>% mutate(LEP = paste0(LEP23NM1, " LEP"), LEP2 = paste0(LEP23NM2, " LEP"), LSIP = paste0(LSIP23NM, " LSIP")) %>% select(LAD23CD, LSIP, LEP, LEP2), by = c("LAD23CD" = "LAD23CD")) %>%
  left_join(C_mcalookup %>% mutate(MCA = paste0(CAUTH23NM, " MCA")) %>% select(LAD23CD, MCA), by = c("LAD23CD" = "LAD23CD")) %>%
  filter(is.na(LSIP) == FALSE) %>% # remove non England
  mutate(MCA = case_when(LEP == "The London Economic Action Partnership LEP" ~ "Greater London Authority MCA", TRUE ~ MCA)) %>% # add on gla as mca
  rename(areaName = LAD23NM, areaCode = LAD23CD)

neatMCA <- I_mapMCA %>%
  mutate(geog = "MCA") %>% # add geog type
  rename(areaCode = CAUTH22CD, areaName = CAUTH22NM) # consistent naming

neatLEP <- I_mapLEP %>%
  mutate(geog = "LEP") %>% # add geog type
  rename(areaCode = LEP22CD, areaName = LEP22NM) %>% # consistent naming
  inner_join(distinct(F_LEP2020, LEP23CD1), by = c("areaCode" = "LEP23CD1")) # remove any areas that are no longer LEPs in 2023 (Black Country and Coventry)

addEngland <- data.frame(
  areaName = "England", areaCode = "x",
  geog = "COUNTRY"
)

# add on LSIPs to LA file
LasLsip <- merge(I_mapLA, F_LEP2020 %>% select(LAD23CD, LSIP = LSIP23NM, LEP = LEP23NM1, LEP2 = LEP23NM2), by.x = "LAD23CD", by.y = "LAD23CD")
# dissolve the LSIP LAs
sf_use_s2(F) # to avoid overlapping error
LSIPsh <- LasLsip %>%
  group_by(LSIP) %>%
  summarize(geometry = st_union(geometry))
# turn into GoeJson
LSIPgeojson <- st_as_sf(LSIPsh)

# add on LSIP names
LSIPmap <- bind_cols(LSIPgeojson, F_LEP2020 %>%
  distinct(Area = LSIP23NM) %>%
  arrange(Area) %>%
  mutate(geographic_level = "LSIP") %>%
  mutate(Area = trimws(Area)))
# neaten
neatLSIP <- LSIPmap %>%
  rename(areaName = Area, geog = geographic_level) %>%
  mutate(areaCode = paste0("LSIP", row_number())) %>%
  mutate(
    LONG = map_dbl(geometry, ~ st_centroid(.x)[[1]]),
    LAT = map_dbl(geometry, ~ st_centroid(.x)[[2]])
  )

neatGeog <- bind_rows(
  neatMCA, neatLEP, addEngland, neatLA, neatLSIP,
  neatLEP %>% filter(areaCode == "E37000051") %>% mutate(areaName = "Greater London Authority", geog = "MCA"), # add GLA as an MCA (it isn't officially but people like to find it there)
  neatLA %>%
    filter(LEP2 != "0 LEP") %>%
    select(-LEP) %>%
    rename(LEP = LEP2) # add LAs with more than one LEP
) %>%
  mutate(geogConcat = case_when(
    areaName == "England" ~ "England",
    TRUE ~ paste0(areaName, " ", geog)
  ))

# 2 Data cleaning ----
## 2.0 cleaning functions ----

# convert to data format function
formatLong <- function(x) {
  x %>%
    # make long
    pivot_longer(!c("geogConcat", "timePeriod", "chartPeriod", "latest"),
      names_to = "subgroup",
      values_to = "valueText"
    ) %>%
    mutate(value = as.numeric(valueText)) %>% # for calculations
    mutate_at(vars(valueText), function(x) str_replace_all(x, c("!" = "c", "\\*" = "u", "~" = "low", "-" = "x"))) # common supression notation
}

# Cleaning function for nomis data
formatNomis <- function(x) {
  x %>%
    filter(!(GEOGRAPHY_TYPE == "countries" & GEOGRAPHY_NAME != "England")) %>%
    # Add dates
    mutate(timePeriod = as.Date(paste("01", substr(DATE_NAME, 1, 8), sep = ""), format = "%d %b %Y")) %>%
    mutate(latest = case_when(
      timePeriod == max(timePeriod) ~ 1,
      timePeriod == (max(timePeriod) - years(1)) ~ -1,
      TRUE ~ 0
    )) %>%
    mutate(geogConcat = case_when(
      GEOGRAPHY_TYPE == "local authorities: district / unitary (as of April 2021)" ~ paste0(GEOGRAPHY_NAME, " LADU"),
      GEOGRAPHY_TYPE == "local enterprise partnerships (as of April 2021)" ~ paste0(GEOGRAPHY_NAME, " LEP"),
      TRUE ~ GEOGRAPHY_NAME
    )) %>%
    select(-GEOGRAPHY_TYPE, -GEOGRAPHY_NAME, -GEOGRAPHY_CODE) %>%
    rename(chartPeriod = DATE_NAME, value = OBS_VALUE)
}

## 2.1 Employment volumes ----
# convert into format used in dashboard
F_emp <- formatNomis(I_emp) %>%
  rename(metric = CELL_NAME) %>%
  mutate(metric = gsub(" : All People )", "", metric)) %>%
  mutate(metric = gsub("[[:digit:]]+", "", metric)) %>%
  mutate(metric = gsub("T: \\(Aged - - ", "", metric)) %>%
  mutate(metric = gsub(" ", "", tolower(metric))) %>%
  mutate(breakdown = "Total", subgroup = "Total")
# add rates
C_emp <- bind_rows(
  F_emp %>%
    left_join(F_emp %>% filter(metric == "all") %>% rename(all = value) %>% select(-metric)) %>%
    mutate(value = value / all, metric = paste0(metric, "Rate")) %>%
    filter(metric != "allRate") %>%
    select(-all),
  # original data
  F_emp
) %>%
  mutate(valueText = as.character(value))

## 2.2 Employment by occupation ----
# convert into format used in dashboard
C_empOcc <- formatNomis(I_empOcc) %>%
  rename(subgroup = CELL_NAME) %>%
  mutate(subgroup = gsub("[[:digit:]]+", "", subgroup)) %>%
  mutate(subgroup = gsub(" \\(SOC\\) : All people \\)", "", subgroup)) %>%
  mutate(subgroup = gsub("Tb: \\(All people - ", "", subgroup)) %>%
  mutate(valueText = as.character(value)) %>%
  mutate(breakdown = "Occupation", metric = "inemployment")

## 2.3 Employment by industry ----
C_empInd <- formatNomis(I_empInd) %>%
  rename(subgroup = CELL_NAME) %>%
  mutate(subgroup = gsub("[[:digit:]]+", "", subgroup)) %>%
  mutate(subgroup = gsub(" \\(SIC \\) : All people \\)", "", subgroup)) %>%
  mutate(subgroup = gsub("Ta: ", "", subgroup)) %>%
  mutate(subgroup = gsub("^\\S* ", "", subgroup)) %>% # delete before the first space
  mutate(subgroup = gsub("&", "and", subgroup)) %>%
  mutate(valueText = as.character(value)) %>%
  mutate(breakdown = "Industry", metric = "inemployment")

## 2.4.1 Enterprise by employment size ----
C_entSize <- formatNomis(
  I_entSize %>% rename(subgroup = EMPLOYMENT_SIZEBAND_NAME) %>%
    select(-INDUSTRY_NAME) %>%
    mutate(DATE_NAME = paste0("Mar ", DATE_NAME), CELL_NAME = "enterpriseCount")
) %>%
  rename(metric = CELL_NAME) %>%
  mutate(breakdown = case_when(
    subgroup == "Total" ~ "Total",
    TRUE ~ "Size"
  ))

## 2.4.2 Enterprise by industry ----
C_entInd <- formatNomis(
  I_entInd %>% filter(EMPLOYMENT_SIZEBAND_NAME == "Total") %>%
    select(-EMPLOYMENT_SIZEBAND_NAME) %>%
    mutate(DATE_NAME = paste0("Mar ", DATE_NAME))
) %>%
  rename(subgroup = INDUSTRY_NAME) %>%
  mutate(subgroup = gsub("[[:digit:]]+", "", subgroup)) %>%
  mutate(subgroup = gsub(" : ", "", subgroup)) %>%
  mutate(subgroup = gsub(" \\(.*", "", subgroup)) %>% # delete after first bracket
  mutate(metric = "enterpriseCount", breakdown = "Industry")

## 2.5 Qualification level by age and gender ----
C_qualAgeGender <- formatNomis(I_qualAgeGender) %>%
  mutate(
    metric = case_when(
      grepl("None", CELL_NAME) ~ "qualNone",
      grepl("NVQ1", CELL_NAME) ~ "qualL1",
      grepl("NVQ2", CELL_NAME) ~ "qualL2",
      grepl("Trade Apprenticeships", CELL_NAME) ~ "qualApp",
      grepl("NVQ3", CELL_NAME) ~ "qualL3",
      grepl("NVQ4", CELL_NAME) ~ "qualL4",
      grepl("Other Qualifications", CELL_NAME) ~ "qualOther"
    ),
    subgroup = case_when(
      grepl("All people aged 16-64", CELL_NAME) ~ "Total",
      grepl("Males aged 16-64", CELL_NAME) ~ "Males",
      grepl("Females aged 16-64", CELL_NAME) ~ "Females",
      TRUE ~ substr(CELL_NAME, 33, 37) # get ages
    ),
    breakdown = case_when(
      subgroup %in% c("Males", "Females") ~ "Gender",
      subgroup == "Total" ~ "Total",
      TRUE ~ "Age"
    )
  ) %>%
  select(-CELL_NAME)

# sum all quals for each subgroup
qualSum <- C_qualAgeGender %>%
  group_by(geogConcat, timePeriod, subgroup) %>%
  summarise(allQuals = sum(value, na.rm = T))

# calculate % of peoplw with L3+ (ie (L3+L4)/all quals)
C_qualL3PlusAgeGender <- C_qualAgeGender %>%
  filter(metric %in% c("qualL3", "qualL4")) %>%
  group_by(chartPeriod, timePeriod, geogConcat, breakdown, subgroup, latest) %>%
  summarise(qualL3Plus = sum(value, na.rm = T)) %>%
  left_join(qualSum) %>%
  mutate(metric = "L3PlusRate", value = qualL3Plus / allQuals) %>%
  select(-qualL3Plus, -allQuals) %>%
  mutate(valueText = case_when(value == 0 ~ "c", TRUE ~ as.character(value)))

## 2.6 FE starts/achievements/participation by provision, level, age ----
# tidy up data
F_FeProvLevelAge <- I_FeProvLevelAge %>%
  # filter uneeded columns and rows
  filter(
    geographic_level %in% c("Local authority district", "National", "Local enterprise partnership", "Local skills improvement plan area"), # just keep area used
    # keep only the combinations shown in dashboard
    (((apprenticeships_or_further_education == "Further education and skills" | str_sub(level_or_type, -5, -1) == "Total") & age_group == "Total") | level_or_type == "Further education and skills: Total"),
    lad_code != "z" # ignore Outside of England and unknown
  ) %>%
  mutate(area = case_when(
    geographic_level == "National" ~ country_name,
    geographic_level == "Local authority district" ~ lad_name,
    geographic_level == "Local enterprise partnership" ~ local_enterprise_partnership_name,
    geographic_level == "Local skills improvement plan area" ~ lsip_name
  )) %>%
  mutate(areaCode = case_when(
    geographic_level == "Local authority district" ~ lad_code,
    geographic_level == "Local enterprise partnership" ~ local_enterprise_partnership_code,
    geographic_level == "Local skills improvement plan area" ~ lsip_code,
    TRUE ~ ""
  )) %>%
  # add dates
  mutate(chartPeriod = paste("AY", substr(time_period, 3, 4), "/", substr(time_period, 5, 6), sep = "")) %>%
  mutate(timePeriod = as.Date(paste("01 Aug", substr(time_period, 1, 4), sep = ""), format = "%d %b %Y")) %>%
  mutate(latest = case_when(
    timePeriod == max(timePeriod) ~ 1,
    timePeriod == (max(timePeriod) - years(1)) ~ -1,
    TRUE ~ 0
  )) %>%
  select(-time_identifier, -time_period, -country_code, -country_name, -region_code, -region_name, -new_la_code, -old_la_code, -la_name, -pcon_code, -pcon_name, -lad_code, -lad_name, -english_devolved_area_code, -english_devolved_area_name, -local_enterprise_partnership_code, -local_enterprise_partnership_name, -lsip_code, -lsip_name) %>%
  # find populations at the grouping level so we use the highest volume of population (ie nopt calculate the pop for every small group using small data volumes)
  mutate(populationGroup = case_when(
    apprenticeships_or_further_education %in% c("Apprenticeships", "Community Learning") ~ paste("popIncludesUnder19", age_group), # apps and CL include under 19, so the the population used in the per 100k calcs are slightly higher
    TRUE ~ age_group
  )) %>%
  # ILR uses the LEP names as they were at the time of the data. Here we allign those LEPs whose geography has not changed to the latest name
  mutate(area = case_when(
    area == "Buckinghamshire Thames Valley" ~ "Buckinghamshire",
    area == "Humber" ~ "Hull and East Yorkshire",
    area == "Derby, Derbyshire, Nottingham and Nottinghamshire" ~ "D2N2",
    area == "Gloucestershire" ~ "GFirst",
    area == "Oxfordshire" & geographic_level == "Local enterprise partnership" ~ "OxLEP"
    # and also rename london to the new name
    , area == "London" & geographic_level == "Local enterprise partnership" ~ "The London Economic Action Partnership",
    TRUE ~ area
  ))

# add on population
addPopulation <- F_FeProvLevelAge %>%
  left_join(
    F_FeProvLevelAge %>%
      filter(level_or_type == "Further education and skills: Total" | (level_or_type == "Apprenticeships: Total" & (age_group == "Under 19" | age_group == "Total"))) %>% # use the apps poplation as it is bigger than the CL
      mutate(population = 100000 * as.numeric(participation) / as.numeric(participation_rate_per_100000_population)) %>% # use values to get population
      select(geographic_level, area, populationGroup, timePeriod, population),
    by = c("geographic_level" = "geographic_level", "area" = "area", "populationGroup" = "populationGroup", "timePeriod" = "timePeriod")
  ) %>%
  select(-populationGroup)

# add on new LADUs/LEP/LSIP/MCA areas to all LAs
addGeogs <- function(x) {
  withAreas <- x %>%
    filter(
      geographic_level %in% c("Local authority district", "National")
    ) %>%
    # Use new LA names from 2011 areas
    left_join(I_LaLookup %>% select(LAD11CD, LAD23CD_11 = LAD23CD), by = c("areaCode" = "LAD11CD")) %>% # make new LAs
    # Use new LA names from 2021 areas
    left_join(I_LaLookup %>% select(LAD21CD, LAD23CD_21 = LAD23CD), by = c("areaCode" = "LAD21CD")) %>% # make new LAs
    # create flag for when the lad code has changed
    mutate(
      newArea = case_when(
        (LAD23CD_11 != areaCode) | (LAD23CD_21 != areaCode) ~ 1, TRUE ~ 0
      ),
      areaCode = case_when(
        is.na(LAD23CD_11) == FALSE ~ LAD23CD_11,
        is.na(LAD23CD_21) == FALSE ~ LAD23CD_21,
        TRUE ~ areaCode
      )
    ) %>%
    # select new name
    select(-area, -LAD23CD_11, -LAD23CD_21) %>%
    left_join(distinct(F_LEP2020, LAD23CD, area = LAD23NM), by = c("areaCode" = "LAD23CD")) %>% # use to get consistent LA names
    # add lep names
    left_join(select(C_LADLEP2020, -LAD23NM), by = c("areaCode" = "LAD23CD")) %>%
    # addLSIPS
    left_join(select(C_LADLSIP2020, -LAD23NM), by = c("areaCode" = "LAD23CD")) %>%
    # addMCA
    left_join(select(C_mcalookup, -CAUTH23CD, -LAD23NM), by = c("areaCode" = "LAD23CD")) %>%
    # add national name
    mutate(area = case_when(
      geographic_level == "National" ~ "England",
      TRUE ~ area
    ))

  # make long
  bind_rows(
    withAreas %>%
      mutate(geogConcat = case_when(
        geographic_level == "National" ~ area,
        TRUE ~ paste0(area, " LADU")
      )) %>%
      # group by and slice to remove those LAs that are in multiple LEPs
      group_by(across(c(-area, -LEP23NM1, -LSIP23NM, -CAUTH23NM, -areaCode, -geographic_level, -LEP23NM2))) %>%
      slice(1),
    withAreas %>%
      filter(is.na(LEP23NM1) == FALSE) %>%
      mutate(geogConcat = paste0(LEP23NM1, " LEP"), newArea = 1),
    withAreas %>%
      # group by and slice to remove those LAs that are in multiple LEPs
      group_by(across(c(-area, -LEP23NM1, -LSIP23NM, -CAUTH23NM, -geographic_level, -LEP23NM2))) %>%
      slice(1) %>%
      filter(is.na(LSIP23NM) == FALSE) %>%
      mutate(geogConcat = paste0(LSIP23NM, " LSIP"), newArea = 1),
    withAreas %>%
      # group by and slice to remove those LAs that are in multiple LEPs
      group_by(across(c(-area, -LEP23NM1, -LSIP23NM, -CAUTH23NM, -geographic_level, -LEP23NM2))) %>%
      slice(1) %>%
      filter(is.na(CAUTH23NM) == FALSE) %>%
      mutate(geogConcat = paste0(CAUTH23NM, " MCA"), newArea = 1)
  ) %>%
    select(-area, -LEP23NM1, -LSIP23NM, -CAUTH23NM, -areaCode, -geographic_level, -LEP23NM2)
}

# add on new LADUs/LEP/LSIP/MCA areas
feWithAreas <- addGeogs(addPopulation)

# Get new LSIP and LEP groups (ILR now publish at that level). However, ILR LEP data is published based on the LEP mappings in each year.
# As these mappings have changed each year, we don't have a consistent time series.
# Here we get those LEPs which have stayed consistent across the years so we can use the published data throughout
feLepsLsips <- addPopulation %>%
  filter((geographic_level == "Local enterprise partnership" &
    area %in% c(
      "Buckinghamshire",
      "Cheshire and Warrington",
      "Cornwall and Isles of Scilly",
      "Coventry and Warwickshire",
      "Cumbria",
      "D2N2",
      "Dorset",
      "GFirst",
      "Greater Birmingham and Solihull",
      "Greater Manchester",
      "Heart of the South West",
      "Hull and East Yorkshire",
      "Lancashire",
      "Leicester and Leicestershire",
      "Liverpool City Region",
      "North East",
      "OxLEP",
      "Stoke-on-Trent and Staffordshire",
      "Swindon and Wiltshire",
      "Tees Valley",
      "Thames Valley Berkshire",
      "The Marches",
      "West of England",
      "Worcestershire",
      "South East Midlands"
    )) | geographic_level == "Local skills improvement plan area") %>%
  mutate(
    geogConcat = case_when(
      geographic_level == "Local enterprise partnership" ~ paste0(area, " LEP"),
      geographic_level == "Local skills improvement plan area" ~ paste0(area, " LSIP"),
      TRUE ~ "NA"
    ),
    newArea = 0
  ) %>%
  select(-geographic_level, -area, -areaCode)

# group up all the stats for combined areas (including the new LAs)
groupedStats <- feWithAreas %>%
  filter(newArea == 1 # areas that have been calculated
  & (str_sub(geogConcat, -4, -1) == "LADU" | # lads that have changed over time
      geogConcat %in% c("West of England MCA", "West Midlands MCA") | # MCAs that don't excactly match an LSIP
      # LEPs that have changed their geography over time
      geogConcat %in% c(
        "Coast to Capital LEP",
        "South East LEP",
        "The London Economic Action Partnership LEP",
        "Enterprise M3 LEP",
        "Solent LEP",
        "Greater Lincolnshire LEP",
        "Hertfordshire LEP",
        "York and North Yorkshire LEP",
        "South Yorkshire LEP",
        "Leeds City Region LEP",
        "New Anglia LEP",
        "The Business Board LEP"
      ) |
      # since the ilr only publish the latest years lsip we need to calculate the history of those as well
      (str_sub(geogConcat, -4, -1) == "LSIP" & chartPeriod != "AY22/23")
    )) %>%
  ungroup() %>%
  select(-newArea, -achievements_rate_per_100000_population, -starts_rate_per_100000_population, -participation_rate_per_100000_population) %>%
  mutate_at(vars(starts, participation, achievements, population_estimate), as.numeric) %>% # Convert to numeric
  group_by(apprenticeships_or_further_education, level_or_type, age_group, chartPeriod, timePeriod, latest, geogConcat) %>% # sum for each LEP
  summarise(across(everything(), list(sum), na.rm = T)) %>%
  mutate(
    starts_rate_per_100000_population = as.character(100000 * starts_1 / population_1),
    participation_rate_per_100000_population = as.character(100000 * participation_1 / population_1),
    achievements_rate_per_100000_population = as.character(100000 * achievements_1 / population_1)
  ) %>%
  mutate(starts = as.character(starts_1), participation = as.character(participation_1), achievements = as.character(achievements_1)) %>%
  select(-population_1, -starts_1, -participation_1, -achievements_1, -population_estimate_1)

# add back on original LADUs and format
F_FeProvLevelAge <- bind_rows(
  groupedStats,
  feLepsLsips,
  feWithAreas %>%
    filter(newArea == 0)
) %>%
  select(-population, -newArea, -population_estimate) %>%
  # get in new format
  mutate(subgroup = case_when(
    level_or_type == "Further education and skills: Total" & age_group == "Total" ~ "Total",
    age_group != "Total" ~ age_group,
    str_sub(level_or_type, -5, -1) != "Total" ~ gsub(": Total", "", level_or_type),
    TRUE ~ apprenticeships_or_further_education
  )) %>%
  mutate(breakdown = case_when(
    level_or_type == "Further education and skills: Total" & age_group == "Total" ~ "Total",
    age_group != "Total" ~ "Age",
    str_sub(level_or_type, -5, -1) != "Total" ~ "Level",
    TRUE ~ "Provision"
  )) %>%
  ungroup() %>%
  select(-apprenticeships_or_further_education, -level_or_type, -age_group) %>%
  # make long
  pivot_longer(!c("geogConcat", "timePeriod", "chartPeriod", "latest", "breakdown", "subgroup"),
    names_to = "metric",
    values_to = "valueText"
  ) %>%
  mutate(value = as.numeric(valueText))

# Repeat some LSIPs to represent MCAs with the same geography
C_FeProvLevelAge <- F_FeProvLevelAge %>%
  filter(geogConcat %in% c(
    "Greater Manchester LSIP",
    "South Yorkshire LSIP",
    "West Yorkshire LSIP",
    "Liverpool City Region LSIP",
    "Tees Valley LSIP",
    "Cambridgeshire and Peterborough LSIP",
    "North East LSIP",
    "North of Tyne LSIP"
  )) %>%
  mutate(geogConcat = gsub("LSIP", "MCA", geogConcat)) %>%
  # add back onto other data
  bind_rows(F_FeProvLevelAge)

## 2.7 FE enrolments/achievements by ssa ----
feSsaWithAreas <- I_FeSsa %>%
  filter(notional_nvq_level == "Total", sex == "Total", ethnicity_major == "Total", ssa_t1_desc != "Total") %>%
  mutate(subgroup = ssa_t1_desc, breakdown = "SSA") %>%
  mutate(
    areaCode = case_when(
      geographic_level == "Local authority district" ~ lad_code,
      geographic_level == "National" ~ country_code,
      TRUE ~ "NA"
    ),
    area = case_when(
      geographic_level == "Local authority district" ~ lad_name,
      geographic_level == "National" ~ country_name,
      TRUE ~ "NA"
    )
  ) %>%
  # add dates
  mutate(chartPeriod = paste("AY", substr(time_period, 3, 4), "/", substr(time_period, 5, 6), sep = "")) %>%
  mutate(timePeriod = as.Date(paste("01 Aug", substr(time_period, 1, 4), sep = ""), format = "%d %b %Y")) %>%
  mutate(latest = case_when(
    timePeriod == max(timePeriod) ~ 1,
    timePeriod == (max(timePeriod) - years(1)) ~ -1,
    TRUE ~ 0
  )) %>%
  select(-time_period, -ssa_t1_desc, -notional_nvq_level, -lad_code, -country_code, -lad_name, -country_name, -sex, -ethnicity_major, -english_devolved_area_code, -english_devolved_area_name, -time_identifier, -region_code, -region_name) %>%
  addGeogs()
# group up areas
groupedStats <- feSsaWithAreas %>%
  filter(newArea == 1) %>% # no need to group national or LAs that haven't changed
  ungroup() %>%
  select(-newArea) %>%
  mutate_at(vars(e_and_t_aims_ach, e_and_t_aims_enrolments), as.numeric) %>% # Convert to numeric
  group_by(chartPeriod, timePeriod, latest, geogConcat, subgroup, breakdown) %>% # sum for each LEP
  summarise(across(everything(), list(sum), na.rm = T)) %>%
  mutate(e_and_t_aims_ach = as.character(e_and_t_aims_ach_1), e_and_t_aims_enrolments = as.character(e_and_t_aims_enrolments_1))

# add back on original LADUs and format
C_FeSsa <- bind_rows(
  groupedStats,
  feSsaWithAreas %>%
    filter(newArea == 0)
) %>%
  select(-newArea, -e_and_t_aims_ach_1, -e_and_t_aims_enrolments_1) %>%
  rename(achievements = e_and_t_aims_ach, enrolments = e_and_t_aims_enrolments) %>%
  # make long
  pivot_longer(!c("geogConcat", "timePeriod", "chartPeriod", "latest", "breakdown", "subgroup"),
    names_to = "metric",
    values_to = "valueText"
  ) %>%
  mutate(value = as.numeric(valueText))

## 2.8 Skills imperative 2035 ----
employmentProjections <-
  # bind_rows(
  bind_rows(
    I_wfIndF2 %>%
      select(-"0") %>% # get rid of row caused by formula error in spreadsheet
      rename(subgroup = X1) %>%
      mutate(
        metric = "employmentProjection",
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
        metric = "employmentProjection",
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
      mutate(metric = "employmentProjection", breakdown = "Qualification") %>%
      rename(subgroup = X1) %>%
      mutate_at(vars(`2010`:`2035`),
        .funs = funs(. * 1000)
      ), # put in unit numbers
  ) %>%
  left_join(I_wfAreaName) %>%
  mutate(geogConcat = paste0(trimws(`.Main`, "both"), " ", sub(" name", "", Scenario))) %>% # get name
  mutate(geogConcat = case_when(
    #   file_name == "BH_LSIP_MainTables.Main.xlsm" ~ "Brighton and Hove, East Sussex, West Sussex LSIP",
    trimws(`.Main`, "both") == "England" ~ "England",
    TRUE ~ geogConcat
  )) %>% # correct error in file calling this a LEP instead of LSIP and getting name wrong
  select(-`.Main`, -Scenario, -file_name) %>%
  pivot_longer(!c("geogConcat", "breakdown", "subgroup", "metric"),
    names_to = "timePeriod",
    values_to = "value"
  ) %>%
  mutate(geogConcat = case_when(
    geogConcat == "Sheffield City Region LEP" ~ "South Yorkshire LEP",
    geogConcat == "Derby, Derbyshire, Nottingham and Nottinghamshire LEP" ~ "D2N2 LEP",
    geogConcat == "Oxfordshire LEP" ~ "OxLEP LEP",
    geogConcat == "Gloucestershire LEP" ~ "GFirst LEP",
    geogConcat == "Greater Cambridge and Greater Peterborough LEP" ~ "The Business Board LEP",
    geogConcat == "Buckinghamshire Thames Valley LEP" ~ "Buckinghamshire LEP",
    geogConcat == "Cambridge and Peterborough MCA" ~ "Cambridgeshire and Peterborough MCA",
    geogConcat == "London Enterprise Panel LEP" ~ "The London Economic Action Partnership LEP",
    geogConcat == "York, North Yorkshire and East Riding LEP" ~ "York and North Yorkshire LEP",
    geogConcat == "Humber LEP" ~ "Hull and East Yorkshire LEP",
    geogConcat == "Essex Southend-on-Sea and Thurrock LSIP" ~ "Essex, Southend-on-Sea and Thurrock LSIP",
    geogConcat == "Brighton and Hove East Sussex West Sussex LSIP" ~ "Brighton and Hove, East Sussex, West Sussex LSIP",
    geogConcat == "Norfolk and Suffolk LSIP" ~ "New Anglia LSIP",
    geogConcat == "South East Midlands LSIP" ~ "South-East Midlands LSIP",
    geogConcat == "Gloucestershire LSIP" ~ "G First (Gloucestershire) LSIP",
    geogConcat == "Enterprise M3 LEP (including all of Surrey) LSIP" ~ "Enterprise M3 LSIP",
    TRUE ~ geogConcat
  )) %>% # correct different spellings
  mutate(subgroup = trimws(gsub("[[:digit:]]+", "", subgroup))) %>% # remove numbers from soc codes for presentation
  # get time period
  rename(chartPeriod = timePeriod) %>%
  mutate(timePeriod = as.Date(paste("01", "Jan", chartPeriod, sep = " "), format = "%d %b %Y")) %>%
  mutate(latest = case_when(
    timePeriod == max(timePeriod) ~ 1,
    timePeriod == (max(timePeriod) - years(1)) ~ -1,
    TRUE ~ 0
  ))

# use dorset lep for dorset lsip because dorset lsip has the wrong LAs and it is the same as dorset LEP anyway
# use Worchestershire lsip for Worchestershire lep because Worchestershire lep is missing some LAs and it is the same as Worchestershire lsip anyway
employmentProjectionsCorrections <- employmentProjections %>%
  filter(geogConcat %in% c("Dorset LEP", "Worcestershire LSIP", "Stoke-on-Trent and Staffordshire LSIP")) %>%
  mutate(geogConcat = case_when(
    geogConcat == "Dorset LEP" ~ "Dorset LSIP",
    geogConcat == "Worcestershire LSIP" ~ "Worcestershire LEP",
    geogConcat == "Stoke-on-Trent and Staffordshire LSIP" ~ "Stoke-on-Trent and Staffordshire LEP",
    TRUE ~ "NA"
  ))

# calculate enterprise M3 LSIP from a combination of LEP areas that include Enterprise LSIP area, and then minus off the areas in that larger area that are not Enterprise Lsip
# Enterprise LSIP=(Enterprise LEP + C2C LEP + South east LEP)-(Brighton LSIP + Essex LSIP + Kent LSIP)
employmentProjectionsEntM3Lsip <- employmentProjections %>%
  filter(geogConcat %in% c("Enterprise M3 LEP", "Coast to Capital LEP", "South East LEP", "Brighton and Hove, East Sussex, West Sussex LSIP", "Essex, Southend-on-Sea and Thurrock LSIP", "Kent and Medway LSIP")) %>% # get all the relavant areas
  mutate(value = case_when(
    geogConcat %in% c("Enterprise M3 LEP", "Coast to Capital LEP", "South East LEP") ~ value,
    TRUE ~ 0 - value
  )) %>%
  select(-geogConcat) %>%
  group_by(subgroup, metric, breakdown, chartPeriod, timePeriod, latest) %>%
  summarise(value = sum(value)) %>%
  mutate(geogConcat = "Enterprise M3 LEP (including all of Surrey) LSIP")

# combine them all
employmentProjections <- bind_rows(
  employmentProjections %>% filter(!geogConcat %in% c("Dorset LSIP", "Enterprise M3 LEP (including all of Surrey) LSIP", "Worcestershire LEP", "Stoke-on-Trent and Staffordshire LEP")), # remove incorrect dorset and Enterprise LSIPs
  employmentProjectionsCorrections,
  employmentProjectionsEntM3Lsip
)

# Get future year on year growth metric
empGrowth <- employmentProjections %>%
  filter(chartPeriod >= 2022) %>%
  # get growth
  arrange(chartPeriod, timePeriod, latest) %>%
  group_by(geogConcat, metric, breakdown, subgroup) %>%
  mutate(value = (value - lag(value)) / lag(value)) %>%
  filter(chartPeriod != 2022) %>%
  mutate(metric = "employmentProjectionAnnualGrowth")

# Get 2023 to 2035 growth metric
empGrowth2023_2035 <- employmentProjections %>%
  filter(chartPeriod %in% c(2023, 2035)) %>%
  # get growth
  arrange(chartPeriod, timePeriod, latest) %>%
  group_by(geogConcat, metric, breakdown, subgroup) %>%
  mutate(value = (value - lag(value)) / lag(value)) %>%
  filter(chartPeriod != 2023) %>%
  mutate(metric = "employmentProjectionGrowth2023to2035")

# combine all skills imperative  metrics
C_skillsImperative <- bind_rows(
  employmentProjections,
  empGrowth,
  empGrowth2023_2035
) %>%
  mutate(valueText = as.character(value))

## 2.9 Destinations ----
# create all the other geographical areas based on LAs
destinationsWithAreas <-
  # combine dataset
  bind_rows(
    I_KS4 %>% mutate(metric = "sustainedPositiveDestinationKS4Rate"),
    I_KS5 %>% mutate(metric = "sustainedPositiveDestinationKS5Rate")
  ) %>%
  select(-data_type, -institution_group, -level_methodology, -old_la_code, -opportunity_area_code, -opportunity_area_name, -new_la_code, -la_name, -pcon_code, -pcon_name, -time_identifier, -country_code, -country_name, -region_code, -region_name, -breakdown_topic, -breakdown, -cohort_level) %>% # remove unused columns
  rename(areaCode = lad_code, area = lad_name, timePeriod = time_period) %>%
  # add dates
  mutate(chartPeriod = paste("AY", substr(timePeriod, 3, 4), "/", substr(timePeriod, 5, 6), sep = "")) %>%
  mutate(timePeriod = as.Date(paste("01 Aug", substr(timePeriod, 1, 4), sep = ""), format = "%d %b %Y")) %>%
  mutate(latest = case_when(
    timePeriod == max(timePeriod) ~ 1,
    timePeriod == (max(timePeriod) - years(1)) ~ -1,
    TRUE ~ 0
  )) %>%
  addGeogs()

# For destinations we need to also sum up for the whole country, so we take all LEPs again and relabel country ahead of the sum
destinationsWithAreas <- destinationsWithAreas %>%
  filter(str_sub(geogConcat, -3, -1) == "LEP") %>%
  mutate(geogConcat = "England") %>%
  bind_rows(destinationsWithAreas)

groupedStats <- destinationsWithAreas %>%
  filter(newArea == 1) %>% # no need to group national or LAs that haven't changed
  ungroup() %>%
  select(-newArea) %>%
  group_by(chartPeriod, timePeriod, latest, geogConcat, cohort_level_group, metric) %>% # sum for each LEP
  summarise(across(everything(), list(sum), na.rm = T)) %>%
  rename_with(~ gsub("_1", "", .))

# add back on original LADUs and format
C_destinations <- bind_rows(
  groupedStats,
  destinationsWithAreas %>%
    filter(newArea == 0)
) %>%
  select(-newArea) %>%
  # get metrics
  mutate(
    Total = (education + appren + all_work) / cohort,
    Education = education / cohort,
    Apprenticeships = appren / cohort,
    Employment = all_work / cohort,
  ) %>%
  select(-education, -appren, -all_work, -all_notsust, -all_unknown, -cohort) %>%
  # make long
  pivot_longer(!c("geogConcat", "timePeriod", "chartPeriod", "latest", "cohort_level_group", "metric"),
    names_to = "subgroup",
    values_to = "value"
  ) %>%
  mutate(breakdown = case_when(
    (subgroup == "Total" & (is.na(cohort_level_group) == TRUE | cohort_level_group == "Total")) ~ "Total",
    (subgroup == "Total" & is.na(cohort_level_group) == FALSE) ~ "Level",
    TRUE ~ "Outcome"
  )) %>%
  mutate(subgroup = case_when(breakdown == "Level" ~ cohort_level_group, TRUE ~ subgroup)) %>%
  filter(!(breakdown == "Outcome" & cohort_level_group %in% c("All other qualifications", "Level 2", "Level 3"))) %>%
  ungroup() %>%
  select(-cohort_level_group) %>%
  mutate(valueText = as.character(value))

## 2.10 Job adverts by profession ----
formatVacancies <- function(x) {
  # Tidy
  reformat <- x %>%
    row_to_names(row_number = 4) # set columns
  geogLevel <- colnames(reformat)[1] # get geog level
  reformat <- reformat %>%
    rename(area = geogLevel) %>% # rename to match other datafiles
    mutate(geographic_level = geogLevel) %>% # set to the current geographic type
    relocate(geographic_level, .before = area) %>%
    rename(
      `Summary Profession Category` = `Summary profession category`,
      `Detailed Profession Category` = `Detailed profession category`
    )

  # Make long
  reformat %>%
    pivot_longer(!c("geographic_level", "area", "Summary Profession Category", "Detailed Profession Category"),
      names_to = "time_period", values_to = "valueText"
    ) %>%
    mutate(area = case_when(
      area == "Sheffield City Region" ~ "South Yorkshire",
      area == "Derby, Derbyshire, Nottingham and Nottinghamshire" ~ "D2N2",
      area == "London" ~ "The London Economic Action Partnership",
      area == "Oxfordshire" & geographic_level == "Local Enterprise Partnership" ~ "OxLEP",
      area == "Oxfordshire" & geographic_level == "Local Skills Improvement Plan" ~ "Oxfordshire",
      area == "Gloucestershire" & geographic_level == "Local Enterprise Partnership" ~ "GFirst",
      area == "Gloucestershire" & geographic_level == "Local Skills Improvement Plan" ~ "G First (Gloucestershire)",
      area == "Greater Cambridge and Greater Peterborough" ~ "The Business Board",
      area == "Cambridge and Peterborough" ~ "Cambridgeshire and Peterborough",
      area == "Buckinghamshire " ~ "Buckinghamshire",
      area == "North East*" ~ "North East",
      area == "Norfolk and Suffolk " & geographic_level == "Local Enterprise Partnership" ~ "Norfolk and Suffolk",
      area == "Norfolk and Suffolk " & geographic_level == "Local Skills Improvement Plan" ~ "New Anglia",
      area == "South East Midlands" & geographic_level == "Local Skills Improvement Plan" ~ "South-East Midlands",
      area == "Enterprise M3 LEP (including all of Surrey)" ~ "Enterprise M3",
      TRUE ~ area
    ))
}

# format data
# For the LAs, there are some old LA names within the data so we need to update those
advertsWithAreas <-
  # format and bind both LA files
  formatVacancies(I_OnsProfDetailLA %>% rename(region = 1) %>% filter(!(region %in% c("Scotland", "Wales", "Northern Ireland", "Unknown", "London"))) %>% select(-region)) %>% # remove region code. get rid of london since it isn't really an LA.We get that from the region data
  bind_rows(
    formatVacancies(I_OnsProfLA %>% rename(region = 1) %>% filter(!(region %in% c("Scotland", "Wales", "Northern Ireland", "Unknown", "London"))) %>% select(-region) %>% mutate(`Detailed Profession Category` = "Detailed profession category")) %>% mutate(`Detailed Profession Category` = "None"),
  ) %>%
  # Use new LA names from 2011 areas
  left_join(I_LaLookup %>% select(LAD11NM, LAD23NM_11 = LAD23NM, LAD23CD_11 = LAD23CD), by = c("area" = "LAD11NM")) %>% # make new LAs
  # Use new LA names from 2021 areas
  left_join(I_LaLookup %>% select(LAD21NM, LAD23NM_21 = LAD23NM, LAD23CD_21 = LAD23CD), by = c("area" = "LAD21NM")) %>% # make new LAs
  # create flag for when the lad code has changed
  mutate(
    newArea = case_when(
      (LAD23NM_11 != area) | (LAD23NM_21 != area) ~ 1, TRUE ~ 0
    ),
    area = case_when(
      is.na(LAD23CD_11) == FALSE ~ LAD23NM_11,
      is.na(LAD23CD_21) == FALSE ~ LAD23NM_21,
      TRUE ~ area
    )
  ) %>%
  # select new name
  select(-LAD23CD_11, -LAD23CD_21, -LAD23NM_21, -LAD23NM_11) %>%
  left_join(distinct(F_LEP2020, areaCode = LAD23CD, LAD23NM), by = c("area" = "LAD23NM")) # use to get consistent LA names

# Group up the new LAs
groupedStats <- advertsWithAreas %>%
  filter(newArea == 1) %>% # no need to group national or LAs that haven't changed
  ungroup() %>%
  select(-newArea) %>%
  mutate(valueText = as.numeric(valueText)) %>% # so we can sum
  mutate(timePeriod = as.Date(paste("01 ", time_period, sep = ""), "%d %b %y")) %>%
  filter(timePeriod == max(timePeriod) | `Detailed Profession Category` == "None") %>% # we only show the latest year at this level of detail
  select(-timePeriod) %>%
  group_by(geographic_level, area, `Summary Profession Category`, `Detailed Profession Category`, time_period, areaCode) %>% # sum for each LEP
  summarise(across(everything(), list(sum), na.rm = T)) %>%
  rename_with(~ gsub("_1", "", .)) %>%
  mutate(valueText = as.character(valueText)) # so we can merge

# Format all other area types
F_adverts <- bind_rows(
  formatVacancies(I_OnsProfDetailLep),
  formatVacancies(I_OnsProfDetailLsip),
  formatVacancies(I_OnsProfDetailMca),
  formatVacancies(I_OnsProfDetailEng) %>% filter(area == "England"),
  formatVacancies(I_OnsProfDetailRegion) %>% filter(area == "The London Economic Action Partnership") %>% mutate(geographic_level = "Local Skills Improvement Plan", area = "Greater London"),
  formatVacancies(I_OnsProfDetailRegion) %>% filter(area == "The London Economic Action Partnership") %>% mutate(geographic_level = "Local Enterprise Partnership"),
  formatVacancies(I_OnsProfLep %>% mutate(`Detailed Profession Category` = "Detailed profession category")) %>% mutate(`Detailed Profession Category` = "None"),
  formatVacancies(I_OnsProfLsip %>% mutate(`Detailed Profession Category` = "Detailed profession category")) %>% mutate(`Detailed Profession Category` = "None"),
  formatVacancies(I_OnsProfMca %>% mutate(`Detailed Profession Category` = "Detailed profession category")) %>% mutate(`Detailed Profession Category` = "None"),
  formatVacancies(I_OnsProfEng %>% mutate(`Detailed Profession Category` = "Detailed profession category")) %>% mutate(`Detailed Profession Category` = "None") %>% filter(area == "England"),
  formatVacancies(I_OnsProfRegion %>% mutate(`Detailed Profession Category` = "Detailed profession category")) %>% mutate(`Detailed Profession Category` = "None") %>% filter(area == "The London Economic Action Partnership") %>% mutate(geographic_level = "Local Skills Improvement Plan", area = "Greater London"),
  formatVacancies(I_OnsProfRegion %>% mutate(`Detailed Profession Category` = "Detailed profession category")) %>% mutate(`Detailed Profession Category` = "None") %>% filter(area == "The London Economic Action Partnership") %>% mutate(geographic_level = "Local Enterprise Partnership"),
  # add on LAs
  groupedStats,
  advertsWithAreas %>%
    filter(newArea == 0)
) %>%
  # change lep naming to match other datafiles
  mutate(geogConcat = case_when(
    geographic_level == "Local Authority District" ~ paste0(area, " LADU"),
    geographic_level == "Local Enterprise Partnership" ~ paste0(area, " LEP"),
    geographic_level == "Local Skills Improvement Plan" ~ paste0(area, " LSIP"),
    geographic_level == "Mayoral Combined Authorities" ~ paste0(area, " MCA"),
    TRUE ~ area
  )) %>%
  mutate(timePeriod = as.Date(paste("01 ", time_period, sep = ""), "%d %b %y")) %>%
  rename(chartPeriod = time_period) %>%
  mutate(latest = case_when(
    timePeriod == max(timePeriod) ~ 1,
    timePeriod == (max(timePeriod) - years(1)) ~ -1,
    TRUE ~ 0
  )) %>%
  select(-area, -geographic_level) %>%
  # make suppressed data zero to use in dashboard
  mutate(value = gsub("\\[x\\]", "0", valueText)) %>%
  mutate(value = gsub("\\[X\\]", "0", value)) %>%
  mutate(value = as.numeric(value))

# Get summaries
C_adverts <- bind_rows(
  # detailed
  F_adverts %>%
    filter(`Detailed Profession Category` != "None") %>%
    mutate(breakdown = "Detailed Profession Category") %>%
    rename(subgroup = `Detailed Profession Category`),
  # summary
  F_adverts %>%
    filter(`Detailed Profession Category` == "None") %>%
    mutate(breakdown = "Summary Profession Category") %>%
    rename(subgroup = `Summary Profession Category`),
  # total
  F_adverts %>%
    filter(`Detailed Profession Category` == "None") %>%
    group_by(chartPeriod, timePeriod, geogConcat, latest) %>%
    summarise(value = sum(value)) %>%
    mutate(breakdown = "Total", valueText = as.character(value)) %>%
    mutate(subgroup = "Total")
) %>%
  select(-`Summary Profession Category`, -`Detailed Profession Category`) %>%
  mutate(metric = "vacancies")


# use dorset lep for dorset lsip because dorset lsip has the wrong LAs and it is the same as dorset LEP anyway
C_advertsDorsetLsip <- C_adverts %>%
  filter(geogConcat == "Dorset LEP") %>%
  mutate(geogConcat = "Dorset LSIP") # add in dorset lep as lsip

# calculate enterprise M3 LSIP from a combination of LEP areas that include Enterprise LSIP area, and then minus off the areas in that larger area that are not Enterprise Lsip
# Enterprise LSIP=(Enterprise LEP + C2C LEP + South east LEP)-(Brighton LSIP + Essex LSIP + Kent LSIP)
C_advertsEntM3Lsip <- C_adverts %>%
  filter(geogConcat %in% c("Enterprise M3 LEP", "Coast to Capital LEP", "South East LEP", "Brighton and Hove, East Sussex, West Sussex LSIP", "Essex, Southend-on-Sea and Thurrock LSIP", "Kent and Medway LSIP")) %>% # get all the relavant areas
  mutate(value = case_when(
    geogConcat %in% c("Enterprise M3 LEP", "Coast to Capital LEP", "South East LEP") ~ value,
    TRUE ~ 0 - value
  )) %>%
  select(-geogConcat, -valueText, -newArea) %>%
  group_by(subgroup, metric, breakdown, chartPeriod, timePeriod, latest) %>%
  summarise(value = sum(value)) %>%
  mutate(geogConcat = "Enterprise M3 LEP (including all of Surrey) LSIP")

# combine them all
C_adverts <- bind_rows(
  C_adverts %>% filter(geogConcat != "Dorset LSIP", geogConcat != "Enterprise M3 LEP (including all of Surrey) LSIP"), # remove incorrect dorset and Enterprise LSIPs
  C_advertsDorsetLsip,
  C_advertsEntM3Lsip
)

## 2.11 Business births/deaths  ----
formatBusiness <- function(x, y) {
  colnames(x)[1] <- "areaCode"
  colnames(x)[2] <- "area"
  x %>%
    pivot_longer(!c("areaCode", "area"), names_to = "chartPeriod", values_to = "value") %>%
    mutate(areaCode = trimws(areaCode)) %>% # 2021 file has spaces in the code
    filter(substr(areaCode, 1, 2) == "E0" | areaCode == "E92000001") %>%
    mutate(
      metric = y,
      geographic_level = case_when(areaCode == "E92000001" ~ "National", TRUE ~ "Local authority district"),
      area = case_when(areaCode == "E92000001" ~ "England", TRUE ~ trimws(area))
    )
}

## Business rate fomrat
businessesWithAreas <-
  # combine all datasets
  bind_rows(
    formatBusiness(I_births_ONS1618, "births"),
    formatBusiness(I_births_ONS19, "births"),
    formatBusiness(I_births_ONS20, "births"),
    formatBusiness(I_births_ONS21, "births"),
    formatBusiness(I_deaths_ONS1618, "deaths"),
    formatBusiness(I_deaths_ONS19, "deaths"),
    formatBusiness(I_deaths_ONS20, "deaths"),
    formatBusiness(I_deaths_ONS21, "deaths"),
    formatBusiness(I_active_ONS1618, "active"),
    formatBusiness(I_active_ONS19, "active"),
    formatBusiness(I_active_ONS20, "active"),
    formatBusiness(I_active_ONS21, "active")
  ) %>%
  # add dates
  mutate(timePeriod = as.Date(paste("01", "Jan", chartPeriod, sep = " "), format = "%d %b %Y")) %>%
  mutate(chartPeriod = paste0("Dec ", as.numeric(chartPeriod) - 1, " - Dec ", chartPeriod)) %>%
  mutate(latest = case_when(
    timePeriod == max(timePeriod) ~ 1,
    timePeriod == (max(timePeriod) - years(1)) ~ -1,
    TRUE ~ 0
  )) %>%
  addGeogs()

groupedStats <- businessesWithAreas %>%
  filter(newArea == 1) %>% # no need to group national or LAs that haven't changed
  ungroup() %>%
  select(-newArea) %>%
  group_by(chartPeriod, timePeriod, latest, geogConcat, metric) %>% # sum for each LEP
  summarise(across(everything(), list(sum), na.rm = T)) %>%
  rename_with(~ gsub("_1", "", .))

# add back on original LADUs and format
C_businesses <- bind_rows(
  groupedStats,
  businessesWithAreas %>%
    filter(newArea == 0)
) %>%
  select(-newArea) %>%
  # unpivot to get rates
  pivot_wider(names_from = metric, values_from = value) %>%
  mutate(
    birthRate = births / (births + deaths + active),
    deathRate = deaths / (births + deaths + active)
  ) %>%
  # pivot back
  pivot_longer(!c("chartPeriod", "timePeriod", "latest", "geogConcat"), names_to = "metric", values_to = "value") %>%
  mutate(valueText = as.character(value), breakdown = "Total", subgroup = "Total")

# 3. Combine datasets ----
C_localSkillsDataset <- bind_rows(
  C_emp,
  C_empOcc,
  C_empInd,
  C_entSize,
  C_entInd,
  C_qualAgeGender,
  C_qualL3PlusAgeGender,
  C_FeProvLevelAge,
  C_FeSsa,
  C_skillsImperative,
  C_destinations,
  C_adverts,
  C_businesses
)
## add in GLA as an MCA
C_localSkillsDataset <- bind_rows(
  C_localSkillsDataset # %>%
  #   filter(geogConcat != "Dorset LSIP")
  ,
  C_localSkillsDataset %>%
    filter(geogConcat == "The London Economic Action Partnership LEP") %>%
    mutate(geogConcat = "Greater London Authority MCA") # ,
  # ##add in Dorset LSIP to match Dorset LEP
  # C_localSkillsDataset %>%
  #   filter(geogConcat == "Dorset LEP") %>%
  #   mutate(geogConcat = "Dorset LSIP")
)

# 4. Create datasets used by the app----
## 4.1 Unused metrics ----
# We do not use all the metrics we capture in the data in the dashboard. here we list those we want to ignore
dashboardMetricIgnore <- c("all", "economicallyactive", "employees", "starts_rate_per_100000_population", "starts", "active", "births", "deaths", "qualNone", "qualL1", "qualL2", "qualApp", "qualL3", "qualL4", "qualOther", "employmentProjection")

## 4.2 C_Geog ----
# This is used in the maps. It contains only the latest total data for each metric and area.
C_Geog <- neatGeog %>%
  left_join(
    (C_localSkillsDataset %>%
      filter(
        breakdown == "Total", latest == 1,
        metric != "employmentProjectionAnnualGrowth", # the maps use the employmentProjectionGrowth2023to2035 metric
        !metric %in% dashboardMetricIgnore # remove metrics not used
      ) %>%
      select(value, metric, geogConcat) %>%
      pivot_wider(names_from = metric, values_from = value)),
    by = c("geogConcat" = "geogConcat")
  ) %>%
  rename(employmentProjection = employmentProjectionGrowth2023to2035) %>% # for the emp projections page we use two metrics on different charts. we give them the same name so the filters work
  st_transform(4326) # transform to WG84 that leaflet can plot
save(C_Geog, file = "Data\\AppData\\C_Geog.rdata")

## 4.2 C_time ----
# This is used in the line charts and KPIs. It contains historic data for each metric and area.
C_time <- C_localSkillsDataset %>%
  filter(breakdown == "Total" |
    # We also have a few overview charts that are subgroups-E&T acheivements, app achievments
    (metric == "achievements" & subgroup %in% c("Apprenticeships", "Education and training"))) %>%
  # and also micro businesses
  bind_rows(
    C_localSkillsDataset %>%
      filter(metric == "enterpriseCount", subgroup %in% c("Total", "Micro (0 to 9)")) %>%
      select(-valueText, -breakdown) %>%
      arrange(geogConcat, chartPeriod, timePeriod, latest, metric, subgroup) %>%
      mutate(value = lag(value) / value) %>% # get % micro of total
      filter(subgroup == "Total") %>%
      mutate(metric = "enterprisePctMicro", breakdown = "Total", valueText = as.character(value))
  ) %>%
  filter(
    metric != "employmentProjectionGrowth2023to2035", # time charts only use employmentProjectionAnnualGrowth metric
    !metric %in% dashboardMetricIgnore # remove metrics not used
  ) %>%
  mutate(metric = case_when(
    breakdown != "Total" ~ paste(metric, subgroup),
    TRUE ~ metric
  )) %>% # set metric name to subgroup when we want subgroup data
  select(geogConcat, metric, timePeriod, chartPeriod, latest, value, valueText) %>%
  mutate(metric = case_when(
    metric == "employmentProjectionAnnualGrowth" ~ "employmentProjection",
    TRUE ~ metric
  )) # for the emp projections page we use two metrics on different charts. we give them the same name so the filters work
write.csv(C_time, file = "Data\\AppData\\C_time.csv", row.names = FALSE)

### 4.2.1 Axis min and max ----
# Create max and min for each metric used in setting axis on the overview page
C_axisMinMax <- C_time %>%
  filter(str_sub(geogConcat, -4, -1) != "LADU") %>%
  group_by(metric) %>%
  summarise(minAxis = min(value), maxAxis = max(value)) # , .groups = "drop"
write.csv(C_axisMinMax, file = "Data\\AppData\\C_axisMinMax.csv", row.names = FALSE)

## 4.3 C_breakdown ----
# This is used in the bar chart. It contains the latest data with all splits available.
C_breakdown <- bind_rows(
  # Metric where the proportion needs to be calculated
  C_localSkillsDataset %>%
    filter(
      breakdown != "Total", subgroup != "Total", latest == 1,
      metric %in% c("inemployment", "vacancies", "enterpriseCount", "achievements", "participation", "starts")
    ) %>%
    select(geogConcat, metric, breakdown, subgroup, value) %>%
    mutate_all(~ replace(., is.na(.), 0)) %>%
    group_by(across(c(-value, -subgroup))) %>%
    mutate(across(value, ~ round(prop.table(.), 3))) %>%
    mutate(valueText = as.character(value)),
  # Metric where value is used as it is
  C_localSkillsDataset %>%
    filter(
      breakdown != "Total", subgroup != "Total", latest == 1,
      !metric %in% c("inemployment", "vacancies", "enterpriseCount", "achievements", "participation", "starts")
    ) %>%
    select(geogConcat, metric, breakdown, subgroup, value, valueText)
) %>%
  filter(
    metric != "employmentProjectionAnnualGrowth", # breakdown chart only uses employmentProjectionGrowth2023to2035 metric
    !metric %in% dashboardMetricIgnore # remove metrics not used
  ) %>%
  mutate(metric = case_when(
    metric == "employmentProjectionGrowth2023to2035" ~ "employmentProjection",
    TRUE ~ metric
  )) # for the emp projections page we use two metrics on different charts. we give them the same name so the filters work
write.csv(C_breakdown, file = "Data\\AppData\\C_breakdown.csv", row.names = FALSE)

### 4.3.1 Find top ten for each breakdown ----
# (these are chosen in the filter)
# get a list of summary and detailed professions
C_detailLookup <- advertsWithAreas %>% distinct(`Summary Profession Category`, `Detailed Profession Category`)
write.csv(C_detailLookup, file = "Data\\AppData\\C_detailLookup.csv", row.names = FALSE)

C_topTenEachBreakdown <-
  bind_rows(
    C_breakdown %>%
      filter(str_sub(geogConcat, -4, -1) != "LADU") %>%
      group_by(metric, breakdown, geogConcat) %>%
      arrange(desc(value)) %>%
      slice(1:10)
      %>%
      mutate(`Summary Profession Category` = "All"),
    C_breakdown %>%
      filter(breakdown == "Detailed Profession Category", str_sub(geogConcat, -4, -1) != "LADU") %>%
      left_join(C_detailLookup, by = c("subgroup" = "Detailed Profession Category")) %>%
      group_by(geogConcat, metric, breakdown, `Summary Profession Category`) %>%
      arrange(desc(value)) %>%
      slice(1:10)
  ) %>%
  select(metric, breakdown, geogConcat, subgroup, `Summary Profession Category`)
write.csv(C_topTenEachBreakdown, file = "Data\\AppData\\C_topTenEachBreakdown.csv", row.names = FALSE)

## 4.4 C_dataHub ----
# This is used in the data explorer page
C_datahub <- C_localSkillsDataset %>%
  select(geogConcat, metric, breakdown, subgroup, chartPeriod, valueText, latest) %>%
  filter(!metric %in% c("economicallyactive", "employees", "inemploymentRate", "selfemployedRate", "inactiveRate", "unemployedRate", "starts_rate_per_100000_population", "starts", "enrolments", "employmentProjectionAnnualGrowth", "employmentProjectionGrowth2023to2035", "birthRate", "deathRate", "L3PlusRate")) %>% # very bad data coverage
  # rename some of the elements so they make sense here
  mutate(metricNeat = case_when(
    metric == "all" ~ "Population volume",
    # metric == "inemploymentRate" ~ "Employment rate",
    # metric == "selfemployedRate" ~ "Self-employment rate",
    # metric == "unemployedRate" ~ "Unemployment rate",
    # metric == "inactiveRate" ~ "Inactive rate",
    metric == "inemployment" ~ "Employment volume",
    metric == "selfemployed" ~ "Self-employment volume",
    metric == "unemployed" ~ "Unemployed volume",
    metric == "inactive" ~ "Inactive volume",
    # metric == "economicallyactive" ~ "Economically active volume",
    # metric == "employees" ~ "Employees volume",
    metric == "achievements_rate_per_100000_population" ~ "FE achievement rate per 100,000 population",
    metric == "participation_rate_per_100000_population" ~ "FE participation rate per 100,000 population",
    # metric == "starts_rate_per_100000_population" ~ "FE start rate per 100k",
    metric == "achievements" ~ "FE achievements volume",
    metric == "participation" ~ "FE participation volume",
    metric == "enrolments" ~ "FE enrolled volume",
    # metric == "starts" ~ "FE starts volume",
    metric == "births" ~ "Enterprise births",
    metric == "deaths" ~ "Enterprise deaths",
    metric == "active" ~ "Enterprises active",
    metric == "enterpriseCount" ~ "Enterprise count",
    # metric == "L3PlusRate" ~ "Qualified at Level 3 or above",
    metric == "sustainedPositiveDestinationKS4Rate" ~ "KS4 sustained positive detination rate",
    metric == "sustainedPositiveDestinationKS5Rate" ~ "KS5 sustained positive detination rate",
    metric == "vacancies" ~ "Online job adverts",
    metric == "employmentProjection" ~ "Employment projections",
    metric == "qualNone" ~ "None",
    metric == "qualL1" ~ "NVQ1",
    metric == "qualL2" ~ "NVQ2",
    metric == "qualApp" ~ "Trade apprenticeship",
    metric == "qualL3" ~ "NVQ3",
    metric == "qualL4" ~ "NVQ4",
    metric == "qualOther" ~ "Other qualification",
    TRUE ~ metric
  )) %>%
  select(Area = geogConcat, metric, metricNeat, Breakdown = breakdown, Subgroup = subgroup, Period = chartPeriod, valueText, latest)
# Because adverts has so much data, we limit to splits only for the latest year
C_datahubLimitAds <- bind_rows(
  C_datahub %>%
    filter(metric == "vacancies" & Breakdown == "Total"),
  C_datahub %>%
    filter(metric == "vacancies" & Breakdown != "Total" & latest == 1),
  C_datahub %>%
    filter(metric != "vacancies")
) %>% select(-latest)
write.csv(C_datahubLimitAds, file = "Data\\AppData\\C_datahub.csv", row.names = FALSE)

## 4.5 Core indicator download ----
list_of_datasets0 <- list(
  "1a.Employment by occupation" = C_datahub %>%
    filter(metric == "inemployment", Breakdown == "Occupation") %>%
    select(-metric, -metricNeat, -Breakdown) %>%
    rename("Employment volume" = valueText, Occupation = Subgroup),
  "1b.Employment volumes" = C_datahub %>%
    filter(metric %in% c("all", "inemployment", "selfemployed", "unemployed", "inactive"), Breakdown != "Occupation", Breakdown != "Industry") %>%
    select(-metric, -Breakdown, -Subgroup) %>%
    rename("Volume" = valueText, Metric = metricNeat),
  "1c.Employment by industry" = C_datahub %>%
    filter(metric == "inemployment", Breakdown == "Industry") %>%
    select(-metric, -metricNeat, -Breakdown, Industry = Subgroup) %>%
    rename("Employment volume" = valueText),
  "2a.Job adverts" = C_datahub %>%
    filter(metric == "vacancies", Breakdown == "Total") %>%
    select(-metric, -metricNeat, -Breakdown, -Subgroup) %>%
    rename("Online job adverts" = valueText),
  "2b.Job adverts by profession" = C_datahub %>%
    filter(metric == "vacancies", Breakdown != "Total", latest == 1) %>%
    select(-metric, -metricNeat) %>%
    rename("Online job adverts" = valueText, "Detailed/Summary" = Breakdown, Profession = Subgroup),
  "3a.FE achievements by SSA" = C_datahub %>%
    filter(metric == "achievements", Breakdown == "SSA") %>%
    select(-metric, -metricNeat, -Breakdown) %>%
    rename(Achievements = valueText, "Sector subject area tier 1" = Subgroup),
  "3b.FE achievement&participation" = C_datahub %>%
    filter(metric %in% c("achievements", "participation"), Breakdown != "SSA") %>%
    select(-metric) %>%
    rename(Volume = valueText, Metric = metricNeat),
  "4.Highest qualification" = C_datahub %>%
    filter(metric %in% c("qualNone", "qualL1", "qualL2", "qualApp", "qualL3", "qualL4", "qualOther")) %>%
    select(-metric) %>%
    rename("16-64 year olds" = valueText, "Highest qualification" = metricNeat),
  "5a.Enterprises by size" = C_datahub %>%
    filter(metric == "enterpriseCount", Breakdown != "Industry") %>%
    select(-metric, -metricNeat, -Breakdown) %>%
    rename("Enterprise count" = valueText, "Size band" = Subgroup),
  "5b.Enterprises by industry" = C_datahub %>%
    filter(metric == "enterpriseCount", Breakdown != "Size") %>%
    select(-metric, -metricNeat, -Breakdown) %>%
    rename("Enterprise count" = valueText, Industry = Subgroup),
  "5c.Enterprise demography" = C_datahub %>%
    filter(metric %in% c("births", "deaths", "active")) %>%
    select(-metric, -Breakdown, -Subgroup) %>%
    rename("Enterprise count" = valueText, Metric = metricNeat),
  "6a.Key Stage 4 destinations" = C_datahub %>%
    filter(metric == "sustainedPositiveDestinationKS4Rate") %>%
    select(-metric, -metricNeat, -Breakdown) %>%
    rename("KS4 sustained positive destination rate" = valueText, Outcome = Subgroup),
  "6b.Key Stage 5 destinations" = C_datahub %>%
    filter(metric == "sustainedPositiveDestinationKS5Rate") %>%
    select(-metric, -metricNeat) %>%
    rename("KS5 sustained positive destination rate" = valueText),
  "7.Projected employment" = C_datahub %>%
    filter(metric == "employmentProjection") %>%
    select(-metric, -metricNeat) %>%
    rename("Projected employment" = valueText)
)
write_xlsx(list_of_datasets0, "Data\\AppData\\CoreIndicators.xlsx")

# 5 Tidy up the app text ----
# Tidy up data table
names(I_DataTable) <- gsub(".", " ", names(I_DataTable), fixed = TRUE)
write.csv(I_DataTable, file = "Data\\AppData\\I_DataTable.csv", row.names = FALSE)

# Tidy up intervention table - not currently used
# names(I_InterventionTable) <- gsub(".", " ", names(I_InterventionTable), fixed = TRUE)
# write.csv(I_InterventionTable, file = "Data\\AppData\\I_InterventionTable.csv", row.names = FALSE)

# Tidy up sources table
names(I_SourcesTable) <- gsub(".", " ", names(I_SourcesTable), fixed = TRUE)
write.csv(I_SourcesTable, file = "Data\\AppData\\I_SourcesTable.csv", row.names = FALSE)
names(I_ToolsTable) <- gsub(".", " ", names(I_ToolsTable), fixed = TRUE)
write.csv(I_ToolsTable, file = "Data\\AppData\\I_ToolsTable.csv", row.names = FALSE)

# Tidy up data text table
names(I_DataText) <- gsub(".", " ", names(I_DataText), fixed = TRUE)
write.csv(I_DataText, file = "Data\\AppData\\I_DataText.csv", row.names = FALSE)
