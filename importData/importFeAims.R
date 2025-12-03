### 2.2.2 Achievements/starts/part by LAD and provision, level and age------------
## Download "Geography Region, LA, LAD, PCON - Participation, Achievement by provision type (rates per 100,000 population)" 
## From https://explore-education-statistics.service.gov.uk/find-statistics/further-education-and-skills/2024-25

###  DO ONCE ON RELEASE OF DATA, THEN IGNORE ###

# Read original csv file
#folder <- "2-8_ILRach"
#I_FeProvLevelAge <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))

# Save a copy as RDS
#saveRDS(I_FeProvLevelAge, paste0("./Data/", folder, "/", gsub(".csv", ".RDS", list.files(path = paste0("./Data/", folder)))))

# Remove the csv copy
#file.remove(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder), pattern = "*.csv")))

# Remove the R object, to ensure code runs correctly
#rm(I_FeProvLevelAge)

#################################################

folder <- "2-8_ILRach"
I_FeProvLevelAge <- readRDS(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))

# tidy up data
F_FeProvLevelAge <- I_FeProvLevelAge %>%
  # filter needed columns and rows
  filter(
    geographic_level %in% c("Local authority district", "National", "Local skills improvement plan area","English devolved area"), # just keep area used
    # keep only the combinations shown in dashboard
    (((provision_type == "Further education and skills" | stringr::str_sub(level_or_type, -5, -1) == "Total") & age_summary == "Total" & regulated_status == "Total") | 
       level_or_type == "Further education and skills: Total"),
    lad_code != "z" # ignore Outside of England and unknown
  ) %>%
  mutate(area = case_when(
    geographic_level == "National" ~ country_name,
    geographic_level == "Local authority district" ~ lad_name,
    geographic_level == "Local skills improvement plan area" ~ lsip_name,
    geographic_level == "English devolved area" ~ english_devolved_area_name,
    TRUE ~ ""
  )) %>%
  mutate(areaCode = case_when(
    geographic_level == "Local authority district" ~ lad_code,
    geographic_level == "Local skills improvement plan area" ~ lsip_code,
    geographic_level == "English devolved area" ~ english_devolved_area_code,
    TRUE ~ ""
  )) %>%
  # add dates
  mutate(chartPeriod = paste("AY", substr(time_period, 3, 4), "/", substr(time_period, 5, 6), sep = "")) %>%
  mutate(timePeriod = as.Date(paste("01 Aug", substr(time_period, 1, 4), sep = ""), format = "%d %b %Y")) %>%
  mutate(latest = case_when(
    timePeriod == max(timePeriod) ~ 1,
    timePeriod == (max(timePeriod) - lubridate::years(1)) ~ -1,
    TRUE ~ 0
  )) %>%
  #filter to last 5 years
  filter(timePeriod>=(max(timePeriod) - lubridate::years(4)))%>%
  #rename cols
  rename(achievements = learner_achievements, achievements_rate_per_100000_population = learner_achievements_rate_per_100000_population) %>% 
  #remove unused cols
  select(-regulated_status, -time_identifier, -time_period, -country_code, -country_name, -region_code, -region_name, -new_la_code, -old_la_code, -la_name, -pcon_code, -pcon_name, -lad_code, -lad_name, -english_devolved_area_code, -english_devolved_area_name, -local_enterprise_partnership_code, -local_enterprise_partnership_name, -lsip_code, -lsip_name)

# add on new LADUs/LSIP/CA areas
feWithAreas <- addGeogs(F_FeProvLevelAge)

# Get LSIP and CA groups (ILR now publish at that level). 
# Here we get those LSIPs and CAs which have stayed consistent across the years so we can use the published data throughout
feLsipsCas <- F_FeProvLevelAge %>%
  filter(geographic_level == "Local skills improvement plan area" 
         | (geographic_level == "English devolved area" &
              area %in% c("Cambridgeshire and Peterborough",
                          "Greater London Authority",
                          "Greater Manchester",
                          "Liverpool City Region",
                          "Tees Valley",
                          "West Midlands",
                          "West of England",
                          "West Yorkshire"))
  ) %>%
  mutate(
    geogConcat = case_when(
      geographic_level == "Local skills improvement plan area" ~ paste0(area, " LSIP"),
      geographic_level == "English devolved area" ~ paste0(area, " CA"),
      TRUE ~ "NA"
    ),
    newArea = 0
  ) %>%
  select(-geographic_level, -area, -areaCode)

# group up all the stats for combined areas (including the new LAs)
groupedStats <- feWithAreas %>%
  filter(newArea == 1 # areas that have been calculated
         & (stringr::str_sub(geogConcat, -4, -1) == "LADU" | # lads that have changed over time
              # CAs that have changed their geography over time or are newly formed
              geogConcat %in% c(
                "East Midlands CA",
                "North East CA",
                "South Yorkshire CA",
                "York and North Yorkshire CA",
                "Devon and Torbay CA", "Greater Lincolnshire CA", "Hull and East Yorkshire CA", "Lancashire CA"
              ) |
              # since the ilr only publish the latest 3 years lsip we need to calculate the history of those as well
              (stringr::str_sub(geogConcat, -4, -1) == "LSIP" & !(chartPeriod %in% c("AY22/23","AY23/24", "AY24/25")))
         )) %>%
  ungroup() %>%
  select(-newArea, -achievements_rate_per_100000_population, -starts_rate_per_100000_population, -participation_rate_per_100000_population) %>%
  mutate_at(vars(starts, participation, achievements, population_estimate), safe_numeric) %>% # Convert to numeric
  group_by(provision_type, level_or_type, age_summary, chartPeriod, timePeriod, latest, geogConcat) %>% # sum for each LEP
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) %>%
  mutate(
    starts_rate_per_100000_population = as.character(100000 * starts / population_estimate),
    participation_rate_per_100000_population = as.character(100000 * participation / population_estimate),
    achievements_rate_per_100000_population = as.character(100000 * achievements / population_estimate)
  ) %>%
  mutate(starts = as.character(starts), participation = as.character(participation), achievements = as.character(achievements)) %>%
  select(-population_estimate)

# add back on original LADUs and format
C_FeProvLevelAge <- bind_rows(
  groupedStats,
  feLsipsCas,
  feWithAreas %>%
    filter(newArea == 0)
) %>%
  select(-newArea, -population_estimate) %>%
  # get in new format
  mutate(subgroup = case_when(
    level_or_type == "Further education and skills: Total" & age_summary == "Total" ~ "Total",
    age_summary != "Total" ~ age_summary,
    stringr::str_sub(level_or_type, -5, -1) != "Total" ~ gsub(": Total", "", level_or_type),
    TRUE ~ provision_type
  )) %>%
  mutate(breakdown = case_when(
    level_or_type == "Further education and skills: Total" & age_summary == "Total" ~ "Total",
    age_summary != "Total" ~ "Age",
    stringr::str_sub(level_or_type, -5, -1) != "Total" ~ "Level",
    TRUE ~ "Provision"
  )) %>%
  ungroup() %>%
  select(-provision_type, -level_or_type, -age_summary) %>%
  # make long
  tidyr::pivot_longer(!c("geogConcat", "timePeriod", "chartPeriod", "latest", "breakdown", "subgroup"),
                      names_to = "metric",
                      values_to = "valueText"
  ) %>%
  mutate(value = safe_numeric(valueText)) %>% 
  # Remove invalid value text for NA values, e.g. Inf, NaN, z
  mutate(valueText = ifelse(is.na(value), "NA", valueText))
