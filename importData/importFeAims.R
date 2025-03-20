### 2.2.2 Achievements/starts/part by LAD and provision, level and age------------
## Download "Further education and skills geography - detailed summary " from https://explore-education-statistics.service.gov.uk/data-catalogue/further-education-and-skills/2021-22
folder <- "2-8_ILRach"
I_FeProvLevelAge <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))

# tidy up data
F_FeProvLevelAge <- I_FeProvLevelAge %>%
  # filter uneeded columns and rows
  filter(
    geographic_level %in% c("Local authority district", "National", "Local skills improvement plan area","English devolved area"), # just keep area used
    # keep only the combinations shown in dashboard
    (((provision_type == "Further education and skills" | stringr::str_sub(level_or_type, -5, -1) == "Total") & age_summary == "Total") | level_or_type == "Further education and skills: Total"),
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
  select(-time_identifier, -time_period, -country_code, -country_name, -region_code, -region_name, -new_la_code, -old_la_code, -la_name, -pcon_code, -pcon_name, -lad_code, -lad_name, -english_devolved_area_code, -english_devolved_area_name, -local_enterprise_partnership_code, -local_enterprise_partnership_name, -lsip_code, -lsip_name) %>%
  # find populations at the grouping level so we use the highest volume of population (ie not calculate the pop for every small group using small data volumes)
  mutate(populationGroup = case_when(
    provision_type %in% c("Apprenticeships", "Community Learning") ~ paste("popIncludesUnder19", age_summary), # apps and CL include under 19, so the the population used in the per 100k calcs are slightly higher
    TRUE ~ age_summary
  )) %>%
  # ILR uses the LEP names as they were at the time of the data. Here we align those LEPs whose geography has not changed to the latest name
  mutate(area = case_when(
    area == "Buckinghamshire Thames Valley" ~ "Buckinghamshire",
    area == "Humber" ~ "Hull and East Yorkshire",
    area == "Derby, Derbyshire, Nottingham and Nottinghamshire" ~ "D2N2",
    area == "Gloucestershire" ~ "GFirst",
    TRUE ~ area
  ))

# add on population
addPopulation <- F_FeProvLevelAge %>%
  left_join(
    F_FeProvLevelAge %>%
      filter(level_or_type == "Further education and skills: Total" | (level_or_type == "Apprenticeships: Total" & (age_summary == "Under 19" | age_summary == "Total"))) %>% # use the apps poplation as it is bigger than the CL
      mutate(population = 100000 * as.numeric(participation) / as.numeric(participation_rate_per_100000_population)) %>% # use values to get population
      select(geographic_level, area, populationGroup, timePeriod, population),
    by = c("geographic_level" = "geographic_level", "area" = "area", "populationGroup" = "populationGroup", "timePeriod" = "timePeriod")
  ) %>%
  select(-populationGroup)

# add on new LADUs/LSIP/MCA areas
feWithAreas <- addGeogs(addPopulation)

# Get LSIP and MCA groups (ILR now publish at that level). 
# Here we get those LSIPs and MCAs which have stayed consistent across the years so we can use the published data throughout
feLepsLsips <- addPopulation %>%
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
      geographic_level == "English devolved area" ~ paste0(area, " MCA"),
      TRUE ~ "NA"
    ),
    newArea = 0
  ) %>%
  select(-geographic_level, -area, -areaCode)

# group up all the stats for combined areas (including the new LAs)
groupedStats <- feWithAreas %>%
  filter(newArea == 1 # areas that have been calculated
         & (stringr::str_sub(geogConcat, -4, -1) == "LADU" | # lads that have changed over time
              # MCAs that have changed their geography over time or are newly formed
              geogConcat %in% c(
                "East Midlands MCA",
                "North East MCA",
                "South Yorkshire MCA",
                "York and North Yorkshire MCA"
              ) |
              # since the ilr only publish the latest 2 years lsip we need to calculate the history of those as well
              (stringr::str_sub(geogConcat, -4, -1) == "LSIP" & !(chartPeriod %in% c("AY22/23","AY23/24")))
         )) %>%
  ungroup() %>%
  select(-newArea, -achievements_rate_per_100000_population, -starts_rate_per_100000_population, -participation_rate_per_100000_population) %>%
  mutate_at(vars(starts, participation, achievements, population_estimate), as.numeric) %>% # Convert to numeric
  group_by(provision_type, level_or_type, age_summary, chartPeriod, timePeriod, latest, geogConcat) %>% # sum for each LEP
  summarise(across(everything(), list(sum), na.rm = T)) %>%
  mutate(
    starts_rate_per_100000_population = as.character(100000 * starts_1 / population_1),
    participation_rate_per_100000_population = as.character(100000 * participation_1 / population_1),
    achievements_rate_per_100000_population = as.character(100000 * achievements_1 / population_1)
  ) %>%
  mutate(starts = as.character(starts_1), participation = as.character(participation_1), achievements = as.character(achievements_1)) %>%
  select(-population_1, -starts_1, -participation_1, -achievements_1, -population_estimate_1)

# add back on original LADUs and format
C_FeProvLevelAge <- bind_rows(
  groupedStats,
  feLepsLsips,
  feWithAreas %>%
    filter(newArea == 0)
) %>%
  select(-population, -newArea, -population_estimate) %>%
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
  mutate(value = as.numeric(valueText))