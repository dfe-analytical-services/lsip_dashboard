### 2.2.1 Achievements by SSAt1, LAD, gender, level------------
folder <- "2-7_ILRachSSA"
I_FeSsa <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))

feSsaWithAreas <- I_FeSsa %>%
  filter(notional_nvq_level == "Total", sex == "Total", ethnicity_major == "Total") %>%
  mutate(
    subgroup = ssa_t1_desc,
    breakdown = case_when(
      subgroup == "Total" ~ "Total",
      TRUE ~ "SSA"
    )
  ) %>%
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
    timePeriod == (max(timePeriod) - lubridate::years(1)) ~ -1,
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
  rename(achievementsAims = e_and_t_aims_ach, enrolmentsAims = e_and_t_aims_enrolments) %>%
  # make long
  tidyr::pivot_longer(!c("geogConcat", "timePeriod", "chartPeriod", "latest", "breakdown", "subgroup"),
               names_to = "metric",
               values_to = "valueText"
  ) %>%
  mutate(value = as.numeric(valueText))
