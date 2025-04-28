### 2.2.3 KS4 destinations----
# National pupil database
folder <- "2-9_KS4destin"
I_KS4 <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))

### 2.2.4 KS5 destinations----
folder <- "2-10_KS5destin"
I_KS5 <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))

# create all the other geographical areas based on LAs
destinationsWithAreas <-
  # combine dataset
  bind_rows(
    I_KS4 %>% mutate(metric = "sustainedPositiveDestinationKS4Rate")%>%filter(institution_group=="State-funded mainstream schools"),#just schools to match the headline stats in the publication
    I_KS5 %>% mutate(metric = "sustainedPositiveDestinationKS5Rate")%>%filter(institution_group=="State-funded mainstream schools & colleges",level_methodology=="16-18 Provider location")
  ) %>%
  filter(data_type=="Number of pupils",breakdown_topic=="Total")%>%
  select(-data_type, -institution_group, -institution_type, -version, -level_methodology, -old_la_code, -opportunity_area_code, -opportunity_area_name, -new_la_code, -la_name, -pcon_code, -pcon_name, -time_identifier, -country_code, -country_name, -region_code, -region_name, -breakdown_topic, -breakdown, -cohort_level,-overall, -fe, -ssf,-sfc,-other_edu,-appl3,-appl2,-appl4,-he,-fel1,-fel2,-fel3) %>% # remove unused columns
  rename(areaCode = lad_code, area = lad_name, timePeriod = time_period) %>%
  # add dates
  mutate(chartPeriod = paste("AY", substr(timePeriod, 3, 4), "/", substr(timePeriod, 5, 6), sep = "")) %>%
  mutate(timePeriod = as.Date(paste("01 Aug", substr(timePeriod, 1, 4), sep = ""), format = "%d %b %Y")) %>%
  mutate(latest = case_when(
    timePeriod == max(timePeriod) ~ 1,
    timePeriod == (max(timePeriod) - lubridate::years(1)) ~ -1,
    TRUE ~ 0
  )) %>%
  #filter to last 5 years
  filter(timePeriod>=(max(timePeriod) - lubridate::years(4)))%>%
  mutate_at(c('cohort','education','appren','all_work','all_notsust','all_unknown'), as.numeric)%>%#convert to numeric to sum
  addGeogs()

# For destinations we need to also sum up for the whole country, so we take all LEPs again and relabel country ahead of the sum
destinationsEngland <- destinationsWithAreas %>%
  filter(stringr::str_sub(geogConcat, -3, -1) == "LEP") %>%
  mutate(geogConcat = "England") %>%
  bind_rows(destinationsWithAreas)#%>%

groupedStats <- destinationsEngland %>%
  filter(newArea == 1) %>% # no need to group national or LAs that haven't changed
  ungroup() %>%
  select(-newArea) %>%
  group_by(chartPeriod, timePeriod, latest, geogConcat, cohort_level_group, metric) %>% # sum for each LEP
  summarise(across(everything(), list(sum), na.rm = T)) %>%
  rename_with(~ gsub("_1", "", .))

# add back on original LADUs and format
C_destinations <- bind_rows(
  groupedStats,
  destinationsEngland %>%
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
  tidyr::pivot_longer(!c("geogConcat", "timePeriod", "chartPeriod", "latest", "cohort_level_group", "metric"),
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