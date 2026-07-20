### 2.2.3 KS4 destinations----
ks4_raw <- read_csv(
  "https://api.education.gov.uk/statistics/v1/data-sets/019d4f41-22d1-71b2-a1a7-f3b91026815b/csv"
) |> 
  filter(establishment_type_group=="State-funded mainstream schools") |>  #just schools to match the headline stats in the publication
  mutate(metric="sustainedPositiveDestinationKS4Rate", cohort_level_group ="Total")

### 2.2.4 KS5 destinations----
ks5_raw <- read_csv(
  "https://api.education.gov.uk/statistics/v1/data-sets/019d4e73-6440-7523-b60c-bfab1ad4a30d/csv"
) |> 
  filter(establishment_type_group=="State-funded mainstream schools & colleges",
         cohort_level=="Total") |> 
  mutate(metric="sustainedPositiveDestinationKS5Rate")

# create all the other geographical areas based on LAs
destinationsWithAreas<-bind_rows(ks4_raw,ks5_raw) |>  
  filter(disadvantage_status=="Total",
         ethnicity_major=="Total",
         breakdown_topic=="Total",
         sex=="Total",
         (destination_description == "Sustained education, employment & apprenticeships" |
            (destination_description %in% c("Sustained education destination",
                                            "Sustained apprenticeships",
                                            "Sustained employment destination") & cohort_level_group =="Total")),
         (is.na(lad_code)==FALSE | geographic_level=="National"),
         time_period>=max(time_period)-404#only latest 5 years of data
  ) |> 
  select(timePeriod = time_period,geographic_level,areaCode = lad_code,area = lad_name,cohort_level_group,destination_description, cohort=cohort_count, pupil_count, metric)  |>  # remove unused columns
  # add dates
  mutate(chartPeriod = paste("AY", substr(timePeriod, 3, 4), "/", substr(timePeriod, 5, 6), sep = "")) %>%
  mutate(timePeriod = as.Date(paste("01 Aug", substr(timePeriod, 1, 4), sep = ""), format = "%d %b %Y")) %>%
  mutate(latest = case_when(
    timePeriod == max(timePeriod) ~ 1,
    timePeriod == (max(timePeriod) - lubridate::years(1)) ~ -1,
    TRUE ~ 0
  )) |> 
  mutate_at(c('cohort','pupil_count'), as.numeric)%>%#convert to numeric to sum
  addGeogs() %>%
  group_by(chartPeriod, timePeriod, latest, geogConcat, cohort_level_group, destination_description,metric, newArea) %>%
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))

groupedStats <- destinationsWithAreas %>%
  filter(newArea == 1) %>% # no need to group national or LAs that haven't changed
  ungroup() %>%
  select(-newArea) %>%
  group_by(chartPeriod, timePeriod, latest, geogConcat, cohort_level_group, destination_description,metric) %>% # sum for each area
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) %>%
  rename_with(~ gsub("_1", "", .))

# add back on original LADUs and format
C_destinations <- bind_rows(
  groupedStats,
  destinationsWithAreas |> 
    filter(newArea == 0)
) %>%
  select(-newArea) |> 
  # get metrics
  mutate(value=pupil_count/cohort) |> 
  select(-cohort) %>%
  mutate(breakdown = case_when(
    (destination_description == "Sustained education, employment & apprenticeships" & cohort_level_group == "Total") ~ "Total",
    cohort_level_group == "Total" ~ "Outcome",
    TRUE ~ "Level"
  )) |> 
  mutate(subgroup = case_when(breakdown == "Level" ~ cohort_level_group, 
                              destination_description=="Sustained education, employment & apprenticeships" ~ "Total",
                              TRUE ~ str_to_sentence(str_split(destination_description, " ")[[1]][2]))) |> 
  ungroup() |> 
  select(-cohort_level_group,-destination_description,-pupil_count) |> 
  mutate(valueText = as.character(value))

#save output
saveRDS(C_destinations, "Data/processing/C_destinations.rds")