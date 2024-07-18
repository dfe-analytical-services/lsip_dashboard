### 2.1.4 Skill by age gender ------------
# Geog and date as above
# Cell: T19	Qualification by age and gender. All people aged 16-64. only updated every Jan-Dec. NVQ data is available until 2021 - then on rcf
# find cells we want in NVQ
cellsUseAps_qualNvq <- cellsListAps %>% filter(description.en %like% "T19:" & description.en %like% "Total")
# get data
I_qualAgeGenderNvq <- extractNomis("NM_17_1", "2019-12,2020-12,2021-12", cellsUseAps_qualNvq$id)
# find cells we want in NVQ
cellsUseAps_qualRqf <- cellsListAps %>% filter(description.en %like% "T19a:" & description.en %like% "Total")
# get rqf data from 2022 onwards. only jan-dec data avaialable so input of date in manual
I_qualAgeGenderRqf <- extractNomis("NM_17_1", "2022-12,2023-12", cellsUseAps_qualRqf$id)

## Qualification level by age and gender ----
C_qualAgeGender <- formatNomis(I_qualAgeGenderNvq %>%
                                 bind_rows(I_qualAgeGenderRqf)) %>%
  mutate(
    metric = case_when(
      grepl("None", CELL_NAME) ~ "qualNone",
      grepl("NVQ1|RQF1", CELL_NAME) ~ "qualL1",
      grepl("NVQ2|RQF2", CELL_NAME) ~ "qualL2",
      grepl("Trade Apprenticeships", CELL_NAME) ~ "qualApp",
      grepl("NVQ3|RQF3", CELL_NAME) ~ "qualL3",
      grepl("NVQ4|RQF4", CELL_NAME) ~ "qualL4",
      grepl("Other Qualifications", CELL_NAME) ~ "qualOther"
    ),
    subgroup = case_when(
      grepl("All people aged 16-64", CELL_NAME) ~ "Total",
      grepl("Males aged 16-64", CELL_NAME) ~ "Males",
      grepl("Females aged 16-64", CELL_NAME) ~ "Females",
      TRUE ~ case_when(
        grepl("19a", CELL_NAME) ~ substr(CELL_NAME, 34, 38),
        TRUE ~ substr(CELL_NAME, 33, 37)
      ) # get ages
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

# calculate % of people with L3+ (ie (L3+L4)/all quals)
C_qualL3PlusAgeGender <- C_qualAgeGender %>%
  filter(metric %in% c("qualL3", "qualL4")) %>%
  group_by(chartPeriod, timePeriod, geogConcat, breakdown, subgroup, latest) %>%
  summarise(qualL3Plus = sum(value, na.rm = T)) %>%
  left_join(qualSum) %>%
  mutate(metric = "L3PlusRate", value = qualL3Plus / allQuals) %>%
  select(-qualL3Plus, -allQuals) %>%
  mutate(valueText = case_when(value == 0 ~ "c", TRUE ~ as.character(value)))

# calculate % of people with L4+ (ie L4/all quals)
C_qualL4PlusAgeGender <- C_qualAgeGender %>%
  filter(metric == "qualL4") %>%
  group_by(chartPeriod, timePeriod, geogConcat, breakdown, subgroup, latest) %>%
  summarise(qualL4Plus = sum(value, na.rm = T)) %>%
  left_join(qualSum) %>%
  mutate(metric = "L4PlusRate", value = qualL4Plus / allQuals) %>%
  select(-qualL4Plus, -allQuals) %>%
  mutate(valueText = case_when(value == 0 ~ "c", TRUE ~ as.character(value)))
