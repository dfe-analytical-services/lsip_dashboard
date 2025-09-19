# 3. Combine datasets ----
C_localSkillsDatasetBind <- bind_rows(
  C_emp,
  C_empOcc,
  C_empInd,
  C_entSize,
  C_entInd,
  C_qualAgeGender,
  C_qualL3PlusAgeGender,
  C_qualL4PlusAgeGender,
  C_FeProvLevelAge,
  C_FeSsa,
  C_skillsImperative,
  C_destinations,
  C_adverts,
  C_businesses
)
# add in GLA as an CA because people expect it be there (no need in FE data because it is published as GLA)
C_localSkillsDataset <- bind_rows(
  C_localSkillsDatasetBind 
  ,C_localSkillsDatasetBind %>%
    filter(!metric %in% c("starts_rate_per_100000_population","participation_rate_per_100000_population",
                          "achievements_rate_per_100000_population","starts",                                 
                          "participation","achievements", "achievementsAims","enrolmentsAims"),
           geogConcat == "Greater London LSIP") %>%
    mutate(geogConcat = "Greater London Authority CA")
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
         latest == 1,
         metric != "employmentProjectionAnnualGrowth", # the maps use the employmentProjectionGrowth2024to2035 metric
         !metric %in% dashboardMetricIgnore # remove metrics not used
       ) %>%
       mutate(metric=gsub("employmentProjectionGrowth2024to2035","employmentProjection",metric))%>%# for the emp projections page we use two metrics on different charts. we give them the same name so the filters work
       mutate(metric=case_when(
         breakdown=="Total" ~ metric,
         metric == "achievementsAims" ~ paste0("achievements",breakdown,subgroup),# align achieve aims metric name so shows up when achievements chosen
         TRUE ~ paste0(metric,breakdown,subgroup))
       )%>%
       select(value, metric, geogConcat) %>%
       tidyr::pivot_wider(names_from = metric, values_from = value)),
    by = c("geogConcat" = "geogConcat")
  ) %>% 
  mutate(England=case_when(geog=="LADU" ~ "England",
                           TRUE ~ NA)) #add in a column for all LAs for when england is selected
save(C_Geog, file = "Data\\AppData\\C_Geog.rdata")

## 4.2 C_time ----
# This is used in the line charts and KPIs. It contains historic data for each metric and area.
C_time <- C_localSkillsDataset %>%
  filter(
    !metric %in% c("economicallyactiveRate","employeesRate" ,"employmentProjectionGrowth2024to2035"), # time charts only use employmentProjectionAnnualGrowth metric
    !metric %in% dashboardMetricIgnore # remove metrics not used
  ) %>%
  mutate(metric = case_when(
    metric == "achievementsAims" ~ paste0("achievements",breakdown,subgroup),# align achieve aims metric name so shows up when achievements chosen
    breakdown != "Total" ~ paste0(metric,breakdown, subgroup),
    TRUE ~ metric
  )) %>% # set metric name to subgroup when we want subgroup data
  select(geogConcat, metric, timePeriod, chartPeriod, latest, value, valueText) %>%
  mutate(metric = gsub("employmentProjectionAnnualGrowth","employmentProjection",metric))# for the emp projections page we use two metrics on different charts. we give them the same name so the filters work
#Need to split this to two files less than 100mb to upload to git
index <- seq.int(nrow(C_time) / 2)
C_time1<-C_time[index, ]
C_time2<-C_time[-index, ]
write.csv(C_time1, file = "Data\\AppData\\C_time1.csv", row.names = FALSE)
write.csv(C_time2, file = "Data\\AppData\\C_time2.csv", row.names = FALSE)

### 4.2.1 Axis min and max ----
# Create max and min for each metric used in setting axis on the overview page
C_axisMinMax <- C_time %>%
  filter(stringr::str_sub(geogConcat, -4, -1) != "LADU") %>%
  group_by(metric) %>%
  summarise(minAxis = min(value), maxAxis = max(value)) # , .groups = "drop"
write.csv(C_axisMinMax, file = "Data\\AppData\\C_axisMinMax.csv", row.names = FALSE)

## 4.3 C_breakdown ----
# This is used in the bar chart. It contains the latest data with all splits available.
C_breakdown <- bind_rows(
  # Metric where the proportion needs to be calculated. get proprtion of the total
  C_localSkillsDataset %>%
    filter(
      breakdown != "Total", subgroup != "Total", latest == 1,
      (metric %in% c("inemployment", "vacancies", "enterpriseCount", "achievementsAims", "achievements", "participation", "starts"))
    ) %>%
    select(geogConcat, metric, breakdown, subgroup, value) %>%
    mutate_all(~ replace(., is.na(.), 0)) %>%
    filter(breakdown != "Provision") %>%
    # get totals for the denominator
    left_join(
      C_localSkillsDataset %>%
        filter(
          breakdown == "Total", subgroup == "Total", latest == 1,
          metric %in% c("vacancies", "enterpriseCount", "achievements", "achievementsAims", "participation", "starts")
        ) %>%
        # add on the 16 plus totals
        bind_rows(F_emp16plus %>%
                    filter(
                      breakdown == "Total", subgroup == "Total", latest == 1
                    )) %>%
        select(geogConcat, metric, total = value)
    ) %>%
    ## add in provision split as the sub groups include under 19 apps where the total doesn't
    bind_rows(
      C_localSkillsDataset %>%
        filter(
          breakdown == "Provision", subgroup != "Total", latest == 1,
          (metric %in% c("inemployment", "vacancies", "enterpriseCount", "achievementsAims", "achievements", "participation", "starts"))
        ) %>%
        select(geogConcat, metric, breakdown, subgroup, value) %>%
        # get totals for the denominator
        left_join(
          C_localSkillsDataset %>%
            filter(
              breakdown == "Provision", subgroup != "Total", latest == 1,
              (metric %in% c("inemployment", "vacancies", "enterpriseCount", "achievementsAims", "achievements", "participation", "starts"))
            ) %>%
            group_by(geogConcat, metric, breakdown) %>%
            summarise(total = sum(value, na.rm = T))
        )
    ) %>%
    mutate_all(~ replace(., is.na(.), 0)) %>%
    mutate(value = round(value / total, 3)) %>%
    mutate(valueText = as.character(value)) %>%
    mutate(metric = case_when(
      metric == "achievementsAims" ~ "achievements",
      TRUE ~ metric
    )), # allign metric name so shows up when acievemnts chosen
  # Metric where value is used as it is
  C_localSkillsDataset %>%
    filter(
      breakdown != "Total", subgroup != "Total", latest == 1,
      !metric %in% c("inemployment", "vacancies", "enterpriseCount", "achievements", "achievementsAims", "participation", "starts")
    ) %>%
    select(geogConcat, metric, breakdown, subgroup, value, valueText)
) %>%
  filter(
    metric != "employmentProjectionAnnualGrowth", # breakdown chart only uses employmentProjectionGrowth2024to2035 metric
    !metric %in% dashboardMetricIgnore # remove metrics not used
  ) %>%
  mutate(metric = case_when(
    metric == "employmentProjectionGrowth2024to2035" ~ "employmentProjection",
    TRUE ~ metric
  )) # for the emp projections page we use two metrics on different charts. we give them the same name so the filters work
write.csv(C_breakdown, file = "Data\\AppData\\C_breakdown.csv", row.names = FALSE)

### 4.3.1 Find top ten for each breakdown ----
# (these are chosen in the filter)
# get a list of summary and detailed professions
C_detailLookup <- C_breakdown %>%
  filter(breakdown == "Occupation (SOC2020 Sub-Major Group)") %>%
  distinct(subgroup) %>%
  mutate(
    SOC1digitCode = substr(subgroup, 1, 1),
    `Occupation (SOC2020 Major Group)` = case_when(
      SOC1digitCode == "1" ~ "1 - Managers, directors and senior officials",
      SOC1digitCode == "2" ~ "2 - Professional occupations",
      SOC1digitCode == "3" ~ "3 - Associate professional occupations",
      SOC1digitCode == "4" ~ "4 - Administrative and secretarial occupations",
      SOC1digitCode == "5" ~ "5 - Skilled trades occupations",
      SOC1digitCode == "6" ~ "6 - Caring, leisure and other service occupations",
      SOC1digitCode == "7" ~ "7 - Sales and customer service occupations",
      SOC1digitCode == "8" ~ "8 - Process, plant and machine operatives",
      SOC1digitCode == "9" ~ "9 - Elementary occupations",
      TRUE ~ "NULL"
    )
  ) %>%
  select(`Occupation (SOC2020 Major Group)`, `Occupation (SOC2020 Sub-Major Group)` = subgroup)
write.csv(C_detailLookup, file = "Data\\AppData\\C_detailLookup.csv", row.names = FALSE)

C_topTenEachBreakdown <-
  bind_rows(
    C_breakdown %>%
      filter(stringr::str_sub(geogConcat, -4, -1) != "LADU") %>%
      group_by(metric, breakdown, geogConcat) %>%
      arrange(desc(value)) %>%
      slice(1:10) %>%
      mutate(`Occupation (SOC2020 Major Group)` = "All"),
    C_breakdown %>%
      filter(breakdown == "Occupation (SOC2020 Sub-Major Group)", stringr::str_sub(geogConcat, -4, -1) != "LADU") %>%
      mutate(
        SOC1digitCode = substr(subgroup, 1, 1),
        `Occupation (SOC2020 Major Group)` = case_when(
          SOC1digitCode == "1" ~ "1 - Managers, directors and senior officials",
          SOC1digitCode == "2" ~ "2 - Professional occupations",
          SOC1digitCode == "3" ~ "3 - Associate professional occupations",
          SOC1digitCode == "4" ~ "4 - Administrative and secretarial occupations",
          SOC1digitCode == "5" ~ "5 - Skilled trades occupations",
          SOC1digitCode == "6" ~ "6 - Caring, leisure and other service occupations",
          SOC1digitCode == "7" ~ "7 - Sales and customer service occupations",
          SOC1digitCode == "8" ~ "8 - Process, plant and machine operatives",
          SOC1digitCode == "9" ~ "9 - Elementary occupations",
          TRUE ~ "NULL"
        )
      ) %>%
      group_by(geogConcat, metric, breakdown, `Occupation (SOC2020 Major Group)`) %>%
      arrange(desc(value)) %>%
      slice(1:10)
  ) %>%
  select(metric, breakdown, geogConcat, subgroup, `Occupation (SOC2020 Major Group)`)
write.csv(C_topTenEachBreakdown, file = "Data\\AppData\\C_topTenEachBreakdown.csv", row.names = FALSE)

## 4.4 C_dataHub ----
# This is used in the data explorer page
C_datahub <- C_localSkillsDataset %>%
  select(geogConcat, metric, breakdown, subgroup, chartPeriod, valueText, latest) %>%
  filter(!metric %in% c("economicallyactive", "employees","economicallyactiveRate", "employeesRate", "inemploymentRate", "selfemployedRate", "inactiveRate", "unemployedRate", "starts_rate_per_100000_population", "starts", "enrolments", "employmentProjectionAnnualGrowth", "employmentProjectionGrowth2024to2035", "birthRate", "deathRate", "L3PlusRate","L4PlusRate")) %>% 
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
    metric == "achievementsAims" ~ "FE achievement aims volume",
    metric == "enrolmentsAims" ~ "FE enrolment aims volume",
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
    metric == "qualNone" ~ "No qualification",
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
    filter(metric == "inemployment", Breakdown == "Occupation (SOC2020 Sub-Major Group)") %>%
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
    filter(metric == "achievementsAims", Breakdown == "SSA") %>%
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

writexl::write_xlsx(list_of_datasets0, "Data\\AppData\\CoreIndicators.xlsx")

##4.6 Create nested list of all areas to use in drop downs ----
areaChoicesUnique<-C_time%>%
  distinct(geogConcat)%>%
  mutate(geography=case_when(stringr::str_sub(geogConcat, -4, -1) == "LSIP" ~ "Local skills partnership area",
                             stringr::str_sub(geogConcat, -4, -1) == "LADU" ~ "Local authority",
                             stringr::str_sub(geogConcat, -2, -1) == "CA" ~ "Combined authority",
                             TRUE ~ "Country"),
         areaClean=gsub("\\s+\\w*$", "", geogConcat)
  )%>%
  #filter out closed areas
  filter(!geogConcat %in% c("Black Country LEP", "Coventry and Warwickshire LEP", "Outside of an English Devolved Area and unknown CA", "North of Tyne CA", "NA LADU"))%>%
  arrange(geography,geogConcat)
#check these aren;t in there

areaChoices <- areaChoicesUnique %>%
  group_by(geography) %>%
  summarise(mapping = list(setNames(geogConcat, areaClean)), .groups = "drop") %>%
  { setNames(.$mapping, .$geography) }

# Reorder nested_list according to desired_order
areaChoices <- areaChoices[c("Local skills partnership area", "Combined authority", "Country", "Local authority")]

saveRDS(areaChoices, file = "Data/AppData/areaChoices.rds")