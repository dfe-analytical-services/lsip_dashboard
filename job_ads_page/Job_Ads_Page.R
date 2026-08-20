# Set inputs =========================================================

# Set the base date for the growth rate calculations (currently set to January 2022)
base_date <- make_date(2022, 1, 1)

# Set constant demand cutoff (currently set to 5%)
constant_cutoff <- 0.05

# Set emerging demand cutoff (currently set to 15%)
emerging_cutoff <- 0.15

# Load the data ======================================================
# see importJobAds.R for import code if you need to import this file out of sequence
# ONS job ads by region
new_ads_national <- read.xlsx(job_ads_raw, sheet = "Table 1", startRow = 5)

# ONS job ads by Local Authority District
new_ads_LAD <- read.xlsx(job_ads_raw, sheet = "Table 2", startRow = 5)

# ONS job ads by region and 4-digit SOC
new_ads_SOC <- read.xlsx(job_ads_raw, sheet = "Table 3", startRow = 5)

# APS economic activity data from NOMIS (to be used as population estimates). NOMIS filtered for:
# Dataset = annual population survey (Data sources > Annual Population Survey/Labour Force Survey > annual population survey)
# Geography = England and 9 English regions
# Date = All dates
# Cell = Table T01 Economic activity by age > Aged 16-64 > All
APS_econ_activity <- read.csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_17_1.data.csv?geography=2092957699,2013265926,2013265924,2013265927,2013265921,2013265922,2013265928,2013265929,2013265925,2013265923&cell=402720769&measures=20100,20701&select=date_name,geography_name,geography_code,cell_name,measures_name,obs_value,obs_status_name")

# APS employment data from NOMIS. NOMIS filtered for:
# Dataset = annual population survey
# Geography = England and 9 English regions
# Date = All dates
# Cell = Table T01 Economic activity by age > Aged 16-64 > In employment
APS_employment <- read.csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_17_1.data.csv?geography=2092957699,2013265926,2013265924,2013265927,2013265921,2013265922,2013265928,2013265929,2013265925,2013265923&cell=402721281&measures=20100,20701&select=date_name,geography_name,geography_code,cell_name,measures_name,obs_value,obs_status_name")

# APS employment data by SOC 2020 from NOMIS. NOMIS filtered for:
# Dataset = annual population survey - regional - occupation (Data sources > Annual Population Survey/Labour Force Survey > annual population survey - regional - occupation (SOC2020) by sex by employment type)
# Geography = England and 9 English regions
# Date = All dates
# occupation (SOC2020) = All 4-digit SOC codes
# NOTE: NOMIS limits public API downloads to 25,000 rows. The below link has been created using a NOMIS account to 
# bypass this limit and so cannot be changed.
APS_employment_soc <- read.csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_218_1.data.csv?geography=2092957699,2013265926,2013265924,2013265927,2013265921,2013265922,2013265928,2013265929,2013265925,2013265923&jtype=0&ftpt=0&etype=0&c_sex=0&soc2020_full=0...412&measure=1&measures=20100,20701&signature=NPK-60c752aed1cf7912d2f205:0xbf75e66477c4ba11e1a67d27dd94c9d2e2052942")

# If there are issues with the above link, run this script:
# source("./job_ads_page/import_aps_employment_data.R")

# geojson file for map
map_region <- sf::st_read("./job_ads_page/Regions_December_2024_Boundaries_EN_BFC_1195854647342073399.geojson",
                           stringsAsFactors = F)

# Clean the data =====================================================

# Reshape job ads data into tidy format
new_ads_national_clean <- new_ads_national %>%
  janitor::clean_names() %>%
  # Filter to England only
  filter((country %in% "England")) %>%
  mutate(across(jan_17:last_col(), as.numeric)) %>%
  pivot_longer(jan_17:last_col(), names_to = "timePeriod", values_to = "n_jobs") %>%
  mutate(timePeriod = as.Date(paste0("01-", timePeriod), format = "%d-%b_%y"))

new_ads_LAD_clean <- new_ads_LAD %>%
  janitor::clean_names() %>%
  # Filter to England only
  filter(!(region %in% c("Northern Ireland", "Scotland", "Wales"))) %>%
  mutate(across(jan_17:last_col(), as.numeric)) %>%
  pivot_longer(jan_17:last_col(), names_to = "timePeriod", values_to = "n_jobs") %>%
  mutate(timePeriod = as.Date(paste0("01-", timePeriod), format = "%d-%b_%y"))

new_ads_SOC_clean <- new_ads_SOC %>%
  janitor::clean_names() %>%
  # Filter to England only
  filter(!(region %in% c("Northern Ireland", "Scotland", "Total UK", "Wales"))) %>%
  mutate(across(jan_17:last_col(), as.numeric)) %>%
  pivot_longer(jan_17:last_col(), names_to = "timePeriod", values_to = "n_jobs") %>%
  mutate(timePeriod = as.Date(paste0("01-", timePeriod), format = "%d-%b_%y"))

# Clean APS economic activity data
APS_econ_activity_clean <- APS_econ_activity %>%
  janitor::clean_names() %>%
  select(date_name, geography_name, geography_code, measures_name, obs_value) %>%
  filter(measures_name == "Value") %>%
  rename(chartPeriod = date_name,
         region = geography_name,
         population = obs_value) %>%
  # rename 'East' to 'East of England' to line up with the shape file
  mutate(region = if_else(region == "East", "East of England", region))

# Clean APS employment data
APS_employment_clean <- APS_employment %>%
  janitor::clean_names() %>%
  select(date_name, geography_name, measures_name, obs_value) %>%
  filter(measures_name == "Value") %>%
  rename(chartPeriod = date_name,
         region = geography_name,
         employment = obs_value) %>%
  mutate(region = if_else(region == "East", "East of England", region))

# Clean APS employment by SOC data
APS_employment_soc_clean <- APS_employment_soc %>%
  janitor::clean_names() %>%
  filter(soc2020_full_name != "Total") %>%
  # split out soc2020_full_name into SOC code and label
  separate(soc2020_full_name,
           into = c("soc_4_digit_code", "soc_4_digit_label"),
           sep = " : ",
           remove = TRUE) %>%
  select(date_name, geography_name, soc_4_digit_code, soc_4_digit_label, measures_name, obs_value) %>%
  filter(measures_name == "Value") %>%
  rename(chartPeriod = date_name,
         region = geography_name,
         employment = obs_value) %>%
  mutate(region = if_else(region == "East", "East of England", region))

# Clean geojson file
map_region_clean <- map_region %>%
  select(RGN24CD, RGN24NM, geometry) %>%
  rename(areaCode = RGN24CD,
         areaName = RGN24NM)

# Find the latest date in the job ads data
latest_date <- max(new_ads_SOC_clean$timePeriod, na.rm = TRUE)

# Three-month rolling average ========================================

# Calculate the number of job ads as a three-month rolling average to smooth the data

# Three-month rolling average by SOC
new_ads_SOC_roll <- new_ads_SOC_clean %>%
  group_by(soc_4_digit_code, soc_4_digit_label, timePeriod) %>%
  summarise(n_jobs = sum(n_jobs)) %>%
  mutate(n_jobs_3m_avg = slide_dbl(n_jobs, ~ mean(.x, na.rm = TRUE), .before = 2, .complete = TRUE)) %>%
  ungroup()

# Three-month rolling average by region
new_ads_region_roll <- new_ads_LAD_clean %>%
  group_by(region, timePeriod) %>%
  summarise(n_jobs = sum(n_jobs)) %>%
  mutate(n_jobs_3m_avg = slide_dbl(n_jobs, ~ mean(.x, na.rm = TRUE), .before = 2, .complete = TRUE)) %>%
  ungroup()

# Three-month rolling average by SOC and region
new_ads_region_SOC_roll <- new_ads_SOC_clean %>%
  group_by(region, soc_4_digit_code, soc_4_digit_label, timePeriod) %>%
  summarise(n_jobs = sum(n_jobs)) %>%
  mutate(n_jobs_3m_avg = slide_dbl(n_jobs, ~ mean(.x, na.rm = TRUE), .before = 2, .complete = TRUE)) %>%
  ungroup()

# Three-month rolling average across England
new_ads_national_roll <- new_ads_national_clean %>%
  group_by(timePeriod) %>%
  summarise(n_jobs = sum(n_jobs)) %>%
  mutate(n_jobs_3m_avg = slide_dbl(n_jobs, ~ mean(.x, na.rm = TRUE), .before = 2, .complete = TRUE)) %>%
  ungroup()

# National summary ===================================================

# Growth rate of job ads across England since base
new_ads_national_growth <- new_ads_national_roll %>%
  # Set the base value to be the n_jobs_3m_avg value at the base date
  mutate(n_jobs_base = n_jobs_3m_avg[timePeriod == base_date],
         growth_rate = (n_jobs_3m_avg - n_jobs_base) / n_jobs_base) %>%
  ungroup() %>%
  select(-n_jobs_base)

# Population rate of job ads across England

# Filter APS economic activity data to England
APS_econ_activity_national <- APS_econ_activity_clean %>%
  filter(region == "England") %>%
  select(chartPeriod, population)

new_ads_national_pop <- population_data(new_ads_national_roll) %>%
  # Join on APS economic activity data
  left_join(APS_econ_activity_national, by = "chartPeriod") %>%
  filter(!is.na(population)) %>%
  mutate(pop_rate = n_jobs_yr_sum / population)

# Employment rate of job ads across England

# Filter APS employment data
APS_employment_national <- APS_employment_clean %>%
  filter(region == "England") %>%
  select(chartPeriod, employment)

new_ads_national_job <- new_ads_national_pop %>%
  # Join on APS employment data
  left_join(APS_employment_national, by = "chartPeriod") %>%
  mutate(job_rate = n_jobs_yr_sum / employment)

# Occupational summary ===============================================

# Growth rate of job ads by SOC
new_ads_SOC_growth <- new_ads_SOC_roll %>%
  filter(timePeriod >= base_date) %>%
  group_by(soc_4_digit_code, soc_4_digit_label) %>%
  mutate(n_jobs_base = n_jobs_3m_avg[timePeriod == base_date],
         growth_rate = (n_jobs_3m_avg - n_jobs_base) / n_jobs_base) %>%
  ungroup() %>%
  select(-n_jobs_base)

# Population rate of job ads by SOC across England
new_ads_SOC_pop <- population_data(new_ads_SOC_roll, soc_4_digit_code, soc_4_digit_label) %>%
  # Join on APS economic activity data
  left_join(APS_econ_activity_national, by = "chartPeriod") %>%
  filter(!is.na(population)) %>%
  mutate(pop_rate = n_jobs_yr_sum / population,
         pop_rate = round2(pop_rate * 1000, 3)) # Per 100,000 population (will be converted to %)

# Employment rate of job ads by SOC

# Filter APS employment SOC data
APS_employment_soc_national <- APS_employment_soc_clean %>%
  filter(region == "England") %>%
  select(chartPeriod, soc_4_digit_code, employment)

new_ads_SOC_job <- new_ads_SOC_pop %>%
  # Join on employment data
  left_join(APS_employment_soc_national, by = c("chartPeriod", "soc_4_digit_code")) %>%
  # Filter out rows with missing employment data (APS employment data by SOC starts from 2021)
  filter(!is.na(employment)) %>%
  mutate(job_rate = n_jobs_yr_sum / employment)

# Regional summary ===================================================

# Volume of job ads by region for map
new_ads_region_vol <- new_ads_region_roll %>%
  filter(timePeriod == max(timePeriod)) %>%
  mutate(soc_4_digit_group = "All occupations") %>%
  rename(value = n_jobs_3m_avg) %>%
  select(region, soc_4_digit_group, timePeriod, value)

# Growth rate of job ads by region
new_ads_region_growth <- new_ads_region_roll %>%
  filter(timePeriod >= base_date) %>%
  group_by(region) %>%
  mutate(n_jobs_base = n_jobs_3m_avg[timePeriod == base_date],
         growth_rate = (n_jobs_3m_avg - n_jobs_base) / n_jobs_base) %>%
  ungroup() %>%
  select(-n_jobs_base)

# Population rate of job ads by region

# Filter APS economic activity data
APS_econ_activity_region <- APS_econ_activity_clean %>%
  select(chartPeriod, region, geography_code, population)

new_ads_region_pop <- population_data(new_ads_region_roll, region) %>%
  # Join on APS economic activity data
  left_join(APS_econ_activity_region, by = c("chartPeriod", "region")) %>%
  filter(!is.na(population)) %>%
  mutate(pop_rate = n_jobs_yr_sum / population)

# Employment rate of job ads by region

# Filter APS employment data
APS_employment_region <- APS_employment_clean %>%
  filter(region != "England") %>%
  select(chartPeriod, region, employment)

new_ads_region_job <- new_ads_region_pop %>%
  # Join on APS employment data
  left_join(APS_employment_region, by = c("chartPeriod", "region")) %>%
  mutate(job_rate = n_jobs_yr_sum / employment)

# Occupational and regional summary ==================================

# Volume of job ads by region and SOC for map
new_ads_region_SOC_vol <- new_ads_region_SOC_roll %>%
  filter(timePeriod == max(timePeriod)) %>%
  mutate(soc_4_digit_group = paste(soc_4_digit_label, soc_4_digit_code, sep = " - ")) %>%
  rename(value = n_jobs_3m_avg) %>%
  select(region, soc_4_digit_group, timePeriod, value)

# Growth rate of job ads by region and SOC
new_ads_region_SOC_growth <- new_ads_region_SOC_roll %>%
  filter(timePeriod >= base_date) %>%
  group_by(region, soc_4_digit_code, soc_4_digit_label) %>%
  mutate(n_jobs_base = n_jobs_3m_avg[timePeriod == base_date],
         growth_rate = (n_jobs_3m_avg - n_jobs_base) / n_jobs_base) %>%
  ungroup() %>%
  select(-n_jobs_base)

# Population rate of job ads by region and SOC
new_ads_region_SOC_pop <- population_data(new_ads_region_SOC_roll, region, soc_4_digit_code, soc_4_digit_label) %>%
  # Join on APS economic activity data
  left_join(APS_econ_activity_region, by = c("chartPeriod", "region")) %>%
  filter(!is.na(population)) %>%
  mutate(pop_rate = n_jobs_yr_sum / population,
         pop_rate = round2(pop_rate * 1000, 3)) # Per 100,000 population

# Employment rate of job ads by region and SOC

# Filter APS employment SOC data
APS_employment_soc_region <- APS_employment_soc_clean %>%
  filter(region != "England") %>%
  select(chartPeriod, region, employment, soc_4_digit_code)

new_ads_region_SOC_job <- new_ads_region_SOC_pop %>%
  # Join on employment data
  left_join(APS_employment_soc_region, by = c("region", "chartPeriod", "soc_4_digit_code")) %>%
  # Filter out rows with missing employment data (APS employment data by SOC starts from 2021)
  filter(!is.na(employment)) %>%
  mutate(job_rate = n_jobs_yr_sum / employment)

# Constant high demand ===============================================

# Find which occupations have been in the top 5% for every month in the past year

# Constant high demand of job ads by SOC

# Filter the data for the previous one year
new_ads_SOC_constant <- new_ads_SOC_roll %>%
  filter(timePeriod >= (latest_date - months(11)))

# Find the 95th percentile of the total number of job ads for each month
new_ads_percentile <- new_ads_SOC_constant %>%
  group_by(timePeriod) %>%
  summarise(percentile = quantile(n_jobs_3m_avg, probs = (1-constant_cutoff), na.rm = TRUE)) %>%
  ungroup()

# Flag whether each value of n_job is in the top 5% per month
new_ads_SOC_constant <- new_ads_SOC_constant %>%
  left_join(new_ads_percentile, by = "timePeriod") %>%
  mutate(top_10 = case_when(
    n_jobs_3m_avg >= percentile ~ TRUE,
    TRUE ~ FALSE)) %>%
  # Pull out the occupations that have been in the top 5% for every month
  group_by(soc_4_digit_code, soc_4_digit_label) %>%
  summarise(top_10_all = ifelse(sum(top_10) == n(), TRUE, FALSE)) %>%
  filter(top_10_all) %>%
  select(-top_10_all)

# Calculate percentage change for these occupations
new_ads_SOC_constant_output <- new_ads_SOC_roll %>%
  filter(timePeriod %in% c(latest_date, latest_date %m-% years(1))) %>%
  mutate(period = if_else(timePeriod == latest_date, 
                          "n_jobs_latest", 
                          "n_jobs_previous")) %>%
  select(soc_4_digit_code, period, n_jobs_3m_avg) %>%
  pivot_wider(names_from = period, values_from = n_jobs_3m_avg) %>%
  # Join on the constant data
  right_join(new_ads_SOC_constant, by = "soc_4_digit_code") %>%
  arrange(desc(n_jobs_latest)) %>%
  # Percentage change will be the latest month compared to the same month in the previous year
  mutate(Region = "England",
         `Percentage change` = (n_jobs_latest - n_jobs_previous) / n_jobs_previous,
         `Number of new job adverts` = format(round2(n_jobs_latest, 0), big.mark = ",")) %>%
  select(Region,
         Occupation = soc_4_digit_label,
         `Number of new job adverts`,
         `Percentage change`)

# Constant high demand of job ads by region and SOC

# Filter the data for the previous one year
new_ads_region_SOC_constant <- new_ads_region_SOC_roll %>%
  filter(timePeriod >= (latest_date - months(11)))

# Find the 95th percentile of the total number of job ads for each month
new_ads_region_percentile <- new_ads_region_SOC_constant %>%
  group_by(region, timePeriod) %>%
  summarise(percentile = quantile(n_jobs_3m_avg, probs = (1-constant_cutoff), na.rm = TRUE)) %>%
  ungroup()

# Flag whether each value of n_job is in the top 5% per month
new_ads_region_SOC_constant <- new_ads_region_SOC_constant %>%
  left_join(new_ads_region_percentile, by = c("region", "timePeriod")) %>%
  mutate(top_10 = case_when(
    n_jobs_3m_avg >= percentile ~ TRUE,
    TRUE ~ FALSE)) %>%
  # Pull out the occupations that have been in the top 5% for every month
  group_by(region, soc_4_digit_code, soc_4_digit_label) %>%
  summarise(top_10_all = ifelse(sum(top_10) == n(), TRUE, FALSE)) %>%
  filter(top_10_all) %>%
  select(-top_10_all)

# Calculate percentage change for these occupations
new_ads_region_SOC_constant_output <- new_ads_region_SOC_roll %>%
  filter(timePeriod %in% c(latest_date, latest_date %m-% years(1))) %>%
  mutate(period = if_else(timePeriod == latest_date, 
                          "n_jobs_latest", 
                          "n_jobs_previous")) %>%
  select(region, soc_4_digit_code, period, n_jobs_3m_avg) %>%
  pivot_wider(names_from = period, values_from = n_jobs_3m_avg) %>%
  # Join on the constant data
  right_join(new_ads_region_SOC_constant, by = c("region", "soc_4_digit_code")) %>%
  arrange(region, desc(n_jobs_latest)) %>%
  # Percentage change will be the latest month compared to the same month in the previous year
  mutate(`Percentage change` = (n_jobs_latest - n_jobs_previous) / n_jobs_previous,
         `Number of new job adverts` = format(round2(n_jobs_latest, 0), big.mark = ",")) %>%
  select(Region = region,
         Occupation = soc_4_digit_label,
         `Number of new job adverts`,
         `Percentage change`)

# Emerging high demand ===============================================

# Find which occupations have been in the top 15% in the latest 3-months but not in the top 15% in the previous 9-months

# Emerging high demand of job ads by SOC

# Filter data for the previous 3-months and then the 9-months prior to that

# Use the 3-month average from the latest month (as it would just be scaled down by 3)
new_ads_SOC_3months <- new_ads_SOC_roll %>%
  filter(timePeriod == latest_date) %>%
  # Pull out the occupations that are in the top 15%
  filter(n_jobs_3m_avg >= quantile(n_jobs_3m_avg, (1-emerging_cutoff)))

# Filter data for the 9-months prior to that
new_ads_SOC_9months <- new_ads_SOC_roll %>%
  filter(timePeriod >= (latest_date %m-% months(11)) & timePeriod <= (latest_date %m-% months(3))) %>%
  group_by(soc_4_digit_code, soc_4_digit_label) %>% 
  summarise(n_jobs_sum = sum(n_jobs, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(n_jobs_sum >= quantile(n_jobs_sum, (1-emerging_cutoff)))

# Pull out the occupations that are in the top 15% in the latest 3-months but not in the previous 9-months
new_ads_SOC_emerging <- new_ads_SOC_3months %>%
  anti_join(new_ads_SOC_9months, by = "soc_4_digit_code")

# Calculate percentage change for these occupations
new_ads_SOC_emerging_output <- new_ads_SOC_roll %>%
  filter(timePeriod %in% c(latest_date, latest_date %m-% years(1))) %>%
  mutate(period = if_else(timePeriod == latest_date, 
                          "n_jobs_latest", 
                          "n_jobs_previous")) %>%
  select(soc_4_digit_code, period, n_jobs_3m_avg) %>%
  pivot_wider(names_from = period, values_from = n_jobs_3m_avg) %>%
  # Join on the emerging data
  right_join(new_ads_SOC_emerging, by = "soc_4_digit_code") %>%
  # Percentage change will be the latest month compared to the same month in the previous year
  mutate(Region = "England",
         `Percentage change` = (n_jobs_latest - n_jobs_previous) / n_jobs_previous,
         `Number of new job adverts` = format(round2(n_jobs_latest, 0), big.mark = ",")) %>%
  select(Region,
         Occupation = soc_4_digit_label,
         `Number of new job adverts`,
         `Percentage change`)

# Emerging high demand of job ads by region and SOC

# Filter data for the previous 3-months and then the 9-months prior to that
new_ads_region_SOC_3months <- new_ads_region_SOC_roll %>%
  filter(timePeriod == latest_date) %>%
  # Pull out the occupations that are in the top 15%
  group_by(region) %>%
  filter(n_jobs_3m_avg >= quantile(n_jobs_3m_avg, (1-emerging_cutoff))) %>%
  ungroup()

new_ads_region_SOC_9months <- new_ads_region_SOC_roll %>%
  filter(timePeriod >= (latest_date %m-% months(11)) & timePeriod <= (latest_date %m-% months(3))) %>%
  group_by(region, soc_4_digit_code, soc_4_digit_label) %>% 
  summarise(n_jobs_sum = sum(n_jobs, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(n_jobs_sum >= quantile(n_jobs_sum, (1-emerging_cutoff)))

# Pull out the occupations that are in the top 15% in the latest 3-months but not in the previous 9-months
new_ads_region_SOC_emerging <- new_ads_region_SOC_3months %>%
  anti_join(new_ads_region_SOC_9months, by = c("region", "soc_4_digit_code"))

# Calculate percentage change for these occupations
new_ads_region_SOC_emerging_output <- new_ads_region_SOC_roll %>%
  filter(timePeriod %in% c(latest_date, latest_date %m-% years(1))) %>%
  mutate(period = if_else(timePeriod == latest_date, 
                          "n_jobs_latest", 
                          "n_jobs_previous")) %>%
  select(region, soc_4_digit_code, period, n_jobs_3m_avg) %>%
  pivot_wider(names_from = period, values_from = n_jobs_3m_avg) %>%
  # Join on the emerging data
  right_join(new_ads_region_SOC_emerging, by = c("region", "soc_4_digit_code")) %>%
  # Percentage change will be the latest month compared to the same month in the previous year
  mutate(`Percentage change` = (n_jobs_latest - n_jobs_previous) / n_jobs_previous,
         `Number of new job adverts` = format(round2(n_jobs_latest, 0), big.mark = ",")) %>%
  select(Region = region,
         Occupation = soc_4_digit_label,
         `Number of new job adverts`,
         `Percentage change`)

# Rank of occupations ================================================

# Rank of job ads by SOC
new_ads_SOC_ranking <- new_ads_SOC_roll %>%
  filter(timePeriod == latest_date) %>%
  # Create ranking column
  mutate(rank = dense_rank(desc(n_jobs_3m_avg))) %>%
  select(-c(timePeriod, n_jobs)) %>%
  arrange(rank)

# Final formatted table for the dashboard page
new_ads_SOC_ranking <- new_ads_SOC_ranking %>%
  select(rank, soc_4_digit_label, n_jobs_3m_avg) %>%
  mutate(n_jobs_3m_avg = format(round2(n_jobs_3m_avg, 0), big.mark = ","),
         Region = "England") %>%
  select(Rank = rank,
         Region,
         Occupation = soc_4_digit_label,
         `Number of new job adverts` = n_jobs_3m_avg)

# Rank of job ads by region and SOC
new_ads_region_SOC_ranking <- new_ads_region_SOC_roll %>%
  filter(timePeriod == latest_date) %>%
  # Create ranking column
  group_by(region) %>%
  mutate(rank = dense_rank(desc(n_jobs_3m_avg))) %>%
  ungroup() %>%
  select(-c(timePeriod, n_jobs)) %>%
  arrange(region, rank)

# Final formatted table for the dashboard page
new_ads_region_SOC_ranking <- new_ads_region_SOC_ranking %>%
  select(rank, region,soc_4_digit_label, n_jobs_3m_avg) %>%
  mutate(n_jobs_3m_avg = format(round2(n_jobs_3m_avg, 0), big.mark = ",")) %>%
  select(Rank = rank,
         Region = region,
         Occupation = soc_4_digit_label,
         `Number of new job adverts` = n_jobs_3m_avg)

# Format tables to output ============================================

# Simplify geojson file for quicker loading
map_region_simple <- rmapshaper::ms_simplify(map_region_clean, keep = 0.01, keep_shapes = TRUE)

# National output table
output_national <- bind_rows(
  
  new_ads_national_roll %>%
    select(timePeriod, n_jobs_3m_avg) %>%
    rename(value = n_jobs_3m_avg) %>%
    filter(timePeriod >= make_date(year(latest_date) - 4, 1, 1)) %>%
    mutate(metric = "volume"),
  
  new_ads_national_growth  %>%
    select(timePeriod, growth_rate) %>%
    rename(value = growth_rate) %>%
    filter(timePeriod >= make_date(year(latest_date) - 4, 1, 1)) %>%
    mutate(metric = "growthRate"),
  
  new_ads_national_pop  %>%
    select(timePeriod, pop_rate) %>%
    rename(value = pop_rate) %>%
    filter(timePeriod >= make_date(year(latest_date) - 4, 1, 1)) %>%
    mutate(metric = "popRate"),
  
  new_ads_national_job  %>%
    select(timePeriod, job_rate) %>%
    rename(value = job_rate) %>%
    filter(timePeriod >= make_date(year(latest_date) - 4, 1, 1)) %>%
    mutate(metric = "jobRate")
  )

output_national <- output_national %>%
  mutate(geogConcat = "England",
         soc_4_digit_group = "All occupations",
         chartPeriod = format(timePeriod, "%b-%y"),
         timePeriod = as.character(timePeriod)) %>%
  select(geogConcat, soc_4_digit_group, metric, timePeriod, chartPeriod, value)

# Occupational output table
output_occupations <- bind_rows(
  
  new_ads_SOC_roll %>%
    select(timePeriod, soc_4_digit_code, soc_4_digit_label, n_jobs_3m_avg) %>%
    rename(value = n_jobs_3m_avg) %>%
    filter(timePeriod >= make_date(year(latest_date) - 4, 1, 1)) %>%
    mutate(metric = "volume"),
  
  new_ads_SOC_growth  %>%
    select(timePeriod, soc_4_digit_code, soc_4_digit_label, growth_rate) %>%
    rename(value = growth_rate) %>%
    mutate(metric = "growthRate"),
  
  new_ads_SOC_pop  %>%
    select(timePeriod, soc_4_digit_code, soc_4_digit_label, pop_rate) %>%
    rename(value = pop_rate) %>%
    filter(timePeriod >= make_date(year(latest_date) - 4, 1, 1)) %>%
    mutate(metric = "popRate"),
  
  new_ads_SOC_job  %>%
    select(timePeriod, soc_4_digit_code, soc_4_digit_label, job_rate) %>%
    filter(timePeriod >= make_date(year(latest_date) - 4, 1, 1)) %>%
    rename(value = job_rate) %>%
    mutate(metric = "jobRate")
)

output_occupations <- output_occupations %>%
  mutate(geogConcat = "England",
         soc_4_digit_group = paste(soc_4_digit_label, soc_4_digit_code, sep = " - "),
         chartPeriod = format(timePeriod, "%b-%y"),
         timePeriod = as.character(timePeriod)) %>%
  select(geogConcat, soc_4_digit_group, metric, timePeriod, chartPeriod, value)


# Regional output table
output_regions <- bind_rows(
  
  new_ads_region_roll %>%
    select(timePeriod, region, n_jobs_3m_avg) %>%
    rename(value = n_jobs_3m_avg) %>%
    filter(timePeriod >= make_date(year(latest_date) - 4, 1, 1)) %>%
    mutate(metric = "volume"),
  
  new_ads_region_growth  %>%
    select(timePeriod, region, growth_rate) %>%
    rename(value = growth_rate) %>%
    mutate(metric = "growthRate"),
  
  new_ads_region_pop  %>%
    select(timePeriod, region, pop_rate) %>%
    rename(value = pop_rate) %>%
    filter(timePeriod >= make_date(year(latest_date) - 4, 1, 1)) %>%
    mutate(metric = "popRate"),
  
  new_ads_region_job  %>%
    select(timePeriod, region, job_rate) %>%
    filter(timePeriod >= make_date(year(latest_date) - 4, 1, 1)) %>%
    rename(value = job_rate) %>%
    mutate(metric = "jobRate")
)

output_regions <- output_regions %>%
  rename(geogConcat = region) %>%
  mutate(soc_4_digit_group = "All occupations",
         chartPeriod = format(timePeriod, "%b-%y"),
         timePeriod = as.character(timePeriod)) %>%
  select(geogConcat, soc_4_digit_group, metric, timePeriod, chartPeriod, value)

# Occupational and regional output table
output_occupations_regions <- bind_rows(
  
  new_ads_region_SOC_roll %>%
    select(timePeriod, region, soc_4_digit_code, soc_4_digit_label, n_jobs_3m_avg) %>%
    rename(value = n_jobs_3m_avg) %>%
    filter(timePeriod >= make_date(year(latest_date) - 4, 1, 1)) %>%
    mutate(metric = "volume"),
  
  new_ads_region_SOC_growth  %>%
    select(timePeriod, region, soc_4_digit_code, soc_4_digit_label, growth_rate) %>%
    rename(value = growth_rate) %>%
    mutate(metric = "growthRate"),
  
  new_ads_region_SOC_pop  %>%
    select(timePeriod, region, soc_4_digit_code, soc_4_digit_label, pop_rate) %>%
    rename(value = pop_rate) %>%
    filter(timePeriod >= make_date(year(latest_date) - 4, 1, 1)) %>%
    mutate(metric = "popRate"),
  
  new_ads_region_SOC_job  %>%
    select(timePeriod, region, soc_4_digit_code, soc_4_digit_label, job_rate) %>%
    filter(timePeriod >= make_date(year(latest_date) - 4, 1, 1)) %>%
    rename(value = job_rate) %>%
    mutate(metric = "jobRate")
)

output_occupations_regions <- output_occupations_regions %>%
  rename(geogConcat = region) %>%
  mutate(soc_4_digit_group = paste(soc_4_digit_label, soc_4_digit_code, sep = " - "),
         chartPeriod = format(timePeriod, "%b-%y"),
         timePeriod = as.character(timePeriod)) %>%
  select(geogConcat, soc_4_digit_group, metric, timePeriod, chartPeriod, value)

# Combine tables ===================================================

output_line_chart <- bind_rows(output_national, output_occupations, output_regions, output_occupations_regions)

output_map <- bind_rows(new_ads_region_vol, new_ads_region_SOC_vol)

output_constant <- bind_rows(new_ads_SOC_constant_output, new_ads_region_SOC_constant_output)

output_emerging <- bind_rows(new_ads_SOC_emerging_output, new_ads_region_SOC_emerging_output)

output_ranking <- bind_rows(new_ads_SOC_ranking, new_ads_region_SOC_ranking)

# Write out ==========================================================

saveRDS(map_region_simple, "./job_ads_page/job_ads_page_geog.rds")

saveRDS(output_line_chart, "./job_ads_page/job_ads_page_line_chart.rds")

saveRDS(output_map, "./job_ads_page/job_ads_page_map.rds")

saveRDS(output_constant, "./job_ads_page/job_ads_page_constant.rds")

saveRDS(output_emerging, "./job_ads_page/job_ads_page_emerging.rds")

saveRDS(output_ranking, "./job_ads_page/job_ads_page_ranking.rds")
