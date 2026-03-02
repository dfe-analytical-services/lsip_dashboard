# Load the dependencies ==============================================

######### This will need to be added to ExtractLoadData.R #########
######### The geojson file below will need to be saved in the data folder #########

library(tidyverse)
library(lubridate)
library(openxlsx)
library(sf)

# Load the data ======================================================

# Job ads by region and 4-digit SOC
folder <- "2-12_OnsProf"
sheet <- "Table 3"
startRow <- 5
I_Ons4digReg <- openxlsx::read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, startRow = startRow)

# APS economic activity data from NOMIS (to be used as population estimates)
F_econ_Aps <- read.csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_17_1.data.csv?geography=2092957699,2013265926,2013265924,2013265927,2013265921,2013265922,2013265928,2013265929,2013265925,2013265923&cell=402720769&measures=20100,20701&select=date_name,geography_name,geography_code,cell_name,measures_name,obs_value,obs_status_name")

# APS employment data from NOMIS
F_emp_Aps <- read.csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_17_1.data.csv?geography=2092957699&cell=402721281&measures=20100,20701&select=date_name,geography_name,geography_code,cell_name,measures_name,obs_value,obs_status_name")

# APS employment data by SOC 2020 from NOMIS
F_emp_soc_Aps <- read.csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_218_1.data.csv?geography=2092957699&jtype=0&ftpt=0&etype=0&c_sex=0&soc2020_full=1...412&measure=1&measures=20100,20701&select=date_name,geography_name,geography_code,jtype_name,ftpt_name,etype_name,c_sex_name,soc2020_full_name,measure_name,measures_name,obs_value,obs_status_name")

# geojson file for map
I_mapRegion <- sf::st_read("./job_ads_page/Regions_December_2024_Boundaries_EN_BFC_1195854647342073399.geojson",
                           stringsAsFactors = F)

# Clean the data =====================================================

# Reshape job ads data into tidy format
I_Ons4digReg_clean <- I_Ons4digReg %>%
  janitor::clean_names() %>%
  filter(!(region %in% c("Northern Ireland", "Scotland", "Total UK", "Wales"))) %>%
  mutate(across(jan_17:last_col(), as.numeric)) %>%
  pivot_longer(jan_17:last_col(), names_to = "timePeriod", values_to = "n_jobs") %>%
  mutate(timePeriod = as.Date(paste0("01-", timePeriod), format = "%d-%b_%y"))

# Clean APS economic activity data
F_econ_Aps_clean <- F_econ_Aps %>%
  janitor::clean_names() %>%
  select(date_name, geography_name, geography_code, measures_name, obs_value) %>%
  filter(measures_name == "Value") %>%
  rename(chartPeriod = date_name,
         region = geography_name,
         population = obs_value) %>%
  # rename 'East' to 'East of England' to line up with the shape file
  mutate(region = if_else(region == "East", "East of England", region))

# Clean APS employment data
F_emp_Aps_clean <- F_emp_Aps %>%
  janitor::clean_names() %>%
  select(date_name, geography_name, measures_name, obs_value) %>%
  filter(measures_name == "Value") %>%
  rename(chartPeriod = date_name,
         employment = obs_value)

# Clean APS employment by SOC data
F_emp_soc_Aps_clean <- F_emp_soc_Aps %>%
  janitor::clean_names() %>%
  # split out soc2020_full_name into SOC code and label
  mutate(soc_4_digit_code = substr(soc2020_full_name, 1, 4),
         soc_4_digit_label = substring(soc2020_full_name, 8)) %>%
  select(date_name, geography_name, soc_4_digit_code, soc_4_digit_label, measures_name, obs_value) %>%
  filter(measures_name == "Value") %>%
  rename(chartPeriod = date_name,
         employment = obs_value)

# Clean geojson file
I_mapRegion_clean <- I_mapRegion %>%
  select(RGN24CD, RGN24NM, geometry)

# Find the latest date in the job ads data
latest_date <- max(I_Ons4digReg_clean$timePeriod, na.rm = TRUE)

# Three-month rolling average ========================================

# Calculate the number of job ads as a three-month rolling average to smooth the data

# Three-month rolling average by SOC
new_ads_SOC_roll <- I_Ons4digReg_clean %>%
  group_by(soc_4_digit_code, soc_4_digit_label, timePeriod) %>%
  summarise(n_jobs = sum(n_jobs)) %>%
  mutate(n_jobs_3m_avg = slide_dbl(n_jobs, ~ mean(.x, na.rm = TRUE), .before = 2, .complete = TRUE)) %>%
  ungroup()

# National summary ===================================================

# Volume of job ads across England
new_ads_national_roll <- I_Ons4digReg_clean %>%
  group_by(timePeriod) %>%
  summarise(n_jobs = sum(n_jobs, na.rm = TRUE)) %>%
  mutate(n_jobs_3m_avg = slide_dbl(n_jobs, ~ mean(.x, na.rm = TRUE), .before = 2, .complete = TRUE)) %>%
  ungroup()

# Volume of job ads across regions (for map)
new_ads_region_vol <- I_Ons4digReg_clean %>%
  group_by(region, timePeriod) %>%
  summarise(n_jobs = sum(n_jobs, na.rm = TRUE)) %>%
  ungroup()

# Growth rate of job ads across England since base (i.e. the first January 4 years prior to the latest date)
new_ads_national_growth <- new_ads_national_roll %>%
  # filter the data to the base date and onwards
  filter(timePeriod >= make_date(year(latest_date) - 4, 1, 1)) %>%
  mutate(n_jobs_base = if_else(is.na(n_jobs_3m_avg), NA_real_, first(na.omit(n_jobs_3m_avg))),
         growth_rate = (n_jobs_3m_avg - n_jobs_base) / n_jobs_base,
         growth_perc_from_base = round2(growth_rate * 100, 2)) %>%
  ungroup() %>%
  select(-n_jobs_base, -growth_rate)

# Population rate of job ads across England
new_ads_national_pop <- new_ads_national_roll %>%
  # Calculate 12-month rolling sum
  mutate(n_jobs_yr_sum = slide_dbl(n_jobs, ~ sum(.x, na.rm = TRUE), .before = 11, .complete = TRUE)) %>%
  # Only keep rows with a full 12-month rolling period
  filter(!is.na(n_jobs_yr_sum)) %>%
  # Filter the 12-month rolling period into quarters to line up with the APS data
  filter(month(timePeriod) %in% c(3, 6, 9, 12)) %>%
  # Add in a chart period column to allow joining of the APS data
  mutate(start_date = timePeriod %m-% months(11),
         chartPeriod = str_c(format(start_date, "%b %Y"), "-", format(timePeriod, "%b %Y"))) %>%
  # Join on APS economic activity data
  left_join(F_econ_Aps_clean %>% filter(region == "England") %>% select(chartPeriod, population), by = "chartPeriod") %>%
  mutate(pop_rate = n_jobs_yr_sum / population,
         pop_rate = round2(pop_rate * 100, 2))

# Employment rate of job ads across England
new_ads_national_job <- new_ads_national_pop %>%
  # Join on APS employment data
  left_join(F_emp_Aps_clean %>% select(chartPeriod, employment), by = "chartPeriod") %>%
  mutate(job_rate = n_jobs_yr_sum / employment,
         job_rate = round2(job_rate * 100, 2))

# Occupations summary ================================================

# Growth rate of job ads by SOC
new_ads_SOC_growth <- new_ads_SOC_roll %>%
  filter(timePeriod >= make_date(year(latest_date) - 4, 1, 1)) %>%
  group_by(soc_4_digit_code, soc_4_digit_label) %>%
  mutate(n_jobs_base = if_else(is.na(n_jobs_3m_avg), NA_real_, first(na.omit(n_jobs_3m_avg))),
         growth_rate = (n_jobs_3m_avg - n_jobs_base) / n_jobs_base,
         growth_perc_from_base = round2(growth_rate * 100, 2)) %>%
  ungroup() %>%
  select(-n_jobs_base, -growth_rate)

# Population rate of job ads by SOC across England
new_ads_SOC_pop <- new_ads_SOC_roll %>%
  mutate(n_jobs_yr_sum = slide_dbl(n_jobs, ~ sum(.x, na.rm = TRUE), .before = 11, .complete = TRUE)) %>%
  filter(!is.na(n_jobs_yr_sum)) %>%
  filter(month(timePeriod) %in% c(3, 6, 9, 12)) %>%
  mutate(start_date = timePeriod %m-% months(11),
         chartPeriod = str_c(format(start_date, "%b %Y"), "-", format(timePeriod, "%b %Y"))) %>%
  ungroup() %>%
  # Join on APS economic activity data
  left_join(F_econ_Aps_clean %>% filter(region == "England") %>% select(chartPeriod, population), by = "chartPeriod") %>%
  mutate(pop_rate = n_jobs_yr_sum / population,
         pop_rate = round2(pop_rate * 100000, 2)) # Per 100,000 population

# Population rate of job ads by SOC across regions (for map)
new_ads_region_SOC_pop <- I_Ons4digReg_clean %>%
  mutate(n_jobs_yr_sum = slide_dbl(n_jobs, ~ sum(.x, na.rm = TRUE), .before = 11, .complete = TRUE)) %>%
  filter(!is.na(n_jobs_yr_sum)) %>%
  filter(month(timePeriod) %in% c(3, 6, 9, 12)) %>%
  mutate(start_date = timePeriod %m-% months(11),
         chartPeriod = str_c(format(start_date, "%b %Y"), "-", format(timePeriod, "%b %Y"))) %>%
  ungroup() %>%
  # Join on APS economic activity data
  left_join(F_econ_Aps_clean %>% select(chartPeriod, region, geography_code, population), by = c("chartPeriod", "region")) %>%
  mutate(pop_rate = n_jobs_yr_sum / population,
         pop_rate = round2(pop_rate * 100000, 2)) # Per 100,000 population

# Employment rate of job ads by SOC
new_ads_SOC_job <- new_ads_SOC_pop %>%
  group_by(soc_4_digit_code, soc_4_digit_label, timePeriod) %>%
  # Join on employment data
  left_join(F_emp_soc_Aps_clean %>% select(chartPeriod, employment, soc_4_digit_code, soc_4_digit_label), by = c("chartPeriod", "soc_4_digit_code", "soc_4_digit_label")) %>%
  # Filter out rows with missing employment data (APS employment data by SOC starts from 2021)
  filter(!is.na(employment)) %>%
  mutate(job_rate = n_jobs_yr_sum / employment,
         job_rate = round2(job_rate * 100, 2))

# Add geometry data for map ==========================================

new_ads_region_vol_map <- new_ads_region_vol %>%
  filter(timePeriod == max(timePeriod)) %>%
  left_join(I_mapRegion_clean, by = c("region" = "RGN24NM"))

new_ads_region_SOC_map <- new_ads_region_SOC_pop %>%
  filter(timePeriod == max(timePeriod)) %>%
  left_join(I_mapRegion_clean, by = c("geography_code" = "RGN24CD", "region" = "RGN24NM"))

# Constant high demand ===============================================

# Find which occupations have been in the top 10% for every month in the past year

# Filter the data for the previous one year
new_ads_SOC_constant <- new_ads_SOC_roll %>%
  filter(timePeriod >= (latest_date - months(11)))

# Find the 90th percentile of the total number of job ads for each month
new_ads_90_percentile <- new_ads_SOC_constant %>%
  group_by(timePeriod) %>%
  summarise(percentile_90 = quantile(n_jobs, probs = 0.9, na.rm = TRUE)) %>%
  ungroup()

# Flag whether each value of n_job is in the top 10% per month
new_ads_SOC_constant <- new_ads_SOC_constant %>%
  left_join(new_ads_90_percentile, by = "timePeriod") %>%
  mutate(top_10 = replace_na(n_jobs_3m_avg >= percentile_90, FALSE)) %>%
  # Pull out the occupations that have been in the top 10% for every month
  select(soc_4_digit_code, soc_4_digit_label, timePeriod, top_10) %>%
  tidyr::pivot_wider(names_from = "timePeriod", values_from = "top_10") %>%
  filter(if_all(c(-soc_4_digit_code, -soc_4_digit_label), ~ .x == TRUE)) %>%
  select(soc_4_digit_code, soc_4_digit_label)

# Emerging high demand ===============================================

# Find which occupations have been in the top 15% in the latest 3-months but not in the top 15% in the previous 9-months

# Filter data for the previous 3-months and then the 9-months prior to that
new_ads_SOC_3months <- new_ads_SOC_roll %>%
  filter(timePeriod >= (latest_date %m-% months(2)) & timePeriod <= latest_date) %>%
  # Sum up job ads for each occupation
  group_by(soc_4_digit_code, soc_4_digit_label) %>% 
  summarise(n_jobs_sum = sum(n_jobs, na.rm = TRUE)) %>%
  ungroup() %>%
  # Pull out the occupations that are in the top 15%
  filter(n_jobs_sum >= quantile(n_jobs_sum, 0.85))

# Filter data for the 9-months prior to that
new_ads_SOC_9months <- new_ads_SOC_roll %>%
  filter(timePeriod >= (latest_date %m-% months(11)) & timePeriod <= (latest_date %m-% months(3))) %>%
  group_by(soc_4_digit_code, soc_4_digit_label) %>% 
  summarise(n_jobs_sum = sum(n_jobs, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(n_jobs_sum >= quantile(n_jobs_sum, 0.85))

# Pull out the occupations that are in the top 15% in the latest 3-months but not in the previous 9-months
new_ads_SOC_emerging <- new_ads_SOC_3months %>%
  anti_join(new_ads_SOC_9months, by = "soc_4_digit_code")

# Rank of occupations ================================================

# Filter the data for the previous one year
new_ads_SOC_ranking <- new_ads_SOC_roll %>%
  filter(timePeriod >= (latest_date - months(11))) %>%
  # Sum up job ads for each occupation
  group_by(soc_4_digit_code, soc_4_digit_label) %>% 
  summarise(n_jobs_sum = sum(n_jobs, na.rm = TRUE)) %>%
  ungroup() %>%
  # Create ranking column
  mutate(rank = dense_rank(desc(n_jobs_sum))) %>%
  arrange(rank)

# For loop to filter the ranked data for specific occupations
occupation_1 <- 1111
occupation_2 <- 4143
occupation_3 <- 5222

occupation_codes <- c(occupation_1, occupation_2, occupation_3)

ranking_table <- data.frame()

for (i in occupation_codes) {
  
  # Get the ranking value for the specified occupation
  ranking_value <- new_ads_SOC_ranking %>%
    filter(soc_4_digit_code == i) %>%
    pull(rank)
  
  # Filter the data for rows +/- 3 of ranking_value
  filtered_df <- new_ads_SOC_ranking %>%
    filter(between(rank, ranking_value - 3, ranking_value + 3)) %>%
    arrange(rank)
  
  # Append results
  ranking_table <- bind_rows(ranking_table, filtered_df)
  
  # Add a blank row between ranked occupation groups
  blank_row <- as.data.frame(matrix(NA, nrow = 1, ncol = ncol(ranking_table)))
  colnames(blank_row) <- colnames(ranking_table)
  
  ranking_table <- bind_rows(ranking_table, blank_row)
  
}

# Summaries ==========================================================

# National summary table
output_national <- bind_rows(
  
  new_ads_national_roll %>%
    select(timePeriod, n_jobs_3m_avg) %>%
    rename(value = n_jobs_3m_avg) %>%
    filter(timePeriod >= make_date(year(latest_date) - 4, 1, 1)) %>%
    mutate(metric = "national_3m_avg"),
  
  new_ads_national_growth  %>%
    select(timePeriod, growth_perc_from_base) %>%
    rename(value = growth_perc_from_base) %>%
    mutate(metric = "national_growth"),
  
  new_ads_national_pop  %>%
    select(timePeriod, pop_rate) %>%
    rename(value = pop_rate) %>%
    filter(timePeriod >= make_date(year(latest_date) - 4, 1, 1)) %>%
    mutate(metric = "national_pop"),
  
  new_ads_national_job  %>%
    select(timePeriod, job_rate) %>%
    rename(value = job_rate) %>%
    filter(timePeriod >= make_date(year(latest_date) - 4, 1, 1)) %>%
    mutate(metric = "national_job")
  )

# Occupations summary table
output_occupations <- bind_rows(
  
  new_ads_SOC_roll %>%
    select(timePeriod, soc_4_digit_code, soc_4_digit_label, n_jobs_3m_avg) %>%
    rename(value = n_jobs_3m_avg) %>%
    filter(timePeriod >= make_date(year(latest_date) - 4, 1, 1)) %>%
    mutate(metric = "soc_3m_avg"),
  
  new_ads_SOC_growth  %>%
    select(timePeriod, soc_4_digit_code, soc_4_digit_label, growth_perc_from_base) %>%
    rename(value = growth_perc_from_base) %>%
    mutate(metric = "soc_growth"),
  
  new_ads_SOC_pop  %>%
    select(timePeriod, soc_4_digit_code, soc_4_digit_label, pop_rate) %>%
    rename(value = pop_rate) %>%
    filter(timePeriod >= make_date(year(latest_date) - 4, 1, 1)) %>%
    mutate(metric = "soc_pop"),
  
  new_ads_SOC_job  %>%
    select(timePeriod, soc_4_digit_code, soc_4_digit_label, job_rate) %>%
    rename(value = job_rate) %>%
    mutate(metric = "soc_job")
)

# Write out Summaries ================================================

saveRDS(output_national, "./job_ads_page/job_ads_page_national_output.rds")

saveRDS(output_occupations, "./job_ads_page/job_ads_page_occupations_output.rds")

