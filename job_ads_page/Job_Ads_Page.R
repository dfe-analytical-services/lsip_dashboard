# Set inputs =========================================================

# Set the base date for the growth rate calculations (currently set to January 2022)
base_date <- make_date(2022, 1, 1)

# Set constant demand cutoff (currently set to 5%)
constant_cutoff <- 0.05

# Set emerging demand cutoff (currently set to 15%)
emerging_cutoff <- 0.15

# Load the data ======================================================

# ONS job ads by region
folder <- "2-12_OnsProf"
sheet <- "Table 1"
startRow <- 5
new_ads_national <- openxlsx::read.xlsx(xlsxFile = file.path("Data", folder, list.files(path = file.path("Data", folder))),
                                   sheet = sheet, startRow = startRow)

# ONS job ads by region and 4-digit SOC
folder <- "2-12_OnsProf"
sheet <- "Table 3"
startRow <- 5
new_ads_SOC <- openxlsx::read.xlsx(xlsxFile = file.path("Data", folder, list.files(path = file.path("Data", folder))),
                                   sheet = sheet, startRow = startRow)

# APS economic activity data from NOMIS (to be used as population estimates). NOMIS filtered for:
# Dataset = annual population survey
# Geography = England and 9 English regions
# Date = All dates
# Cell = Table T01 Economic activity by age > Aged 16-64 > All
APS_econ_activity <- read.csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_17_1.data.csv?geography=2092957699,2013265926,2013265924,2013265927,2013265921,2013265922,2013265928,2013265929,2013265925,2013265923&cell=402720769&measures=20100,20701&select=date_name,geography_name,geography_code,cell_name,measures_name,obs_value,obs_status_name")

# APS employment data from NOMIS. NOMIS filtered for:
# Dataset = annual population survey
# Geography = England
# Date = All dates
# Cell = Table T01 Economic activity by age > Aged 16-64 > In employment
APS_employment <- read.csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_17_1.data.csv?geography=2092957699&cell=402721281&measures=20100,20701&select=date_name,geography_name,geography_code,cell_name,measures_name,obs_value,obs_status_name")

# APS employment data by SOC 2020 from NOMIS. NOMIS filtered for:
# Dataset = annual population survey - regional - occupation
# Geography = England
# Date = All dates
# occupation (SOC2020) = All 4-digit SOC codes
APS_employment_soc <- read.csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_218_1.data.csv?geography=2092957699&jtype=0&ftpt=0&etype=0&c_sex=0&soc2020_full=1...412&measure=1&measures=20100,20701&select=date_name,geography_name,geography_code,jtype_name,ftpt_name,etype_name,c_sex_name,soc2020_full_name,measure_name,measures_name,obs_value,obs_status_name")

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
         employment = obs_value)

# Clean APS employment by SOC data
APS_employment_soc_clean <- APS_employment_soc %>%
  janitor::clean_names() %>%
  # split out soc2020_full_name into SOC code and label
  separate(soc2020_full_name,
           into = c("soc_4_digit_code", "soc_4_digit_label"),
           sep = " : ",
           remove = TRUE) %>%
  select(date_name, geography_name, soc_4_digit_code, soc_4_digit_label, measures_name, obs_value) %>%
  filter(measures_name == "Value") %>%
  rename(chartPeriod = date_name,
         employment = obs_value)

# Clean geojson file
map_region_clean <- map_region %>%
  select(RGN24CD, RGN24NM, geometry)

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

# National summary ===================================================

# Volume of job ads across England
new_ads_national_roll <- new_ads_national_clean %>%
  group_by(timePeriod) %>%
  summarise(n_jobs = sum(n_jobs, na.rm = TRUE)) %>%
  mutate(n_jobs_3m_avg = slide_dbl(n_jobs, ~ mean(.x, na.rm = TRUE), .before = 2, .complete = TRUE)) %>%
  ungroup()

# Volume of job ads across regions (for map)
new_ads_region_vol <- new_ads_SOC_clean %>%
  group_by(region, timePeriod) %>%
  summarise(n_jobs = sum(n_jobs)) %>%
  mutate(n_jobs_3m_avg = slide_dbl(n_jobs, ~ mean(.x, na.rm = TRUE), .before = 2, .complete = TRUE)) %>%
  ungroup()

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
APS_employment_filtered <- APS_employment_clean %>%
  select(chartPeriod, employment)

new_ads_national_job <- new_ads_national_pop %>%
  # Join on APS employment data
  left_join(APS_employment_filtered, by = "chartPeriod") %>%
  mutate(job_rate = n_jobs_yr_sum / employment)

# Occupations summary ================================================

# Growth rate of job ads by SOC
new_ads_SOC_growth <- new_ads_SOC_roll %>%
  filter(timePeriod >= base_date) %>%
  group_by(soc_4_digit_code, soc_4_digit_label) %>%
  mutate(n_jobs_base = if_else(is.na(n_jobs_3m_avg), NA_real_, first(na.omit(n_jobs_3m_avg))),
         growth_rate = (n_jobs_3m_avg - n_jobs_base) / n_jobs_base) %>%
  ungroup() %>%
  select(-n_jobs_base)

# Population rate of job ads by SOC across England
new_ads_SOC_pop <- population_data (new_ads_SOC_roll, soc_4_digit_code, soc_4_digit_label) %>%
  # Join on APS economic activity data
  left_join(APS_econ_activity_national, by = "chartPeriod") %>%
  filter(!is.na(population)) %>%
  mutate(pop_rate = n_jobs_yr_sum / population,
         pop_rate = round2(pop_rate * 100000, 2)) # Per 100,000 population

# Population rate of job ads by SOC across regions (for map)

# Filter APS economic activity data
APS_econ_activity_regional <- APS_econ_activity_clean %>%
  select(chartPeriod, region, geography_code, population)

new_ads_region_SOC_pop <- population_data (new_ads_SOC_clean, region, soc_4_digit_code, soc_4_digit_label) %>%
  # Join on APS economic activity data
  left_join(APS_econ_activity_regional, by = c("chartPeriod", "region")) %>%
  filter(!is.na(population)) %>%
  mutate(pop_rate = n_jobs_yr_sum / population,
         pop_rate = round2(pop_rate * 100000, 2)) # Per 100,000 population

# Employment rate of job ads by SOC

# Filter APS employment SOC data
APS_employment_soc_filtered <- APS_employment_soc_clean %>%
  select(chartPeriod, employment, soc_4_digit_code)

new_ads_SOC_job <- new_ads_SOC_pop %>%
  group_by(soc_4_digit_code, soc_4_digit_label, timePeriod) %>%
  # Join on employment data
  left_join(APS_employment_soc_filtered, by = c("chartPeriod", "soc_4_digit_code")) %>%
  # Filter out rows with missing employment data (APS employment data by SOC starts from 2021)
  filter(!is.na(employment)) %>%
  mutate(job_rate = n_jobs_yr_sum / employment)

# Add geometry data for map ==========================================

new_ads_region_vol_map <- map_region_clean %>%
  left_join(new_ads_region_vol %>% filter(timePeriod == max(timePeriod)), by = c("RGN24NM" = "region")) %>%
  rename(areaCode = RGN24CD,
         areaName = RGN24NM,
         value = n_jobs_3m_avg) %>%
  select(-n_jobs)

new_ads_region_SOC_map <- new_ads_region_SOC_pop %>%
  filter(timePeriod == max(timePeriod)) %>%
  left_join(map_region_clean, by = c("geography_code" = "RGN24CD", "region" = "RGN24NM"))

# Constant high demand ===============================================

# Find which occupations have been in the top 5% for every month in the past year

# Filter the data for the previous one year
new_ads_SOC_constant <- new_ads_SOC_roll %>%
  filter(timePeriod >= (latest_date - months(11)))

# Find the 95th percentile of the total number of job ads for each month
new_ads_percentile <- new_ads_SOC_constant %>%
  group_by(timePeriod) %>%
  summarise(percentile = quantile(n_jobs, probs = (1-constant_cutoff), na.rm = TRUE)) %>%
  ungroup()

# Flag whether each value of n_job is in the top 5% per month
new_ads_SOC_constant <- new_ads_SOC_constant %>%
  left_join(new_ads_percentile, by = "timePeriod") %>%
  mutate(top_10 = case_when(
    is.na(n_jobs) ~ TRUE,
    TRUE ~ n_jobs >= percentile)) %>%
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
  select(soc_4_digit_code, period, n_jobs) %>%
  pivot_wider(names_from = period, values_from = n_jobs) %>%
  # Join on the constant data
  right_join(new_ads_SOC_constant, by = "soc_4_digit_code") %>%
  arrange(desc(n_jobs_latest)) %>%
  # Percentage change will be the latest month compared to the same month in the previous year
  mutate(`Percentage change` = (n_jobs_latest - n_jobs_previous) / n_jobs_previous,
         `Number of new job adverts` = format(n_jobs_latest, big.mark = ",")) %>%
  select(Occupation = soc_4_digit_label,
         `Number of new job adverts`,
         `Percentage change`)

# Emerging high demand ===============================================

# Find which occupations have been in the top 15% in the latest 3-months but not in the top 15% in the previous 9-months

# First calculate percentage change for the latest month compared to the same month in the previous year

# Filter data for the previous 3-months and then the 9-months prior to that
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
  select(soc_4_digit_code, period, n_jobs) %>%
  pivot_wider(names_from = period, values_from = n_jobs) %>%
  # Join on the emerging data
  right_join(new_ads_SOC_emerging, by = "soc_4_digit_code") %>%
  # Percentage change will be the latest month compared to the same month in the previous year
  mutate(`Percentage change` = (n_jobs_latest - n_jobs_previous) / n_jobs_previous,
         `Number of new job adverts` = format(n_jobs_latest, big.mark = ",")) %>%
  select(Occupation = soc_4_digit_label,
         `Number of new job adverts`,
         `Percentage change`)

# Rank of occupations ================================================

# Filter the data for the latest month
new_ads_SOC_ranking <- new_ads_SOC_roll %>%
  filter(timePeriod == latest_date) %>%
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
loop_iteration <- 1

for (selected_occupation in occupation_codes) {
  
  # Get the ranking value for the specified occupation
  ranking_value <- new_ads_SOC_ranking %>%
    filter(soc_4_digit_code == selected_occupation) %>%
    pull(rank)
  
  # Filter the data for rows +/- 3 of ranking_value
  filtered_df <- new_ads_SOC_ranking %>%
    filter(between(rank, ranking_value - 3, ranking_value + 3)) %>%
    arrange(rank)
  
  # Append results
  ranking_table <- bind_rows(ranking_table, filtered_df)
  
  # Add a blank row between ranked occupation groups (but not at the end)
  if (loop_iteration < length(occupation_codes)) {
    ranking_table <- ranking_table %>% add_row()
  }
  
  loop_iteration <- loop_iteration + 1
 
}

# Final formatted table for the dashboard page
new_ads_SOC_ranking <- new_ads_SOC_ranking %>%
  select(rank, soc_4_digit_label, n_jobs_sum) %>%
  mutate(n_jobs_sum = format(n_jobs_sum, big.mark = ",")) %>%
  rename(Rank = rank,
         Occupation = soc_4_digit_label,
         `Number of new job adverts` = n_jobs_sum)

# Summaries ==========================================================

# National summary table
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
         chartPeriod = format(timePeriod, "%b-%y"),
         timePeriod = as.character(timePeriod)) %>%
  select(geogConcat, metric, timePeriod, chartPeriod, value)

# Occupations summary table
output_occupations <- bind_rows(
  
  new_ads_SOC_roll %>%
    select(timePeriod, soc_4_digit_code, soc_4_digit_label, n_jobs_3m_avg) %>%
    rename(value = n_jobs_3m_avg) %>%
    filter(timePeriod >= make_date(year(latest_date) - 4, 1, 1)) %>%
    mutate(metric = "soc_3m_avg"),
  
  new_ads_SOC_growth  %>%
    select(timePeriod, soc_4_digit_code, soc_4_digit_label, growth_rate) %>%
    rename(value = growth_rate) %>%
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

saveRDS(output_national, "./job_ads_page/job_ads_page_national.rds")

saveRDS(output_occupations, "./job_ads_page/job_ads_page_occupations.rds")

saveRDS(new_ads_region_vol_map, "./job_ads_page/job_ads_page_map.rds")

saveRDS(new_ads_SOC_constant_output, "./job_ads_page/job_ads_page_constant.rds")

saveRDS(new_ads_SOC_emerging_output, "./job_ads_page/job_ads_page_emerging.rds")

saveRDS(new_ads_SOC_ranking, "./job_ads_page/job_ads_page_ranking.rds")

