# Get app data in detail from underlying data file
# The url will change for each new AY.

url <- "https://content.explore-education-statistics.service.gov.uk/api/releases/cd40bdd7-0927-4ccd-8197-37668e1da233/files/5754d4c2-88dd-41b1-b51a-9ca367925aa1"

zip_file <- tempfile(fileext = ".zip")

download.file(url, zip_file, mode = "wb")

# Extract files
unzip(zip_file, exdir = tempdir())

# List CSV files
csv_files <- list.files(tempdir(), pattern = "\\.csv$", full.names = TRUE)

# Read the apps CSV
I_app_detail <- read.csv(csv_files[1])

# tidy up data
F_app_detail <- I_app_detail |> 
  # select needed columns and rows
  select(time_period=year,area=learner_home_lad
,areaCode=learner_home_lad_code,age_summary,apps_level,ssa_tier_1,st_code,std_fwk_name,value = achievements) |> 
  mutate(st_code = paste0(st_code," - ",std_fwk_name)) |> 
  mutate(geographic_level="Local authority district") |> 
  # add dates
  mutate(chartPeriod = paste("AY", substr(time_period, 3, 4), "/", substr(time_period, 5, 6), sep = "")) %>%
  mutate(timePeriod = as.Date(paste("01 Aug", substr(time_period, 1, 4), sep = ""), format = "%d %b %Y")) %>%
  mutate(latest = case_when(
    timePeriod == max(timePeriod) ~ 1,
    timePeriod == (max(timePeriod) - lubridate::years(1)) ~ -1,
    TRUE ~ 0
  )) %>%
  #filter to last 5 years
  filter(timePeriod>=(max(timePeriod) - lubridate::years(4))) |> 
  select(-time_period,-std_fwk_name)

#Create England volumes
app_detail_England<-F_app_detail |> 
  group_by(age_summary,apps_level,ssa_tier_1,st_code,chartPeriod,timePeriod,latest) |> 
  summarise(value=sum(value)) |> 
  mutate(geographic_level="National",area="England",areaCode="E92000001")

# add on new LADUs/LSIP/CA areas
appsWithAreas <- addGeogs(bind_rows(F_app_detail,app_detail_England))

# group up all the stats for combined areas (including the new LAs)
groupedStats <- appsWithAreas %>%
  filter(newArea == 1) %>%
  ungroup() %>%
  select(-newArea) %>%
  mutate_at(vars(value), safe_numeric) %>% # Convert to numeric
  group_by(age_summary,apps_level,ssa_tier_1,st_code, chartPeriod, timePeriod, latest, geogConcat) %>%
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) 

# add back on original LADUs and format long
C_app_detail<- bind_rows(
  groupedStats,
  appsWithAreas %>%
    filter(newArea == 0)
) %>%
  mutate(Total="Total") |> 
  pivot_longer(
    cols = c(Total,age_summary, apps_level, ssa_tier_1, st_code),
    names_to = "breakdown",
    values_to = "subgroup",
    values_transform = list(subgroup = as.character)
  ) %>%
  group_by(
    chartPeriod,
    timePeriod,
    latest,
    geogConcat,
    subgroup,
    breakdown
  ) %>%
  summarise(
    value = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  mutate(valueText = as.character(value),
         metric="apprenticeships") 

#save output
saveRDS(C_app_detail, "Data/processing/C_app_detail.rds")