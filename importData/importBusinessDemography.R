### 2.3.1 Business demography, UK ----
# Number of enterprise births, deaths and active
# Geography: England and LADS (as of April 2021)
firstRow <- 4

#Download data
url<-"https://www.ons.gov.uk/file?uri=/businessindustryandtrade/business/activitysizeandlocation/datasets/businessdemographyreferencetable/current/businessdemographyexceltables2024.xlsx"
tmp <- tempfile(fileext = ".xlsx")
download.file(url, tmp, mode = "wb")

# births
I_births_ONS19<-read.xlsx(tmp, sheet = "Table 1.1a", skipEmptyRows = T, startRow = firstRow)
I_births_ONS20<-read.xlsx(tmp, sheet = "Table 1.1b", skipEmptyRows = T, startRow = firstRow)
I_births_ONS2123<-read.xlsx(tmp, sheet = "Table 1.1c", skipEmptyRows = T, startRow = firstRow)
I_births_ONS24<-read.xlsx(tmp, sheet = "Table 1.1d", skipEmptyRows = T, startRow = firstRow)

# deaths
I_deaths_ONS19<-read.xlsx(tmp, sheet = "Table 2.1a", skipEmptyRows = T, startRow = firstRow)
I_deaths_ONS20<-read.xlsx(tmp, sheet = "Table 2.1b", skipEmptyRows = T, startRow = firstRow)
I_deaths_ONS2123<-read.xlsx(tmp, sheet = "Table 2.1c", skipEmptyRows = T, startRow = firstRow)
I_deaths_ONS24<-read.xlsx(tmp, sheet = "Table 2.1d", skipEmptyRows = T, startRow = firstRow)

# active
I_active_ONS19<-read.xlsx(tmp, sheet = "Table 3.1a", skipEmptyRows = T, startRow = firstRow)
I_active_ONS20<-read.xlsx(tmp, sheet = "Table 3.1b", skipEmptyRows = T, startRow = firstRow)
I_active_ONS2123<-read.xlsx(tmp, sheet = "Table 3.1c", skipEmptyRows = T, startRow = firstRow)
I_active_ONS24<-read.xlsx(tmp, sheet = "Table 3.1d", skipEmptyRows = T, startRow = firstRow)

formatBusiness <- function(x, y) {
  colnames(x)[1] <- "areaCode"
  colnames(x)[2] <- "area"
  x %>%
    tidyr::pivot_longer(!c("areaCode", "area"), names_to = "chartPeriod", values_to = "value") %>%
    mutate(areaCode = trimws(areaCode)) %>% # 2021 file has spaces in the code
    filter(substr(areaCode, 1, 2) == "E0" | areaCode == "E92000001") %>%
    mutate(
      metric = y,
      geographic_level = case_when(areaCode == "E92000001" ~ "National", TRUE ~ "Local authority district"),
      area = case_when(areaCode == "E92000001" ~ "England", TRUE ~ trimws(area))
    )
}

## Business rate fomrat
businessesWithAreas <-
  # combine all datasets
  bind_rows(
    formatBusiness(I_births_ONS19, "births"),
    formatBusiness(I_births_ONS20, "births"),
    formatBusiness(I_births_ONS2123, "births"),
    formatBusiness(I_births_ONS24, "births"),
    formatBusiness(I_deaths_ONS19, "deaths"),
    formatBusiness(I_deaths_ONS20, "deaths"),
    formatBusiness(I_deaths_ONS2123, "deaths"),
    formatBusiness(I_deaths_ONS24, "deaths"),
    formatBusiness(I_active_ONS19, "active"),
    formatBusiness(I_active_ONS20, "active"),
    formatBusiness(I_active_ONS2123, "active"),
    formatBusiness(I_active_ONS24, "active")
  ) %>%
  # add dates
  mutate(timePeriod = as.Date(paste("01", "Jan", chartPeriod, sep = " "), format = "%d %b %Y")) %>%
  mutate(chartPeriod = paste0("Dec ", as.numeric(chartPeriod) - 1, " - Dec ", chartPeriod)) %>%
  mutate(latest = case_when(
    timePeriod == max(timePeriod) ~ 1,
    timePeriod == (max(timePeriod) - lubridate::years(1)) ~ -1,
    TRUE ~ 0
  )) %>%
  #filter to last 5 years
  filter(timePeriod>=(max(timePeriod) - lubridate::years(4)))%>%
  addGeogs()

groupedStats <- businessesWithAreas %>%
  filter(newArea == 1) %>% # no need to group national or LAs that haven't changed
  ungroup() %>%
  select(-newArea) %>%
  group_by(chartPeriod, timePeriod, latest, geogConcat, metric) %>% # sum for each area
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) %>%
  rename_with(~ gsub("_1", "", .))

# add back on original LADUs and format
C_businesses <- bind_rows(
  groupedStats,
  businessesWithAreas %>%
    filter(newArea == 0)
) %>%
  select(-newArea) %>%
  # unpivot to get rates
  tidyr::pivot_wider(names_from = metric, values_from = value) %>%
  mutate(
    birthRate = births / (births + deaths + active),
    deathRate = deaths / (births + deaths + active)
  ) %>%
  # pivot back
  tidyr::pivot_longer(!c("chartPeriod", "timePeriod", "latest", "geogConcat"), names_to = "metric", values_to = "value") %>%
  mutate(valueText = as.character(value), breakdown = "Total", subgroup = "Total")

#save output
saveRDS(C_businesses, "Data/processing/C_businesses.rds")