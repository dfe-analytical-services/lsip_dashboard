### 2.3.2 ONS job adverts by SOC ----
folder <- "2-12_OnsProf"
sheet <- "Table 12"
I_Ons2digLA <- openxlsx::read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
I_Ons2digLA <- I_Ons2digLA[, c(1, 4, 2, 3, 5:ncol(I_Ons2digLA))]
sheet <- "Table 9"
I_Ons2digLsip <- openxlsx::read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 11"
I_Ons2digMca <- openxlsx::read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 8"
I_Ons2digRegion <- openxlsx::read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)

sheet <- "Table 1"
I_OnsEng <- openxlsx::read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 6"
I_OnsLA <- openxlsx::read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 3"
I_OnsLsip <- openxlsx::read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 5"
I_OnsMca <- openxlsx::read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)

formatVacancies <- function(x) {
  # Tidy
  reformat <- x %>%
    janitor::row_to_names(row_number = 4) # set columns
  geogLevel <- colnames(reformat)[1] # get geog level
  reformat <- reformat %>%
    rename(area = geogLevel) %>% # rename to match other datafiles
    mutate(
      geographic_level = geogLevel # set to the current geographic type
      , SOC2digit = paste0(
        {
          if ("SOC 2 digit code" %in% names(.)) `SOC 2 digit code` else NULL
        },
        " - ",
        {
          if ("SOC 2 digit label" %in% names(.)) `SOC 2 digit label` else NULL
        }
      ),
    ) %>%
    relocate(geographic_level, .before = area) %>%
    select(-contains("SOC 2 digit code"), -contains("SOC 2 digit label"))
  
  # Make long
  reformat %>%
    tidyr::pivot_longer(!c("geographic_level", "area", "SOC2digit"),
                 names_to = "time_period", values_to = "valueText"
    ) %>%
    mutate(area = case_when(
      area == "Sheffield City Region" ~ "South Yorkshire",
      area == "Derby, Derbyshire, Nottingham and Nottinghamshire" ~ "D2N2",
      area == "London" ~ "The London Economic Action Partnership",
      area == "Oxfordshire" & geographic_level == "Local Skills Improvement Plan" ~ "Oxfordshire",
      area == "Gloucestershire" & geographic_level == "Local Skills Improvement Plan" ~ "G First (Gloucestershire)",
      area == "Greater Cambridge and Greater Peterborough" ~ "The Business Board",
      area == "Cambridge and Peterborough" ~ "Cambridgeshire and Peterborough",
      area == "Buckinghamshire " ~ "Buckinghamshire",
      area == "North East" ~ "North East",
      area == "Norfolk and Suffolk " & geographic_level == "Local Skills Improvement Plan" ~ "New Anglia",
      area == "South East Midlands" & geographic_level == "Local Skills Improvement Plan" ~ "South-East Midlands",
      area == "Enterprise M3 LEP (including all of Surrey)" ~ "Enterprise M3",
      area == "Heart of the South West" & geographic_level == "Local Skills Improvement Plan" ~ "Heart of the South-West",
      area == "Stoke on Trent and Staffordshire" & geographic_level == "Local Skills Improvement Plan" ~ "Stoke-on-Trent and Staffordshire",
      area == "Norfolk and Suffolk" ~ "New Anglia",
      area == "Essex, Southend on Sea and Thurrock" & geographic_level == "Local Skills Improvement Plan" ~ "Essex, Southend-on-Sea and Thurrock",
      TRUE ~ area
    ))
}

# format data
# For the LAs, there are some old LA names within the data so we need to update those
# Also need to remove Local Authority Code column (X5 in 2digLA and X3 in LA)
advertsWithAreas <-
  # format and bind both LA files
  bind_rows(
    formatVacancies(I_Ons2digLA %>% 
                      rename(region = 1) %>% 
                      filter(!(region %in% c("Scotland", "Wales", "Northern Ireland", "Unknown", "London"))) %>% 
                      select(-region, -X5)%>%
                      relocate(X2)),#raw data column order had changed so readjusting
    formatVacancies(I_OnsLA %>% 
                      rename(region = 1) %>% 
                      filter(!(region %in% c("Scotland", "Wales", "Northern Ireland", "Unknown", "London"))) %>% 
                      select(-region, -X3))
  ) %>% # remove region code. get rid of london since it isn't really an LA.We get that from the region data
  # Use new LA names from 2011 areas
  left_join(I_LaLookup %>% distinct(LAD11NM, LAD23NM_11 = LAD23NM, LAD23CD_11 = LAD23CD), by = c("area" = "LAD11NM")) %>% # make new LAs
  # Use new LA names from 2021 areas
  left_join(I_LaLookup %>% distinct(LAD21NM, LAD23NM_21 = LAD23NM, LAD23CD_21 = LAD23CD), by = c("area" = "LAD21NM")) %>% # make new LAs
  # create flag for when the lad code has changed
  mutate(
    newArea = case_when(
      (LAD23NM_11 != area) | (LAD23NM_21 != area) ~ 1, TRUE ~ 0
    ),
    area = case_when(
      is.na(LAD23CD_11) == FALSE ~ LAD23NM_11,
      is.na(LAD23CD_21) == FALSE ~ LAD23NM_21,
      TRUE ~ area
    )
  ) %>%
  # select new name
  select(-LAD23CD_11, -LAD23CD_21, -LAD23NM_21, -LAD23NM_11) %>%
  left_join(distinct(C_LADLSIP, areaCode = LAD23CD, LAD23NM), by = c("area" = "LAD23NM")) # use to get consistent LA names

# Group up the new LAs
groupedStats <- advertsWithAreas %>%
  filter(newArea == 1) %>% # no need to group national or LAs that haven't changed
  ungroup() %>%
  select(-newArea) %>%
  mutate(valueText = as.numeric(valueText)) %>% # so we can sum
  mutate(timePeriod = as.Date(paste0("01 ", gsub("-", " ", time_period)), "%d %b %y")) %>%
  filter(timePeriod == max(timePeriod)) %>% # we only show the latest year at this level of detail
  select(-timePeriod) %>%
  group_by(geographic_level, area, SOC2digit, time_period, areaCode) %>% # sum for each LEP
  summarise(across(everything(), list(sum), na.rm = T)) %>%
  rename_with(~ gsub("_1", "", .)) %>%
  mutate(valueText = as.character(valueText)) # so we can merge

# Format all other area types
F_adverts <- bind_rows(
  formatVacancies(I_Ons2digLsip),
  formatVacancies(I_Ons2digMca), 
  # get england soc stats summed from regions
  formatVacancies(I_Ons2digRegion) %>%
    group_by(geographic_level, SOC2digit, time_period) %>%
    summarise(valueText = as.character(sum(as.numeric(valueText), na.rm = T))) %>%
    mutate(area = "England"),
  formatVacancies(I_OnsLsip),
  formatVacancies(I_OnsMca),
  formatVacancies(I_OnsEng),
  # add on LAs
  groupedStats,
  advertsWithAreas %>%
    filter(newArea == 0)
) %>%
  # change lep naming to match other datafiles
  mutate(geogConcat = case_when(
    geographic_level == "Local Authority District" ~ paste0(area, " LADU"),
    geographic_level == "Local Skills Improvement Plan" ~ paste0(area, " LSIP"),
    geographic_level == "Mayoral Combined Authorities" ~ paste0(area, " MCA"),
    TRUE ~ area
  )) %>%
  mutate(timePeriod = as.Date(paste0("01 ", gsub("-", " ", time_period)), "%d %b %y")) %>%
  rename(chartPeriod = time_period) %>%
  mutate(latest = case_when(
    timePeriod == max(timePeriod) ~ 1,
    timePeriod == (max(timePeriod) - lubridate::years(1)) ~ -1,
    TRUE ~ 0
  )) %>%
  #filter to last 5 years
  filter(timePeriod>=(max(timePeriod) - lubridate::years(4)))%>%
  select(-area, -geographic_level) %>%
  # make suppressed data zero to use in dashboard
  mutate(value = gsub("\\[x\\]", "0", valueText)) %>%
  mutate(value = gsub("\\[X\\]", "0", value)) %>%
  mutate(value = as.numeric(value))

# Get summaries
C_adverts <- bind_rows(
  # soc 2 digit
  F_adverts %>%
    filter(SOC2digit != " - ") %>%
    mutate(breakdown = "Occupation (SOC2020 Sub-Major Group)",
           SOC2digit = case_when(
            SOC2digit == "Unknown - Unknown" ~ "Unknown",
            TRUE ~ SOC2digit
            )
           ) %>%
    rename(subgroup = SOC2digit),
  # total
  F_adverts %>%
    filter(SOC2digit == " - ") %>%
    mutate(breakdown = "Total", subgroup = "Total"),
  # soc 1 digit
  F_adverts %>%
    filter(SOC2digit != " - ") %>%
    mutate(
      SOC1digitCode = substr(SOC2digit, 1, 1),
      SOC1digit = case_when(
        SOC1digitCode == "1" ~ "1 - Managers, directors and senior officials",
        SOC1digitCode == "2" ~ "2 - Professional occupations",
        SOC1digitCode == "3" ~ "3 - Associate professional occupations",
        SOC1digitCode == "4" ~ "4 - Administrative and secretarial occupations",
        SOC1digitCode == "5" ~ "5 - Skilled trades occupations",
        SOC1digitCode == "6" ~ "6 - Caring, leisure and other service occupations",
        SOC1digitCode == "7" ~ "7 - Sales and customer service occupations",
        SOC1digitCode == "8" ~ "8 - Process, plant and machine operatives",
        SOC1digitCode == "9" ~ "9 - Elementary occupations",
        SOC1digitCode == "U" ~ "Unknown",
        TRUE ~ "NULL"
      )
    ) %>%
    group_by(chartPeriod, timePeriod, geogConcat, latest, SOC1digit) %>%
    summarise(value = sum(value)) %>%
    mutate(breakdown = "Occupation (SOC2020 Major Group)", valueText = as.character(value)) %>%
    mutate(subgroup = SOC1digit)
) %>%
  select(-SOC2digit) %>%
  mutate(metric = "vacancies")

