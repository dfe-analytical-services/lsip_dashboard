## 2.4 Skills imperative----
# These take a long time to run ~20mins
folder <- "2-13_skillsImperative2035"
dir_path <- paste0("./Data/", folder, "/")
skillsImpFileList <- list.files(dir_path)

read_dir <- function(dir_path, file_name, sheet_name, row_nos) {
  openxlsx::read.xlsx(paste0(dir_path, file_name), sheet = sheet_name, skipEmptyRows = T, rows = row_nos) %>%
    mutate(file_name = file_name)
}

# Industry future
I_wfIndF2 <-
  skillsImpFileList %>%
  purrr::map_df(~ read_dir(dir_path, .x, "Ind F2", 4:38))
# Occupation future
I_wfOccF2 <-
  skillsImpFileList %>%
  purrr::map_df(~ read_dir(dir_path, .x, "Occ F2", 4:49))
# Qualification future
I_wfQualF1 <-
  skillsImpFileList %>%
  purrr::map_df(~ read_dir(dir_path, .x, "Qual F1", 4:14))
# Area names
I_wfAreaName <-
  skillsImpFileList %>%
  purrr::map_df(~ read_dir(dir_path, .x, "Info", 2:5)) %>%
  filter(grepl("name", Scenario, fixed = TRUE))

employmentProjections <-
  # bind_rows(
  bind_rows(
    I_wfIndF2 %>%
      select(-"0") %>% # get rid of row caused by formula error in spreadsheet
      rename(subgroup = X1) %>%
      mutate(
        metric = "employmentProjection",
        breakdown = case_when(
          subgroup %in% c("Primary sector and utilities", "Manufacturing", "Construction", "Trade, accomod. and transport", "Business and other services", "Non-marketed services") ~ "Broad sector",
          subgroup == "All industries" ~ "Total",
          TRUE ~ "Industry"
        ),
        subgroup = case_when(
          subgroup == "All industries" ~ "Total",
          TRUE ~ subgroup
        )
      ) %>%
      mutate(across(`2010`:`2035`, ~ .x * 1000)), # put in unit numbers
    I_wfOccF2 %>%
      rename(subgroup = thousands) %>%
      mutate(
        metric = "employmentProjection",
        breakdown = case_when(
          grepl("[0-9]", subgroup) == TRUE ~ "Occupation (SOC2020 Sub-Major Group)",
          subgroup == "All occupations" ~ "Total",
          TRUE ~ "Occupation (SOC2020 Major Group)"
        ),
        subgroup = case_when(
          subgroup == "All occupations" ~ "Total",
          grepl("[0-9]", subgroup) == TRUE ~ gsub("(...)(.*)", "\\1- \\2", subgroup),
          TRUE ~ subgroup
        )
      ) %>%
      mutate(across(`2010`:`2035`, ~ .x * 1000)) %>% # put in unit numbers
      filter(breakdown != "Total"), # remove this total since we already have it fro industry
    I_wfQualF1 %>%
      mutate(metric = "employmentProjection", breakdown = "Qualification") %>%
      rename(subgroup = X1) %>%
      mutate(across(`2010`:`2035`, ~ .x * 1000)), # put in unit numbers
  ) %>%
  left_join(I_wfAreaName) %>%
  mutate(geogConcat = paste0(trimws(`.Main`, "both"), " ", sub(" name", "", Scenario))) %>% # get name
  mutate(geogConcat = case_when(
    trimws(`.Main`, "both") == "England" ~ "England",
    TRUE ~ geogConcat
  )) %>% 
  select(-`.Main`, -Scenario, -file_name) %>%
  tidyr::pivot_longer(!c("geogConcat", "breakdown", "subgroup", "metric"),
                      names_to = "timePeriod",
                      values_to = "value"
  ) %>%
  #Change to CA ending
  mutate(geogConcat = case_when(str_sub(geogConcat, start = -3)=="MCA" ~ gsub("MCA","CA",geogConcat),
                                TRUE ~ geogConcat))%>%
  mutate(geogConcat = case_when(
    geogConcat == "Cambridge and Peterborough CA" ~ "Cambridgeshire and Peterborough CA",
    geogConcat == "Essex Southend-on-Sea and Thurrock LSIP" ~ "Greater Essex LSIP",
    geogConcat == "Brighton and Hove East Sussex West Sussex LSIP" ~ "Sussex and Brighton LSIP",
    geogConcat == "South East Midlands LSIP" ~ "South-East Midlands LSIP",
    geogConcat == "Derbyshire and Nottinghamshire LSIP" ~ "East Midlands LSIP",
    TRUE ~ geogConcat
  )) %>% # correct different spellings
  # get time period
  rename(chartPeriod = timePeriod) %>%
  mutate(timePeriod = as.Date(paste("01", "Jan", chartPeriod, sep = " "), format = "%d %b %Y")) %>%
  mutate(latest = case_when(
    timePeriod == max(timePeriod) ~ 1,
    timePeriod == (max(timePeriod) - lubridate::years(1)) ~ -1,
    TRUE ~ 0
  )) %>%
  mutate(subgroup = case_when(
    subgroup == "Managers, directors and senior officials" ~ "1 - Managers, directors and senior officials",
    subgroup == "Professional occupations" ~ "2 - Professional occupations",
    subgroup == "Associate professional occupations" ~ "3 - Associate professional occupations",
    subgroup == "Administrative and secretarial occupations" ~ "4 - Administrative and secretarial occupations",
    subgroup == "Skilled trades occupations" ~ "5 - Skilled trades occupations",
    subgroup == "Caring, leisure and other service occupations" ~ "6 - Caring, leisure and other service occupations",
    subgroup == "Sales and customer service occupations" ~ "7 - Sales and customer service occupations",
    subgroup == "Process, plant and machine operatives" ~ "8 - Process, plant and machine operatives",
    subgroup == "Elementary occupations" ~ "9 - Elementary occupations",
    TRUE ~ subgroup
  ))

# use dorset lep for dorset lsip because dorset lsip has the wrong LAs and it is the same as dorset LEP anyway
# use Derbyshire and Nottinghamshire LSIP (renamed as East Midlands LSIP) for East Midlands CA. East midlands CA is new so is not in the data. D&N LSIP has the same LADs
# Use North East LEP for North East CA. North East CA is now covering both NE and North of Tyne and North East LEP covers the same LADs 
# Use  York and North Yorkshire LSIP for York and North Yorkshire CA. York and North Yorkshire CA is new and has the same LADs as the LSIP. 
# Use Greater London LSIP for Greater London Authority CA
employmentProjectionsCorrections <- employmentProjections %>%
  filter(geogConcat %in% c("Dorset LEP",
                           "East Midlands LSIP","York and North Yorkshire LSIP","North East LEP","Greater London LSIP")) %>%
  mutate(geogConcat = case_when(
    geogConcat == "Dorset LEP" ~ "Dorset LSIP",
    geogConcat == "East Midlands LSIP" ~ "East Midlands CA",
    geogConcat == "North East LEP" ~ "North East CA",
    geogConcat == "York and North Yorkshire LSIP" ~ "York and North Yorkshire CA",
    geogConcat == "Greater London LSIP" ~ "Greater London Authority CA",
    TRUE ~ "NA"
  ))%>%
  #add on north east lsip as north east LEP as they are the same geography now
  bind_rows(
    employmentProjections%>%
      filter(geogConcat =="North East LEP") %>%
      mutate(geogConcat ="North East LSIP")
  )

# combine them all
employmentProjectionsCombined <- bind_rows(
  employmentProjections %>% 
    filter(!geogConcat %in% c("Dorset LSIP", "North East CA", "Greater London LSIP"
                              )), # remove incorrect dorset LSIPs and outdated North East CA, all the LSIPs that have now been changed
  employmentProjectionsCorrections
)%>%#filter out LEPs
  filter(stringr::str_sub(geogConcat, start = -3) !="LEP")

# Get future year on year growth metric
empGrowth <- employmentProjectionsCombined %>%
  filter(chartPeriod >= 2023) %>%
  # get growth
  arrange(chartPeriod, timePeriod, latest) %>%
  group_by(geogConcat, metric, breakdown, subgroup) %>%
  mutate(value = (value - lag(value)) / lag(value)) %>%
  filter(chartPeriod != 2023) %>%
  mutate(metric = "employmentProjectionAnnualGrowth")

# Get 2024 to 2035 growth metric
empGrowth2024_2035 <- employmentProjectionsCombined %>%
  filter(chartPeriod %in% c(2024, 2035)) %>%
  # get growth
  arrange(chartPeriod, timePeriod, latest) %>%
  group_by(geogConcat, metric, breakdown, subgroup) %>%
  mutate(value = (value - lag(value)) / lag(value)) %>%
  filter(chartPeriod != 2024) %>%
  mutate(metric = "employmentProjectionGrowth2024to2035")

# combine all skills imperative  metrics
C_skillsImperative <- bind_rows(
  employmentProjectionsCombined,
  empGrowth,
  empGrowth2024_2035
) %>%
  mutate(valueText = as.character(value))