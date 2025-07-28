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
      mutate_at(vars(`2010`:`2035`),
                .funs = funs(. * 1000)
      ), # put in unit numbers
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
      mutate_at(vars(`2010`:`2035`),
                .funs = funs(. * 1000)
      ) %>% # put in unit numbers
      filter(breakdown != "Total"), # remove this total since we already have it fro industry
    I_wfQualF1 %>%
      mutate(metric = "employmentProjection", breakdown = "Qualification") %>%
      rename(subgroup = X1) %>%
      mutate_at(vars(`2010`:`2035`),
                .funs = funs(. * 1000)
      ), # put in unit numbers
  ) %>%
  left_join(I_wfAreaName) %>%
  mutate(geogConcat = paste0(trimws(`.Main`, "both"), " ", sub(" name", "", Scenario))) %>% # get name
  mutate(geogConcat = case_when(
    trimws(`.Main`, "both") == "England" ~ "England",
    TRUE ~ geogConcat
  )) %>% # correct error in file calling this a LEP instead of LSIP and getting name wrong
  select(-`.Main`, -Scenario, -file_name) %>%
  tidyr::pivot_longer(!c("geogConcat", "breakdown", "subgroup", "metric"),
               names_to = "timePeriod",
               values_to = "value"
  ) %>%
  mutate(geogConcat = case_when(
    geogConcat == "Sheffield City Region LEP" ~ "South Yorkshire LEP",
    geogConcat == "Derby, Derbyshire, Nottingham and Nottinghamshire LEP" ~ "D2N2 LEP",
    geogConcat == "Oxfordshire LEP" ~ "OxLEP LEP",
    geogConcat == "Gloucestershire LEP" ~ "GFirst LEP",
    geogConcat == "Greater Cambridge and Greater Peterborough LEP" ~ "The Business Board LEP",
    geogConcat == "Buckinghamshire Thames Valley LEP" ~ "Buckinghamshire LEP",
    geogConcat == "Cambridge and Peterborough MCA" ~ "Cambridgeshire and Peterborough MCA",
    geogConcat == "London Enterprise Panel LEP" ~ "The London Economic Action Partnership LEP",
    geogConcat == "York, North Yorkshire and East Riding LEP" ~ "York and North Yorkshire LEP",
    geogConcat == "Humber LEP" ~ "Hull and East Yorkshire LEP",
    geogConcat == "Essex Southend-on-Sea and Thurrock LSIP" ~ "Essex, Southend-on-Sea and Thurrock LSIP",
    geogConcat == "Brighton and Hove East Sussex West Sussex LSIP" ~ "Brighton and Hove, East Sussex, West Sussex LSIP",
    geogConcat == "Norfolk and Suffolk LSIP" ~ "New Anglia LSIP",
    geogConcat == "South East Midlands LSIP" ~ "South-East Midlands LSIP",
    geogConcat == "Gloucestershire LSIP" ~ "G First (Gloucestershire) LSIP",
    geogConcat == "Enterprise M3 LEP (including all of Surrey) LSIP" ~ "Enterprise M3 LSIP",
    TRUE ~ geogConcat
  )) %>% # correct different spellings
  # mutate(subgroup = trimws(gsub("[[:digit:]]+", "", subgroup))) %>% # remove numbers from soc codes for presentation
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
# use Worchestershire lsip for Worchestershire lep because Worchestershire lep is missing some LAs and it is the same as Worchestershire lsip anyway
# use Derbyshire and Nottinghamshire LSIP for East Midlands MCA. East midlands MCA is new so is not in the data. D&N LSIP has the same LADs
# Use North East LEP for North East MCA. North East MCA is now covering both NE and North of Tyne and North East LEP covers the same LADs 
# Use  York and North Yorkshire LSIP for York and North Yorkshire MCA. York and North Yorkshire MCA is new and has the same LADs as the LSIP. 
employmentProjectionsCorrections <- employmentProjections %>%
  filter(geogConcat %in% c("Dorset LEP", "Worcestershire LSIP", "Stoke-on-Trent and Staffordshire LSIP",
                           "Derbyshire and Nottinghamshire LSIP","York and North Yorkshire LSIP","North East LEP")) %>%
  mutate(geogConcat = case_when(
    geogConcat == "Dorset LEP" ~ "Dorset LSIP",
    geogConcat == "Worcestershire LSIP" ~ "Worcestershire LEP",
    geogConcat == "Stoke-on-Trent and Staffordshire LSIP" ~ "Stoke-on-Trent and Staffordshire LEP",
    geogConcat == "Derbyshire and Nottinghamshire LSIP" ~ "East Midlands MCA",
    geogConcat == "North East LEP" ~ "North East MCA",
    geogConcat == "York and North Yorkshire LSIP" ~ "York and North Yorkshire MCA",
    TRUE ~ "NA"
  ))

# calculate enterprise M3 LSIP from a combination of LEP areas that include Enterprise LSIP area, and then minus off the areas in that larger area that are not Enterprise Lsip
# Enterprise LSIP=(Enterprise LEP + C2C LEP + South east LEP)-(Brighton LSIP + Essex LSIP + Kent LSIP)
employmentProjectionsEntM3Lsip <- employmentProjections %>%
  filter(geogConcat %in% c("Enterprise M3 LEP", "Coast to Capital LEP", "South East LEP", "Brighton and Hove, East Sussex, West Sussex LSIP", "Essex, Southend-on-Sea and Thurrock LSIP", "Kent and Medway LSIP")) %>% # get all the relavant areas
  mutate(value = case_when(
    geogConcat %in% c("Enterprise M3 LEP", "Coast to Capital LEP", "South East LEP") ~ value,
    TRUE ~ 0 - value
  )) %>%
  select(-geogConcat) %>%
  group_by(subgroup, metric, breakdown, chartPeriod, timePeriod, latest) %>%
  summarise(value = sum(value)) %>%
  mutate(geogConcat = "Enterprise M3 LSIP")

# combine them all
employmentProjections <- bind_rows(
  employmentProjections %>% filter(!geogConcat %in% c("Dorset LSIP", "Enterprise M3 LSIP", "Worcestershire LEP", "Stoke-on-Trent and Staffordshire LEP", "North East MCA","North of Tyne MCA")), # remove incorrect dorset and Enterprise LSIPs. Remove legacy Worchester and Stoke on trent LEPs. Remove North East MCA and North of Tyne MCA as they have been merged to North East MCA
  employmentProjectionsCorrections,
  employmentProjectionsEntM3Lsip
)

# Get future year on year growth metric
empGrowth <- employmentProjections %>%
  filter(chartPeriod >= 2022) %>%
  # get growth
  arrange(chartPeriod, timePeriod, latest) %>%
  group_by(geogConcat, metric, breakdown, subgroup) %>%
  mutate(value = (value - lag(value)) / lag(value)) %>%
  filter(chartPeriod != 2022) %>%
  mutate(metric = "employmentProjectionAnnualGrowth")

# Get 2023 to 2035 growth metric
empGrowth2023_2035 <- employmentProjections %>%
  filter(chartPeriod %in% c(2023, 2035)) %>%
  # get growth
  arrange(chartPeriod, timePeriod, latest) %>%
  group_by(geogConcat, metric, breakdown, subgroup) %>%
  mutate(value = (value - lag(value)) / lag(value)) %>%
  filter(chartPeriod != 2023) %>%
  mutate(metric = "employmentProjectionGrowth2023to2035")

# combine all skills imperative  metrics
C_skillsImperative <- bind_rows(
  employmentProjections,
  empGrowth,
  empGrowth2023_2035
) %>%
  mutate(valueText = as.character(value))
