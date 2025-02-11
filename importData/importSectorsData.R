####
# Title: Jobs and skills dashboard data load
# Author: Paul James
# Date: 27th Oct 2023
# Last updated: 9/11/2023
# Aim: Loads the original data sources used within the dashboard.
# Use: To update any non-nomis datasets, delete the relevant file within the relevant folder, and paste in the new data file. Rerun this file and then run TransformData.R. For nomis files just rerun this file and transform (data is fetched from the api)
# Sources: See the dashboard page Data sources for links to the datasets
# Notes: Ensure there is *only* the file(s) that will be used in each of the folders in ./Data/
# Best to run source("~/RProjects/ufs-jobs-and-skills-dashboard-internal/ExtractLoadData.R", echo=TRUE) rather than highlight and run
# Naming: I_ = imported data
# Running time: ~30secs
###

# Load libraries ----
library(openxlsx) # use read.xlsx, read.csv
library(tidyverse) # use map_df, mutate
library(nomisr) # use nomis api
library(data.table) # use %like%
library(readxl) # read xls

# 2. Datasets ----
## 2.1 Nomis datasets ----
### 2.1.1 Employment by occupation ----
# Read in Employment data.
# Query data
# Geography: UK
# Date: SOC2010: last 5 years annual. SOC2020: last 6 quarters (data doesn't go back further)
# Cell: 4digit occupation and sex

# SOC2010
I_empOcc2010 <- nomis_get_data(
  id = "NM_168_1",
  # To allign with the soc20 data we have to manually choose dates
  date = "latestMINUS16,latestMINUS12,latestMINUS8,latestMINUS4,latest",
  geography = "2092957697,2013265921...2013265932",
  C_SEX = "0...2",
  c_occpuk11h_0 = "0,1000001,100000,10000,1,2,10001,3...5,10002,6...12,10003,13,10004,14,15,10005,16...18,10006,19,20,10007,21,100001,10008,22,23,10009,24...28,10010,29,30,10011,31...36,2000000,200000,20000,37...41,20001,42...48,20002,49...54,20003,55,56,20004,57,2000001,20005,58...66,20006,67...70,20007,71,72,200002,20008,73...80,200003,20009,81...83,20010,84...89,20011,90...95,20012,96...99,20013,100,101,20014,102...104,20015,105...107,3000000,300000,30000,108...114,30001,115,116,30002,117,118,300001,30003,119...123,30004,124...128,300003,30005,129...134,300004,30006,135...141,30007,142,143,30008,144...146,300005,30009,147...149,30010,150,30011,151...159,30012,160...165,30013,166,30014,167...172,4000000,400000,40000,173...175,40001,176...180,40002,181...186,40003,187,188,40004,189,190,400001,40005,191...197,5000000,500000,50000,198...202,500001,50001,203...208,50002,209...213,50003,214...219,50004,220...224,50005,225,500005,50006,226...232,50007,233...235,50008,236,500009,50009,237...241,50010,242...244,50011,245...250,50012,251...254,6000000,600000,60000,255...259,60001,260...262,60002,263...270,600002,60003,271...275,60004,276,277,60005,278,279,60006,280,7000000,700000,70000,281...285,70001,286...291,70002,292,700002,70003,293...297,70004,298,8000000,800000,80000,299...307,80001,308...315,80002,316...322,80003,323...326,800002,80004,327...331,80005,332...335,80006,336...340,9000000,900000,90000,341...343,90001,344,90002,345...347,900002,90003,348,349,90004,350...356,90005,357...360,90006,361,362,90007,363,90008,364...369"
)

# SOC2020
I_empOcc2020 <- nomis_get_data(
  id = "NM_218_1",
  date = "latestMINUS16,latestMINUS12,latestMINUS8,latestMINUS4,latest",
  geography = "2092957697,2013265921...2013265932",
  C_SEX = "0...2",
  jtype = "0",
  ftpt = "0",
  etype = "0",
  soc2020_full = "0,1000001,100000,10000,1,2,10001,3...5,10002,6...13,10003,14,10004,15,10005,16...18,10006,19,20,100001,10007,21,22,10008,23...27,10009,28...30,10010,31...33,10011,34...42,2000000,200000,10012,43...48,10013,49...56,10014,57...64,10015,65,66,10016,67,68,10017,69,70,200001,10018,71,72,10019,73...79,10020,80...86,10021,87,10022,88...94,200002,10023,95...102,10024,103...107,200003,10025,108...110,10026,111...113,10027,114...119,10028,120,10029,121...125,10030,126...130,10031,131,132,10032,133...135,10033,136...139,3000000,300000,10034,140...146,10035,147,10036,148...150,300001,10037,151...155,10038,156...160,10039,161,162,10040,163,300002,10041,164...168,300003,10042,169...175,10043,176...178,10044,179...181,300004,10045,182,183,10046,184,10047,185...188,10048,189...193,10049,194...200,10050,201,10051,202...205,10052,206,207,4000000,400000,10053,208...210,10054,211...215,10055,216...221,10056,222...224,10057,225...227,400001,10058,228...234,5000000,500000,10059,235...239,500001,10060,240...243,10061,244...248,10062,249...254,10063,255...261,10064,262,500002,10065,263...270,10066,271...273,10067,274,500003,10068,275...278,10069,279...281,10070,282...287,10071,288...291,6000000,600000,10072,292...297,10073,298,299,10074,300...307,600001,10075,308...312,10076,313,314,10077,315,316,10078,317,10079,318,600002,10080,319,320,7000000,700000,10081,321...325,10082,326...331,10083,332,333,700001,10084,334...338,10085,339,8000000,800000,10086,340...345,10087,346,10088,347...352,10089,353...359,10090,360...363,10091,364,800001,10092,365...370,10093,371...373,10094,374...378,9000000,900000,10095,379...381,10096,382,383,10097,384...386,900001,10098,387,388,10099,389...395,10100,396...398,10101,399,400,10102,401...404,10103,405...412"
)

## 2.2 Raw datafiles ----
## 2.2 Other datasets ----
### 2.2.1 Ashe earnings data ----
# Read in earnings data
# https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/occupation4digitsoc2010ashetable14
folder <- "2_2_1_AsheEarnings23"
sheet <- "All"
I_ashe17 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[1]), sheet = sheet)
I_ashe18 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[2]), sheet = sheet)
I_ashe19 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[3]), sheet = sheet)
I_ashe20 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[4]), sheet = sheet)
I_ashe21 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[5]), sheet = sheet)
I_ashe22 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[6]), sheet = sheet)
I_ashe23 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[7]), sheet = sheet)

sheet <- "Male"
I_asheMale17 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[1]), sheet = sheet)
I_asheMale18 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[2]), sheet = sheet)
I_asheMale19 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[3]), sheet = sheet)
I_asheMale20 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[4]), sheet = sheet)
I_asheMale21 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[5]), sheet = sheet)
I_asheMale22 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[6]), sheet = sheet)
I_asheMale23 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[7]), sheet = sheet)

sheet <- "Female"
I_asheFemale17 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[1]), sheet = sheet)
I_asheFemale18 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[2]), sheet = sheet)
I_asheFemale19 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[3]), sheet = sheet)
I_asheFemale20 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[4]), sheet = sheet)
I_asheFemale21 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[5]), sheet = sheet)
I_asheFemale22 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[6]), sheet = sheet)
I_asheFemale23 <- read_excel(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))[7]), sheet = sheet)

### 2.2.1 Skills imperative----
# Read in employment by 4 digit occupation projections to 2035
# Currently unpublished
folder <- "2_2_1_SkillsImp2035"
sheetNum <- "skillsImp2035"
I_wfOccF2 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

### 2.2.2 Pathways - highest qualification ----
# Gives highest qualification for each SOC2010 code for a sample of learners
folder <- "2_2_2_pathways"
I_pathways <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))


### 2.2.3 AI exposure ----
# Read in AI exposure score by 4 digit SOC 2010 occupation- see here for more details:
# https://educationgovuk.sharepoint.com/sites/lvewp00254/SitePages/Impact-of-AI-on-UK-jobs-and-training.aspx
folder <- "2_2_3_1_AiExposure2010"
sheetNum <- "SOC Outputs"
I_AiExposure2010 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T, startRow = 6)
# and by soc2020
# https://educationgovuk.sharepoint.com/:x:/r/sites/UnitforFutureSkills/Shared%20Documents/Analytical%20Projects/Analysis/S016%20AI%20and%20Automation/03.%20Impact%20of%20AI%20on%20UK%20Jobs%20and%20training/06.%20Update%20with%20SOC2020/AI%20Exposure%20Model%202020.xlsx?d=we00e98bcb03647b2ac9b1551c06e6500&csf=1&web=1&e=jned0Q
folder <- "2_2_3_2_AiExposure2020"
sheetNum <- "Summary"
I_AiExposure2020 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T, startRow = 6)


### 2.2.3 Employer Skills Survey ----
# Get skills survey data (vacancies, hard to fill, skill shortages) for SOC2010 4 digit occs at UK level
# https://explore-education-statistics.service.gov.uk/data-tables/permalink/3d538e10-5baf-4d02-0e30-08dbd1400c33
folder <- "2_2_3_skillsSurvey"
I_skillsSurvey <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))


### 2.2.5 in demand ----
# Read in a list of soc2010 occupations with their In demand rating
# source: https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/1786e46f-f2ea-41f1-a0c1-fccd53875872
folder <- "2_2_5_inDemand"
sheetNum <- "OiD by SIC"
I_inDemand <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))

### 3.2 SOC - level lookup ----
# Read in the level rating for each SOC
folder <- "3_2_SOClevelLookup"
sheetNum <- "LevelLookup"
I_levelLookup <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

####
# Title: Jobs explorer dashboard data transform
# Author: Paul James
# Date: 1st September 2023
# Last updated: 9/11/2023
# Use: Transforms/format the raw data into the dataframes used in the dashboard.
# The dataframes are then saved as .csv in ./Data/AppData to be read by the dashboard app.
# Sources: The files created by running ExtractLoadData.R
# Naming: I_ import, F_ formatted (also used in transformation steps), C_ clean (for use in compiling the data for the dashboard)
# Running time: ~10secs
###

# Load libraries ----
library(tidyverse) # mostly dplyr
library(janitor) # use rows to names
library(sf) # use st_as_sf st_union sf_use_s2
library(lubridate) # use years
library(writexl) # use write_xlsx
library(janitor) # row to names
library(tidyr) # pivot_longer

# 2 Data cleaning ----
## 2.0 cleaning functions ----
# Cleaning function for nomis data
formatNomis <- function(x) {
  x %>%
    filter(!(GEOGRAPHY_TYPE == "countries" & GEOGRAPHY_NAME != "United Kingdom")) %>% # remove countries that aren't UK
    # Add dates. use end of year as timePeriod but set to Jan as that is what we want charted (ie a tick for every year)
    mutate(timePeriod = as.Date(paste("01 Jan ", str_sub(DATE_NAME, -4, -1), sep = ""), format = "%d %b %Y")) %>%
    mutate(latest = case_when(
      timePeriod == max(timePeriod) ~ 1,
      timePeriod == (max(timePeriod) - years(1)) ~ -1,
      TRUE ~ 0
    )) %>%
    mutate(geogConcat = case_when(
      GEOGRAPHY_NAME == "United Kingdom" ~ "UK",
      TRUE ~ GEOGRAPHY_NAME
    )) %>%
    rename(chartPeriod = DATE_NAME, value = OBS_VALUE) %>%
    select(-GEOGRAPHY_TYPE, -GEOGRAPHY_NAME, -GEOGRAPHY_CODE)
}

## 2.1 Tidy up sector lookup ----
C_sectorLookup <-
  # Combine 2010 and 2020 and promote headings
  bind_rows(
    I_sectorLookup2010 %>%
      row_to_names(1) %>%
      rename(SOCcode = `SOC2010 code`) %>%
      mutate(SOC = "SOC2010"),
    I_sectorLookup2020 %>%
      row_to_names(1) %>%
      rename(SOCcode = `SOC2020 code`) %>%
      mutate(SOC = "SOC2020")
  ) %>%
  select(-`Unit Group`) %>% # remove occ name - will get this later from a lookup to make sure correct
  pivot_longer(!c(SOCcode, SOC), names_to = "Sector", values_to = "inOut") %>% # make long
  # rename to those used in dashboard
  mutate(
    Sector = case_when(
      Sector == "AI shortlist" ~ "Artificial Intelligence",
      Sector == "STEM binary" ~ "STEM",
      Sector == "DSCET_Occs" ~ "Digital and Computing",
      Sector == "Eng Bio shortlist" ~ "Engineering Biology",
      Sector == "Future Telecoms shortlist" ~ "Future Telecommunications",
      Sector == "Quantum shortlist" ~ "Quantum",
      Sector == "Semiconductors shortlist" ~ "Semiconductors",
      Sector == "Critical_Tech_Occs" ~ "Critical Technologies",
      TRUE ~ "notNeeded"
    )
    # remove all those in the Not acategory
    , inOut = case_when(
      (word(inOut, 1) %in% c("Not", "Other") | inOut == "STEM M&H") ~ "notNeeded",
      TRUE ~ inOut
    )
  ) %>%
  filter(inOut != "notNeeded", Sector != "notNeeded") %>%
  # Add on occupation names
  mutate(SOCcode = as.numeric(SOCcode)) %>%
  left_join(
    bind_rows(
      I_socNames2010 %>% select(occ = Group.Title, SOCcode = Unit.Group) %>% mutate(SOC = "SOC2010"),
      I_socNames2020 %>% select(occ = SOC2020.Group.Title, SOCcode = SOC2020.Unit.Group) %>% mutate(SOC = "SOC2020")
    )
  ) %>%
  # get just what we use in dashboard
  mutate(
    SectorSocYear = paste(Sector, SOC),
    occYear = paste(occ, SOC)
  ) %>%
  select(Sector, SOC, SectorSocYear, SOCcode, occYear)
write.csv(C_sectorLookup, file = "Data\\AppData\\C_sectorLookup.csv", row.names = FALSE)

## 2.2 Nomis dataset ----
### 2.2.1 Employment by occupation ----
# split Soc lookup table into 2010 and 2020 for joins later
F_sectorLookup2020 <- C_sectorLookup %>%
  filter(SOC == "SOC2020") %>%
  rename(SOC2020code = SOCcode)
F_sectorLookup2010 <- C_sectorLookup %>%
  filter(SOC == "SOC2010") %>%
  rename(SOC2010code = SOCcode)

# tidy raw data into format used in dashboard (filter to only relevant data, align column names, format to uniform structure)
F_empOcc2010 <- formatNomis(I_empOcc2010) %>%
  filter(MEASURES_NAME == "Value", MEASURE_NAME == "Count") %>% # get only count (not rate) data
  mutate(
    breakdown = case_when(
      C_SEX_NAME == "All persons" & C_OCCPUK11H_0_NAME == "Total" ~ "Total",
      C_SEX_NAME != "All persons" & C_OCCPUK11H_0_NAME == "Total" ~ "Sex",
      TRUE ~ paste(C_OCCPUK11H_0_TYPE, "SOC2010")
    ),
    subgroup = case_when(
      C_SEX_NAME == "All persons" & C_OCCPUK11H_0_NAME == "Total" ~ "Total",
      C_SEX_NAME != "All persons" & C_OCCPUK11H_0_NAME == "Total" ~ C_SEX_NAME,
      TRUE ~ C_OCCPUK11H_0_NAME
    ),
    # get sex breakdown
    breakdown2 = case_when(
      C_SEX_NAME != "All persons" & C_OCCPUK11H_0_NAME != "Total" ~ "Sex",
      TRUE ~ "Total"
    ),
    ids = case_when(
      C_SEX_NAME != "All persons" & C_OCCPUK11H_0_NAME != "Total" ~ C_SEX_NAME,
      TRUE ~ "Total"
    ),
    metric = "Employed",
    valueText = as.character(value),
    subgroup = trimws(gsub("[[:digit:]]+", "", subgroup)), # remove soc codes from name
    SOC2010code = as.numeric(sub(" .*$", "", C_OCCPUK11H_0_NAME)), # get just soc code
    latest = case_when(
      chartPeriod == "Jul 2020-Jun 2021" ~ 1,
      chartPeriod == "Jul 2019-Jun 2020" ~ -1,
      TRUE ~ 0
    ) # doing this to get the data in the table in the dashboard to be 2020 to match the earnings data
  ) %>%
  select(geogConcat, chartPeriod, timePeriod, latest, metric, breakdown, subgroup, breakdown2, ids, value, valueText, SOC2010code)

# add on sectors by repeating any occs matched to a sector then grouping them up into the sectors
F_empOcc2010Sector <- F_empOcc2010 %>%
  inner_join(F_sectorLookup2010, relationship = "many-to-many") %>%
  filter(subgroup != "Total") %>%
  group_by(geogConcat, chartPeriod, timePeriod, latest, metric, breakdown, breakdown2, ids, Sector) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  rename(subgroup = Sector)

# soc2020 formatting
F_empOcc2020 <- formatNomis(I_empOcc2020) %>%
  filter(MEASURES_NAME == "Value", MEASURE_NAME == "Count") %>%
  mutate(
    breakdown = case_when(
      C_SEX_NAME == "All persons" & SOC2020_FULL_NAME == "Total" ~ "Total",
      C_SEX_NAME != "All persons" & SOC2020_FULL_NAME == "Total" ~ "Sex",
      TRUE ~ paste(SOC2020_FULL_TYPE, "SOC2020")
    ),
    subgroup = case_when(
      C_SEX_NAME == "All persons" & SOC2020_FULL_NAME == "Total" ~ "Total",
      C_SEX_NAME != "All persons" & SOC2020_FULL_NAME == "Total" ~ C_SEX_NAME,
      TRUE ~ SOC2020_FULL_NAME
    ),
    breakdown2 = case_when(
      C_SEX_NAME != "All persons" & SOC2020_FULL_NAME != "Total" ~ "Sex",
      TRUE ~ "Total"
    ),
    ids = case_when(
      C_SEX_NAME != "All persons" & SOC2020_FULL_NAME != "Total" ~ C_SEX_NAME,
      TRUE ~ "Total"
    ),
    metric = "Employed",
    subgroup = gsub("[[:digit:]]+", "", subgroup),
    subgroup = trimws(gsub(":", "", subgroup)),
    SOC2020code = as.numeric(sub(" .*$", "", SOC2020_FULL_NAME))
  ) %>%
  select(geogConcat, chartPeriod, timePeriod, metric, latest, breakdown, subgroup, breakdown2, ids, value, SOC2020code)

# add on sectors by repeating any occs matched to a sector then grouping them up into the sectors
F_empOcc2020Sector <- F_empOcc2020 %>%
  inner_join(F_sectorLookup2020, relationship = "many-to-many") %>%
  filter(subgroup != "Total") %>%
  group_by(geogConcat, chartPeriod, timePeriod, latest, metric, breakdown, breakdown2, ids, Sector) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  rename(subgroup = Sector)

# Get year on year growth metric for sectors
F_empOccgrowth <- bind_rows(F_empOcc2010Sector, F_empOcc2020Sector) %>% # combine both SOCs so have a full time series
  # order
  arrange(chartPeriod, timePeriod, latest) %>%
  group_by(geogConcat, metric, subgroup, breakdown2, ids) %>%
  # get growth
  mutate(value = (value - lag(value)) / lag(value)) %>%
  filter(!chartPeriod %in% c("Jul 2012-Jun 2013")) %>% # ignore first year since no year before to get growth
  mutate(metric = "EmployedAnnualGrowth")

# Combine datasets
C_employment <- bind_rows(
  bind_rows(
    F_empOcc2010,
    F_empOcc2010Sector
  ) %>% mutate(source = "SOC2010"),
  bind_rows(
    F_empOcc2020,
    F_empOcc2020Sector
  ) %>% mutate(source = "SOC2020"),
  F_empOccgrowth
) %>%
  mutate(valueText = as.character(value)) %>%
  filter(breakdown %in% c("4-digit occupation SOC2010", "4-digit occupation SOC2020", "Total", "Sex"))

## 2.3 Other datasets ----
### 2.3.1 Skills imperative 2035 ----
# Clean up and reformat data and then caluclate some metrics; annual growth and growth to 2035

# Clena data
F_employmentProjections <-
  I_wfOccF2 %>%
  # add annual total
  bind_rows(I_wfOccF2 %>%
              group_by(year) %>%
              summarise(jobs = (sum(jobs))) %>%
              mutate(occupation_name = "Total")) %>%
  rename(
    subgroup = occupation_name,
    chartPeriod = year,
    value = jobs,
    SOC2020code = soc_code
  ) %>%
  mutate(
    metric = "employmentProjection",
    geogConcat = "UK"
  ) %>%
  # get time period
  mutate(timePeriod = as.Date(paste("01", "Jan", chartPeriod, sep = " "), format = "%d %b %Y")) %>%
  mutate(latest = case_when(
    timePeriod == max(timePeriod) ~ 1,
    timePeriod == (max(timePeriod) - years(1)) ~ -1,
    TRUE ~ 0
  ))

# Get future year on year growth metric
F_empGrowth <- F_employmentProjections %>%
  filter(chartPeriod >= 2022) %>% # only need growth for future
  # order
  arrange(chartPeriod, timePeriod, latest) %>%
  group_by(geogConcat, metric, SOC2020code, subgroup) %>%
  # get growth
  mutate(value = (value - lag(value)) / lag(value)) %>%
  filter(chartPeriod != 2022) %>% # ignore first year since no year before to get growth
  mutate(metric = "employmentProjectionAnnualGrowth")

# Get 2023 to 2035 growth metric
F_empGrowth2023_2035 <- F_employmentProjections %>%
  filter(chartPeriod %in% c(2023, 2035)) %>%
  # order
  arrange(chartPeriod, timePeriod, latest) %>%
  group_by(geogConcat, metric, SOC2020code, subgroup) %>%
  # get growth
  mutate(value = (value - lag(value)) / lag(value)) %>%
  filter(chartPeriod != 2023) %>% # ignore first year since no year before to get growth
  mutate(metric = "employmentProjectionGrowth2023to2035")

# Get 2023 to 2030 growth metric
F_empGrowth2023_2030 <- F_employmentProjections %>%
  filter(chartPeriod %in% c(2023, 2030)) %>%
  # order
  arrange(chartPeriod, timePeriod, latest) %>%
  group_by(geogConcat, metric, SOC2020code, subgroup) %>%
  # get growth
  mutate(value = (value - lag(value)) / lag(value)) %>%
  filter(chartPeriod != 2023) %>% # ignore first year since no year before to get growth
  mutate(metric = "employmentProjectionGrowth2023to2030")

# combine all skills imperative  metrics
F_skillsImperative <- bind_rows(
  F_employmentProjections,
  F_empGrowth,
  F_empGrowth2023_2035,
  F_empGrowth2023_2030
) %>%
  mutate(valueText = as.character(value))

# Now get all 4 digit soc codes (skills imp is only in 2 digit SOC) and list the growth for them all based on their 2 digit code. So for SOC code 1112, we would apply growth for 11 group
F_skillsImperative4digit <-
  # get 2020 emp data
  bind_rows(
    F_empOcc2020,
    F_empOcc2020Sector
  ) %>%
  mutate(source = "SOC2020") %>%
  filter(breakdown == "4-digit occupation SOC2020" | breakdown == "Total", latest == 1, breakdown2 == "Total") %>% # get one row per 4 digit code only
  mutate(SOC20202digit = case_when(
    breakdown == "Total" & subgroup == "Total" ~ 99 # code for total
    , breakdown == "4-digit occupation SOC2020" ~ as.numeric(substr(SOC2020code, 1, 2)),
    TRUE ~ NA
  )) %>%
  filter(is.na(SOC20202digit) == FALSE) %>% # remove sector groups
  select(geogConcat, SOC2020code, subgroup, employed = value) %>%
  left_join(F_skillsImperative %>%
              select(-subgroup) %>%
              filter(metric %in% c("employmentProjectionGrowth2023to2035", "employmentProjectionGrowth2023to2030")), relationship = "many-to-many") %>%
  select(geogConcat, SOC2020code, subgroup, metric, value, employed)

# add on sectors by joining to the lookup, then grouping and recalculating the stats
C_skillsImperative <- F_skillsImperative4digit %>%
  bind_rows(F_skillsImperative4digit %>%
              inner_join(F_sectorLookup2020, relationship = "many-to-many") %>%
              mutate(employed20xx = employed * (1 + value)) %>% # calculate the projected employment based on the growth rate
              group_by(geogConcat, Sector, metric) %>%
              summarise(value = (sum(employed20xx, na.rm = TRUE) - (sum(employed, na.rm = TRUE))) / sum(employed, na.rm = TRUE)) %>% # then calculate the growth for the group
              rename(subgroup = Sector)) %>%
  mutate(
    breakdown = "4-digit occupation SOC2020"
    # add 9999 code for all occs to use in join later
    , SOC2020code = case_when(
      subgroup == "Total" ~ 9999,
      TRUE ~ SOC2020code
    )
  ) %>%
  select(-employed)

### 2.3.2 Employer Skills Survey ----
# Tidy the data into common format
C_skillsSurvey <- I_skillsSurvey %>%
  # clean
  mutate(
    SOC2010code = as.numeric(sub(" .*$", "", soc_4_labelled)),
    geogConcat = "UK"
  ) %>%
  # Add on occupation names
  left_join(C_sectorLookup %>% filter(SOC == "SOC2010") %>% select(SOC2010code = SOCcode, subgroup = occYear) %>% unique()) %>%
  # Add dates
  mutate(timePeriod = as.Date(paste("01 Jan", substr(time_period, 1, 4), sep = ""), format = "%d %b %Y")) %>%
  mutate(latest = case_when(
    timePeriod == max(timePeriod) ~ 1,
    timePeriod == (max(timePeriod) - years(1)) ~ -1,
    TRUE ~ 0
  )) %>%
  # fornat as numbers
  mutate(vac_sum = as.numeric(vac_sum), htfv_den = as.numeric(htfv_den) / 100, ssv_den = as.numeric(ssv_den) / 100, chartPeriod = as.character(time_period)) %>%
  select(timePeriod, chartPeriod, latest, geogConcat, subgroup, SOC2010code, vac_sum, htfv_sum, htfv_den, ssv_sum, ssv_den, sample_occupations_estab_has_vacancy)

### 2.3.3 Pathways ----
# Tidy up data and add in all the ids for the sunburst chart
F_pathways <- I_pathways %>%
  select(subgroup = group, STEM = stem, pathway, SubjectHEFE = top_10_subjects, value = percentage, englandPer = all_occupations_percentage) %>%
  mutate(
    # id for sunburst
    ids = case_when(
      is.na(pathway) == TRUE ~ STEM,
      is.na(SubjectHEFE) == TRUE ~ paste0(STEM, " - ", pathway),
      TRUE ~ paste0(STEM, " - ", pathway, " - ", SubjectHEFE)
    )
    # label for sunburst segment
    , label = case_when(
      is.na(pathway) == TRUE ~ STEM,
      is.na(SubjectHEFE) == TRUE ~ pathway,
      TRUE ~ SubjectHEFE
    ),
    parents = case_when(
      is.na(pathway) == TRUE ~ "",
      is.na(SubjectHEFE) == TRUE ~ STEM,
      TRUE ~ paste0(STEM, " - ", pathway)
    )
    # colour for sunburst segment
    , colors = case_when(
      is.na(pathway) == TRUE & STEM == "STEM" ~ "#12436D",
      is.na(pathway) == TRUE & STEM == "Not STEM" ~ "#000000",
      is.na(SubjectHEFE) == TRUE & STEM == "STEM" ~ "#2073BC",
      is.na(SubjectHEFE) == TRUE & STEM == "Not STEM" ~ "#3D3D3D",
      STEM == "STEM" ~ "#C6DFF6",
      STEM == "Not STEM" ~ "#BFBFBF"
    )
  ) %>%
  group_by(label, value) %>% # group so label wrap is for each row
  mutate(
    labelPer = paste0(paste(strwrap(label, 20), collapse = " <br> "), "<br>", round(value * 100, 0), "%"),
    idsPer = paste0(ids, "<br>", round(value * 100, 0), "%")
  )

# 4 Create datasets used by the app----

## 4.1 C_time ----
# This is used in the line charts. It contains historic data for each metric and area.
C_time <- C_employment %>%
  mutate(
    valueText = as.character(valueText),
    subgroup = case_when(
      subgroup != "Total" ~ paste0(subgroup, " ", str_sub(breakdown, -7, -1)),
      TRUE ~ paste0("All occupations ", source)
    )
  ) %>%
  bind_rows(C_skillsSurvey %>%
              # filter to only metrics we need
              select(timePeriod, chartPeriod, latest, geogConcat, subgroup, SOC2010code, ssv_den, vac_sum) %>%
              # make data long
              pivot_longer(!c("timePeriod", "chartPeriod", "latest", "geogConcat", "subgroup", "SOC2010code"),
                           names_to = "metric",
                           values_to = "value"
              ))

write.csv(C_time, file = "Data\\AppData\\C_time.csv", row.names = FALSE)

## 4.2 C_table ----
# This is used to create the dashboard summary table and also the KPIs
# We get the employment volumes then join on all the other metrics

### 4.2.1 Employment stats ----
# get only the latest year of soc10 and soc20 data for the table
F_tableEmployment <- C_employment %>%
  filter(
    latest == 1,
    metric == "Employed",
    breakdown %in% c("4-digit occupation SOC2020", "4-digit occupation SOC2010", "Total"),
    breakdown2 == "Total"
  ) %>%
  mutate(breakdown = case_when(
    breakdown == "Total" & chartPeriod == "Jul 2023-Jun 2024" ~ "4-digit occupation SOC2020",
    breakdown == "Total" & chartPeriod == "Jul 2020-Jun 2021" ~ "4-digit occupation SOC2010",
    TRUE ~ breakdown
  )) %>%
  select(geogConcat, subgroup, breakdown, Employed = value, SOC2020code, SOC2010code) %>%
  # add on employment growth
  left_join(C_employment %>%
              filter(
                latest == -1, # get year before
                metric == "Employed",
                breakdown %in% c("4-digit occupation SOC2020", "4-digit occupation SOC2010", "Total"),
                breakdown2 == "Total"
              ) %>%
              mutate(breakdown = case_when(
                breakdown == "Total" & chartPeriod == "Jul 2022-Jun 2023" ~ "4-digit occupation SOC2020",
                breakdown == "Total" & chartPeriod == "Jul 2019-Jun 2020" ~ "4-digit occupation SOC2010",
                TRUE ~ breakdown
              )) %>%
              select(geogConcat, subgroup, breakdown, EmployedLast = value)) %>%
  # calculate growth
  mutate(growth = (Employed - EmployedLast) / EmployedLast) %>%
  # add % female
  left_join(C_employment %>%
              filter(
                latest == 1,
                metric == "Employed",
                ((breakdown %in% c("4-digit occupation SOC2020", "4-digit occupation SOC2010", "Total") & ids == "Females") |
                   breakdown == "Sex" & subgroup == "Females")
              ) %>%
              mutate(
                breakdown = case_when(
                  chartPeriod == "Jul 2023-Jun 2024" ~ "4-digit occupation SOC2020",
                  chartPeriod == "Jul 2020-Jun 2021" ~ "4-digit occupation SOC2010",
                  TRUE ~ breakdown
                ),
                subgroup = case_when(
                  subgroup == "Females" ~ "Total",
                  TRUE ~ subgroup
                )
              ) %>%
              select(geogConcat, subgroup, breakdown, EmployedFemale = value)) %>%
  # caluclate the female percenatge
  mutate(
    femalePerc = EmployedFemale / Employed,
    SOC20202digit = case_when(
      breakdown == "4-digit occupation SOC2020" & subgroup == "Total" ~ 99 # code for total
      , breakdown == "4-digit occupation SOC2020" ~ as.numeric(substr(SOC2020code, 1, 2)),
      TRUE ~ NA
    ),
    SOC2010code = case_when(
      breakdown == "4-digit occupation SOC2010" & subgroup == "Total" ~ 9999 # code for total
      , TRUE ~ SOC2010code
    ),
    SOC2020code = case_when(
      breakdown == "4-digit occupation SOC2020" & subgroup == "Total" ~ 9999 # code for total
      , TRUE ~ SOC2020code
    )
  )

### 4.2.2 add stats on to table ----
C_table <- F_tableEmployment %>%
  # add skills imp growth to 2030 and 2035
  left_join(
    C_skillsImperative %>%
      filter(metric == "employmentProjectionGrowth2023to2035") %>%
      rename(employmentProjectionGrowth2023to2035 = value) %>%
      select(-metric)
  ) %>%
  left_join(
    C_skillsImperative %>%
      filter(metric == "employmentProjectionGrowth2023to2030") %>%
      rename(employmentProjectionGrowth2023to2030 = value) %>%
      select(-metric)
  ) %>%
  ### 4.2.3 Skills survey data ----
# add skills survey data
left_join(
  C_skillsSurvey %>% filter(latest == 1, is.na(SOC2010code) == FALSE) %>% select(-subgroup, -timePeriod, -chartPeriod)
) %>%
  select(-SOC2020code, -SOC2010code, -SOC20202digit) %>%
  mutate(subgroup = case_when(
    subgroup != "Total" ~ paste0(subgroup, " ", str_sub(breakdown, -7, -1)),
    TRUE ~ paste0("All occupations ", str_sub(breakdown, -7, -1))
  ))

write.csv(C_table, file = "Data\\AppData\\C_table.csv", row.names = FALSE)

## 4.3 C_pathways ----
# add national to table
C_pathways <- F_pathways
write.csv(C_pathways, file = "Data\\AppData\\C_pathways.csv", row.names = FALSE)

# 5 Tidy up the app text ----
## 5.1 Tidy up data table text ----
names(I_DataTable) <- gsub(".", " ", names(I_DataTable), fixed = TRUE)
C_DataTable <- I_DataTable %>% rename(Data = `Data​`, Source = `Source​`)
write.csv(C_DataTable, file = "Data\\AppData\\C_DataTable.csv", row.names = FALSE)

## 5.2 Add sector descriptions----
write.csv(I_sectorDescriptions, file = "Data\\AppData\\C_sectorDescriptions.csv", row.names = FALSE)

####
# Title: Jobs explorer dashboard data transform
# Author: Paul James
# Date: 1st September 2023
# Last updated: 2nd Feb 2024
# Use: Transforms/format the raw data into the dataframes used in the dashboard.
# The dataframes are then saved as .csv in ./Data/AppData to be read by the dashboard app.
# Sources: The files created by running ExtractLoadData.R
# Naming: I_ import, F_ formatted (also used in transformation steps), C_ clean (for use in compiling the data for the dashboard)
# Running time: ~10secs
###

# Load libraries ----
library(tidyverse) # mostly dplyr
library(janitor) # use clean_names()
library(sf) # use st_as_sf st_union sf_use_s2
library(lubridate) # use years
library(writexl) # use write_xlsx
library(janitor) # row to names

# 1 Geographical data ----
# clean data to make sure region names match other data
neatRegion <- I_mapRegion %>%
  mutate(
    geog = "Region",
    geogConcat = gsub(" \\(England\\)", "", nuts118nm),
    geogConcat = case_when(
      geogConcat == "East of England" ~ "East",
      TRUE ~ geogConcat
    )
  ) %>% # match to employment data
  rename(areaCode = nuts118cd, areaName = nuts118nm) # consistent column naming

# 2 Data cleaning ----
## 2.0 cleaning functions ----
# Cleaning function for nomis data
formatNomis <- function(x) {
  x %>%
    filter(!(GEOGRAPHY_TYPE == "countries" & GEOGRAPHY_NAME != "United Kingdom")) %>% # remove countries that aren't UK
    # Add dates
    mutate(timePeriod = as.Date(paste("01", substr(DATE_NAME, 1, 8), sep = ""), format = "%d %b %Y")) %>%
    mutate(latest = case_when(
      timePeriod == max(timePeriod) ~ 1,
      timePeriod == (max(timePeriod) - years(1)) ~ -1,
      TRUE ~ 0
    )) %>%
    mutate(geogConcat = case_when(
      GEOGRAPHY_NAME == "United Kingdom" ~ "UK",
      TRUE ~ GEOGRAPHY_NAME
    )) %>%
    rename(chartPeriod = DATE_NAME, value = OBS_VALUE) %>%
    select(-GEOGRAPHY_TYPE, -GEOGRAPHY_NAME, -GEOGRAPHY_CODE)
}

## 2.1 Nomis dataset ----
### 2.1.1 Employment by occupation ----
# split Soc lookup table into 2010 and 2020 for joins later
F_sectorLookup2020 <- I_sectorLookup %>%
  filter(SOCyear == 2020) %>%
  rename(SOC2020code = SOCcode, SOC2020desc = SOCdesc)
F_sectorLookup2010 <- I_sectorLookup %>%
  filter(SOCyear == 2010) %>%
  rename(SOC2010code = SOCcode, SOC2010desc = SOCdesc)

# tidy raw data into format used in dashboard (filter to only relevant data, align column names, format to uniform structure)
F_empOcc2010 <- formatNomis(I_empOcc2010) %>%
  filter(MEASURES_NAME == "Value", MEASURE_NAME == "Count") %>% # get only count (not rate) data
  mutate(
    breakdown = case_when(
      C_SEX_NAME == "All persons" & C_OCCPUK11H_0_NAME == "Total" ~ "Total",
      C_SEX_NAME != "All persons" & C_OCCPUK11H_0_NAME == "Total" ~ "Sex",
      TRUE ~ paste(C_OCCPUK11H_0_TYPE, "SOC2010")
    ),
    subgroup = case_when(
      C_SEX_NAME == "All persons" & C_OCCPUK11H_0_NAME == "Total" ~ "Total",
      C_SEX_NAME != "All persons" & C_OCCPUK11H_0_NAME == "Total" ~ C_SEX_NAME,
      TRUE ~ C_OCCPUK11H_0_NAME
    ),
    # get sex breakdown
    breakdown2 = case_when(
      C_SEX_NAME != "All persons" & C_OCCPUK11H_0_NAME != "Total" ~ "Sex",
      TRUE ~ "Total"
    ),
    subgroup2 = case_when(
      C_SEX_NAME != "All persons" & C_OCCPUK11H_0_NAME != "Total" ~ C_SEX_NAME,
      TRUE ~ "Total"
    ),
    metric = "Employed",
    valueText = as.character(value),
    subgroup = trimws(gsub("[[:digit:]]+", "", subgroup)), # remove soc codes from name
    SOC2010code = as.numeric(sub(" .*$", "", C_OCCPUK11H_0_NAME)), # get just soc code
    latest = case_when(
      chartPeriod == "Jan 2020-Dec 2020" ~ 1,
      chartPeriod == "Jan 2019-Dec 2019" ~ -1,
      TRUE ~ 0
    ) # doing this to get the data in the table in the dashboard to be 2020 to match the earnings data
  ) %>%
  select(geogConcat, chartPeriod, timePeriod, latest, metric, breakdown, subgroup, breakdown2, subgroup2, value, valueText, SOC2010code)

# add on sectors by repeating any occs matched to a sector then grouping them up into the sectors
C_empOcc2010 <- F_empOcc2010 %>%
  bind_rows(F_empOcc2010 %>%
              inner_join(F_sectorLookup2010, relationship = "many-to-many") %>%
              filter(subgroup != "Total") %>%
              group_by(geogConcat, chartPeriod, timePeriod, latest, metric, breakdown, breakdown2, subgroup2, Sector) %>%
              summarise(value = sum(value, na.rm = TRUE)) %>%
              rename(subgroup = Sector)) %>%
  mutate(valueText = as.character(value))

# soc2020 formatting
F_empOcc2020 <- formatNomis(I_empOcc2020) %>%
  filter(MEASURES_NAME == "Value", MEASURE_NAME == "Count") %>%
  mutate(
    breakdown = case_when(
      C_SEX_NAME == "All persons" & SOC2020_FULL_NAME == "Total" ~ "Total",
      C_SEX_NAME != "All persons" & SOC2020_FULL_NAME == "Total" ~ "Sex",
      TRUE ~ paste(SOC2020_FULL_TYPE, "SOC2020")
    ),
    subgroup = case_when(
      C_SEX_NAME == "All persons" & SOC2020_FULL_NAME == "Total" ~ "Total",
      C_SEX_NAME != "All persons" & SOC2020_FULL_NAME == "Total" ~ C_SEX_NAME,
      TRUE ~ SOC2020_FULL_NAME
    ),
    breakdown2 = case_when(
      C_SEX_NAME != "All persons" & SOC2020_FULL_NAME != "Total" ~ "Sex",
      TRUE ~ "Total"
    ),
    subgroup2 = case_when(
      C_SEX_NAME != "All persons" & SOC2020_FULL_NAME != "Total" ~ C_SEX_NAME,
      TRUE ~ "Total"
    ),
    metric = "Employed",
    subgroup = gsub("[[:digit:]]+", "", subgroup),
    subgroup = trimws(gsub(":", "", subgroup)),
    SOC2020code = as.numeric(sub(" .*$", "", SOC2020_FULL_NAME))
  ) %>%
  select(geogConcat, chartPeriod, timePeriod, metric, latest, breakdown, subgroup, breakdown2, subgroup2, value, SOC2020code)

# add on sectors by repeating any occs matched to a sector then grouping them up into the sectors
C_empOcc2020 <- F_empOcc2020 %>%
  bind_rows(F_empOcc2020 %>%
              inner_join(F_sectorLookup2020, relationship = "many-to-many") %>%
              filter(subgroup != "Total") %>%
              group_by(geogConcat, chartPeriod, timePeriod, latest, metric, breakdown, breakdown2, subgroup2, Sector) %>%
              summarise(value = sum(value, na.rm = TRUE)) %>%
              rename(subgroup = Sector)) %>%
  mutate(valueText = as.character(value))

## 2.2 Other datasets ----
### 2.2.1 Ashe earnings ----
# set headers and rename to match other data
cleanAshe <- function(dataset, year, breakdownName, subgroupName) {
  dataset %>%
    row_to_names(4) %>%
    clean_names() %>%
    mutate(chartPeriod = year, breakdown2 = breakdownName, subgroup2 = subgroupName)
}
# join the various sheets together
C_ashe <- bind_rows(
  cleanAshe(I_ashe17, 2017, "Total", "Total"),
  cleanAshe(I_ashe18, 2018, "Total", "Total"),
  cleanAshe(I_ashe19, 2019, "Total", "Total"),
  cleanAshe(I_ashe20, 2020, "Total", "Total"),
  cleanAshe(I_ashe21, 2021, "Total", "Total"),
  cleanAshe(I_ashe22, 2022, "Total", "Total"),
  cleanAshe(I_ashe23, 2023, "Total", "Total"),
  cleanAshe(I_asheMale17, 2017, "Sex", "Males"),
  cleanAshe(I_asheMale18, 2018, "Sex", "Males"),
  cleanAshe(I_asheMale19, 2019, "Sex", "Males"),
  cleanAshe(I_asheMale20, 2020, "Sex", "Males"),
  cleanAshe(I_asheMale21, 2021, "Sex", "Males"),
  cleanAshe(I_asheMale22, 2022, "Sex", "Males"),
  cleanAshe(I_asheMale23, 2023, "Sex", "Males"),
  cleanAshe(I_asheFemale17, 2017, "Sex", "Females"),
  cleanAshe(I_asheFemale18, 2018, "Sex", "Females"),
  cleanAshe(I_asheFemale19, 2019, "Sex", "Females"),
  cleanAshe(I_asheFemale20, 2020, "Sex", "Females"),
  cleanAshe(I_asheFemale21, 2021, "Sex", "Females"),
  cleanAshe(I_asheFemale22, 2022, "Sex", "Females"), 
  cleanAshe(I_asheFemale23, 2023, "Sex", "Females")
) %>%
  mutate(
    breakdown = case_when(
      chartPeriod >= 2021 ~ paste0(nchar(code), "-digit occupation SOC2020"),
      TRUE ~ paste0(nchar(code), "-digit occupation SOC2010")
    ),
    SOCcode = as.integer(code),
    SOCyear = case_when(
      chartPeriod >= 2021 ~ 2020,
      TRUE ~ 2010
    )
  ) %>%
  select(breakdown, subgroup = description, breakdown2, subgroup2, valueText = median, population = thousand, chartPeriod, SOCcode) %>%
  mutate(
    timePeriod = as.Date(paste("01 Jan", substr(chartPeriod, 1, 4), sep = ""), format = "%d %b %Y"),
    geogConcat = "UK",
    metric = "Earnings",
    latest = case_when(
      timePeriod == max(timePeriod) ~ 1,
      timePeriod == (max(timePeriod) - years(1)) ~ -1,
      chartPeriod == "2020" ~ 1,
      chartPeriod == "2019" ~ -1, # These are for soc2010 so the year in the dashboard table is 2020 for soc2010 occs
      TRUE ~ 0
    ),
    chartPeriod = as.character(chartPeriod),
    value = as.numeric(valueText),
    population = as.numeric(population),
    breakdown = case_when(
      (subgroup == "All employees" & chartPeriod >= 2021) ~ "4-digit occupation SOC2020",
      (subgroup == "All employees" & chartPeriod < 2021) ~ "4-digit occupation SOC2010",
      TRUE ~ breakdown
    ),
    subgroup = case_when(subgroup == "All employees" ~ "Total", TRUE ~ subgroup)
  ) %>%
  # get rid of footnotes
  filter(
    breakdown != "NA-digit occupation SOC2020",
    breakdown != "NA-digit occupation SOC2010"
  )

### 2.2.2 Skills imperative 2035 ----
# Clean up and reformat data and then caluclate some metrics; annual growth and growth to 2035

# Clena data
F_employmentProjections <-
  I_wfOccF2 %>%
  rename(subgroup = thousands) %>%
  mutate(
    metric = "employmentProjection",
    SOC20202digit = case_when(
      subgroup == "All occupations" ~ 99, # use this code as lookup for totals later
      TRUE ~ as.numeric(sub(" .*$", "", subgroup))
    ),
    subgroup = case_when(
      subgroup == "All occupations" ~ "Total",
      TRUE ~ trimws(gsub("[[:digit:]]+", "", subgroup)) # remove codes
    )
  ) %>%
  left_join(I_wfAreaName) %>% # add on area name
  mutate(geogConcat = paste0(trimws(`.Main`, "both"))) %>% # get name
  filter(is.na(subgroup) == FALSE, (is.na(SOC20202digit) == FALSE | subgroup == "Total")) %>% # remove missimg data
  select(-`.Main`, -Scenario, -file_name) %>%
  # reformat to long data
  pivot_longer(!c("geogConcat", "subgroup", "SOC20202digit", "metric"),
               names_to = "chartPeriod",
               values_to = "value"
  ) %>%
  mutate(
    value = value * 1000, # originla data in 1,000s
    chartPeriod = gsub("X", "", chartPeriod)
  ) %>%
  # allign names/correct different spellings
  mutate(geogConcat = case_when(
    geogConcat == "Yorkshire and the Humber" ~ "Yorkshire and The Humber",
    geogConcat == "United Kingdom" ~ "UK",
    TRUE ~ geogConcat
  )) %>%
  mutate(subgroup = case_when(
    subgroup == "Total" ~ "All occupations",
    TRUE ~ subgroup
  )) %>%
  # get time period
  mutate(timePeriod = as.Date(paste("01", "Jan", chartPeriod, sep = " "), format = "%d %b %Y")) %>%
  mutate(latest = case_when(
    timePeriod == max(timePeriod) ~ 1,
    timePeriod == (max(timePeriod) - years(1)) ~ -1,
    TRUE ~ 0
  ))

# Get future year on year growth metric
F_empGrowth <- F_employmentProjections %>%
  filter(chartPeriod >= 2022) %>% # only need growth for future
  # order
  arrange(chartPeriod, timePeriod, latest) %>%
  group_by(geogConcat, metric, SOC20202digit, subgroup) %>%
  # get growth
  mutate(value = (value - lag(value)) / lag(value)) %>%
  filter(chartPeriod != 2022) %>% # ignore first year since no year before to get growth
  mutate(metric = "employmentProjectionAnnualGrowth")

# Get 2022 to 2035 growth metric
F_empGrowth2023_2035 <- F_employmentProjections %>%
  filter(chartPeriod %in% c(2023, 2035)) %>%
  # order
  arrange(chartPeriod, timePeriod, latest) %>%
  group_by(geogConcat, metric, SOC20202digit, subgroup) %>%
  # get growth
  mutate(value = (value - lag(value)) / lag(value)) %>%
  filter(chartPeriod != 2023) %>% # ignore first year since no year before to get growth
  mutate(metric = "employmentProjectionGrowth2023to2035")

# combine all skills imperative  metrics
F_skillsImperative <- bind_rows(
  F_employmentProjections,
  F_empGrowth,
  F_empGrowth2023_2035
) %>%
  mutate(valueText = as.character(value))

# Now get all 4 digit soc codes (skills imp is only in 2 digit SOC) and list the growth for them all based on their 2 digit code. So for SOC code 1112, we would apply growth for 11 group
F_skillsImperative4digit <- C_empOcc2020 %>%
  filter(breakdown == "4-digit occupation SOC2020" | breakdown == "Total", latest == 1, breakdown2 == "Total") %>% # get one row per 4 digit code only
  mutate(SOC20202digit = case_when(
    breakdown == "Total" & subgroup == "Total" ~ 99 # code for total
    , breakdown == "4-digit occupation SOC2020" ~ as.numeric(substr(SOC2020code, 1, 2)),
    TRUE ~ NA
  )) %>%
  filter(is.na(SOC20202digit) == FALSE) %>% # remove sector groups
  select(geogConcat, SOC2020code, SOC20202digit, subgroup, employed = value) %>%
  left_join(F_skillsImperative %>%
              select(-subgroup) %>%
              filter(metric == "employmentProjectionGrowth2023to2035"), relationship = "many-to-many") %>%
  select(geogConcat, SOC2020code, subgroup, employmentProjectionGrowth2023to2035 = value, employed)

# add on sectors by joining to the lookup, then grouping and recalculating the stats
C_skillsImperative <- F_skillsImperative4digit %>%
  bind_rows(F_skillsImperative4digit %>%
              inner_join(F_sectorLookup2020, relationship = "many-to-many") %>%
              mutate(employed2035 = employed * (1 + employmentProjectionGrowth2023to2035)) %>% # calculate the projected employment based on the growth rate
              group_by(geogConcat, Sector) %>%
              summarise(employmentProjectionGrowth2023to2035 = (sum(employed2035, na.rm = TRUE) - (sum(employed, na.rm = TRUE))) / sum(employed, na.rm = TRUE)) %>% # then calculate the growth for the group
              rename(subgroup = Sector)) %>%
  mutate(
    breakdown = "4-digit occupation SOC2020",
    subgroup = case_when(
      subgroup == "Total" ~ "All occupations",
      TRUE ~ subgroup
    )
    , SOC2020code = case_when(
      subgroup == "All occupations" ~ 9999,
      TRUE ~ SOC2020code
    )
  ) %>%
  select(-employed)

### 2.2.3 AI exposure ----
# Clean up score for AI exposure for each occ data
F_AiExposure2010 <- I_AiExposure2010 %>%
  clean_names() %>%
  select(SOC2010code = soc_code, AiExposure = all_ai_2) %>%
  filter(is.na(SOC2010code) == FALSE, SOC2010code != "Table 2: Summary statistics") %>%
  mutate(SOC2010code = as.numeric(SOC2010code), geogConcat = "UK", breakdown = "4-digit occupation SOC2010") %>%
  # add on employment volumes for use in sector calculations
  left_join(C_empOcc2010 %>%
              filter(geogConcat == "UK", breakdown == "4-digit occupation SOC2010", breakdown2 == "Total", latest == 1) %>%
              select(SOC2010code, subgroup, employVol = value))

F_AiExposure2020 <- I_AiExposure2020 %>%
  select(SOC2020code = SOC.code, AiExposure = All.AI) %>%
  mutate(geogConcat = "UK", breakdown = "4-digit occupation SOC2020") %>%
  # add on employment volumes for use in sector calculations
  left_join(C_empOcc2020 %>%
              filter(geogConcat == "UK", breakdown == "4-digit occupation SOC2020", breakdown2 == "Total", latest == 1) %>%
              select(SOC2020code, subgroup, employVol = value))

# add on sectors, then weight the group ai exposure based on employmnet volumes
C_AiExposure <- F_AiExposure2010 %>%
  bind_rows(F_AiExposure2010 %>%
              inner_join(F_sectorLookup2010, relationship = "many-to-many") %>%
              group_by(geogConcat, Sector, breakdown) %>%
              summarise(AiExposure = sum(AiExposure * employVol, na.rm = TRUE) / sum(employVol, na.rm = TRUE)) %>%
              rename(subgroup = Sector)) %>%
  bind_rows(F_AiExposure2020)%>%
  bind_rows(F_AiExposure2020 %>%
              inner_join(F_sectorLookup2020, relationship = "many-to-many") %>%
              group_by(geogConcat, Sector, breakdown) %>%
              summarise(AiExposure = sum(AiExposure * employVol, na.rm = TRUE) / sum(employVol, na.rm = TRUE)) %>%
              rename(subgroup = Sector)) %>%
  # add a row for UK total which is standardised at 1
  bind_rows(data.frame(
    SOC2010code = 9999,
    AiExposure = 1,
    geogConcat = "UK",
    breakdown = "4-digit occupation SOC2010",
    subgroup = "All occupations"
  ))%>%
  bind_rows(data.frame(
    SOC2020code = 9999,
    AiExposure = 1,
    geogConcat = "UK",
    breakdown = "4-digit occupation SOC2020",
    subgroup = "All occupations"
  ))

### 2.2.4 Pathways ----
# tidy up pathways data, and group by 4 digit occ, and subject, and count FPE (full time poart time count)
F_pathways <- I_pathways %>%
  select(SOC2010code = OCC10, pathway, Level_group, SubjectHEFE, FPE) %>%
  group_by(SOC2010code, pathway, Level_group, SubjectHEFE) %>%
  summarise(count = sum(FPE, na.rm = TRUE)) %>% # sum people using their FPE split
  ungroup() %>%
  mutate(geogConcat = "UK", breakdown = "4-digit occupation SOC2010") %>%
  # add on soc names
  left_join(C_empOcc2010 %>%
              filter(geogConcat == "UK", breakdown == "4-digit occupation SOC2010", breakdown2 == "Total", latest == 1) %>%
              select(SOC2010code, subgroup))

# add on sectors and summarise the FPE for that group
F_pathwaysSectors <- F_pathways %>%
  bind_rows(F_pathways %>%
              inner_join(F_sectorLookup2010, relationship = "many-to-many") %>%
              group_by(geogConcat, Sector, breakdown, pathway, Level_group, SubjectHEFE) %>%
              summarise(count = sum(count, na.rm = TRUE)) %>%
              rename(subgroup = Sector))

# count FPE for each occupation or sector and filter out bad quality data and label poor quality data
F_pathwaysOcc <- F_pathwaysSectors %>%
  left_join(F_pathwaysSectors %>%
              group_by(subgroup) %>%
              summarise(occSectorCount = sum(count, na.rm = TRUE))) %>%
  filter(occSectorCount >= 50) %>% # remove those with less than 50 because data is not good enough
  # for those between 50 and 100 add an asterix to denote low data quality
  mutate(
    pathway = case_when(occSectorCount < 100 ~ paste0("*", pathway), TRUE ~ pathway),
    Level_group = case_when(occSectorCount < 100 ~ paste0("*", Level_group), TRUE ~ Level_group),
    # add on pathway to subject so we can group by this later
    SubjectHEFE = paste0(pathway, "-", SubjectHEFE)
  )

# Summarise the most common pathway for each occ and give the % of the occupation that took that pathway
F_commonPathway <- F_pathwaysOcc %>%
  select(breakdown, SOC2010code, subgroup, pathway, count) %>%
  group_by(breakdown, SOC2010code, subgroup, pathway) %>%
  summarise(countPath = sum(count)) %>%
  mutate(per = countPath / sum(countPath))

# Summarise the most common pathway-subject for each occ and give the % of the occupation that took that pathway
F_commonSubject <- F_pathwaysOcc %>%
  select(breakdown, SOC2010code, subgroup, SubjectHEFE, count) %>%
  group_by(breakdown, SOC2010code, subgroup, SubjectHEFE) %>%
  summarise(countSubject = sum(count)) %>%
  mutate(per = countSubject / sum(countSubject)) %>%
  # remove A-levels and GCSEs from the table since they don't have a single subject
  filter(!SubjectHEFE %in% c("A/AS levels-A/AS levels", "GCSEs-GCSEs", "*A/AS levels-A/AS levels", "*GCSEs-GCSEs"))

# Summarise the most common level for each occ and give the % of the occupation that took that pathway
F_commonLevel <- F_pathwaysOcc %>%
  select(breakdown, SOC2010code, subgroup, Level_group, count) %>%
  group_by(breakdown, SOC2010code, subgroup, Level_group) %>%
  summarise(countLevel = sum(count)) %>%
  mutate(per = countLevel / sum(countLevel))

### 2.2.5 In demand ----
# filter to only those in demand (signified by being in the upper quartile of ranking)
F_inDemand <- I_inDemand %>%
  filter(demand_level!="Total",SOC20!="Total" )%>%#ignore the total rows
  distinct(SOC2020code=as.numeric(SOC20),inDemand=demand_level) %>%
  # add on employment volumes to use in calculating group demand %s
  right_join(C_empOcc2020 %>%
               filter(geogConcat == "UK", breakdown == "4-digit occupation SOC2020", breakdown2 == "Total", latest == 1) %>%
               select(SOC2020code, geogConcat,subgroup, breakdown, employVol = value)) 

# add on sectors and calculate the % of employment in each group that is in and in demand occupation
C_inDemand <- F_inDemand %>%
  filter(is.na(SOC2020code) == FALSE) %>% # remove groups
  bind_rows(F_inDemand %>%
              inner_join(F_sectorLookup2020%>%select(SOC2020code,Sector), relationship = "many-to-many") %>%
              group_by(geogConcat,breakdown, Sector, inDemand) %>%
              summarise(employVol = sum(employVol, na.rm = TRUE)) %>%
              mutate(per = employVol / sum(employVol)) %>%
              filter(inDemand=="Critical demand")%>%
              # add text with % in demand for use in kpi
              mutate(inDemand = paste0(round(per * 100), "% of group in critical")) %>%
              select(geogConcat, breakdown, subgroup = Sector, inDemand)) %>%
  select(-employVol)

## 2.3 occupation list ----
# Get a full list of clenaed occupations for SOC10 and SOC20 with level and other SOC levels (these will be used to filter the occupation list in the dashboard)
C_occList <- formatNomis(I_empOcc2010) %>%
  distinct(C_OCCPUK11H_0_NAME, C_OCCPUK11H_0_TYPE) %>%
  mutate(
    Occupation = trimws(gsub("[[:digit:]]+", "", C_OCCPUK11H_0_NAME)),
    code = sub(" .*$", "", C_OCCPUK11H_0_NAME),
    Soc1digit = substr(code, 1, 1),
    Soc2digit = substr(code, 1, 2),
    Soc3digit = substr(code, 1, 3),
    SOC = "SOC2010"
  ) %>%
  select(-C_OCCPUK11H_0_NAME) %>%
  rename(type = C_OCCPUK11H_0_TYPE) %>%
  # add soc20
  bind_rows(
    formatNomis(I_empOcc2020) %>%
      distinct(SOC2020_FULL_NAME, SOC2020_FULL_TYPE) %>%
      mutate(
        Occupation = trimws(gsub("[[:digit:]]+", "", SOC2020_FULL_NAME)),
        Occupation = trimws(gsub(":", "", Occupation)),
        code = sub(" .*$", "", SOC2020_FULL_NAME),
        Soc1digit = substr(code, 1, 1),
        Soc2digit = substr(code, 1, 2),
        Soc3digit = substr(code, 1, 3),
        SOC = "SOC2020"
      ) %>%
      select(-SOC2020_FULL_NAME) %>%
      rename(type = SOC2020_FULL_TYPE)
  ) %>%
  # add on level
  left_join(I_levelLookup) %>%
  mutate(
    occYear = case_when(
      Occupation == "Total" ~ paste0("All occupations", " ", SOC),
      TRUE ~ paste0(Occupation, " ", SOC)
    ),
    type = case_when(
      Occupation == "Total" ~ "4-digit occupation",
      TRUE ~ type
    )
  )
write.csv(C_occList, file = "Data\\AppData\\C_occList.csv", row.names = FALSE)

# 3 Combine datasets ----
C_localSkillsDataset <- bind_rows(
  C_empOcc2010 %>% mutate(source = "SOC2010"),
  C_empOcc2020 %>% mutate(source = "SOC2020"),
  C_ashe
)

# 4 Create datasets used by the app----
## 4.1 C_Geog ----
# This is the map boundary data.
C_Geog <- neatRegion
save(C_Geog, file = "Data\\AppData\\C_Geog.rdata")

## 4.2 C_mapData ----
# This contains only the latest total data for each employment, emp % and growth for each region

# get employed volumes for each region
F_mapDataEmployed <- C_localSkillsDataset %>%
  filter(
    geogConcat != "UK", # regions only
    metric %in% c("Employed"),
    breakdown %in% c("4-digit occupation SOC2020", "4-digit occupation SOC2010", "Total"),
    breakdown2 == "Total" # sex breakdown not needed
  ) %>%
  mutate(breakdown = case_when(
    breakdown == "Total" & source == "SOC2020" ~ "4-digit occupation SOC2020",
    breakdown == "Total" & source == "SOC2010" ~ "4-digit occupation SOC2010",
    TRUE ~ breakdown
  )) %>%
  select(chartPeriod, timePeriod, breakdown, subgroup, value, metric, geogConcat)

# add on % of employees in each occupation for each region
F_mapDataEmployedPerc <- F_mapDataEmployed %>%
  # add on totals
  left_join(F_mapDataEmployed %>% filter(subgroup == "Total") %>% rename(totalValue = value) %>% select(-subgroup)) %>%
  # calculate %
  mutate(metric = "Percentage of all employment in region", value = value / totalValue) %>%
  select(-totalValue) %>%
  filter(subgroup != "Total")

# add on % growth of employees in each occupation for each region
F_mapDataEmployedGrowth <- F_mapDataEmployed %>%
  # get growth
  arrange(chartPeriod) %>%
  group_by(geogConcat, metric, breakdown, subgroup) %>%
  mutate(value = (value - lag(value)) / lag(value)) %>%
  # get rid of periods without data a year earlier since we can't get growth for them
  filter(!(chartPeriod == "Jan 2017-Dec 2017" & breakdown == "4-digit occupation SOC2010")) %>%
  filter(!(chartPeriod == "Jan 2021-Dec 2021" & breakdown == "4-digit occupation SOC2020")) %>%
  filter(!(chartPeriod == "Apr 2021-Mar 2022" & breakdown == "4-digit occupation SOC2020")) %>%
  filter(!(chartPeriod == "Jul 2021-Jun 2022" & breakdown == "4-digit occupation SOC2020")) %>%
  filter(!(chartPeriod == "Oct 2021-Sep 2022" & breakdown == "4-digit occupation SOC2020")) %>%
  mutate(metric = "Employment annual growth")

# Join them all together
C_mapData <- bind_rows(
  F_mapDataEmployed,
  F_mapDataEmployedPerc,
  F_mapDataEmployedGrowth
) %>%
  mutate(subgroup = case_when(
    subgroup != "Total" ~ paste0(subgroup, " ", str_sub(breakdown, -7, -1)),
    TRUE ~ subgroup
  ))
write.csv(C_mapData, file = "Data\\AppData\\C_mapData.csv", row.names = FALSE)

## 4.2 C_time ----
# This is used in the line charts. It contains historic data for each metric and area.
C_time <- C_localSkillsDataset %>%
  filter(metric %in% c("Employed", "Earnings")) %>%
  mutate(
    valueText = as.character(valueText),
    subgroup = case_when(
      subgroup != "Total" ~ paste0(subgroup, " ", str_sub(breakdown, -7, -1)),
      TRUE ~ paste0("All occupations ", source)
    )
  )
write.csv(C_time, file = "Data\\AppData\\C_time.csv", row.names = FALSE)

## 4.3 C_table ----
# This is used to create the dashboard summary table and also the KPIs
# We get the employment volumes then join on all the other metrics

### 4.3.1 Employment stats ----
# get only the latest year of soc10 and soc20 data for the table
F_tableEmployment <- C_localSkillsDataset %>%
  filter(
    latest == 1,
    metric == "Employed",
    breakdown %in% c("4-digit occupation SOC2020", "4-digit occupation SOC2010", "Total"),
    breakdown2 == "Total"
  ) %>%
  mutate(breakdown = case_when(
    breakdown == "Total" & chartPeriod == unique((C_empOcc2020%>%filter(latest==1))$chartPeriod) ~ "4-digit occupation SOC2020",
    breakdown == "Total" & chartPeriod == unique((C_empOcc2010%>%filter(latest==1))$chartPeriod) ~ "4-digit occupation SOC2010",
    TRUE ~ breakdown
  )) %>%
  select(geogConcat, subgroup, breakdown, Employed = value, SOC2020code, SOC2010code) %>%
  # add on employment growth
  left_join(C_localSkillsDataset %>%
              filter(
                latest == -1, # get year before
                metric == "Employed",
                breakdown %in% c("4-digit occupation SOC2020", "4-digit occupation SOC2010", "Total"),
                breakdown2 == "Total"
              ) %>%
              mutate(breakdown = case_when(
                breakdown == "Total" & chartPeriod == unique((C_empOcc2020%>%filter(latest==-1))$chartPeriod) ~ "4-digit occupation SOC2020",
                breakdown == "Total" & chartPeriod == unique((C_empOcc2010%>%filter(latest==-1))$chartPeriod) ~ "4-digit occupation SOC2010",
                TRUE ~ breakdown
              )) %>%
              select(geogConcat, subgroup, breakdown, EmployedLast = value)) %>%
  # calculate growth
  mutate(growth = (Employed - EmployedLast) / EmployedLast) %>%
  # add % female
  left_join(C_localSkillsDataset %>%
              filter(
                latest == 1,
                metric == "Employed",
                ((breakdown %in% c("4-digit occupation SOC2020", "4-digit occupation SOC2010") & subgroup2 == "Females") |
                   breakdown == "Sex" & subgroup == "Females")
              ) %>%
              mutate(
                breakdown = case_when(
                  chartPeriod == unique((C_empOcc2020%>%filter(latest==1))$chartPeriod) ~ "4-digit occupation SOC2020",
                  chartPeriod == unique((C_empOcc2010%>%filter(latest==1))$chartPeriod) ~ "4-digit occupation SOC2010",
                  TRUE ~ breakdown
                ),
                subgroup = case_when(
                  subgroup == "Females" ~ "Total",
                  TRUE ~ subgroup
                )
              ) %>%
              select(geogConcat, subgroup, breakdown, EmployedFemale = value)) %>%
  # caluclate the female percenatge
  mutate(
    femalePerc = EmployedFemale / Employed,
    SOC20202digit = case_when(
      breakdown == "4-digit occupation SOC2020" & subgroup == "Total" ~ 99 # code for total
      , breakdown == "4-digit occupation SOC2020" ~ as.numeric(substr(SOC2020code, 1, 2)),
      TRUE ~ NA
    ),
    SOC2010code = case_when(
      breakdown == "4-digit occupation SOC2010" & subgroup == "Total" ~ 9999 # code for total
      , TRUE ~ SOC2010code
    ),
    SOC2020code = case_when(
      breakdown == "4-digit occupation SOC2020" & subgroup == "Total" ~ 9999 # code for total
      , TRUE ~ SOC2020code
    )
  )

### 4.3.2 Region representation stats ----
# Get occupation split across regions
F_occSplitByRegion <-
  C_localSkillsDataset %>%
  filter(
    latest == 1,
    geogConcat != "UK",
    metric == "Employed",
    breakdown %in% c("4-digit occupation SOC2020", "4-digit occupation SOC2010", "Total"),
    subgroup2 == "Total"
    # ,subgroup!="Total"
  ) %>%
  mutate(breakdown = case_when(
    breakdown == "Total" & chartPeriod == unique((C_empOcc2020%>%filter(latest==1))$chartPeriod) ~ "4-digit occupation SOC2020",
    breakdown == "Total" & chartPeriod == unique((C_empOcc2010%>%filter(latest==1))$chartPeriod) ~ "4-digit occupation SOC2010",
    TRUE ~ breakdown
  )) %>%
  select(geogConcat, breakdown, subgroup, value) %>%
  group_by(subgroup, breakdown) %>% # group to occ and soc type
  mutate(F_occSplitByRegion = replace_na(value / sum(value, na.rm = TRUE), 0)) %>% # calculate the split
  ungroup() %>%
  select(-value)

# compare that occupation split to the national split
Underepresented <- F_occSplitByRegion %>%
  filter(subgroup != "Total") %>%
  # join on national split
  left_join(F_occSplitByRegion %>% filter(subgroup == "Total") %>% select(-subgroup) %>% rename(allOccsSplitByRegion = F_occSplitByRegion)) %>%
  # calculate difference
  mutate(representation = F_occSplitByRegion - allOccsSplitByRegion) %>%
  # order and slice off the smallest
  group_by(subgroup, breakdown) %>%
  arrange(representation) %>%
  slice(1) %>%
  select(subgroup, breakdown, Underepresented = geogConcat)

# Do the same but for the biggest difference to the national split
Overepresented <- F_occSplitByRegion %>%
  filter(subgroup != "Total") %>%
  left_join(F_occSplitByRegion %>% filter(subgroup == "Total") %>% select(-subgroup) %>% rename(allOccsSplitByRegion = F_occSplitByRegion)) %>%
  mutate(representation = F_occSplitByRegion - allOccsSplitByRegion) %>%
  group_by(subgroup, breakdown) %>%
  arrange(desc(representation)) %>%
  slice(1) %>%
  select(subgroup, breakdown, Overepresented = geogConcat)

### 4.3.3 Earnings stats
F_Earnings <- C_localSkillsDataset %>%
  filter(
    latest == 1,
    metric == "Earnings",
    breakdown %in% c("4-digit occupation SOC2020", "4-digit occupation SOC2010", "Total"),
    breakdown2 == "Total"
  ) %>%
  select(geogConcat, subgroup, breakdown, Earnings = value, population)

# earnings growth
EarningsGrowth <- C_localSkillsDataset %>%
  filter(
    latest == -1, # last year
    metric == "Earnings",
    breakdown %in% c("4-digit occupation SOC2020", "4-digit occupation SOC2010", "Total"),
    breakdown2 == "Total"
  ) %>%
  select(geogConcat, subgroup, breakdown, EarningsLast = value, populationLast = population)

### 4.3.3 add stats on to table ----
C_table <- F_tableEmployment %>%
  left_join(Underepresented) %>%
  left_join(Overepresented) %>%
  mutate(
    Underepresented = case_when(geogConcat == "UK" ~ Underepresented, TRUE ~ NA),
    Overepresented = case_when(geogConcat == "UK" ~ Overepresented, TRUE ~ NA)
  ) %>%
  left_join(F_Earnings) %>%
  left_join(EarningsGrowth) %>%
  # caluclte earnings growth
  mutate(
    growthEarn = (Earnings - EarningsLast) / EarningsLast,
    subgroup = case_when(
      subgroup == "Total" ~ "All occupations",
      TRUE ~ subgroup
    ),
    EarnPop = Earnings * population,
    EarnPopLast = EarningsLast * populationLast
  ) %>%
  select(-EarningsLast) %>%
  # add skills imp growth
  left_join(
    C_skillsImperative
  ) %>%
  # add on AI exposure
  left_join(
    C_AiExposure
  ) %>%
  select(-employVol) %>%
  ### 4.3.4 Pathways data ----
# add pathways data
left_join(
  F_commonPathway %>%
    slice_max(per, n = 1, with_ties = FALSE) %>%
    mutate(pathwayText = paste0(pathway, " (", round(per * 100, 0), "%)")) %>%
    select(SOC2010code, pathwayText)
) %>%
  # add level
  left_join(
    F_commonLevel %>%
      slice_max(per, n = 1, with_ties = FALSE) %>%
      mutate(levelText = paste0(Level_group, " (", round(per * 100, 0), "%)")) %>%
      select(SOC2010code, levelText)
  ) %>%
  # add subject
  left_join(
    F_commonSubject %>%
      slice_max(per, n = 1, with_ties = FALSE) %>%
      mutate(subjectText = paste0(SubjectHEFE, " (", round(per * 100, 0), "%)")) %>%
      select(SOC2010code, subjectText)
  ) %>%
  # add in demand
  left_join(
    C_inDemand
  ) %>%
  mutate(inDemand = case_when(
    is.na(inDemand) == TRUE & breakdown == "4-digit occupation SOC2010" & geogConcat == "UK" & subgroup != "All occupations" ~ "No",
    TRUE ~ inDemand
  )) %>%
  select(-SOC2020code, -SOC2010code, -SOC20202digit) %>%
  mutate(subgroup = paste0(subgroup, " ", str_sub(breakdown, -7, -1)))

write.csv(C_table, file = "Data\\AppData\\C_table.csv", row.names = FALSE)

## 4.4 C_empTot ----
# Get employment totals for each occ and region. This is used in the calculations on the fly in the app when getting group builder stats
C_empTot <- C_localSkillsDataset %>%
  filter(
    latest == 1,
    geogConcat != "UK",
    metric == "Employed",
    breakdown %in% c("4-digit occupation SOC2010", "4-digit occupation SOC2020", "Total"),
    subgroup2 == "Total"
  ) %>%
  mutate(subgroup = case_when(
    subgroup != "Total" ~ paste0(subgroup, " ", str_sub(breakdown, -7, -1)),
    TRUE ~ paste0("All occupations ", source)
  ))
write.csv(C_empTot, file = "Data\\AppData\\C_empTot.csv", row.names = FALSE)

# 5 Tidy up the app text ----
## 5.1 Tidy up data table text ----
names(I_DataTable) <- gsub(".", " ", names(I_DataTable), fixed = TRUE)
C_DataTable <- I_DataTable %>% rename(Data = `Data​`, Source = `Source​`)
write.csv(C_DataTable, file = "Data\\AppData\\C_DataTable.csv", row.names = FALSE)

## 5.2 Tidy up sector lookup ----
C_sectorLookup <- I_sectorLookup %>%
  mutate(SOC = paste0("SOC", SOCyear)) %>%
  select(-SOCyear, -SOCdesc) %>%
  left_join(C_occList %>% filter(type == "4-digit occupation") %>% select(SOCcode = code, SOC, occYear) %>% mutate(SOCcode = as.numeric(SOCcode))) %>%
  mutate(
    SectorYear = paste0(Sector, " ", SOC)
  )
write.csv(C_sectorLookup, file = "Data\\AppData\\C_sectorLookup.csv", row.names = FALSE)

# 5.3 Add sector descriptions----
write.csv(I_sectorDescriptions, file = "Data\\AppData\\C_sectorDescriptions.csv", row.names = FALSE)

