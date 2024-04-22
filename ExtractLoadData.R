####
# Title: LSIP dashboard data load
# Author: Paul James
# Date: 30th Aug 2022
# Last updated: 17th Apr 2023
# Aim: Loads the original data sources used within the dashboard.
# Use: To update any datasets, delete the relevant file within the relevant folder, and paste in the new data file. Rerun this file and then run TransformData.R
# Sources: See the dashboard page Data sources for links to the datasets
# Notes: Ensure there is *only* the file(s) that will be used in each of the numbered folders in ./Data/
# Running time: ~20mins
# NB 2.2.2 has a clause to ignore the latest partial year of data (since we only work with full years). You may need to amend this.
# NB 2.1.1 and 2.1.4 are currently importing the last 4 populated datsets and so are hardcoded (following data issues). check if there is new data and then you can change to LATEST dat format
###

# Load libraries ----
library(openxlsx) # use read.xlsx, read.csv
library(sf) # use st_read
library(tidyverse) # use map_df, mutate
library(nomisr) # use nomis api
library(data.table) # use %like%

# This is done here before any data changes so we can compare the data as it was to the updated data
C_timeOld <- read_csv("Data/AppData/C_time.csv")

# 1.Geography and mapping tables ----
## 1.1 LEPs 2020 and LA%20LSIP lookup ----
folder <- "1-1_GeogLkup"
sheetNum <- "LAD23-LSIP23-LEP23-LEP22"
I_LEP2020 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

## 1.2 Missing LAD-LEP lookup----
# This happens because we have some old LADs in the ILR (and other) data that have since been made inactive. These do not feature in the most recent LAD-LEP matching. We have manually mapped these LADs to the latest LEPS (2021)
folder <- "1-2_LEPmissing"
sheetNum <- 2
I_missingLAD <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

## 1.3 MCA lookup ----
folder <- "1-3_MCA_lookup"
sheetNum <- "Local_Authority_District_to_Com"
I_mcalookup <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

## 1.4 LA 2011/2021 to 2023 lookup ----
folder <- "1-4_LaLookup"
sheetNum <- "Local_Authority_District_(2011)"
I_LaLookup <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

## 1.5 LEP boundary----
folder <- "1-5_LEPBoundary"
I_mapLEP <- sf::st_read(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))),
  stringsAsFactors = F
)

## 1.6 LA boundary----
folder <- "1-6_LABoundary"
I_mapLA <- sf::st_read(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))),
  stringsAsFactors = F
)

## 1.7 MCA boundary----
folder <- "1-7_MCABoundary"
I_mapMCA <- sf::st_read(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))),
  stringsAsFactors = F
)

# 2. Datasets ----
## 2.1 Nomis datasets ----
# list of all the geogs we need (excluding the user defined which we add later)
geogUseAps <- nomis_get_metadata(id = "NM_17_1", concept = "geography", type = "type") %>%
  filter(description.en %in% c("local enterprise partnerships (as of April 2021)", "local authorities: district / unitary (as of April 2021)", "countries"))

# Get data and filter function for APS
extractNomis <- function(tableID, dates, cells) {
  bind_rows(
    # user defined pt 1 (do in two parts as falls over all together)
    nomis_get_data(
      id = tableID, date = dates, geography =
        "MAKE|Brighton%20and%20Hove%2C%20East%20Sussex%2C%20West%20Sussex%20LSIP|1811939621;1811939622;1811939560;1811939623;1811939624;1811939576;1811939577;1811939625;1811939578;1811939626;1811939579;1811939580;1811939627,MAKE|Buckinghamshire%20LSIP|1811939575,MAKE|Cambridgeshire%20and%20Peterborough%20LSIP|1811939479;1811939480;1811939481;1811939482;1811939476;1811939483,MAKE|Cambridgeshire%20and%20Peterborough%20MCA|1811939479;1811939480;1811939481;1811939482;1811939476;1811939483,MAKE|Cheshire%20and%20Warrington%20LSIP|1811939345;1811939346;1811939348,MAKE|Cornwall%20and%20the%20Isles%20of%20Scilly%20LSIP|1811939631;1811939632,MAKE|Cumbria%20LSIP|1811939349;1811939350;1811939351;1811939352;1811939353;1811939354,MAKE|Derbyshire%20and%20Nottinghamshire%20LSIP|1811939407;1811939436;1811939437;1811939408;1811939438;1811939409;1811939403;1811939410;1811939411;1811939439;1811939412;1811939440;1811939441;1811939413;1811939405;1811939442;1811939414,MAKE|Dorset%20LSIP|1811939649;1811939655,MAKE|Enterprise%20M3%20LSIP|1811939581;1811939582;1811939610;1811939611;1811939612;1811939586;1811939613;1811939614;1811939615;1811939589;1811939616;1811939617;1811939618;1811939590;1811939619;1811939591;1811939620,MAKE|Essex%2C%20Southend-on-Sea%20and%20Thurrock%20LSIP|1811939484;1811939485;1811939486;1811939487;1811939488;1811939489;1811939490;1811939491;1811939492;1811939493;1811939477;1811939494;1811939478;1811939495,MAKE|G%20First%20(Gloucestershire)%20LSIP|1811939656;1811939657;1811939658;1811939659;1811939660;1811939661,MAKE|Greater%20Lincolnshire%20LSIP|1811939422;1811939423;1811939424;1811939425;1811939406;1811939426;1811939427;1811939428;1811939384;1811939385,MAKE|Greater%20London%20LSIP|1811939540;1811939541;1811939542;1811939543;1811939544;1811939526;1811939527;1811939545;1811939546;1811939547;1811939548;1811939528;1811939529;1811939530;1811939549;1811939550;1811939551;1811939552;1811939531;1811939532;1811939553;1811939533;1811939534;1811939554;1811939535;1811939555;1811939556;1811939536;1811939557;1811939537;1811939558;1811939538;1811939539,MAKE|Greater%20Manchester%20LSIP|1811939355;1811939356;1811939357;1811939358;1811939359;1811939360;1811939361;1811939362;1811939363;1811939364,MAKE|Greater%20Manchester%20MCA|1811939355;1811939356;1811939357;1811939358;1811939359;1811939360;1811939361;1811939362;1811939363;1811939364,MAKE|Heart%20of%20the%20South-West%20LSIP|1811939640;1811939641;1811939662;1811939642;1811939643;1811939634;1811939663;1811939667;1811939644;1811939664;1811939645;1811939638;1811939646;1811939647,MAKE|Hertfordshire%20LSIP|1811939496;1811939497;1811939499;1811939500;1811939501;1811939503;1811939505;1811939506;1811939507;1811939509,MAKE|Hull%20and%20East%20Yorkshire%20LSIP|1811939382;1811939383,MAKE|Kent%20and%20Medway%20LSIP|1811939592;1811939593;1811939594;1811939595;1811939599;1811939596;1811939597;1811939562;1811939598;1811939601;1811939602;1811939603;1811939604,MAKE|Lancashire%20LSIP|1811939343;1811939344;1811939365;1811939366;1811939367;1811939368;1811939369;1811939370;1811939371;1811939372;1811939373;1811939374;1811939375;1811939376,MAKE|Leicester%20and%20Leicestershire%20LSIP|1811939415;1811939416;1811939417;1811939418;1811939404;1811939419;1811939420;1811939421,MAKE|Liverpool%20City%20Region%20LSIP|1811939347;1811939377;1811939378;1811939379;1811939380;1811939381,MAKE|Liverpool%20City%20Region%20MCA|1811939347;1811939377;1811939378;1811939379;1811939380;1811939381,MAKE|New%20Anglia%20LSIP|1811939517;1811939510;1811939511;1811939524;1811939512;1811939519;1811939513;1811939520;1811939514;1811939515;1811939516;1811939525",
      cell = cells
    ) %>%
      select(DATE_NAME, GEOGRAPHY_NAME, GEOGRAPHY_CODE, GEOGRAPHY_TYPE, CELL_NAME, OBS_VALUE, MEASURES_NAME),
    # user defined pt 2
    nomis_get_data(
      id = tableID, date = dates, geography =
        "MAKE|North%20East%20LSIP|1811939330;1811939338;1811939341;1811939342,MAKE|North%20East%20MCA|1811939330;1811939338;1811939341;1811939342,MAKE|North%20of%20Tyne%20LSIP|1811939339;1811939340;1811939334,MAKE|North%20of%20Tyne%20MCA|1811939339;1811939340;1811939334,MAKE|Oxfordshire%20LSIP|1811939605;1811939606;1811939607;1811939608;1811939609,MAKE|Solent%20LSIP|1811939583;1811939584;1811939585;1811939587;1811939561;1811939588;1811939564;1811939567,MAKE|South-East%20Midlands%20LSIP|1811939473;1811939474;1811939475;1811939768;1811939769;1811939563,MAKE|South%20Yorkshire%20LSIP|1811939394;1811939395;1811939396;1811939397,MAKE|South%20Yorkshire%20MCA|1811939394;1811939395;1811939396;1811939397,MAKE|Stoke-on-Trent%20and%20Staffordshire%20LSIP|1811939447;1811939448;1811939449;1811939450;1811939451;1811939452;1811939453;1811939445;1811939454,MAKE|Swindon%20and%20Wiltshire%20LSIP|1811939637;1811939639,MAKE|Tees%20Valley%20LSIP|1811939329;1811939331;1811939332;1811939335;1811939336,MAKE|Tees%20Valley%20MCA|1811939329;1811939331;1811939332;1811939335;1811939336,MAKE|Thames%20Valley%20Berkshire%20LSIP|1811939559;1811939565;1811939566;1811939568;1811939569;1811939570,MAKE|The%20Marches%20LSIP|1811939443;1811939444;1811939446,MAKE|West%20Midlands%20and%20Warwickshire%20LSIP|1811939460;1811939461;1811939462;1811939455;1811939456;1811939457;1811939463;1811939464;1811939458;1811939465;1811939459;1811939466,MAKE|West%20Midlands%20MCA|1811939460;1811939461;1811939462;1811939463;1811939464;1811939465;1811939466,MAKE|West%20of%20England%20and%20North%20Somerset%20LSIP|1811939628;1811939630;1811939633;1811939636,MAKE|West%20of%20England%20MCA|1811939628;1811939630;1811939636,MAKE|West%20Yorkshire%20LSIP|1811939398;1811939399;1811939400;1811939401;1811939402,MAKE|West%20Yorkshire%20MCA|1811939398;1811939399;1811939400;1811939401;1811939402,MAKE|Worcestershire%20LSIP|1811939467;1811939468;1811939469;1811939470;1811939471;1811939472,MAKE|York%20and%20North%20Yorkshire%20LSIP|1811939387;1811939388;1811939389;1811939390;1811939391;1811939392;1811939393;1811939386",
      cell = cells
    ) %>%
      select(DATE_NAME, GEOGRAPHY_NAME, GEOGRAPHY_CODE, GEOGRAPHY_TYPE, CELL_NAME, OBS_VALUE, MEASURES_NAME),
    # user defined pt 3 new LAs
    nomis_get_data(
      id = tableID, date = dates, geography =
        "MAKE|Westmorland%20and%20Furness|1811939350,1811939353,1811939354,MAKE|Cumberland|1811939349,1811939351,1811939352,MAKE|North%20Yorkshire|1811939387...1811939393,MAKE|Somerset|1811939662,1811939663,1811939667,1811939664",
      cell = cells
    ) %>%
      select(DATE_NAME, GEOGRAPHY_NAME, GEOGRAPHY_CODE, GEOGRAPHY_TYPE, CELL_NAME, OBS_VALUE, MEASURES_NAME) %>%
      mutate(
        GEOGRAPHY_TYPE = "local authorities: district / unitary (as of April 2021)",
        GEOGRAPHY_CODE = case_when(
          GEOGRAPHY_NAME == "Cumberland" ~ "E06000063",
          GEOGRAPHY_NAME == "Westmorland and Furness" ~ "E06000064",
          GEOGRAPHY_NAME == "Somerset" ~ "E06000066",
          GEOGRAPHY_NAME == "North Yorkshire" ~ "E06000065",
          TRUE ~ "NA"
        )
      ),
    # other geogs
    nomis_get_data(
      id = tableID, date = dates, geography = geogUseAps$id,
      cell = cells
    ) %>%
      select(DATE_NAME, GEOGRAPHY_NAME, GEOGRAPHY_CODE, GEOGRAPHY_TYPE, CELL_NAME, OBS_VALUE, MEASURES_NAME)
  ) %>%
    filter(
      MEASURES_NAME == "Value",
      !GEOGRAPHY_NAME %in% c(
        "Allerdale", "Carlisle", "Copeland", "Barrow-in-Furness", "Eden", "South Lakeland",
        "Craven", "Hambleton", "Harrogate", "Richmondshire", "Ryedale", "Scarborough", "Selby",
        "Mendip", "Sedgemoor", "South Somerset", "Somerset West and Taunton"
      ) # remove old LADUs
    ) %>%
    select(-MEASURES_NAME)
}

# list all the APS cells available
cellsListAps <- nomis_get_metadata(id = "NM_17_1", concept = "CELL")

### 2.1.1 Employment by occupation ----
# Query data
# Geography: England, LEPs, regions, LADs (as of April 2021)
# Cell: T09a Employment by occupation (SOC2010) sub-major group and full-time/part-time; All people/ All people
# find cells we want
cellsUseAps_empOcc <- cellsListAps %>% filter(description.en %like% "T09b:" & description.en %like% "All people - ")
# get data
I_empOcc <-
  extractNomis("NM_17_1", "latestMINUS16,latestMINUS12,latestMINUS8,latestMINUS4,latest", cellsUseAps_empOcc$id) %>%
  filter(str_sub(CELL_NAME, -12, -1) == "All people )") # ignore part time

### 2.1.2 Employment level and rate ------------
# Geog and date as above
# Cell: T01 Economic activity by age Aged 16-64/ All people
# find cells we want
cellsUseAps_emp <- cellsListAps %>% filter(description.en %like% "T01:" & description.en %like% "Aged 16-64" & description.en %like% "All People")
# get data
I_emp <- extractNomis("NM_17_1", "latestMINUS16,latestMINUS12,latestMINUS8,latestMINUS4,latest", cellsUseAps_emp$id)

# Cell: T01 Economic activity by age Aged 16+/ All people. We need this as the denominator of the bar charts where the splits are only available in 16+
# find cells we want
cellsUseAps_emp <- cellsListAps %>% filter(description.en %like% "T01:" & description.en %like% "All aged 16 & over" & description.en %like% "All People")
# get data
I_emp16plus <- extractNomis("NM_17_1", "latestMINUS16,latestMINUS12,latestMINUS8,latestMINUS4,latest", cellsUseAps_emp$id)

### 2.1.3 UK Business Count----
# Enterprise by employment size and industry
# Query data
# Geography: England, LEPs, regions, LADs (as of April 2022)
# Date: 12 months to Dec 2018-2022
# Cell: UK Business Counts - enterprises by industry and employment size band
# Enterprise by employment size and industry
I_entIndSize <-
  bind_rows(
    # user defined pt 1 (do in two parts as falls over all together)
    nomis_get_data("NM_142_1",
      geography =
        "MAKE|Brighton%20and%20Hove%2C%20East%20Sussex%2C%20West%20Sussex%20LSIP|1811939621;1811939622;1811939560;1811939623;1811939624;1811939576;1811939577;1811939625;1811939578;1811939626;1811939579;1811939580;1811939627,MAKE|Buckinghamshire%20LSIP|1811939575,MAKE|Cambridgeshire%20and%20Peterborough%20LSIP|1811939479;1811939480;1811939481;1811939482;1811939476;1811939483,MAKE|Cambridgeshire%20and%20Peterborough%20MCA|1811939479;1811939480;1811939481;1811939482;1811939476;1811939483,MAKE|Cheshire%20and%20Warrington%20LSIP|1811939345;1811939346;1811939348,MAKE|Cornwall%20and%20the%20Isles%20of%20Scilly%20LSIP|1811939631;1811939632,MAKE|Cumbria%20LSIP|1811939349;1811939350;1811939351;1811939352;1811939353;1811939354,MAKE|Derbyshire%20and%20Nottinghamshire%20LSIP|1811939407;1811939436;1811939437;1811939408;1811939438;1811939409;1811939403;1811939410;1811939411;1811939439;1811939412;1811939440;1811939441;1811939413;1811939405;1811939442;1811939414,MAKE|Dorset%20LSIP|1811939649;1811939655,MAKE|Enterprise%20M3%20LSIP|1811939581;1811939582;1811939610;1811939611;1811939612;1811939586;1811939613;1811939614;1811939615;1811939589;1811939616;1811939617;1811939618;1811939590;1811939619;1811939591;1811939620,MAKE|Essex%2C%20Southend-on-Sea%20and%20Thurrock%20LSIP|1811939484;1811939485;1811939486;1811939487;1811939488;1811939489;1811939490;1811939491;1811939492;1811939493;1811939477;1811939494;1811939478;1811939495,MAKE|G%20First%20(Gloucestershire)%20LSIP|1811939656;1811939657;1811939658;1811939659;1811939660;1811939661,MAKE|Greater%20Lincolnshire%20LSIP|1811939422;1811939423;1811939424;1811939425;1811939406;1811939426;1811939427;1811939428;1811939384;1811939385,MAKE|Greater%20London%20LSIP|1811939540;1811939541;1811939542;1811939543;1811939544;1811939526;1811939527;1811939545;1811939546;1811939547;1811939548;1811939528;1811939529;1811939530;1811939549;1811939550;1811939551;1811939552;1811939531;1811939532;1811939553;1811939533;1811939534;1811939554;1811939535;1811939555;1811939556;1811939536;1811939557;1811939537;1811939558;1811939538;1811939539,MAKE|Greater%20Manchester%20LSIP|1811939355;1811939356;1811939357;1811939358;1811939359;1811939360;1811939361;1811939362;1811939363;1811939364,MAKE|Greater%20Manchester%20MCA|1811939355;1811939356;1811939357;1811939358;1811939359;1811939360;1811939361;1811939362;1811939363;1811939364,MAKE|Heart%20of%20the%20South-West%20LSIP|1811939640;1811939641;1811939662;1811939642;1811939643;1811939634;1811939663;1811939667;1811939644;1811939664;1811939645;1811939638;1811939646;1811939647,MAKE|Hertfordshire%20LSIP|1811939496;1811939497;1811939499;1811939500;1811939501;1811939503;1811939505;1811939506;1811939507;1811939509,MAKE|Hull%20and%20East%20Yorkshire%20LSIP|1811939382;1811939383,MAKE|Kent%20and%20Medway%20LSIP|1811939592;1811939593;1811939594;1811939595;1811939599;1811939596;1811939597;1811939562;1811939598;1811939601;1811939602;1811939603;1811939604,MAKE|Lancashire%20LSIP|1811939343;1811939344;1811939365;1811939366;1811939367;1811939368;1811939369;1811939370;1811939371;1811939372;1811939373;1811939374;1811939375;1811939376,MAKE|Leicester%20and%20Leicestershire%20LSIP|1811939415;1811939416;1811939417;1811939418;1811939404;1811939419;1811939420;1811939421,MAKE|Liverpool%20City%20Region%20LSIP|1811939347;1811939377;1811939378;1811939379;1811939380;1811939381,MAKE|Liverpool%20City%20Region%20MCA|1811939347;1811939377;1811939378;1811939379;1811939380;1811939381,MAKE|New%20Anglia%20LSIP|1811939517;1811939510;1811939511;1811939524;1811939512;1811939519;1811939513;1811939520;1811939514;1811939515;1811939516;1811939525",
      industry = 37748736, date = "latestMINUS4-latest", employment_sizeband = "0,10,20,30,40", industry = "163577857...163577874", legal_status = "0", measures = "20100"
    ) %>%
      select(DATE_NAME, GEOGRAPHY_NAME, GEOGRAPHY_CODE, GEOGRAPHY_TYPE, INDUSTRY_NAME, EMPLOYMENT_SIZEBAND_NAME, OBS_VALUE),
    # user defined pt 2
    nomis_get_data("NM_142_1",
      geography =
        "MAKE|North%20East%20LSIP|1811939330;1811939338;1811939341;1811939342,MAKE|North%20East%20MCA|1811939330;1811939338;1811939341;1811939342,MAKE|North%20of%20Tyne%20LSIP|1811939339;1811939340;1811939334,MAKE|North%20of%20Tyne%20MCA|1811939339;1811939340;1811939334,MAKE|Oxfordshire%20LSIP|1811939605;1811939606;1811939607;1811939608;1811939609,MAKE|Solent%20LSIP|1811939583;1811939584;1811939585;1811939587;1811939561;1811939588;1811939564;1811939567,MAKE|South-East%20Midlands%20LSIP|1811939473;1811939474;1811939475;1811939768;1811939769;1811939563,MAKE|South%20Yorkshire%20LSIP|1811939394;1811939395;1811939396;1811939397,MAKE|South%20Yorkshire%20MCA|1811939394;1811939395;1811939396;1811939397,MAKE|Stoke-on-Trent%20and%20Staffordshire%20LSIP|1811939447;1811939448;1811939449;1811939450;1811939451;1811939452;1811939453;1811939445;1811939454,MAKE|Swindon%20and%20Wiltshire%20LSIP|1811939637;1811939639,MAKE|Tees%20Valley%20LSIP|1811939329;1811939331;1811939332;1811939335;1811939336,MAKE|Tees%20Valley%20MCA|1811939329;1811939331;1811939332;1811939335;1811939336,MAKE|Thames%20Valley%20Berkshire%20LSIP|1811939559;1811939565;1811939566;1811939568;1811939569;1811939570,MAKE|The%20Marches%20LSIP|1811939443;1811939444;1811939446,MAKE|West%20Midlands%20and%20Warwickshire%20LSIP|1811939460;1811939461;1811939462;1811939455;1811939456;1811939457;1811939463;1811939464;1811939458;1811939465;1811939459;1811939466,MAKE|West%20Midlands%20MCA|1811939460;1811939461;1811939462;1811939463;1811939464;1811939465;1811939466,MAKE|West%20of%20England%20and%20North%20Somerset%20LSIP|1811939628;1811939630;1811939633;1811939636,MAKE|West%20of%20England%20MCA|1811939628;1811939630;1811939636,MAKE|West%20Yorkshire%20LSIP|1811939398;1811939399;1811939400;1811939401;1811939402,MAKE|West%20Yorkshire%20MCA|1811939398;1811939399;1811939400;1811939401;1811939402,MAKE|Worcestershire%20LSIP|1811939467;1811939468;1811939469;1811939470;1811939471;1811939472,MAKE|York%20and%20North%20Yorkshire%20LSIP|1811939387;1811939388;1811939389;1811939390;1811939391;1811939392;1811939393;1811939386",
      industry = 37748736, date = "latestMINUS4-latest", employment_sizeband = "0,10,20,30,40", industry = "163577857...163577874", legal_status = "0", measures = "20100"
    ) %>%
      select(DATE_NAME, GEOGRAPHY_NAME, GEOGRAPHY_CODE, GEOGRAPHY_TYPE, INDUSTRY_NAME, EMPLOYMENT_SIZEBAND_NAME, OBS_VALUE),
    # user defined pt 3 new LAs
    nomis_get_data(
      "NM_142_1",
      geography =
        "MAKE|Westmorland%20and%20Furness|1811939350,1811939353,1811939354,MAKE|Cumberland|1811939349,1811939351,1811939352,MAKE|North%20Yorkshire|1811939387...1811939393,MAKE|Somerset|1811939662,1811939663,1811939667,1811939664",
      industry = 37748736, date = "latestMINUS4-latest", employment_sizeband = "0,10,20,30,40", industry = "163577857...163577874", legal_status = "0", measures = "20100"
    ) %>%
      select(DATE_NAME, GEOGRAPHY_NAME, GEOGRAPHY_CODE, GEOGRAPHY_TYPE, INDUSTRY_NAME, EMPLOYMENT_SIZEBAND_NAME, OBS_VALUE) %>%
      mutate(
        GEOGRAPHY_TYPE = "local authorities: district / unitary (as of April 2021)",
        GEOGRAPHY_CODE = case_when(
          GEOGRAPHY_NAME == "Cumberland" ~ "E06000063",
          GEOGRAPHY_NAME == "Westmorland and Furness" ~ "E06000064",
          GEOGRAPHY_NAME == "Somerset" ~ "E06000066",
          GEOGRAPHY_NAME == "North Yorkshire" ~ "E06000065",
          TRUE ~ "NA"
        )
      ),
    # other geogs
    nomis_get_data("NM_142_1", geography = geogUseAps$id, industry = 37748736, date = "latestMINUS4-latest", employment_sizeband = "0,10,20,30,40", industry = "163577857...163577874", legal_status = "0", measures = "20100") %>%
      select(DATE_NAME, GEOGRAPHY_NAME, GEOGRAPHY_CODE, GEOGRAPHY_TYPE, INDUSTRY_NAME, EMPLOYMENT_SIZEBAND_NAME, OBS_VALUE),
  ) %>%
  filter(
    !GEOGRAPHY_NAME %in% c(
      "Allerdale", "Carlisle", "Copeland", "Barrow-in-Furness", "Eden", "South Lakeland",
      "Craven", "Hambleton", "Harrogate", "Richmondshire", "Ryedale", "Scarborough", "Selby",
      "Mendip", "Sedgemoor", "South Somerset", "Somerset West and Taunton"
    ) # remove old LADUs
  )
# Ignore total
I_entInd <- I_entIndSize %>% filter(INDUSTRY_NAME != "Total")
# Just by size
I_entSize <- I_entIndSize %>% filter(INDUSTRY_NAME == "Total")

### 2.1.4 Skill by age gender ------------
# Geog and date as above
# Cell: T19	Qualification by age and gender. All people aged 16-64. only updated every Jan-Dec. NVQ data is available until 2021 - then on rcf
# find cells we want in NVQ
cellsUseAps_qualNvq <- cellsListAps %>% filter(description.en %like% "T19:" & description.en %like% "Total")
# get data
I_qualAgeGenderNvq <- extractNomis("NM_17_1", "2019-12,2020-12,2021-12", cellsUseAps_qualNvq$id)
# find cells we want in NVQ
cellsUseAps_qualRqf <- cellsListAps %>% filter(description.en %like% "T19a:" & description.en %like% "Total")
# get data
I_qualAgeGenderRqf <- extractNomis("NM_17_1", "latestMINUS4,latest", cellsUseAps_qualRqf$id)

### 2.1.5 Employment by industry------------
# Geog and date as above
# Cell: T13a	Employment by industry (SIC 2007) and flexibility
# find cells we want
cellsUseAps_Ind <- cellsListAps %>% filter(description.en %like% "T13a:" & description.en %like% "All people")
# get data
I_empInd <- extractNomis("NM_17_1", "latestMINUS16,latestMINUS12,latestMINUS8,latestMINUS4,latest", cellsUseAps_Ind$id)

## 2.2 EES datasets----
### 2.2.1 Achievements by SSAt1, LAD, gender, level------------
folder <- "2-7_ILRachSSA"
I_FeSsa <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))

### 2.2.2 Achievements/starts/part by LAD and provision, level and age------------
## Download "Further education and skills geography - detailed summary " from https://explore-education-statistics.service.gov.uk/data-catalogue/further-education-and-skills/2021-22
folder <- "2-8_ILRach"
I_FeProvLevelAge <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))

### 2.2.3 KS4 destinations----
# National pupil database
folder <- "2-9_KS4destin"
I_KS4 <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))

### 2.2.4 KS5 destinations----
folder <- "2-10_KS5destin"
I_KS5 <- read.csv(file = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))

## 2.3 ONS datasets ----
### 2.3.1 Business demography, UK ----
# Number of enterprise births, deaths and active
# Geography: England and LADS (as of April 2021)
folder <- "2-11_bussdemo"
firstRow <- 4

# births
sheet <- "Table 1.1a"
I_births_ONS1618 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)
sheet <- "Table 1.1b"
I_births_ONS19 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)
sheet <- "Table 1.1c"
I_births_ONS20 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)
sheet <- "Table 1.1d"
I_births_ONS21 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)

# deaths
sheet <- "Table 2.1a"
I_deaths_ONS1618 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)
sheet <- "Table 2.1b"
I_deaths_ONS19 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)
sheet <- "Table 2.1c"
I_deaths_ONS20 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)
sheet <- "Table 2.1d"
I_deaths_ONS21 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)

# active
sheet <- "Table 3.1a"
I_active_ONS1618 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)
sheet <- "Table 3.1b"
I_active_ONS19 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)
sheet <- "Table 3.1c"
I_active_ONS20 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)
sheet <- "Table 3.1d"
I_active_ONS21 <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T, startRow = firstRow)

### 2.3.2 ONS job adverts by SOC ----
folder <- "2-12_OnsProf"
sheet <- "Table 12"
I_Ons2digLA <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 10"
I_Ons2digLep <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 9"
I_Ons2digLsip <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 11"
I_Ons2digMca <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 8"
I_Ons2digRegion <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)

sheet <- "Table 1"
I_OnsEng <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 6"
I_OnsLA <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 4"
I_OnsLep <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 3"
I_OnsLsip <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
sheet <- "Table 5"
I_OnsMca <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)

## 2.4 Skills imperative----
# These take a long time to run ~20mins
folder <- "2-13_skillsImperative2035"
dir_path <- paste0("./Data/", folder, "/")
skillsImpFileList <- list.files(dir_path)

read_dir <- function(dir_path, file_name, sheet_name, row_nos) {
  read.xlsx(paste0(dir_path, file_name), sheet = sheet_name, skipEmptyRows = T, rows = row_nos) %>%
    mutate(file_name = file_name)
}

# Industry future
I_wfIndF2 <-
  skillsImpFileList %>%
  map_df(~ read_dir(dir_path, .x, "Ind F2", 4:38))
# Occupation future
I_wfOccF2 <-
  skillsImpFileList %>%
  map_df(~ read_dir(dir_path, .x, "Occ F2", 4:49))
# Qualification future
I_wfQualF1 <-
  skillsImpFileList %>%
  map_df(~ read_dir(dir_path, .x, "Qual F1", 4:14))
# Area names
I_wfAreaName <-
  skillsImpFileList %>%
  map_df(~ read_dir(dir_path, .x, "Info", 2:5)) %>%
  filter(grepl("name", Scenario, fixed = TRUE))

# 3 Dashboard text----
## 3.1 Data sources ----
folder <- "3-1_DataTable"
sheetNum <- 1
I_DataTable <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

## 3.2 Data notes and caveats ----
folder <- "3-2_dataText"
sheetNum <- 1
I_DataText <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

## 3.3 FE Interventions table (not currently used)----
# folder <- "3-3_FeInterventions"
# sheetNum <- 1
# I_InterventionTable <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

## 3.4 Load FE sources and tools tables ----
folder <- "3-4_FeSources"
sheetNum <- "Tools"
I_ToolsTable <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)
sheetNum <- "Sources"
I_SourcesTable <- read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)
