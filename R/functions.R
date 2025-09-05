# Function to get data from nomis and tidy given the table ID, the time periods wanted and the metric (cells) wanted
extractNomis <- function(tableID, dates, cells) {
  bind_rows(
    # user defined pt 1 (changed LSIPs)(do in multiple parts as falls over when all together)
    nomisr::nomis_get_data(
      id = tableID, date = dates, geography =
        "MAKE|Central%20London%20Forward|1778385166;1778385160;1778385171;1778385173;1778385178;1778385179;1778385181;1778385182;1778385187;1778385189;1778385191;1778385192,MAKE|Greater%20Devon|1778384973;1778384974;1778384975;1778384976;1778384922;1778384977;1778384978;1778384923;1778384979;1778384980,MAKE|Greater%20Lincolnshire|1778385052;1778385053;1778385054;1778385055;1778385056;1778385057;1778385058;1778384908;1778384909,MAKE|Hampshire%20and%20the%20Solent|1778385004;1778385005;1778385006;1778385007;1778385008;1778385009;1778385010;1778384940;1778385011;1778384938;1778385012;1778384939;1778385013;1778385014,MAKE|Leicester%2C%20Leicestershire%20and%20Rutland|1778385045;1778385046;1778385047;1778385048;1778384912;1778385049;1778385050;1778385051;1778384913,MAKE|Local%20London|1778385161;1778385163;1778385165;1778385169;1778385170;1778385175;1778385184;1778385185;1778385190,MAKE|North%20East|1778384941;1778385159;1778385143;1778385144;1778384950;1778385145;1778385146,MAKE|Somerset|1778384959,MAKE|South%20London%20Partnership|1778385167;1778385180;1778385183;1778385186;1778385188,MAKE|Surrey|1778385089;1778385090;1778385091;1778385092;1778385093;1778385094;1778385095;1778385096;1778385097;1778385098;1778385099,MAKE|Warwickshire|1778385100;1778385101;1778385102;1778385103;1778385104,MAKE|West%20London%20Alliance|1778385162;1778385164;1778385168;1778385172;1778385174;1778385176;1778385177,MAKE|West%20Midlands|1778385147;1778385148;1778385149;1778385150;1778385151;1778385152;1778385153",
      cell = cells
    ) %>%
      select(DATE_NAME, GEOGRAPHY_NAME, GEOGRAPHY_CODE, GEOGRAPHY_TYPE, CELL_NAME, OBS_VALUE, MEASURES_NAME),
    # user defined pt 2 (changed names LSIPs)
    nomisr::nomis_get_data(
      id = tableID, date = dates, geography =
        "MAKE|East%20Midlands|1811939407;1811939436;1811939437;1811939408;1811939438;1811939409;1811939403;1811939410;1811939411;1811939439;1811939412;1811939440;1811939441;1811939413;1811939405;1811939442;1811939414,MAKE|Gloucestershire|1811939656;1811939657;1811939658;1811939659;1811939660;1811939661,MAKE|Greater%20Essex|1811939484;1811939485;1811939486;1811939487;1811939488;1811939489;1811939490;1811939491;1811939492;1811939493;1811939477;1811939494;1811939478;1811939495,MAKE|Norfolk%20and%20Suffolk|1811939517;1811939510;1811939511;1811939524;1811939512;1811939519;1811939513;1811939520;1811939514;1811939515;1811939516;1811939525,MAKE|Sussex%20and%20Brighton|1811939621;1811939622;1811939560;1811939623;1811939624;1811939576;1811939577;1811939625;1811939578;1811939626;1811939579;1811939580;1811939627",
      cell = cells
    ) %>%
      select(DATE_NAME, GEOGRAPHY_NAME, GEOGRAPHY_CODE, GEOGRAPHY_TYPE, CELL_NAME, OBS_VALUE, MEASURES_NAME),
    # user defined pt 3 (LSIPs that haven't changed)
    nomisr::nomis_get_data(
      id = tableID, date = dates, geography =
        "MAKE|Buckinghamshire|1811939575,MAKE|Cambridgeshire%20and%20Peterborough|1811939479;1811939480;1811939481;1811939482;1811939476;1811939483,MAKE|Cheshire%20and%20Warrington|1811939345;1811939346;1811939348,MAKE|Cornwall%20and%20the%20Isles%20of%20Scilly|1811939631;1811939632,MAKE|Cumbria|1811939349;1811939350;1811939351;1811939352;1811939353;1811939354,MAKE|Dorset|1811939649;1811939655,MAKE|Greater%20Manchester|1811939355;1811939356;1811939357;1811939358;1811939359;1811939360;1811939361;1811939362;1811939363;1811939364,MAKE|Hertfordshire|1811939496;1811939497;1811939499;1811939500;1811939501;1811939503;1811939505;1811939506;1811939507;1811939509,MAKE|Hull%20and%20East%20Yorkshire|1811939382;1811939383,MAKE|Kent%20and%20Medway|1811939592;1811939593;1811939594;1811939595;1811939599;1811939596;1811939597;1811939562;1811939598;1811939601;1811939602;1811939603;1811939604,MAKE|Lancashire|1811939343;1811939344;1811939365;1811939366;1811939367;1811939368;1811939369;1811939370;1811939371;1811939372;1811939373;1811939374;1811939375;1811939376,MAKE|Liverpool%20City%20Region|1811939347;1811939377;1811939378;1811939379;1811939380;1811939381,MAKE|Oxfordshire|1811939605;1811939606;1811939607;1811939608;1811939609,MAKE|South-East%20Midlands|1811939473;1811939474;1811939475;1811939768;1811939769;1811939563,MAKE|South%20Yorkshire|1811939394;1811939395;1811939396;1811939397,MAKE|Stoke-on-Trent%20and%20Staffordshire|1811939447;1811939448;1811939449;1811939450;1811939451;1811939452;1811939453;1811939445;1811939454,MAKE|Swindon%20and%20Wiltshire|1811939637;1811939639,MAKE|Tees%20Valley|1811939329;1811939331;1811939332;1811939335;1811939336,MAKE|Thames%20Valley%20Berkshire|1811939559;1811939565;1811939566;1811939568;1811939569;1811939570,MAKE|The%20Marches|1811939443;1811939444;1811939446,MAKE|West%20of%20England%20and%20North%20Somerset|1811939628;1811939630;1811939633;1811939636,MAKE|West%20Yorkshire|1811939398;1811939399;1811939400;1811939401;1811939402,MAKE|Worcestershire|1811939467;1811939468;1811939469;1811939470;1811939471;1811939472,MAKE|York%20and%20North%20Yorkshire|1811939387;1811939388;1811939389;1811939390;1811939391;1811939392;1811939393;1811939386",
      cell = cells
    ) %>%
      select(DATE_NAME, GEOGRAPHY_NAME, GEOGRAPHY_CODE, GEOGRAPHY_TYPE, CELL_NAME, OBS_VALUE, MEASURES_NAME),
    # user defined pt 4 (Greater London Authority)
    nomisr::nomis_get_data(
      id = tableID, date = dates, geography =
        "MAKE|Greater%20London%20Authority|1811939540;1811939541;1811939542;1811939543;1811939544;1811939526;1811939527;1811939545;1811939546;1811939547;1811939548;1811939528;1811939529;1811939530;1811939549;1811939550;1811939551;1811939552;1811939531;1811939532;1811939553;1811939533;1811939534;1811939554;1811939535;1811939555;1811939556;1811939536;1811939557;1811939537;1811939558;1811939538;1811939539",
      cell = cells
    ) %>%
      select(DATE_NAME, GEOGRAPHY_NAME, GEOGRAPHY_CODE, GEOGRAPHY_TYPE, CELL_NAME, OBS_VALUE, MEASURES_NAME) %>%
      mutate(GEOGRAPHY_TYPE = "combined authorities (as of May 2025)"),
    # user defined pt 5 new LAs aggregated from old LAs
    nomisr::nomis_get_data(
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
    nomisr::nomis_get_data(
      id = tableID, date = dates, geography = geogUseAps$id,
      cell = cells
    ) %>%
      select(DATE_NAME, GEOGRAPHY_NAME, GEOGRAPHY_CODE, GEOGRAPHY_TYPE, CELL_NAME, OBS_VALUE, MEASURES_NAME)
  ) %>%
    filter(
      MEASURES_NAME == "Value",
      # remove old LADUs no longer in use
      !GEOGRAPHY_NAME %in% c(
        "Allerdale", "Carlisle", "Copeland", "Barrow-in-Furness", "Eden", "South Lakeland",
        "Craven", "Hambleton", "Harrogate", "Richmondshire", "Ryedale", "Scarborough", "Selby",
        "Mendip", "Sedgemoor", "South Somerset", "Somerset West and Taunton"
      )
    ) %>%
    select(-MEASURES_NAME)
}

## 2.0 cleaning functions ----

# convert to data format function
formatLong <- function(x) {
  x %>%
    # make long
    pivot_longer(!c("geogConcat", "timePeriod", "chartPeriod", "latest"),
      names_to = "subgroup",
      values_to = "valueText"
    ) %>%
    mutate(value = as.numeric(valueText)) %>% # for calculations
    mutate_at(vars(valueText), function(x) str_replace_all(x, c("!" = "c", "\\*" = "u", "~" = "low", "-" = "x"))) # common supression notation
}

# Cleaning function for nomis data
formatNomis <- function(x) {
  x %>%
    filter(!(GEOGRAPHY_TYPE == "countries" & GEOGRAPHY_NAME != "England")) %>%
    # Add dates
    mutate(timePeriod = as.Date(paste("01", substr(DATE_NAME, 1, 8), sep = ""), format = "%d %b %Y")) %>%
    mutate(latest = case_when(
      timePeriod == max(timePeriod) ~ 1,
      timePeriod == (max(timePeriod) - lubridate::years(1)) ~ -1,
      TRUE ~ 0
    )) %>%
    mutate(geogConcat = case_when(
      GEOGRAPHY_TYPE == "local authorities: district / unitary (as of April 2021)" ~ paste0(GEOGRAPHY_NAME, " LADU"),
      GEOGRAPHY_TYPE == "combined authorities (as of May 2025)" ~ paste0(GEOGRAPHY_NAME, " MCA"),
      GEOGRAPHY_TYPE == "User Defined Geography" ~ paste0(GEOGRAPHY_NAME, " LSIP"),
      TRUE ~ GEOGRAPHY_NAME
    )) %>%
    select(-GEOGRAPHY_TYPE, -GEOGRAPHY_NAME, -GEOGRAPHY_CODE) %>%
    rename(chartPeriod = DATE_NAME, value = OBS_VALUE)
}

# add on new LADUs/LEP/LSIP/MCA areas to all LAs. Used for those data withonly LAD data
addGeogs <- function(x) {
  withAreas <- x %>%
    filter(
      geographic_level %in% c("Local authority district", "National")
    ) %>%
    # Use new LA names from 2011 areas
    left_join(I_LaLookup %>% distinct(LAD11CD, LAD23CD_11 = LAD23CD), by = c("areaCode" = "LAD11CD")) %>% # make new LAs
    # Use new LA names from 2021 areas
    left_join(I_LaLookup %>% distinct(LAD21CD, LAD23CD_21 = LAD23CD), by = c("areaCode" = "LAD21CD")) %>% # make new LAs
    # create flag for when the lad code has changed
    mutate(
      newArea = case_when(
        (LAD23CD_11 != areaCode) | (LAD23CD_21 != areaCode) ~ 1, TRUE ~ 0
      ),
      areaCode = case_when(
        is.na(LAD23CD_11) == FALSE ~ LAD23CD_11,
        is.na(LAD23CD_21) == FALSE ~ LAD23CD_21,
        TRUE ~ areaCode
      )
    ) %>%
    # select new name
    select(-area, -LAD23CD_11, -LAD23CD_21) %>%
    left_join(distinct(neatLA, areaCode, area = areaName), by = c("areaCode" = "areaCode")) %>% # use to get consistent LA names
    # addLSIPS
    left_join(C_LADLSIP, by = c("areaCode" = "LAD23CD")) %>%
    # addMCA
    left_join(select(C_mcalookup, -CAUTH24CD, -LAD24NM), by = c("areaCode" = "LAD24CD")) %>%
    # add national name
    mutate(area = case_when(
      geographic_level == "National" ~ "England",
      TRUE ~ area
    ))

  # make long
  bind_rows(
    withAreas %>%
      mutate(geogConcat = case_when(
        geographic_level == "National" ~ area,
        TRUE ~ paste0(area, " LADU")
      )),
    withAreas %>%
      filter(is.na(LSIPname) == FALSE) %>%
      mutate(geogConcat = paste0(LSIPname, " LSIP"), newArea = 1),
    withAreas %>%
      filter(is.na(CAUTH24NM) == FALSE) %>%
      mutate(geogConcat = paste0(CAUTH24NM, " MCA"), newArea = 1)
  ) %>%
    select(-area, -LSIPname, -CAUTH24NM, -areaCode, -geographic_level)
}

format_pm <- function(x) {
  strNum <- format(abs(x), big.mark = ",", trim = TRUE)
  strNum <- paste0(ifelse(x < 0, "-", "+"), strNum)
}

# Conditional color for widget
# Returns 'green' on true, 'red' on false, e.g. api usage % change > 0
#                                               load time % change < 0
cond_color <- function(condition, true_color = "green") {
  if (is.na(condition)) {
    return("black")
  }
  # if change too small
  if (abs(condition) < 0.005) {
    return("black")
  }
  colours <- c("green", "#e00000")
  return(ifelse(condition > 0, true_color, colours[!colours == true_color]))
}

# Checks if numbers are numeric before converting to numeric (to avoid warnings)
safe_numeric <- function(x) {
  out <- rep(NA_real_, length(x))
  good <- grepl("^[0-9.]+$", x)
  out[good] <- as.numeric(x[good])
  out
}
