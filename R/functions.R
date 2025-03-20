# Function to get data from nomis and tidy given the table ID, the time periods wanted and the metric (cells) wanted
extractNomis <- function(tableID, dates, cells) {
  bind_rows(
    # user defined pt 1 (do in two parts as falls over when all together)
    nomisr::nomis_get_data(
      id = tableID, date = dates, geography =
        "MAKE|Brighton%20and%20Hove%2C%20East%20Sussex%2C%20West%20Sussex%20LSIP|1811939621;1811939622;1811939560;1811939623;1811939624;1811939576;1811939577;1811939625;1811939578;1811939626;1811939579;1811939580;1811939627,MAKE|Buckinghamshire%20LSIP|1811939575,MAKE|Cambridgeshire%20and%20Peterborough%20LSIP|1811939479;1811939480;1811939481;1811939482;1811939476;1811939483,MAKE|Cheshire%20and%20Warrington%20LSIP|1811939345;1811939346;1811939348,MAKE|Cornwall%20and%20the%20Isles%20of%20Scilly%20LSIP|1811939631;1811939632,MAKE|Cumbria%20LSIP|1811939349;1811939350;1811939351;1811939352;1811939353;1811939354,MAKE|Derbyshire%20and%20Nottinghamshire%20LSIP|1811939407;1811939436;1811939437;1811939408;1811939438;1811939409;1811939403;1811939410;1811939411;1811939439;1811939412;1811939440;1811939441;1811939413;1811939405;1811939442;1811939414,MAKE|Dorset%20LSIP|1811939649;1811939655,MAKE|Enterprise%20M3%20LSIP|1811939581;1811939582;1811939610;1811939611;1811939612;1811939586;1811939613;1811939614;1811939615;1811939589;1811939616;1811939617;1811939618;1811939590;1811939619;1811939591;1811939620,MAKE|Essex%2C%20Southend-on-Sea%20and%20Thurrock%20LSIP|1811939484;1811939485;1811939486;1811939487;1811939488;1811939489;1811939490;1811939491;1811939492;1811939493;1811939477;1811939494;1811939478;1811939495,MAKE|G%20First%20(Gloucestershire)%20LSIP|1811939656;1811939657;1811939658;1811939659;1811939660;1811939661,MAKE|Greater%20Lincolnshire%20LSIP|1811939422;1811939423;1811939424;1811939425;1811939406;1811939426;1811939427;1811939428;1811939384;1811939385,MAKE|Greater%20London%20LSIP|1811939540;1811939541;1811939542;1811939543;1811939544;1811939526;1811939527;1811939545;1811939546;1811939547;1811939548;1811939528;1811939529;1811939530;1811939549;1811939550;1811939551;1811939552;1811939531;1811939532;1811939553;1811939533;1811939534;1811939554;1811939535;1811939555;1811939556;1811939536;1811939557;1811939537;1811939558;1811939538;1811939539,MAKE|Greater%20Manchester%20LSIP|1811939355;1811939356;1811939357;1811939358;1811939359;1811939360;1811939361;1811939362;1811939363;1811939364,MAKE|Heart%20of%20the%20South-West%20LSIP|1811939640;1811939641;1811939662;1811939642;1811939643;1811939634;1811939663;1811939667;1811939644;1811939664;1811939645;1811939638;1811939646;1811939647,MAKE|Hertfordshire%20LSIP|1811939496;1811939497;1811939499;1811939500;1811939501;1811939503;1811939505;1811939506;1811939507;1811939509,MAKE|Hull%20and%20East%20Yorkshire%20LSIP|1811939382;1811939383,MAKE|Kent%20and%20Medway%20LSIP|1811939592;1811939593;1811939594;1811939595;1811939599;1811939596;1811939597;1811939562;1811939598;1811939601;1811939602;1811939603;1811939604,MAKE|Lancashire%20LSIP|1811939343;1811939344;1811939365;1811939366;1811939367;1811939368;1811939369;1811939370;1811939371;1811939372;1811939373;1811939374;1811939375;1811939376,MAKE|Leicester%20and%20Leicestershire%20LSIP|1811939415;1811939416;1811939417;1811939418;1811939404;1811939419;1811939420;1811939421,MAKE|Liverpool%20City%20Region%20LSIP|1811939347;1811939377;1811939378;1811939379;1811939380;1811939381,MAKE|New%20Anglia%20LSIP|1811939517;1811939510;1811939511;1811939524;1811939512;1811939519;1811939513;1811939520;1811939514;1811939515;1811939516;1811939525",
      cell = cells
    ) %>%
      select(DATE_NAME, GEOGRAPHY_NAME, GEOGRAPHY_CODE, GEOGRAPHY_TYPE, CELL_NAME, OBS_VALUE, MEASURES_NAME),
    # user defined pt 2
    nomisr::nomis_get_data(
      id = tableID, date = dates, geography =
        "MAKE|North%20East%20LSIP|1811939330;1811939338;1811939341;1811939342,MAKE|North%20of%20Tyne%20LSIP|1811939339;1811939340;1811939334,MAKE|Oxfordshire%20LSIP|1811939605;1811939606;1811939607;1811939608;1811939609,MAKE|Solent%20LSIP|1811939583;1811939584;1811939585;1811939587;1811939561;1811939588;1811939564;1811939567,MAKE|South-East%20Midlands%20LSIP|1811939473;1811939474;1811939475;1811939768;1811939769;1811939563,MAKE|South%20Yorkshire%20LSIP|1811939394;1811939395;1811939396;1811939397,MAKE|Stoke-on-Trent%20and%20Staffordshire%20LSIP|1811939447;1811939448;1811939449;1811939450;1811939451;1811939452;1811939453;1811939445;1811939454,MAKE|Swindon%20and%20Wiltshire%20LSIP|1811939637;1811939639,MAKE|Tees%20Valley%20LSIP|1811939329;1811939331;1811939332;1811939335;1811939336,MAKE|Thames%20Valley%20Berkshire%20LSIP|1811939559;1811939565;1811939566;1811939568;1811939569;1811939570,MAKE|The%20Marches%20LSIP|1811939443;1811939444;1811939446,MAKE|West%20Midlands%20and%20Warwickshire%20LSIP|1811939460;1811939461;1811939462;1811939455;1811939456;1811939457;1811939463;1811939464;1811939458;1811939465;1811939459;1811939466,MAKE|West%20of%20England%20and%20North%20Somerset%20LSIP|1811939628;1811939630;1811939633;1811939636,MAKE|West%20Yorkshire%20LSIP|1811939398;1811939399;1811939400;1811939401;1811939402,MAKE|Worcestershire%20LSIP|1811939467;1811939468;1811939469;1811939470;1811939471;1811939472,MAKE|York%20and%20North%20Yorkshire%20LSIP|1811939387;1811939388;1811939389;1811939390;1811939391;1811939392;1811939393;1811939386",
      cell = cells
    ) %>%
      select(DATE_NAME, GEOGRAPHY_NAME, GEOGRAPHY_CODE, GEOGRAPHY_TYPE, CELL_NAME, OBS_VALUE, MEASURES_NAME),
    # user defined pt 3 new LAs aggregated from old LAs
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
      GEOGRAPHY_TYPE == "combined authorities" ~ paste0(GEOGRAPHY_NAME, " MCA"),
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
    left_join(distinct(F_LEP2020, LAD23CD, area = LAD23NM), by = c("areaCode" = "LAD23CD")) %>% # use to get consistent LA names
    # add lep names
    left_join(select(C_LADLEP2020, -LAD23NM), by = c("areaCode" = "LAD23CD")) %>%
    # addLSIPS
    left_join(select(C_LADLSIP2020, -LAD23NM), by = c("areaCode" = "LAD23CD")) %>%
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
      )) %>%
      # group by and slice to remove those LAs that are in multiple LEPs
      group_by(across(c(-area, -LEP23NM1, -LSIP23NM, -CAUTH24NM, -areaCode, -geographic_level, -LEP23NM2))) %>%
      slice(1),
    withAreas %>%
      filter(is.na(LEP23NM1) == FALSE) %>%
      mutate(geogConcat = paste0(LEP23NM1, " LEP"), newArea = 1),
    withAreas %>%
      # group by and slice to remove those LAs that are in multiple LEPs
      group_by(across(c(-area, -LEP23NM1, -LSIP23NM, -CAUTH24NM, -geographic_level, -LEP23NM2))) %>%
      slice(1) %>%
      filter(is.na(LSIP23NM) == FALSE) %>%
      mutate(geogConcat = paste0(LSIP23NM, " LSIP"), newArea = 1),
    withAreas %>%
      # group by and slice to remove those LAs that are in multiple LEPs
      group_by(across(c(-area, -LEP23NM1, -LSIP23NM, -CAUTH24NM, -geographic_level, -LEP23NM2))) %>%
      slice(1) %>%
      filter(is.na(CAUTH24NM) == FALSE) %>%
      mutate(geogConcat = paste0(CAUTH24NM, " MCA"), newArea = 1)
  ) %>%
    select(-area, -LEP23NM1, -LSIP23NM, -CAUTH24NM, -areaCode, -geographic_level, -LEP23NM2)
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
  colours <- c("green", "#e00000")
  return(ifelse(condition, true_color, colours[!colours == true_color]))
}
