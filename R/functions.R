# Function to get data from nomis and tidy given the table ID, the time periods wanted and the metric (cells) wanted
extractNomis <- function(tableID, dates, cells) {
  bind_rows(
    # user defined LSIPs
    nomisr::nomis_get_data(
      id = tableID, date = dates, geography = geo_param,
      cell = cells
    ) %>%
      select(DATE_NAME, GEOGRAPHY_NAME, GEOGRAPHY_CODE, GEOGRAPHY_TYPE, CELL_NAME, OBS_VALUE, MEASURES_NAME),
    # user defined Greater London Authority
    nomisr::nomis_get_data(
      id = tableID, date = dates, geography = geo_paramGLA,
      cell = cells
    ) %>%
      select(DATE_NAME, GEOGRAPHY_NAME, GEOGRAPHY_CODE, GEOGRAPHY_TYPE, CELL_NAME, OBS_VALUE, MEASURES_NAME) %>%
      mutate(GEOGRAPHY_TYPE = "combined authorities (as of May 2025)"),
    # user defined new LAs aggregated from old LAs
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
