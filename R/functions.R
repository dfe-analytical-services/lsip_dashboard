# Function to get data from nomis and tidy given the table ID, the time periods wanted and the metric (cells) wanted
fetch_nomis <- function(tableID, date, geog, cells, other_variables = NULL, other_variables_size = 1, select_cols = c("DATE_NAME", "GEOGRAPHY_NAME", "GEOGRAPHY_CODE", "GEOGRAPHY_TYPE", "CELL_NAME", "OBS_VALUE")) {
  # Most api calls go over the nomis limit, so run in chunks of geography areas
  n_cells <- length(cells)
  n_dates <- length(strsplit(date, ",")[[1]])

  # automatically compute safe chunk size
  geog_chunk_size <- floor(25000 / (n_cells * n_dates * other_variables_size * 2)) # times two for both value and confidence interval measures

  if (geog_chunk_size < 1) {
    stop("Too many cells/dates — even one geography exceeds Nomis limit")
  }

  message("Using geography chunk size: ", geog_chunk_size)

  geo_vec <- unlist(strsplit(geog, ","))

  geo_chunks <- split(
    geo_vec,
    ceiling(seq_along(geo_vec) / geog_chunk_size)
  ) |>
    lapply(paste, collapse = ",")

  # Get data from nomis
  map_dfr(geo_chunks, function(g_chunk) {
    base_url <- "https://www.nomisweb.co.uk/api/v01/dataset"

    url <- paste0(
      base_url, "/", tableID, ".data.csv?",
      "date=", date,
      "&geography=", paste(g_chunk, collapse = ","),
      "&cell=", paste(cells, collapse = ","),
      "&measures=20100,20701",
      other_variables
    )

    df <- readr::read_csv(url, show_col_types = FALSE)

    df <- df |>
      filter(MEASURES == 20100) |> # keep only values
      select(select_cols)

    if (nrow(df) == 25000) {
      message("ERROR! NOMIS API limit reached (25000 rows). Try chunking your request.")
    }
    return(df)
  })
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
      GEOGRAPHY_TYPE == "local authorities: district / unitary (as of April 2023)" ~ paste0(GEOGRAPHY_NAME, " LADU"),
      (GEOGRAPHY_TYPE == "combined authorities (as of May 2025)" | GEOGRAPHY_NAME == "Greater London Authority") ~ paste0(GEOGRAPHY_NAME, " CA"),
      GEOGRAPHY_TYPE == "User Defined Geography" ~ paste0(GEOGRAPHY_NAME, " LSIP"),
      TRUE ~ GEOGRAPHY_NAME
    )) %>%
    select(-GEOGRAPHY_TYPE, -GEOGRAPHY_NAME, -GEOGRAPHY_CODE) %>%
    rename(chartPeriod = DATE_NAME, value = OBS_VALUE)
}

# add on new LADUs/CA areas to all LAs. Used for those data with only LAD data
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
    # addCA
    left_join(select(C_calookup, -CAUTH25CD, -LAD25NM), by = c("areaCode" = "LAD25CD")) %>%
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
      filter(is.na(CAUTH25NM) == FALSE) %>%
      mutate(geogConcat = paste0(CAUTH25NM, " CA"), newArea = 1)
  ) %>%
    select(-area, -LSIPname, -CAUTH25NM, -areaCode, -geographic_level)
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

# R round function rounds to the nearest even number on .5 values. Most people expect a round up, so
# this function rounds up on 0.5
round2 <- function(x, digits) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- z + 0.5 + sqrt(.Machine$double.eps)
  z <- trunc(z)
  z <- z / 10^digits
  z * posneg
}

# Function to create job adverts per population data for Job_Ads_Page.R
population_data <- function(data, ...) {
  data %>%
    group_by(...) %>%
    # Calculate 12-month rolling sum
    mutate(n_jobs_yr_sum = slide_dbl(n_jobs, ~ sum(.x, na.rm = TRUE), .before = 11, .complete = TRUE)) %>%
    ungroup() %>%
    # Only keep rows with a full 12-month rolling period
    filter(!is.na(n_jobs_yr_sum)) %>%
    # Filter the 12-month rolling period into quarters to line up with the APS data
    filter(month(timePeriod) %in% c(3, 6, 9, 12)) %>%
    # Add in a chart period column to allow joining of the APS data
    mutate(
      start_date = timePeriod %m-% months(11),
      chartPeriod = str_c(format(start_date, "%b %Y"), "-", format(timePeriod, "%b %Y"))
    )
}
