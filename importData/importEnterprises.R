### 2.1.3 UK Business Count----
# Enterprise by employment size and industry
# Query data
# Geography: England, regions, LADs (as of April 2022)
# Date: 12 months to Dec 2018-2022
# Cell: UK Business Counts - enterprises by industry and employment size band
# Enterprise by employment size and industry
I_entIndSize <-
  bind_rows(
    # user defined LSIPs
    nomisr::nomis_get_data("NM_142_1",
                           geography = geo_param,
                           industry = 37748736, date = "latestMINUS4-latest", employment_sizeband = "0,10,20,30,40", industry = "163577857...163577874", legal_status = "0", measures = "20100"
    ) %>%
      select(DATE_NAME, GEOGRAPHY_NAME, GEOGRAPHY_CODE, GEOGRAPHY_TYPE, INDUSTRY_NAME, EMPLOYMENT_SIZEBAND_NAME, OBS_VALUE),
    # user defined Greater London Authority
    nomisr::nomis_get_data("NM_142_1",
                           geography = geo_paramGLA,
                           industry = 37748736, date = "latestMINUS4-latest", employment_sizeband = "0,10,20,30,40", industry = "163577857...163577874", legal_status = "0", measures = "20100"
    ) %>%
      select(DATE_NAME, GEOGRAPHY_NAME, GEOGRAPHY_CODE, GEOGRAPHY_TYPE, INDUSTRY_NAME, EMPLOYMENT_SIZEBAND_NAME, OBS_VALUE)%>%
      mutate(GEOGRAPHY_TYPE = "combined authorities (as of May 2025)"),
    # user defined new LAs
    nomisr::nomis_get_data(
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
    nomisr::nomis_get_data("NM_142_1", geography = geogUseAps$id, industry = 37748736, date = "latestMINUS4-latest", employment_sizeband = "0,10,20,30,40", industry = "163577857...163577874", legal_status = "0", measures = "20100") %>%
      select(DATE_NAME, GEOGRAPHY_NAME, GEOGRAPHY_CODE, GEOGRAPHY_TYPE, INDUSTRY_NAME, EMPLOYMENT_SIZEBAND_NAME, OBS_VALUE),
  ) %>%
  filter(
    !GEOGRAPHY_NAME %in% c(
      "Allerdale", "Carlisle", "Copeland", "Barrow-in-Furness", "Eden", "South Lakeland",
      "Craven", "Hambleton", "Harrogate", "Richmondshire", "Ryedale", "Scarborough", "Selby",
      "Mendip", "Sedgemoor", "South Somerset", "Somerset West and Taunton"
    ) # remove old LADUs
  )

# Enterprise by industry ----
C_entInd <- I_entIndSize %>% 
  filter(INDUSTRY_NAME != "Total"
         ,EMPLOYMENT_SIZEBAND_NAME == "Total")%>%
  select(-EMPLOYMENT_SIZEBAND_NAME) %>%
  mutate(DATE_NAME = paste0("Mar ", DATE_NAME))%>%
  #format into dashboard form
  formatNomis() %>%
  rename(subgroup = INDUSTRY_NAME) %>%
  mutate(subgroup = gsub("[[:digit:]]+", "", subgroup)) %>%
  mutate(subgroup = gsub(" : ", "", subgroup)) %>%
  mutate(subgroup = gsub(" \\(.*", "", subgroup)) %>% # delete after first bracket
  mutate(metric = "enterpriseCount", breakdown = "Industry")%>%
  mutate(valueText = as.character(value))

# Enterprise by employment size
C_entSize <- I_entIndSize %>% 
  filter(INDUSTRY_NAME == "Total")%>%
  rename(subgroup = EMPLOYMENT_SIZEBAND_NAME) %>%
  select(-INDUSTRY_NAME) %>%
  mutate(DATE_NAME = paste0("Mar ", DATE_NAME), CELL_NAME = "enterpriseCount")%>%
  #format into dashboard form
  formatNomis() %>%
  rename(metric = CELL_NAME) %>%
  mutate(breakdown = case_when(
    subgroup == "Total" ~ "Total",
    TRUE ~ "Size"
  ))%>%
  mutate(valueText = as.character(value))