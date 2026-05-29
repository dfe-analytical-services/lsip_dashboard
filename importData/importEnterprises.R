### 2.1.3 UK Business Count----
# Enterprise by employment size and industry
# Query data
# Geography: England, regions, LADs (as of April 2022)
# Date: 12 months to Dec 2018-2022
# Cell: UK Business Counts - enterprises by industry and employment size band
# Enterprise by employment size and industry

# This has so much data it goes over the nomis api call limit, so it must be chucked into two industries at a time
I_entIndSize <-  fetch_nomis(
  "NM_142_1",
  "latestMINUS4,latestMINUS3,latestMINUS2,latestMINUS1,latest",
  geog_all,
  cell="",
"&industry=37748736,163577857,163577858,163577859,163577860,163577861,163577862,163577863,163577864,163577865,163577866,163577867,163577868,163577869,163577870,163577871,163577872,163577873,163577874&employment_sizeband=0,10,20,30,40&legal_status=0",
  19*5*1,
  c("DATE_NAME", "GEOGRAPHY_NAME", "GEOGRAPHY_CODE","GEOGRAPHY_TYPE", "INDUSTRY_NAME", "EMPLOYMENT_SIZEBAND_NAME", "OBS_VALUE")
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