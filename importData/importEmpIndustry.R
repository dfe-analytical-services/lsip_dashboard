### 2.1.5 Employment by industry------------
# Geog and date as above
# Cell: T13a	Employment by industry (SIC 2007) and flexibility
# find cells we want
cellsUseAps_Ind <- cellsListAps %>% filter(description.en %like% "T13a:" & description.en %like% "All people")
# get data
C_empInd <- extractNomis("NM_17_1", "latestMINUS16,latestMINUS12,latestMINUS8,latestMINUS4,latest", cellsUseAps_Ind$id)%>%
#format into form used in dashboard
formatNomis() %>%
  rename(subgroup = CELL_NAME) %>%
  mutate(subgroup = gsub("[[:digit:]]+", "", subgroup)) %>%
  mutate(subgroup = gsub(" \\(SIC \\) : All people \\)", "", subgroup)) %>%
  mutate(subgroup = gsub("Ta: ", "", subgroup)) %>%
  mutate(subgroup = gsub("^\\S* ", "", subgroup)) %>% # delete before the first space
  mutate(subgroup = gsub("&", "and", subgroup)) %>%
  mutate(valueText = as.character(value)) %>%
  mutate(breakdown = "Industry", metric = "inemployment")