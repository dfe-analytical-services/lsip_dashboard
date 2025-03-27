### 2.1.1 Employment by occupation ----
# Query data
# Geography: England, regions, LADs (as of April 2021)
# Cell: T09a Employment by occupation (SOC2010) sub-major group and full-time/part-time; All people/ All people
# find cells we want

# Get two digit soc codes from the ons job ads data to add on later
folder <- "2-12_OnsProf"
sheet <- "Table 8"
I_Ons2digRegion <- openxlsx::read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)

cellsUseAps_empOcc <- cellsListAps %>% filter(description.en %like% "T09b:" & description.en %like% "All people - ")
# get data
C_empOcc <-
  extractNomis("NM_17_1", "latestMINUS16,latestMINUS12,latestMINUS8,latestMINUS4,latest", cellsUseAps_empOcc$id) %>%
  filter(stringr::str_sub(CELL_NAME, -12, -1) == "All people )") %>%# ignore part time
# convert into format used in dashboard
formatNomis() %>%
  rename(subgroup = CELL_NAME) %>%
  mutate(subgroup = gsub("[[:digit:]]+", "", subgroup)) %>%
  mutate(subgroup = gsub(" \\(SOC\\) : All people \\)", "", subgroup)) %>%
  mutate(subgroup = gsub("Tb: \\(All people - ", "", subgroup)) %>%
  mutate(valueText = as.character(value)) %>%
  mutate(
    breakdown = "Occupation (SOC2020 Sub-Major Group)",
    metric = "inemployment",
    cleanName = stringr::str_to_lower(subgroup)
  ) %>%
  # add on soc codes
  left_join(I_Ons2digRegion %>% distinct(code = X2, cleanName = stringr::str_to_lower(X3))) %>%
  mutate(subgroup = paste0(code, " - ", stringr::str_to_sentence(cleanName))) %>%
  select(-code, -cleanName)