### 2.1.1 Employment by occupation ----
# Query data
# Geography: England, regions, LADs (as of April 2021)
# Cell: T09a Employment by occupation (SOC2010) sub-major group and full-time/part-time; All people/ All people
# find cells we want

# Get two digit soc codes (https://www.ons.gov.uk/methodology/classificationsandstandards/standardoccupationalclassificationsoc/soc2020/soc2020volume1structureanddescriptionsofunitgroups)
folder <- "1-9_SOC2020structure"
sheet <- "SOC2020 Framework"
I_SOC2020structure <- openxlsx::read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
C_SOC2020structure<-I_SOC2020structure%>%
  distinct(code=`SOC2020.Sub-Major.Group`,cleanName=stringr::str_to_lower(`SOC2020.Group.Title`))%>%
  filter(is.na(code)==FALSE)

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
  left_join(C_SOC2020structure) %>%
  mutate(subgroup = paste0(code, " - ", stringr::str_to_sentence(cleanName))) %>%
  select(-code, -cleanName)