#Import all lookups

# 1 LA to LSIP lookup ----
folder <- "1-1_GeogLkup"
sheetNum <- "LAD_to_Local_skills_improvement"
F_LADLSIP <- openxlsx::read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)%>%
  mutate(LSIP23NM = trimws(LSIP23NM)) # Clean LSIP names

# Tidy LAD-LSIP lookup table
C_LADLSIP <- distinct(F_LADLSIP, LAD23CD, LAD23NM, LSIP23NM)

# 2 Missing LADs lookup----
# This happens because we have some old LADs in the ILR (and other) data that have since been made inactive. These do not feature in the most recent LSIP lookups. We have manually mapped these LADs to the latest LSIPs
folder <- "1-2_OldLas"
sheetNum <- 2
I_missingLAD <- openxlsx::read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

# 3 MCA lookup ----
folder <- "1-3_MCA_lookup"
C_mcalookup <- read.csv(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))%>% 
  select(-ObjectId)

# 4 LA 2011/2021 to 2023 lookup ----
folder <- "1-4_LaLookup"
sheetNum <- "Local_Authority_District_(2011)"
I_LaLookup <- openxlsx::read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)