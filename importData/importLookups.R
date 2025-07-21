#Import all lookups

# 1 LEPs 2020 and LA%20LSIP lookup ----
folder <- "1-1_GeogLkup"
sheetNum <- "LAD23-LSIP23-LEP23-LEP22"
F_LEP2020 <- openxlsx::read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)%>%
  mutate(LSIP23NM = trimws(LSIP23NM)) # Clean LSIP names

# Create LAD-LEP lookup table
C_LADLEP2020 <- distinct(F_LEP2020, LAD23CD, LAD23NM, LEP23NM1) %>% # get latest LAD and LEP lookup
  bind_rows(distinct(F_LEP2020 %>% filter(LEP23NM2 != 0), LAD23CD, LAD23NM, LEP23NM2)) # %>%#add on areas which are in the LEP overlaps

# Create LAD-LSIP lookup table
C_LADLSIP2020 <- distinct(F_LEP2020, LAD23CD, LAD23NM, LSIP23NM)

# 2 Missing LAD-LEP lookup----
# This happens because we have some old LADs in the ILR (and other) data that have since been made inactive. These do not feature in the most recent LAD-LEP matching. We have manually mapped these LADs to the latest LEPS (2021)
folder <- "1-2_LEPmissing"
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
