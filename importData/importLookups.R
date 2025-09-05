#Import all lookups

# 1 LA to LSIP lookup ----
folder <- "1-1_LSIP_lookup"
sheetNum <- "LA Districts to LSIPs"
C_LADLSIP <- openxlsx::read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)%>%
  mutate(LSIPname = trimws(`NEW.LSIP.GEOGRAPHY.(4.london.areas)`))%>%# Clean LSIP names
  select(LAD23CD,LSIPname)

# 2 Missing LADs lookup----
# This happens because we have some old LADs in the ILR (and other) data that have since been made inactive. These do not feature in the most recent LSIP lookups. We have manually mapped these LADs to the latest LSIPs
folder <- "1-2_OldLas"
sheetNum <- 2
I_missingLAD <- openxlsx::read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

# 3 MCA lookup ----
folder <- "1-3_MCA_lookup"
F_mcalookup <- read.csv(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))))%>% 
  select(-ObjectId)
# Add on Greater London Authority built from the LOndon LSIPs
C_mcalookup<-F_mcalookup%>%
  bind_rows(C_LADLSIP%>%
    filter(LSIPname %in% c("Central London Forward","Local London","South London Partnership","West London Alliance"))%>%
      mutate(CAUTH24NM="Greater London Authority")%>%
      rename(LAD24CD=LAD23CD)%>%
      select(-LSIPname)
  )

# 4 LA 2011/2021 to 2023 lookup ----
folder <- "1-4_LaLookup"
sheetNum <- "Local_Authority_District_(2011)"
I_LaLookup <- openxlsx::read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)