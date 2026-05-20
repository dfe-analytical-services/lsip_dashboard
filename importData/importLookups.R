#Import all lookups

# 1 LA to LSIP lookup ----
# https://geoportal.statistics.gov.uk/datasets/ons::local-authority-district-to-local-skills-improvement-plan-areas-october-2025-lookup-in-en/about
C_LADLSIP <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LAD25_LSIP25_EN_LU/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json") |> 
  mutate(LSIP25NM=str_squish(LSIP25NM)) |> 
  select(LAD25CD,LSIP25NM)

# 3 CA lookup ----
# https://geoportal.statistics.gov.uk/datasets/ons::local-authority-district-to-combined-authority-may-2025-lookup-in-en/about
F_calookup <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LAD25_CAUTH25_EN_LU/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")

# Add on Greater London Authority built from the London LSIPs
C_calookup<-F_calookup |> 
  select(-ObjectId) |> 
  mutate(CAUTH25NM=str_squish(CAUTH25NM)) |> 
  bind_rows(C_LADLSIP |> 
    filter(LSIP25NM %in% c("Central London Forward","Local London","South London Partnership","West London Alliance"))%>%
      mutate(CAUTH25NM="Greater London Authority") |> 
      select(-LSIP25NM)
  )

# 4 LA 2011/2021 to 2023 lookup and to 2025----
folder <- "1-4_LaLookup"
sheetNum <- "Local_Authority_District_(2011)"
LAD21_to_LAD23 <- openxlsx::read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

# https://geoportal.statistics.gov.uk/datasets/ons::local-authority-district-2024-to-local-authority-district-2025-lookup-in-the-uk-v2/about
LAD24_to_LAD25 <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LAD24_LAD25_UK_LU_v2/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")

I_LaLookup <-LAD21_to_LAD23 |>  
  left_join(LAD24_to_LAD25, by=c("LAD23CD" = "LAD24CD"))
