#Import all geographical boundaries

# 1 England boundary----
# https://geoportal.statistics.gov.uk/datasets/ons::countries-december-2025-boundaries-uk-buc/about
Nationalgeojson <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Countries_December_2025_Boundaries_UK_BUC/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")

neatEngland<- Nationalgeojson |> 
  filter(CTRY25NM=="England") |> 
  mutate(geog = "England") |> # add geog type
  rename(areaCode = CTRY25CD, areaName = CTRY25NM) |>  # consistent naming
  mutate(areaName=str_squish(areaName)) |> 
  sf::st_transform(4326) |> # transform to WG84 that leaflet can plot
  select(areaCode,areaName,LAT,LONG,geog,geometry)

# 2 LA boundary----
# https://geoportal.statistics.gov.uk/datasets/ons::local-authority-districts-december-2025-boundaries-uk-buc/about
LAgeojson <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local_Authority_Districts_DEC_2025_Boundaries_UK_BUC/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")

neatLA<- LAgeojson |> 
  mutate(geog = "LADU") |> # add geog type
  # add on lsip, and ca groupings
  left_join(C_LADLSIP |> mutate(LSIP = paste0(LSIP25NM, " LSIP")) |>  select(LAD25CD, LSIP), by = c("LAD25CD" = "LAD25CD")) |> 
  left_join(C_calookup |>  mutate(CA = paste0(CAUTH25NM, " CA")) |>  select(LAD25CD, CA), by = c("LAD25CD" = "LAD25CD")) |> 
  filter(is.na(LSIP) == FALSE) |>  # remove non England
  mutate(CA = case_when(LSIP == "Greater London LSIP" ~ "Greater London Authority CA", TRUE ~ CA)) |>  # add on gla as ca
  rename(areaCode = LAD25CD, areaName = LAD25NM) |>  # consistent naming
  mutate(areaName=str_squish(areaName)) |> 
  sf::st_transform(4326) |> # transform to WG84 that leaflet can plot
  select(areaCode,areaName,LAT,LONG,geog,LSIP, CA, geometry)

# 3 CA boundary----
# https://geoportal.statistics.gov.uk/datasets/ons::combined-authorities-december-2025-boundaries-en-buc/about
CAgeojson <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Combined_Authorities_December_2025_Boundaries_EN_BUC/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")

neatCA<- CAgeojson |> 
  mutate(geog = "CA") |> # add geog type
  rename(areaCode = CAUTH25CD, areaName = CAUTH25NM) |>  # consistent naming
  mutate(areaName=str_squish(areaName)) |> 
  sf::st_transform(4326) |> # transform to WG84 that leaflet can plot
  select(areaCode,areaName,LAT,LONG,geog,geometry)

# 3 LSIP boundary----
# https://geoportal.statistics.gov.uk/datasets/ons::local-skills-improvement-plan-areas-october-2025-boundaries-en-buc/about
LSIPgeojson <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LSIP_OCT_2025_EN_BUC/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")

neatLSIP<- LSIPgeojson |> 
  mutate(geog = "LSIP") |> # add geog type
  rename(areaCode = LSIP25CD, areaName = LSIP25NM) |>  # consistent naming
  mutate(areaName=str_squish(areaName)) |> 
  sf::st_transform(4326) |> # transform to WG84 that leaflet can plot
  select(areaCode,areaName,LAT,LONG,geog,geometry)

# 5 London boundary----
# Create Greater London Authority CA boundary from LA data
# Filter LSIP file to London LSIPs and rename as Greater London CA
LasLondonCA<-neatLSIP%>%
  filter(areaName %in% c("Central London Forward","Local London","South London Partnership","West London Alliance"))%>%
  mutate(CA="Greater London Authority")%>%
  select(-areaName)
# dissolve the LSIP LAs
sf::sf_use_s2(F) # to avoid overlapping error
LondonCAsh <- LasLondonCA %>%
  group_by(CA) %>%
  summarize(geometry = sf::st_union(geometry))
# turn into GeoJson
LondonCAgeojson <- sf::st_as_sf(LondonCAsh)

# neaten
neatLondonCA <- LondonCAgeojson %>%
  rename(areaName = CA) %>%
  mutate(areaCode = paste0("CA", nrow(neatCA)+1)
         ,geog="CA") %>%
  sf::st_transform(4326) %>%
  mutate(
    LONG = purrr::map_dbl(geometry, ~ sf::st_centroid(.x)[[1]]),
    LAT = purrr::map_dbl(geometry, ~ sf::st_centroid(.x)[[2]])
  )

# 5 Combine all boundary data ----
neatGeog <- bind_rows(
  neatCA,neatEngland, neatLA, neatLSIP,
  neatLondonCA # add GLA as an CA (it isn't officially but people like to find it there)
) %>%
  mutate(geogConcat = case_when(
    areaName == "England" ~ "England",
    TRUE ~ paste0(areaName, " ", geog)
  ))

#save data
save(neatGeog, file = "Data/processing/neatGeog.rdata")