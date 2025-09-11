#Import all geographical boundaries

# 1 England boundary----
folder <- "1-5_EnglandBoundary"
I_mapEngland <- sf::st_read(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))),
                            stringsAsFactors = F
)

neatEngland <- I_mapEngland %>%
  mutate(geog = "England") %>% # add geog type
  rename(areaCode = CTRY24CD, areaName = CTRY24NM) %>% # consistent naming
  select(-CTRY24NMW)%>%
  sf::st_transform(4326) # transform to WG84 that leaflet can plot

# 2 LA boundary----
folder <- "1-6_LABoundary"
I_mapLA <- sf::st_read(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))),
                       stringsAsFactors = F
)

# Neaten
neatLA <- I_mapLA %>%
  select(-LAD24NMW) %>% # remove extra welsh column
  mutate(geog = "LADU") %>% # add geog type
  rename(OBJECTID = FID) %>% # consistent naming
  # add on lsip, and mca groupings
  left_join(C_LADLSIP %>% mutate(LSIP = paste0(LSIPname, " LSIP")) %>% select(LAD23CD, LSIP), by = c("LAD24CD" = "LAD23CD")) %>%
  left_join(C_mcalookup %>% mutate(MCA = paste0(CAUTH25NM, " MCA")) %>% select(LAD25CD, MCA), by = c("LAD24CD" = "LAD25CD")) %>%
  filter(is.na(LSIP) == FALSE) %>% # remove non England
  mutate(MCA = case_when(LSIP == "Greater London LSIP" ~ "Greater London Authority MCA", TRUE ~ MCA)) %>% # add on gla as mca
  rename(areaName = LAD24NM, areaCode = LAD24CD) %>%
  sf::st_transform(4326) # transform to WG84 that leaflet can plot

# 3 MCA boundary----
folder <- "1-7_MCABoundary"
I_mapMCA <- sf::st_read(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))),
                        stringsAsFactors = F
)

neatOldMCA <- I_mapMCA %>%
  mutate(geog = "MCA") %>% # add geog type
  rename(areaCode = CAUTH24CD, areaName = CAUTH24NM) %>% # consistent naming
  sf::st_transform(4326) # transform to WG84 that leaflet can plot

# Add on new MCA boundary from LA data (that are not in the current boundary file)
# add on MCAs to LA file
LasMCA <- merge(I_mapLA, C_mcalookup %>% 
                  filter(CAUTH25NM %in% c("Devon and Torbay", "Greater Lincolnshire", "Hull and East Yorkshire", "Lancashire"))%>%
                  select(LAD25CD, MCA = CAUTH25NM), 
                by.x = "LAD24CD", by.y = "LAD25CD")
# dissolve the MCA LAs
sf::sf_use_s2(F) # to avoid overlapping error
MCAsh <- LasMCA %>%
  group_by(MCA) %>%
  summarize(geometry = sf::st_union(geometry))
# turn into GeoJson
MCAgeojson <- sf::st_as_sf(MCAsh)

# neaten
neatNewMCA <- MCAgeojson %>%
  rename(areaName = MCA) %>%
  mutate(areaCode = paste0("MCA", row_number())
         ,geog="MCA") %>%
  sf::st_transform(4326) %>%
  mutate(
    LONG = purrr::map_dbl(geometry, ~ sf::st_centroid(.x)[[1]]),
    LAT = purrr::map_dbl(geometry, ~ sf::st_centroid(.x)[[2]])
  )

# 3 LSIP boundary----
# Create LSIP boundary from LA data
# add on LSIPs to LA file
LasLsip <- merge(I_mapLA, C_LADLSIP %>% select(LAD23CD, LSIP = LSIPname), by.x = "LAD24CD", by.y = "LAD23CD")
# dissolve the LSIP LAs
sf::sf_use_s2(F) # to avoid overlapping error
LSIPsh <- LasLsip %>%
  group_by(LSIP) %>%
  summarize(geometry = sf::st_union(geometry))
# turn into GoeJson
LSIPgeojson <- sf::st_as_sf(LSIPsh)

# neaten
neatLSIP <- LSIPgeojson %>%
  rename(areaName = LSIP) %>%
  mutate(areaCode = paste0("LSIP", row_number())
         ,geog="LSIP") %>%
  sf::st_transform(4326) %>%
  mutate(
    LONG = purrr::map_dbl(geometry, ~ sf::st_centroid(.x)[[1]]),
    LAT = purrr::map_dbl(geometry, ~ sf::st_centroid(.x)[[2]])
  )

# 5 London boundary----
# Create Greater London Authority MCA boundary from LA data
# Filter LA file to London LSIPs and rename as Greater London MCA
LasLondonMCA<-LasLsip%>%
  filter(LSIP %in% c("Central London Forward","Local London","South London Partnership","West London Alliance"))%>%
  mutate(MCA="Greater London Authority")%>%
  select(-LSIP)
# dissolve the LSIP LAs
sf::sf_use_s2(F) # to avoid overlapping error
LondonMCAsh <- LasLondonMCA %>%
  group_by(MCA) %>%
  summarize(geometry = sf::st_union(geometry))
# turn into GoeJson
LondonMCAgeojson <- sf::st_as_sf(LondonMCAsh)

# neaten
neatLondonMCA <- LondonMCAgeojson %>%
  rename(areaName = MCA) %>%
  mutate(areaCode = paste0("MCA", nrow(neatNewMCA)+1)
         ,geog="MCA") %>%
  sf::st_transform(4326) %>%
  mutate(
    LONG = purrr::map_dbl(geometry, ~ sf::st_centroid(.x)[[1]]),
    LAT = purrr::map_dbl(geometry, ~ sf::st_centroid(.x)[[2]])
  )

# 5 Combine all boundary data ----
neatGeog <- bind_rows(
  neatOldMCA,neatNewMCA, neatEngland, neatLA, neatLSIP,
  neatLondonMCA # add GLA as an MCA (it isn't officially but people like to find it there)
) %>%
  mutate(geogConcat = case_when(
    areaName == "England" ~ "England",
    TRUE ~ paste0(areaName, " ", geog)
  ))