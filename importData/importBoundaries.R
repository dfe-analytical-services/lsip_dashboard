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
  left_join(C_LADLSIP %>% mutate(LSIP = paste0(LSIP23NM, " LSIP")) %>% select(LAD23CD, LSIP), by = c("LAD24CD" = "LAD23CD")) %>%
  left_join(C_mcalookup %>% mutate(MCA = paste0(CAUTH24NM, " MCA")) %>% select(LAD24CD, MCA), by = c("LAD24CD" = "LAD24CD")) %>%
  filter(is.na(LSIP) == FALSE) %>% # remove non England
  mutate(MCA = case_when(LSIP == "Greater London LSIP" ~ "Greater London Authority MCA", TRUE ~ MCA)) %>% # add on gla as mca
  rename(areaName = LAD24NM, areaCode = LAD24CD) %>%
  sf::st_transform(4326) # transform to WG84 that leaflet can plot

# 3 MCA boundary----
folder <- "1-7_MCABoundary"
I_mapMCA <- sf::st_read(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))),
                        stringsAsFactors = F
)

neatMCA <- I_mapMCA %>%
  mutate(geog = "MCA") %>% # add geog type
  rename(areaCode = CAUTH24CD, areaName = CAUTH24NM) %>% # consistent naming
  sf::st_transform(4326) # transform to WG84 that leaflet can plot

# 3 LSIP boundary----
folder <- "1-8_LSIPBoundary"
I_mapLSIP <- sf::st_read(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))),
                        stringsAsFactors = F
)

neatLSIP <- I_mapLSIP %>%
  mutate(geog = "LSIP") %>% # add geog type
  rename(areaCode = LSIP23CD, areaName = LSIP23NM) %>% # consistent naming
  sf::st_transform(4326) # transform to WG84 that leaflet can plot

# 5 Combine all boundary data ----
neatGeog <- bind_rows(
  neatMCA, neatEngland, neatLA, neatLSIP,
  neatLSIP %>% filter(areaCode == "E69000013") %>% mutate(areaName = "Greater London Authority", geog = "MCA") # add GLA as an MCA (it isn't officially but people like to find it there)
) %>%
  mutate(geogConcat = case_when(
    areaName == "England" ~ "England",
    TRUE ~ paste0(areaName, " ", geog)
  ))
