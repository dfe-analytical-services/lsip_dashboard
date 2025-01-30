#Import all geographical boundaries

# 1 LEP boundary----
folder <- "1-5_LEPBoundary"
I_mapLEP <- sf::st_read(paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))),
                        stringsAsFactors = F
)

neatLEP <- I_mapLEP %>%
  mutate(geog = "LEP") %>% # add geog type
  rename(areaCode = LEP22CD, areaName = LEP22NM) %>% # consistent naming
  inner_join(distinct(F_LEP2020, LEP23CD1), by = c("areaCode" = "LEP23CD1")) %>% # remove any areas that are no longer LEPs in 2023 (Black Country and Coventry)
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
  # add on lsip, lep and mca groupings
  left_join(F_LEP2020 %>% mutate(LEP = paste0(LEP23NM1, " LEP"), LEP2 = paste0(LEP23NM2, " LEP"), LSIP = paste0(LSIP23NM, " LSIP")) %>% select(LAD23CD, LSIP, LEP, LEP2), by = c("LAD24CD" = "LAD23CD")) %>%
  left_join(C_mcalookup %>% mutate(MCA = paste0(CAUTH24NM, " MCA")) %>% select(LAD24CD, MCA), by = c("LAD24CD" = "LAD24CD")) %>%
  filter(is.na(LSIP) == FALSE) %>% # remove non England
  mutate(MCA = case_when(LEP == "The London Economic Action Partnership LEP" ~ "Greater London Authority MCA", TRUE ~ MCA)) %>% # add on gla as mca
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

# 4 England blank data ----
addEngland <- data.frame(
  areaName = "England", areaCode = "x",
  geog = "COUNTRY"
)

# 5 Combine all boundary data ----
neatGeog <- bind_rows(
  neatMCA, neatLEP, addEngland, neatLA, neatLSIP,
  neatLEP %>% filter(areaCode == "E37000051") %>% mutate(areaName = "Greater London Authority", geog = "MCA"), # add GLA as an MCA (it isn't officially but people like to find it there)
  neatLA %>%
    filter(LEP2 != "0 LEP") %>%
    select(-LEP) %>%
    rename(LEP = LEP2) # add LAs with more than one LEP
) %>%
  mutate(geogConcat = case_when(
    areaName == "England" ~ "England",
    TRUE ~ paste0(areaName, " ", geog)
  ))
