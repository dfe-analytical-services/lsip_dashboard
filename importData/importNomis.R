# Nomis datasets
# get all APS data that comes via the nomis API
library(nomisr)

# First list of all the geographies we need (excluding the user defined which are added in the function)
geogUseAps <- nomisr::nomis_get_metadata(id = "NM_17_1", concept = "geography", type = "type") %>%
  filter(description.en %in% c("combined authorities (as of May 2025)","local authorities: district / unitary (as of April 2021)", "countries"))

# now create the api string for the geographies we define (that are not stored in the NOMI geographies)
userGeogString <- C_LADLSIP %>%
  group_by(LSIPname) %>%
  summarise(
    make_geo = paste0(
      "MAKE|", gsub(" ", "%20", first(LSIPname)), "|",
      paste(unique(LAD23CD), collapse = ";")
    ),
    .groups = "drop"
  )

# Combine strings
geo_param <- paste(userGeogString$make_geo, collapse = ",")

#Also make GLA geography api string
userGeogStringGLA <- C_mcalookup %>%
  filter(CAUTH25NM=="Greater London Authority")%>%
  group_by(CAUTH25NM) %>%
  summarise(
    make_geo = paste0(
      "MAKE|", gsub(" ", "%20", first(CAUTH25NM)), "|",
      paste(unique(LAD25CD), collapse = ";")
    ),
    .groups = "drop"
  )

# Combine strings
geo_paramGLA <- paste(userGeogStringGLA$make_geo, collapse = ",")

# list all the APS cells available
cellsListAps <- nomisr::nomis_get_metadata(id = "NM_17_1", concept = "CELL")

### 1 Employment level and rate ------------
source("importData/importEmp.R", echo=TRUE)

### 2 Employment by occupation ----
source("importData/importEmpOcc.R", echo=TRUE)

### 3 Employment by industry------------
source("importData/importEmpIndustry.R", echo=TRUE)

### 4 UK Business Count----
source("importData/importEnterprises.R", echo=TRUE)

### 5 Skill by age gender ------------
source("importData/importQualification.R", echo=TRUE)
