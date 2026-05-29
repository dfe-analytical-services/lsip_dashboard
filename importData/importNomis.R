# Nomis datasets
library(purrr)
library(rsdmx)

# Get IDs of LADUs (TYPE424), CAs (TYPE442), countries (TYPE499)
nomis_geog_types <- as.data.frame(readSDMX("https://www.nomisweb.co.uk/api/v01/codelist/CL_17_1_GEOGRAPHY/TYPE424,TYPE442,TYPE499.def.sdmx.xml"))

# now create the string for the geographies we define (that are not stored in the NOMIS geographies)
userGeogString <- C_LADLSIP %>%
  group_by(LSIPname) %>%
  summarise(
    make_geo = paste0(
      "MAKE|", gsub(" ", "%20", first(LSIPname)), "|",
      paste(unique(LAD23CD), collapse = ";")
    ),
    .groups = "drop"
  )

#Also make GLA geography api string
userGeogStringGLA <- C_calookup %>%
  filter(CAUTH25NM=="Greater London Authority")%>%
  group_by(CAUTH25NM) %>%
  summarise(
    make_geo = paste0(
      "MAKE|", gsub(" ", "%20", first(CAUTH25NM)), "|",
      paste(unique(LAD25CD), collapse = ";")
    ),
    .groups = "drop"
  )

#combine all into one long geography string
geog_all <- paste0(paste(nomis_geog_types$id, collapse = ","),",",paste(userGeogStringGLA$make_geo, collapse = ","),",",paste(userGeogString$make_geo, collapse = ","))

#get a list of cells available to filter later for each dataset
cellsListAps <- readr::read_csv(paste0("https://www.nomisweb.co.uk/api/v01/dataset/NM_17_1.data.csv?date=latest&geography=E92000001&measures=20100"))  |> #just pick England and one measure to get a row per cell
  select(CELL,CELL_NAME)

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
