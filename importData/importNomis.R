# Nomis datasets
# get all APS data that comes via the nomis API

# First list of all the geographies we need (excluding the user defined which are added in the function)
geogUseAps <- nomisr::nomis_get_metadata(id = "NM_17_1", concept = "geography", type = "type") %>%
  filter(description.en %in% c("combined authorities","local authorities: district / unitary (as of April 2021)", "countries"))

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

