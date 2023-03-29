# load app data

load(file = "./Data/AppData/C_Geog.rdata")
C_time <- read.csv(file = "./Data/AppData/C_time.csv", check.names = FALSE)
C_breakdown <- read.csv(file = "./Data/AppData/C_breakdown.csv", check.names = FALSE)
C_detailLookup <- read.csv(file = "./Data/AppData/C_detailLookup.csv", check.names = FALSE)
C_topTenEachBreakdown <- read.csv(file = "./Data/AppData/C_topTenEachBreakdown.csv", check.names = FALSE)
C_datahub <- read.csv(file = "./Data/AppData/C_datahub.csv", check.names = FALSE)
C_axisMinMax <- read.csv(file = "./Data/AppData/C_axisMinMax.csv", check.names = FALSE)

## Intervention table ----
# I_InterventionTable <- read.csv(file = "./Data/AppData/I_InterventionTable.csv", check.names = FALSE)

## Sources table ----
I_SourcesTable <- read.csv(file = "./Data/AppData/I_SourcesTable.csv", check.names = FALSE)
I_ToolsTable <- read.csv(file = "./Data/AppData/I_ToolsTable.csv", check.names = FALSE)

## data text table ----
I_DataText <- read.csv(file = "./Data/AppData/I_DataText.csv", check.names = FALSE)

## Data table ----
I_DataTable <- read.csv(file = "./Data/AppData/I_DataTable.csv", check.names = FALSE)
