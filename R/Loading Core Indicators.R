# load app data

load(file = "./Data/AppData/C_Geog.rdata")
C_time1 <- read.csv(file = "./Data/AppData/C_time1.csv", check.names = FALSE)
C_time2 <- read.csv(file = "./Data/AppData/C_time2.csv", check.names = FALSE)
C_time <- bind_rows(C_time1, C_time2)
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
I_ReportsTable <- read.csv(file = "./Data/AppData/I_ReportsTable.csv", check.names = FALSE)

## data text table ----
I_DataText <- read.csv(file = "./Data/AppData/I_DataText.csv", check.names = FALSE)

## Data table ----
I_DataTable <- read.csv(file = "./Data/AppData/I_DataTable.csv", check.names = FALSE)
