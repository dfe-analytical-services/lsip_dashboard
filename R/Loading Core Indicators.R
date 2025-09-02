# load app data

load(file = "./Data/AppData/C_Geog.rdata")
C_time <- readRDS("Data/AppData/C_time.rds")
C_breakdown <- readRDS("Data/AppData/C_breakdown.rds")
C_detailLookup <- readRDS("Data/AppData/C_detailLookup.rds")
C_topTenEachBreakdown <- readRDS("Data/AppData/C_topTenEachBreakdown.rds")
C_datahub <- readRDS("Data/AppData/C_datahub.rds")
C_axisMinMax <- readRDS("Data/AppData/C_axisMinMax.rds")
areaChoices <- readRDS("Data/AppData/areaChoices.rds")

## Intervention table ----
# I_InterventionTable <- read.csv(file = "./Data/AppData/I_InterventionTable.csv", check.names = FALSE)

## Sources table ----
I_SourcesTable <- readRDS("Data/AppData/I_SourcesTable.rds")
I_ToolsTable <- readRDS("Data/AppData/I_ToolsTable.rds")
I_ReportsTable <- readRDS("Data/AppData/I_ReportsTable.rds")

## data text table ----
I_DataText <- readRDS("Data/AppData/I_DataText.rds")

## Data table ----
I_DataTable <- readRDS("Data/AppData/I_DataTable.rds")
