# load app data

load(file = "./Data/AppData/C_Geog.rdata")
C_time <- readRDS("Data/AppData/C_time.rds")
C_breakdown <- readRDS("Data/AppData/C_breakdown.rds")
C_detailLookup <- readRDS("Data/AppData/C_detailLookup.rds")
C_topTenEachBreakdown <- readRDS("Data/AppData/C_topTenEachBreakdown.rds")
C_datahub <- readRDS("Data/AppData/C_datahub.rds")
C_axisMinMax <- readRDS("Data/AppData/C_axisMinMax.rds")
areaChoices <- readRDS("Data/AppData/areaChoices.rds")
jobAdsGeog <- readRDS("job_ads_page/job_ads_page_geog.rds")
jobAdsMap <- readRDS("job_ads_page/job_ads_page_map.rds")
jobAdsLineChart <- readRDS("job_ads_page/job_ads_page_line_chart.rds")
jobAdsEmerging <- readRDS("job_ads_page/job_ads_page_emerging.rds")
jobAdsConstant <- readRDS("job_ads_page/job_ads_page_constant.rds")
jobAdsRanking <- readRDS("job_ads_page/job_ads_page_ranking.rds")
socQualityIssues <- read.csv(file = "job_ads_page/SOC_issues.csv", check.names = FALSE)

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
