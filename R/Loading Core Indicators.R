# load app data

## LEP data ----
# list of LEPS
C_LEP2020 <- read.csv(file = "./Data/AppData/C_LEP2020.csv", check.names = FALSE)

## Employment by occupation ----
# data for download
D_EmpOcc_APS1721 <- read.csv(file = "./Data/AppData/D_EmpOcc_APS1721.csv", check.names = FALSE)

## Employment level and rate ----
# data for download
D_EmpRate_APS1822 <- read.csv(file = "./Data/AppData/D_EmpRate_APS1822.csv", check.names = FALSE)
# data for dashboard
#C_EmpRate_APS1822 <- read.csv(file = "./Data/AppData/C_EmpRate_APS1822.csv", check.names = FALSE)
#C_EmpRate_APS1822_max_min <- read.csv(file = "./Data/AppData/C_EmpRate_APS1822_max_min.csv", check.names = FALSE)

## FE achievements ----
# data for download
D_Achieve_ILR1621 <- read.csv(file = "./Data/AppData/D_Achieve_ILR1621.csv", check.names = FALSE)
# data for dashboard
C_Achieve_ILR1621 <- read.csv(file = "./Data/AppData/C_Achieve_ILR1621.csv", check.names = FALSE)
C_Achieve_ILR1621_max_min <- read.csv(file = "./Data/AppData/C_Achieve_ILR1621_max_min.csv", check.names = FALSE)

## FE achievements by SSA----
# data for download
D_Achieve_ILR21 <- read.csv(file = "./Data/AppData/D_Achieve_ILR21.csv", check.names = FALSE)

## Enterprise by employment size ----
# data for download
D_empent_UBC1822 <- read.csv(file = "./Data/AppData/D_empent_UBC1822.csv", check.names = FALSE)

## KS4 destinations ----
# data for download
D_KS4destin_1521 <- read.csv(file = "./Data/AppData/D_KS4destin_1521.csv", check.names = FALSE)

## KS5 destinations ----
# data for download
D_KS5destin_1721 <- read.csv(file = "./Data/AppData/D_KS5destin_1721.csv", check.names = FALSE)

## 2.3 employment by industry ----
D_EmpInd_APS1822 <- read.csv(file = "./Data/AppData/D_EmpInd_APS1822.csv", check.names = FALSE)

## 2.4 qualification level by age and gender ----
# data for download
D_qual_APS1721 <- read.csv(file = "./Data/AppData/D_qual_APS1721.csv", check.names = FALSE)

## 4.2 Enterprise by employment size and industry----
# data for download
D_empentind_UBC1822 <- read.csv(file = "./Data/AppData/D_empentind_UBC1822.csv", check.names = FALSE)

# 5. Enterprise births, deaths and active ----
# data for download
D_enterprise_demo1621 <- read.csv(file = "./Data/AppData/D_enterprise_demo1621.csv", check.names = FALSE)

## Data table ----
I_DataTable <- read.csv(file = "./Data/AppData/I_DataTable.csv", check.names = FALSE)

C_qualevel3plus_APS1721 <- read.csv(file = "./Data/AppData/C_qualevel3plus_APS1721.csv", check.names = FALSE)
C_empentind3_UBC1822 <- read.csv(file = "./Data/AppData/C_empentind3_UBC1822.csv", check.names = FALSE)
C_KS4_KS5eduempapp <- read.csv(file = "./Data/AppData/C_KS4_KS5eduempapp.csv", check.names = FALSE)
C_KS5_eduempapp_max_min <- read.csv(file = "./Data/AppData/C_KS5_eduempapp_max_min.csv", check.names = FALSE)
C_empentind_max_min <- read.csv(file = "./Data/AppData/C_empentind_max_min.csv", check.names = FALSE)


# map tables
load(file = "./Data/AppData/C_Geog.rdata")

# v1 files
C_time <- read.csv(file = "./Data/AppData/C_time.csv", check.names = FALSE)
C_breakdown <- read.csv(file = "./Data/AppData/C_breakdown.csv", check.names = FALSE)
C_datahub <- read.csv(file = "./Data/AppData/C_datahub.csv", check.names = FALSE)
D_OnsProfTime <- read.csv(file = "./Data/AppData/D_OnsProfTime.csv", check.names = FALSE)
D_OnsProfDetail <- read.csv(file = "./Data/AppData/D_OnsProfDetail.csv", check.names = FALSE)
#C_OnsProfTime <- read.csv(file = "./Data/AppData/C_OnsProfTime.csv", check.names = FALSE)

## Intervention table ----
I_InterventionTable <- read.csv(file = "./Data/AppData/I_InterventionTable.csv", check.names = FALSE)

## Sources table ----
I_SourcesTable <- read.csv(file = "./Data/AppData/I_SourcesTable.csv", check.names = FALSE)

## data text table ----
I_DataText <- read.csv(file = "./Data/AppData/I_DataText.csv", check.names = FALSE)
