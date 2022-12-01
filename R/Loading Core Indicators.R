# load app data

## LEP data ----
# list of LEPS
C_LEP2020 <- read.csv(file = "./Data/AppData/C_LEP2020.csv", check.names = FALSE)

## Employment by occupation ----
# data for download
D_EmpOcc_APS1721 <- read.csv(file = "./Data/AppData/D_EmpOcc_APS1721.csv", check.names = FALSE)
# data for dashboard
C_EmpOcc_APS1721 <- read.csv(file = "./Data/AppData/C_EmpOcc_APS1721.csv", check.names = FALSE)

## Employment level and rate ----
# data for download
D_EmpRate_APS1822 <- read.csv(file = "./Data/AppData/D_EmpRate_APS1822.csv", check.names = FALSE)
# data for dashboard
C_EmpRate_APS1822 <- read.csv(file = "./Data/AppData/C_EmpRate_APS1822.csv", check.names = FALSE)
C_EmpRate_APS1822_max_min <- read.csv(file = "./Data/AppData/C_EmpRate_APS1822_max_min.csv", check.names = FALSE)

## employment by industry ----
D_EmpInd_APS1822 <- read.csv(file = "./Data/AppData/D_EmpInd_APS1822.csv", check.names = FALSE)

# data for dashboard
C_EmpInd2_APS1822 <- read.csv(file = "./Data/AppData/C_EmpInd2_APS1822.csv", check.names = FALSE)

## qualification level by age and gender ----
# data for download
D_qual_APS1721 <- read.csv(file = "./Data/AppData/D_qual_APS1721.csv", check.names = FALSE)

# data for dashboard
C_qual2_APS1721 <- read.csv(file = "./Data/AppData/C_qual2_APS1721.csv", check.names = FALSE)

## FE achievements ----
# data for download
D_Achieve_ILR1621 <- read.csv(file = "./Data/AppData/D_Achieve_ILR1621.csv", check.names = FALSE)
# data for dashboard
C_Achieve_ILR1621 <- read.csv(file = "./Data/AppData/C_Achieve_ILR1621.csv", check.names = FALSE)
C_Achieve_ILR1621_max_min <- read.csv(file = "./Data/AppData/C_Achieve_ILR1621_max_min.csv", check.names = FALSE)

## FE achievements by SSA----
# data for download
D_Achieve_ILR21 <- read.csv(file = "./Data/AppData/D_Achieve_ILR21.csv", check.names = FALSE)
# data for dashboard
C_Achieve_ILR21 <- read.csv(file = "./Data/AppData/C_Achieve_ILR21.csv", check.names = FALSE)

## ONS vacancy ----
# data for download
C_Vacancy_ONS1722 <- read.csv(file = "./Data/AppData/C_Vacancy_ONS1722.csv", check.names = FALSE)
# data for dashboard
C_Vacancy_England <- read.csv(file = "./Data/AppData/C_Vacancy_England.csv", check.names = FALSE)
C_Vacancy_England_max_min <- read.csv(file = "./Data/AppData/C_Vacancy_England_max_min.csv", check.names = FALSE)
C_Vacancy_England_change <- read.csv(file = "./Data/AppData/C_Vacancy_England_change.csv", check.names = FALSE)

## Enterprise by employment size ----
# data for download
D_empent_UBC1822 <- read.csv(file = "./Data/AppData/D_empent_UBC1822.csv", check.names = FALSE)

## KS4 destinations ----
# data for download
D_KS4destin_1521 <- read.csv(file = "./Data/AppData/D_KS4destin_1521.csv", check.names = FALSE)

# data for dashboard
C_KS4destin_1521 <- read.csv(file = "./Data/AppData/C_KS4destin_1521.csv", check.names = FALSE)

## KS5 destinations ----
# data for download
D_KS5destin_1721 <- read.csv(file = "./Data/AppData/D_KS5destin_1721.csv", check.names = FALSE)

#data for dashboard
C_KS5destin_1721 <- read.csv(file = "./Data/AppData/C_KS5destin_1721.csv", check.names = FALSE)

#data for dashboard
C_KS4_KS5_2021 <- read.csv(file = "./Data/AppData/C_KS4_KS5_2021.csv", check.names = FALSE)


## Data table ----
I_DataTable <- read.csv(file = "./Data/AppData/I_DataTable.csv", check.names = FALSE)
