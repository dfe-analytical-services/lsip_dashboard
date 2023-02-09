# load app data

# 1. LEP data ----
# list of LEPS
C_LEP2020 <- read.csv(file = "./Data/AppData/C_LEP2020.csv", check.names = FALSE)

# 2. APS data ----
## 2.1 Employment by occupation ----
# data for download
D_EmpOcc_APS1721 <- read.csv(file = "./Data/AppData/D_EmpOcc_APS1721.csv", check.names = FALSE)
# data for dashboard
C_EmpOcc_APS1721 <- read.csv(file = "./Data/AppData/C_EmpOcc_APS1721.csv", check.names = FALSE)

## 2.2 Employment level and rate ----
# data for download
D_EmpRate_APS1822 <- read.csv(file = "./Data/AppData/D_EmpRate_APS1822.csv", check.names = FALSE)
# data for dashboard
C_EmpRate_APS1822 <- read.csv(file = "./Data/AppData/C_EmpRate_APS1822.csv", check.names = FALSE)
C_EmpRate_APS1822_max_min <- read.csv(file = "./Data/AppData/C_EmpRate_APS1822_max_min.csv", check.names = FALSE)

## 2.3 employment by industry ----
D_EmpInd_APS1822 <- read.csv(file = "./Data/AppData/D_EmpInd_APS1822.csv", check.names = FALSE)

# data for dashboard
C_EmpInd2_APS1822 <- read.csv(file = "./Data/AppData/C_EmpInd2_APS1822.csv", check.names = FALSE)

## 2.4 qualification level by age and gender ----
# data for download
D_qual_APS1721 <- read.csv(file = "./Data/AppData/D_qual_APS1721.csv", check.names = FALSE)

# data for dashboard
C_qual2_APS1721 <- read.csv(file = "./Data/AppData/C_qual2_APS1721.csv", check.names = FALSE)

# data for dashboard
# C_qual_max_min <- read.csv(file = "./Data/AppData/C_qual_max_min.csv", check.names = FALSE)

# data for dashbaord level 3 and below
C_qualevel2_APS1721 <- read.csv(file = "./Data/AppData/C_qualevel2_APS1721.csv", check.names = FALSE)

# data for dashbaord level 3 and below
C_qualevel3plus_APS1721 <- read.csv(file = "./Data/AppData/C_qualevel3plus_APS1721.csv", check.names = FALSE)

# 3.FE achievements ----
# data for download
D_Achieve_ILR1621 <- read.csv(file = "./Data/AppData/D_Achieve_ILR1621.csv", check.names = FALSE)
# data for dashboard
C_Achieve_ILR1621 <- read.csv(file = "./Data/AppData/C_Achieve_ILR1621.csv", check.names = FALSE)
C_Achieve_ILR1621_max_min <- read.csv(file = "./Data/AppData/C_Achieve_ILR1621_max_min.csv", check.names = FALSE)

## 3.1 FE achievements by SSA----
# data for download
D_Achieve_ILR21 <- read.csv(file = "./Data/AppData/D_Achieve_ILR21.csv", check.names = FALSE)
# data for dashboard
C_Achieve_ILR21 <- read.csv(file = "./Data/AppData/C_Achieve_ILR21.csv", check.names = FALSE)

# 4. ONS vacancy ----
# data for download
# C_Vacancy_ONS1722 <- read.csv(file = "./Data/AppData/C_Vacancy_ONS1722.csv", check.names = FALSE)
# data for dashboard
# C_Vacancy_England <- read.csv(file = "./Data/AppData/C_Vacancy_England.csv", check.names = FALSE)
# C_Vacancy_England_max_min <- read.csv(file = "./Data/AppData/C_Vacancy_England_max_min.csv", check.names = FALSE)
# C_Vacancy_England_change <- read.csv(file = "./Data/AppData/C_Vacancy_England_change.csv", check.names = FALSE)

## 4.1 Enterprise by employment size ----
# data for download
D_empent_UBC1822 <- read.csv(file = "./Data/AppData/D_empent_UBC1822.csv", check.names = FALSE)

# data for dashboard
C_empent2_UBC1822 <- read.csv(file = "./Data/AppData/C_empent2_UBC1822.csv", check.names = FALSE)

## 4.2 Enterprise by employment size and industry----
# data for download
D_empentind_UBC1822 <- read.csv(file = "./Data/AppData/D_empentind_UBC1822.csv", check.names = FALSE)

# data for dashboard
C_empentind3_UBC1822 <- read.csv(file = "./Data/AppData/C_empentind3_UBC1822.csv", check.names = FALSE)

# data for dashboard
C_empentind_max_min <- read.csv(file = "./Data/AppData/C_empentind_max_min.csv", check.names = FALSE)
## 4.3  Ons by profession ----
D_OnsProfDetail <- read.csv(file = "./Data/AppData/D_OnsProfDetail.csv", check.names = FALSE)
D_OnsProfTime <- read.csv(file = "./Data/AppData/D_OnsProfTime.csv", check.names = FALSE)
C_OnsProfTime <- read.csv(file = "./Data/AppData/C_OnsProfTime.csv", check.names = FALSE)
C_OnsProfDetail <- read.csv(file = "./Data/AppData/C_OnsProfDetail.csv", check.names = FALSE)
# C_VacPcArea <- read.csv(file = "./Data/AppData/C_VacPcArea.csv", check.names = FALSE)

# 5. Enterprise births, deaths and active ----
# data for download
D_enterprise_demo1621 <- read.csv(file = "./Data/AppData/D_enterprise_demo1621.csv", check.names = FALSE)

# data for dashboard
C_enterprise_demo1621 <- read.csv(file = "./Data/AppData/C_enterprise_demo1621.csv", check.names = FALSE)


# 6. KS4 and KS5 destinations ----
# data for download
D_KS4destin_1521 <- read.csv(file = "./Data/AppData/D_KS4destin_1521.csv", check.names = FALSE)
D_KS5destin_1721 <- read.csv(file = "./Data/AppData/D_KS5destin_1721.csv", check.names = FALSE)

# data for dashboard
C_KS4_KS5_2021 <- read.csv(file = "./Data/AppData/C_KS4_KS5_2021.csv", check.names = FALSE)

# data for dashboard
C_KS4_KS5eduempapp <- read.csv(file = "./Data/AppData/C_KS4_KS5eduempapp.csv", check.names = FALSE)

# data for dashboard
C_KS5_eduempapp_max_min <- read.csv(file = "./Data/AppData/C_KS5_eduempapp_max_min.csv", check.names = FALSE)


## 7. Data table ----
I_DataTable <- read.csv(file = "./Data/AppData/I_DataTable.csv", check.names = FALSE)
