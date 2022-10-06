# check the basic shape of updated files match previous files
# to be run after 1)ExtractLoadData.R and 2)TransformData.R and 3)R/Loading Core Indicators.R

# Load in base versions of app file to compare against
## LEP data ----
# list of LEPS
C_LEP2020Base <- read.csv(file = "Data\\AppDataBaseForQA\\C_LEP2020.csv", check.names = FALSE)

## Employment by occupation ----
# data for download
D_EmpOcc_APS1721Base <- read.csv(file = "Data\\AppDataBaseForQA\\D_EmpOcc_APS1721.csv", check.names = FALSE)
# data for dashboard
C_EmpOcc_APS1721Base <- read.csv(file = "Data\\AppDataBaseForQA\\C_EmpOcc_APS1721.csv", check.names = FALSE)

## Employment level and rate ----
# data for download
D_EmpRate_APS1721Base <- read.csv(file = "Data\\AppDataBaseForQA\\D_EmpRate_APS1721.csv", check.names = FALSE)
# data for dashboard
C_EmpRate_APS1721Base <- read.csv(file = "Data\\AppDataBaseForQA\\C_EmpRate_APS1721.csv", check.names = FALSE)
C_EmpRate_APS1721_max_minBase <- read.csv(file = "Data\\AppDataBaseForQA\\C_EmpRate_APS1721_max_min.csv", check.names = FALSE)

## FE achievements ----
# data for download
D_Achieve_ILR1621Base <- read.csv(file = "Data\\AppDataBaseForQA\\D_Achieve_ILR1621.csv", check.names = FALSE)
# data for dashboard
C_Achieve_ILR1621Base <- read.csv(file = "Data\\AppDataBaseForQA\\C_Achieve_ILR1621.csv", check.names = FALSE)
C_Achieve_ILR1621_max_minBase <- read.csv(file = "Data\\AppDataBaseForQA\\C_Achieve_ILR1621_max_min.csv", check.names = FALSE)

## FE achievements by SSA----
# data for download
D_Achieve_ILR21Base <- read.csv(file = "Data\\AppDataBaseForQA\\D_Achieve_ILR21.csv", check.names = FALSE)
# data for dashboard
C_Achieve_ILR21Base <- read.csv(file = "Data\\AppDataBaseForQA\\C_Achieve_ILR21.csv", check.names = FALSE)

## ONS vacancy ----
# data for download
C_Vacancy_ONS1722Base <- read.csv(file = "Data\\AppDataBaseForQA\\C_Vacancy_ONS1722.csv", check.names = FALSE)
# data for dashboard
C_Vacancy_EnglandBase <- read.csv(file = "Data\\AppDataBaseForQA\\C_Vacancy_England.csv", check.names = FALSE)
C_Vacancy_England_max_minBase <- read.csv(file = "Data\\AppDataBaseForQA\\C_Vacancy_England_max_min.csv", check.names = FALSE)
C_Vacancy_England_changeBase <- read.csv(file = "Data\\AppDataBaseForQA\\C_Vacancy_England_change.csv", check.names = FALSE)

# check there are 38 leps
QA <- data.frame(QAcheck = nrow(C_LEP2020) == nrow(C_LEP2020Base))
# Check data tables have same columns and class base case, you might get cases where the year is an interger in one and a numeric in another. not a problem for the dashboard
QA[2, 1] <- identical(D_EmpOcc_APS1721[NA, ][1, ], D_EmpOcc_APS1721Base[NA, ][1, ])
QA[3, 1] <- identical(C_EmpOcc_APS1721[NA, ][1, ], C_EmpOcc_APS1721Base[NA, ][1, ])
QA[4, 1] <- identical(D_EmpRate_APS1721[NA, ][1, ], D_EmpRate_APS1721Base[NA, ][1, ])
QA[5, 1] <- identical(C_EmpRate_APS1721[NA, ][1, ], C_EmpRate_APS1721Base[NA, ][1, ])
QA[6, 1] <- identical(C_EmpRate_APS1721_max_min[NA, ][1, ], C_EmpRate_APS1721_max_minBase[NA, ][1, ])
QA[7, 1] <- identical(D_Achieve_ILR1621[NA, ][1, ], D_Achieve_ILR1621Base[NA, ][1, ])
QA[8, 1] <- identical(C_Achieve_ILR1621[NA, ][1, ], C_Achieve_ILR1621Base[NA, ][1, ])
QA[9, 1] <- identical(C_Achieve_ILR1621_max_min[NA, ][1, ], C_Achieve_ILR1621_max_minBase[NA, ][1, ])
QA[10, 1] <- identical(D_Achieve_ILR21[NA, ][1, ], D_Achieve_ILR21Base[NA, ][1, ])
QA[11, 1] <- identical(C_Achieve_ILR21[NA, ][1, ], C_Achieve_ILR21Base[NA, ][1, ])
QA[12, 1] <- identical(C_Vacancy_ONS1722[NA, ][1, ], C_Vacancy_ONS1722Base[NA, ][1, ])
QA[13, 1] <- identical(C_Vacancy_England[NA, ][1, ], C_Vacancy_EnglandBase[NA, ][1, ])
QA[14, 1] <- identical(C_Vacancy_England_max_min[NA, ][1, ], C_Vacancy_England_max_minBase[NA, ][1, ])
QA[15, 1] <- identical(C_Vacancy_England_change[NA, ][1, ], C_Vacancy_England_changeBase[NA, ][1, ])

# check they have the same key categories
QA[16, 1] <- identical(distinct(D_EmpOcc_APS1721, area, geographic_level), distinct(D_EmpOcc_APS1721Base, area, geographic_level))
QA[17, 1] <- identical(distinct(C_EmpOcc_APS1721, year, area, geographic_level), distinct(C_EmpOcc_APS1721Base, year, area, geographic_level))
QA[18, 1] <- identical(distinct(D_EmpRate_APS1721, area, geographic_level), distinct(D_EmpRate_APS1721, area, geographic_level))
QA[19, 1] <- identical(distinct(C_EmpRate_APS1721, area, geographic_level), distinct(C_EmpRate_APS1721, area, geographic_level))
QA[20, 1] <- identical(distinct(C_EmpRate_APS1721_max_min, area), distinct(C_EmpRate_APS1721_max_min, area))
QA[21, 1] <- identical(distinct(D_Achieve_ILR1621, area, geographic_level, time_period, age_group, level_or_type), distinct(D_Achieve_ILR1621, area, geographic_level, time_period, age_group, level_or_type))
QA[22, 1] <- identical(distinct(C_Achieve_ILR1621,area, geographic_level, time_period, age_group, level_or_type), distinct(C_Achieve_ILR1621, area, geographic_level, time_period, age_group, level_or_type))
QA[23, 1] <- identical(distinct(C_Achieve_ILR1621_max_min, area, geographic_level, level_or_type), distinct(C_Achieve_ILR1621_max_min, area, geographic_level, level_or_type))
QA[24, 1] <- identical(distinct(D_Achieve_ILR21, area, geographic_level, ssa_t1_desc), distinct(D_Achieve_ILR21, area, geographic_level, ssa_t1_desc))
QA[25, 1] <- identical(distinct(C_Achieve_ILR21, area, geographic_level, SSA), distinct(C_Achieve_ILR21, area, geographic_level, SSA))
QA[26, 1] <- identical(distinct(C_Vacancy_ONS1722, year,area,geographic_level), distinct(C_Vacancy_ONS1722, year,area,geographic_level))
QA[27, 1] <- identical(distinct(C_Vacancy_England, year,area,geographic_level), distinct(C_Vacancy_England, year,area,geographic_level))
QA[28, 1] <- identical(distinct(C_Vacancy_England_max_min,area,geographic_level), distinct(C_Vacancy_England_max_min,area,geographic_level))
QA[29, 1] <- identical(distinct(C_Vacancy_England_change,area,geographic_level), distinct(C_Vacancy_England_change,area,geographic_level))

# check for any errors
print("If you have any errors they will be listed here:")
which(QA == "FALSE", arr.ind = TRUE)
