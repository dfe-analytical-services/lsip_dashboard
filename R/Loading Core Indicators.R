####
# Title: LSIP dashboard - proof of concept
# Author: Hannah Cox/Paul James
# Date: 18th May 2022
# Last updated: 15th June 2022
###

# Load libraries ----
library(dplyr)
library(data.table)
library(tidyverse)
library(stringr)
library(eeptools)
library(odbc)
library(ggplot2)
library(openxlsx)
library(janitor)

# Load data ----
## LEP 2020 ----
I_LEP2020 <- read.xlsx(xlsxFile="./Data/OA11_LAD21_LSOA11_MSOA11_LEP21_EN_v3.xlsx", sheet=1, skipEmptyRows=T)
C_LEP2020 <- I_LEP2020 %>%
  distinct(LEP=LEP21NM1)

## APS ----
### Core indicator 2: Employment by occupation ----
# Download from https://www.nomisweb.co.uk/datasets/apsnew
#Query data
#Geography: England, LEPs, regions, LADs (as of April 2021)
#Date: 12 months to Dec 2017-2021
#Cell: T09a Employment by occupation (SOC2010) sub-major group and full-time/part-time; All people/ All people
I_EmpOcc_APS1721 <- read.xlsx(xlsxFile="./Data/nomis_2022_06_14_092401.xlsx",skipEmptyRows=T)

### Core indicator 5: Employment level and rate ------------
#Geog and date as above
#Cell: T01 Economic activity by age Aged 16-64/ All people
I_EmpRate_APS1721 <- read.xlsx(xlsxFile="./Data/nomis_2022_06_14_095314.xlsx", sheet=1, skipEmptyRows=T)

##ILR
### Core indicator 12: AY21/22 achievements by SSAt1 and LAD ------------
## Permalink: https://explore-education-statistics.service.gov.uk/data-tables/permalink/c390bb3f-8577-40f1-869c-fc8a8195516e
I_Achieve_ILR21 <- read.csv(file="./Data/permalink-c390bb3f-8577-40f1-869c-fc8a8195516e.csv")

### Core indicator 13: AY16/17-20/21 achievements by LAD and provision------------
## Permalink: https://explore-education-statistics.service.gov.uk/data-tables/permalink/3960ad0f-fd8a-49bb-91d7-f3ca1181b93f
I_Achieve_ILR1621 <- read.csv(file="./Data/permalink-3960ad0f-fd8a-49bb-91d7-f3ca1181b93f.csv")

##ONS
### Core indicator 22: Vacancy by year and LAD ------------
##Download link: https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/datasets/onlinejobadvertsbyitl1regionandlocalauthority
I_Vacancy_ONS1722 <- read.xlsx(xlsxFile="./Data/referencetablesupdated.xlsx", sheet="1", skipEmptyRows=T,startRow=4)

# Data cleaning functions ----
## Employment by occupation ----
format.EmpOcc.APS <- function(x) {
  x %>% mutate(year = ifelse(annual.population.survey == "date", X2, NA))%>% # tag time periods
    fill(year)%>% # fill time periods for all rows
    row_to_names(row_number=4) %>% # set col names
    clean_names()%>%
    select(-starts_with('na'))%>% # remove na columns (flags and confidence)
    mutate(check = ifelse(grepl(":", area), 1, 0))%>% # remove anything but LEP and Country
    filter(check ==1)%>%
    filter(!grepl("nomisweb", area))%>%
    select(year = jan_2017_dec_2017, area, everything(), - check) %>%# reorder and remove
    mutate(geographic_level = gsub(":.*", "", area)) %>% # Get geog type
    mutate(area = gsub(".*:", "", area)) %>% # Tidy up Area names
    mutate_at(c(3:27),as.numeric)%>% # Convert to numeric
    mutate(area=case_when(area=="Hull and East Riding" ~ "Hull and East Yorkshire",TRUE ~ area))%>%#Rename so matches official name
    relocate(geographic_level, .after = area)%>%
    mutate(year=as.numeric(substr(year, 5, 8)))%>%
    rename_with(.fn = ~ str_replace_all(.x, c("t09a_"="","all_people"="","soc2010"="","_"=" ")),
                .cols = starts_with("t09a_"))%>%
    rename_with(~gsub('[[:digit:]]+', "", .))#remove numbers from occupations since they don't match the ONS ones
}

C_EmpOcc_APS1721 <- format.EmpOcc.APS(I_EmpOcc_APS1721)

## Employment level and rate ----
format.EmpRate.APS <- function(x) { 
  x %>% mutate(year = ifelse(annual.population.survey == "date", substr(X2,nchar(X2)-4+1, nchar(X2)), NA))%>% # tag time periods
    fill(year)%>% # fill time periods for all rows
    row_to_names(row_number=4) %>% # set col names
    clean_names()%>%
    select(-starts_with('na'))%>% # remove na columns (flags and confidence)
    mutate(check = ifelse(grepl(":", area), 1, 0))%>% # remove anything but LEP and Country
    filter(check ==1)%>%
    filter(!grepl("nomisweb", area))%>%
    mutate(geographic_level = gsub(":.*", "", area)) %>% # Get geog type
    mutate(area = gsub(".*:", "", area))%>% # Tidy up Area names
    mutate_at(c(2:9),as.numeric)%>% # Convert to numeric
    mutate(area=case_when(area=="Hull and East Riding" ~ "Hull and East Yorkshire",TRUE ~ area))%>%#Rename so matches official name
    select(year=x2017, area, everything(), - check)%>%# reorder and remove
    mutate(empRate = .[[5]]/.[[3]])%>%
    relocate(geographic_level, .after = area)%>%
    rename_with(.fn = ~ str_replace_all(.x, c("t01_"="","all_people"="","aged_16_64"="","_"=" ")),
                .cols = starts_with("t01")) 
}

C_EmpRate_APS1721 <- format.EmpRate.APS(I_EmpRate_APS1721)

#Clean ILR column names, reorder and reformat
format.AchieveSSA.ILR <- function(x) { # need to clean up colnames
  colnames(x)[1] <- "area"
  x %>% 
    left_join(distinct(I_LEP2020,LAD21CD,LEP=LEP21NM1),by= c("location_code"="LAD21CD"))%>%
    relocate(LEP, .after = geographic_level)%>%
    relocate(time_period, .before = area)%>%
    rename_all(recode, e_and_t_aims_ach="achievements")%>%
    mutate(achievements=as.numeric(achievements))
}

##Achievements
C_Achieve_ILR1621 <- format.AchieveSSA.ILR(I_Achieve_ILR1621)
C_Achieve_ILR21 <- format.AchieveSSA.ILR(I_Achieve_ILR21)

#Reshape vacancy data to long, rename and reorder and reformat some columns
format.Vacancy.ONS <- function(x) { # need to clean up colnames
  x %>% gather(year, vacancy_unit, 3:8)%>%
    rename(LA="Local.authority.[note.1]",region="Region.[note.2]")%>%
    left_join(distinct(I_LEP2020,LAD21NM,LEP=LEP21NM1),by= c("LA"="LAD21NM"))%>%
    relocate(LEP, .after = region)%>%
    relocate(year,.before=LA)%>%
    mutate(year=as.numeric(year))
}

# Clean indicators datasets----






#vacancy
C_Vacancy_ONS1722 <- format.Vacancy.ONS(I_Vacancy_ONS1722)

C_Vacancy_England <- C_Vacancy_ONS1722 %>%
  filter(region !="Wales"|
           region != "Scotland"|
           region != "Northern Ireland")%>%
  group_by(year)%>%
  summarise(England = sum(vacancy_unit))%>%
  right_join(C_Vacancy_ONS1722, by="year")%>%
  mutate(pc_total = vacancy_unit/England)



# EmpRate_time <- reactive({
#   C_EmpRate_APS1721 %>%
#     select(year, area, empRate)%>%
#     filter(area == "England" |
#              area == input$lep1 |
#              area == input$lep2) %>%
#     ggplot(aes(x=year, y=empRate, group = area, colour = area))+
#     geom_line()+
#     theme_minimal()+
#     expand_limits(y = 0.6)+
#     labs(colour = "Area")+
#     theme(legend.position="bottom")+
#     ggtitle("Employment Rate \n 2017-2021")+
#     xlab("Year")+
#     ylab("Employment Rate")+
#     scale_y_continuous(labels = scales::percent_format(accuracy=1))
# })
# Save to SQL ----
#con <- dbConnect(odbc(), Driver = "SQL Server Native Client 11.0", 

#                 Server = "T1PRANMSQL\SQLPROD,60125", 

#                Database = "MA_UFS_S_DATA",

#                Trusted_Connection = "yes")



#and then I think you can use dbWriteTable to save the table 
#(haven't used it before so you may need to tweak the code below). I'm calling X your R table.


#con %>% dbWriteTable("MA_UFS_S_DATA.LSIP.dashboard_data", X)

# Combine into single workbook ----

# list_of_datasets <- list("2.Emp by occupation" = C_EmpOcc_APS1721,
#                         "5.Emp rate" = C_EmpRate_APS1721,
#                         "12.FE achievements SSA"=C_Achieve_ILR21,
#                         "x.FE achievements"=C_Achieve_ILR1621,
#                         "22.Vacancies"=C_Vacancy_ONS1722)
# write.xlsx(list_of_datasets, file = "202206CoreIndicators.xlsx")
