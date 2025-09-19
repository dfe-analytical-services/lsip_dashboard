### 1 ONS job adverts by 3 digit SOC and LA----
folder <- "2-12_OnsProf"
 sheet <- "Table 4"
 I_Ons3digLA <- openxlsx::read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)

 #Tidy up data
 tidydata<-I_Ons3digLA %>% 
   janitor::row_to_names(row_number = 4)%>% # set columns
   filter(!(Region %in% c("Scotland", "Wales", "Northern Ireland", "Unknown")))%>%
   rename(area=`Local Authority District`,areaCode=`Local Authority Code`)%>%
   mutate(geographic_level="Local authority district")%>%
   mutate(areaCode=case_when(area=="London" ~ "E09000001", TRUE ~ areaCode))%>%#Online job ad data is not at LA level for London. To make the lookups for CA work, just add in City of London LA code
 select(-Region,-ITL2,-`SOC 3 digit label`)
 
 #turn into 2 digit SOC
 SOC2digit<-tidydata%>%
   mutate(SOC2digitCode= as.numeric(substr(case_when(`SOC 3 digit code`=="Unknown" ~ "999", TRUE ~ `SOC 3 digit code`),1,2)))%>% #get 2 digit code
   left_join(C_SOC2020structure%>%mutate(SOC2digit=paste0(code," - ",stringr::str_to_sentence(cleanName))), by = c("SOC2digitCode" = "code")) %>% #add on SOC 2 digit names
   mutate(SOC2digit=case_when(SOC2digitCode==99 ~ "Unknown", TRUE ~ SOC2digit))%>%
   select(-cleanName,-`SOC 3 digit code`, -SOC2digitCode)
 
 #unique(SOC2digit$SOC2digit) #Check all are populated
 
 #make long and group up to 2 digit
 SOC2digitLong<-SOC2digit %>%
   tidyr::pivot_longer(!c("geographic_level", "area","areaCode","SOC2digit"),
                       names_to = "time_period", values_to = "value"
   )%>%
   mutate(timePeriod = as.Date(paste0("01 ", gsub("-", " ", time_period)), "%d %b %y")) %>%
   group_by(geographic_level,area,areaCode,SOC2digit,time_period)%>%
   summarise(value=sum(as.numeric(value)))%>%
   mutate(value = as.character(value))%>% # so we can merge
   ungroup()
 
 ### 2 ONS job adverts by LA----
 sheet <- "Table 2"
 I_OnsLA <- openxlsx::read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
 
 #Tidy up data
 tidyDataLA<-I_OnsLA %>% 
   janitor::row_to_names(row_number = 4)%>% # set columns
   filter(!(Region %in% c("Scotland", "Wales", "Northern Ireland", "Unknown")))%>%
   rename(area=`Local Authority District`,areaCode=`Local Authority Code`)%>%
   mutate(geographic_level="Local authority district")%>%
   mutate(areaCode=case_when(area=="London" ~ "E09000001", TRUE ~ areaCode))%>%#Online job ad data is not at LA level for London. To make the lookups for CA work, just add in City of London LA code
   select(-Region,-ITL2)
 
 #make long
 LaLong<-tidyDataLA %>%
   tidyr::pivot_longer(!c("geographic_level", "area","areaCode"),
                       names_to = "time_period", values_to = "value"
   )
 
 ### 3 ONS job adverts England----
 sheet <- "Table 1"
 I_OnsEng <- openxlsx::read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheet, skipEmptyRows = T)
 
 #Tidy up data
 tidyDataEng<-I_OnsEng %>% 
   janitor::row_to_names(row_number = 4)%>% # set columns
   filter(Country == "England")%>%
   rename(area=Country)%>%
   mutate(geographic_level="National")
 
 #make long
 EngLong<-tidyDataEng %>%
   tidyr::pivot_longer(!c("geographic_level", "area"),
                       names_to = "time_period", values_to = "value"
   )
 
 ### 4 Get all the other geographies by joining to the lookups----
 #add on CA, LSIP
 geogs<-bind_rows(
   SOC2digitLong,
   LaLong,
   EngLong)%>%
   addGeogs()%>% #repeat data for each geography
   filter(geogConcat!="City of London LADU", #remove the City of London data which was used just to get the london LSIP.
   geogConcat!="Central London Forward LSIP") # Also remove Central London Forward LSIP which has been joined via the City of London LA but there is no data for
 #group up CA, LSIP
 geogsSummed <- geogs %>%
   filter(newArea == 1) %>% # no need to group LAs that haven't changed
   ungroup() %>%
   select(-newArea) %>%
   group_by(geogConcat, SOC2digit, time_period) %>% # sum for each area
   summarise(value=sum(as.numeric(value))) %>%
   mutate(value = as.character(value)) # so we can merge
 
 # get england soc stats summed from LSIPs
 SOC2digitEngland<-geogsSummed %>%
   filter(stringr::str_sub(geogConcat, -4, -1)=="LSIP",is.na(SOC2digit)==FALSE )%>%
   group_by(SOC2digit, time_period) %>%
   summarise(value = as.character(sum(as.numeric(value), na.rm = T))) %>%
   mutate(geogConcat = "England")

# Format all other area types
F_adverts <- bind_rows(
  SOC2digitEngland,#SOC 2 digit for england
  geogsSummed, #summed up geographies
  geogs %>% filter(newArea == 0)#LAs
)%>%
  mutate(timePeriod = as.Date(paste0("01 ", gsub("-", " ", time_period)), "%d %b %y")) %>%
  rename(chartPeriod = time_period) %>%
  mutate(latest = case_when(
    timePeriod == max(timePeriod) ~ 1,
    timePeriod == (max(timePeriod) - lubridate::years(1)) ~ -1,
    TRUE ~ 0
  )) %>%
  #filter to last 5 years
  filter(timePeriod>=(max(timePeriod) - lubridate::years(4)))%>%
  mutate(valueText = value)%>%
  mutate(value = as.numeric(valueText))%>%
  select(-newArea)

# Get summaries
C_adverts <- bind_rows(
  # soc 2 digit
  F_adverts %>%
    filter(is.na(SOC2digit) == FALSE) %>%
    mutate(breakdown = "Occupation (SOC2020 Sub-Major Group)") %>%
    rename(subgroup = SOC2digit),
  # total
  F_adverts %>%
    filter(is.na(SOC2digit) == TRUE) %>%
    mutate(breakdown = "Total", subgroup = "Total"),
  # soc 1 digit
  F_adverts %>%
    filter(is.na(SOC2digit) == FALSE) %>%
    mutate(
      SOC1digitCode = substr(SOC2digit, 1, 1),
      SOC1digit = case_when(
        SOC1digitCode == "1" ~ "1 - Managers, directors and senior officials",
        SOC1digitCode == "2" ~ "2 - Professional occupations",
        SOC1digitCode == "3" ~ "3 - Associate professional occupations",
        SOC1digitCode == "4" ~ "4 - Administrative and secretarial occupations",
        SOC1digitCode == "5" ~ "5 - Skilled trades occupations",
        SOC1digitCode == "6" ~ "6 - Caring, leisure and other service occupations",
        SOC1digitCode == "7" ~ "7 - Sales and customer service occupations",
        SOC1digitCode == "8" ~ "8 - Process, plant and machine operatives",
        SOC1digitCode == "9" ~ "9 - Elementary occupations",
        SOC1digitCode == "U" ~ "Unknown",
        TRUE ~ "NULL"
      )
    ) %>%
    group_by(chartPeriod, timePeriod, geogConcat, latest, SOC1digit) %>%
    summarise(value = sum(value)) %>%
    mutate(breakdown = "Occupation (SOC2020 Major Group)", valueText = as.character(value)) %>%
    mutate(subgroup = SOC1digit)
) %>%
  select(-SOC2digit, -SOC1digit) %>%
  mutate(metric = "vacancies")

