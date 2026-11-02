# Get the NEET/not known perecntage for each area where possible. 
# This value is for 16-17 year olds only so it will be caveated a lot in the dashboard
# Source: https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/a7bc111a-9503-40df-99fa-764fcb13a2e9

I_neet<-read.csv("https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/a7bc111a-9503-40df-99fa-764fcb13a2e9/csv")

# This data is at Unitary Authority level. The dashboard works at LAD level. 
# We will keep all the Unitary Authorties that match a LAD and map and chart those as usual
# For the UAs that cover multiple LADs we blank out the LADs so they are not mapped

UA_to_LSIP_CA<-LAD25_to_CTYUA25CD |> 
  #add LSIPs
  left_join(C_LADLSIP,by = join_by(LAD25CD == LAD25CD)) %>%
  # add CAs
  left_join(C_calookup |> select(-LAD25NM),by = join_by(LAD25CD == LAD25CD)) |> 
  #Remove multiple LADs matched to UAs
  group_by(CTYUA25NM) |> 
  mutate(n_cty = n()) |> 
  slice(1) |> 
  mutate(
    LAD25CD = if_else(n_cty > 1, NA_character_, LAD25CD),
    LAD25NM = if_else(n_cty > 1, NA_character_, LAD25NM)
  ) |> 
  ungroup() |> 
  select(LAD25CD,CTYUA25CD,LAD25NM,LSIP25NM,CAUTH25NM)

# Add on LAD, LSIP, CA
area_neet <- I_neet |> 
  # filter needed columns and rows
  filter(geographic_level != "Regional", age=="16 to 17") |> 
  #Add on Local authority district
  left_join(UA_to_LSIP_CA, by = join_by(new_la_code == CTYUA25CD)) |> 
  mutate(avg_cohort_count=as.numeric(avg_cohort_count),
         avg_neet_nk_count=as.numeric(avg_neet_nk_count))

#National
England_neet<-area_neet |> 
  filter(geographic_level=="National") |> 
  mutate(geogConcat="England") |> 
  select(geogConcat,time_period,characteristic_grouping,characteristic,avg_cohort_count,avg_neet_nk_count)

#Get LADs (ie areas where LAD matches UA)
LA_neet<-area_neet |> 
  filter(is.na(LAD25NM)==FALSE) |> 
  mutate(geogConcat=paste0(LAD25NM," LADU")) |> 
  select(geogConcat,time_period,characteristic_grouping,characteristic,avg_cohort_count,avg_neet_nk_count)

#Sum for LSIPs
LSIP_neet<-area_neet |> 
  filter(is.na(LSIP25NM)==FALSE) |> 
  mutate(geogConcat=paste0(LSIP25NM," LSIP")) |> 
  group_by(geogConcat,time_period, characteristic_grouping, characteristic) |> 
  summarise(avg_cohort_count=sum(avg_cohort_count, na.rm = TRUE),
            avg_neet_nk_count=sum(avg_neet_nk_count, na.rm = TRUE))

#Sum for LSIPs
CA_neet<-area_neet |> 
  filter(is.na(CAUTH25NM)==FALSE) |> 
  mutate(geogConcat=paste0(CAUTH25NM," CA")) |> 
  group_by(geogConcat,time_period,characteristic_grouping, characteristic) |> 
  summarise(avg_cohort_count=sum(avg_cohort_count, na.rm = TRUE),
            avg_neet_nk_count=sum(avg_neet_nk_count, na.rm = TRUE))

#Combine and tidy
C_neet<-bind_rows(England_neet,LA_neet,LSIP_neet,CA_neet) |> 
  # add dates
  mutate(chartPeriod = paste("CY", time_period)) |> 
  mutate(timePeriod = as.Date(paste("01 Jan", time_period, sep = ""), format = "%d %b %Y")) %>%
  mutate(latest = case_when(
    timePeriod == max(timePeriod) ~ 1,
    timePeriod == (max(timePeriod) - lubridate::years(1)) ~ -1,
    TRUE ~ 0
  )) |> 
  #filter to last 5 years
  filter(timePeriod>=(max(timePeriod) - lubridate::years(4))) |> 
  #calculate NEET %
  mutate(value = avg_neet_nk_count/avg_cohort_count) |> 
  # Remove invalid value text for NA values, e.g. Inf, NaN, z
  mutate(valueText = ifelse(is.na(value), "NA", paste0(round2(value*100,2),"%"))) |> 
  mutate(metric="neet_percentage") |> 
  mutate(breakdown= case_when(characteristic_grouping!="SEND" ~ str_to_sentence(characteristic_grouping),
                              TRUE ~ characteristic_grouping),
         subgroup= case_when(characteristic_grouping!="SEND" ~ str_to_sentence(characteristic),
                   TRUE ~ characteristic)) |> 
  #remove unused cols
  select(geogConcat,chartPeriod,timePeriod,latest,breakdown,subgroup,value,valueText,metric) 

#save output
saveRDS(C_neet, "Data/processing/C_neet.rds")