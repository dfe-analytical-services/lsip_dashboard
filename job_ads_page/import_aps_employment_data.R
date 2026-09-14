# Script to read in the APS employment data by SOC 2020 as NOMIS limits public API downloads to 25,000 rows.

APS_employment_soc_1 <- read.csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_218_1.data.csv?geography=2092957699,2013265926,2013265924,2013265927,2013265921,2013265922,2013265928,2013265929,2013265925,2013265923&jtype=0&ftpt=0&etype=0&c_sex=0&soc2020_full=0...412&measure=1&measures=20100,20701&select=date_name,geography_name,geography_code,jtype_name,ftpt_name,etype_name,c_sex_name,soc2020_full_name,measure_name,measures_name,obs_value,obs_status_name")


APS_employment_soc_2 <- read.csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_218_1.data.csv?geography=2092957699,2013265926,2013265924,2013265927,2013265921,2013265922,2013265928,2013265929,2013265925,2013265923&jtype=0&ftpt=0&etype=0&c_sex=0&soc2020_full=0...412&measure=1&measures=20100,20701&select=date_name,geography_name,geography_code,jtype_name,ftpt_name,etype_name,c_sex_name,soc2020_full_name,measure_name,measures_name,obs_value,obs_status_name&RecordOffset=25000")

APS_employment_soc_3 <- read.csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_218_1.data.csv?geography=2092957699,2013265926,2013265924,2013265927,2013265921,2013265922,2013265928,2013265929,2013265925,2013265923&jtype=0&ftpt=0&etype=0&c_sex=0&soc2020_full=0...412&measure=1&measures=20100,20701&select=date_name,geography_name,geography_code,jtype_name,ftpt_name,etype_name,c_sex_name,soc2020_full_name,measure_name,measures_name,obs_value,obs_status_name&RecordOffset=50000")

APS_employment_soc_4 <- read.csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_218_1.data.csv?geography=2092957699,2013265926,2013265924,2013265927,2013265921,2013265922,2013265928,2013265929,2013265925,2013265923&jtype=0&ftpt=0&etype=0&c_sex=0&soc2020_full=0...412&measure=1&measures=20100,20701&select=date_name,geography_name,geography_code,jtype_name,ftpt_name,etype_name,c_sex_name,soc2020_full_name,measure_name,measures_name,obs_value,obs_status_name&RecordOffset=75000")

APS_employment_soc_5 <- read.csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_218_1.data.csv?geography=2092957699,2013265926,2013265924,2013265927,2013265921,2013265922,2013265928,2013265929,2013265925,2013265923&jtype=0&ftpt=0&etype=0&c_sex=0&soc2020_full=0...412&measure=1&measures=20100,20701&select=date_name,geography_name,geography_code,jtype_name,ftpt_name,etype_name,c_sex_name,soc2020_full_name,measure_name,measures_name,obs_value,obs_status_name&RecordOffset=100000")

APS_employment_soc_6 <- read.csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_218_1.data.csv?geography=2092957699,2013265926,2013265924,2013265927,2013265921,2013265922,2013265928,2013265929,2013265925,2013265923&jtype=0&ftpt=0&etype=0&c_sex=0&soc2020_full=0...412&measure=1&measures=20100,20701&select=date_name,geography_name,geography_code,jtype_name,ftpt_name,etype_name,c_sex_name,soc2020_full_name,measure_name,measures_name,obs_value,obs_status_name&RecordOffset=125000")

APS_employment_soc <- bind_rows(APS_employment_soc_1, APS_employment_soc_2, APS_employment_soc_3, APS_employment_soc_4,
                            APS_employment_soc_5, APS_employment_soc_6)

rm(APS_employment_soc_1, APS_employment_soc_2, APS_employment_soc_3, APS_employment_soc_4,
   APS_employment_soc_5, APS_employment_soc_6)


