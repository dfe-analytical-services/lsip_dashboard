### 2.1.2 Employment level and rate ------------
# Geog and date as above
# Cell: T01 Economic activity by age Aged 16-64/ All people
# find cells we want
cellsUseAps_emp <- cellsListAps %>% filter(description.en %like% "T01:" & description.en %like% "Aged 16-64" & description.en %like% "All People")
# get data
F_emp <- extractNomis("NM_17_1", "latestMINUS16,latestMINUS12,latestMINUS8,latestMINUS4,latest", cellsUseAps_emp$id)%>%

# convert into format used in dashboard
formatNomis() %>%
  rename(metric = CELL_NAME) %>%
  mutate(metric = gsub(" : All People )", "", metric)) %>%
  mutate(metric = gsub("[[:digit:]]+", "", metric)) %>%
  mutate(metric = gsub("T: \\(Aged - - ", "", metric)) %>%
  mutate(metric = gsub(" ", "", tolower(metric))) %>%
  mutate(breakdown = "Total", subgroup = "Total")

# Create rates
C_emp <- F_emp %>%
  filter(metric != "unemployed") %>%
  left_join(F_emp %>% filter(metric == "all") %>% rename(all = value) %>% select(-metric)) %>%
  mutate(value = value / all, metric = paste0(metric, "Rate")) %>%
  filter(metric != "allRate") %>%
  select(-all)

# Cell: T01 Economic activity by age Aged 16+/ All people. We need this as the denominator of the bar charts where the splits are only available in 16+
# find cells we want
cellsUseAps_emp <- cellsListAps %>% filter(description.en %like% "T01:" & description.en %like% "All aged 16 & over" & description.en %like% "All People")
# get data
I_emp16plus <- extractNomis("NM_17_1", "latestMINUS16,latestMINUS12,latestMINUS8,latestMINUS4,latest", cellsUseAps_emp$id)

# we need the totals for 16plus to use as the denomintor of the bar charts
F_emp16plus <- formatNomis(I_emp16plus) %>%
  rename(metric = CELL_NAME) %>%
  mutate(metric = gsub(" : All People )", "", metric)) %>%
  mutate(metric = gsub("[[:digit:]]+", "", metric)) %>%
  mutate(metric = gsub("T: \\(All aged  & over - ", "", metric)) %>%
  mutate(metric = gsub(" ", "", tolower(metric))) %>%
  mutate(breakdown = "Total", subgroup = "Total")

# Create rates for unemployment (base is 16+ economically active)
C_unemp <- F_emp16plus %>%
  filter(metric == "unemployed") %>%
  left_join(F_emp16plus %>% filter(metric == "economicallyactive") %>% rename(economicallyactive = value) %>% select(-metric)) %>%
  mutate(value = value / economicallyactive, metric = paste0(metric, "Rate")) %>%
  select(-economicallyactive)

# Bind rates and volumes (volumes are 16+ except inactivity which is 16 to 64)
C_emp <- bind_rows(
  C_emp,
  C_unemp,
  F_emp16plus %>% filter(metric != "inactive"),
  F_emp %>% filter(metric == "inactive")
) %>%
  mutate(valueText = as.character(value))