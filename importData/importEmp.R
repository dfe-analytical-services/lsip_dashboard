### 2.1.2 Employment level and rate ------------
# Geog and date as above
# Cell: T01 Economic activity by age Aged 16-64/ All people
# find cells we want
cellsUseAps_emp <- cellsListAps %>% filter(CELL_NAME %like% "T01:" & CELL_NAME %like% "Aged 16-64" & CELL_NAME %like% "All People")
# get data
F_emp <- fetch_nomis(
  "NM_17_1",
  c("latestMINUS16","latestMINUS12","latestMINUS8","latestMINUS4","latest"),
  geog_all,
  cellsUseAps_emp$CELL
)%>%
  
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
cellsUseAps_emp <- cellsListAps %>% filter(CELL_NAME %like% "T01:" & CELL_NAME %like% "All aged 16 & over" & CELL_NAME %like% "All People")
# get data
I_emp16plus <- fetch_nomis(
  "NM_17_1",
  c("latestMINUS16","latestMINUS12","latestMINUS8","latestMINUS4","latest"),
  geog_all,
  cellsUseAps_emp$CELL
)

# we need the totals for 16plus to use as the denomintor of the bar charts
C_emp16plus <- formatNomis(I_emp16plus) %>%
  rename(metric = CELL_NAME) %>%
  mutate(metric = gsub(" : All People )", "", metric)) %>%
  mutate(metric = gsub("[[:digit:]]+", "", metric)) %>%
  mutate(metric = gsub("T: \\(All aged  & over - ", "", metric)) %>%
  mutate(metric = gsub(" ", "", tolower(metric))) %>%
  mutate(breakdown = "Total", subgroup = "Total")

# Create rates for unemployment (base is 16+ economically active)
C_unemp <- C_emp16plus %>%
  filter(metric == "unemployed") %>%
  left_join(C_emp16plus %>% filter(metric == "economicallyactive") %>% rename(economicallyactive = value) %>% select(-metric)) %>%
  mutate(value = value / economicallyactive, metric = paste0(metric, "Rate")) %>%
  select(-economicallyactive)

# Bind rates and volumes (volumes are 16+ except inactivity which is 16 to 64)
C_emp <- bind_rows(
  C_emp,
  C_unemp,
  C_emp16plus %>% filter(metric != "inactive"),
  F_emp %>% filter(metric == "inactive")
) %>%
  mutate(valueText = as.character(value))

#save data
saveRDS(C_emp16plus, "Data/processing/C_emp16plus.rds")
saveRDS(C_emp, "Data/processing/C_emp.rds")