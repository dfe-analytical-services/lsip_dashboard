# Code to create charts using the Job_Ads_Page.R script

# Volume of new job adverts nationally ===============================
ggplot(new_ads_national_roll, aes(x = timePeriod, y = n_jobs_3m_avg)) +
  geom_line() +
  scale_x_date(breaks = "1 year",
               date_labels = "%b %y") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                     labels = scales::comma,
                     limits = c(0,1500000)) +
  labs(y = "Volume",
       title = "Number of new job adverts nationally")

# Growth rate of new job adverts nationally ==========================
ggplot(new_ads_national_growth, aes(x = timePeriod, y = growth_perc_from_base)) +
  geom_line() +
  scale_x_date(breaks = "1 year",
               date_labels = "%b %y") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                     labels = scales::comma,
                     limits = c(-20,80)) +
  labs(y = "Growth Rate (%)",
       title = "Growth rate of new job adverts nationally")

# Pop rate of new job adverts nationally =============================
ggplot(new_ads_national_pop, aes(x = timePeriod, y = pop_rate)) +
  geom_line() +
  scale_x_date(breaks = "1 year",
               date_labels = "%b %y") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                     labels = scales::comma,
                     limits = c(0,50)) +
  labs(y = "Population Rate (%)",
       title = "Population rate of new job adverts nationally")

# Job rate of new job adverts nationally =============================
ggplot(new_ads_national_job, aes(x = timePeriod, y = job_rate)) +
  geom_line() +
  scale_x_date(breaks = "1 year",
               date_labels = "%b %y") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                     labels = scales::comma,
                     limits = c(0,50)) +
  labs(y = "Job Rate (%)",
       title = "Job rate of new job adverts nationally")

# Map of new job adverts ==========================================

map <- new_ads_region_vol_map %>%
  mutate(geometry = st_simplify(geometry, dTolerance = 1000)) %>%
  ggplot() +
  geom_sf(aes(fill = n_jobs, geometry = geometry), colour = "white", size = 0.2) +
  scale_fill_gradient(high = "#183860", low = "white",
                      labels=function(x) format(x, big.mark = ",", scientific = FALSE),
                      name = "Volume of job adverts") +
  theme_void() +
  labs(title = "Number of new job adverts across regions")

plot(map)

# Volume of new job adverts by SOC ===================================

occupation_1 <- 1111
occupation_2 <- 4143
occupation_3 <- 5222

filtered_df <- new_ads_SOC_roll %>%
  filter(soc_4_digit_code %in% c(occupation_1, occupation_2, occupation_3))

ggplot(filtered_df, aes(x = timePeriod, y = n_jobs_3m_avg, colour = soc_4_digit_label, group = soc_4_digit_label)) +
  geom_line() +
  scale_x_date(breaks = "1 year",
               date_labels = "%b %y") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                     labels = scales::comma,
                     limits = c(0,5000)) +
  labs(y = "Volume",
       title = "Volume of new job adverts by occupation")

# Growth rate of new job adverts by SOC ==============================

filtered_df <- new_ads_SOC_growth %>%
  filter(soc_4_digit_code %in% c(occupation_1, occupation_2, occupation_3))

ggplot(filtered_df, aes(x = timePeriod, y = growth_perc_from_base, colour = soc_4_digit_label, group = soc_4_digit_label)) +
  geom_line() +
  scale_x_date(breaks = "1 year",
               date_labels = "%b %y") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                     labels = scales::comma,
                     limits = c(-70,150)) +
  labs(y = "Growth Rate (%)",
       title = "Growth rate of new job adverts by occupation")

# Pop rate of new job adverts by SOC =================================

filtered_df <- new_ads_SOC_pop %>%
  filter(soc_4_digit_code %in% c(occupation_1, occupation_2, occupation_3))

ggplot(filtered_df, aes(x = timePeriod, y = pop_rate, colour = soc_4_digit_label, group = soc_4_digit_label)) +
  geom_line() +
  scale_x_date(breaks = "1 year",
               date_labels = "%b %y") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                     labels = scales::comma,
                     limits = c(0,120)) +
  labs(y = "Population Rate (%)",
       title = "Population rate of new job adverts by occupation")

# Job rate of new job adverts by SOC =================================

filtered_df <- new_ads_SOC_job %>%
  filter(soc_4_digit_code %in% c(occupation_1, occupation_2, occupation_3))

ggplot(filtered_df, aes(x = timePeriod, y = job_rate, colour = soc_4_digit_label, group = soc_4_digit_label)) +
  geom_line() +
  scale_x_date(breaks = "1 year",
               date_labels = "%b %y") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                     labels = scales::comma,
                     limits = c(0,100)) +
  labs(y = "Job Rate (%)",
       title = "Job rate of new job adverts by occupation")

# Map of new job adverts by SOC ===================================

map <- new_ads_region_SOC_map %>%
  filter(soc_4_digit_code == occupation_2) %>%
  mutate(geometry = st_simplify(geometry, dTolerance = 1000)) %>%
  ggplot() +
  geom_sf(aes(fill = pop_rate, geometry = geometry), colour = "white", size = 0.2) +
  scale_fill_gradient(high = "#183860", low = "white",
                      labels=function(x) format(x, big.mark = ",", scientific = FALSE),
                      name = "Volume of job adverts per population rate") +
  theme_void() +
  labs(title = "Number of job ads for 'Customer service managers' per 100,000 population")

plot(map)





