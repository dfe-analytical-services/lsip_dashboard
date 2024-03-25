###
# Title: LSIP dashboard data update QA
# Author: Paul James
# Date: 28th Feb 2024
# Last updated: 28th Feb 2024
# Use: Checks whether there are any missing data following a data update
# The dataframes are then saved as .csv in ./Data/AppData to be read by the dashboard app.
# Sources: The files within /Data/AppData (created by running TransformData.R)
###

# Load libraries ----
library(dplyr)
library(waldo)

# Order both dataframes for comparison
C_timeOld <- C_timeOld %>% arrange(geogConcat, metric, timePeriod, chartPeriod, latest)
C_time <- C_time %>% arrange(geogConcat, metric, timePeriod, chartPeriod, latest)

# 1 Find changes ----
# Compare C_time before any changes (C_timeOld) with C_time after changes
# Make sure you understand any of the changes highlighted by the code below before pushing to main

## 1.1 Compare geographies
# Will show if there are any changes to geogconcat names
compare(C_timeOld %>% distinct(geogConcat), C_time %>% distinct(geogConcat))

## 1.2 Compare metrics
# Will show if there are any changes to metric names
compare(C_timeOld %>% distinct(metric), C_time %>% distinct(metric))

## 1.3 Compare timePeriod
# Will show if there are any changes to timePeriods. You may well expect an addition of the latest time period you have added
compare(C_timeOld %>% distinct(timePeriod, chartPeriod, latest), C_time %>% distinct(timePeriod, chartPeriod, latest))

## 1.4 Compare values
# This is the trickier part. Obviously lots of values will have changed.
# You will need to make sure that all the previous checks have passed otherwise you may be comparing rows which are now ordered in a different way.
# Here we compare any NA rows. It may be the case that these are genuine changes (eg a cohort is now (or previously) supressed) but you are really looking for big batches of NAs where something has not linked up correctly.
compare(C_timeOld %>% filter(is.na(value)), C_time %>% filter(is.na(value)))

# Another view is an outer join to show all the rows that do not match
anti_join(C_timeOld %>% filter(is.na(value)), C_time %>% filter(is.na(value)))
anti_join(C_time %>% filter(is.na(value)), C_timeOld %>% filter(is.na(value)))
