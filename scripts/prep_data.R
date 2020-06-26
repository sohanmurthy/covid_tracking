library(tidyverse)
library(lubridate)
source("utils/analytics.R")

download.file("https://covidtracking.com/api/v1/states/daily.csv", "source/state_daily.csv")
download.file("https://covidtracking.com/api/v1/us/daily.csv", "source/us_daily.csv")

#' State data
#' adds 7 day rolling averages and pct positive
state_daily.df <-
read_csv("source/state_daily.csv") %>%
  mutate(date = ymd(date),
         pct_pos_cume = positive / totalTestResults,
         pct_pos_daily = positiveIncrease / totalTestResultsIncrease) %>%
  arrange(state, date) %>%
  group_by(state) %>%
  mutate(pos7day = roll_mean_na(positiveIncrease),
         total7day = roll_mean_na(totalTestResultsIncrease),
         pct_pos_7day = pos7day / total7day,
         death7day = roll_mean_na(deathIncrease),
         hospitalized7day = roll_mean_na(hospitalizedCurrently),
         inIcu7day = roll_mean_na(inIcuCurrently)) %>%
  ungroup()


#' region data
#' groups states into BLS regions and adds 7 day rolling averages and pct positive
region_daily.df <-
  read_csv("source/state_daily.csv") %>%
  mutate(
    date = ymd(date),
    region = case_when (
      state %in% c("CT", "ME", "MA", "NH", "RI", "VT", "NJ", "NY", "PA") ~ "Northeast",
      state %in% c("DE", "DC", "FL", "GA", "MD", "NC", "SC", "VA", "WV", "AL", "KY", "MS", "TN", "AR", "LA", "OK", "TX") ~ "South",
      state %in% c("IL", "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD") ~"Midwest",
      state %in% c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY", "AK", "CA", "HI", "OR", "WA") ~"West"
      )
    ) %>%
  filter(!is.na(region)) %>%
  select(date,
         region,
         positive,
         totalTestResults,
         positiveIncrease,
         totalTestResultsIncrease,
         deathIncrease,
         hospitalizedCurrently,
         inIcuCurrently
         ) %>%
    group_by(date, region) %>%
    summarise(positive = sum(positive, na.rm = TRUE),
              totalTestResults = sum(totalTestResults, na.rm = TRUE),
              positiveIncrease = sum(positiveIncrease, na.rm = TRUE),
              totalTestResultsIncrease = sum(totalTestResultsIncrease, na.rm = TRUE),
              deathIncrease = sum(deathIncrease, na.rm = TRUE),
              hospitalizedCurrently = sum(hospitalizedCurrently, na.rm = TRUE),
              inIcuCurrently = sum(inIcuCurrently, na.rm = TRUE)
              ) %>%
    ungroup() %>%
    mutate(pct_pos_cume = positive / totalTestResults,
           pct_pos_daily = positiveIncrease / totalTestResultsIncrease
           ) %>%
    group_by(region) %>%
    mutate(pos7day = roll_mean_na(positiveIncrease),
           total7day = roll_mean_na(totalTestResultsIncrease),
           pct_pos_7day = pos7day / total7day,
           death7day = roll_mean_na(deathIncrease),
           hospitalized7day = roll_mean_na(hospitalizedCurrently),
           inIcu7day = roll_mean_na(inIcuCurrently)) %>%
    ungroup()


#' US data
#' adds 7 day rolling averages and pct positive
us_daily.df <-
  read.csv("source/us_daily.csv") %>%
  mutate(date = ymd(date),
         pct_pos_cume = positive / totalTestResults,
         pct_pos_daily = positiveIncrease / totalTestResultsIncrease,
         pos7day = roll_mean_na(positiveIncrease),
         total7day = roll_mean_na(totalTestResultsIncrease),
         pct_pos_7day = pos7day / total7day,
         death7day = roll_mean_na(deathIncrease),
         hospitalized7day = roll_mean_na(hospitalizedCurrently),
         inIcu7day = roll_mean_na(inIcuCurrently))

