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

