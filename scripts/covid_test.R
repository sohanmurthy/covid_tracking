library(tidyverse)
library(lubridate)
source("utils/analytics.R")

download.file("https://covidtracking.com/api/v1/states/daily.csv", "source/state_daily.csv")
state_daily.df <- read_csv("source/state_daily.csv")

state_daily.df %>%
  mutate(pct_positive = positive / totalTestResults,
         date = ymd(date)) %>%
  filter(state == "AZ",
         date >= today() - 60) %>%
  ggplot(aes(x = date, y = pct_positive)) +
  geom_line()
