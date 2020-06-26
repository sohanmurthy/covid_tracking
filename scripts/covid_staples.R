#' The main charts, cleaned up and ready for publishing

source("scripts/prep_data.R")
library(scales)
library(reshape2)
library(gganimate)


#hospitalized patients by State
state_daily.df %>%
  filter(date >= today() - 60,
         #remove states with incomplete data
         !state %in% c( "AL", "AS", "FL", "GU", "HI", "ID", "KS", "MP", "TN", "VI")
  ) %>%
  select(date, state, hospitalized7day, hospitalizedCurrently) %>%
  melt(id.vars = c("date", "state")) %>%
  mutate(variable = str_replace_all(variable, c("hospitalized7day" = "Hospitalizations (7 day moving avg.)",
                                                "hospitalizedCurrently" = "Hospitalizations")
  )) %>%
  ggplot(aes(x = date, y = value, color = variable)) +
  geom_line() +
  geom_hline(yintercept = 0, size = 0.25, color = color.dark_gray) +
  scale_y_continuous(labels = comma, limits = c(0, NA)) +
  scale_color_manual(values = c(color.carto_bold[12], color.carto_bold[1]), name = "") +
  facet_wrap(~state, ncol = 4, scales = "free_y") +
  labs(title = "Daily COVID-19 Hospitalizations", subtitle = "Select states, last 60 Days. Source: covidtracking.com") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "top", 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(color = guide_legend(override.aes = list(size=1.5)))

ggsave("output/state_hospitalized_grid.png", dpi = 150, width = 7, height = 14)


#hospitalized patients by Region
region_daily.df %>%
  filter(date >= today() - 60) %>%
  select(date, region, hospitalized7day, hospitalizedCurrently) %>%
  melt(id.vars = c("date", "region")) %>%
  mutate(variable = str_replace_all(variable, c("hospitalized7day" = "Hospitalizations (7 day moving avg.)",
                                                "hospitalizedCurrently" = "Hospitalizations")
  )) %>%
  ggplot(aes(x = date, y = value, color = variable)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, size = 0.25, color = color.dark_gray) +
  scale_y_continuous(labels = comma, limits = c(0, NA)) +
  scale_color_manual(values = c(color.carto_bold[12], color.carto_bold[1]), name = "") +
  facet_wrap(~region, ncol = 2, scales = "free_y") +
  labs(title = "Daily COVID-19 Hospitalizations", subtitle = "Last 60 Days. Source: covidtracking.com") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "top", 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(color = guide_legend(override.aes = list(size=1.5)))

ggsave("output/region_hospitalized_grid.png", dpi = 150, width = 7, height = 6)


#walk graph US hospitalizations
us_daily.plot <-
  us_daily.df %>%
  mutate(date_abbr = paste0(month(date, label = TRUE),'. ', day(date),', ', year(date))) %>%
  filter(date >= "2020-03-15") %>%
  ggplot(aes(x = hospitalized7day, y = pct_pos_7day, size = total7day)) +
  geom_point(shape = 21, fill = color.carto_bold[2], stroke = 0, alpha = 0.3) +
  geom_path(size = 0.5, color = "black") +
  geom_hline(yintercept = 0, size = 0.25, color = color.dark_gray) +
  scale_y_continuous(label = percent, limits = c(0,NA)) +
  scale_x_continuous(label = comma, limits = c(0,NA)) +
  scale_size_continuous(range = c(1, 8), name = "Daily Tests", label = label_number_si()) +
  theme(legend.position = "right")


us_daily.plot +
  #geom_text(aes(label = date_abbr), size = 2, check_overlap = TRUE) +
  labs(title = "Daily Hospitalizations vs. Case Positivity",
       subtitle = "Seven-day rolling averages, since March 15, 2020\nSource: covidtesting.com",
       x = "Hospitalizations",
       y = "Case Positivity")

#walk graph US hospitalizations animation
us_daily.anim <-
  us_daily.plot +
  labs(title = "US COVID-19 Hospitalizations vs. Case Positivity",
       subtitle = "Seven-day rolling averages for { current_frame }\nSource: covidtracking.com",
       x = "Hospitalizations",
       y = "Case Positivity") +
  transition_manual(date, cumulative = TRUE)

animate(us_daily.anim, fps = 4, end_pause = 25, rewind = FALSE,
        width = 900, height = 850, res = 150)

anim_save("output/us_hosp_walkgraph.gif")


#walk graph regional hospitalizations
region_daily.plot <-
  region_daily.df %>%
  mutate(date_abbr = paste0(month(date, label = TRUE),'. ', day(date),', ', year(date))) %>%
  filter(date >= "2020-03-15") %>%
  ggplot(aes(x = hospitalized7day, y = pct_pos_7day, size = total7day)) +
  geom_point(shape = 21, fill = color.carto_bold[2], stroke = 0, alpha = 0.3) +
  geom_path(size = 0.5, color = "black") +
  geom_hline(yintercept = 0, size = 0.25, color = color.dark_gray) +
  facet_wrap(~region, ncol = 2) +
  scale_y_continuous(label = percent, limits = c(0,NA)) +
  scale_x_continuous(label = label_number_si(), limits = c(0,NA)) +
  scale_size_continuous(range = c(0.5, 6), name = "Daily Tests", label = label_number_si()) +
  theme(legend.position = "right")

#static image
region_daily.plot +
  #geom_text(aes(label = date_abbr), size = 2, check_overlap = TRUE) +
  labs(title = "Daily Hospitalizations vs. Case Positivity",
       subtitle = "Seven-day rolling averages, since March 15, 2020\nSource: covidtesting.com",
       x = "Hospitalizations",
       y = "Case Positivity")

#animation
region_daily.anim <-
  region_daily.plot +
  labs(title = "US COVID-19 Hospitalizations vs. Case Positivity",
       subtitle = "Seven-day rolling averages for { current_frame }\nSource: covidtracking.com",
       x = "Hospitalizations",
       y = "Case Positivity") +
  transition_manual(date, cumulative = TRUE)

animate(region_daily.anim, fps = 4, end_pause = 30, rewind = FALSE,
        width = 900, height = 875, res = 150)

anim_save("output/regional_hosp_walkgraph.gif")


#walk graph US ICU admissions
us_daily.plot <-
  us_daily.df %>%
  mutate(date_abbr = paste0(month(date, label = TRUE),'. ', day(date),', ', year(date))) %>%
  filter(date >= "2020-03-15") %>%
  ggplot(aes(x = inIcu7day, y = pct_pos_7day, size = total7day)) +
  geom_point(shape = 21, fill = color.carto_bold[2], stroke = 0, alpha = 0.3) +
  geom_path(size = 0.5, color = "black") +
  geom_hline(yintercept = 0, size = 0.25, color = color.dark_gray) +
  scale_y_continuous(label = percent, limits = c(0,NA)) +
  scale_x_continuous(label = comma, limits = c(0,NA)) +
  scale_size_continuous(range = c(1, 8), name = "Daily Tests", label = label_number_si()) +
  theme(legend.position = "right")


us_daily.plot +
  #geom_text(aes(label = date_abbr), size = 2, check_overlap = TRUE) +
  labs(title = "Daily ICU Admissions vs. Case Positivity",
       subtitle = "Seven-day rolling averages, since March 15, 2020\nSource: covidtesting.com",
       x = "ICU Admissions",
       y = "Case Positivity")

#walk graph US hospitalizations animation
us_daily.anim <-
  us_daily.plot +
  labs(title = "US COVID-19 ICU Admissions vs. Case Positivity",
       subtitle = "Seven-day rolling averages for { current_frame }\nSource: covidtracking.com",
       x = "ICU Admissions",
       y = "Case Positivity") +
  transition_manual(date, cumulative = TRUE)

animate(us_daily.anim, fps = 4, end_pause = 25, rewind = FALSE,
        width = 900, height = 850, res = 150)

anim_save("output/us_icu_walkgraph.gif")


#walk graph regional ICU admissions
region_daily.plot <-
  region_daily.df %>%
  mutate(date_abbr = paste0(month(date, label = TRUE),'. ', day(date),', ', year(date))) %>%
  filter(date >= "2020-03-15") %>%
  ggplot(aes(x = inIcu7day, y = pct_pos_7day, size = total7day)) +
  geom_point(shape = 21, fill = color.carto_bold[2], stroke = 0, alpha = 0.3) +
  geom_path(size = 0.5, color = "black") +
  geom_hline(yintercept = 0, size = 0.25, color = color.dark_gray) +
  facet_wrap(~region, ncol = 2) +
  scale_y_continuous(label = percent, limits = c(0,NA)) +
  scale_x_continuous(label = label_number_si(), limits = c(0,NA)) +
  scale_size_continuous(range = c(0.5, 6), name = "Daily Tests", label = label_number_si()) +
  theme(legend.position = "right")

#static image
region_daily.plot +
  #geom_text(aes(label = date_abbr), size = 2, check_overlap = TRUE) +
  labs(title = "Daily ICU Admissions vs. Case Positivity",
       subtitle = "Seven-day rolling averages, since March 15, 2020\nSource: covidtesting.com",
       x = "ICU Admissions",
       y = "Case Positivity")

#animation
region_daily.anim <-
  region_daily.plot +
  labs(title = "US COVID-19 ICU Admissions vs. Case Positivity",
       subtitle = "Seven-day rolling averages for { current_frame }\nSource: covidtracking.com",
       x = "ICU Admissions",
       y = "Case Positivity") +
  transition_manual(date, cumulative = TRUE)

animate(region_daily.anim, fps = 4, end_pause = 30, rewind = FALSE,
        width = 900, height = 875, res = 150)

anim_save("output/regional_icu_walkgraph.gif")


#walk graph US deaths
us_daily.plot <-
  us_daily.df %>%
  mutate(date_abbr = paste0(month(date, label = TRUE),'. ', day(date),', ', year(date))) %>%
  filter(date >= "2020-03-15") %>%
  ggplot(aes(x = death7day, y = pct_pos_7day, size = total7day)) +
  geom_point(shape = 21, fill = color.carto_bold[2], stroke = 0, alpha = 0.3) +
  geom_path(size = 0.5, color = "black") +
  geom_hline(yintercept = 0, size = 0.25, color = color.dark_gray) +
  scale_y_continuous(label = percent, limits = c(0,NA)) +
  scale_x_continuous(label = comma, limits = c(0,NA)) +
  scale_size_continuous(range = c(1, 8), name = "Daily Tests", label = label_number_si()) +
  theme(legend.position = "right")


us_daily.plot +
  #geom_text(aes(label = date_abbr), size = 2, check_overlap = TRUE) +
  labs(title = "Daily Deaths vs. Case Positivity",
       subtitle = "Seven-day rolling averages, since March 15, 2020\nSource: covidtesting.com",
       x = "Deaths",
       y = "Case Positivity")

#walk graph US hospitalizations animation
us_daily.anim <-
  us_daily.plot +
  labs(title = "US COVID-19 Deaths vs. Case Positivity",
       subtitle = "Seven-day rolling averages for { current_frame }\nSource: covidtracking.com",
       x = "Deaths",
       y = "Case Positivity") +
  transition_manual(date, cumulative = TRUE)

animate(us_daily.anim, fps = 4, end_pause = 25, rewind = FALSE,
        width = 900, height = 850, res = 150)

anim_save("output/us_deaths_walkgraph.gif")


#walk graph regional ICU admissions
region_daily.plot <-
  region_daily.df %>%
  mutate(date_abbr = paste0(month(date, label = TRUE),'. ', day(date),', ', year(date))) %>%
  filter(date >= "2020-03-15") %>%
  ggplot(aes(x = death7day, y = pct_pos_7day, size = total7day)) +
  geom_point(shape = 21, fill = color.carto_bold[2], stroke = 0, alpha = 0.3) +
  geom_path(size = 0.5, color = "black") +
  geom_hline(yintercept = 0, size = 0.25, color = color.dark_gray) +
  facet_wrap(~region, ncol = 2) +
  scale_y_continuous(label = percent, limits = c(0,NA)) +
  scale_x_continuous(label = comma, limits = c(0,NA)) +
  scale_size_continuous(range = c(0.5, 6), name = "Daily Tests", label = label_number_si()) +
  theme(legend.position = "right")

#static image
region_daily.plot +
  #geom_text(aes(label = date_abbr), size = 2, check_overlap = TRUE) +
  labs(title = "Daily Deaths vs. Case Positivity",
       subtitle = "Seven-day rolling averages, since March 15, 2020\nSource: covidtesting.com",
       x = "Deaths",
       y = "Case Positivity")

#animation
region_daily.anim <-
  region_daily.plot +
  labs(title = "US COVID-19 Deaths vs. Case Positivity",
       subtitle = "Seven-day rolling averages for { current_frame }\nSource: covidtracking.com",
       x = "Deaths",
       y = "Case Positivity") +
  transition_manual(date, cumulative = TRUE)

animate(region_daily.anim, fps = 4, end_pause = 30, rewind = FALSE,
        width = 900, height = 875, res = 150)

anim_save("output/regional_deaths_walkgraph.gif")