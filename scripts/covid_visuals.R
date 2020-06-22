source("scripts/prep_data.R")
library(scales)
library(reshape2)

#state cumulative and 7 day rolling case positivity
state_daily.df %>%
  filter(date >= today() - 60,
         state %in% c("FL", "AZ", "CA", "IL", "OH", "CO", "TX")
         ) %>%
  select(date, state, pct_pos_7day, pct_pos_cume) %>%
  melt(id.vars = c("date", "state")) %>%
  mutate(variable = str_replace_all(variable, c("pct_pos_7day" = "Daily % Positive",
                                                "pct_pos_cume" = "Cumulative % Positive")
                                    )) %>%
ggplot(aes(x = date, y = value, color = variable)) +
  geom_line() +
  geom_hline(yintercept = 0, size = 0.25, color = color.dark_gray) +
  scale_y_continuous(labels = percent, limits = c(0, NA)) +
  scale_color_manual(values = color.carto_bold, name = "") +
  facet_wrap(~state, ncol = 3) +
  labs(title = "Daily & Cumulative Case Positivity", subtitle = "Select states, last 60 Days") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(color = guide_legend(override.aes = list(size=1.5)))


#national cumulative and 7 day rolling case positivity
us_daily.df %>%
  filter(date >= today() - 60) %>%
  select(date, pct_pos_7day, pct_pos_cume) %>%
  melt(id.vars = c("date")) %>%
  mutate(variable = str_replace_all(variable, c("pct_pos_7day" = "Daily % Positive",
                                                "pct_pos_cume" = "Cumulative % Positive"
  ))) %>%
  ggplot(aes(x = date, y = value, color = variable)) +
  geom_line() +
  scale_y_continuous(labels = percent, limits = c(0, NA)) +
  scale_color_manual(values = color.carto_bold, name = "") +
  labs(title = "Daily & Cumulative Case Positivity", subtitle = "Select states, last 60 Days") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom") +
  guides(color = guide_legend(override.aes = list(size=1.5)))


#walk graph US
us_daily.df %>%
  filter(date >= "2020-03-15") %>%
  ggplot(aes(x = death7day, y = pct_pos_7day, size = total7day)) +
  geom_point(shape = 21, fill = color.carto_bold[2], stroke = 0, alpha = 0.3) +
  geom_path(size = 0.5, color = "black") +
  scale_y_continuous(label = percent, limits = c(0,0.25)) +
  scale_x_continuous(label = comma, limits = c(0,NA)) +
  scale_size_continuous(range = c(2, 10), name = "Daily Tests", label = label_number_si()) +
  labs(title = "Case Positivity and Deaths",
       subtitle = "Seven-day rolling averages, since March 15, 2020",
       x = "Daily Deaths",
       y = "Daily % Positive Tests") +
  theme(legend.position = "right")

#walk graph state
state_daily.df %>%
filter(state == "TX",
       date >= today() - 60) %>%
  ggplot(aes(x = death7day, y = pct_pos_7day, size = total7day)) +
  geom_point(shape = 21, fill = color.carto_bold[2], stroke = 0, alpha = 0.3) +
  geom_path(size = 0.5, color = "black") +
  scale_y_continuous(label = percent, limits = c(0,0.25)) +
  scale_x_continuous(label = comma, limits = c(0,NA)) +
  scale_size_continuous(range = c(2, 10), name = "Daily Tests", label = label_number_si()) +
  labs(title = "Case Positivity and Deaths",
       subtitle = "Seven-day rolling averages, since March 15, 2020",
       x = "Daily Deaths",
       y = "Daily % Positive Tests") +
  theme(legend.position = "right")


# ICU patients by State
state_daily.df %>%
  filter(date >= today() - 60,
         state %in% c( "AZ", "CA", "IL", "OH")
  ) %>%
  select(date, state, inIcu7day, inIcuCurrently) %>%
  melt(id.vars = c("date", "state")) %>%
  mutate(variable = str_replace_all(variable, c("inIcu7day" = "ICU (7 day avg)",
                                                "inIcuCurrently" = "ICU")
  )) %>%
  ggplot(aes(x = date, y = value, color = variable)) +
  geom_line() +
  geom_hline(yintercept = 0, size = 0.25, color = color.dark_gray) +
  scale_y_continuous(labels = comma, limits = c(0, NA)) +
  scale_color_manual(values = color.carto_bold, name = "") +
  facet_wrap(~state, ncol = 2, scales = "free_y") +
  labs(title = "Daily Patients in ICU", subtitle = "Select states, last 60 Days") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(color = guide_legend(override.aes = list(size=1.5)))

#hospitalized patients by State
state_daily.df %>%
  filter(date >= today() - 60,
         state %in% c( "AZ", "CA", "IL", "OH", "NY", "NJ", "TX", "NC", "SC")
  ) %>%
  select(date, state, hospitalized7day, hospitalizedCurrently) %>%
  melt(id.vars = c("date", "state")) %>%
  mutate(variable = str_replace_all(variable, c("hospitalized7day" = "Hospitalized (7 day avg)",
                                                "hospitalizedCurrently" = "Hospitalized")
  )) %>%
  ggplot(aes(x = date, y = value, color = variable)) +
  geom_line() +
  geom_hline(yintercept = 0, size = 0.25, color = color.dark_gray) +
  scale_y_continuous(labels = comma, limits = c(0, NA)) +
  scale_color_manual(values = color.carto_bold, name = "") +
  facet_wrap(~state, ncol = 3, scales = "free_y") +
  labs(title = "Daily Hospitalized Patients", subtitle = "Select states, last 60 Days") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(color = guide_legend(override.aes = list(size=1.5)))
