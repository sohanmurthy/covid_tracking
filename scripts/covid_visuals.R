#' Experimental visuals, need some work before publishing

source("scripts/prep_data.R")
library(scales)
library(reshape2)
library(gganimate)

#state cumulative and 7 day rolling case positivity
state_daily.df %>%
  filter(date >= today() - 60,
         !state %in% c("AS", "GU", "MP", "PR")
         ) %>%
  select(date, state, pct_pos_7day) %>%
  melt(id.vars = c("date", "state")) %>%
  mutate(variable = str_replace_all(variable, c("pct_pos_7day" = "Daily % Positive",
                                                "pct_pos_cume" = "Cumulative % Positive")
                                    )) %>%
ggplot(aes(x = date, y = value, color = variable)) +
  geom_line() +
  geom_hline(yintercept = 0, size = 0.25, color = color.dark_gray) +
  scale_y_continuous(labels = percent, limits = c(0, NA)) +
  scale_color_manual(values = color.carto_bold, name = "") +
  facet_wrap(~state, ncol = 7) +
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
