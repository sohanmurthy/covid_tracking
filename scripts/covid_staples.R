#' The main charts, cleaned up and ready for publishing

source("scripts/prep_data.R")
library(scales)
library(reshape2)


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