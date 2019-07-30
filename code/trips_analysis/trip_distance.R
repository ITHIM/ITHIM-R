rm(list = ls())

library(tidyverse)
library(ggthemes)

io <- read_rds("results/multi_city/io.rds")

rd <- io$accra$trip_scen_sets %>% filter(scenario == "Baseline")

rd <- rd %>%
  filter(!(trip_mode %in% c("taxi", "rail")))

# ggplot(rd) +
#   aes(x = trip_distance) +
#   geom_density(adjust = 1L) +
#   scale_fill_hue() +
#   theme_tufte() +
#   facet_wrap(vars(trip_mode), scales = "free") +
#   labs(title = "Distribution of trips by distance - Accra")


ggplot(rd) +
  aes(x = trip_mode, y = trip_distance) +
  geom_boxplot(adjust = 1L, scale = "area", fill = "#39486b") +
  geom_hline(yintercept = c(2, 6), linetype="dashed", color = "red", size=0.1) +
  labs(title = "Distribution of trips by distance - Accra", x = "Mode", y = "Distance (km)")
