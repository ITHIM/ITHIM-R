# Clear workspace
rm (list = ls())
library(tidyverse)


# Read raw_data
rd <- read_csv("data/scenarios/accra/baseline_and_scenarios.csv")


# Remove short walking, 99, Train, Other and Unspecified modes
dataset <- filter(rd, ! trip_mode %in% c('Short Walking', "99", "Train", "Other", "Unspecified"))

# Trip mode plot

total_ind <- length(unique(dataset$participant_id))

l <- list()
for (i in 1:length(unique(dataset$scenario))){
  
  bd <- filter(dataset, scenario == unique(dataset$scenario)[i])
  bdnr <- nrow(bd)
  
  bd <- bd %>% group_by(trip_mode) %>%  summarise(pert = n())
  
  bd <- bd %>%  select(trip_mode, pert) %>% 
    setNames(c("trip_mode",unique(dataset$scenario)[i])) 
  
  l[[i]] <- bd
  
}

bd <- NULL
for (i in 1:length(l)){
  if (i == 1)
    bd <- l[[i]]
  else
    bd <- left_join(bd, l[[i]], by = "trip_mode")
}

write_csv(bd, 'data/scenarios/accra/trip_modes.csv')

bd <- NULL

l <- list()
for (i in 1:length(unique(dataset$scenario))){
  
  bd <- filter(dataset, scenario == unique(dataset$scenario)[i])
  bdnr <- nrow(bd)
  
  bd <- bd %>% group_by(trip_mode) %>%  summarise(pert = round(n()/bdnr * 100, 1))
  
  bd <- bd %>%  select(trip_mode, pert) %>% 
    setNames(c("trip_mode",unique(dataset$scenario)[i])) 
  
  l[[i]] <- bd
  
}

bd <- NULL
for (i in 1:length(l)){
  if (i == 1)
    bd <- l[[i]]
  else
    bd <- left_join(bd, l[[i]], by = "trip_mode")
}

bd <- reshape2::melt(bd)

plotly::ggplotly(ggplot(data = bd, aes(x = trip_mode, y = value, fill = variable)) + 
           geom_bar(stat = 'identity', position = "dodge", color = "black") + 
           theme_minimal() + xlab('Mode') + ylab('Percentage (%)') + labs(title = "Mode distribution per week"))
# Calculate trip distance for baseline and three scenarios


l <- list()
for (i in 1:length(unique(dataset$scenario))){
  # i <- 1
  dist <- rd %>% filter(scenario == unique(dataset$scenario)[i]) %>% 
    group_by(trip_mode) %>% 
    summarise(sum_dist = sum(trip_distance))
  
  dist <- filter(dist, !is.na(trip_mode))
  
  dist$sum_dist[dist$trip_mode == "Walking"] <- 
    dist$sum_dist[dist$trip_mode == "Walking"] + 
    dist$sum_dist[dist$trip_mode == "Short Walking"]
  
  dist <- dist %>%  select(trip_mode, sum_dist) %>% 
    setNames(c("trip_mode",unique(dataset$scenario)[i])) 
  
  l[[i]] <- dist
  
}

dist <- NULL
for (i in 1:length(l)){
  if (i == 1)
    dist <- l[[i]]
  else
    dist <- left_join(dist, l[[i]], by = "trip_mode")
}

# Remove short walking, 99, Train, Other and Unspecified modes
dist <- filter(dist, ! trip_mode %in% c('Short Walking', "99", "Train", "Other", "Unspecified"))

write_csv(dist, "data/scenarios/accra/dist_by_mode_all_scenarios_all_ages.csv")

dist <- reshape2::melt(dist, by = trip_mode)
# Plot
plotly::ggplotly(ggplot(data = dist, aes(x = trip_mode, y = value / total_ind, fill = variable)) + 
                   geom_bar(stat = 'identity', position = 'dodge', color = "black") + 
                   theme_minimal() + xlab('Mode') + ylab('Distance (km)') + labs(title = "Mode distance  per person per week (km)")
)



l <- list()
for (i in 1:length(unique(dataset$scenario))){
  # i <- 1
  dur <- rd %>% filter(scenario == unique(dataset$scenario)[i]) %>% 
    group_by(trip_mode) %>% 
    summarise(sum_dur = sum(trip_duration))
  
  dur <- filter(dur, !is.na(trip_mode))
  
  dur$sum_dur[dur$trip_mode == "Walking"] <- 
    dur$sum_dur[dur$trip_mode == "Walking"] + 
    dur$sum_dur[dur$trip_mode == "Short Walking"]
  
  dur <- dur %>%  select(trip_mode, sum_dur) %>% 
    setNames(c("trip_mode",unique(dataset$scenario)[i])) 
  
  l[[i]] <- dur
  
}

dur <- NULL
for (i in 1:length(l)){
  if (i == 1)
    dur <- l[[i]]
  else
    dur <- left_join(dur, l[[i]], by = "trip_mode")
}

# Remove short walking, 99, Train, Other and Unspecified modes
dur <- filter(dur, ! trip_mode %in% c('Short Walking', "99", "Train", "Other", "Unspecified"))

write_csv(dur, "data/scenarios/accra/dur_by_mode_all_scenarios_all_ages.csv")

dur <- reshape2::melt(dur, by = trip_mode)

dur$value <- round(dur$value / (60 * total_ind), 2)

# Plot
plotly::ggplotly(ggplot(data = dur, aes(x = trip_mode, y = value, fill = variable)) + 
                   geom_bar(stat = 'identity', position = 'dodge', color = 'black') + 
                   theme_minimal() + xlab('Mode') + ylab('Duration (hours)') + labs(title = 
                                                                                      "Mode Duration per person per week (hours)")
)
