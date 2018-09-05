## Clear workspace
#rm (list = ls())
#library(tidyverse)


#read_csv("data/scenarios/accra/baseline_and_scenarios.csv")


# Remove short walking, 99, Train, Other and Unspecified modes
dataset <- filter(bs[[INDEX]], ! trip_mode %in% c('Short Walking', "99", "Train", "Other", "Unspecified"))

# Unique number of ind
total_ind <- length(unique(bs[[INDEX]]$participant_id))

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

# write_csv(bd, 'data/scenarios/accra/trip_modes.csv')

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

# write_csv(bd, 'data/scenarios/accra/trip_modes_pert.csv')

bd <- reshape2::melt(bd)

plotly::ggplotly(ggplot(data = bd, aes(x = trip_mode, y = value, fill = variable)) + 
           geom_bar(stat = 'identity', position = "dodge", color = "black") + 
           theme_minimal() + xlab('Mode') + ylab('Percentage (%)') + labs(title = "Mode distribution per week"))
# Calculate trip distance for baseline and three scenarios




l <- list()
for (i in 1:length(unique(dataset$scenario))){
  # i <- 1
  local_dist <- bs[[INDEX]] %>% filter(scenario == unique(dataset$scenario)[i]) %>% 
    group_by(trip_mode) %>% 
    summarise(sum_dist = sum(trip_distance))
  
  local_dist <- filter(local_dist, !is.na(trip_mode))
  
  # print(paste('W : ', dist$sum_dist[dist$trip_mode == "Walking"]))
  
  # print(paste('SW : ', dist$sum_dist[dist$trip_mode == "Short Walking"]))
  
  local_dist$sum_dist[local_dist$trip_mode == "Walking"] <- 
    local_dist$sum_dist[local_dist$trip_mode == "Walking"] + 
    local_dist$sum_dist[local_dist$trip_mode == "Short Walking"]
  
  #local_dist$sum_dist <- local_dist$sum_dist/total_ind
  
  local_dist <- local_dist %>%  select(trip_mode, sum_dist) %>% 
    setNames(c("trip_mode",unique(dataset$scenario)[i])) 
  
  l[[i]] <- local_dist
  
}

local_dist <- NULL
for (i in 1:length(l)){
  if (i == 1)
    local_dist <- l[[i]]
  else
    local_dist <- left_join(local_dist, l[[i]], by = "trip_mode")
}

# Remove short walking, 99, Train, Other and Unspecified modes
local_dist <- filter(local_dist, ! trip_mode %in% c('Short Walking', "99", "Train", "Other", "Unspecified"))

dist[[INDEX]] <- local_dist

# write_csv(dist, "data/scenarios/accra/dist_by_mode_all_scenarios_all_ages.csv")

dist_melted <- reshape2::melt(dist, by = trip_mode)
# Plot
plotly::ggplotly(ggplot(data = dist_melted, aes(x = trip_mode, y = value / total_ind, fill = variable)) + 
                   geom_bar(stat = 'identity', position = 'dodge', color = "black") + 
                   theme_minimal() + xlab('Mode') + ylab('Distance (km)') + labs(title = "Mode distance  per person per week (km)")
)



l <- list()
for (i in 1:length(unique(dataset$scenario))){
  # i <- 1
  local_dur <- bs[[INDEX]] %>% filter(scenario == unique(dataset$scenario)[i]) %>% 
    group_by(trip_mode) %>% 
    summarise(sum_dur = sum(trip_duration))
  
  local_dur <- filter(local_dur, !is.na(trip_mode))
  
  local_dur$sum_dur[local_dur$trip_mode == "Walking"] <- 
    local_dur$sum_dur[local_dur$trip_mode == "Walking"] + 
    local_dur$sum_dur[local_dur$trip_mode == "Short Walking"]
  
  local_dur <- local_dur %>%  select(trip_mode, sum_dur) %>% 
    setNames(c("trip_mode",unique(dataset$scenario)[i])) 
  
  l[[i]] <- local_dur
  
}

local_dur <- NULL
for (i in 1:length(l)){
  if (i == 1)
    local_dur <- l[[i]]
  else
    local_dur <- left_join(local_dur, l[[i]], by = "trip_mode")
}

# Remove short walking, 99, Train, Other and Unspecified modes
local_dur <- filter(local_dur, ! trip_mode %in% c('Short Walking', "99", "Train", "Other", "Unspecified"))

dur[[INDEX]] <- local_dur

# write_csv(dur, "data/scenarios/accra/dur_by_mode_all_scenarios_all_ages.csv")

dur_melted <- reshape2::melt(dur, by = trip_mode)

dur_melted$value <- round(dur_melted$value / (60 * total_ind), 2)

# Plot
plotly::ggplotly(ggplot(data = dur_melted, aes(x = trip_mode, y = value, fill = variable)) + 
                   geom_bar(stat = 'identity', position = 'dodge', color = 'black') + 
                   theme_minimal() + xlab('Mode') + ylab('Duration (hours)') + labs(title = 
                                                                                      "Mode Duration per person per week (hours)")
)