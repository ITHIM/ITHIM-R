# Clear workspace
rm (list = ls())
library(tidyverse)


# Read raw_data
rd <- read_csv("data/scenarios/accra/baseline_and_three_scenarios.csv")

# Create summary frequency for baseline and three scenarios
td <- select(rd, trip_mode, scen1_mode, scen2_mode, scen3_mode) 
td1 <- td %>% filter(!is.na(scen1_mode)) %>% group_by(trip_mode) %>% summarise(n = n()) %>% mutate(baseline_freq = round(n / sum(n) * 100, 2)) %>% select(trip_mode, baseline_freq)
td1 <- cbind(td1, td %>% filter(!is.na(scen1_mode)) %>% group_by(scen1_mode) %>% summarise(n = n()) %>% mutate(scen1_freq = round(n / sum(n) * 100, 2)) %>% select(scen1_freq))
td1 <- cbind(td1, td %>% filter(!is.na(scen1_mode)) %>% group_by(scen2_mode) %>% summarise(n = n()) %>% mutate(scen2_freq = round(n / sum(n) * 100, 2)) %>% select(scen2_freq))
td1 <- cbind(td1, td %>% group_by(scen3_mode) %>% summarise(n = n()) %>% mutate(scen3_freq = round(n / sum(n) * 100, 2)) %>% select(scen3_freq))
td2 <- reshape2::melt(td1,id.vars="trip_mode")

td2 <- rename(td2, percentage = value)

td2 <- filter(td2, trip_mode != 'Short Walking')

# Plot mode distribution for baseline and three scenarios
ggplot(data = td2, aes(x = trip_mode, y = percentage, fill = variable)) + geom_bar(stat = 'identity', position = 'dodge') + theme_minimal()+ xlab('Mode') + ylab('Percentage (%)') + labs(title = 'Mode distribution in baseline and three scenarios')

# Calculate trip distance for baseline and three scenarios

dist <- rd %>% group_by(trip_mode) %>% summarise(baseline_dist = sum(trip_distance))
dist1 <- rd %>% group_by(scen1_mode) %>% summarise(scen1_dist = sum(trip_distance)) %>% rename(trip_mode = scen1_mode)
dist2 <- rd %>% group_by(scen2_mode) %>% summarise(scen2_dist = sum(trip_distance)) %>% rename(trip_mode = scen2_mode)
dist3 <- rd %>% group_by(scen3_mode) %>% summarise(scen3_dist = sum(trip_distance)) %>% rename(trip_mode = scen3_mode)

dist <- filter(dist, !is.na(trip_mode))
dist1 <- filter(dist1, !is.na(trip_mode))
dist2 <- filter(dist2, !is.na(trip_mode))
dist3 <- filter(dist3, !is.na(trip_mode))

dist$baseline_dist[dist$trip_mode == "Walking"] <- dist$baseline_dist[dist$trip_mode == "Walking"] + dist$baseline_dist[dist$trip_mode == "Short Walking"]

dist1$scen1_dist[dist1$trip_mode == "Walking"] <- dist1$scen1_dist[dist1$trip_mode == "Walking"] + dist1$scen1_dist[dist1$trip_mode == "Short Walking"]

dist2$scen2_dist[dist2$trip_mode == "Walking"] <- dist2$scen2_dist[dist2$trip_mode == "Walking"] + dist2$scen2_dist[dist2$trip_mode == "Short Walking"]

dist3$scen3_dist[dist3$trip_mode == "Walking"] <- dist3$scen3_dist[dist3$trip_mode == "Walking"] + dist3$scen3_dist[dist3$trip_mode == "Short Walking"]

dist <- left_join(dist, dist1, by = "trip_mode")
dist <- left_join(dist, dist2, by = "trip_mode")
dist <- left_join(dist, dist3, by = "trip_mode")


dist <- rename(dist, "Baseline" = baseline_dist,
               "Scenario 1" = scen1_dist,
               "Scenario 2" = scen2_dist,
               "Scenario 3" = scen3_dist
)

# Remove short walking, 99, Train, Other and Unspecified modes
dist <- filter(dist, ! trip_mode %in% c('Short Walking', "99", "Train", "Other", "Unspecified"))

write_csv(dist, "data/scenarios/accra/dist_by_mode_all_scenarios_all_ages.csv")

dist <- reshape2::melt(dist, by = trip_mode)
# Plot
ggplot(data = dist, aes(x = trip_mode, y = value, fill = variable)) + geom_bar(stat = 'identity', position = 'dodge') + theme_minimal() + xlab('Mode') + ylab('Distance (km)') + labs(title = "Mode distance (km)")


dur <- rd %>% group_by(trip_mode) %>% summarise(baseline_dur = sum(trip_duration))
dur1 <- rd %>% group_by(scen1_mode) %>% summarise(scen1_dur = sum(scen1_duration)) %>% rename(trip_mode = scen1_mode)
dur2 <- rd %>% group_by(scen2_mode) %>% summarise(scen2_dur = sum(scen2_duration)) %>% rename(trip_mode = scen2_mode)
dur3 <- rd %>% group_by(scen3_mode) %>% summarise(scen3_dur = sum(scen3_duration)) %>% rename(trip_mode = scen3_mode)

dur <- filter(dur, !is.na(trip_mode))
dur1 <- filter(dur1, !is.na(trip_mode))
dur2 <- filter(dur2, !is.na(trip_mode))
dur3 <- filter(dur3, !is.na(trip_mode))

dur$baseline_dur[dur$trip_mode == "Walking"] <- dur$baseline_dur[dur$trip_mode == "Walking"] + dur$baseline_dur[dur$trip_mode == "Short Walking"]

dur1$scen1_dur[dur1$trip_mode == "Walking"] <- dur1$scen1_dur[dur1$trip_mode == "Walking"] + dur1$scen1_dur[dur1$trip_mode == "Short Walking"]

dur2$scen2_dur[dur2$trip_mode == "Walking"] <- dur2$scen2_dur[dur2$trip_mode == "Walking"] + dur2$scen2_dur[dur2$trip_mode == "Short Walking"]

dur3$scen3_dur[dur3$trip_mode == "Walking"] <- dur3$scen3_dur[dur3$trip_mode == "Walking"] + dur3$scen3_dur[dur3$trip_mode == "Short Walking"]

dur <- left_join(dur, dur1, by = "trip_mode")
dur <- left_join(dur, dur2, by = "trip_mode")
dur <- left_join(dur, dur3, by = "trip_mode")

dur <- rename(dur, "Baseline" = baseline_dur,
              "Scenario 1" = scen1_dur,
              "Scenario 2" = scen2_dur,
              "Scenario 3" = scen3_dur
)

# Remove short walking, 99, Train, Other and Unspecified modes
dur <- filter(dur, ! trip_mode %in% c('Short Walking', "99", "Train", "Other", "Unspecified"))

write_csv(dur, "data/scenarios/accra/dur_by_mode_all_scenarios_all_ages.csv")

dur <- reshape2::melt(dur, by = trip_mode)

dur$value <- round(dur$value / 60, 2)

# Plot
ggplot(data = dur, aes(x = trip_mode, y = value, fill = variable)) + geom_bar(stat = 'identity', position = 'dodge') + theme_minimal() + xlab('Mode') + ylab('Duration (hours)') + labs(title = "Mode Duration (hours)")
