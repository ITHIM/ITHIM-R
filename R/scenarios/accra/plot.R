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

# Remove short walking
dist <- filter(dist, trip_mode != 'Short Walking')


write_csv(dist, "data/scenarios/accra/dist_by_mode_all_scenarios_all_ages.csv")

distm <- reshape2::melt(dist, by = trip_mode)

# Remove short walking
distm <- filter(distm, trip_mode != 'Short Walking')

# Plot
ggplot(data = distm, aes(x = trip_mode, y = value, fill = variable)) + geom_bar(stat = 'identity', position = 'dodge') + theme_minimal() + xlab('Mode') + ylab('Distance (km)') + labs(title = "Mode distance (km)")


dur <- filter(rd, !is.na(scen1_mode)) %>% group_by(trip_mode) %>% summarise(sum = sum(trip_duration))
dur1 <- filter(rd, !is.na(scen1_mode)) %>% group_by(scen1_mode) %>% summarise(sum = sum(scen1_duration))
dur2 <- filter(rd, !is.na(scen1_mode)) %>% group_by(scen2_mode) %>% summarise(sum = sum(scen2_duration))
dur3 <- rd %>% group_by(scen3_mode) %>% summarise(sum = sum(scen3_duration))
dur$scen1 <- dur1$sum
dur$scen2 <- dur2$sum
dur$scen3 <- dur3$sum
View(dur)
dur <- rename(dur, baseline = sum)

durm <- reshape2::melt(dur, by = trip_mode)

# Remove short walking
durm <- filter(durm, trip_mode != 'Short Walking')

durh <- durm
durh$value <- round(durh$value / 60, 2)

# Plot
ggplot(data = durh, aes(x = trip_mode, y = value, fill = variable)) + geom_bar(stat = 'identity', position = 'dodge') + theme_minimal() + xlab('Mode') + ylab('Duration (hours)') + labs(title = "Mode Duration (hours)")