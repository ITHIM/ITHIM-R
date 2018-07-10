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

rd <- rd

# write_csv(rd, "rd.csv")

dist <- filter(rd, !is.na(scen1_mode)) %>% group_by(trip_mode) %>% summarise(sum = sum(trip_distance))
dist1 <- filter(rd, !is.na(scen1_mode)) %>% group_by(scen1_mode) %>% summarise(sum = sum(trip_distance))
dist2 <- filter(rd, !is.na(scen1_mode)) %>% group_by(scen2_mode) %>% summarise(sum = sum(trip_distance))
dist3 <- rd %>% group_by(scen3_mode) %>% summarise(sum = sum(trip_distance))
dist$sum_scen1 <- dist1$sum
dist$sum_scen2 <- dist2$sum
dist$sum_scen3 <- dist3$sum
View(dist)
dist <- rename(dist, sum_baseline = sum)

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