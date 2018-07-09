rm (list = ls())
# Load packages
library(tidyverse)
library(haven)
library(plotly)
library(ReIns)

# set seed
set.seed(1)

# Read travel survey data
rd <- read_csv("data/synth_pop_data/accra/travel_survey/synthetic_population_with_trips.csv")

# Create a row id
rd$rid <- 1:nrow(rd)


# Define motorcycle mode
rd <- rd %>% mutate(trip_mode = ifelse( (trip_mode == 'Other' & trip_duration < 60), 'Motorcycle', trip_mode))


# Divide bus trips into bus and walk trips
bus_trips <- filter(rd, trip_mode == "Bus")

# Copy pasted rob's code with minor adjustments
modes <- list(bus.passenger='Bus',car.passenger='Taxi',pedestrian='Walking',car='Private Car',cyclist='Bicycle',motorcycle='Motorcycle')
## speeds
speeds <- list(bus.passenger=15,car.passenger=21,pedestrian=4.8,car=21,cyclist=14.5,motorcycle=25,tuktuk=22)

## bus wait and walk times
min_wait_time <- c(0,5,10,15,20,30)#exclusive
max_wait_time <- c(5,10,15,20,30,60)#inclusive
wait_time_proportion <- c(51.3,20.3,7.6,6.3,8.3,6.2)
wait_rate <- 0.11
min_walk_time <- c(0,10,20,30,40,60)#exclusive
max_walk_time <- c(10,20,30,40,60, max(subset(bus_trips, trip_mode == 'Bus')$trip_duration) + 1)#inclusive
walk_time_proportion <- c(80.6,13.5,4.3,1.3,0.1,0)
walk_rate <- 0.15
min_bus_duration <- 5

## subtract wait and walk times, and add new walk journeys
bus_trips <- subset(bus_trips, trip_mode == 'Bus')
bus_trips$trip_duration <- sapply(bus_trips$trip_duration, function(x) x- rtexp(1, rate = wait_rate, endpoint = x-min_bus_duration))

walk_trips <- bus_trips
walk_trips$trip_mode <- 'Short Walking'
walk_trips$trip_duration <- sapply(walk_trips$trip_duration,function(x)rtexp(1,rate=wait_rate,endpoint=x-min_bus_duration))

bus_trips$trip_duration <- bus_trips$trip_duration - walk_trips$trip_duration

rd[rd$trip_mode == 'Bus' & rd$rid %in% bus_trips$rid,]$trip_duration <- bus_trips$trip_duration

rd <- rbind(rd, walk_trips)

# Define trip_distances (in km)
# Based on travel mode and trip duration, calculate distances

trip_mode <- c("Bus", "Private Car", "Taxi", "Walking",
               "Short Walking", "Bicycle", "Motorcycle")
speeds <- c(15, 21, 21, 4.8, 4.8, 14.5, 25)

modes_speeds_lt <- data.frame(trip_mode, speeds)
modes_speeds_lt$trip_mode <- as.character(modes_speeds_lt$trip_mode)

rd <- left_join(rd, modes_speeds_lt, by = "trip_mode")

rd$speeds[is.na(rd$speeds)] <- 0

rd$trip_distance <- (rd$trip_duration / 60) * rd$speeds

rd$speeds <- NULL

# Define distance categories
dist_cat <- c("0-1 km", "1-2 km", "2-3 km", "3-5 km", "5-7 km", "7-10 km", ">10 km" )

# Initialize them
rd$trip_distance_cat[rd$trip_distance > 0 & rd$trip_distance <= 1] <- dist_cat[1]
rd$trip_distance_cat[rd$trip_distance > 1 & rd$trip_distance <= 2] <- dist_cat[2]
rd$trip_distance_cat[rd$trip_distance > 2 & rd$trip_distance <= 3] <- dist_cat[3]
rd$trip_distance_cat[rd$trip_distance > 3 & rd$trip_distance <= 5] <- dist_cat[4]
rd$trip_distance_cat[rd$trip_distance > 5 & rd$trip_distance <= 7] <- dist_cat[5]
rd$trip_distance_cat[rd$trip_distance > 7 & rd$trip_distance <= 10] <- dist_cat[6]
rd$trip_distance_cat[rd$trip_distance > 10] <- dist_cat[7]

# Make age category
age_category <- c("15-49", "50-70", ">70")
rd$age_cat[rd$age >= 15 & rd$age < 50] <- age_category[1]
rd$age_cat[rd$age >= 50 & rd$age <= 70] <- age_category[2]
rd$age_cat[rd$age > 70] <- age_category[3]

# Save all participants greater than 70 in a df
raw_data_70g <- filter(rd, age_cat == age_category[3])

# Remove all participants greater than 70 years of age
rd <- filter(rd, age_cat != age_category[3])

# Create row_id columns for all trips
rd$row_id <- 1:nrow(rd)

# Redefine short walk as their own category (part of bus trips)
rd[duplicated(rd$trip_id),]$trip_mode <- 'Short Walking'

# Create scenario 1: 50% of all trips walking trips (in each dist bracket) to Private Car
walking_trips <- subset(rd, trip_mode == "Walking")

# Copy mode and duration from baseline
rd$scen1_mode <- rd$trip_mode
rd$scen1_duration <- rd$trip_duration

# Take 50% of trips in each distance category and then convert them into walking 
for (i in 1:length(dist_cat)){
  print(dist_cat[i])
  # i <- 2
  trips <- filter(walking_trips, trip_distance_cat == dist_cat[i]) 
  trips_sample <- trips %>% sample_frac(.5) %>% mutate(trip_mode = "Private Car")
  # Recalculate trip duration for Private car trips
  trips_sample$trip_duration <- (trips_sample$trip_distance / 4.8 ) * 21
  trips_sample$scen1_mode <- trips_sample$trip_mode
  trips_sample$scen1_duration <- trips_sample$trip_duration
  trips_sample <- select(trips_sample, row_id, scen1_mode, scen1_duration)
  print(nrow(trips_sample))
  
  # Update selected rows for mode and duration
  rd[rd$row_id %in% trips_sample$row_id,]$scen1_mode <- trips_sample$scen1_mode
  rd[rd$row_id %in% trips_sample$row_id,]$scen1_duration <- trips_sample$scen1_duration
  
}

# Scenario 2: All car to Cycle
# 50% of all trips less than 7km to cycle
car_trips <- subset(rd, trip_mode == "Private Car" | trip_mode == "Taxi" & !is.na(trip_duration))

# Copy baseline's mode and duration
rd$scen2_mode <- rd$trip_mode
rd$scen2_duration <- rd$trip_duration

# Loop through all trips less than 7 km
for (i in 1:5){
  print(dist_cat[i])
  # i <- 2
  trips <- filter(car_trips, trip_distance_cat == dist_cat[i]) 
  trips_sample <- trips %>% sample_frac(.5) %>% mutate(trip_mode = "Bicycle")
  
  # Recalculate trip duration for new cycling trips
  trips_sample$trip_duration <- (trips_sample$trip_distance / 21 ) * 14.5
  trips_sample$scen1_mode <- trips_sample$trip_mode
  trips_sample$scen1_duration <- trips_sample$trip_duration
  trips_sample <- select(trips_sample, row_id, scen1_mode, scen1_duration)
  print(nrow(trips_sample))
  
  # Update relevant rows
  rd[rd$row_id %in% trips_sample$row_id,]$scen2_mode <- trips_sample$scen1_mode
  rd[rd$row_id %in% trips_sample$row_id,]$scen2_duration <- trips_sample$scen1_duration
  
}


# Scenario 3: All car to Bus
# 50% of all trips longer than 10km to Bus 
# In this scenario, you will need to add walking trip of 10 minutes 

# Copy mode and duration from baseline
rd$scen3_mode <- rd$trip_mode
rd$scen3_duration <- rd$trip_duration


# Only consider trips greater than 10 km
for (i in 7){
  print(dist_cat[i])
  # i <- 2
  trips <- filter(car_trips, trip_distance_cat == dist_cat[i]) 
  # Create a specific category for bus trips, so that later on short walk trips can easily be introduced
  trips_sample <- trips %>% sample_frac(.5) %>% mutate(trip_mode = "CB")
  # Recalculate trip duration
  trips_sample$trip_duration <- (trips_sample$trip_distance / 21 ) * 15
  trips_sample$scen1_mode <- trips_sample$trip_mode
  trips_sample$scen1_duration <- trips_sample$trip_duration
  trips_sample <- select(trips_sample, row_id, scen1_mode, scen1_duration)
  print(nrow(trips_sample))
  # Update relevant rows
  rd[rd$row_id %in% trips_sample$row_id,]$scen3_mode <- trips_sample$scen1_mode
  rd[rd$row_id %in% trips_sample$row_id,]$scen3_duration <- trips_sample$scen1_duration
  
}


# for scenario 3, add short walking trips for the newly created bus trips
# code originally written by rob

# Copy pasted rob's code with minor adjustments

modes <- list(bus.passenger='CB',car.passenger='Taxi',pedestrian='Walking',car='Private Car',cyclist='Bicycle',motorcycle='Motorcycle')
## speeds
speeds <- list(bus.passenger=15,car.passenger=21,pedestrian=4.8,car=21,cyclist=14.5,motorcycle=25,tuktuk=22)

## bus wait and walk times
min_wait_time <- c(0,5,10,15,20,30)#exclusive
max_wait_time <- c(5,10,15,20,30,60)#inclusive
wait_time_proportion <- c(51.3,20.3,7.6,6.3,8.3,6.2)
wait_rate <- 0.11
min_walk_time <- c(0,10,20,30,40,60)#exclusive
max_walk_time <- c(10,20,30,40,60, max(subset(rd, scen3_mode == 'Bus')$trip_duration) + 1)#inclusive
walk_time_proportion <- c(80.6,13.5,4.3,1.3,0.1,0)
walk_rate <- 0.15
min_bus_duration <- 5

# Use the specialized CB category to identify new short walk trips
## subtract wait and walk times. add new walk journeys
bus_trips <- subset(rd, scen3_mode == 'CB')
other_trips <- subset(rd, scen3_mode != 'CB')
bus_trips$scen3_duration <- sapply(bus_trips$scen3_duration, function(x) x- rtexp(1, rate = wait_rate, endpoint = x-min_bus_duration))
walk_trips <- bus_trips
walk_trips$scen3_mode <- 'Short Walking'
walk_trips$scen3_duration <- sapply(walk_trips$scen3_duration,function(x)rtexp(1,rate=wait_rate,endpoint=x-min_bus_duration))
bus_trips$scen3_duration <- bus_trips$scen3_duration - walk_trips$scen3_duration

# For the newly added short walk trips for scen3, make NAs for the baseline and two scenarios
walk_trips[, c("trip_mode", "trip_duration", "scen1_mode", "scen1_duration", "scen2_mode", "scen2_duration")] <- NA
walk_trips$row_id <- 0

rd <- rbind(rd, walk_trips)

# Rename intermediate mode CB to Bus
rd [rd$scen3_mode == 'CB',]$scen3_mode <- "Bus"

# Redefine row_id
rd$rid <- 1:nrow(rd)

write_csv(rd, "baseline_and_three_scenarios.csv")

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

# Add 70g distance trips to all scenarios
raw_data_70g$scen1_mode <- raw_data_70g$scen2_mode <- raw_data_70g$scen3_mode <- raw_data_70g$trip_mode
raw_data_70g$scen1_duration <- raw_data_70g$scen2_duration <- raw_data_70g$scen3_duration <- raw_data_70g$trip_duration
raw_data_70g$scen1_distance <- raw_data_70g$scen2_distance <- raw_data_70g$scen3_distance <- raw_data_70g$trip_distance


## ADD 70g distance trips
raw_data_dist <- plyr::rbind.fill(rd, raw_data_70g)

# write_csv(raw_data_dist, "raw_data_dist.csv")

dist <- filter(raw_data_dist, !is.na(scen1_mode)) %>% group_by(trip_mode) %>% summarise(sum = sum(trip_distance))
dist1 <- filter(raw_data_dist, !is.na(scen1_mode)) %>% group_by(scen1_mode) %>% summarise(sum = sum(trip_distance))
dist2 <- filter(raw_data_dist, !is.na(scen1_mode)) %>% group_by(scen2_mode) %>% summarise(sum = sum(trip_distance))
dist3 <- raw_data_dist %>% group_by(scen3_mode) %>% summarise(sum = sum(trip_distance))
dist$sum_scen1 <- dist1$sum
dist$sum_scen2 <- dist2$sum
dist$sum_scen3 <- dist3$sum
View(dist)
dist <- rename(dist, sum_baseline = sum)

write_csv(dist, "dist_by_mode_all_scenarios_all_ages.csv")

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