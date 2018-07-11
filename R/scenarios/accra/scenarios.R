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


# Corrrect walk trips distance
# walk_trips$trip_distance <- 0

bus_trips$trip_duration <- bus_trips$trip_duration - walk_trips$trip_duration

rd[rd$trip_mode == 'Bus' & rd$rid %in% bus_trips$rid,]$trip_duration <- bus_trips$trip_duration

rd <- rbind(rd, walk_trips)

rd$scenario <- "Baseline"

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
# Don't apply to people without trips
rd[duplicated(rd$trip_id) & rd$trip_id != 0 & rd$trip_mode != "99",]$trip_mode <- 'Short Walking'



rd1 <- filter(rd, scenario == "Baseline")
rd1$scenario <- "Scenario 1"

# Create scenario 1: 50% of all trips walking trips (in each dist bracket) to Private Car
walking_trips <- subset(rd1, trip_mode == "Walking")

# Take 50% of trips in each distance category and then convert them into walking 
for (i in 1:length(dist_cat)){
  print(dist_cat[i])
  # i <- 2
  trips <- filter(walking_trips, trip_distance_cat == dist_cat[i]) 
  trips_sample <- trips %>% sample_frac(.5) %>% mutate(trip_mode = "Private Car")
  # Recalculate trip duration for Private car trips
  trips_sample$trip_duration <- (trips_sample$trip_distance / 4.8 ) * 21
  #trips_sample$scen1_mode <- trips_sample$trip_mode
  #trips_sample$scen1_duration <- trips_sample$trip_duration
  trips_sample <- select(trips_sample, row_id, trip_mode, trip_duration)
  print(nrow(trips_sample))
  
  # Update selected rows for mode and duration
  rd1[rd1$row_id %in% trips_sample$row_id,]$trip_mode <- trips_sample$trip_mode
  rd1[rd1$row_id %in% trips_sample$row_id,]$trip_duration <- trips_sample$trip_duration
  
}


rd <- rbind(rd, rd1)

rm (rd1)


rd2 <- filter(rd, scenario == "Baseline")
rd2$scenario <- "Scenario 2"

# Scenario 2: All car to Cycle
# 50% of all trips less than 7km to cycle
car_trips <- subset(rd2, trip_mode == "Private Car" | trip_mode == "Taxi" & !is.na(trip_duration))

# Loop through all trips less than 7 km
for (i in 1:5){
  print(dist_cat[i])
  # i <- 2
  trips <- filter(car_trips, trip_distance_cat == dist_cat[i]) 
  trips_sample <- trips %>% sample_frac(.5) %>% mutate(trip_mode = "Bicycle")
  
  # Recalculate trip duration for new cycling trips
  trips_sample$trip_duration <- (trips_sample$trip_distance / 21 ) * 14.5
  trips_sample <- select(trips_sample, row_id, trip_mode, trip_duration)
  print(nrow(trips_sample))
  
  # Update relevant rows
  rd2[rd2$row_id %in% trips_sample$row_id,]$trip_mode <- trips_sample$trip_mode
  rd2[rd2$row_id %in% trips_sample$row_id,]$trip_duration <- trips_sample$trip_duration
  
}


rd <- rbind(rd, rd2)

rm (rd2)


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
max_walk_time <- c(10,20,30,40,60, max(subset(rd, scen3_mode == 'CB')$scen3_duration) + 1)#inclusive
walk_time_proportion <- c(80.6,13.5,4.3,1.3,0.1,0)
walk_rate <- 0.15
min_bus_duration <- 5

## subtract wait and walk times, and add new walk journeys
bus_trips <- subset(rd, scen3_mode == 'CB')
bus_trips$scen3_duration <- sapply(bus_trips$scen3_duration, function(x) x- rtexp(1, rate = wait_rate, endpoint = x-min_bus_duration))

walk_trips <- bus_trips
walk_trips$scen3_mode <- 'Short Walking'
walk_trips$scen3_duration <- sapply(walk_trips$scen3_duration,function(x)rtexp(1,rate=wait_rate,endpoint=x-min_bus_duration))

bus_trips$scen3_duration <- bus_trips$scen3_duration - walk_trips$scen3_duration

rd[rd$scen3_mode == 'CB' & rd$rid %in% bus_trips$rid,]$scen3_duration <- bus_trips$scen3_duration

rd <- rbind(rd, walk_trips)

# Rename intermediate mode CB to Bus
rd [rd$scen3_mode == 'CB',]$scen3_mode <- "Bus"

# Redefine row_id
rd$rid <- 1:nrow(rd)

write.csv(rd, "data/scenarios/accra/baseline_and_three_scenarios.csv", row.names = F)