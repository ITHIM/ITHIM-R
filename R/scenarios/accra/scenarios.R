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
dist_cat <- c("0-6 km", "7-9 km", "10+ km")

# Initialize them
rd$trip_distance_cat[rd$trip_distance > 0 & rd$trip_distance < 7] <- dist_cat[1]
rd$trip_distance_cat[rd$trip_distance >= 7 & rd$trip_distance < 10] <- dist_cat[2]
rd$trip_distance_cat[rd$trip_distance >= 10] <- dist_cat[3]

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
walk_trips$trip_distance <- (walk_trips$trip_duration / 60) * 4.8

bus_trips$trip_duration <- bus_trips$trip_duration - walk_trips$trip_duration

rd[rd$trip_mode == 'Bus' & rd$rid %in% bus_trips$rid,]$trip_duration <- bus_trips$trip_duration

rd <- rbind(rd, walk_trips)

rd$scenario <- "Baseline"

# 2030 BAU bus 16%, walk 49%, car + taxi 34%, cycle 0.5%, motorcycle 0.5%
# 2030 scenario 1: bus 35%, walk 49%, car + taxi 15.5%, cycle 0.5%
# 2030 Scenario 2: bus 16%, walk 49%, car + taxi 31.5%, cycle 3.5%
# 2030 Scenario 3: bus 14%, walk 54%, car + taxi 31%, cycle 0.5%, motorcycle 0.5%

# Make age category
age_category <- c("15-49", "50-70", ">70")
rd$age_cat[rd$age >= 15 & rd$age < 50] <- age_category[1]
rd$age_cat[rd$age >= 50 & rd$age <= 70] <- age_category[2]
rd$age_cat[rd$age > 70] <- age_category[3]

# Remove all participants greater than 70 years of age
rd <- filter(rd, age_cat != age_category[3])

# Create row_id columns for all trips
rd$row_id <- 1:nrow(rd)

# Redefine short walk as their own category (part of bus trips)
# Don't apply to people without trips
rd[duplicated(rd$trip_id) & rd$trip_id != 0 & rd$trip_mode != "99",]$trip_mode <- 'Short Walking'

# > unique(rd$trip_mode)
# [1] "99"          "Bus"         "Taxi"        "Walking"     "Train"      
# [6] "Private Car" "Motorcycle"  "Other"       "Unspecified" "Bicycle"    

pwt <- filter(rd, trip_mode == '99')

rdr <- filter(rd, ! trip_mode %in% c('Short Walking', '99'))

#rd_without_short_trips <- arrange(rd_without_short_trips, trip_duration)

# 16% Bus
# 49% walk

tt <- nrow(rdr)

source_modes <- c('Bus', 'Walking')
target_modes <- c('Private Car')

target_new_trips <- c(round(0.16 * tt), round(0.49 * tt ))

for (i in source_modes){
  for (j in target_modes){
    
  }
}




# 16 % Bus	1622
# 49% walk	4969



# ## Create reference
# 
# pwt <- filter(rd, trip_mode == '99')
# 
# rd <- filter(rd, trip_mode != '99')
# 
# td <- rd %>% group_by(trip_mode) %>% summarise(count = n(), c = n() / nrow(rd) * 100)
# 
# bwtrips <- filter(rd, trip_mode %in% c('Bus', 'Walking'))

# 2030 BAU bus 16%, walk 49%, car + taxi 34%, cycle 0.5%, motorcycle 0.5%
# 2030 scenario 1: bus 35%, walk 49%, car + taxi 15.5%, cycle 0.5%
# 2030 Scenario 2: bus 16%, walk 49%, car + taxi 31.5%, cycle 3.5%
# 2030 Scenario 3: bus 14%, walk 54%, car + taxi 31%, cycle 0.5%, motorcycle 0.5%



# Create scenario 1: 50% of all trips walking trips (in each dist bracket) to Private Car
# Copy baseline trips to scenario 1's df
rd1 <- filter(rd, scenario == "Baseline")

# Rename scenario name
rd1$scenario <- "Scenario 1"

# Subset walking trips
walking_trips <- subset(rd1, trip_mode == "Walking")

# Take 50% of trips in each distance category and then convert them into walking 
for (i in 1:length(dist_cat)){
  print(dist_cat[i])
  trips <- filter(walking_trips, trip_distance_cat == dist_cat[i]) 
  trips_sample <- trips %>% sample_frac(.5) %>% mutate(trip_mode = "Private Car")
  # Recalculate trip duration for Private car trips
  trips_sample$trip_duration <- (trips_sample$trip_distance / 4.8 ) * 21
  trips_sample <- select(trips_sample, row_id, trip_mode, trip_duration)
  print(nrow(trips_sample))
  
  # Update selected rows for mode and duration
  rd1[rd1$row_id %in% trips_sample$row_id,]$trip_mode <- trips_sample$trip_mode
  rd1[rd1$row_id %in% trips_sample$row_id,]$trip_duration <- trips_sample$trip_duration
  
}

# Append scenario 1's trips to baseline
rd <- rbind(rd, rd1)

# Remove intermediate object
rm (rd1)



# Scenario 2: All car to Cycle
# 50% of all trips less than 7km to cycle

# Filter only baselien trips
rd2 <- filter(rd, scenario == "Baseline")

# Change their scenario to scenario 2
rd2$scenario <- "Scenario 2"

# Subset car and taxi trips
car_trips <- subset(rd2, trip_mode == "Private Car" | trip_mode == "Taxi" & !is.na(trip_duration))

# Loop through all trips less than 7 km
for (i in 1){
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

# Append scenario 2's trips to baseline + scenario 1 trips
rd <- rbind(rd, rd2)

# Remove intermediate file
rm (rd2)

# Scenario 3: All car to Bus
# 50% of all trips longer than 10km to Bus 
# In this scenario, you will need to add walking trip of 10 minutes 

# Subset to baseline trips
rd3 <- filter(rd, scenario == "Baseline")

# Change scenario to scenario 3
rd3$scenario <- "Scenario 3"

# Only consider trips greater than 10 km
for (i in 3){
  print(dist_cat[i])
  # i <- 2
  trips <- filter(car_trips, trip_distance_cat == dist_cat[i]) 
  # Create a specific category for bus trips, so that later on short walk trips can easily be introduced
  trips_sample <- trips %>% sample_frac(.5) %>% mutate(trip_mode = "CB")
  # Recalculate trip duration
  trips_sample$trip_duration <- (trips_sample$trip_distance / 21 ) * 15
  trips_sample <- select(trips_sample, row_id, trip_mode, trip_duration)
  print(nrow(trips_sample))
  # Update relevant rows
  rd3[rd3$row_id %in% trips_sample$row_id,]$trip_mode <- trips_sample$trip_mode
  rd3[rd3$row_id %in% trips_sample$row_id,]$trip_duration <- trips_sample$trip_duration
  
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
max_walk_time <- c(10,20,30,40,60, max(subset(rd3, trip_mode == 'CB')$trip_duration) + 1)#inclusive
walk_time_proportion <- c(80.6,13.5,4.3,1.3,0.1,0)
walk_rate <- 0.15
min_bus_duration <- 5

## subtract wait and walk times, and add new walk journeys
bus_trips <- subset(rd3, trip_mode == 'CB')
bus_trips$trip_duration <- sapply(bus_trips$trip_duration, function(x) x- rtexp(1, rate = wait_rate, endpoint = x-min_bus_duration))

walk_trips <- bus_trips
walk_trips$trip_mode <- 'Short Walking'
walk_trips$trip_duration <- sapply(walk_trips$trip_duration,function(x)rtexp(1,rate=wait_rate,endpoint=x-min_bus_duration))

# Correct walk trips distance
walk_trips$trip_distance <- (walk_trips$trip_duration / 60) * 4.8

bus_trips$trip_duration <- bus_trips$trip_duration - walk_trips$trip_duration

rd3[rd3$trip_mode == 'CB' & rd3$rid %in% bus_trips$rid,]$trip_duration <- bus_trips$trip_duration

# Add additional walk trips to scen 3
rd3 <- rbind(rd3, walk_trips)

# Rename intermediate mode CB to Bus
rd3 [rd3$trip_mode == 'CB',]$trip_mode <- "Bus"

# Rbind scenario 3
rd <- rbind(rd, rd3)

# Redefine row_id
rd$rid <- 1:nrow(rd)

write_csv(rd, "data/scenarios/accra/baseline_and_three_scenarios.csv")