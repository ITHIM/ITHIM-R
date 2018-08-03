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

# Make age category
age_category <- c("15-49", "50-70", ">70")
rd$age_cat[rd$age >= 15 & rd$age < 50] <- age_category[1]
rd$age_cat[rd$age >= 50 & rd$age <= 70] <- age_category[2]
rd$age_cat[rd$age > 70] <- age_category[3]

# Remove all participants greater than 70 years of age
rd <- filter(rd, age_cat != age_category[3])

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

bus_trips$trip_distance <- (bus_trips$trip_duration / 60 ) * 15


rd[rd$trip_mode == 'Bus' & rd$rid %in% bus_trips$rid,]$trip_duration <- bus_trips$trip_duration

rd[rd$trip_mode == 'Bus' & rd$rid %in% bus_trips$rid,]$trip_distance <- bus_trips$trip_distance

rd <- rbind(rd, walk_trips)

rd$scenario <- "Baseline"

# Create row_id columns for all trips
rd$row_id <- 1:nrow(rd)

# Redefine short walk as their own category (part of bus trips)
# Don't apply to people without trips
rd[duplicated(rd$trip_id) & rd$trip_id != 0 & rd$trip_mode != "99",]$trip_mode <- 'Short Walking'


###############################################################
# Scenario 1

t_not_used <- filter(rd, trip_mode %in% c('Short Walking', '99'))

rdr <- filter(rd, ! trip_mode %in% c('Short Walking', '99'))

tt <- nrow(rdr)

source_modes <- c('Bus', 'Walking')
target_modes <- c('Private Car')

# 16% Bus
# 49% walk

source_trips <- c(nrow(filter(rdr, trip_mode == 'Bus')) - round(0.16 * tt), 
                      nrow(filter(rdr, trip_mode == 'Walking')) - round(0.49 * tt ))

# trip_mode <- c("Bus", "Private Car", "Taxi", "Walking",
#                "Short Walking", "Bicycle", "Motorcycle")
# speeds <- c(15, 21, 21, 4.8, 4.8, 14.5, 25)

bus_trips_sample <- filter(rdr, trip_mode == source_modes[1]) %>% sample_n(source_trips[1]) %>% 
  mutate(trip_mode = target_modes[1],
         # Recalculate trip duration for Private car trips
         trip_duration = (trip_distance * 60 ) / 21)
#bus_trips$trip_distance <- (bus_trips$trip_duration / 60 ) * 15

# Update selected rows for mode and duration
rdr[rdr$row_id %in% bus_trips_sample$row_id,]$trip_mode <- bus_trips_sample$trip_mode
rdr[rdr$row_id %in% bus_trips_sample$row_id,]$trip_duration <- bus_trips_sample$trip_duration
#rdr[rdr$row_id %in% bus_trips_sample$row_id,]$trip_distance <- bus_trips$trip_distance

walk_trips_sample <- filter(rdr, trip_mode == source_modes[2]) %>% sample_n(source_trips[2]) %>% 
  mutate(trip_mode = target_modes[1],
         # Recalculate trip duration for Private car trips
         trip_duration = (trip_distance * 60 ) / 4.8)

# Update selected rows for mode and duration
rdr[rdr$row_id %in% walk_trips_sample$row_id,]$trip_mode <- walk_trips_sample$trip_mode
rdr[rdr$row_id %in% walk_trips_sample$row_id,]$trip_duration <- walk_trips_sample$trip_duration

rdr %>% group_by(trip_mode) %>% summarise(c = n(), p = n() / nrow(.) * 100)

rdr <- rbind(rdr, t_not_used)

rdr$scenario <- "Scenario 1"

rdfinal <- rbind(rd, rdr)

###############################################################
# Scenario 2

rdr <- filter(rdfinal, scenario == 'Scenario 1' & ! trip_mode %in% c('Short Walking', '99'))

# 35 % Bus

tt <- nrow(rdr)

source_modes <- c('Private Car', 'Taxi')
target_modes <- c('Bus')

target_new_trips <- c(round(0.35 * tt) - nrow(filter(rdr, trip_mode == 'Bus')))

# trip_mode <- c("Bus", "Private Car", "Taxi", "Walking",
#                "Short Walking", "Bicycle", "Motorcycle")
# speeds <- c(15, 21, 21, 4.8, 4.8, 14.5, 25)

# 16 % Bus	1622
# 49% walk	4969


car_trips_sample <- filter(rdr, (trip_mode %in% c(source_modes[1], source_modes[2]) & 
                                   (trip_distance_cat != dist_cat[1]))) %>% 
  sample_n(target_new_trips[1]) %>% 
  mutate(trip_mode = target_modes[1],
         # Recalculate trip duration for Private car trips
         trip_duration = (trip_distance * 60 ) / 15)

##  ADDING SHORT WALK TRIPS FOR NEW BUS TRIPS

# Divide bus trips into bus and walk trips
bus_trips <- car_trips_sample

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
min_bus_duration <- min(bus_trips$trip_duration) - 0.1

## subtract wait and walk times, and add new walk journeys
#bus_trips <- subset(bus_trips, trip_mode == 'Bus')
#bus_trips$trip_duration <- sapply(bus_trips$trip_duration, function(x) x - rtexp(1, rate = wait_rate,
#                                                                                 endpoint = x - min_bus_duration))

walk_trips <- bus_trips
walk_trips$trip_mode <- 'Short Walking'
walk_trips$trip_duration <- sapply(walk_trips$trip_duration,function(x) rtexp(1, rate = wait_rate, 
                                                                              endpoint = x - min_bus_duration))


# Corrrect walk trips distance
walk_trips$trip_distance <- (walk_trips$trip_duration / 60) * 4.8

bus_trips$trip_duration <- bus_trips$trip_duration - walk_trips$trip_duration
# Corrrect bus trips distance
bus_trips$trip_distance <- (bus_trips$trip_duration / 60) * 15

# Update selected rows for mode and duration
rdr[rdr$row_id %in% bus_trips$row_id,]$trip_mode <- bus_trips$trip_mode
rdr[rdr$row_id %in% bus_trips$row_id,]$trip_duration <- bus_trips$trip_duration
rdr[rdr$row_id %in% bus_trips$row_id,]$trip_distance <- bus_trips$trip_distance

rdr %>% group_by(trip_mode) %>% summarise(c = n(), p = n() / nrow(rdr) * 100)

rdr <- rbind(rdr, walk_trips)

rdr <- rbind(rdr, t_not_used)

rdr$scenario <- "Scenario 2"

rdfinal <- rbind(rdfinal, rdr)


###############################################################
# Scenario 3

rdr <- filter(rdfinal, scenario == 'Scenario 1' & ! trip_mode %in% c('Short Walking', '99'))

# 16 % Bus remain as is
# 10 % Mcycle increase 
# x decrease private car

tt <- nrow(rdr)


# trip_mode <- c("Bus", "Private Car", "Taxi", "Walking",
#                "Short Walking", "Bicycle", "Motorcycle")
# speeds <- c(15, 21, 21, 4.8, 4.8, 14.5, 25)

source_modes <- c('Private Car')
target_modes <- c('Motorcycle')

target_new_trips <- c(round(0.1 * tt) - 
                        nrow(filter(rdr, trip_mode == 'Motorcycle')))

# trip_mode <- c("Bus", "Private Car", "Taxi", "Walking",
#                "Short Walking", "Bicycle", "Motorcycle")
# speeds <- c(15, 21, 21, 4.8, 4.8, 14.5, 25)

# 16 % Bus	1622
# 49% walk	4969

mcycle_trips_sample <- filter(rdr, trip_mode == source_modes[1]) %>% sample_n(target_new_trips[1]) %>% 
  mutate(trip_mode = target_modes[1],
         # Recalculate trip duration for Motorcycle
         trip_duration = (trip_distance * 60 / 25))

# Update selected rows for mode and duration
rdr[rdr$row_id %in% mcycle_trips_sample$row_id,]$trip_mode <- mcycle_trips_sample$trip_mode
rdr[rdr$row_id %in% mcycle_trips_sample$row_id,]$trip_duration <- mcycle_trips_sample$trip_duration


rdr %>% group_by(trip_mode) %>% summarise(c = n(), p = n() / nrow(rdr) * 100)

rdr <- rbind(rdr, t_not_used)

rdr$scenario <- "Scenario 3"

rdfinal <- rbind(rdfinal, rdr)



###############################################################
# Scenario 4

rdr <- filter(rdfinal, scenario == 'Scenario 1' & ! trip_mode %in% c('Short Walking', '99'))

# 3.5 % Cycle

tt <- nrow(rdr)

source_modes <- c('Motorcycle', 'Private Car', 'Taxi')
target_modes <- c('Bicycle')

target_new_trips <- c(52,
  round(0.035 * tt) - nrow(filter(rdr, trip_mode == 'Bicycle')) - 52)

# trip_mode <- c("Bus", "Private Car", "Taxi", "Walking",
#                "Short Walking", "Bicycle", "Motorcycle")
# speeds <- c(15, 21, 21, 4.8, 4.8, 14.5, 25)

mbike_trips <- filter(rdr, trip_mode %in% c(source_modes[1])) %>% 
  sample_n(target_new_trips[1]) %>% 
  mutate(trip_mode = target_modes[1],
         # Recalculate trip duration for Cycle trips
         trip_duration = (trip_distance * 60 ) / 14.5)

car_trips <- filter(rdr, trip_mode %in% c(source_modes[2], source_modes[3] ) & 
                            (trip_distance_cat == dist_cat[1])) %>% 
  sample_n(target_new_trips[2]) %>% 
  mutate(trip_mode = target_modes[1],
         # Recalculate trip duration for Cycle trips
         trip_duration = (trip_distance * 60 ) / 14.5)

car_mbike_trips <- rbind(mbike_trips, car_trips)


# Update selected rows for mode and duration
rdr[rdr$row_id %in% car_mbike_trips$row_id,]$trip_mode <- car_mbike_trips$trip_mode
rdr[rdr$row_id %in% car_mbike_trips$row_id,]$trip_duration <- car_mbike_trips$trip_duration

rdr %>% group_by(trip_mode) %>% summarise(c = n(), p = n() / nrow(rdr) * 100)

rdr <- rbind(rdr, t_not_used)

rdr$scenario <- "Scenario 4"

rdfinal <- rbind(rdfinal, rdr)


###############################################################
# Scenario 5

rdr <- filter(rdfinal, scenario == 'Scenario 1' & ! trip_mode %in% c('Short Walking', '99'))

# 3.5 % Cycle

tt <- nrow(rdr)

source_modes <- c('Private Car', 'Taxi')
target_modes <- c('Walking')

target_new_trips <- c(round(0.54 * tt) - nrow(filter(rdr, trip_mode == 'Walking')))

# trip_mode <- c("Bus", "Private Car", "Taxi", "Walking",
#                "Short Walking", "Bicycle", "Motorcycle")
# speeds <- c(15, 21, 21, 4.8, 4.8, 14.5, 25)

motorised_trips <- filter(rdr, trip_mode %in% c(source_modes[1], source_modes[2] )& 
                            (trip_distance_cat == dist_cat[1])) %>% 
  sample_n(target_new_trips[1]) %>% 
  mutate(trip_mode = target_modes[1],
         # Recalculate trip duration for Walking trips
         trip_duration = (trip_distance * 60 ) / 14.5)


# Update selected rows for mode and duration
rdr[rdr$row_id %in% motorised_trips$row_id,]$trip_mode <- motorised_trips$trip_mode
rdr[rdr$row_id %in% motorised_trips$row_id,]$trip_duration <- motorised_trips$trip_duration

rdr %>% group_by(trip_mode) %>% summarise(c = n(), p = n() / nrow(rdr) * 100)

rdr <- rbind(rdr, t_not_used)

rdr$scenario <- "Scenario 5"

rdfinal <- rbind(rdfinal, rdr)


####

write_csv(rdfinal, "data/scenarios/accra/baseline_and_scenarios.csv")