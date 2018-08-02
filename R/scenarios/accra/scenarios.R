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

t_not_used <- filter(rd, trip_mode %in% c('Short Walking', '99'))

rdr <- filter(rd, ! trip_mode %in% c('Short Walking', '99'))

#rd_without_short_trips <- arrange(rd_without_short_trips, trip_duration)

# 16% Bus
# 49% walk

tt <- nrow(rdr)

source_modes <- c('Bus', 'Walking')
target_modes <- c('Private Car')

target_new_trips <- c(nrow(filter(rdr, trip_mode == 'Bus')) - round(0.16 * tt), 
                      nrow(filter(rdr, trip_mode == 'Walking')) - round(0.49 * tt ))

# trip_mode <- c("Bus", "Private Car", "Taxi", "Walking",
#                "Short Walking", "Bicycle", "Motorcycle")
# speeds <- c(15, 21, 21, 4.8, 4.8, 14.5, 25)

# 16 % Bus	1622
# 49% walk	4969


bus_trips_sample <- filter(rdr, trip_mode == source_modes[1]) %>% sample_n(target_new_trips[1]) %>% mutate(trip_mode = target_modes[1])
# Recalculate trip duration for Private car trips
bus_trips_sample$trip_duration <- (bus_trips_sample$trip_distance / 15 ) * 21
bus_trips_sample <- select(bus_trips_sample, row_id, trip_mode, trip_duration)
print(nrow(bus_trips_sample))

# Update selected rows for mode and duration
rdr[rdr$row_id %in% bus_trips_sample$row_id,]$trip_mode <- bus_trips_sample$trip_mode
rdr[rdr$row_id %in% bus_trips_sample$row_id,]$trip_duration <- bus_trips_sample$trip_duration


walk_trips_sample <- filter(rdr, trip_mode == source_modes[2]) %>% sample_n(target_new_trips[2]) %>% mutate(trip_mode = target_modes[1])
# Recalculate trip duration for Private car trips
walk_trips_sample$trip_duration <- (walk_trips_sample$trip_distance / 4.8 ) * 21
walk_trips_sample <- select(walk_trips_sample, row_id, trip_mode, trip_duration)
print(nrow(walk_trips_sample))

# Update selected rows for mode and duration
rdr[rdr$row_id %in% walk_trips_sample$row_id,]$trip_mode <- walk_trips_sample$trip_mode
rdr[rdr$row_id %in% walk_trips_sample$row_id,]$trip_duration <- walk_trips_sample$trip_duration

rdr %>% group_by(trip_mode) %>% summarise(c = n(), p = n() / nrow(rdr) * 100)

rdr <- rbind(rdr, t_not_used)

rdr$scenario <- "Scenario 1"

rdfinal <- rbind(rd, rdr)



## Scenario 1 (named as Scenario 2)

rdr <- filter(rd, ! trip_mode %in% c('Short Walking', '99'))

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


car_trips_sample <- filter(rdr, trip_mode %in% c(source_modes[1], source_modes[2] )) %>% 
  sample_n(target_new_trips[1]) %>% mutate(trip_mode = target_modes[1])

# Recalculate trip duration for Private car trips
car_trips_sample$trip_duration <- (car_trips_sample$trip_distance / 15 ) * 21
car_trips_sample <- select(car_trips_sample, row_id, trip_mode, trip_duration)

# Update selected rows for mode and duration
rdr[rdr$row_id %in% car_trips_sample$row_id,]$trip_mode <- car_trips_sample$trip_mode
rdr[rdr$row_id %in% car_trips_sample$row_id,]$trip_duration <- car_trips_sample$trip_duration

rdr %>% group_by(trip_mode) %>% summarise(c = n(), p = n() / nrow(rdr) * 100)

rdr <- rbind(rdr, t_not_used)

rdr$scenario <- "Scenario 2"

rdfinal <- rbind(rdfinal, rdr)


## Scenario 1 (named as Scenario 2)

rdr <- filter(rd, ! trip_mode %in% c('Short Walking', '99'))

# 10 % Bus

tt <- nrow(rdr)


# trip_mode <- c("Bus", "Private Car", "Taxi", "Walking",
#                "Short Walking", "Bicycle", "Motorcycle")
# speeds <- c(15, 21, 21, 4.8, 4.8, 14.5, 25)

source_modes <- c('Bus', 'Walking')
target_modes <- c('Motorcycle')

target_new_trips <- c(round(0.1 * tt) - nrow(filter(rdr, trip_mode == 'Motorcycle')))

# trip_mode <- c("Bus", "Private Car", "Taxi", "Walking",
#                "Short Walking", "Bicycle", "Motorcycle")
# speeds <- c(15, 21, 21, 4.8, 4.8, 14.5, 25)

# 16 % Bus	1622
# 49% walk	4969


bus_trips_sample <- filter(rdr, trip_mode == source_modes[1]) %>% sample_n(target_new_trips[1] / 2) %>% mutate(trip_mode = target_modes[1])
# Recalculate trip duration for Private car trips
bus_trips_sample$trip_duration <- (bus_trips_sample$trip_distance / 15 ) * 25
bus_trips_sample <- select(bus_trips_sample, row_id, trip_mode, trip_duration)
print(nrow(bus_trips_sample))

# Update selected rows for mode and duration
rdr[rdr$row_id %in% bus_trips_sample$row_id,]$trip_mode <- bus_trips_sample$trip_mode
rdr[rdr$row_id %in% bus_trips_sample$row_id,]$trip_duration <- bus_trips_sample$trip_duration


walk_trips_sample <- filter(rdr, trip_mode == source_modes[2]) %>% sample_n(target_new_trips[1] / 2) %>% mutate(trip_mode = target_modes[1])
# Recalculate trip duration for Private car trips
walk_trips_sample$trip_duration <- (walk_trips_sample$trip_distance / 4.8 ) * 25
walk_trips_sample <- select(walk_trips_sample, row_id, trip_mode, trip_duration)
print(nrow(walk_trips_sample))

# Update selected rows for mode and duration
rdr[rdr$row_id %in% walk_trips_sample$row_id,]$trip_mode <- walk_trips_sample$trip_mode
rdr[rdr$row_id %in% walk_trips_sample$row_id,]$trip_duration <- walk_trips_sample$trip_duration

rdr %>% group_by(trip_mode) %>% summarise(c = n(), p = n() / nrow(rdr) * 100)

rdr <- rbind(rdr, t_not_used)

rdr$scenario <- "Scenario 3"

rdfinal <- rbind(rdfinal, rdr)

write_csv(rdfinal, "data/scenarios/accra/baseline_and_three_scenarios.csv")