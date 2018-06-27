# Remove everything
rm (list = ls())
# Load packages
library(tidyverse)
library(haven)
library(plotly)

# Read travel survey data
raw_data <- readRDS("data/synth_pop_data/accra/travel_survey/accra_processed_trips.Rds")

raw_data[,c("trip_id", "participant_id", "age")] <- lapply(raw_data[,c("trip_id", "participant_id", "age")], as.numeric)
raw_data[,c("trip_duration", "trip_distance")] <- lapply(raw_data[,c("trip_duration", "trip_distance")], as.double)


dist_cat <- c("0-1 km", "1-2 km", "2-3 km", "3-5 km", "5-7 km", "7-10 km", ">10 km" )

raw_data$trip_distance_cat[raw_data$trip_distance > 0 & raw_data$trip_distance <= 1] <- dist_cat[1]
raw_data$trip_distance_cat[raw_data$trip_distance > 1 & raw_data$trip_distance <= 2] <- dist_cat[2]
raw_data$trip_distance_cat[raw_data$trip_distance > 2 & raw_data$trip_distance <= 3] <- dist_cat[3]
raw_data$trip_distance_cat[raw_data$trip_distance > 3 & raw_data$trip_distance <= 5] <- dist_cat[4]
raw_data$trip_distance_cat[raw_data$trip_distance > 5 & raw_data$trip_distance <= 7] <- dist_cat[5]
raw_data$trip_distance_cat[raw_data$trip_distance > 7 & raw_data$trip_distance <= 10] <- dist_cat[6]
raw_data$trip_distance_cat[raw_data$trip_distance > 10] <- dist_cat[7]


raw_data[,c("trip_mode", "trip_distance_cat")] <- lapply(raw_data[,c("trip_mode", "trip_distance_cat")], as.character)

# Create row_id columns for all trips
raw_data$row_id <- 1:nrow(raw_data)

raw_data[duplicated(raw_data$trip_id),]$trip_mode <- 'Short Walking'



# Create scenario 1: 50% of all trips walking trips (in each dist bracket) to Private Car
walking_trips <- subset(raw_data, trip_mode == "Walking")

raw_data$scen1_mode <- raw_data$trip_mode
raw_data$scen1_duration <- raw_data$trip_duration

# speeds <- list(bus.passenger=15,car.passenger=21,pedestrian=4.8,car=21,cyclist=14.5,motorcycle=25,tuktuk=22)

for (i in 1:length(dist_cat)){
  print(dist_cat[i])
  # i <- 2
  trips <- filter(walking_trips, trip_distance_cat == dist_cat[i]) 
  trips_sample <- trips %>% sample_frac(.5) %>% mutate(trip_mode = "Private Car")
  trips_sample$trip_duration <- (trips_sample$trip_distance / 4.8 ) * 21
  trips_sample$scen1_mode <- trips_sample$trip_mode
  trips_sample$scen1_duration <- trips_sample$trip_duration
  trips_sample <- select(trips_sample, row_id, scen1_mode, scen1_duration)
  print(nrow(trips_sample))
  raw_data[raw_data$row_id %in% trips_sample$row_id,]$scen1_mode <- trips_sample$scen1_mode
  raw_data[raw_data$row_id %in% trips_sample$row_id,]$scen1_duration <- trips_sample$scen1_duration
  
}


