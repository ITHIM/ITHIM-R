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
