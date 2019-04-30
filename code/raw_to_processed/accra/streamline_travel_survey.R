# Clear workspace
rm (list = ls())

# Load libraries
library(tidyverse)
library(plotly)
library(ithimr)

# Read accra travel survey 
raw_trip_set <- read_csv("data/local/accra/trips_accra.csv")

# Convert all current modes to lower case
# Change 'Private Car' to 'car'
raw_trip_set$trip_mode[raw_trip_set$trip_mode == "Private Car"] <- "car"
raw_trip_set$trip_mode <- tolower(raw_trip_set$trip_mode)

# Copy trip mode to stage mode
raw_trip_set$stage_mode <- raw_trip_set$trip_mode

# Copy trip duration to stage duration
raw_trip_set$stage_duration <- raw_trip_set$trip_duration

# Make trip duration NULL
raw_trip_set$trip_duration <- NULL

# Convert participant_id to integer
raw_trip_set$participant_id <- as.integer(as.factor(raw_trip_set$participant_id))

## Add walk to bus stages to all bus trips
walk_to_bus <- raw_trip_set[raw_trip_set$stage_mode == "bus",]
walk_to_bus$stage_mode <- "walk_to_bus"
walk_to_bus$stage_duration <- 10.55

# Add walk to bus stage
raw_trip_set <- rbind(raw_trip_set, walk_to_bus)