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


