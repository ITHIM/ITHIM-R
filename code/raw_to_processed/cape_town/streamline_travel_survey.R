# Clear workspace
rm (list = ls())

# Load libraries
library(tidyverse)
library(summarytools)

# Read capetown's travel survey
raw_rd <- read_csv("data/local/cape_town/cape_town_trip.csv")

rd <- raw_rd

rd %>% filter(!is.na(trip_id)) %>% distinct(trip_id, .keep_all = TRUE) %>% group_by(trip_mode, .drop = F) %>% summarise(mode_share = round(n()*100/nrow(.),1))

# Create a new var id to represent unique trips
rd <- rd %>% mutate(id = ifelse(!is.na(trip_mode), as.integer(as.factor(with(rd, paste0(cluster_id,household_id,participant_id, trip_id, trip_mode, trip_duration)))), NA))

# Replace trip id with id
rd$trip_id <- rd$id

rd <- arrange(rd, trip_id)

# Create a temporary id for participant_id
rd$pid <- as.factor(with(rd, paste0(cluster_id, household_id, participant_id)))

# Convert factor to integer
rd$pid <- as.integer(rd$pid)

# Replace participant_id with new id
rd$participant_id <- rd$pid

# Remove new id
rd$pid <- NULL

# Assign auto-increasing stage_id
rd$stage_id <- sequence(rle(rd$trip_id)$lengths)

# Remove unused column
rd <- rd %>% dplyr::select(-c(X1, cluster_id, household_id, participant_wt, year, trip_purpose, id))
