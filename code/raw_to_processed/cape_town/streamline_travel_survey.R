# Clear workspace
rm (list = ls())

# Load libraries
library(tidyverse)
library(summarytools)

# Read capetown's travel survey
raw_rd <- read_csv("data/local/cape_town/cape_town_trip.csv")

# Replace train by rail
raw_rd[raw_rd$trip_mode == 'train' & !is.na(raw_rd$trip_mode),]$trip_mode <- 'rail'

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

# Copy single trip info to stages
rd$stage_duration <- rd$trip_duration
rd$stage_mode <- rd$trip_mode

# Get distinct trips without any walk elements
distinct_rdpt <- rd %>% filter(!is.na(trip_id) & trip_mode %in% c('bus', 'train') ) %>% distinct(trip_id, .keep_all = T)

# Add walk_to_pt with default values
distinct_rdpt$stage_mode <- "walk_to_pt"
distinct_rdpt$stage_duration <- 10.55
distinct_rdpt$stage_id <- distinct_rdpt$stage_id + 1

# Add pt trips
rd <- plyr::rbind.fill(rd, distinct_rdpt)

# Arrange by trip_id
rd <- arrange(rd, trip_id)

#####
# Write streamlined travel survey data as a csv in the inst folder
write_csv(rd, "inst/extdata/local/cape_town/trips_cape_town.csv")