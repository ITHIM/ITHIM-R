# Clear workspace
rm (list = ls())

# Load libraries
library(tidyverse)
library(summarytools)

# Read visakhapatnam's travel survey
raw_rd <- read_csv("data/local/vizag/visakhapatnam_trips.csv")

# Expand dataset by wk_freq
# Add trip and stage id
rd <- raw_rd %>% mutate(wk_freq = if_else(is.na(wk_freq), 1, wk_freq)) %>% uncount(wk_freq) %>% mutate(trip_id = as.integer(rownames(.)))

# Remove extra columns
rd$X1 <- NULL

rd <- rename(rd, participant_id = ind_id, trip_mode = mode, trip_distance = distance, trip_duration = travel_time) %>% mutate(participant_id = as.integer(as.factor(participant_id)))

rd$stage_id <- 1

# Copy trip mode to stage
rd$stage_mode <- rd$trip_mode

# copy trip distance and duration to stage
rd$stage_duration <- rd$trip_duration
rd$stage_distance <- rd$trip_distance

# Print mode share
rd %>% filter(!is.na(trip_mode)) %>% group_by(trip_mode, .drop = F) %>% summarise(`mode_share (%)` = round(n()*100/nrow(.),1))

# Get public transport modes
distinct_rdpt <- rd %>% filter(trip_mode %in% c('bus', 'shared_autorickshaw'))

# Add walk_to_pt with default values
distinct_rdpt$stage_mode <- "walk_to_pt"
distinct_rdpt$stage_duration <- 10.55
distinct_rdpt$stage_id <- distinct_rdpt$stage_id + 1

# Add pt trips
rd <- plyr::rbind.fill(rd %>% filter(!trip_mode %in% c('bus', 'train', 'metro') | is.na(trip_mode)), distinct_rdpt)