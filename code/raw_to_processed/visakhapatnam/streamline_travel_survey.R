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
distinct_rdpt$stage_distance <- distinct_rdpt$stage_duration * 4.8 /60
distinct_rdpt$stage_id <- distinct_rdpt$stage_id + 1

# Add pt trips
rd <- plyr::rbind.fill(rd, distinct_rdpt)

# Arrange by trip_id
rd <- arrange(rd, trip_id)

# Recalculate trip duration by adding walk stage duration
rd$trip_duration <- ave(rd$stage_duration, rd$trip_id, FUN = function(x) sum(x, na.rm=T))

# Recalculate trip duration by adding walk stage duration
rd$trip_distance <- ave(rd$stage_distance, rd$trip_id, FUN = function(x) sum(x, na.rm=T))

# Fix gender
rd$sex[rd$sex == "F"] <- "Female"
rd$sex[rd$sex == "M"] <- "Male"


#####
# Write streamlined travel survey data as a csv in the inst folder
write_csv(rd, "inst/extdata/local/vizag/trips_vizag.csv")