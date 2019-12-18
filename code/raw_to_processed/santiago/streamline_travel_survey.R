# Clear workspace
rm (list = ls())

# Load libraries
library(tidyverse)
library(summarytools)

# Read santiago's travel survey
raw_rd <- read_csv("data/local/santiago/santiago_trip.csv")

# Create a new var id to represent unique trips
raw_rd <- raw_rd %>% mutate(id = ifelse(!is.na(trip_mode), as.integer(as.factor(with(raw_rd, paste0(cluster_id,household_id,participant_id, trip_id, trip_mode, trip_duration)))), NA))

# Replace trip id with id
raw_rd$trip_id <- raw_rd$id

raw_rd <- arrange(raw_rd, trip_id)

# Make id null
raw_rd$id <- NULL

# Create a temporary id for participant_id
raw_rd$pid <- as.factor(with(raw_rd, paste0(cluster_id, household_id, participant_id)))

# Convert factor to integer
raw_rd$pid <- as.integer(raw_rd$pid)

# Replace participant_id with new id
raw_rd$participant_id <- raw_rd$pid

# Remove new id
raw_rd$pid <- NULL

# Create a local copy of raw df
rd <- raw_rd

# Assign auto-increasing stage_id
rd$stage_id <- sequence(rle(rd$trip_id)$lengths)

# Remove unused column
rd <- rd %>% dplyr::select(-c(X1, cluster_id, household_id, participant_wt, year, trip_purpose))

# Print summary
rd %>% filter(!is.na(trip_id)) %>% distinct(trip_id, .keep_all = TRUE) %>% group_by(trip_mode, .drop = F) %>% summarise(mode_share = round(n()*100/nrow(.),1))

# Get public transport modes
rdpt <- rd %>% filter(trip_mode %in% c('bus', 'train', 'metro'))

# # Rename walk to walk_to_pt
# rdpt[rdpt$stage_mode == "walk",]$stage_mode <- "walk_to_pt"

#distinct_rdpt <- rdpt %>% distinct(trip_id, .keep_all = T)

## Filter trips with walk elements
#rdptww <- rdpt %>% filter(stage_mode == "walk_to_pt")

# Get distinct trips without any walk elements
distinct_rdpt <- rdpt %>% distinct(trip_id, .keep_all = T)

# Add walk_to_pt with default values
distinct_rdpt$stage_mode <- "walk_to_pt"
distinct_rdpt$stage_duration <- 10.55
distinct_rdpt$stage_id <- distinct_rdpt$stage_id + 1

# Add walk elements to the missing pt trips
rdpt <- plyr::rbind.fill(rdpt, distinct_rdpt)

# Add pt trips
rd <- plyr::rbind.fill(rd %>% filter(!trip_mode %in% c('bus', 'train', 'metro') | is.na(trip_mode)), rdpt)

# Add trip duration to stages
rd[rd$stage_id == 1,]$stage_duration <- rd[rd$stage_id == 1,]$trip_duration

# Recalculate trip duration by adding walk stage duration
rd$trip_duration <- ave(rd$stage_duration, rd$trip_id, FUN = function(x) sum(x, na.rm=T))

# Rename walk to walk_to_pt for pt modes
#rd[rd$trip_mode %in% c("bus", "train", 'metro') && rd$stage_mode == "walk",] <- "walk_to_pt"

# Arrange df
rd <- arrange(rd, participant_id, trip_id, stage_id)

#####
# Write csv
write_csv(rd, "inst/extdata/local/santiago/trips_santiago.csv")
