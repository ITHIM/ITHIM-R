# Clear workspace
rm (list = ls())

# Load libraries
library(tidyverse)
library(summarytools)

# Read belo horizonte's travel survey
raw_rd <- read_csv("data/local/mexico/mexico_city_trip.csv")

# convert age to integer
raw_rd$age <- as.integer(raw_rd$age)

# Create a new var id to represent unique trips
raw_rd <- raw_rd %>% mutate(id = ifelse(!is.na(trip_mode), as.integer(as.factor(with(raw_rd, paste0(cluster_id,household_id,participant_id, trip_id, trip_mode, trip_duration)))), NA))

# Replace trip id with id
raw_rd$trip_id <- raw_rd$id

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

# Remove unused column
rd <- rd %>% dplyr::select(-c(X1, cluster_id, household_id, participant_wt, year, trip_purpose))

# Print summary
rd %>% filter(!is.na(trip_id)) %>% distinct(trip_id, .keep_all = TRUE) %>% group_by(trip_mode, .drop = F) %>% summarise(mode_share = round(n()*100/nrow(.),1))

# Get public transport modes
rdpt <- rd %>% filter(trip_mode %in% c('bus', 'train', 'metro'))

# Rename walk to walk_to_pt
rdpt[rdpt$stage_mode == "walk",]$stage_mode <- "walk_to_pt"

#distinct_rdpt <- rdpt %>% distinct(trip_id, .keep_all = T)

# Filter trips with walk elements
rdptww <- rdpt %>% filter(stage_mode == "walk_to_pt")

# Get distinct trips without any walk elements
distinct_rdpt <- rdpt %>% filter(!trip_id %in% rdptww$trip_id) %>% distinct(trip_id, .keep_all = T)

# Add walk_to_pt with default values
distinct_rdpt$stage_mode <- "walk_to_pt"
distinct_rdpt$stage_duration <- 10.55
distinct_rdpt$stage_id <- distinct_rdpt$stage_id + 1

# Add walk elements to the missing pt trips
rdpt <- rbind(rdpt, distinct_rdpt)

# Add pt trips
rd <- rbind(rd %>% filter(!trip_mode %in% c('bus', 'train', 'metro') | is.na(trip_mode)), rdpt)

# Rename walk to walk_to_pt for pt modes
rd$stage_mode[rd$trip_mode %in% c("bus", "train") & rd$stage_mode == "walk"] <- "walk_to_pt"

# Arrange df
rd <- arrange(rd, participant_id, trip_id, stage_id)

#####
# Write csv
write_csv(rd, "inst/extdata/local/mexico_city/trips_mexico_city.csv")
