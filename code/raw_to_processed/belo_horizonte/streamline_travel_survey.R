# Clear workspace
rm (list = ls())

# Load libraries
library(tidyverse)
library(summarytools)

# Read belo horizonte's travel survey
raw_rd <- read_csv("data/local/belo_horizonte/belo_horizonte_trip.csv")

raw_rd$trip_id <- as.integer(as.factor(with(raw_rd, paste0(cluster_id, household_id, participant_id, trip_id, sep = "_"))))

# Remove unused column
rd <- raw_rd %>% dplyr::select(-c(X1, cluster_id, household_id, participant_wt, year, trip_purpose))

# Convert id to numeric
rd$participant_id <- as.numeric(as.factor(rd$participant_id))

rd <- arrange(rd, participant_id)

rd$stage_mode <- rd$trip_mode

# Rename columns
rd <- rd %>% rename(stage_duration = trip_duration)
# Show mode share (%)
rd %>% filter(!is.na(stage_mode)) %>% group_by(stage_mode) %>% summarise(count = n(), `share (%)` = (n() / nrow(.) * 100) %>% round(2))

# Show mode duration (minutes)
rd %>% group_by(stage_mode) %>% summarise(mean_duration = mean(stage_duration) %>% round(2))

# View summary of people without any trips
view(dfSummary(rd %>% filter(is.na(trip_id))))

# Add stage_id
rd <- rd %>% mutate(stage_id = 1)

# Select pt trips
rd_pt <- rd %>% filter(stage_mode %in% c('bus', 'metro'))

# Add short walks 
rd_pt_short_walk <- rd_pt %>% mutate(stage_mode = "walk_to_pt", stage_id = 2, stage_duration = 10.55)

rd <- plyr::rbind.fill(rd, rd_pt_short_walk)

# Calculate trip distance by summing all stages' distance
rd$trip_duration <- ave(rd$stage_duration, rd$trip_id, FUN=sum)

rd <- arrange(rd, trip_id)

# Write csv
write_csv(rd, "inst/extdata/local/belo_horizonte/trips_belo_horizonte.csv")
