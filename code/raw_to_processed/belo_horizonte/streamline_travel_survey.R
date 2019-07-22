# Clear workspace
rm (list = ls())

# Load libraries
library(tidyverse)
library(summarytools)

# Read belo horizonte's travel survey
rd <- read_csv("data/local/belo_horizonte/trips_dataset_LT_170719.csv")

# Remove unused column
rd$X1 <- NULL

# Rename columns
rd <- rd %>% rename(participant_id = id_person,
                    age = Idade,
                    sex = female,
                    trip_id = id_trip,
                    stage_mode = mode_eng,
                    stage_duration = trip_duration
                    )

# Convert factor to appropriate classes
rd <- mutate(rd, participant_id = as.integer(as.factor(participant_id)),
             sex = ifelse(sex == 1, "Female", "Male"),
             trip_id = as.integer(as.factor(trip_id)))

# Show mode share (%)
rd %>% group_by(stage_mode) %>% summarise(count = n(), `share (%)` = (n() / nrow(.) * 100) %>% round(2))

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
