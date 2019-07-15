# Clear workspace
rm (list = ls())

# Load libraries
library(tidyverse)
library(summarytools)

# Read belo horizonte's travel survey
rd <- read_csv("data/local/belo_horizonte/trips_belo_horizonte.csv")

# Remove unused column
rd$X1 <- NULL

# Rename columns
rd <- rd %>% rename(participant_id = id_person,
                    age = Idade,
                    sex = female,
                    trip_id = id_trip,
                    stage_mode = mode_eng
                    )

# Convert factor to appropriate classes
rd <- mutate(rd, participant_id = as.integer(as.factor(participant_id)),
             sex = ifelse(sex == 1, "Female", "Male"),
             trip_id = as.integer(as.factor(trip_id)))

# Show mode share (%)
rd %>% group_by(stage_mode) %>% summarise(share = (n() / nrow(.) * 100) %>% round(2))

# Show mode duration (minutes)
rd %>% group_by(stage_mode) %>% summarise(mean_duration = mean(trip_duration) %>% round(2))

# View summary of people without any trips
view(dfSummary(rd %>% filter(is.na(trip_id))))
