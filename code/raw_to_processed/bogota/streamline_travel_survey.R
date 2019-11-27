# Clear workspace
rm (list = ls())

# Load libraries
library(tidyverse)
library(summarytools)

# Read belo horizonte's travel survey
raw_rd <- read_csv("data/local/bogota/bogota_trip_original.csv")

raw_rd <- raw_rd %>% mutate(id = ifelse(!is.na(trip_mode), as.integer(as.factor(with(raw_rd, paste0(cluster_id,household_id,participant_id, trip_id, trip_mode, trip_duration)))), NA))

raw_rd$trip_id <- raw_rd$id

raw_rd$id <- NULL