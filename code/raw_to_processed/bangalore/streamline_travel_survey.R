# Clear workspace
rm (list = ls())

# Load libraries
library(tidyverse)
library(plotly)

# Read bangalore travel survey with stages
raw_data <- read.csv("data/local/bangalore/bangalore_travel_survey_2011_March_14.csv", stringsAsFactors = F)

#####
## Remove duplicate rows
raw_data <- raw_data[!duplicated(raw_data), ]

## Remove duplicated rows
raw_data <- raw_data[(!duplicated(raw_data$uniqueid )) | (raw_data$uniqueid == "NA_NA"),]

## Remove uniqueid
raw_data$uniqueid <- NULL

# Save read data in a separate var
rd <- raw_data

#####
## Remove duplicate rows for people without trips
rd_pwt <- rd %>% filter(!is.na(trip_id))

rd_pwot <- rd %>% filter(is.na(trip_id))

rd_pwot <- rd_pwot[!duplicated(rd_pwot$person_id), ]

rd <- rbind(rd_pwt, rd_pwot)

#####
#Convert trip id to numeric
rd$trip_id <- as.numeric(as.factor(rd$trip_id))

#####
## Assign a new trip id to people without trips
# Get unique number of trips
last_trip_id <- rd %>% filter(!is.na(trip_id)) %>% distinct(trip_id) %>% nrow()
# Identify new number of trip_id - based on unique person_id
ntrips <- rd[is.na(rd$trip_id), ] %>% nrow()
# Auto-increasing trip_id
rd[is.na(rd$trip_id), ]$trip_id <- seq(last_trip_id, last_trip_id + ntrips - 1, by = 1)

#####
## Recalculate distances from speed when they're NA
# Read speed table for Bangalore
speed_tbl <- read_csv("inst/extdata/local/bangalore/speed_modes_india.csv")

# Update distance by duration (hours) * speed (kmh)
rd[is.na(rd$distance) & !is.na(rd$duration), ]$distance <- 
  (rd[is.na(rd$distance) & !is.na(rd$duration), ]$duration) * 
  speed_tbl$Speed[match(rd[is.na(rd$distance) & !is.na(rd$duration), ]$mode_name, speed_tbl$Mode)]