# Clear workspace
rm (list = ls())

# Load libraries
library(tidyverse)
library(plotly)

# Read bangalore travel survey with stages
raw_data <- read.csv("data/local/bangalore/bangalore_travel_survey_April2019_post_cleaning.csv", stringsAsFactors = F)

# Remove ind with incorrect sex or age values
raw_data <- filter(raw_data, sex %in% c("male","female") & (!is.na(age)))

# Convert person_id to numeric
raw_data$person_id <- as.numeric(as.factor(raw_data$person_id))

#####
## Rename columns
raw_data <- rename(raw_data, stage_distance = Distance, stage_duration = Time, stage_id = Stage, stage_mode = mode)


#####
## Remove duplicate rows
raw_data <- raw_data[!duplicated(raw_data), ]

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
# Rename Short Walk to walk_to_pt
rd$stage_mode[rd$stage_mode != "Walk" & rd$main_mode != 1] <- "walk_to_pt"

#####
# Copy stage mode to trip mode
rd$trip_mode <- rd$stage_mode
# Create unique trip ids
utripid <- unique(rd$trip_id)

# For each trip, copy main_mode to trip_mode if it is 1
for (i in 1:length(utripid)){
  tid <- utripid[i]
  mmode <- rd$stage_mode[rd$trip_id == tid & rd$main_mode == 1]
  if (length(mmode) > 0)
    rd$trip_mode[rd$trip_id == tid] <- mmode
  
}

td <- rd

rdpt <- rd %>% filter(trip_mode %in% c('bus', 'mini_bus', 'shared_auto', 'train'))

rdptww <- rdpt %>% filter(stage_mode == "walk_to_pt")

rdpt <- rdpt %>% filter(!trip_id %in% rdptww$trip_id)

rdpt$stage_mode <- "walk_to_pt"
rdpt$stage_duration <- 10.55 / 60
rdpt$stage_id <- rdpt$stage_id + 1

rd <- rbind(rd, rdpt)


#####
## Recalculate distances from speed when they're NA
# Read speed table for Bangalore
speed_tbl <- read_csv("data/local/bangalore/speed_modes_india.csv")

speed_tbl[nrow(speed_tbl) + 1,] = list("walk_to_pt", 4.8)

if (nrow(rd[is.na(rd$stage_distance) & !is.na(rd$stage_duration), ]) > 0){
  # Update stage_distance by stage_duration (mins / 60) * speed (kmh)
  rd[is.na(rd$stage_distance) & !is.na(rd$stage_duration), ]$stage_distance <- 
    ((rd[is.na(rd$stage_distance) & !is.na(rd$stage_duration), ]$stage_duration) / 60) * 
    speed_tbl$Speed[match(rd[is.na(rd$stage_distance) & !is.na(rd$stage_duration), ]$stage_mode, speed_tbl$Mode)]
}

# Remove row number column
rd$X <- NULL
rd$main_mode <- NULL

# Rename person_id to participant_id
rd <- rename(rd, participant_id = person_id)

# Set sex case
rd$sex[rd$sex == "male"] <- "Male"
rd$sex[rd$sex == "female"] <- "Female"

# Calculate trip distance by summing all stages' distance
rd$trip_distance <- ave(rd$stage_distance, rd$trip_id, FUN=sum)

#####
## Reorder columns
rd1 <- rd %>% dplyr::select(participant_id, age, sex, stage_id, stage_mode, stage_duration, stage_distance,
                            trip_id, trip_mode, trip_distance)

####
# Remove trips with only walk_to_pt as main mode
# Explicity keep trip_mode with NA - otherwise the command will remove them
rd1 <- rd1 %>% filter(is.na(trip_mode) | trip_mode != "walk_to_pt")

#####
# Write streamlined travel survey data as a csv in the inst folder
write_csv(rd1, "inst/extdata/local/bangalore/trips_bangalore.csv")