# Clear workspace
rm (list = ls())

# Load libraries
library(tidyverse)
library(plotly)

# Read delhi travel survey with stages
raw_data <- read.csv("data/local/delhi/delhi_travel_survey.csv", stringsAsFactors = F)

#####
## Remove duplicate rows
raw_data <- raw_data[!duplicated(raw_data), ]

## Create temporary var for checking repeating trip ids and stage ids
raw_data$nid <- paste0(raw_data$trip_id, "_", raw_data$stage)

## Remove duplicated rows
raw_data <- raw_data[(!duplicated(raw_data$nid )) | (raw_data$nid == "NA_NA"),]

## Remove temporary var
raw_data$nid <- NULL

# Save read data in a separate var
rd <- raw_data

#####
## Remove duplicate rows for people without trips
rd_pwt <- rd %>% filter(!is.na(trip_id))

rd_pwot <- rd %>% filter(is.na(trip_id))

rd_pwot <- rd_pwot[!duplicated(rd_pwot$person_id), ]

rd <- rbind(rd_pwt, rd_pwot)

#####
## Assign a new trip id to people without trips
# Get unique number of trips
last_trip_id <- rd %>% filter(!is.na(trip_id)) %>% distinct(trip_id) %>% nrow()
# Identify new number of trip_id - based on unique person_id
ntrips <- rd[is.na(rd$trip_id), ] %>% nrow()
# Auto-increasing trip_id
rd[is.na(rd$trip_id), ]$trip_id <- seq(last_trip_id, last_trip_id + ntrips - 1, by = 1)

# Rename Short Walk to walk_to_pt
rd$mode_name[rd$main_mode_name != "Walk" & rd$mode_name %in% 'Walk'] <- "walk_to_pt"

rdpt <- rd %>% filter(main_mode_name %in% c('Bus', 'Metro', 'Rail', 'Share Auto'))

rdptww <- rdpt %>% filter(mode_name == "walk_to_pt")

rdpt <- rdpt %>% filter(!trip_id %in% rdptww$trip_id)

rdpt$mode_name <- "walk_to_pt"
rdpt$duration <- 10.55 / 60
rdpt$stage <- rdpt$stage + 1

rd <- rbind(rd, rdpt)

#####
## Recalculate distances from speed when they're NA
# Read speed table for Delhi
speed_tbl <- read_csv("data/local/delhi/speed_modes_india.csv")

speed_tbl[nrow(speed_tbl) + 1,] = list("walk_to_pt", 4.8)

# Update distance by duration (hours) * speed (kmh)
rd[is.na(rd$distance) & !is.na(rd$duration), ]$distance <- 
  (rd[is.na(rd$distance) & !is.na(rd$duration), ]$duration) * 
  speed_tbl$Speed[match(rd[is.na(rd$distance) & !is.na(rd$duration), ]$mode_name, speed_tbl$Mode)]

#####
# Expand by household IDs

rd <- arrange(rd, hh_id, person_id)
rd <- rd %>% mutate(w = if_else(is.na(hh_weights), 0, round(hh_weights)))
rd <- rd %>% mutate(w = if_else(w > 0, w - 1, w))
rd$hh_weights <- rd$w

exp1 <- rd %>% filter(w > 0) %>% uncount(w, .id = "pid")
exp2 <- rd %>% filter(w == 0) %>% mutate(pid = 1)

rd <- plyr::rbind.fill(exp1, exp2) %>% arrange(hh_id, person_id, trip_id)

rd$participant_id <- as.integer(as.factor(with(rd, paste(hh_id, person_id, pid, sep = "_"))))

rd$trip_id <- as.integer(as.factor(with(rd, paste(hh_id, person_id, pid, trip_id, sep = "_"))))


# save this in a local temp var
b <- rd

#####
# Column (vars) manipulations
# Introduce sex column - and remove female column
rd$sex <- "Male"
rd$sex[rd$female == 1] <- "Female"
rd$female <- NULL

# Remove unused columns
# (hh_id)
rd$hh_id <- NULL

# Calculate total distance by summing all stages' distance
rd$total_distance <- ave(rd$distance, rd$trip_id, FUN = function(x) sum(x, na.rm=T))

# Change unit of stage_duration from hours to mins
rd <- rd %>% mutate(duration = duration * 60)

#####
## Rename columns

rd <- rd %>% dplyr::rename(stage_mode_int = mode, stage_id = stage, stage_mode = mode_name, stage_distance = distance, stage_duration = duration, trip_mode_int = main_mode,
                     trip_mode = main_mode_name, trip_distance = total_distance)

# Calculate total duration by summing all stages' duration
rd$trip_duration <- ave(rd$stage_duration, rd$trip_id, FUN = function(x) sum(x, na.rm=T))


#####
## Reorder columns
rd <- rd %>% dplyr::select(participant_id, age, sex, hh_weights, stage_id, stage_mode_int, stage_mode, stage_duration, stage_distance,
                     trip_id, trip_mode_int, trip_mode, trip_distance, trip_duration)

#####
# Write streamlined travel survey data as a csv in the inst folder
write_csv(rd, "inst/extdata/local/delhi/trips_delhi.csv")