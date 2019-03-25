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

#####
## Recalculate distances from speed when they're NA
# Read speed table for Delhi
speed_tbl <- read_csv("inst/extdata/local/delhi/speed_modes_india.csv")

# Update distance by duration (hours) * speed (kmh)
rd[is.na(rd$distance) & !is.na(rd$duration), ]$distance <- 
  (rd[is.na(rd$distance) & !is.na(rd$duration), ]$duration) * 
  speed_tbl$Speed[match(rd[is.na(rd$distance) & !is.na(rd$duration), ]$mode_name, speed_tbl$Mode)]

#####
# Modify raw dataset to expand based on household weights
# Add new ids for trips and persons

# Initialize all pid (new column for person_id) as -1
rd$pid <- -1
# Initialize all tid as -1
rd$tid <- -1
# Starting tid from 1
tid <- 1
# Starting id from 1
id <- 1
# Get unique person_ids
pid_list <- unique(rd$person_id)
#pid_list <- c("M00031")

saved_obj <- rd

#rd <- saved_obj %>% filter(person_id == "M00031")

# Loop through them and expand them using household weights
for(i in 1:length(pid_list)) {
  # print(i)
  # i <- 1
  # Filter by person_id to get all trips for a person
  pg <- filter(rd, person_id == pid_list[i])
  
  # Local var for p id
  local_pid <- pid_list[i]

  # If there are more than 1 trip and house hold weight for that person/trip is not NA
  if (!is.na(pg) && !is.na(pg$hh_weights)){
    # Get household weight (and round it)
    count <- round(pg$hh_weights[1])
    # Get unique trips count
    utrips <- length(unique(pg$trip_id))
    # If there is no weight (after rounding), assign a new id to that person
    if (count == 0 || count == 1){
      # Assign id (an auto-increasing id)
      rd[rd$person_id == pid_list[i],]$pid <- id
      # Increment id by 1
      id <- id + 1
      ## Increment by 1
      #tid <- tid + 1
      # Assing a new trip ID
      for (utid in unique(rd[rd$person_id == pid_list[i],]$trip_id)){
        # Identify total stages
        ustages <- rd %>% filter(trip_id == utid) %>% group_by(trip_id) %>% summarise(count = length(unique(stage))) %>% select(count) %>% as.integer()
        if (ustages > 0){
          # print("L82")
          a <- nrow(rd[rd$trip_id == utid,])
          b <- ustages
          if (a != b){
            cat(local_pid, " L82: rows ", a, " repeated by ", b, "\n")
          }
          # Update same trip id stages time
          rd$tid[rd$trip_id == utid] <- rep(tid, ustages)
          
        }else{
          rd$tid[rd$trip_id == utid] <- tid
        }
        # Increment tid by adding 1
        tid <- tid + 1
      }
    }
    # If household weight is greater than 1
    if (count > 1){
      # Replicate filtered dataset 'count' many times
      d <- bind_rows(replicate((count), pg, simplify = FALSE))
      # Sort it by trip id
      d <- arrange(d, trip_id)
      # Assign id for the selected person
      rd[rd$person_id == pid_list[i],]$pid <- id
      # Assing a new trip ID
      for (utid in unique(pg$trip_id)){
        ustages <- rd %>% filter(trip_id == utid) %>% group_by(trip_id) %>% summarise(count = length(unique(stage))) %>% select(count) %>% as.integer()
        # cat(utid, " - ", ustages, "\n")
        if (ustages > 0){
          a <- nrow(rd[rd$trip_id == utid,])
          b <- ustages
          if (a != b){
            cat(local_pid, " L111: rows ", a, " repeated by ", b, "\n")
          }
          # Update same trip id stages time
          rd$tid[rd$trip_id == utid] <- rep(tid, ustages)
        }else{
          rd$tid[rd$trip_id == utid] <- tid
        }
        # Increment tid by adding utrips to it
        tid <- tid + 1
      }
      
      # Generate new IDs for all newly generated persons (along with their trips)
      d$pid <- rep((id + 1):(id + count), nrow(pg))
      # s <- 1
      # e <- nrow(d)
      # d$urep <- -1
      # for (i in 1:count){
      #   d$urep[s:nrow(pg)]  
      # }
      
      # Assing a new trip ID
      for (utid in unique(d$trip_id)){
        ustages <- d %>% filter(trip_id == utid) %>% group_by(trip_id) %>% summarise(count = length(unique(stage))) %>% select(count) %>% as.integer()
        # cat(utid, " - ", ustages, "\n")
        if (ustages > 0){
          a <- nrow(d[d$trip_id == utid,]) / count
          b <- ustages
          if (a != b){
            cat(local_pid, " L128: rows ", a, " repeated by ", b, "\n")
          }
          
          stid <- tid
          etid <- tid + (ustages) - 1
          
          # Update same trip id stages time
          d$tid[d$trip_id == utid] <- rep(stid:etid, each = 2)
          tid <- etid
          #rep(tid, ustages)
        }else{
          d$tid[d$trip_id == utid] <- tid
        }
        # Increment tid by 1
        tid <- tid + 1
      }
      # Increment id by adding count to it
      id <- id + count
      # Rbind new trips to the overall dataset
      rd <- bind_rows(rd, d)
    }
    # If household weight is undefined
  }else{
    # Assign the new id
    rd[rd$person_id == pid_list[i],]$pid <- id
    # Increment by 1
    id <- id + 1
    ## Increment by 1
    #tid <- tid + 1
    # Assing a new trip ID
    for (utid in unique(rd[rd$person_id == pid_list[i],]$trip_id)){
      # filter rd for a person
      ustages <- rd %>% filter(trip_id == utid) %>% group_by(trip_id) %>% summarise(count = length(unique(stage))) %>% select(count) %>% as.integer()
      # cat(utid, " - ", ustages, "\n")
      
      if (ustages > 0){
        a <- nrow(rd[rd$trip_id == utid,])
        b <- ustages
        if (a != b){
          cat(local_pid, " L168: rows ", a, " repeated by ", b, "\n")
        }
        # Update same trip id stages time
        rd$tid[rd$trip_id == utid] <- rep(tid, ustages)
      }else{
        rd$tid[rd$trip_id == utid] <- tid
      }
      # Increment tid by adding utrips to it
      tid <- tid + 1
    }
    
  }
}

#####
# Column (vars) manipulations

# Replace trip_id by tid
rd$trip_id <- rd$tid
rd$tid <- NULL

# Replace person_id by pid
rd$person_id <- rd$pid
rd$pid <- NULL

# Rename person_id to participant_id
rd <- rename(rd, participant_id = person_id)

# Introduce sex column - and remove female column
rd$sex <- "Male"
rd$sex[rd$female == 1] <- "Female"
rd$female <- NULL

# Remove unused columns
# (hh_id)
rd$hh_id <- NULL

# Calculate total distance by summing all stages' distance
rd$total_distance <- ave(rd$distance, rd$trip_id, FUN=sum)

# save this in a local temp var
b <- rd

#####
## Rename columns

rd <- rd %>% rename(stage_mode_int = mode, stage_id = stage, stage_mode = mode_name, stage_distance = distance, stage_duration = duration, trip_mode_int = main_mode,
                     trip_mode = main_mode_name, trip_distance = total_distance)


#####
## Reorder columns
rd <- rd %>% select(participant_id, age, sex, hh_weights, stage_id, stage_mode_int, stage_mode, stage_duration, stage_distance,
                     trip_id, trip_mode_int, trip_mode, trip_distance)
  
#####
# Write streamlined travel survey data as a csv in the inst folder
write_csv(rd, "inst/extdata/local/delhi/trips.csv")