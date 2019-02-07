# Clear workspace
rm (list = ls())

# Load libraries
library(tidyverse)
library(plotly)

# Read delhi travel survey with stages
rd <- read_csv("data/local/delhi/delhi_travel_survey.csv")

# Initialize all pid (new column for person_id) as -1
rd$pid <- -1
# Initialize all tid as -1
rd$tid <- -1
# Starting tid from 1
tid <- 1
# Starting id from 1
id <- 1
# rd <- rd %>% filter(person_id == "S08544")
# Get unique person_ids
pid_list <- unique(rd$person_id)
# Loop through them and expand them using household weights
for(i in 1:length(pid_list)) {
  # i <- 1
  # Filter by person_id to get all trips for a person
  pg <- filter(rd, person_id == pid_list[i])
  # If there are more than 1 trip and house hold weight for that person/trip is not NA
  if (!is.na(pg) && !is.na(pg$hh_weights)){
    # Get household weight (and round it)
    count <- round(pg$hh_weights[1])
    # Get unique trips count
    utrips <- length(unique(pg$trip_id))
    # If there is no weight (after rounding), assign a new id to that person
    if (count == 0){
      # Assign id (an auto-increasing id)
      rd[rd$person_id == pid_list[i],]$pid <- id
      # Increment id by 1
      id <- id + 1
      # Increment by 1
      tid <- tid + 1
      # Assing a new trip ID
      for (utid in unique(rd[rd$person_id == pid_list[i],]$trip_id)){
        # Identify total stages
        ustages <- rd %>% filter(trip_id == utid) %>% group_by(trip_id) %>% summarise(count = length(unique(stage))) %>% select(count) %>% as.integer()
        # cat(utid, " - ", ustages, "\n")
        # Update same trip id stages time
        rd$tid[rd$trip_id == utid] <- rep(tid, ustages)
        # Increment tid by adding 1
        tid <- tid + 1
      }
    }
    # If household weight is greater than 0
    if (count > 0){
      # Replicate filtered dataset 'count' many times
      d <- bind_rows(replicate((count), pg, simplify = FALSE))
      # Sort it by trip id
      d <- arrange(d, trip_id)
      # Assign id for the selected person
      rd[rd$person_id == pid_list[i],]$pid <- id
      # Assing a new trip ID
      for (utid in unique(pg$trip_id)){
        ustages <- rd %>% filter(trip_id == utid) %>% group_by(trip_id) %>% summarise(count = length(unique(stage))) %>% select(count) %>% as.integer()
        cat(utid, " - ", ustages, "\n")
        rd$tid[rd$trip_id == utid] <- rep(tid, ustages)
        # Increment tid by adding utrips to it
        tid <- tid + 1
      }
      
      # Generate new IDs for all newly generated persons (along with their trips)
      d$pid <- rep((id + 1):(id + count), nrow(pg))
      
      # Assing a new trip ID
      for (utid in unique(d$trip_id)){
        ustages <- d %>% filter(trip_id == utid) %>% group_by(trip_id) %>% summarise(count = length(unique(stage))) %>% select(count) %>% as.integer()
        # cat(utid, " - ", ustages, "\n")
        # Update same trip id stages time
        d$tid[d$trip_id == utid] <- rep(tid, ustages)
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
    # Increment by 1
    tid <- tid + 1
    # Assing a new trip ID
    for (utid in unique(rd[rd$person_id == pid_list[i],]$trip_id)){
      # filter rd for a person
      ustages <- rd %>% filter(trip_id == utid) %>% group_by(trip_id) %>% summarise(count = length(unique(stage))) %>% select(count) %>% as.integer()
      # cat(utid, " - ", ustages, "\n")
      # Update same trip id stages time
      rd$tid[rd$trip_id == utid] <- rep(tid, ustages)
      # Increment tid by adding utrips to it
      tid <- tid + 1
    }
    
  }
}
