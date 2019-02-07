# Clear workspace
rm (list = ls())

# Load libraries
library(tidyverse)
library(plotly)

# Read delhi travel survey with stages
rd <- read_csv("data/local/delhi/delhi_travel_survey.csv")

# Initialize all pid (new column for person_id) as -1
rd$pid <- -1
# Starting id from 1
id <- 1
# Get unique person_ids
pid_list <- unique(rd$person_id)
# Loop through them and expand them using household weights
for(i in 1:length(pid_list)) {
  # Filter by person_id to get all trips for a person
  pg <- filter(rd, person_id == pid_list[i])
  # If there are more than 1 trip and house hold weight for that person/trip is not NA
  if (!is.na(pg) && !is.na(pg$hh_weights)){
    # Get household weight (and round it)
    count <- round(pg$hh_weights[1])
    # If there is no weight (after rounding), assign a new id to that person
    if (count == 0){
      # Assign id (an auto-increasing id)
      rd[rd$person_id == pid_list[i],]$pid <- id
      # Increment id by 1
      id <- id + 1
    }
    # If household weight is greater than 0
    if (count > 0){
      # Replicate filtered dataset 'count' many times
      d <- bind_rows(replicate((count), pg, simplify = FALSE))
      # Sort it by trip id
      d <- arrange(d, trip_id)
      # Assign id for the selected person
      rd[rd$person_id == pid_list[i],]$pid <- id
      # Generate new IDs for all newly generated persons (along with their trips)
      d$pid <- rep((id + 1):(id + count), nrow(pg))
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
  }
}
