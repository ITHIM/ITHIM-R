# Clear workspace
rm (list = ls())

# Load libraries
library(tidyverse)
library(plotly)
library(ithimr)

# Read accra travel survey 
raw_trip_set <- read_csv("data/local/accra/accra_trip.csv")

# Remove already added motorcycle trips

# raw_trip_set <- filter()

# # Convert all current modes to lower case
# # Change 'Private Car' to 'car'
# raw_trip_set$trip_mode[raw_trip_set$trip_mode == "Private Car"] <- "car"
# raw_trip_set$trip_mode <- tolower(raw_trip_set$trip_mode)

# Convert participant_id to integer
raw_trip_set$participant_id <- as.integer(as.factor(raw_trip_set$participant_id))

# ## Add walk to bus stages to all bus trips
# walk_to_bus <- raw_trip_set[raw_trip_set$stage_mode %in% c('bus', 'train'),]
# walk_to_bus$stage_mode <- "pedestrian"
# walk_to_bus$stage_duration <- 10.55

# Add walk to bus stage
# raw_trip_set <- rbind(raw_trip_set, walk_to_bus)

# Redefine motorcycle mode for a select 14 rows
raw_trip_set$trip_mode[!is.na(raw_trip_set$trip_mode) & raw_trip_set$trip_mode == 'other'][1:14] <- 'motorcycle'
raw_trip_set <- subset(raw_trip_set, is.na(trip_mode) | trip_mode !='other')

## Apply census weights to accra trip set at person level

raw_trip_set$age_cat <- ""

raw_trip_set[raw_trip_set$age >= 15 & raw_trip_set$age < 50,]$age_cat <- '15-49'
raw_trip_set[raw_trip_set$age > 49 & raw_trip_set$age < 70,]$age_cat <- '50-69'

raw_trip_set <- arrange(raw_trip_set, trip_id)

pop_weights <- read_csv("data/local/accra/census_weights.csv")

backup <- raw_trip_set

for (i in 1:nrow(pop_weights)){
  # i <- 1
  td <- raw_trip_set[raw_trip_set$age_cat == pop_weights$age[i] & raw_trip_set$sex == pop_weights$sex[i],]
  
  print(td %>% group_by(trip_mode) %>% summarise(p = round(dplyr::n() / length(unique(td$trip_id)) * 100, 1) ))
  
  n <- pop_weights$rweights[i]
  for (j in 1:n){
    last_id <- max(raw_trip_set$participant_id)
    last_trip_id <- max(raw_trip_set$trip_id)
    
    # print(last_id)
    # print(last_trip_id)
    td1 <- td
    td1$participant_id <- td1$participant_id - 1 + last_id
    td1$trip_id <- td1$trip_id + last_trip_id
    raw_trip_set <- rbind(raw_trip_set, td1)
    
  }
  
  
  # td1$participant_id <- td1$participant_id + max(raw_trip_set$participant_id)
  # td1$trip_id <- (max(raw_trip_set$trip_id) + 1): (max(raw_trip_set$trip_id) + nrow(td1))
  # 
  # raw_trip_set <- rbind(raw_trip_set, td1)
  
}

raw_trip_set$age_cat <- NULL

#####
# Write streamlined travel survey data as a csv in the inst folder
write_csv(raw_trip_set, "data/local/accra/accra_trip_with_mbike.csv")