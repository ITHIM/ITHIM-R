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

# Copy trip mode to stage mode
raw_trip_set$stage_mode <- raw_trip_set$trip_mode

# Copy trip duration to stage duration
raw_trip_set$stage_duration <- raw_trip_set$trip_duration

# Make trip duration NULL
raw_trip_set$trip_duration <- NULL

# Convert participant_id to integer
raw_trip_set$participant_id <- as.integer(as.factor(raw_trip_set$participant_id))

# ## Add walk to bus stages to all bus trips
# walk_to_bus <- raw_trip_set[raw_trip_set$stage_mode %in% c('bus', 'train'),]
# walk_to_bus$stage_mode <- "pedestrian"
# walk_to_bus$stage_duration <- 10.55

# Add walk to bus stage
raw_trip_set <- rbind(raw_trip_set, walk_to_bus)

# Redefine motorcycle mode for a select 14 rows
raw_trip_set$trip_mode[!is.na(raw_trip_set$trip_mode) & raw_trip_set$trip_mode == 'other'][1:14] <- 'motorcycle'
raw_trip_set <- subset(raw_trip_set, is.na(trip_mode) | trip_mode !='other')

raw_trip_set$stage_mode[!is.na(raw_trip_set$stage_mode) & raw_trip_set$stage_mode == 'other'][1:14] <- 'motorcycle'
raw_trip_set <- subset(raw_trip_set, is.na(stage_mode) | stage_mode !='other')

source("code/producing_trips_rd/used_functions.R")

# Standardized travel modes
raw_trip_set <- standardize_modes(raw_trip_set, mode = c('stage', 'trip'))

default_speeds <- list(
  bus=10.8,
  bus_driver=10.8,
  minibus=10.8,
  minibus_driver=10.8,
  car=11.3,
  taxi=11.3,
  pedestrian=3.5,
  walk_to_pt=3.5,
  cycle=7,
  motorcycle=12,
  truck=10.8,
  van=10.8,
  subway=12.9,
  rail=13.5,
  auto_rickshaw=8,
  shared_auto=11.3,
  shared_taxi=11.3,
  cycle_rickshaw=5.3
)

# Create travel modes with all default speeds
TRAVEL_MODES <- tolower(names(default_speeds))

# Create df with speed for each travel mode
MODE_SPEEDS <- data.frame(stage_mode = TRAVEL_MODES, speed = unlist(default_speeds), stringsAsFactors = F)

# Assign stage_speed according to travel mode
stage_speed <- sapply(raw_trip_set$stage_mode,function(x){speed <- MODE_SPEEDS$speed[MODE_SPEEDS$stage_mode==x]; ifelse(length(speed)==0,0,speed)})

## if distance but no duration, add duration
## duration = distance / speed * 60
if('stage_distance'%in%colnames(raw_trip_set)&&!'stage_duration'%in%colnames(raw_trip_set))
  raw_trip_set$stage_duration <- raw_trip_set$stage_distance / stage_speed * 60
## if duration but no distance, add distance
## distance = speed * duration / 60
if('stage_duration'%in%colnames(raw_trip_set)&&!'stage_distance'%in%colnames(raw_trip_set))
  raw_trip_set$stage_distance <- raw_trip_set$stage_duration * stage_speed / 60
## depending on the situation there might be other (faster) ways to compute trip_distance,
## e.g. as a function of trip duration or it might just be the same as stage_distance but
## to allow for all eventualities we just sum the stages of each trip.
if(!'trip_distance'%in%colnames(raw_trip_set))
  raw_trip_set$trip_distance <- sapply(raw_trip_set$trip_id,function(x)sum(subset(raw_trip_set,trip_id==x)$stage_distance))

# Define constants
DISTANCE_SCALAR_CAR_TAXI <- 1
MOTORCYCLE_TO_CAR_RATIO <- 0.2

total_car_distance <- sum(subset(raw_trip_set,stage_mode=='car')$trip_distance)*DISTANCE_SCALAR_CAR_TAXI

# Create new motorcycle trips
# Add 4 new people with 3 trips each
# Age: 15-59 and gender: male
new_mode <- 'motorcycle'
total_mc_distance <- total_car_distance*MOTORCYCLE_TO_CAR_RATIO
residual_mc_distance <- total_mc_distance - sum(subset(raw_trip_set,stage_mode==new_mode)$trip_distance)
#duration_range <- 15:100
nTrips <- 1
nPeople <- 20
distance <- residual_mc_distance/nPeople
new_gender <- c(rep('Male',20),'Female')
age_range <- 15:69
speed <- MODE_SPEEDS$speed[MODE_SPEEDS$stage_mode==new_mode]

td <- raw_trip_set

for(i in 1:nPeople){
  new_trips <- add_trips(trip_ids   = max(raw_trip_set$trip_id) + 1: nTrips, 
                         new_mode = new_mode, 
                         distance = distance, 
                         participant_id = max(raw_trip_set$participant_id) + 1,
                         age = age_range,
                         sex = new_gender,
                         nTrips=nTrips,
                         speed=speed)
  # Add new motorcycles trips to baseline
  # print(summary(new_trips$trip_distance))
  raw_trip_set <- plyr::rbind.fill(raw_trip_set, new_trips)
}

## Apply census weights to accra trip set at person level

raw_trip_set$age_cat <- ""

raw_trip_set[raw_trip_set$age >= 15 & raw_trip_set$age < 50,]$age_cat <- '15-49'
raw_trip_set[raw_trip_set$age > 49 & raw_trip_set$age < 70,]$age_cat <- '50-69'

raw_trip_set <- arrange(raw_trip_set, trip_id)

pop_weights <- read_csv("data/local/accra/census_weights.csv")

backup <- raw_trip_set

for (i in 1:nrow(pop_weights)){
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