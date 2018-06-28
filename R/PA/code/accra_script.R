# Remove everything
rm (list = ls())
# Load packages
library(tidyverse)
library(haven)
library(plotly)

# set seed
set.seed(1)

# Read travel survey data
raw_data <- readRDS("data/synth_pop_data/accra/travel_survey/accra_processed_trips.Rds")

# Redefine non-factor based column classes
raw_data[,c("trip_id", "participant_id", "age")] <- lapply(raw_data[,c("trip_id", "participant_id", "age")], as.numeric)
raw_data[,c("trip_duration", "trip_distance")] <- lapply(raw_data[,c("trip_duration", "trip_distance")], as.double)

# Define distance categories
dist_cat <- c("0-1 km", "1-2 km", "2-3 km", "3-5 km", "5-7 km", "7-10 km", ">10 km" )

# Initialize them
raw_data$trip_distance_cat[raw_data$trip_distance > 0 & raw_data$trip_distance <= 1] <- dist_cat[1]
raw_data$trip_distance_cat[raw_data$trip_distance > 1 & raw_data$trip_distance <= 2] <- dist_cat[2]
raw_data$trip_distance_cat[raw_data$trip_distance > 2 & raw_data$trip_distance <= 3] <- dist_cat[3]
raw_data$trip_distance_cat[raw_data$trip_distance > 3 & raw_data$trip_distance <= 5] <- dist_cat[4]
raw_data$trip_distance_cat[raw_data$trip_distance > 5 & raw_data$trip_distance <= 7] <- dist_cat[5]
raw_data$trip_distance_cat[raw_data$trip_distance > 7 & raw_data$trip_distance <= 10] <- dist_cat[6]
raw_data$trip_distance_cat[raw_data$trip_distance > 10] <- dist_cat[7]

# Make mode and cat as character
raw_data[,c("trip_mode", "trip_distance_cat")] <- lapply(raw_data[,c("trip_mode", "trip_distance_cat")], as.character)


## FOR LEANDRO

# # Create summary frequency for baseline and three scenarios
# td <- select(raw_data, trip_mode) 
# td1 <- td %>% group_by(trip_mode) %>% summarise(n = n()) %>% mutate(baseline_freq = round(n / sum(n) * 100, 2)) %>% select(trip_mode, baseline_freq)
# td2 <- reshape2::melt(td1,id.vars="trip_mode")
# 
# td2 <- rename(td2, percentage = value)
# 
# td2 <- filter(td2, trip_mode != 'Short Walking')
# 
# bp <- ggplot(td2, aes(x="", y=percentage, fill=trip_mode))+
#   geom_bar(width = 1, stat = "identity")
# bp
# 
# pie <- bp + coord_polar("y", start = 0) +  scale_fill_brewer(palette="Dark2") + xlab("") + ylab("") + labs(title = "Trip Distribution by Mode (baseline)")
# 
# library(scales)
# 
# pie + scale_fill_brewer("Blues") + blank_theme +
#   theme(axis.text.x=element_blank())+
#   geom_text(aes(y = percentage/3 + c(0, cumsum(percentage)[-length(percentage)]), 
#                 label = percent(percentage/100)), size=5)


# Make age category
age_category <- c("15-49", "50-70", ">70")
raw_data$age_cat[raw_data$age >= 15 & raw_data$age < 50] <- age_category[1]
raw_data$age_cat[raw_data$age >= 50 & raw_data$age <= 70] <- age_category[2]
raw_data$age_cat[raw_data$age > 70] <- age_category[3]

# Save all participants greater than 70 in a df
raw_data_70g <- filter(raw_data, age_cat == age_category[3])

# Remove all participants greater than 70 years of age
raw_data <- filter(raw_data, age_cat != age_category[3])

# Create row_id columns for all trips
raw_data$row_id <- 1:nrow(raw_data)

# Redefine short walk as their own category (part of bus trips)
raw_data[duplicated(raw_data$trip_id),]$trip_mode <- 'Short Walking'

# Create scenario 1: 50% of all trips walking trips (in each dist bracket) to Private Car
walking_trips <- subset(raw_data, trip_mode == "Walking")

# Copy mode and duration from baseline
raw_data$scen1_mode <- raw_data$trip_mode
raw_data$scen1_duration <- raw_data$trip_duration

# Take 50% of trips in each distance category and then convert them into walking 
for (i in 1:length(dist_cat)){
  print(dist_cat[i])
  # i <- 2
  trips <- filter(walking_trips, trip_distance_cat == dist_cat[i]) 
  trips_sample <- trips %>% sample_frac(.5) %>% mutate(trip_mode = "Private Car")
  # Recalculate trip duration for Private car trips
  trips_sample$trip_duration <- (trips_sample$trip_distance / 4.8 ) * 21
  trips_sample$scen1_mode <- trips_sample$trip_mode
  trips_sample$scen1_duration <- trips_sample$trip_duration
  trips_sample <- select(trips_sample, row_id, scen1_mode, scen1_duration)
  print(nrow(trips_sample))
  
  # Update selected rows for mode and duration
  raw_data[raw_data$row_id %in% trips_sample$row_id,]$scen1_mode <- trips_sample$scen1_mode
  raw_data[raw_data$row_id %in% trips_sample$row_id,]$scen1_duration <- trips_sample$scen1_duration
  
}

# Scenario 2: All car to Cycle
# 50% of all trips less than 7km to cycle
car_trips <- subset(raw_data, trip_mode == "Private Car" | trip_mode == "Taxi" & !is.na(trip_duration))

# Copy baseline's mode and duration
raw_data$scen2_mode <- raw_data$trip_mode
raw_data$scen2_duration <- raw_data$trip_duration

# speeds <- list(bus.passenger=15,car.passenger=21,pedestrian=4.8,car=21,cyclist=14.5,motorcycle=25,tuktuk=22)
# Loop through all trips less than 7 km
for (i in 1:5){
  print(dist_cat[i])
  # i <- 2
  trips <- filter(car_trips, trip_distance_cat == dist_cat[i]) 
  trips_sample <- trips %>% sample_frac(.5) %>% mutate(trip_mode = "Bicycle")
  
  # Recalculate trip duration for new cycling trips
  trips_sample$trip_duration <- (trips_sample$trip_distance / 21 ) * 14.5
  trips_sample$scen1_mode <- trips_sample$trip_mode
  trips_sample$scen1_duration <- trips_sample$trip_duration
  trips_sample <- select(trips_sample, row_id, scen1_mode, scen1_duration)
  print(nrow(trips_sample))
  
  # Update relevant rows
  raw_data[raw_data$row_id %in% trips_sample$row_id,]$scen2_mode <- trips_sample$scen1_mode
  raw_data[raw_data$row_id %in% trips_sample$row_id,]$scen2_duration <- trips_sample$scen1_duration
  
}


# Scenario 3: All car to Bus
# 50% of all trips longer than 10km to Bus 
# In this scenario, you will need to add walking trip of 10 minutes 

# Copy mode and duration from baseline
raw_data$scen3_mode <- raw_data$trip_mode
raw_data$scen3_duration <- raw_data$trip_duration


# Only consider trips greater than 10 km
for (i in 7){
  print(dist_cat[i])
  # i <- 2
  trips <- filter(car_trips, trip_distance_cat == dist_cat[i]) 
  # Create a specific category for bus trips, so that later on short walk trips can easily be introduced
  trips_sample <- trips %>% sample_frac(.5) %>% mutate(trip_mode = "CB")
  # Recalculate trip duration
  trips_sample$trip_duration <- (trips_sample$trip_distance / 21 ) * 15
  trips_sample$scen1_mode <- trips_sample$trip_mode
  trips_sample$scen1_duration <- trips_sample$trip_duration
  trips_sample <- select(trips_sample, row_id, scen1_mode, scen1_duration)
  print(nrow(trips_sample))
  # Update relevant rows
  raw_data[raw_data$row_id %in% trips_sample$row_id,]$scen3_mode <- trips_sample$scen1_mode
  raw_data[raw_data$row_id %in% trips_sample$row_id,]$scen3_duration <- trips_sample$scen1_duration
  
}


# for scenario 3, add short walking trips for the newly created bus trips
# code originally written by rob

# Backup variable
bd <- raw_data

# raw_data <- bd

# Copy pasted rob's code with minor adjustments
library(ReIns)

modes <- list(bus.passenger='CB',car.passenger='Taxi',pedestrian='Walking',car='Private Car',cyclist='Bicycle',motorcycle='Motorcycle')
## speeds
speeds <- list(bus.passenger=15,car.passenger=21,pedestrian=4.8,car=21,cyclist=14.5,motorcycle=25,tuktuk=22)

## bus wait and walk times
min_wait_time <- c(0,5,10,15,20,30)#exclusive
max_wait_time <- c(5,10,15,20,30,60)#inclusive
wait_time_proportion <- c(51.3,20.3,7.6,6.3,8.3,6.2)
wait_rate <- 0.11
min_walk_time <- c(0,10,20,30,40,60)#exclusive
max_walk_time <- c(10,20,30,40,60, max(subset(raw_data, scen3_mode == 'Bus')$trip_duration) + 1)#inclusive
walk_time_proportion <- c(80.6,13.5,4.3,1.3,0.1,0)
walk_rate <- 0.15
min_bus_duration <- 5

# Use the specialized CB category to identify new short walk trips
## subtract wait and walk times. add new walk journeys
bus_trips <- subset(raw_data, scen3_mode == 'CB')
other_trips <- subset(raw_data, scen3_mode != 'CB')
bus_trips$scen3_duration <- sapply(bus_trips$scen3_duration, function(x) x- rtexp(1, rate = wait_rate, endpoint = x-min_bus_duration))
walk_trips <- bus_trips
walk_trips$scen3_mode <- 'Short Walking'
walk_trips$scen3_duration <- sapply(walk_trips$scen3_duration,function(x)rtexp(1,rate=wait_rate,endpoint=x-min_bus_duration))
bus_trips$scen3_duration <- bus_trips$scen3_duration - walk_trips$scen3_duration

# For the newly added short walk trips for scen3, make NAs for the baseline and two scenarios
walk_trips[, c("trip_mode", "trip_duration", "scen1_mode", "scen1_duration", "scen2_mode", "scen2_duration")] <- NA
walk_trips$row_id <- 0

raw_data <- rbind(raw_data, walk_trips)

# Create another backup
b1 <- raw_data

# Rename intermediate mode CB to Bus
raw_data [raw_data$scen3_mode == 'CB',]$scen3_mode <- "Bus"

# Remove all short walking trips
#raw_data <- filter(raw_data, )

# Redefine row_id
raw_data$row_id <- 1:nrow(raw_data)

write_csv(raw_data, "baseline_and_three_scenarios.csv")


# Create summary frequency for baseline and three scenarios
td <- select(raw_data, trip_mode, scen1_mode, scen2_mode, scen3_mode) 
td1 <- td %>% filter(!is.na(scen1_mode)) %>% group_by(trip_mode) %>% summarise(n = n()) %>% mutate(baseline_freq = round(n / sum(n) * 100, 2)) %>% select(trip_mode, baseline_freq)
td1 <- cbind(td1, td %>% filter(!is.na(scen1_mode)) %>% group_by(scen1_mode) %>% summarise(n = n()) %>% mutate(scen1_freq = round(n / sum(n) * 100, 2)) %>% select(scen1_freq))
td1 <- cbind(td1, td %>% filter(!is.na(scen1_mode)) %>% group_by(scen2_mode) %>% summarise(n = n()) %>% mutate(scen2_freq = round(n / sum(n) * 100, 2)) %>% select(scen2_freq))
td1 <- cbind(td1, td %>% group_by(scen3_mode) %>% summarise(n = n()) %>% mutate(scen3_freq = round(n / sum(n) * 100, 2)) %>% select(scen3_freq))
td2 <- reshape2::melt(td1,id.vars="trip_mode")

td2 <- rename(td2, percentage = value)

td2 <- filter(td2, trip_mode != 'Short Walking')

# Plot mode distribution for baseline and three scenarios
ggplot(data = td2, aes(x = trip_mode, y = percentage, fill = variable)) + geom_bar(stat = 'identity', position = 'dodge') + theme_minimal()+ xlab('Mode') + ylab('Percentage (%)') + labs(title = 'Mode distribution in baseline and three scenarios')


# Calculate trip distance for baseline and three scenarios

# Add 70g distance trips to all scenarios
raw_data_70g$scen1_mode <- raw_data_70g$scen2_mode <- raw_data_70g$scen3_mode <- raw_data_70g$trip_mode

## ADD 70g distance trips
raw_data_dist <- plyr::rbind.fill(raw_data, raw_data_70g)

dist <- filter(raw_data_dist, !is.na(scen1_mode)) %>% group_by(trip_mode) %>% summarise(sum = sum(trip_distance))
dist1 <- filter(raw_data_dist, !is.na(scen1_mode)) %>% group_by(scen1_mode) %>% summarise(sum = sum(trip_distance))
dist2 <- filter(raw_data_dist, !is.na(scen1_mode)) %>% group_by(scen2_mode) %>% summarise(sum = sum(trip_distance))
dist3 <- raw_data_dist %>% group_by(scen3_mode) %>% summarise(sum = sum(trip_distance))
dist$sum_scen1 <- dist1$sum
dist$sum_scen2 <- dist2$sum
dist$sum_scen3 <- dist3$sum
View(dist)
dist <- rename(dist, sum_baseline = sum)

write_csv(dist, "dist_by_mode_all_scenarios_all_ages.csv")

distm <- reshape2::melt(dist, by = trip_mode)

# Remove short walking
distm <- filter(distm, trip_mode != 'Short Walking')

# Plot
ggplot(data = distm, aes(x = trip_mode, y = value, fill = variable)) + geom_bar(stat = 'identity', position = 'dodge') + theme_minimal() + xlab('Mode') + ylab('Distance (km)') + labs(title = "Mode distance (km)")


#ggplot(data=raw_data, aes(x=trip_mode)) + geom_bar(aes(y = (..count..)/sum(..count..))) + theme_minimal() + xlab('Baseline') + ylab ('Percentage') + labs(title = "Mode Distribution")
#ggplot(data=raw_data, aes(x=scen1_mode)) + geom_bar(aes(y = (..count..)/sum(..count..))) + theme_minimal() + xlab('Scenario 1 - 50% of all walking trips to Private Car') + ylab ('Percentage') + labs(title = "Mode Distribution")
#ggplot(data=raw_data, aes(x=scen2_mode)) + geom_bar(aes(y = (..count..)/sum(..count..))) + theme_minimal() + xlab('Scenario 2 - 50% of all trips less than 7km to cycle') + ylab ('Percentage') + labs(title = "Mode Distribution")
#ggplot(data=raw_data, aes(x=scen3_mode)) + geom_bar(aes(y = (..count..)/sum(..count..))) + theme_minimal() + xlab('Scenario 3 - 50% of all car trips longer than 10km to Bus') + ylab ('Percentage') + labs(title = "Mode Distribution")


dur <- filter(raw_data, !is.na(scen1_mode)) %>% group_by(trip_mode) %>% summarise(sum = sum(trip_duration))
dur1 <- filter(raw_data, !is.na(scen1_mode)) %>% group_by(scen1_mode) %>% summarise(sum = sum(scen1_duration))
dur2 <- filter(raw_data, !is.na(scen1_mode)) %>% group_by(scen2_mode) %>% summarise(sum = sum(scen2_duration))
dur3 <- raw_data %>% group_by(scen3_mode) %>% summarise(sum = sum(scen3_duration))
dur$scen1 <- dur1$sum
dur$scen2 <- dur2$sum
dur$scen3 <- dur3$sum
View(dur)
dur <- rename(dur, baseline = sum)

durm <- reshape2::melt(dur, by = trip_mode)

# Remove short walking
durm <- filter(durm, trip_mode != 'Short Walking')

durh <- durm
durh$value <- round(durh$value / 60, 2)

# Plot
ggplot(data = durh, aes(x = trip_mode, y = value, fill = variable)) + geom_bar(stat = 'identity', position = 'dodge') + theme_minimal() + xlab('Mode') + ylab('Duration (hours)') + labs(title = "Mode Duration (hours)")



# 
# 
# ## Calculate individual mmets
# 
# 
# # Constants
# # Initialize  energy expenditure constants - taken from ICT
# METCycling <- 5.63
# METWalking <- 3.53
# METEbikes <- 4.50
# 
# 
# # Get total individual level walking and cycling and sport mmets 
# individual_travel_mmet <- filter(raw_data, !is.na(scen1_mode)) %>% 
#                                     group_by(participant_id) %>% summarise (sex = first(sex), 
#                                                                age_cat = first(age_cat),
#                                                                baseline_cycle_hrs = sum(trip_duration[trip_mode == 'Bicycle']),
#                                                                baseline_walk_hrs = sum(trip_duration[trip_mode == 'Walking'] + 
#                                                                                          trip_duration[trip_mode == 'Short Walking']),
#                                                                scen1_cycle_hrs = sum(trip_duration[scen1_mode == 'Bicycle']),
#                                                                scen1_walk_hrs = sum(trip_duration[scen1_mode == 'Walking'] + 
#                                                                                       trip_duration[scen1_mode == 'Short Walking']),
#                                                                scen2_cycle_hrs = sum(trip_duration[scen2_mode == 'Bicycle']),
#                                                                scen2_walk_hrs = sum(trip_duration[scen2_mode == 'Walking'] + 
#                                                                                                     trip_duration[scen2_mode == 'Short Walking']),
#                                                                scen3_cycle_hrs = sum(trip_duration[scen3_mode == 'Bicycle']),
#                                                                scen3_walk_hrs = sum(trip_duration[scen3_mode == 'Walking'] + 
#                                                                                                     trip_duration[scen3_mode == 'Short Walking']),
#                                                                btmmet = (METCycling - 1) * baseline_cycle_hrs + 
#                                                                  (METWalking - 1) * baseline_walk_hrs,
#                                                                sc1tmmet = (METCycling - 1) * scen1_cycle_hrs + 
#                                                                  (METWalking - 1) * scen1_walk_hrs,
#                                                                sc2tmmet = (METCycling - 1) * scen2_cycle_hrs + 
#                                                                  (METWalking - 1) * scen2_walk_hrs,
#                                                                sc3tmmet = (METCycling - 1) * scen3_cycle_hrs + 
#                                                                  (METWalking - 1) * scen3_walk_hrs
#                                                                )
# 
# 
# individual_travel_mmet <- individual_travel_mmet %>% select(participant_id, sex, age_cat, btmmet, sc1tmmet, sc2tmmet, sc3tmmet)

