# Remove everything
rm (list = ls())
# Load packages
library(tidyverse)
library(haven)
library(plotly)

# Read travel survey data
raw_data <- readRDS("data/synth_pop_data/accra/travel_survey/accra_processed_trips.Rds")

raw_data[,c("trip_id", "participant_id", "age")] <- lapply(raw_data[,c("trip_id", "participant_id", "age")], as.numeric)
raw_data[,c("trip_duration", "trip_distance")] <- lapply(raw_data[,c("trip_duration", "trip_distance")], as.double)


dist_cat <- c("0-1 km", "1-2 km", "2-3 km", "3-5 km", "5-7 km", "7-10 km", ">10 km" )

raw_data$trip_distance_cat[raw_data$trip_distance > 0 & raw_data$trip_distance <= 1] <- dist_cat[1]
raw_data$trip_distance_cat[raw_data$trip_distance > 1 & raw_data$trip_distance <= 2] <- dist_cat[2]
raw_data$trip_distance_cat[raw_data$trip_distance > 2 & raw_data$trip_distance <= 3] <- dist_cat[3]
raw_data$trip_distance_cat[raw_data$trip_distance > 3 & raw_data$trip_distance <= 5] <- dist_cat[4]
raw_data$trip_distance_cat[raw_data$trip_distance > 5 & raw_data$trip_distance <= 7] <- dist_cat[5]
raw_data$trip_distance_cat[raw_data$trip_distance > 7 & raw_data$trip_distance <= 10] <- dist_cat[6]
raw_data$trip_distance_cat[raw_data$trip_distance > 10] <- dist_cat[7]


raw_data[,c("trip_mode", "trip_distance_cat")] <- lapply(raw_data[,c("trip_mode", "trip_distance_cat")], as.character)

# Create row_id columns for all trips
raw_data$row_id <- 1:nrow(raw_data)

raw_data[duplicated(raw_data$trip_id),]$trip_mode <- 'Short Walking'



# Create scenario 1: 50% of all trips walking trips (in each dist bracket) to Private Car
walking_trips <- subset(raw_data, trip_mode == "Walking")

raw_data$scen1_mode <- raw_data$trip_mode
raw_data$scen1_duration <- raw_data$trip_duration

# speeds <- list(bus.passenger=15,car.passenger=21,pedestrian=4.8,car=21,cyclist=14.5,motorcycle=25,tuktuk=22)

for (i in 1:length(dist_cat)){
  print(dist_cat[i])
  # i <- 2
  trips <- filter(walking_trips, trip_distance_cat == dist_cat[i]) 
  trips_sample <- trips %>% sample_frac(.5) %>% mutate(trip_mode = "Private Car")
  trips_sample$trip_duration <- (trips_sample$trip_distance / 4.8 ) * 21
  trips_sample$scen1_mode <- trips_sample$trip_mode
  trips_sample$scen1_duration <- trips_sample$trip_duration
  trips_sample <- select(trips_sample, row_id, scen1_mode, scen1_duration)
  print(nrow(trips_sample))
  raw_data[raw_data$row_id %in% trips_sample$row_id,]$scen1_mode <- trips_sample$scen1_mode
  raw_data[raw_data$row_id %in% trips_sample$row_id,]$scen1_duration <- trips_sample$scen1_duration
  
}

# Scenario 2: All car to Cycle
# 50% of all trips less than 7km to cycle
car_trips <- subset(raw_data, trip_mode == "Private Car" | trip_mode == "Taxi" & !is.na(trip_duration))

raw_data$scen2_mode <- raw_data$trip_mode
raw_data$scen2_duration <- raw_data$trip_duration

# speeds <- list(bus.passenger=15,car.passenger=21,pedestrian=4.8,car=21,cyclist=14.5,motorcycle=25,tuktuk=22)

for (i in 1:5){
  print(dist_cat[i])
  # i <- 2
  trips <- filter(car_trips, trip_distance_cat == dist_cat[i]) 
  trips_sample <- trips %>% sample_frac(.5) %>% mutate(trip_mode = "Bicycle")
  trips_sample$trip_duration <- (trips_sample$trip_distance / 21 ) * 14.5
  trips_sample$scen1_mode <- trips_sample$trip_mode
  trips_sample$scen1_duration <- trips_sample$trip_duration
  trips_sample <- select(trips_sample, row_id, scen1_mode, scen1_duration)
  print(nrow(trips_sample))
  raw_data[raw_data$row_id %in% trips_sample$row_id,]$scen2_mode <- trips_sample$scen1_mode
  raw_data[raw_data$row_id %in% trips_sample$row_id,]$scen2_duration <- trips_sample$scen1_duration
  
}


# Scenario 3: All car to Bus
# 50% of all trips longer than 10km to Bus 
# In this scenario, you will need to add walking trip of 10 minutes 


raw_data$scen3_mode <- raw_data$trip_mode
raw_data$scen3_duration <- raw_data$trip_duration

# speeds <- list(bus.passenger=15,car.passenger=21,pedestrian=4.8,car=21,cyclist=14.5,motorcycle=25,tuktuk=22)

for (i in 7){
  print(dist_cat[i])
  # i <- 2
  trips <- filter(car_trips, trip_distance_cat == dist_cat[i]) 
  trips_sample <- trips %>% sample_frac(.5) %>% mutate(trip_mode = "CB")
  trips_sample$trip_duration <- (trips_sample$trip_distance / 21 ) * 15
  trips_sample$scen1_mode <- trips_sample$trip_mode
  trips_sample$scen1_duration <- trips_sample$trip_duration
  trips_sample <- select(trips_sample, row_id, scen1_mode, scen1_duration)
  print(nrow(trips_sample))
  raw_data[raw_data$row_id %in% trips_sample$row_id,]$scen3_mode <- trips_sample$scen1_mode
  raw_data[raw_data$row_id %in% trips_sample$row_id,]$scen3_duration <- trips_sample$scen1_duration
  
}


# for scenario 3, add short walking trips for the newly created bus trips
# code originally written by rob


bd <- raw_data

raw_data <- bd

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
max_walk_time <- c(10,20,30,40,60,max(subset(raw_data, scen3_mode == 'Bus')$trip_duration)+1)#inclusive
walk_time_proportion <- c(80.6,13.5,4.3,1.3,0.1,0)
walk_rate <- 0.15
min_bus_duration <- 5


## subtract wait and walk times. add new walk journeys
bus_trips <- subset(raw_data, scen3_mode == 'CB')
other_trips <- subset(raw_data, scen3_mode != 'CB')
bus_trips$scen3_duration <- sapply(bus_trips$scen3_duration, function(x) x- rtexp(1, rate = wait_rate, endpoint = x-min_bus_duration))
walk_trips <- bus_trips
walk_trips$scen3_mode <- 'Short Walking'
walk_trips$scen3_duration <- sapply(walk_trips$scen3_duration,function(x)rtexp(1,rate=wait_rate,endpoint=x-min_bus_duration))
bus_trips$scen3_duration <- bus_trips$scen3_duration - walk_trips$scen3_duration

#raw_data[raw_data$row_id %in% bus_trips$row_id,]$scen3_duration <- bus_trips$scen3_duration


# "scen1_mode"       
# [11] "scen1_duration"    "scen2_mode"       
# [13] "scen2_duration"    "scen3_mode"       
# [15] "scen3_duration"   

walk_trips[, c("scen1_mode", "scen1_duration", "scen2_mode", "scen2_duration")] <- NA
walk_trips$row_id <- 0

raw_data <- rbind(raw_data, walk_trips)


b1 <- raw_data

raw_data [raw_data$scen3_mode == 'CB',]$scen3_mode <- "Bus"

#write_csv(raw_data, "baseline_and_three_scenarios.csv")

td <- select(raw_data, trip_mode, scen1_mode, scen2_mode, scen3_mode) 
td1 <- td %>% filter(!is.na(scen1_mode)) %>% group_by(trip_mode) %>% summarise(n = n()) %>% mutate(baseline_freq = round(n / sum(n) * 100, 2)) %>% select(trip_mode, baseline_freq)
td1 <- cbind(td1, td %>% filter(!is.na(scen1_mode)) %>% group_by(scen1_mode) %>% summarise(n = n()) %>% mutate(scen1_freq = round(n / sum(n) * 100, 2)) %>% select(scen1_freq))
td1 <- cbind(td1, td %>% filter(!is.na(scen1_mode)) %>% group_by(scen2_mode) %>% summarise(n = n()) %>% mutate(scen2_freq = round(n / sum(n) * 100, 2)) %>% select(scen2_freq))
td1 <- cbind(td1, td %>% group_by(scen3_mode) %>% summarise(n = n()) %>% mutate(scen3_freq = round(n / sum(n) * 100, 2)) %>% select(scen3_freq))
td2 <- reshape2::melt(td1,id.vars="trip_mode")

td2 <- rename(td2, percentage = value)


ggplot(data = td2, aes(x = trip_mode, y = percentage, fill = variable)) + geom_bar(stat = 'identity', position = 'dodge') + theme_minimal()