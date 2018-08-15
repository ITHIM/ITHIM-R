rm (list = ls())
# Load packages
library(tidyverse)
library(haven)
library(plotly)
library(ReIns)

# set seed
set.seed(1)

# Read travel survey data
rd <- read_csv("data/synth_pop_data/accra/travel_survey/synthetic_population_with_trips.csv")

# Create a row id
rd$rid <- 1:nrow(rd)

# Define trip_distances (in km)
# Based on travel mode and trip duration, calculate distances

trip_mode <- c("Bus", "Private Car", "Taxi", "Walking",
               "Short Walking", "Bicycle", "Motorcycle")
speeds <- c(15, 21, 21, 4.8, 4.8, 14.5, 25)

modes_speeds_lt <- data.frame(trip_mode, speeds)
modes_speeds_lt$trip_mode <- as.character(modes_speeds_lt$trip_mode)

rd <- left_join(rd, modes_speeds_lt, by = "trip_mode")

rd$speeds[is.na(rd$speeds)] <- 0

rd$trip_distance <- (rd$trip_duration / 60) * rd$speeds

rd$speeds <- NULL

# Define distance categories
dist_cat <- c("0-6 km", "7-9 km", "10+ km")

# Initialize them
rd$trip_distance_cat[rd$trip_distance > 0 & rd$trip_distance < 7] <- dist_cat[1]
rd$trip_distance_cat[rd$trip_distance >= 7 & rd$trip_distance < 10] <- dist_cat[2]
rd$trip_distance_cat[rd$trip_distance >= 10] <- dist_cat[3]

# Make age category
age_category <- c("15-49", "50-70", ">70")
rd$age_cat[rd$age >= 15 & rd$age < 50] <- age_category[1]
rd$age_cat[rd$age >= 50 & rd$age <= 70] <- age_category[2]
rd$age_cat[rd$age > 70] <- age_category[3]

# Remove all participants greater than 70 years of age
rd <- filter(rd, age_cat != age_category[3])

rd$scenario <- "Baseline"

add_walk_trips <- function(bus_trips, ln_mean, ln_sd){
  
  # bus_trips
  # ln_mean = 5
  # ln_sd = 1.2
  
  bus_trips <- arrange(bus_trips, trip_duration)
  
  walk_trips <- bus_trips
  
  walk_trips$trip_mode <- 'Short Walking'
  
  walk_trips$trip_duration <- sort(rlnorm(n = nrow(bus_trips), meanlog = log(ln_mean), sdlog = log(ln_sd)))
  
  # Replace walk trips with duration greater than that of bus needs to be set to 0
  if (nrow(walk_trips[(walk_trips$trip_duration - bus_trips$trip_duration)  > 0,]) > 0)
    walk_trips[(walk_trips$trip_duration - bus_trips$trip_duration)  > 0,]$trip_duration <- 0
  
  bus_trips$trip_duration <- bus_trips$trip_duration - walk_trips$trip_duration
  
  print(summary(bus_trips$trip_duration))
  
  print(summary(walk_trips$trip_duration))

  # Corrrect walk trips distance
  walk_trips$trip_distance <- (walk_trips$trip_duration / 60) * 4.8
  
  bus_trips$trip_distance <- (bus_trips$trip_duration / 60 ) * 15
  
  
  # Define distance categories
  dist_cat <- c("0-6 km", "7-9 km", "10+ km")
  
  # Recategories trip_distance_cat for both bus and walk trips
  bus_trips$trip_distance_cat[bus_trips$trip_distance > 0 & bus_trips$trip_distance < 7] <- dist_cat[1]
  bus_trips$trip_distance_cat[bus_trips$trip_distance >= 7 & bus_trips$trip_distance < 10] <- dist_cat[2]
  bus_trips$trip_distance_cat[bus_trips$trip_distance >= 10] <- dist_cat[3]


  walk_trips$trip_distance_cat[walk_trips$trip_distance > 0 & walk_trips$trip_distance < 7] <- dist_cat[1]
  walk_trips$trip_distance_cat[walk_trips$trip_distance >= 7 & walk_trips$trip_distance < 10] <- dist_cat[2]
  walk_trips$trip_distance_cat[walk_trips$trip_distance >= 10] <- dist_cat[3]
  
  return(list(bus_trips, walk_trips))
  
}

bus_walk_trips <- add_walk_trips(filter(rd, trip_mode == "Bus"), ln_mean = 5, ln_sd = 1.2)

rd[rd$trip_mode == 'Bus' & rd$rid %in% bus_walk_trips[[1]]$rid,]$trip_duration <- bus_walk_trips[[1]]$trip_duration
rd[rd$trip_mode == 'Bus' & rd$rid %in% bus_walk_trips[[1]]$rid,]$trip_distance <- bus_walk_trips[[1]]$trip_distance
rd[rd$trip_mode == 'Bus' & rd$rid %in% bus_walk_trips[[1]]$rid,]$trip_distance_cat <- bus_walk_trips[[1]]$trip_distance_cat

rd <- rbind(rd, bus_walk_trips[[2]])

baseline_mode_distance <- (rd %>% filter(! trip_mode %in% c('Short Walking', "99", "Train", "Other", "Unspecified")) %>% 
                group_by(trip_mode, trip_distance_cat) %>% summarise(c = n(), p = n() / nrow(.) * 100))

# Create row_id columns for all trips
rd$row_id <- 1:nrow(rd)

###############################################################
# Scenario 1

rdr <- rd


source_modes <- c('Bus', 'Walking')
target_modes <- c('Private Car')

source_percentages <- c(0.16, 0.49)

tt <- nrow(filter(rdr, ! trip_mode %in% c('99', 'Short Walking')))

create_scenario <- function(rdr, scen_name, source_modes, target_modes, source_distance_cats, target_distance_cat,
                            source_trips, target_trips){
  
  local_source_trips <- list()
  
  for (i in 1:length(source_modes)){
    #if (nrow(filter(rdr, trip_mode == source_modes[i])) - round(source_percentages[i] * tt) > 0)
    local_source_trips[i] <- nrow(filter(rdr, trip_mode == source_modes[i])) - source_trips[i]
    
    print(local_source_trips[i])
    #else
    #  source_trips[i] <- round(source_percentages[i] * tt) - nrow(filter(rdr, trip_mode == source_modes[i]))
  }
  
  
  local_source_trips <- purrr::flatten_dbl(local_source_trips)
  
  mode_speeds <- data.frame(modes = c("Bus", "Private Car", "Taxi", "Walking", "Short Walking", "Bicycle", "Motorcycle"),
                            speed = c(15, 21, 21, 4.8, 4.8, 14.5, 25))
  
  # browser()
    
  all_samples <- NULL
  
  for (i in 1:length(source_modes)){
    
    sample <- filter(rdr, trip_mode == source_modes[i]) %>% sample_n(local_source_trips[i]) %>% 
      mutate(trip_mode = target_modes[i],
             trip_duration = (trip_distance * 60 ) / mode_speeds[mode_speeds$modes == source_modes[i],]$speed)
    
    
    # Update selected rows for mode and duration
    rdr[rdr$row_id %in% sample$row_id,]$trip_mode <- sample$trip_mode
    rdr[rdr$row_id %in% sample$row_id,]$trip_duration <- sample$trip_duration
    
    
    if (source_modes[i] == 'Bus'){
      # Remove bus associated short walking trips that have been changed to Private Car trips
      rdr <- rdr[!(rdr$trip_mode == 'Short Walking' & rdr$trip_id %in% sample$trip_id),]
    }
    
    
    # 
    # if (is.null(all_samples))
    #   all_samples <- sample
    # else
    #   all_samples <- rbind(all_samples, sample)
    
    
  }  
  
  
  # all_samples$scenario <- scen_name
  # 
  # all_samples
  
  rdr$scenario <- scen_name
  
  rdr
  
}

rdfinal <- rbind(rd, create_scenario(rdr, scen_name = 'Scenario 1', source_modes = source_modes, target_modes = target_modes,
                                     source_distance_cats = dist_cat, target_distance_cat = dist_cat,
                                     source_trips = c(round(source_percentages[1] * tt), round(source_percentages[2] * tt)))
)

###############################################################
# Scenario 2

rdr <- filter(rdfinal, scenario == 'Scenario 1')

# 35 % Bus

tt <- nrow(filter(rdfinal, scenario == 'Scenario 1' & ! trip_mode %in% c('99', 'Short Walking')))

source_modes <- c('Private Car', 'Taxi')
target_modes <- c('Bus')

target_new_trips <- c(round(0.35 * tt) - nrow(filter(rdr, trip_mode == 'Bus')))

# trip_mode <- c("Bus", "Private Car", "Taxi", "Walking",
#                "Short Walking", "Bicycle", "Motorcycle")
# speeds <- c(15, 21, 21, 4.8, 4.8, 14.5, 25)

# 16 % Bus	1622
# 49% walk	4969

total_car_trips <- filter(rdr, (trip_mode %in% c(source_modes[1], source_modes[2])))


t_dc <- total_car_trips %>% group_by(trip_distance_cat) %>% summarise(count = n())

long_trips <- sum(t_dc[t_dc$trip_distance_cat != dist_cat[1],]$count)

long_car_trips_sample <- total_car_trips %>%  filter(trip_distance_cat %in% c(dist_cat[2], dist_cat[3]) ) %>% 
  sample_n(long_trips) %>% 
  mutate(trip_mode = target_modes[1],
         # Recalculate trip duration for Private car trips
         trip_duration = (trip_distance * 60 ) / 15)


short_car_trips_sample <- total_car_trips %>%  filter(trip_distance_cat %in% c(dist_cat[1]) ) %>% 
  sample_n(target_new_trips[1] - long_trips) %>% 
  mutate(trip_mode = target_modes[1],
         # Recalculate trip duration for Private car trips
         trip_duration = (trip_distance * 60 ) / 15)

car_trips_sample <- rbind(long_car_trips_sample, short_car_trips_sample)

##  ADDING SHORT WALK TRIPS FOR NEW BUS TRIPS

# Divide bus trips into bus and walk trips
bus_trips <- car_trips_sample

bus_walk_trips <- add_walk_trips(bus_trips, ln_mean = 5, ln_sd = 1.2)

# Update selected rows for mode and duration
rdr[rdr$row_id %in% bus_walk_trips[[1]]$row_id,]$trip_mode <- bus_walk_trips[[1]]$trip_mode
rdr[rdr$trip_mode == 'Bus' & rdr$rid %in% bus_walk_trips[[1]]$rid,]$trip_duration <- bus_walk_trips[[1]]$trip_duration
rdr[rdr$trip_mode == 'Bus' & rdr$rid %in% bus_walk_trips[[1]]$rid,]$trip_distance <- bus_walk_trips[[1]]$trip_distance
rdr[rdr$trip_mode == 'Bus' & rdr$rid %in% bus_walk_trips[[1]]$rid,]$trip_distance_cat <- bus_walk_trips[[1]]$trip_distance_cat

rdr %>% group_by(trip_mode) %>% summarise(c = n(), p = n() / nrow(rdr) * 100)

rdr <- rbind(rdr, bus_walk_trips[[2]])

rdr$scenario <- "Scenario 2"

rdfinal <- rbind(rdfinal, rdr)


###############################################################
# Scenario 3

rdr <- filter(rdfinal, scenario == 'Scenario 1')

# 16 % Bus remain as is
# 10 % Mcycle increase 
# x decrease private car

tt <- nrow(filter(rdfinal, scenario == 'Scenario 1' & ! trip_mode %in% c('99', 'Short Walking')))


# trip_mode <- c("Bus", "Private Car", "Taxi", "Walking",
#                "Short Walking", "Bicycle", "Motorcycle")
# speeds <- c(15, 21, 21, 4.8, 4.8, 14.5, 25)

source_modes <- c('Private Car')
target_modes <- c('Motorcycle')

target_new_trips <- c(round(0.1 * tt) - 
                        nrow(filter(rdr, trip_mode == 'Motorcycle')))

# trip_mode <- c("Bus", "Private Car", "Taxi", "Walking",
#                "Short Walking", "Bicycle", "Motorcycle")
# speeds <- c(15, 21, 21, 4.8, 4.8, 14.5, 25)

# 16 % Bus	1622
# 49% walk	4969

mcycle_trips_sample <- filter(rdr, trip_mode == source_modes[1]) %>% sample_n(target_new_trips[1]) %>% 
  mutate(trip_mode = target_modes[1],
         # Recalculate trip duration for Motorcycle
         trip_duration = (trip_distance * 60 / 25))

# Update selected rows for mode and duration
rdr[rdr$row_id %in% mcycle_trips_sample$row_id,]$trip_mode <- mcycle_trips_sample$trip_mode
rdr[rdr$row_id %in% mcycle_trips_sample$row_id,]$trip_duration <- mcycle_trips_sample$trip_duration


rdr %>% group_by(trip_mode) %>% summarise(c = n(), p = n() / nrow(rdr) * 100)

# rdr <- rbind(rdr, t_not_used)

# Remove bus associated short walking trips that have been changed to Private Car trips
rdr <- rdr[!(rdr$trip_mode == 'Short Walking' & rdr$trip_id %in% bus_trips_sample$trip_id),]

rdr$scenario <- "Scenario 3"

rdfinal <- rbind(rdfinal, rdr)



###############################################################
# Scenario 4

rdr <- filter(rdfinal, scenario == 'Scenario 1')

# 3.5 % Cycle

tt <- nrow(filter(rdfinal, scenario == 'Scenario 1' & ! trip_mode %in% c('99', 'Short Walking')))

source_modes <- c('Motorcycle', 'Private Car', 'Taxi')
target_modes <- c('Bicycle')

target_new_trips <- c(52,
  round(0.035 * tt) - nrow(filter(rdr, trip_mode == 'Bicycle')) - 52)

# trip_mode <- c("Bus", "Private Car", "Taxi", "Walking",
#                "Short Walking", "Bicycle", "Motorcycle")
# speeds <- c(15, 21, 21, 4.8, 4.8, 14.5, 25)

mbike_trips <- filter(rdr, trip_mode %in% c(source_modes[1])) %>% 
  sample_n(target_new_trips[1]) %>% 
  mutate(trip_mode = target_modes[1],
         # Recalculate trip duration for Cycle trips
         trip_duration = (trip_distance * 60 ) / 14.5)

car_trips <- filter(rdr, trip_mode %in% c(source_modes[2], source_modes[3] ) & 
                            (trip_distance_cat == dist_cat[1])) %>% 
  sample_n(target_new_trips[2]) %>% 
  mutate(trip_mode = target_modes[1],
         # Recalculate trip duration for Cycle trips
         trip_duration = (trip_distance * 60 ) / 14.5)

car_mbike_trips <- rbind(mbike_trips, car_trips)


# Update selected rows for mode and duration
rdr[rdr$row_id %in% car_mbike_trips$row_id,]$trip_mode <- car_mbike_trips$trip_mode
rdr[rdr$row_id %in% car_mbike_trips$row_id,]$trip_duration <- car_mbike_trips$trip_duration

rdr %>% group_by(trip_mode) %>% summarise(c = n(), p = n() / nrow(rdr) * 100)

# Remove bus associated short walking trips that have been changed to Private Car trips
rdr <- rdr[!(rdr$trip_mode == 'Short Walking' & rdr$trip_id %in% bus_trips_sample$trip_id),]

rdr$scenario <- "Scenario 4"

rdfinal <- rbind(rdfinal, rdr)


###############################################################
# Scenario 5

rdr <- filter(rdfinal, scenario == 'Scenario 1')

# 3.5 % Cycle

tt <- nrow(filter(rdfinal, scenario == 'Scenario 1' & ! trip_mode %in% c('99', 'Short Walking')))

source_modes <- c('Private Car', 'Taxi')
target_modes <- c('Walking')

target_new_trips <- c(round(0.54 * tt) - nrow(filter(rdr, trip_mode == 'Walking')))

# trip_mode <- c("Bus", "Private Car", "Taxi", "Walking",
#                "Short Walking", "Bicycle", "Motorcycle")
# speeds <- c(15, 21, 21, 4.8, 4.8, 14.5, 25)

motorised_trips <- filter(rdr, trip_mode %in% c(source_modes[1], source_modes[2] )& 
                            (trip_distance_cat == dist_cat[1])) %>% 
  sample_n(target_new_trips[1]) %>% 
  mutate(trip_mode = target_modes[1],
         # Recalculate trip duration for Walking trips
         trip_duration = (trip_distance * 60 ) / 14.5)


# Update selected rows for mode and duration
rdr[rdr$row_id %in% motorised_trips$row_id,]$trip_mode <- motorised_trips$trip_mode
rdr[rdr$row_id %in% motorised_trips$row_id,]$trip_duration <- motorised_trips$trip_duration

rdr %>% group_by(trip_mode) %>% summarise(c = n(), p = n() / nrow(rdr) * 100)

# Remove bus associated short walking trips that have been changed to Private Car trips
rdr <- rdr[!(rdr$trip_mode == 'Short Walking' & rdr$trip_id %in% bus_trips_sample$trip_id),]


rdr$scenario <- "Scenario 5"

rdfinal <- rbind(rdfinal, rdr)


####

write_csv(rdfinal, "data/scenarios/accra/baseline_and_scenarios.csv")