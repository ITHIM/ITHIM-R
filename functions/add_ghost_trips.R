add_ghost_trips <- function(raw_trip_set){
  
  ## values for new ghost journeys
  age_range <- AGE_LOWER_BOUNDS[1]:MAX_AGE
  nPeople <- 2
  nTrips <- 1
  new_gender <- 'Male'
  total_car_duration <- sum(subset(raw_trip_set,trip_mode=='Private Car')$trip_duration)
  total_car_distance <- total_car_duration/60*VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$trip_mode=='Private Car']
  
  ## add Truck travel
  new_mode <- 'Truck'
  total_truck_distance <- total_car_distance*VEHICLE_INVENTORY$distance_ratio_to_car[VEHICLE_INVENTORY$trip_mode==new_mode]
  truck_duration <- total_truck_distance/VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$trip_mode==new_mode]*60
  duration_range <- c(floor(truck_duration/nPeople),ceiling(truck_duration/nPeople))
  for(i in 1:nPeople){
    new_trips <- add_trips(trip_ids   = max(raw_trip_set$trip_id) + 1: nTrips, 
                           new_mode = new_mode, 
                           duration = duration_range, 
                           participant_id = 0,
                           age = age_range,
                           sex = new_gender,
                           nTrips=nTrips)
    raw_trip_set <- rbind(raw_trip_set, new_trips)
  }
  
  ## add Bus_driver travel
  new_mode <- 'Bus_driver'
  total_bus_distance <- total_car_distance*VEHICLE_INVENTORY$distance_ratio_to_car[VEHICLE_INVENTORY$trip_mode==new_mode]
  bus_duration <- total_bus_distance/VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$trip_mode==new_mode]*60
  duration_range <- c(floor(bus_duration/nPeople),ceiling(bus_duration/nPeople))
  for(i in 1:nPeople){
    new_trips <- add_trips(trip_ids   = max(raw_trip_set$trip_id) + 1: nTrips, 
                           new_mode = new_mode, 
                           duration = duration_range, 
                           participant_id = 0,
                           age = age_range,
                           sex = new_gender,
                           nTrips=nTrips)
    raw_trip_set <- rbind(raw_trip_set, new_trips)
  }
  
  raw_trip_set
  
}