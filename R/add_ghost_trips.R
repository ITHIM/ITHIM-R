#' @export
add_ghost_trips <- function(raw_trip_set,trip_mode){
  
  ## values for new ghost journeys
  age_range <- AGE_LOWER_BOUNDS[1]:MAX_AGE
  nPeople <- 2
  nTrips <- 1
  new_gender <- 'Male'
  total_car_duration <- sum(subset(raw_trip_set,trip_mode=='car')$trip_duration)
  total_car_distance <- total_car_duration/60*VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$trip_mode=='car']
  
  ## add new travel
  new_mode <- trip_mode
  distance_ratio <- ifelse(trip_mode=='truck',TRUCK_TO_CAR_RATIO,BUS_TO_CAR_RATIO)
  total_new_distance <- total_car_distance*distance_ratio#VEHICLE_INVENTORY$distance_ratio_to_car[VEHICLE_INVENTORY$trip_mode==new_mode]
  new_duration <- total_new_distance/VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$trip_mode==new_mode]*60
  duration_range <- c(floor(new_duration/nPeople),ceiling(new_duration/nPeople))
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
  
  return(raw_trip_set)
  
}
