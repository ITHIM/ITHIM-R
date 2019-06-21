#' @export
add_ghost_trips <- function(raw_trip_set,trip_mode='bus_driver',distance_ratio=BUS_TO_PASSENGER_RATIO*DISTANCE_SCALAR_PT,reference_mode='bus'){
  
  ## values for new ghost journeys
  age_range <- AGE_LOWER_BOUNDS[1]:MAX_AGE
  nPeople <- 20
  nTrips <- 5
  new_gender <- 'Male'
  total_ref_distance <- sum(raw_trip_set[raw_trip_set$stage_mode==reference_mode,]$stage_distance,na.rm=T)
  
  ## add new travel
  new_mode <- trip_mode
  total_new_distance <- total_ref_distance*distance_ratio
  distance_range <- c(floor(total_new_distance/nPeople),ceiling(total_new_distance/nPeople))
  speed <- MODE_SPEEDS$speed[MODE_SPEEDS$stage_mode==new_mode]
  for(i in 1:nPeople){
    new_trips <- add_trips(trip_ids   = max(raw_trip_set$trip_id) + 1: nTrips, 
                           new_mode = new_mode, 
                           distance = distance_range, 
                           participant_id = 0,
                           age = age_range,
                           sex = new_gender,
                           nTrips=nTrips,
                           speed=speed)
    raw_trip_set <- rbind(raw_trip_set, new_trips)
  }
  
  return(raw_trip_set)
  
}
