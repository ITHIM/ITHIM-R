#' @export
add_walk_trips <- function(bus_trips){
  
  # bus_trips
  # ln_mean = 5
  # ln_sd = 1.2
  
  #bus_trips <- arrange(bus_trips, trip_duration)
  walk_trips <- bus_trips
  ##?? do we need 'Short Walking' instead of 'Walking'?
  walk_trips$trip_mode <- 'Short Walking'
  ##RJ all trips have the same BUS_WALK_TIME
  walk_trips$trip_duration <- BUS_WALK_TIME
  
  # Replace walk trips with duration greater than that of bus needs to be set to 0
  if (any(walk_trips$trip_duration - bus_trips$trip_duration  >= 0))
    walk_trips$trip_duration[(walk_trips$trip_duration - bus_trips$trip_duration)  >= 0] <- 0
  
  bus_trips$trip_duration <- bus_trips$trip_duration - walk_trips$trip_duration
  
  # Corrrect walk trips distance
  walk_trips$trip_distance <- (walk_trips$trip_duration / 60) * VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$trip_mode=='Walking']
  bus_trips$trip_distance <- (bus_trips$trip_duration / 60 ) * VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$trip_mode=='Bus']
  
  # Recategorise trip_distance_cat for both bus and walk trips
  bus_trips$trip_distance_cat[bus_trips$trip_distance > 0 & bus_trips$trip_distance < DIST_LOWER_BOUNDS[2]] <- DIST_CAT[1]
  bus_trips$trip_distance_cat[bus_trips$trip_distance >= DIST_LOWER_BOUNDS[2] & bus_trips$trip_distance < DIST_LOWER_BOUNDS[3]] <- DIST_CAT[2]
  bus_trips$trip_distance_cat[bus_trips$trip_distance >= DIST_LOWER_BOUNDS[3]] <- DIST_CAT[3]
  
  
  walk_trips$trip_distance_cat[walk_trips$trip_distance > 0 & walk_trips$trip_distance < DIST_LOWER_BOUNDS[2]] <- DIST_CAT[1]
  walk_trips$trip_distance_cat[walk_trips$trip_distance >= DIST_LOWER_BOUNDS[2] & walk_trips$trip_distance < DIST_LOWER_BOUNDS[3]] <- DIST_CAT[2]
  walk_trips$trip_distance_cat[walk_trips$trip_distance >= DIST_LOWER_BOUNDS[3]] <- DIST_CAT[3]
  
  return(list(bus_trips, walk_trips))
  
}
