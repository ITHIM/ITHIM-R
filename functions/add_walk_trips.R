add_walk_trips <- function(bus_trips, ln_mean, ln_sd){
  
  # bus_trips
  # ln_mean = 5
  # ln_sd = 1.2
  
  bus_trips <- arrange(bus_trips, trip_duration)
  walk_trips <- bus_trips
  walk_trips$trip_mode <- 'Short Walking'
  ##RJ all trips have the same MEAN_BUS_WALK_TIME
  walk_trips$trip_duration <- 5#MEAN_BUS_WALK_TIME#sort(rlnorm(n = nrow(bus_trips), meanlog = log(MEAN_BUS_WALK_TIME), sdlog = log(ln_sd)))
  
  # Replace walk trips with duration greater than that of bus needs to be set to 0
  if (nrow(walk_trips[(walk_trips$trip_duration - bus_trips$trip_duration)  > 0,]) > 0)
    walk_trips[(walk_trips$trip_duration - bus_trips$trip_duration)  > 0,]$trip_duration <- 0
  
  bus_trips$trip_duration <- bus_trips$trip_duration - walk_trips$trip_duration
  
  # Corrrect walk trips distance
  walk_trips$trip_distance <- (walk_trips$trip_duration / 60) * MODE_SPEEDS$speed[MODE_SPEEDS$trip_mode=='Walking']
  bus_trips$trip_distance <- (bus_trips$trip_duration / 60 ) * MODE_SPEEDS$speed[MODE_SPEEDS$trip_mode=='Bus']
  
  # Recategorise trip_distance_cat for both bus and walk trips
  bus_trips$trip_distance_cat[bus_trips$trip_distance > 0 & bus_trips$trip_distance < DIST_LOWER_BOUNDS[2]] <- DIST_CAT[1]
  bus_trips$trip_distance_cat[bus_trips$trip_distance >= DIST_LOWER_BOUNDS[2] & bus_trips$trip_distance < DIST_LOWER_BOUNDS[3]] <- DIST_CAT[2]
  bus_trips$trip_distance_cat[bus_trips$trip_distance >= DIST_LOWER_BOUNDS[3]] <- DIST_CAT[3]
  
  
  walk_trips$trip_distance_cat[walk_trips$trip_distance > 0 & walk_trips$trip_distance < DIST_LOWER_BOUNDS[2]] <- DIST_CAT[1]
  walk_trips$trip_distance_cat[walk_trips$trip_distance >= DIST_LOWER_BOUNDS[2] & walk_trips$trip_distance < DIST_LOWER_BOUNDS[3]] <- DIST_CAT[2]
  walk_trips$trip_distance_cat[walk_trips$trip_distance >= DIST_LOWER_BOUNDS[3]] <- DIST_CAT[3]
  
  return(list(bus_trips, walk_trips))
  
}
