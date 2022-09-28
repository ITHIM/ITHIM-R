#' Add pedestrian trips to trip set
#' 
#' Create data frame of walk-to-PT trips from PT trips and walk-to-bus time
#' 
#' @param pt_trips data frame of PT trips
#' 
#' @return list of data frames of PT trips and walk-to-PT trips
#' 
#' @export
add_walk_trips <- function(pt_trips){
  
  # pt_trips
  # ln_mean = 5
  # ln_sd = 1.2
  #pt_trips <- arrange(pt_trips, trip_duration)
  walk_trips <- pt_trips
  walk_trips$stage_mode <- 'walk_to_pt'
  
  # Initialize with 0 duration
  walk_trips$stage_duration <- walk_trips$stage_distance <- 0
  ##RJ all trips have the same BUS_WALK_TIME
  # walk_trips$stage_duration <- 0
  
  # Replace walk trips with duration greater than that of bus needs to be set to 0
 # if (any(pt_trips$stage_duration >= 25)){
    # Walk_to_pt for bus trips
  walk_trips$stage_duration[(pt_trips$stage_duration > BUS_WALK_TIME) & 
                              (pt_trips$trip_mode == 'bus' | pt_trips$trip_mode == 'minibus')] <- BUS_WALK_TIME
  # Walk_to_pt for rail trips
  walk_trips$stage_duration[(pt_trips$stage_duration > RAIL_WALK_TIME) &
                              (pt_trips$trip_mode == 'rail' | pt_trips$trip_mode == 'subway')] <- RAIL_WALK_TIME
  
  # Remove walk_to_pt duration from trip duration for bus trips
  pt_trips$stage_duration[(pt_trips$stage_duration > BUS_WALK_TIME) & 
                            (pt_trips$trip_mode == 'bus' | pt_trips$trip_mode == 'minibus')] <-
    pt_trips$stage_duration[(pt_trips$stage_duration > BUS_WALK_TIME) & 
                              (pt_trips$trip_mode == 'bus' | pt_trips$trip_mode == 'minibus')] - BUS_WALK_TIME
  # Remove walk_to_pt duration from trip duration for rail trips
  pt_trips$stage_duration[(pt_trips$stage_duration > RAIL_WALK_TIME) &
                            (pt_trips$trip_mode == 'rail' | pt_trips$trip_mode == 'subway')] <-
    pt_trips$stage_duration[(pt_trips$stage_duration > RAIL_WALK_TIME) &
                              (pt_trips$trip_mode == 'rail' | pt_trips$trip_mode == 'subway')] - RAIL_WALK_TIME
  
  # Replace walk trips with duration greater than that of bus needs to be set to 0
  if (any(walk_trips$stage_duration - pt_trips$stage_duration  >= 0))
    walk_trips$stage_duration[(walk_trips$stage_duration - pt_trips$stage_duration)  >= 0] <- 0
  
  
  # Correct walk trips distance
  walk_trips$stage_distance <- (walk_trips$stage_duration / 60) * VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$stage_mode=='pedestrian']
  #pt_trips$stage_distance <- pt_trips$stage_distance - walk_trips$stage_distance
  
  # Correct pt trip distances using the updated stage duration and the mode speeds
  pt_trips$stage_distance[(pt_trips$stage_mode == 'bus')] <- (pt_trips$stage_duration[(pt_trips$stage_mode == 'bus')] / 60
                                                              ) * VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$stage_mode=='bus']
  pt_trips$stage_distance[(pt_trips$stage_mode == 'minibus')] <- (pt_trips$stage_duration[(pt_trips$stage_mode == 'minibus')] / 60
                                                               ) * VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$stage_mode=='minibus']
  pt_trips$stage_distance[(pt_trips$stage_mode == 'subway')] <- (pt_trips$stage_duration[(pt_trips$stage_mode == 'subway')] / 60
                                                              ) * VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$stage_mode=='subway']
  pt_trips$stage_distance[(pt_trips$stage_mode == 'rail')] <- (pt_trips$stage_duration[(pt_trips$stage_mode == 'rail')] / 60
                                                              ) * VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$stage_mode=='rail']
  
  # correct trip distance by converting current distance into duration, subtracting the walk to pt times and converting back to trip distance
  pt_trips$trip_duration <- 0
  pt_trips$trip_duration[(pt_trips$stage_mode == 'bus')] <- (pt_trips$trip_distance[(pt_trips$stage_mode == 'bus')] /
                                                               VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$stage_mode=='bus'])*60 
    
  pt_trips$trip_duration[(pt_trips$stage_mode == 'minibus')] <- (pt_trips$trip_distance[(pt_trips$stage_mode == 'minibus')] /
                                                               VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$stage_mode=='minibus'])*60   
  pt_trips$trip_duration[(pt_trips$stage_mode == 'rail')] <- (pt_trips$trip_distance[(pt_trips$stage_mode == 'rail')] /
                                                               VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$stage_mode=='rail'])*60   
  pt_trips$trip_duration[(pt_trips$stage_mode == 'subway')] <- (pt_trips$trip_distance[(pt_trips$stage_mode == 'subway')] /
                                                               VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$stage_mode=='subway'])*60   
  
  # subtract any walk time  
  pt_trips$trip_duration <- pt_trips$trip_duration - walk_trips$stage_duration
  
  # convert back to trip_distance
  pt_trips$trip_distance[(pt_trips$stage_mode == 'bus')] <- (pt_trips$trip_duration[(pt_trips$stage_mode == 'bus')] / 60
                                                              ) * VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$stage_mode=='bus']
  pt_trips$trip_distance[(pt_trips$stage_mode == 'minibus')] <- (pt_trips$trip_duration[(pt_trips$stage_mode == 'minibus')] / 60
                                                                  ) * VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$stage_mode=='minibus']
  pt_trips$trip_distance[(pt_trips$stage_mode == 'subway')] <- (pt_trips$trip_duration[(pt_trips$stage_mode == 'subway')] / 60
                                                                 ) * VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$stage_mode=='subway']
  pt_trips$trip_distance[(pt_trips$stage_mode == 'rail')] <- (pt_trips$trip_duration[(pt_trips$stage_mode == 'rail')] / 60
                                                               ) * VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$stage_mode=='rail']

  
  # Recategorise trip_distance_cat for both bus and walk trips
  pt_trips$trip_distance_cat[pt_trips$trip_distance > 0 & pt_trips$trip_distance < DIST_LOWER_BOUNDS[2]] <- DIST_CAT[1]
  pt_trips$trip_distance_cat[pt_trips$trip_distance >= DIST_LOWER_BOUNDS[2] & pt_trips$trip_distance < DIST_LOWER_BOUNDS[3]] <- DIST_CAT[2]
  pt_trips$trip_distance_cat[pt_trips$trip_distance >= DIST_LOWER_BOUNDS[3]] <- DIST_CAT[3]
  
  walk_trips$trip_distance_cat[walk_trips$trip_distance > 0 & walk_trips$trip_distance < DIST_LOWER_BOUNDS[2]] <- DIST_CAT[1]
  walk_trips$trip_distance_cat[walk_trips$trip_distance >= DIST_LOWER_BOUNDS[2] & walk_trips$trip_distance < DIST_LOWER_BOUNDS[3]] <- DIST_CAT[2]
  walk_trips$trip_distance_cat[walk_trips$trip_distance >= DIST_LOWER_BOUNDS[3]] <- DIST_CAT[3]
  
  #}
  
  return(list(pt_trips, walk_trips))
  
}
