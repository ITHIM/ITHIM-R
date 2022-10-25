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
  walk_trips <- pt_trips[pt_trips$stage_mode %in% c('bus','minibus','rail','subway'),]
  
  walk_trips$stage_mode_new <- 'walk_to_pt'
  
  # Initialize with 0 duration
  #walk_trips$stage_duration <- walk_trips$stage_distance <- 0
  ##RJ all trips have the same BUS_WALK_TIME
  # walk_trips$stage_duration <- 0
  
  # Replace walk trips with duration greater than that of bus needs to be set to 0
 # if (any(pt_trips$stage_duration >= 25)){
    # Walk_to_pt for bus trips
  walk_trips$stage_duration_new <- ifelse(((walk_trips$stage_duration > BUS_WALK_TIME + 1) & 
                              (walk_trips$trip_mode == 'bus' | walk_trips$trip_mode == 'minibus')&
                              (walk_trips$stage_mode == 'bus' | walk_trips$stage_mode == 'minibus')), BUS_WALK_TIME,0)
  # Walk_to_pt for rail trips
  walk_trips$stage_duration_new <- ifelse(((walk_trips$stage_duration > RAIL_WALK_TIME + 1) &
                              (walk_trips$trip_mode == 'rail' | walk_trips$trip_mode == 'subway')&
                              (walk_trips$stage_mode == 'rail' | walk_trips$stage_mode == 'subway')), RAIL_WALK_TIME,
                              walk_trips$stage_duration_new)
  
  # Remove walk_to_pt duration from trip duration for bus trips
  pt_trips$stage_duration <- ifelse(((pt_trips$stage_duration > BUS_WALK_TIME + 1) & 
                                      (pt_trips$trip_mode == 'bus' | pt_trips$trip_mode == 'minibus') &
                                       (pt_trips$stage_mode == 'bus' | pt_trips$stage_mode == 'minibus')),
                                    pt_trips$stage_duration - BUS_WALK_TIME, pt_trips$stage_duration)

  # Remove walk_to_pt duration from trip duration for rail trips
  pt_trips$stage_duration <- ifelse(((pt_trips$stage_duration > RAIL_WALK_TIME + 1) &
                            (pt_trips$trip_mode == 'rail' | pt_trips$trip_mode == 'subway') &
                              (pt_trips$stage_mode == 'rail' | pt_trips$stage_mode == 'subway')), 
                            pt_trips$stage_duration - RAIL_WALK_TIME, pt_trips$stage_duration)
  
  
  # Replace walk trips with duration greater than that of bus needs to be set to 0
  # if (any(walk_trips$stage_duration - pt_trips$stage_duration  >= 0))
  #   walk_trips$stage_duration[(walk_trips$stage_duration - pt_trips$stage_duration)  >= 0] <- 0
  
  
  # Correct walk trips distance
  walk_trips$stage_distance_new <- (walk_trips$stage_duration_new / 60) * VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$stage_mode=='pedestrian']
  #pt_trips$stage_distance <- pt_trips$stage_distance - walk_trips$stage_distance
  
  # Correct pt stage distances using the updated stage duration and the mode speeds
  pt_trips$stage_distance[(pt_trips$stage_mode == 'bus')] <- (pt_trips$stage_duration[(pt_trips$stage_mode == 'bus')] / 60
                                                              ) * VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$stage_mode=='bus']
  pt_trips$stage_distance[(pt_trips$stage_mode == 'minibus')] <- (pt_trips$stage_duration[(pt_trips$stage_mode == 'minibus')] / 60
                                                               ) * VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$stage_mode=='minibus']
  pt_trips$stage_distance[(pt_trips$stage_mode == 'subway')] <- (pt_trips$stage_duration[(pt_trips$stage_mode == 'subway')] / 60
                                                              ) * VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$stage_mode=='subway']
  pt_trips$stage_distance[(pt_trips$stage_mode == 'rail')] <- (pt_trips$stage_duration[(pt_trips$stage_mode == 'rail')] / 60
                                                              ) * VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$stage_mode=='rail']
  
  
  # updated stage and distance duration
  walk_trips$stage_distance <- walk_trips$stage_distance_new
  walk_trips$stage_duration <- walk_trips$stage_duration_new
  walk_trips$stage_mode <- walk_trips$stage_mode_new
  
  walk_trips <- walk_trips %>% dplyr::select(-c(stage_distance_new, stage_duration_new, stage_mode_new))
  
  
  
  # merge two datasets to calculate total trip distance by summing across the various stage distances
  all_trips <- rbind(pt_trips, walk_trips)
  
  trip_dist <- all_trips %>% group_by(trip_id) %>% summarise(trip_distance_new = sum(stage_distance))
  
  all_trips <- full_join(all_trips, trip_dist, by = 'trip_id')
  
  all_trips$trip_distance <- all_trips$trip_distance_new
  all_trips <- all_trips %>% dplyr::select(-c(trip_distance_new))
  
  
  # Recategorise trip_distance_cat for both bus and walk trips
  all_trips$trip_distance_cat[all_trips$trip_distance > 0 & all_trips$trip_distance < DIST_LOWER_BOUNDS[2]] <- DIST_CAT[1]
  all_trips$trip_distance_cat[all_trips$trip_distance >= DIST_LOWER_BOUNDS[2] & all_trips$trip_distance < DIST_LOWER_BOUNDS[3]] <- DIST_CAT[2]
  all_trips$trip_distance_cat[all_trips$trip_distance >= DIST_LOWER_BOUNDS[3]] <- DIST_CAT[3]
  
  # split into walk and pt trips again and remove 0 distance walk trips
  pt_trips2 <- all_trips %>% filter(stage_mode != 'walk_to_pt')
  walk_trips2 <- all_trips %>% filter(stage_mode == 'walk_to_pt')
  
  walk_trips2 <- walk_trips2 %>% filter(stage_distance > 0)
  
  
  return(list(pt_trips2, walk_trips2))
  
}
