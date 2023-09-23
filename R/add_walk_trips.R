#' Addition of 'walk to public transport' stages to trip set
#' 
#' Add walk to 'public transport stages' to those public transport trips that do not have a walking stage
#' 
#' Function to add additional walking stages to those public transport trips that do not have a walking stage 
#' and have a longer trip duration than the MINIMUM_PT_TIME plus the BUS_WALK_TIME / RAIL_WALK_TIME parameters.
#' The MINIMUM_PT_TIME is a global input parameter that determines the minimum amount of time  (in minutes) a
#' person travelling on public transport spends on the public transport stage. The BUS_WALK_TIME / RAIL_WALK_TIME 
#' parameter is a city specific input parameter that determines the duration of the walk to public transport walking stage. 
#' 
#' This function performs the following steps:
#' 
#' \itemize{
#' \item filter out the PT stages of PT trips and only keep the stage with the longest duration as we
#'   only add one 'walk to public transport' stage per trip
#'
#' \item find the trip stages that need changing
#' 
#' \item add new 'walk to pt' stages. If the duration of the public transport stage is shorter than  
#'   BUS_WALK_TIME / RAIL_WALK_TIME + MINIMUM_PT_TIME, set the new stage duration to 3. Otherwise, 
#'   set the new stage duration to either BUS_WALK_TIME or RAIL_WALK_TIME depending on the trip mode
#'
#' \item update the original public transport stage duration
#' 
#' \item calculate the 'walk to pt' stage distance using the walking speed
#'
#' \item update the original public transport stage distance using the respective public transport
#'   speed and the updated duration
#' 
#' \item keep record of the number of newly added 'walk to pt' stages
#'
#' \item update the total trip distances
#'
#' \item update the trip distance categories
#' 
#' \item remove any 'walk to pt' stages with zero duration and length
#' }
#' 
#' 
#' @param pt_trips data frame of PT trips without walking component
#' 
#' @return list of data frames of PT trips and walk-to-PT trips
#' 
#' @export
add_walk_trips <- function(pt_trips){
  
  min_pt_time <- MINIMUM_PT_TIME
  
  # filter out stage modes that are PT
  walk_trips <- pt_trips[pt_trips$stage_mode %in% c('bus','minibus','rail','subway'),] 

  # only keep one PT stage (the one with the highest stage duration) for each trip id, as only want to add walk to pt once for each trip
  walk_trips <- walk_trips %>% 
    group_by(trip_id) %>% 
    filter(stage_duration == max(stage_duration)) %>% 
    slice(1) %>% 
    ungroup() 
  
  # find trip stages that remain unchanged
  pt_trips_unchanged <- anti_join(pt_trips, walk_trips, by = 'id')
  
  # find trip stages that need changing, i.e. PT stage distance and duration need updating
  pt_trips_to_change <- anti_join(pt_trips, pt_trips_unchanged, by = 'id')
    
  # add new stage_mode as walk to pt  
  walk_trips$stage_mode_new <- 'walk_to_pt'

  # Replace walk trips with duration greater than that of (pt time + min_pt_time) with new pt time, else set time to 0
  # Walk_to_pt for bus trips
  walk_trips$stage_duration_new <- ifelse(((walk_trips$stage_duration > BUS_WALK_TIME + min_pt_time) & 
                              (walk_trips$stage_mode == 'bus' | walk_trips$stage_mode == 'minibus')), BUS_WALK_TIME,0)
  # Walk_to_pt for rail trips
  walk_trips$stage_duration_new <- ifelse(((walk_trips$stage_duration > RAIL_WALK_TIME + min_pt_time) &
                              (walk_trips$stage_mode == 'rail' | walk_trips$stage_mode == 'subway')), RAIL_WALK_TIME,
                              walk_trips$stage_duration_new)
  
  # Remove walk_to_pt duration from trip duration for bus trips
  pt_trips_to_change$stage_duration <- ifelse(
    ((pt_trips_to_change$stage_duration > BUS_WALK_TIME + min_pt_time) & 
       (pt_trips_to_change$stage_mode == 'bus' | pt_trips_to_change$stage_mode == 'minibus')),
    pt_trips_to_change$stage_duration - BUS_WALK_TIME, 
    pt_trips_to_change$stage_duration)

  # Remove walk_to_pt duration from trip duration for rail trips
  pt_trips_to_change$stage_duration <- ifelse(
    ((pt_trips_to_change$stage_duration > RAIL_WALK_TIME + min_pt_time) &
       (pt_trips_to_change$stage_mode == 'rail' | pt_trips_to_change$stage_mode == 'subway')), 
    pt_trips_to_change$stage_duration - RAIL_WALK_TIME, 
    pt_trips_to_change$stage_duration)
  
  # Correct walk trips distance
  walk_trips$stage_distance_new <- (walk_trips$stage_duration_new / 60) * VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$stage_mode=='pedestrian']
   
  # Correct pt stage distances using the updated stage duration and the mode speeds
  pt_trips_to_change$stage_distance[(pt_trips_to_change$stage_mode == 'bus')] <- (pt_trips_to_change$stage_duration[(pt_trips_to_change$stage_mode == 'bus')] / 60
                                                              ) * VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$stage_mode=='bus']
  pt_trips_to_change$stage_distance[(pt_trips_to_change$stage_mode == 'minibus')] <- (pt_trips_to_change$stage_duration[(pt_trips_to_change$stage_mode == 'minibus')] / 60
                                                               ) * VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$stage_mode=='minibus']
  pt_trips_to_change$stage_distance[(pt_trips_to_change$stage_mode == 'subway')] <- (pt_trips_to_change$stage_duration[(pt_trips_to_change$stage_mode == 'subway')] / 60
                                                              ) * VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$stage_mode=='subway']
  pt_trips_to_change$stage_distance[(pt_trips_to_change$stage_mode == 'rail')] <- (pt_trips_to_change$stage_duration[(pt_trips_to_change$stage_mode == 'rail')] / 60
                                                              ) * VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$stage_mode=='rail']
  
  # save the number of new non-zero walk trips for both rail and bus
  count_new_walk_trips_bus <<- nrow(walk_trips %>% filter(stage_duration_new > 0 & (stage_mode == 'bus' | stage_mode == 'minibus')))
  count_new_walk_trips_rail <<- nrow(walk_trips %>% filter(stage_duration_new > 0 & (stage_mode == 'rail' | stage_mode == 'subway')))
  count_new_walk_trips <<- nrow(walk_trips %>% filter(stage_duration_new > 0))
  
  # updated stage and distance duration
  walk_trips$stage_distance <- walk_trips$stage_distance_new
  walk_trips$stage_duration <- walk_trips$stage_duration_new
  walk_trips$stage_mode <- walk_trips$stage_mode_new
  
  walk_trips <- walk_trips %>% 
    dplyr::select(-c(stage_distance_new, stage_duration_new, stage_mode_new))
  

  
  # merge datasets to calculate total trip distance by summing across the various stage distances
  all_trips <- rbind(pt_trips_to_change, walk_trips, pt_trips_unchanged)
  
  trip_dist <- all_trips %>% 
    group_by(trip_id) %>% 
    summarise(trip_distance_new = sum(stage_distance))
  
  all_trips <- full_join(all_trips, trip_dist, by = 'trip_id')
  
  all_trips$trip_distance <- all_trips$trip_distance_new
  all_trips <- all_trips %>% dplyr::select(-c(trip_distance_new))
  
  
  # Recategorise trip_distance_cat 
  all_trips$trip_distance_cat[all_trips$trip_distance > 0 & all_trips$trip_distance < DIST_LOWER_BOUNDS[2]] <- DIST_CAT[1]
  all_trips$trip_distance_cat[all_trips$trip_distance >= DIST_LOWER_BOUNDS[2] & all_trips$trip_distance < DIST_LOWER_BOUNDS[3]] <- DIST_CAT[2]
  all_trips$trip_distance_cat[all_trips$trip_distance >= DIST_LOWER_BOUNDS[3]] <- DIST_CAT[3]
  
  # split into walk and pt trips again and remove 0 distance walk trips
  pt_trips2 <- all_trips %>% filter(stage_mode != 'walk_to_pt')
  walk_trips2 <- all_trips %>% filter(stage_mode == 'walk_to_pt')
  
  walk_trips2 <- walk_trips2 %>% filter(stage_distance > 0)
  

  
  
  return(list(pt_trips2, walk_trips2))
  
}
