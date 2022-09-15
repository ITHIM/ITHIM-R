#' Add trips taken by no one
#' 
#' Creates trips based on distance relative to another (reference) mode, without altering the synthetic population
#' 
#' @param raw_trip_set data frame of trips
#' @param trip_mode which mode to add
#' @param distance_ratio fraction of reference distance to create
#' @param reference_mode name of reference mode
#' 
#' @return data frame of trips
#' 
#' @export
#add_ghost_trips <- function(raw_trip_set,trip_mode='bus_driver',distance_ratio=BUS_TO_PASSENGER_RATIO*DISTANCE_SCALAR_PT,reference_mode='bus'){

add_ghost_trips <- function(raw_trip_set,trip_mode='bus_driver',
                            distance_ratio=BUS_TO_PASSENGER_RATIO*DISTANCE_SCALAR_PT,
                            reference_mode='bus',
                            scenario = NA){ 
  
  ## values for new ghost journeys
  age_range <- AGE_LOWER_BOUNDS[1]:MAX_AGE
  nPeople <- 20
  nTrips <- 5
  new_gender <- 'male'
  total_ref_distance <- sum(raw_trip_set[raw_trip_set$stage_mode==reference_mode,]$stage_distance,na.rm=T)
  
  ## add new travel
  new_mode <- trip_mode
  total_new_distance <- total_ref_distance*distance_ratio
  # distance_range <- c(floor(total_new_distance/nPeople/nTrips),ceiling(total_new_distance/nPeople/nTrips))
  distance_range <- c(total_new_distance/nPeople/nTrips, total_new_distance/nPeople/nTrips)
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
    
    # if(trip_mode == 'car_driver'){
    #   
    #     age_category <- AGE_CATEGORY
    #     age_lower_bounds <- AGE_LOWER_BOUNDS
    #     for(j in 2:length(age_lower_bounds)-1){
    #       new_trips$age_cat[new_trips[['age']] >= age_lower_bounds[j] & new_trips[['age']] < age_lower_bounds[j+1]] <- age_category[j]
    #     }
    #     new_trips$age_cat[new_trips[['age']] >= age_lower_bounds[length(age_lower_bounds)]] <- age_category[length(age_lower_bounds)]
    #   
    # }
    
    
    if('age_cat' %in% names(raw_trip_set)){
      age_category <- AGE_CATEGORY
      age_lower_bounds <- AGE_LOWER_BOUNDS
      for(j in 2:length(age_lower_bounds)-1){
        new_trips$age_cat[new_trips[['age']] >= age_lower_bounds[j] & new_trips[['age']] < age_lower_bounds[j+1]] <- age_category[j]
      }
      new_trips$age_cat[new_trips[['age']] >= age_lower_bounds[length(age_lower_bounds)]] <- age_category[length(age_lower_bounds)]
    }
    
    if('scenario' %in% names(raw_trip_set)){
      new_trips$scenario <- scenario
    }
    
    if('trip_distance_cat' %in% names(raw_trip_set)){
      
      new_trips$trip_distance_cat[new_trips$trip_distance > 0 & new_trips$trip_distance < DIST_LOWER_BOUNDS[2]] <- DIST_CAT[1]
      new_trips$trip_distance_cat[new_trips$trip_distance >= DIST_LOWER_BOUNDS[2] & new_trips$trip_distance < DIST_LOWER_BOUNDS[3]] <- DIST_CAT[2]
      new_trips$trip_distance_cat[new_trips$trip_distance >= DIST_LOWER_BOUNDS[3]] <- DIST_CAT[3]
    }
    
    raw_trip_set <- plyr::rbind.fill(raw_trip_set, new_trips)
  }
  
  return(raw_trip_set)
  
}