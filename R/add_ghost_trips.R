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
  
  # add base trips for cities for which there are no motorbike trips at all
  if (trip_mode == 'motorcycle' & city %in% c('antofagasta', 'arica', 'copiapo', 'coquimbo_laserena', 
                                              'iquique_altohospicio', 'osorno','temuco_padrelascasas', 'valdivia')){
    
    motorbike_proportion <- 0.012  # 1.2% 
    mbiketrips_per_person <- 3
    max_dur <- 90
    min_dur <- 10

    ind <- raw_trip_set
    # remove trips with NA as trip id
    ind <- ind %>% filter(!is.na(trip_id))
    
    total_trips <- length(unique(ind$trip_id))
    
    # assume proportion of total_trips are motorbike trips 
    mbike_trips <- round(total_trips * motorbike_proportion)
    
    # divide number of motorbike trips by the number of motorbike trips per person to see how many new person ids need to be added
    if (mbike_trips %% 3 == 1){
      mbike_trips <- mbike_trips - 1
    } else if (mbike_trips %% 3 == 2){
      mbike_trips <- mbike_trips + 1
    }
    
    new_person_id <- mbike_trips / mbiketrips_per_person
    
    mbike_speed <- MODE_SPEEDS %>% filter(stage_mode == 'motorcycle')
    mbike_speed <- mbike_speed$speed
    
    trip_duration <- round(runif( (mbike_trips), min_dur, max_dur))
    trip_distance <- trip_duration / mbike_speed
    
    
    new_trips <- data.frame(trip_id = c( (max(ind$trip_id) + 1):(max(ind$trip_id) + mbike_trips )), 
                            trip_mode = 'motorcycle', 
                            trip_distance = trip_distance,
                            #trip_duration = trip_duration, 
                            stage_mode = 'motorcycle',
                            stage_distance = trip_distance,
                            stage_duration = trip_duration,
                            participant_id = rep((max(ind$trip_id)+1):(max(ind$trip_id) + new_person_id), mbiketrips_per_person),
                            age = rep(round(runif(new_person_id, 15, 49)), mbiketrips_per_person),
                            sex = rep(sample(c('male','female'),new_person_id, replace = T), mbiketrips_per_person))
    
    
    raw_trip_set <- rbind(raw_trip_set, new_trips)
  }
  
  
  
  
  
  
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
