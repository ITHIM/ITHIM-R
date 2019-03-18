#' @export
edit_accra_trips <- function(raw_trip_set){
  
  total_car_distance <- sum(subset(raw_trip_set,stage_mode=='car')$trip_distance)
  
  # Redefine motorcycle mode for a select 14 rows
  raw_trip_set$trip_mode[raw_trip_set$trip_mode=='other'&raw_trip_set$trip_distance<30] <- 'motorcycle'
  raw_trip_set <- subset(raw_trip_set,trip_mode!='other')
  raw_trip_set$stage_mode[raw_trip_set$stage_mode=='other'&raw_trip_set$stage_distance<30] <- 'motorcycle'
  raw_trip_set <- subset(raw_trip_set,stage_mode!='other')
  
  # Create new motorbike trips
  # Add 4 new people with 3 trips each
  # Age: 15-59 and gender: male
  new_mode <- 'motorcycle'
  total_mc_distance <- total_car_distance*MOTORCYCLE_TO_CAR_RATIO#VEHICLE_INVENTORY$distance_ratio_to_car[VEHICLE_INVENTORY$trip_mode==new_mode]
  residual_mc_distance <- total_mc_distance - sum(subset(raw_trip_set,stage_mode==new_mode)$trip_distance)
  #duration_range <- 15:100
  nTrips <- 1
  nPeople <- 20#round(residual_mc_duration/nTrips/mean(duration_range))
  distance <- residual_mc_distance/nPeople
  new_gender <- c(rep('Male',20),'Female')
  age_range <- AGE_LOWER_BOUNDS[1]:MAX_AGE
  speed <- MODE_SPEEDS$speed[MODE_SPEEDS$stage_mode==new_mode]
  for(i in 1:nPeople){
    new_trips <- add_trips(trip_ids   = max(raw_trip_set$trip_id) + 1: nTrips, 
                           new_mode = new_mode, 
                           distance = distance, 
                           participant_id = max(raw_trip_set$participant_id) + 1,
                           age = age_range,
                           sex = new_gender,
                           nTrips=nTrips,
                           speed=speed)
    # Add new motorbikes trips to baseline
    raw_trip_set <- rbind(raw_trip_set, new_trips)
  }
  
  # Multiply raw_trip_set by 4 to have a bigger number of trips (and raw_trip_set)
  ind1 <- raw_trip_set
  ind1$participant_id <- ind1$participant_id + max(raw_trip_set$participant_id)
  ind1$trip_id <- (max(raw_trip_set$trip_id) + 1): (max(raw_trip_set$trip_id) + nrow(ind1))
  raw_trip_set <- rbind(raw_trip_set, ind1)
  
  ind1 <- raw_trip_set
  ind1$participant_id <- ind1$participant_id + max(raw_trip_set$participant_id)
  ind1$trip_id <- (max(raw_trip_set$trip_id) + 1): (max(raw_trip_set$trip_id) + nrow(ind1))
  raw_trip_set <- rbind(raw_trip_set, ind1)
  
  return(raw_trip_set)
  
}
