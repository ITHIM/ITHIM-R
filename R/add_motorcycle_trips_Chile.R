#' function to add motorcycle trips to Chilean cities that do not have any motorcycle trips themselves
#' @param raw_trip_set data frame of trips
#' @param PROPORTION_MOTORCYCLE_TRIPS proportion of trips in travel survey that are to be added as personal motorcycle trips
#' 
#' @export



add_motorcycle_trips_Chile <- function(raw_trip_set,PROPORTION_MOTORCYCLE_TRIPS){
  
  motorbike_proportion <- PROPORTION_MOTORCYCLE_TRIPS  
  mbiketrips_per_person <- 2
  min_age_mbike <- 15
  max_age_mbike <- 65
  prop_male <- 0.84
  prop_female <- 0.16
  
  # set up parameters for trip duration - truncated normal distribution
  mean_dur_male <- 25
  sd_dur_male <- 24
  min_dur_male <- 5
  max_dur_male <- 100
  
  mean_dur_female <- 14
  sd_dur_female <- 13
  min_dur_female <- 5
  max_dur_female <- 80
  
  # set up parameters for age of motorbike users - truncated normal distribution
  mean_age_male <- 33
  sd_age_male <- 15
  min_age_male <- 15
  max_age_male <- 65
  
  mean_age_female <- 35
  sd_age_female <- 10
  min_age_female <- 15 
  max_age_female <- 65
  
  
  ind <- raw_trip_set
  # remove trips with NA as trip id
  ind <- ind %>% filter(!is.na(trip_id))
  
  total_trips <- length(unique(ind$trip_id))
  
  # assume proportion of total_trips are motorbike trips 
  mbike_trips_male <- round(total_trips * motorbike_proportion * prop_male)
  mbike_trips_female <- round(total_trips * motorbike_proportion * prop_female)
  
  # divide number of motorbike trips by the number of motorbike trips per person to see how many new person ids need to be added
  if (mbike_trips_male %% 2 == 1){
    mbike_trips_male <- mbike_trips_male + 1
  }
    
    
  if (mbike_trips_female %% 2 == 1){
    mbike_trips_female <- mbike_trips_female + 1
  }
    
 
  # define the number of new person ids
  new_person_id_male <- mbike_trips_male / mbiketrips_per_person
  new_person_id_female <- mbike_trips_female / mbiketrips_per_person
  
  mbike_speed <- MODE_SPEEDS %>% filter(stage_mode == 'motorcycle')
  mbike_speed <- mbike_speed$speed
  
  # define trip duration and distance
  trip_duration_male <- round(truncnorm::rtruncnorm(mbike_trips_male, mean = mean_dur_male, sd = sd_dur_male , a = min_dur_male , b = max_dur_male ))
  trip_duration_female <- round(truncnorm::rtruncnorm(mbike_trips_female, mean = mean_dur_female, sd = sd_dur_female , a = min_dur_female , b = max_dur_female ))
  trip_distance_male <- trip_duration_male / mbike_speed
  trip_distance_female <- trip_duration_female / mbike_speed
  
  new_trips_male <- data.frame(trip_id = c( (max(ind$trip_id) + 1):(max(ind$trip_id) + mbike_trips_male )), 
                          trip_mode = 'motorcycle', 
                          trip_distance = trip_distance_male,
                          #trip_duration = trip_duration, 
                          stage_mode = 'motorcycle',
                          stage_distance = trip_distance_male,
                          stage_duration = trip_duration_male,
                          participant_id = rep((max(ind$trip_id)+1):(max(ind$trip_id) + new_person_id_male), mbiketrips_per_person),
                          age = rep(round(truncnorm::rtruncnorm(new_person_id_male, mean = mean_age_male, sd = sd_age_male,
                                                                a = min_age_male , b = max_age_male )), mbiketrips_per_person),
                          sex = 'male')
  
  new_trips_female <- data.frame(trip_id = c( (max(ind$trip_id) + mbike_trips_male + 1):(max(ind$trip_id) + mbike_trips_male + mbike_trips_female )), 
                               trip_mode = 'motorcycle', 
                               trip_distance = trip_distance_female,
                               #trip_duration = trip_duration, 
                               stage_mode = 'motorcycle',
                               stage_distance = trip_distance_female,
                               stage_duration = trip_duration_female,
                               participant_id = rep((max(ind$trip_id) + new_person_id_male+1):
                                                      (max(ind$trip_id) + new_person_id_male + new_person_id_female), mbiketrips_per_person),
                               age = rep(round(truncnorm::rtruncnorm(new_person_id_female, mean = mean_age_female, sd = sd_age_female,
                                                                     a = min_age_female , b = max_age_female )), mbiketrips_per_person),
                               sex = 'female')
  
  
  raw_trip_set <- rbind(raw_trip_set, new_trips_male, new_trips_female)
  
  raw_trip_set
}



