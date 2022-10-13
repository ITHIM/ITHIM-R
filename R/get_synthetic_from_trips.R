#' Generate synthetic data from trip set
#' 
#' Sequence of functions to set up the synthetic population, the synthetic trips, and the scenarios.
#' Also sets global variables for later use.
#' 
#' @return data frame of all trips from all scenarios
#' 
#' @export
get_synthetic_from_trips <- function(){
  
  ##!! to get the right order of trip columns; needed if trips are added
  if (CITY == 'bogota_wb') {
    raw_trip_set <- data.frame(trip_id=TRIP_SET$trip_id,
                               trip_mode=TRIP_SET$trip_mode,
                               trip_distance=TRIP_SET$trip_distance,
                               stage_mode=TRIP_SET$stage_mode,
                               stage_distance=TRIP_SET$stage_distance,
                               stage_duration=TRIP_SET$stage_duration,
                               participant_id=TRIP_SET$participant_id,
                               age=TRIP_SET$age,
                               sex=TRIP_SET$sex,
                               strata = TRIP_SET$strata,
                               limitation = TRIP_SET$limitation,
                               trip_motive = TRIP_SET$trip_motive,
                               trip_start_time = TRIP_SET$trip_start_time,
                               trip_end_time = TRIP_SET$trip_end_time,
                               stringsAsFactors = F)
  } else {
    raw_trip_set <- data.frame(trip_id=TRIP_SET$trip_id,
                             trip_mode=TRIP_SET$trip_mode,
                             trip_distance=TRIP_SET$trip_distance,
                             stage_mode=TRIP_SET$stage_mode,
                             stage_distance=TRIP_SET$stage_distance,
                             stage_duration=TRIP_SET$stage_duration,
                             participant_id=TRIP_SET$participant_id,
                             age=TRIP_SET$age,
                             sex=TRIP_SET$sex, 
                             stringsAsFactors = F)
  }
  TRIP_SET <- NULL
  
  ##!! number trips
  #raw_trip_set$trip_id[!is.na(raw_trip_set$trip_id)] <- 1:sum(!is.na(raw_trip_set$trip_id))
  
  ## add motorcycle trip to accra, and replicate set four times
  #if(CITY=='accra') raw_trip_set <- edit_accra_trips(raw_trip_set)
  #SURVEY_SCALAR <<- population/length(unique(TRIP_SET$participant_id))/survey_coverage
  
  ## add bus and truck trips
  if(ADD_BUS_DRIVERS) raw_trip_set <- add_ghost_trips(raw_trip_set)

  if(ADD_TRUCK_DRIVERS) raw_trip_set <- add_ghost_trips(raw_trip_set,trip_mode='truck',distance_ratio=TRUCK_TO_CAR_RATIO*DISTANCE_SCALAR_CAR_TAXI,reference_mode='car')
  
  ## because we have the fraction of total MC travel that is fleet, we need to adjust the parameter to compute fleet travel from non-fleet motorcycle travel
  
  if(ADD_PERSONAL_MOTORCYCLE_TRIPS != 'no'){
    if(ADD_PERSONAL_MOTORCYCLE_TRIPS == 'Chile')
      raw_trip_set <- add_motorcycle_trips_Chile(raw_trip_set, PROPORTION_MOTORCYCLE_TRIPS)
  }
    
    
  
  if(ADD_MOTORCYCLE_FLEET) 
    raw_trip_set <- add_ghost_trips(raw_trip_set,trip_mode='motorcycle',distance_ratio=FLEET_TO_MOTORCYCLE_RATIO/(1-FLEET_TO_MOTORCYCLE_RATIO),reference_mode='motorcycle')
    #raw_trip_set <- add_ghost_trips(raw_trip_set,trip_mode='motorcycle',distance_ratio=(1-FLEET_TO_MOTORCYCLE_RATIO)/FLEET_TO_MOTORCYCLE_RATIO,reference_mode='motorcycle')

  # create synthetic population
  synth_pop <- create_synth_pop(raw_trip_set)
  # add car drivers
  if(ADD_CAR_DRIVERS){
    car_driver_scalar <<- min(1, CAR_OCCUPANCY_RATIO*1/population_in_model_ratio)
    synth_pop$trip_set<- add_ghost_trips(synth_pop$trip_set,trip_mode='car_driver',
                                    distance_ratio=car_driver_scalar*DISTANCE_SCALAR_CAR_TAXI,reference_mode='car')  }   
  raw_trip_set <- NULL
  SYNTHETIC_POPULATION <<- synth_pop$synthetic_population
  trip_set <- synth_pop$trip_set
  synth_pop <- NULL
  
  # create scenarios: either the pedestrian test case, or the 5 hard-coded Accra scenarios
  trip_set <- ithim_setup_baseline_scenario(trip_set)
  if (SCENARIO_NAME == "TEST_WALK_SCENARIO") {
    SYNTHETIC_TRIPS <- create_walk_scenario(trip_set)
  }else if (SCENARIO_NAME == "TEST_CYCLE_SCENARIO") {
    SYNTHETIC_TRIPS <- create_cycle_scenarios(trip_set)
  }else if (SCENARIO_NAME == "MAX_MODE_SHARE_SCENARIO") {
    SYNTHETIC_TRIPS <- create_max_mode_share_scenarios(trip_set)
  }else if (SCENARIO_NAME == "LATAM") {
    SYNTHETIC_TRIPS <- create_latam_scenarios(trip_set)
  }else if (SCENARIO_NAME == "GLOBAL") {
    SYNTHETIC_TRIPS <- create_global_scenarios(trip_set)
  }else if (SCENARIO_NAME == "AFRICA_INDIA") {
    SYNTHETIC_TRIPS <- create_africa_india_scenarios(trip_set)
  }else{
    SYNTHETIC_TRIPS <- create_all_scenarios(trip_set)
  }
  
  #set_scenario_specific_variables()
  # some useful variables.
  NSCEN <<- length(SYNTHETIC_TRIPS) - 1
  SCEN <<- sapply(SYNTHETIC_TRIPS,function(x)x$scenario[1])
  SCEN_SHORT_NAME <<- c("base",paste0("scen", 1:NSCEN) )
  
  # print(data.table::rbindlist(SYNTHETIC_TRIPS) %>% distinct(trip_id, scenario, .keep_all = T) %>% group_by(scenario) %>% summarise(sum(trip_distance)))
  # print(data.table::rbindlist(SYNTHETIC_TRIPS) %>% group_by(scenario) %>% summarise(sum(stage_distance)))
  
  # add walk-to-bus trips, as appropriate, and combines list of scenarios
  trip_scen_sets <- walk_to_pt_and_combine_scen(SYNTHETIC_TRIPS)
  trip_scen_sets
  
}




# add base trips for cities for which there are no motorbike trips at all
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



