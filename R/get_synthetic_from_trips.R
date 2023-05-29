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
  if(ADD_BUS_DRIVERS) raw_trip_set <- add_ghost_trips(raw_trip_set,prop_male = BUS_DRIVER_PROP_MALE,
                                                      agerange_male = BUS_DRIVER_MALE_AGERANGE,
                                                      agerange_female = BUS_DRIVER_FEMALE_AGERANGE)

  if(ADD_TRUCK_DRIVERS){
    if (ADD_CAR_DRIVERS){ # if know car occupancy ratio, convert km of people travelling by car into car vehicle km to get km travelled by truck
      raw_trip_set <- add_ghost_trips(raw_trip_set,trip_mode='truck',
                                      distance_ratio=CAR_OCCUPANCY_RATIO*TRUCK_TO_CAR_RATIO*DISTANCE_SCALAR_CAR_TAXI,reference_mode='car',
                                      prop_male = TRUCK_DRIVER_PROP_MALE,
                                      agerange_male = TRUCK_DRIVER_MALE_AGERANGE,
                                      agerange_female = TRUCK_DRIVER_FEMALE_AGERANGE)
    } else {
      raw_trip_set <- add_ghost_trips(raw_trip_set,trip_mode='truck',
                                      distance_ratio=TRUCK_TO_CAR_RATIO*DISTANCE_SCALAR_CAR_TAXI,reference_mode='car',
                                      prop_male = TRUCK_DRIVER_PROP_MALE,
                                      agerange_male = TRUCK_DRIVER_MALE_AGERANGE,
                                      agerange_female = TRUCK_DRIVER_FEMALE_AGERANGE)
    }
  } 

    
  ## because we have the fraction of total MC travel that is fleet, we need to adjust the parameter to compute fleet travel from non-fleet motorcycle travel
  
  if(ADD_PERSONAL_MOTORCYCLE_TRIPS != 'no'){
    if(ADD_PERSONAL_MOTORCYCLE_TRIPS == 'Chile')
      raw_trip_set <- add_motorcycle_trips_Chile(raw_trip_set, PROPORTION_MOTORCYCLE_TRIPS)
  }
    
    
  
  if(ADD_MOTORCYCLE_FLEET) 
    raw_trip_set <- add_ghost_trips(raw_trip_set,trip_mode='motorcycle',
                                    distance_ratio=FLEET_TO_MOTORCYCLE_RATIO/(1-FLEET_TO_MOTORCYCLE_RATIO),reference_mode='motorcycle',
                                    prop_male = COMMERCIAL_MBIKE_PROP_MALE,
                                    agerange_male = COMMERCIAL_MBIKE_MALE_AGERANGE,
                                    agerange_female = COMMERCIAL_MBIKE_FEMALE_AGERANGE)
    #raw_trip_set <- add_ghost_trips(raw_trip_set,trip_mode='motorcycle',distance_ratio=(1-FLEET_TO_MOTORCYCLE_RATIO)/FLEET_TO_MOTORCYCLE_RATIO,reference_mode='motorcycle')

  # create synthetic population
  synth_pop <- create_synth_pop(raw_trip_set)
  # add car drivers
  if(ADD_CAR_DRIVERS){
    car_driver_scalar <<- min(1, CAR_OCCUPANCY_RATIO*1/population_in_model_ratio)
    # age ranges are not needed as car drivers are only used to calculate total vehicle km travelled for the CO2 model but are not
    # needed for the injury model unlike truck, motorcycle and bus drivers 
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
  }else if (SCENARIO_NAME == "BOGOTA") {
    SYNTHETIC_TRIPS <- create_bogota_scenarios(trip_set)
  }else{
    SYNTHETIC_TRIPS <- create_all_scenarios(trip_set)
  }
  
  #set_scenario_specific_variables()
  # some useful variables.
  NSCEN <<- length(SYNTHETIC_TRIPS) - 1
  SCEN <<- sapply(SYNTHETIC_TRIPS,function(x)x$scenario[1])
  # SCEN_SHORT_NAME <<- c("base",paste0("scen", 1:NSCEN) )
  SCEN_SHORT_NAME <<- c("base",paste0("sc_", rownames(SCENARIO_PROPORTIONS)))
  
  
  # print(data.table::rbindlist(SYNTHETIC_TRIPS) %>% distinct(trip_id, scenario, .keep_all = T) %>% group_by(scenario) %>% summarise(sum(trip_distance)))
  # print(data.table::rbindlist(SYNTHETIC_TRIPS) %>% group_by(scenario) %>% summarise(sum(stage_distance)))
  
  # add walk-to-bus trips, as appropriate, and combines list of scenarios
  trip_scen_sets <- walk_to_pt_and_combine_scen(SYNTHETIC_TRIPS)
  trip_scen_sets
  
}


