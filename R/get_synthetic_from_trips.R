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
  if(ADD_MOTORCYCLE_FLEET) 
    raw_trip_set <- add_ghost_trips(raw_trip_set,trip_mode='motorcycle',distance_ratio=FLEET_TO_MOTORCYCLE_RATIO/(1-FLEET_TO_MOTORCYCLE_RATIO),reference_mode='motorcycle')

  # create synthetic population
  synth_pop <- create_synth_pop(raw_trip_set)
  raw_trip_set <- NULL
  SYNTHETIC_POPULATION <<- synth_pop$synthetic_population
  trip_set <- synth_pop$trip_set
  synth_pop <- NULL
  
  # create scenarios: either the walking test case, or the 5 hard-coded Accra scenarios
  trip_set <- ithim_setup_baseline_scenario(trip_set)
  if(TEST_WALK_SCENARIO){
    SYNTHETIC_TRIPS <- create_walk_scenario(trip_set)
  }else if(TEST_CYCLE_SCENARIO){
    SYNTHETIC_TRIPS <- create_cycle_scenarios(trip_set)
  }else if(MAX_MODE_SHARE_SCENARIO){
    SYNTHETIC_TRIPS <- create_max_mode_share_scenarios(trip_set)
  }else{
    SYNTHETIC_TRIPS <- create_all_scenarios(trip_set)
  }
  
  #set_scenario_specific_variables()
  # some useful variables.
  NSCEN <<- length(SYNTHETIC_TRIPS) - 1
  SCEN <<- sapply(SYNTHETIC_TRIPS,function(x)x$scenario[1])
  SCEN_SHORT_NAME <<- c("base",paste0("scen", 1:NSCEN) )
  
  # add walk-to-bus trips, as appropriate, and combines list of scenarios
  trip_scen_sets <- walk_to_pt_and_combine_scen(SYNTHETIC_TRIPS)
  trip_scen_sets
  
}
