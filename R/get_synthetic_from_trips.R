#' @export
get_synthetic_from_trips <- function(){
  ##!! to get the right order of trip columns; needed if trips are added
  raw_trip_set <- data.frame(trip_id=TRIP_SET$trip_id,
                             trip_mode=TRIP_SET$trip_mode,
                             trip_duration=TRIP_SET$trip_duration,
                             participant_id=TRIP_SET$participant_id,
                             age=TRIP_SET$age,
                             sex=TRIP_SET$sex, 
                             stringsAsFactors = F)
  
  ##!! number trips
  raw_trip_set$trip_id[!is.na(raw_trip_set$trip_id)] <- 1:sum(!is.na(raw_trip_set$trip_id))
  
  ## add motorcycle trip to accra, and replicate set four times
  if(CITY=='accra') raw_trip_set <- edit_accra_trips(raw_trip_set)
  #SURVEY_SCALAR <<- population/length(unique(TRIP_SET$participant_id))/survey_coverage
  
  ## add bus and truck trips
  if(ADD_BUS_DRIVERS) raw_trip_set <- add_ghost_trips(raw_trip_set)
  if(ADD_TRUCK_DRIVERS) raw_trip_set <- add_ghost_trips(raw_trip_set,trip_mode='truck',distance_ratio=TRUCK_TO_CAR_RATIO,reference_mode='car')

  # create synthetic population
  synth_pop <- create_synth_pop(raw_trip_set)
  SYNTHETIC_POPULATION <<- synth_pop$synthetic_population
  trip_set <- synth_pop$trip_set
  
  # create scenarios: either the walking test case, or the 5 hard-coded Accra scenarios
  trip_set <- ithim_setup_baseline_scenario(trip_set)
  if(TEST_WALK_SCENARIO){
    SYNTHETIC_TRIPS <<- create_walk_scenario(trip_set)
  }else{
    SYNTHETIC_TRIPS <<- create_all_scenarios(CITY, trip_set)
  }
  
  set_scenario_specific_variables()
}
