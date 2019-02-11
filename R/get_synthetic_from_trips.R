#' @export
get_synthetic_from_trips <- function(){
  raw_trip_set <- TRIP_SET
  
  ## add motorcycle trip to accra, and replicate set four times
  if(CITY=='accra') raw_trip_set <- edit_accra_trips(raw_trip_set)
  #SURVEY_SCALAR <<- population/length(unique(TRIP_SET$participant_id))/survey_coverage
  
  ## add bus and truck trips
  if(ADD_BUS_DRIVERS) raw_trip_set <- add_ghost_trips(raw_trip_set,trip_mode='bus_driver')
  if(ADD_TRUCK_DRIVERS) raw_trip_set <- add_ghost_trips(raw_trip_set,trip_mode='truck')

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
