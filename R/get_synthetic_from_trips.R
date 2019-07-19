#' @export
get_synthetic_from_trips <- function(){
  
  # create synthetic population
  synth_pop <- create_synth_pop()
  SYNTHETIC_POPULATION <<- synth_pop$synthetic_population
  trip_set <- synth_pop$trip_set
  synth_pop <- NULL
  
  # create scenarios: either the walking test case, or the 5 hard-coded Accra scenarios
  trip_set <- ithim_setup_baseline_scenario(trip_set)
  ## update all distances and durations
  trip_set <- scale_trip_distances(trip_set)
  
  if(TEST_WALK_SCENARIO){
    synthetic_trips <- create_walk_scenario(trip_set)
  }else if(TEST_CYCLE_SCENARIO){
    synthetic_trips <- create_cycle_scenarios(trip_set)
  }else if(MAX_MODE_SHARE_SCENARIO){
    synthetic_trips <- create_max_mode_share_scenarios(trip_set)
  }else{
    synthetic_trips <- create_all_scenarios(CITY, trip_set)
  }
  trip_set <- NULL
  
  #set_scenario_specific_variables()
  # some useful variables.
  NSCEN <<- length(synthetic_trips) - 1
  SCEN <<- sapply(synthetic_trips,function(x)x$scenario[1])
  SCEN_SHORT_NAME <<- c("base",paste0("scen", 1:NSCEN) )
  
  # add walk-to-PT trips, as appropriate
  #trip_scen_sets <- walk_to_pt(synthetic_trips)
  pt_modes <- c('bus','minibus','subway','rail')
  
  if(ADD_WALK_TO_BUS_TRIPS)
    for(i in 1:length(synthetic_trips)){
      # separate out bus trips
      pt_trips <- synthetic_trips[[i]][synthetic_trips[[i]]$stage_mode%in%pt_modes,]
      not_pt_trips <- synthetic_trips[[i]][!synthetic_trips[[i]]$stage_mode%in%pt_modes,]
      # divide bus trips into bus and walking
      pt_walk_trips <- add_walk_trips(pt_trips)
      # recombine all trips
      synthetic_trips[[i]] <- rbind(not_pt_trips,pt_walk_trips[[1]],pt_walk_trips[[2]])
    }
  
  # ## update all distances and durations
  # for(i in 1:length(trip_scen_sets)) trip_scen_sets[[i]] <- scale_trip_distances(trip_scen_sets[[i]])
  
  ##!! might not want to save this object
  #ithim_object$trip_scen_sets <- do.call(rbind,trip_scen_sets)
  
  ##RJ synthetic population
  pp_summary <- generate_synthetic_travel_data(synthetic_trips)
  synthetic_trips <- NULL
  
  return(pp_summary)
}
