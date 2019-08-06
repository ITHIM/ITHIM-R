#' Set up baseline scenario data frame
#' 
#' Create scenario by adding distance categories and scenario=baseline column to trip set data frame
#' 
#' @param trip_set data frame of trips 
#' 
#' @return trip_set as baseline scenario
#' 
#' @export
ithim_setup_baseline_scenario <- function(trip_set){
  ##?? do we need any/all of rid, trip_id, row_id?
  
  ## SET UP TRAVEL DATA
  # Create a row id
  #trip_set$rid <- 1:nrow(trip_set)
  
  # Initialize distance categories
  ## Distance categories are used in scenario generation. They correspond to e.g. ``long trips'' and ``short trips''
  trip_set$trip_distance_cat <- 0
  ##!! assuming more than one distance category
  for(i in 2:length(DIST_LOWER_BOUNDS)-1){
    trip_set$trip_distance_cat[trip_set$trip_distance >= DIST_LOWER_BOUNDS[i] & trip_set$trip_distance < DIST_LOWER_BOUNDS[i+1]] <- DIST_CAT[i]
  }
  trip_set$trip_distance_cat[trip_set$trip_distance >= DIST_LOWER_BOUNDS[length(DIST_LOWER_BOUNDS)]] <- DIST_CAT[length(DIST_LOWER_BOUNDS)]
  
  trip_set$scenario <- "Baseline"
  
  return(trip_set)
}
