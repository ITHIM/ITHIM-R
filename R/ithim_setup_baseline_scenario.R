#' @export
ithim_setup_baseline_scenario <- function(trip_set){
  ##?? do we need any/all of rid, trip_id, row_id?
  
  ## SET UP TRAVEL DATA
  # Create a row id
  #trip_set$rid <- 1:nrow(trip_set)
  
  # Initialize distance categories
  ## Distance categories are used in scenario generation. They correspond to e.g. ``long trips'' and ``short trips''
  trip_set[,'trip_distance_cat':=DIST_CAT[length(DIST_LOWER_BOUNDS)]]
  ##!! assuming more than one distance category
  for(i in length(DIST_LOWER_BOUNDS):2-1){
    trip_set[trip_distance < DIST_LOWER_BOUNDS[i+1],trip_distance_cat:=DIST_CAT[i]]
  }
  #trip_set$trip_distance_cat[trip_set$trip_distance >= DIST_LOWER_BOUNDS[length(DIST_LOWER_BOUNDS)]] <- DIST_CAT[length(DIST_LOWER_BOUNDS)]
  
  trip_set[,'scenario':="Baseline"]
  
  return(trip_set)
}
