#' Set up baseline scenario data frame
#'
#' Set up baseline scenario ready for the scenario development
#'
#' This function performs the following steps to the trip set data:
#'
#' \itemize{
#' \item add distance categories
#'
#' \item add scenario='baseline' column
#' }
#'
#'
#' @param trip_set data frame of trips
#'
#' @return trip_set as baseline scenario
#'
#' @export


ithim_setup_baseline_scenario <- function(trip_set) {
  # Initialize distance categories as they are used in the scenario generation
  trip_set$trip_distance_cat <- 0
  ## !! assuming more than one distance category
  for (i in 2:length(DIST_LOWER_BOUNDS) - 1) {
    trip_set$trip_distance_cat[trip_set$trip_distance >= DIST_LOWER_BOUNDS[i] & trip_set$trip_distance < DIST_LOWER_BOUNDS[i + 1]] <- DIST_CAT[i]
  }
  trip_set$trip_distance_cat[trip_set$trip_distance >= DIST_LOWER_BOUNDS[length(DIST_LOWER_BOUNDS)]] <- DIST_CAT[length(DIST_LOWER_BOUNDS)]

  trip_set$scenario <- "baseline"

  return(trip_set)
}
