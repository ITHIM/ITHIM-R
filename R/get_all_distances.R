#' @export
get_all_distances <- function(ithim_object){
  ithim_object$trip_scen_sets <- walk_to_bus_and_combine_scen()
  # Generate distance and duration matrices
  dist_and_dir <- dist_dur_tbls(ithim_object$trip_scen_sets)
  ithim_object$dist <- dist_and_dir$dist
  ithim_object$dur <- dist_and_dir$dur
  # distances for injuries calculation
  ithim_object$inj_distances <- distances_for_injury_function(ithim_object$trip_scen_sets)
  ithim_object
}
