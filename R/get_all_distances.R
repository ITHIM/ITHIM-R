#' Sequence to get distance data
#' 
#' Sequence of function calls to get distance data for modules from synthetic population
#' 
#' @param ithim_object list containing synthetic trip set
#' 
#' @return ithim_object again, with additional distance objects
#' 
#' @export
get_all_distances <- function(ithim_object){
  
  # add walk-to-bus trips, as appropriate, and combines list of scenarios
  #ithim_object$trip_scen_sets <- walk_to_pt_and_combine_scen()
  
  # Generate distance and duration matrices
  dist_and_dir <- dist_dur_tbls(ithim_object$trip_scen_sets)
  ithim_object$dist <- dist_and_dir$dist
  ithim_object$dur <- dist_and_dir$dur
  
  # distances for injuries calculation
  ithim_object$inj_distances <- distances_for_injury_function(trip_scen_sets=ithim_object$trip_scen_sets,ithim_object$dist)
  
  return(ithim_object)
}
