#' @export
just_distances <- function(seed=1,ithim_object){
  set.seed(seed)
  for(i in 1:length(ithim_object))
    assign(names(ithim_object)[i],ithim_object[[i]])
  # Get parameters
  for(i in 1:length(parameters))
    assign(names(parameters)[i],parameters[[i]][[seed]],pos=1)
  #set_vehicle_inventory() # sets vehicle inventory
  get_synthetic_from_trips() # sets synthetic trips and synthetic population
  ithim_object <- get_all_distances(ithim_object) # uses synthetic trips to calculate distances
  ithim_object
}