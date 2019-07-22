#' @export
ithim_uncertainty <- function(ithim_object,seed=1){ 
  ############################
  for(i in 1:length(ithim_object))
    assign(names(ithim_object)[i],ithim_object[[i]])
  ithim_object$parameters <- 0
  
  # Get parameters
  for(i in 1:length(parameters))
    assign(names(parameters)[i],parameters[[i]][[seed]],pos=1)
  parameters <- NULL
  
  ## Re-do set up if any distance parameter has changed
  if(RECALCULATE_EMISSION_INVENTORY) set_vehicle_inventory() # sets vehicle inventory
  if(RECALCULATE_TRIPS){
    #set_vehicle_inventory()
    ithim_object$pp_summary <- get_synthetic_from_trips()
  }
  
  ## calculate distances, if distances are not variable dependent
  if(RECALCULATE_DISTANCES){
    dist_list <- get_all_distances(ithim_object$pp_summary) # uses synthetic trips to calculate distances
    ithim_object$dist <- dist_list$dist
    ithim_object$dur <- dist_list$dur
    ithim_object$inj_distances <- dist_list$inj_distances
  }
  ############################
  # Run ITHIM cascade of functions
  run_results <- ithim_calculation_sequence(ithim_object,seed)
  run_results$dist <- ithim_object$dist
  run_results$dur <- ithim_object$dur
  #return(run_results)
  ##!! RJ for now return only hb from uncertain simulations; otherwise the file is too big
  return(list(hb=run_results$hb,inj=run_results$ref_injuries))
}

#ithim_object = multi_city_ithim[[ci]]
