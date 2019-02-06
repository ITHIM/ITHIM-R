#' @export
ithim_uncertainty <- function(ithim_object,seed=1){ 
  ############################
  for(i in 1:length(ithim_object))
    assign(names(ithim_object)[i],ithim_object[[i]])
  # Get parameters
  for(i in 1:length(parameters))
    assign(names(parameters)[i],parameters[[i]][[seed]],pos=1)
  ## Re-do set up if BUS_WALK_TIME or MC_TO_CAR_RATIO has changed
  (if(RECALCULATE_TRIPS){
    set_vehicle_inventory()
    get_synthetic_from_trips()
  })
  
  ## calculate distances, if distances are not variable dependent
  (if(RECALCULATE_DISTANCES){
    ithim_object <- get_all_distances(ithim_object)
  })
  ############################
  # Run ITHIM cascade of functions
  (run_results <- run_ithim(ithim_object,seed))
  run_results$dist <- ithim_object$dist
  run_results$dur <- ithim_object$dur
  #return(run_results)
  ##!! RJ for now return only hb from uncertain simulations; otherwise the file is too big
  return(list(hb=run_results$hb,inj=run_results$ref_injuries))
}