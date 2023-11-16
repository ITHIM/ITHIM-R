#' Sampling routine for running ITHIM with uncertainty
#' 
#' Sets sampled parameters to the global environment and calls the ITHIM routine 
#' 
#' 
#' This function works by performing the following steps:
#' 
#' \itemize{
#' \item extract the ithim_object list entries  
#' 
#' \item extract the sampled parameters which are stored in the ithim_object  
#' \item call set_vehicle_inventory.R to update emissions if any  
#'   emission parameters have been sampled from a distribution   
#'   
#' \item call get_synthetic_from_trips.R to update the synthetic trips and synthetic population if any 
#'   relevant input parameters have been sampled from a distribution 
#'    
#' \item call get_all_distances.R to recalculate the distances if any of the relevant input parameters have
#'   been sampled from a distribution  
#'   
#' \item run ITHIM-Global model by calling ithim_calculation_sequence.R
#' }
#' 
#' @param ithim_object list of necessary inputs, including parameters
#' @param seed which sample to take
#' 
#' @return list of ITHIM outcomes
#' 
#' @export


ithim_uncertainty <- function(ithim_object,seed=1){ 
  ############################
  
  # extract variables from ithim_object list
  for(i in 1:length(ithim_object))
    assign(names(ithim_object)[i],ithim_object[[i]])
  ithim_object$parameters <- 0
  
  # extract parameters
  for(i in 1:length(parameters))
    assign(names(parameters)[i],parameters[[i]][[seed]],pos=1)
  parameters <- NULL
  
  # Re-do set up if any emissions data has been sampled from a distribution
  if(RECALCULATE_PM_EMISSION_INVENTORY | RECALCULATE_CO2_EMISSION_INVENTORY) set_vehicle_inventory() # sets vehicle inventory
 
  # Re-do if any trip related data has been sampled from a distribution
  if(RECALCULATE_TRIPS){
    #set_vehicle_inventory()
    ithim_object$trip_scen_sets <- get_synthetic_from_trips()#5 # update trip data
  }
  
  # calculate distances, if any distances have been sampled from a distribution 
  if(RECALCULATE_DISTANCES){
    ithim_object <- get_all_distances(ithim_object)#3 # update distances
  }
  
  
  ############################
  # Run ITHIM cascade of functions
  system.time(run_results <- ithim_calculation_sequence(ithim_object,seed))#9
  run_results$dist <- ithim_object$dist
  run_results$dur <- ithim_object$dur
  #return(run_results)
  # for now return only hb from uncertain simulations; otherwise the file is too big
  return(list(hb=run_results$hb,inj=run_results$ref_injuries, SCEN = run_results$SCEN))
}


