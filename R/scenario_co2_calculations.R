#' Calculate total CO2 exposure per mode and scenario
#' 
#' Calculate total CO2 exposure per person based on population and personal travel for each scenario
#' 
#' This function performs the following steps:
#' - calculate emission factors for each mode by dividing total emissions by distances travelled
#' - calculate CO2 emissions for each mode in each scenario by multiplying the scenario distance times the emission factors
#' - for modes without any assigned distance, use the CO2 emissions from the VEHICLE_INVENTORY instead
#' 
#' 
#' @param dist data frame of population travel from all scenarios
#' 
#' @return total CO2 exposure per mode
#' 
#' @export
#' 


scenario_co2_calculations <- function(dist){
  
  ## total population distances travelled by all modes
  emission_dist <- dist
  
  ## get emission factor by dividing inventory emissions by baseline distance. 
  #(We don't need to scale to a whole year, as we are just scaling the background concentration.)
  ordered_efs <- (VEHICLE_INVENTORY$CO2_emission_inventory[match(emission_dist$stage_mode,VEHICLE_INVENTORY$stage_mode)
                                                           ] %>% as.numeric())/(emission_dist$baseline %>% as.numeric())
  ## get new emission by multiplying emission factor by scenario distance.
  trans_emissions <- emission_dist[,0:NSCEN+2]*t(repmat(ordered_efs,NSCEN+1,1))
  ## augment with travel emission contributions that aren't included in distance calculation
  for(mode_type in which(!VEHICLE_INVENTORY$stage_mode%in%emission_dist$stage_mode))  # loop through modes without an assigned distance
    trans_emissions[nrow(trans_emissions)+1,] <- VEHICLE_INVENTORY$CO2_emission_inventory[mode_type] # add emissions from vehicle inventory
  
  return(trans_emissions)


## we then report this baseline_sum of CO2 emissions for each scenario
}
