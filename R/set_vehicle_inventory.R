#' Collate all vehicle information
#' 
#' Puts all vehicle information in one place including speeds and emission factors. Writes to global environment.
#' 
#' This function performs the following operations:
#' 
#' \itemize{
#' \item Based on the city specific (or default) mode speeds, the vehicle inventory is initialised
#' 
#' \item PM emissions are added from the city specific (or default) PM inventory
#' 
#' \item big_truck mode is added for which there is no distance but it this mode is used in the air pollution modules
#' 
#' \item other mode is added if it does not already exist in the travel survey (and hence in the mode speeds)
#' 
#' \item CO2 emissions are added from the city specific (or default) CO2 inventory
#' 
#' \item if car_driver exists, the car_driver emissions are set to the PM and CO2 car emissions
#' 
#' \item VEHICLE_INVENTORY is set as a global variable
#' }


#' @export
set_vehicle_inventory <- function(){
 
  # mode names and speeds come from the input parameters used in the run_ithim_setup.R
  vehicle_inventory <- MODE_SPEEDS
  
  # Update PM emissions inventory
  vehicle_inventory$PM_emission_inventory <- 0
  
  for(m in names(PM_EMISSION_INVENTORY))
      vehicle_inventory$PM_emission_inventory[vehicle_inventory$stage_mode%in%m] <- PM_EMISSION_INVENTORY[[m]]

  
  # add big truck mode to vehicle inventory
  vehicle_inventory <- rbind(vehicle_inventory,data.frame(stage_mode='big_truck',
                                                          speed=21,
                                                          PM_emission_inventory=PM_EMISSION_INVENTORY[['big_truck']]))
  
  # if other mode does not already exist, add other mode
  if (!'other' %in% vehicle_inventory$stage_mode){
  vehicle_inventory <- rbind(vehicle_inventory,data.frame(stage_mode='other',
                                                          speed=21,
                                                          PM_emission_inventory=PM_EMISSION_INVENTORY[['other']]))
  }
  
  # add CO2 emissions inventory
  for(m in names(CO2_EMISSION_INVENTORY))
    vehicle_inventory$CO2_emission_inventory[vehicle_inventory$stage_mode%in%m] <- CO2_EMISSION_INVENTORY[[m]]
  
  
  # if car driver exists, set car driver emissions to car emissions and set car emissions to 0
  if ('car_driver' %in% vehicle_inventory$stage_mode){
    vehicle_inventory$PM_emission_inventory[vehicle_inventory$stage_mode == 'car_driver'] <- PM_EMISSION_INVENTORY[['car']]
    vehicle_inventory$CO2_emission_inventory[vehicle_inventory$stage_mode == 'car_driver'] <- CO2_EMISSION_INVENTORY[['car']]
    vehicle_inventory$PM_emission_inventory[vehicle_inventory$stage_mode == 'car'] <- 0
    vehicle_inventory$CO2_emission_inventory[vehicle_inventory$stage_mode == 'car'] <- 0
  }
  
  VEHICLE_INVENTORY <<- vehicle_inventory
}
