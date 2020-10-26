#' Collate all vehicle information
#' 
#' Puts all vehicle information in one place. Writes to global environment.
#' 
#' 
#' @export
set_vehicle_inventory <- function(){
  ##!! needs some work, both in terms of how it is built, and how it is used.
  ## the vehicle inventory lists the modes, their speeds, their emission factors, and the distance covered relative to cars.
  ## we use 'distance covered relative to cars' to impute distances for modes not included in the travel survey.
  ## these distances are either added to the synthetic trip set, which means they feature in the emission and injury calculations,
  ## or they are just added ad hoc to the emission calculation.
  ## For Accra, bus_driver and truck trips are added to Synthetic trips. Big truck and other are not, so are included in Emission calculation only.
  ## ratios are heuristic values taken from Delhi study. 
  ## They can become set variables, or random variables, but as present are constant as below. To make variable, move VEHICLE_INVENTORY definition to 'dist' calculation.
  ## N.B.: the mode list is the union of trip_modes and PM_EMISSION_INVENTORY. To omit an undesired mode, we'd need to set the distance ratio to 0.
  
  # mode names and speeds come from the input into run_ithim_setup
  vehicle_inventory <- MODE_SPEEDS
  # the other variables are `emission factor' and `distance ratio to car', which we set per mode, where they differ from 0 and 1, respectively.
  # emission factors come from global data. we will need at to have at least three versions of this, corresponding to different global regulatory standards. For Accra, we use `Euro III'
  # distance ratios can be provided as inputs to run_ithim_setup
  # we don't enter ratio values for cycling and pedestrian as it's assumed they will be covered by the survey.
  vehicle_inventory$PM_emission_inventory <- 0
  #vehicle_inventory$distance_ratio_to_car <- 1

  for(m in names(PM_EMISSION_INVENTORY))
      vehicle_inventory$PM_emission_inventory[vehicle_inventory$stage_mode%in%m] <- PM_EMISSION_INVENTORY[[m]]
  #for(m in names(DISTANCE_RATIOS))
  #    vehicle_inventory$distance_ratio_to_car[vehicle_inventory$stage_mode%in%m] <- DISTANCE_RATIOS[[m]]
  
  ##!! this is the only part that currently changes with uncertainty
  #vehicle_inventory$distance_ratio_to_car[vehicle_inventory$stage_mode%in%c('motorbike')] <- MOTORCYCLE_TO_CAR_RATIO#DISTANCE_RATIOS$motorbike
  
  vehicle_inventory <- rbind(vehicle_inventory,data.frame(stage_mode='big_truck',
                                                          speed=21,
                                                          PM_emission_inventory=PM_EMISSION_INVENTORY[['big_truck']]))#,
  #                                                        distance_ratio_to_car=DISTANCE_RATIOS$big_truck))
  vehicle_inventory <- rbind(vehicle_inventory,data.frame(stage_mode='other',
                                                          speed=0,
                                                          PM_emission_inventory=PM_EMISSION_INVENTORY[['other']]))#,
  #                                                        distance_ratio_to_car=DISTANCE_RATIOS$other))
  
  for(m in names(CO2_EMISSION_INVENTORY))
    vehicle_inventory$CO2_emission_inventory[vehicle_inventory$stage_mode%in%m] <- CO2_EMISSION_INVENTORY[[m]]
  
  VEHICLE_INVENTORY <<- vehicle_inventory
}
