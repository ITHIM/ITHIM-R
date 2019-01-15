#' @export
set_vehicle_inventory <- function(){
  ##!! needs some work, both in terms of how it is built, and how it is used.
  ## the vehicle inventory lists the modes, their speeds, their emission factors, and the distance covered relative to cars.
  ## we use 'distance covered relative to cars' to impute distances for modes not included in the travel survey.
  ## these distances are either added to the synthetic trip set, which means they feature in the emission and injury calculations,
  ## or they are just added ad hoc to the emission calculation.
  ## For Accra, Bus_driver and Truck trips are added to Synthetic trips. LDT and Other are not, so are included in Emission calculation only.
  ## ratios are heuristic values taken from Delhi study. 
  ## They can become set variables, or random variables, but as present are constant as below. To make variable, move VEHICLE_INVENTORY definition to 'dist' calculation.
  ## N.B.: the mode list is the union of trip_modes and EMISSION_FACTORS. To omit an undesired mode, we'd need to set the distance ratio to 0.
  
  # mode names and speeds come from the input into run_ithim_setup
  vehicle_inventory <- MODE_SPEEDS
  # the other variables are `emission factor' and `distance ratio to car', which we set per mode, where they differ from 0 and 1, respectively.
  # emission factors come from global data. we will need at to have at least three versions of this, corresponding to different global regulatory standards. For Accra, we use `Euro III'
  # distance ratios can be provided as inputs to run_ithim_setup
  # we don't enter ratio values for cycling and walking as it's assumed they will be covered by the survey.
  vehicle_inventory$emission_factor <- 0
  vehicle_inventory$distance_ratio_to_car <- 1
  
  vehicle_inventory$emission_factor[vehicle_inventory$trip_mode%in%c('Taxi')] <- EMISSION_FACTORS$PM2_5_emiss_fact[EMISSION_FACTORS$vehicle_type=='TAXI']
  vehicle_inventory$distance_ratio_to_car[vehicle_inventory$trip_mode%in%c('Taxi')] <- TAXI_TO_CAR_RATIO
  
  # private cars in Dehli/Accra are a mix of types 4W1 and 4W2. We estimate their prevalence ratio as 10:2.
  vehicle_inventory$emission_factor[vehicle_inventory$trip_mode%in%c('Private Car')] <- 
    RATIO_4W1_TO_4W2*EMISSION_FACTORS$PM2_5_emiss_fact[EMISSION_FACTORS$vehicle_type=='4W1']+(1-RATIO_4W1_TO_4W2)*EMISSION_FACTORS$PM2_5_emiss_fact[EMISSION_FACTORS$vehicle_type=='4W1']
  
  vehicle_inventory$emission_factor[vehicle_inventory$trip_mode%in%c('Bus_driver')] <- EMISSION_FACTORS$PM2_5_emiss_fact[EMISSION_FACTORS$vehicle_type=='BUS']
  vehicle_inventory$distance_ratio_to_car[vehicle_inventory$trip_mode%in%c('Bus_driver')] <- BUS_TO_CAR_RATIO
  
  vehicle_inventory$emission_factor[vehicle_inventory$trip_mode%in%c('Truck')] <- EMISSION_FACTORS$PM2_5_emiss_fact[EMISSION_FACTORS$vehicle_type=='HDT']
  vehicle_inventory$distance_ratio_to_car[vehicle_inventory$trip_mode%in%c('Truck')] <- TRUCK_TO_CAR_RATIO
  
  vehicle_inventory$emission_factor[vehicle_inventory$trip_mode%in%c('Motorcycle')] <- EMISSION_FACTORS$PM2_5_emiss_fact[EMISSION_FACTORS$vehicle_type=='2W']
  vehicle_inventory$distance_ratio_to_car[vehicle_inventory$trip_mode%in%c('Motorcycle')] <- MC_TO_CAR_RATIO
  
  vehicle_inventory <- rbind(vehicle_inventory,data.frame(trip_mode=EMISSION_FACTORS$vehicle_type[7],speed=21,emission_factor=EMISSION_FACTORS$PM2_5_emiss_fact[7],distance_ratio_to_car=LDT_TO_CAR_RATIO))
  vehicle_inventory <- rbind(vehicle_inventory,data.frame(trip_mode=EMISSION_FACTORS$vehicle_type[8],speed=21,emission_factor=EMISSION_FACTORS$PM2_5_emiss_fact[8],distance_ratio_to_car=OTHER_TO_CAR_RATIO))
  
  VEHICLE_INVENTORY <<- vehicle_inventory
}
