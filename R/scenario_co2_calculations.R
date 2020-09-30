###Please see city parameter file which has CO2 emissions for same set of modes as PM2.5 added in the bottom rows
### I think you need to read it in an object same as VEHICLE_INVENTORY
### To make it simple may be make a new object VEHICLE_CO2_INVENTORY

### After that you need to run the following set of lines, these are from scenario_pm_calculations (lines 22 to 31)
scenario_co2_calculations <- function(dist){
  
  ## adding in travel not covered in the synthetic trip set, based on distances travelled relative to car, set in VEHICLE_INVENTORY
  emission_dist <- dist
  
  ## get emission factor by dividing inventory by baseline distance. (We don't need to scale to a whole year, as we are just scaling the background concentration.)
  ordered_efs <- (VEHICLE_INVENTORY$CO2_emission_inventory[match(emission_dist$stage_mode,VEHICLE_INVENTORY$stage_mode)] %>% as.numeric())/(emission_dist$Baseline %>% as.numeric())
  ## get new emission by multiplying emission factor by scenario distance.
  trans_emissions <- emission_dist[,0:NSCEN+2]*t(repmat(ordered_efs,NSCEN+1,1))
  ## augment with travel emission contributions that aren't included in distance calculation
  for(mode_type in which(!VEHICLE_INVENTORY$stage_mode%in%emission_dist$stage_mode))
    trans_emissions[nrow(trans_emissions)+1,] <- VEHICLE_INVENTORY$CO2_emission_inventory[mode_type]
  
  return(trans_emissions)


## we then report this baseline_sum of CO2 emissions for each scenario
}