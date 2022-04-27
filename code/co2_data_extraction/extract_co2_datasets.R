library(tidyverse)
library(tibble)

# Load ithim object
io <- read_rds(file.choose())

# Init cities
cities <- names(io)[!names(io) %in% 'scen_prop']

# Loop all cities
for (city in cities){
  
  # Create dir if it does not already exist
  if (!file.exists(city))
    dir.create(city)
  
  # Write trips
  write_csv(io[[city]]$trip_scen_sets, paste0(city, "/", paste0(city, "_trips"), ".csv"))
  
  # Read vehicle inventory and remove all duplicates
  vehicle_inventory <- io[[city]]$vehicle_inventory %>% dplyr::distinct(stage_mode, .keep_all = T)
  
  # Write vehicle inventory with PM2.5 and CO2 entries
  write_csv(vehicle_inventory, paste0(city, "/", paste0(city, "_vehicle_inventory"), ".csv"))
  
  # Total distance by mode
  dist <- io[[city]]$dist
  
  # Rename columns
  names(dist)[2:5] <- c("Baseline", "Bicycling", "Driving", "Public Transport")
  
  # Write passenger dist
  # Note if you like to calculate vehicle distance for bus, you need to divide it by 31.
  # In case you have better bus to passenger ratio - for all studied countries, please use that insteat
  write_csv(dist, paste0(city, "/", paste0(city, "_distance"), ".csv"))
  
  # Combine CO2 emission inventory with distance by mode, to calculate emission factors
  # emission_factors = CO2_emission_inventory / Baseline_distance
  co2 <- left_join(vehicle_inventory %>% dplyr::select(stage_mode, CO2_emission_inventory), 
                   dist) %>% 
    mutate(emission_factors = CO2_emission_inventory / Baseline)
  
  # Multiply distance by emission factors
  co2[,3:6] <- co2[,3:6] * co2$emission_factors
  
  # Copy CO2 inventory for all those transport modes that do not have distances
  co2[(is.na(co2$Baseline) & !is.na(co2$CO2_emission_inventory)),3:6] <- 
    co2[(is.na(co2$Baseline) & !is.na(co2$CO2_emission_inventory)),2]
  
  # Calculate column totals
  co2 <- co2 %>% janitor::adorn_totals()
  
  # Write it as a CSV
  write_csv(co2, paste0(city, "/", paste0(city, "_CO2"), ".csv"))
  
}