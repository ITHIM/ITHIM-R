library(tidyverse)
library(tibble)

io <- read_rds(file.choose())

cities <- names(io)[!names(io) %in% 'scen_prop']

for (city in cities){
  
  if (!file.exists(city))
    dir.create(city)
  
  write_csv(io[[city]]$trip_scen_sets, paste0(city, "/", paste0(city, "_trips"), ".csv"))
  
  vehicle_inventory <- data.frame(t(sapply(io[[city]]$PM_emission_inventory,c))) %>% 
    tibble::rowid_to_column() %>% 
    tidyr::pivot_longer(cols = -("rowid")) %>% 
    dplyr::rename(stage_mode = name, CO2_emission_inventory = value) %>% 
    dplyr::distinct(stage_mode, .keep_all = T) %>% 
    dplyr::select(-rowid)
  
  write_csv(vehicle_inventory, paste0(city, "/", paste0(city, "_vehicle_inventory"), ".csv"))
  
  # Total distance by mode
  dist <- io[[city]]$dist
  
  # Rename columns
  names(dist)[2:5] <- c("Baseline", "Bicycling", "Driving", "Public Transport")
  
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