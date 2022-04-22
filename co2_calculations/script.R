# Load libraries
library(tidyverse)
library(janitor)

######### SCENARIO DEFINITIONS
scen <- read_csv("scen_prop.csv")

######### TRIP DISTANCE SUMMARY 
## Code to read trips data and produce total distance by mode for all scenarios

# Read trips data
trips <- read_csv("trips_antofagasta.csv")

# Display total distance by mode for all scenarios
# NOTE: trips with multiple stages are repeated, as it's a stage level dataset, 
trips <- trips %>% group_by(scenario, stage_mode) %>% summarise(dist = sum(stage_distance))

# Add walk_to_pt trips to pedestrian mode
trips$dist[trips$stage_mode == "pedestrian"] <- trips$dist[trips$stage_mode == "pedestrian"] +  
  trips$dist[trips$stage_mode == "walk_to_pt"]

# Remove walk to PT mode
trips <- trips %>% filter(stage_mode != "walk_to_pt")

# Display distance table - similar to the dist variable
trips %>% pivot_wider(names_from = "scenario", values_from = "dist") %>% View()


######### CO2 CALCULATIONS

# Read vehicle inventory
vehicle_inventory <- read_csv("vehicle_inventory_antofagasta.csv")

# Total distance by mode
dist <- read_csv("dist_antofagasta.csv")

# Rename columns
names(dist)[2:5] <- c("Baseline", "Bicycling", "Driving", "Public Transport")

# Remove duplicate rows
vehicle_inventory <- vehicle_inventory %>% distinct(stage_mode, .keep_all = T)

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
write_csv(co2, "CO2_antofagasta.csv")