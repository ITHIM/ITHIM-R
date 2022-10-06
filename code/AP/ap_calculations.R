# Load libraries
library(tidyverse)
library(janitor)
library(here)
library(data.table)
library(stringi)


## Concentration contributed by non-transport share (remains constant across the scenarios)
# non_transport_pm_conc <- PM_CONC_BASE * (1 - PM_TRANS_SHARE)  

## Calculate mode specific travel pm 2.5 concentration by:
# emission_factors = PM_emission_inventory (vehicle specific) / baseline_distance_by_mode
# Multiply mode specific distance by emission factors
# mode specific travel emissions = mode_specific_distance * emission_factors
# For the modes that don't have any distance but there is an entry of them in PM 2.5 inventory use their emission factors instead
# mode specific travel emissions = emission_factors


## Calculate travel PM 2.5 concentration for baseline + scenarios by:
# Calculate baseline travel concentration by: baseline_sum = sum(mode specific travel emissions)
# Calculate conc_pm for baseline + scenarios
# conc_pm = non_transport_pm_conc + PM_TRANS_SHARE * PM_CONC_BASE 
#           * sum(mode specific travel emissions) / baseline_sum
#           
# Use a constant ventilation rate value measured m3/hour - 
# these values come from HEAT, pages 43 and 44: 
# https://cdn.who.int/media/docs/default-source/air-pollution-documents/heat.pdf?sfvrsn=ba0969b9_1&download=true – table 11
# at rest 0.61 m3/h
#	in a car or in transit (assume this is equal to the ventilation rate at rest) 0.61 m3/h
#	cycling 2.55 m3/h
#	walking 1.37 m3/h
#	sleep 0.27 m3/h – assume sleep is 8 hours a day/ 480 minutes a day

#	Use a constant mode specific exposure factor which is the ratio between that mode’s PM2.5 and the 
#	background’s PM2.5 - this comes from Audrey’s paper and is the approach used by HEAT as well: 
#	“Mode-specific PM2.5 concentrations are derived from the background concentrations using conversion factors - below. 
# Exposure ratios
# Cycling 2.0 
# Walking 1.6
# Using a car 2.5
# Using public transit 1.9 
# Suggest we also just stick to the above and disregard the percentage of time that we just might be open and cars transit

# 
# ## Calculate on road air for each stage of a trip
# m3 of air inhaled are the product of the ventilation rate and the time by activity (hours/60) 
# Calculation should be as follows:
#   For any agent, total air inhaled in m3 during the day (implicit assumption that this day presents average day across the year)=  
#   total_air_inhaled (m3) = Air inhaled cycling (2.55 m3/h * duration of cycling in minutes/60) + 
#   Air inhaled walking (1.37 m3/h * duration of walking in minutes/60) + 
#   air inhaled in transit (0.61 m3/h * duration of transit in minutes/60) + 
#   air inhaled in car (0.61 m3/h * duration of car trips in minutes/60) + 
#   air inhaled during sleep (0.27 m3/h * 8 hours) + 
#   air inhaled at rest (0.61 m3/h * (16 hours - hours in and all transportation activities combined: cycling + 
#   walking + transit + car)) #note that the 16 is the 24 hours/day minus the 8 hours/day sleep
#   
## Calculate PM dose in µg as:
# pm dose in µg as the product of the air inhaled in m3, pm concentration (µg/m3), and the exposure ratio
# total_pm_inhaled (µg)= Air inhaled cycling (2.55 m3/h * duration of cycling in minutes/60) *2 * conc_pm  (µg/m3) +  
# Air inhaled walking (1.37 m3/h * duration of walking in minutes/60) *1.6 * conc_pm  (µg/m3) + 
# air inhaled in transit (0.61 m3/h * duration of transit in minutes/60) * 1.9 * conc_pm  (µg/m3) + 
# air inhaled in car (0.61 m3/h * duration of car trips in minutes/60) * 2.5 * conc_pm  (µg/m3)+ 
# air inhaled in sleep (0.27 m3/h * 8 hours) * conc_pm  (µg/m3) + 
# air inhaled at rest (0.61 m3/h * (16 hours – hours in all transportation activities combined: cycling + walking + transit + car)) * conc_pm  (µg/m3)
#  


#  # Concentration of pm inhaled in µg/m3= 
# total_pm_inhaled/ total_air_inhaled 
# this would be the exposure value which we use in association with the ERFs and for presenting statistics

# Static local and global parameters 
# read from the input parameters v6.0 excel sheet
# City specific params
PM_CONC_BASE <- 10.49221667
PM_TRANS_SHARE <- 0.31

# Global params
MMET_CYCLING <- 4.63
MMET_WALKING <- 2.53

# Five fixed parameters: BASE_LEVEL_INHALATION_RATE (10), CLOSED_WINDOW_PM_RATIO (0.5), 
# CLOSED_WINDOW_RATIO (0.5), ROAD_RATIO_MAX (3.216), ROAD_RATIO_SLOPE (0.379)
# NOTE: the above comment says 10, but it is actually 1 in the code.
BASE_LEVEL_INHALATION_RATE <- 1 
CLOSED_WINDOW_PM_RATIO <- 0.5
CLOSED_WINDOW_RATIO <- 0.5
ROAD_RATIO_MAX <- 3.216
ROAD_RATIO_SLOPE <- 0.379

SUBWAY_PM_RATIO <- 0.8

vent_rates <- data.frame(
  stage_mode = c("rest", "car", "bus", "rail", "cycle", "pedestrian", "sleep"), 
  v_rate = c(0.61, 0.61, 0.61, 0.61, 2.55, 1.37, 0.27)
)

exp_facs <- data.frame(
  stage_mode = c("car", "bus", "rail", "cycle", "pedestrian"), 
  e_rate = c(2.5, 1.9, 1.9, 2.0, 1.6)
)



# dir_path
abs_path <- "code/AP"

######### AP CALCULATIONS

# Read vehicle inventory
vehicle_inventory <- read_csv(here(abs_path, "vehicle_inventory_antofagasta.csv"))

# Total distance by mode
dist <- read_csv(here(abs_path, "dist_antofagasta.csv"))

# Trip dataset
trips <- read_csv(here(abs_path, "trips_antofagasta.csv"))

# Scenario names
SCEN <- unique(trips$scenario)

# More explanatory scenario names
SCEN_QN <- c("Baseline", "Bicycling", "Driving", "Public Transport")

# Scenario names without any blanks and in lower case
SCEN_SHORT_NAME <- stri_replace_all_fixed(tolower(SCEN), ' ', '_')

# Synthetic population
synth_population <- read_csv(here(abs_path, "synth_pop_antofagasta.csv"))

# Number of scenarios
NSCEN <- length(SCEN)

# Rename columns
names(dist)[2:5] <- SCEN

# Remove duplicate rows
vehicle_inventory <- vehicle_inventory %>% distinct(stage_mode, .keep_all = T)

# Concentration contributed by non-transport share (remains constant across the scenarios)
non_transport_pm_conc <- PM_CONC_BASE * (1 - PM_TRANS_SHARE)  

# adding in travel not covered in the synthetic trip set, based on distances traveled relative to car, set in VEHICLE_INVENTORY
emission_dist <- dist

# Combine PM 2.5 emission inventory with distance by mode, to calculate emission factors
# emission_factors = PM_emission_inventory / Baseline_distance
ap <- left_join(vehicle_inventory %>% dplyr::select(stage_mode, PM_emission_inventory), 
                 dist) %>% 
  mutate(emission_factors = PM_emission_inventory / Baseline)


# Multiply distance by emission factors
ap <- ap |> 
  mutate(across(contains(SCEN), 
                ~ . * emission_factors))

# Copy PM 2.5 inventory for all those transport modes that do not have distances
ap[(is.na(ap$Baseline) & !is.na(ap$PM_emission_inventory)),SCEN] <- 
  ap[(is.na(ap$Baseline) & !is.na(ap$PM_emission_inventory)),2]

# Scenario travel pm2.5 calculated as relative to the baseline
baseline_sum <- sum(ap[[SCEN[1]]], na.rm = T)
conc_pm <- c()
## in this sum, the non-transport pm is constant; the transport emissions scale the transport contribution (PM_TRANS_SHARE) to the base level (PM_CONC_BASE)
for(i in 1:length(SCEN))
  conc_pm[i] <- non_transport_pm_conc + PM_TRANS_SHARE * PM_CONC_BASE * sum(ap[[SCEN[i]]], na.rm = T) / baseline_sum


# Copy trips dataset
trip_set <- trips

# Rename short walks as pedestrian 
trip_set$stage_mode[trip_set$stage_mode=='walk_to_pt'] <- 'pedestrian'

# Reweight bus duration by multiplying it with baseline's bus_driver and bus ratio
bus_ratio <- trip_set |> 
  filter(stage_mode %in% c("bus", "bus_driver") & scenario == "Baseline") |> 
  summarise(sum(stage_duration[stage_mode == "bus_driver"]) / sum(stage_duration[stage_mode == "bus"])) |> 
  pull()

trip_set[trip_set$stage_mode == "bus",]$stage_duration <- trip_set[trip_set$stage_mode == "bus",]$stage_duration * bus_ratio


# Reweight bus duration by multiplying it with baseline's bus_driver and bus ratio
car_ratio <- trip_set |> 
  filter(stage_mode %in% c("car", "car_driver") & scenario == "Baseline") |> 
  summarise(sum(stage_duration[stage_mode == "car_driver"]) / sum(stage_duration[stage_mode == "car"])) |> 
  pull()

trip_set[trip_set$stage_mode == "car",]$stage_duration <- trip_set[trip_set$stage_mode == "car",]$stage_duration * car_ratio
trip_set[trip_set$stage_mode == "taxi",]$stage_duration <- trip_set[trip_set$stage_mode == "taxi",]$stage_duration * car_ratio

# Rename scenarios
trip_set <- trip_set |> mutate(scenario = case_when(
  scenario == "Scenario 1" ~ "Bicycling", 
  scenario == "Scenario 2" ~ "Driving",
  scenario == "Scenario 3" ~ "Public Transport",
  scenario == "Baseline" ~ "Baseline"))

# trip set is a data.table, vent_rates is a data.frame, returns a data.table
trip_set <- dplyr::left_join(trip_set, vent_rates, 'stage_mode')

trip_set <- dplyr::left_join(trip_set, exp_facs, 'stage_mode')

conc_pm_df <- data.frame(scenario = unique(trip_set$scenario),
                         conc_pm = conc_pm)

trip_set <- left_join(trip_set, conc_pm_df)


# litres of air inhaled are the product of the ventilation rate and the time (hours/60) spent travelling by that mode
trip_set$air_inhaled <- trip_set$stage_duration / 60 * trip_set$v_rate

trip_set$pm_inhaled <- trip_set$stage_duration / 60 * trip_set$v_rate * trip_set$e_rate * trip_set$conc_pm

trip_set <- trip_set |> group_by(participant_id, scenario) |> mutate(total_travel_time_hrs = sum(stage_duration) / 60) |> ungroup()

td <- trip_set |> filter(participant_id != 0) |> 
  # slice_head(prop = 0.08) |> 
  group_by(participant_id, scenario) |> 
  summarise(total_air_inhaled = sum(air_inhaled, na.rm = T) + 
              8 * filter(vent_rates, stage_mode == "sleep") |> dplyr::select(v_rate) |> pull() + 
              ((16 - total_travel_time_hrs) * filter(vent_rates, stage_mode == "rest") |> dplyr::select(v_rate) |> pull()),
            total_pm_inhaled = sum(pm_inhaled, na.rm = T) +
              8 * (filter(vent_rates, stage_mode == "sleep") |> dplyr::select(v_rate) |> pull()) * conc_pm + 
              ((16 - total_travel_time_hrs) * (filter(vent_rates, stage_mode == "rest") |> dplyr::select(v_rate) |> pull()) * conc_pm)) |> 
            # .groups = "keep") |> 
  distinct(participant_id, scenario, .keep_all = T)

td <- td |> mutate(conc_pm_inhaled = total_pm_inhaled / total_air_inhaled) 

td |> group_by(scenario) |> summarise(as_tibble(rbind(summary(conc_pm_inhaled)))) |> print()

td |> write_csv("code/AP/AP_PM25_antofagasta.csv")
