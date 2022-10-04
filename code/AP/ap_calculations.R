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
#   air inhaled at rest (0.61 m3/h * (16 hours - hours in and all transportation activities combined: cycling + walking + transit + car)) #note that the 16 is the 24 hours/day minus the 8 hours/day sleep
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

vent_rate <- data.frame(
  activity = c("rest", "car", "transit", "cycling", "walking", "sleep"), 
  rate = c(0.61, 0.61, 0.61, 2.55, 1.37, 0.27)
)

exp_fac <- data.frame(
  activity = c("car", "transit", "cycling", "walking"), 
  rate = c(2.5, 1.9, 2.0, 1.6)
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
































# Ventilation as a function of MMET_CYCLING and MMET_WALKING, loosely following de Sa's SP model.
vent_rates <- data.frame(stage_mode = vehicle_inventory$stage_mode, stringsAsFactors = F) 
vent_rates$vent_rate <- BASE_LEVEL_INHALATION_RATE # L / min
vent_rates$vent_rate[vent_rates$stage_mode == 'cycle'] <- BASE_LEVEL_INHALATION_RATE + MMET_CYCLING / 2.0

# Remove walk_to_pt if pedestrian already exists
if (any(vent_rates$stage_mode == 'pedestrian'))
  vent_rates <- vent_rates |> filter(stage_mode != 'walk_to_pt')
vent_rates$vent_rate[vent_rates$stage_mode == 'pedestrian'] <- BASE_LEVEL_INHALATION_RATE + MMET_WALKING / 2.0

## Keep only distinct modes
vent_rates <- distinct_at(vent_rates, vars(stage_mode), .keep_all = T)

##RJ rewriting exposure ratio as function of ambient PM2.5, as in Goel et al 2015
on_road_off_road_ratio <- ROAD_RATIO_MAX - ROAD_RATIO_SLOPE * log(conc_pm)

# averaging over windows open and windows closed
in_vehicle_ratio <- ((1 - CLOSED_WINDOW_RATIO) * on_road_off_road_ratio) + 
  (CLOSED_WINDOW_RATIO * CLOSED_WINDOW_PM_RATIO)

# subway ratio is a constant
subway_ratio <- rep(SUBWAY_PM_RATIO, length(conc_pm))

# open vehicles experience the ``on_road_off_road_ratio'', and closed vehicles experience the ``in_vehicle_ratio''
ratio_by_mode <- rbind(on_road_off_road_ratio, in_vehicle_ratio, subway_ratio)

# assign rates according to the order of the ratio_by_mode array: 1 is open vehicle, 
# 2 is closed vehicle, 3 is subway
open_vehicles <- c('pedestrian', 'cycle', 'motorcycle', 'auto_rickshaw', 'shared_auto', 'cycle_rickshaw')
rail_vehicles <- c('subway','rail')
vent_rates$vehicle_ratio_index <- sapply(vent_rates$stage_mode,function(x) ifelse(x %in% rail_vehicles, 3, 
                                                                                  ifelse(x %in% open_vehicles, 1, 2)))

# Copy trips dataset
trip_set <- trips

# Rename short walks as pedestrian 
trip_set$stage_mode[trip_set$stage_mode=='walk_to_pt'] <- 'pedestrian'

# trip set is a data.table, vent_rates is a data.frame, returns a data.table
trip_set <- dplyr::left_join(trip_set, vent_rates, 'stage_mode')

# litres of air inhaled are the product of the ventilation rate and the time (hours/60) spent travelling by that mode
trip_set$on_road_air <- trip_set$stage_duration*trip_set$vent_rate / (60) # L

# get indices for quick matching of values
scen_index <- match(trip_set$scenario, SCEN)

# ordered pm values
scen_pm <- as.numeric(conc_pm[scen_index])

# ordered ratios based on scenario and open/closed mode
scen_ratio <- ratio_by_mode[cbind(trip_set$vehicle_ratio_index, scen_index)]

# pm dose in mg as the product of the air inhaled, the background pm, and the exposure ratio
trip_set$pm_dose <- trip_set$on_road_air * scen_pm * scen_ratio # mg

# prepare individual-level dataset
synth_pop <- setDT(synth_population)

# take subset
trip_set <- setDT(trip_set)[trip_set$participant_id%in%synth_pop$participant_id,]

# compute individual-level pm scenario by scenario
for (i in 1:length(SCEN)){
  # initialise to background. This means persons who undertake zero travel get this value.
  synth_pop[[paste0('pm_conc_',SCEN_SHORT_NAME[i])]] <- conc_pm[i]
  # take trips from this scenario, and exclude trips by individuals not in the synthetic population (which might be truck trips)
  scen_trips <- trip_set[trip_set$scenario == SCEN[i],]
  
  # summarise individual-level time on road, pm inhaled, and air inhaled
  individual_data <- scen_trips[,.(on_road_dur = sum(stage_duration, na.rm=TRUE), 
                                   on_road_pm = sum(pm_dose, na.rm=TRUE)), by = 'participant_id']#, 
  
  # calculate non-travel air inhalation
  non_transport_air_inhaled <- (24 - individual_data$on_road_dur / 60) * BASE_LEVEL_INHALATION_RATE
  
  # concentration of pm inhaled = total pm inhaled / total air inhaled
  pm_conc <- ((non_transport_air_inhaled * as.numeric(conc_pm[i])) + individual_data$on_road_pm)/24#/(non_transport_air_inhaled+individual_data$air_inhaled)
  
  # match individual ids to set per person pm exposure
  synth_pop[[paste0('pm_conc_',SCEN_SHORT_NAME[i])]][
    match(individual_data$participant_id,synth_pop$participant_id)] <- pm_conc
}

# io$antofagasta$outcomes$pm_conc_pp |> 
#   dplyr::select(contains("pm_conc")) |> 
#   summary() |>
#   print()names

summary(synth_pop |> dplyr::select(contains("pm_conc")) |> setNames(SCEN_QN)) |> 
  print()
# Baseline       Bicycling        Driving      Public Transport
# Min.   :10.49   Min.   :10.39   Min.   :10.68   Min.   :10.38   
# 1st Qu.:10.58   1st Qu.:10.49   1st Qu.:10.77   1st Qu.:10.47   
# Median :10.78   Median :10.75   Median :10.95   Median :10.69   
# Mean   :11.01   Mean   :11.12   Mean   :11.16   Mean   :10.89   
# 3rd Qu.:11.17   3rd Qu.:11.39   3rd Qu.:11.31   3rd Qu.:11.08   
# Max.   :17.95   Max.   :33.66   Max.   :18.24   Max.   :17.77 
# Write it as a CSV
write_csv(synth_pop, here(abs_path, "AP_PM25_antofagasta.csv"))