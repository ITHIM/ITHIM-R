# Load libraries
library(tidyverse)
library(janitor)
library(here)
library(data.table)
library(stringi)

# Import dist and trip dataset
# 

# Static local and global parameters read from 
# 
PM_CONC_BASE <- 10.49221667
PM_TRANS_SHARE <- 0.31
MMET_CYCLING <- 4.63
MMET_WALKING <- 2.53

# Five fixed parameters: BASE_LEVEL_INHALATION_RATE (10), CLOSED_WINDOW_PM_RATIO (0.5), 
# CLOSED_WINDOW_RATIO (0.5), ROAD_RATIO_MAX (3.216), ROAD_RATIO_SLOPE (0.379)
BASE_LEVEL_INHALATION_RATE <- 1 # NOTE: the above comment says 10, but it is actually 1
CLOSED_WINDOW_PM_RATIO <- 0.5
CLOSED_WINDOW_RATIO <- 0.5
ROAD_RATIO_MAX <- 3.216
ROAD_RATIO_SLOPE <- 0.379

SUBWAY_PM_RATIO <- 0.8

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
scen_ratio <- ratio_by_mode[cbind(trip_set$vehicle_ratio_index,scen_index)]

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

# from model
# Min.   :10.49   Min.   :10.40   Min.   :10.69   Min.   :10.35  
# 1st Qu.:10.58   1st Qu.:10.50   1st Qu.:10.79   1st Qu.:10.44  
# Median :10.78   Median :10.76   Median :10.97   Median :10.65  
# Mean   :11.01   Mean   :11.13   Mean   :11.18   Mean   :10.86  
# 3rd Qu.:11.17   3rd Qu.:11.40   3rd Qu.:11.33   3rd Qu.:11.05  
# Max.   :17.95   Max.   :33.69   Max.   :18.27   Max.   :17.72 

summary(synth_pop |> dplyr::select(contains("pm_conc"))) |> print()

# pm_conc_baseline pm_conc_scenario_1 pm_conc_scenario_2 pm_conc_scenario_3
# Min.   :10.49    Min.   :10.40      Min.   :10.69      Min.   :10.35     
# 1st Qu.:10.58    1st Qu.:10.50      1st Qu.:10.79      1st Qu.:10.44     
# Median :10.78    Median :10.76      Median :10.97      Median :10.65     
# Mean   :11.01    Mean   :11.13      Mean   :11.18      Mean   :10.86     
# 3rd Qu.:11.17    3rd Qu.:11.40      3rd Qu.:11.33      3rd Qu.:11.05     
# Max.   :17.95    Max.   :33.69      Max.   :18.27      Max.   :17.72   

synth_pop$participant_id <- as.integer(synth_pop$participant_id)

list(scenario_pm=conc_pm, pm_conc_pp=as.data.frame(synth_pop))

# Write it as a CSV
write_csv(co2, "CO2_antofagasta.csv")