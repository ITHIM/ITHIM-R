# Remove everything
rm (list = ls())
# Load packages
library(tidyverse)
library(haven)
library(sqldf)

#Set seed
set.seed(1)

# Load all functions
source("PA/code/functions.R")

# Constants
# Initialize  energy expenditure constants - taken from ICT
METCycling <- 5.63
METWalking <- 3.53
METEbikes <- 4.50

# Read combined individual travel survey and Physical Activity data
raw_data <- haven::read_dta("PA/data/SPtrip_CensusNTSAPS_E06000001.dta")

# Create id columns for all trips (and people without any trips)
raw_data$id <- 1:nrow(raw_data)

# Create a lookup table for selected columns - labelled columns from stata
lt <- create.lookups(raw_data, c("female", "agecat", "agecat_det", "trip_mainmode"))

# Sample 10k unique IDs
# Select trips for the 10k people
baseline <- raw_data %>% filter(census_id %in% sample(unique(census_id), 1000)) 

# Remove labels
baseline <- clear.labels(baseline)
# Replace NAs with 0
baseline$trip_cycletime_min <- ifelse(is.na(baseline$trip_cycletime_min), 0, baseline$trip_cycletime_min)
# Replace NAs with 0
baseline$trip_walktime_min <- ifelse(is.na(baseline$trip_walktime_min), 0, baseline$trip_walktime_min)
# Calculate trip_cycletime_hr by dividing trip_cycletime_min by 60
baseline$trip_cycletime_hr <- baseline$trip_cycletime_min / 60
# Calculate trip_walktime_hr by dividing trip_walktime_min by 60
baseline$trip_walktime_hr <- baseline$trip_walktime_min / 60

# Get total individual level walking and cycling and sport mmets 
individual_mmet <- sqldf('select census_id, female, agecat, sum(trip_cycletime_hr) as cycleNTS_wkhr,
                         sum(trip_walktime_hr) as walkNTS_wkhr,
                         sport_wkmmets
                         from baseline group by census_id')

# Sum all mmets for individuals
individual_mmet$total_mmet <- ((METCycling - 1) * individual_mmet$cycleNTS_wkhr) + 
                          ((METWalking - 1) * individual_mmet$walkNTS_wkhr) + individual_mmet$sport_wkmmets

# Replace NAs with 0
individual_mmet$total_mmet <- ifelse(is.na(individual_mmet$total_mmet), 0, individual_mmet$total_mmet)

# Create a summary of mmets by gender and age
b_mmet <- individual_mmet  %>% group_by(female, agecat) %>% summarise(mean = mean(total_mmet))

# Define a scenario
# Switch modes
# Create cycling specifics distances

# Sample 100 unique individual IDs
# Select non-cycling trips for the 10k people
sc <- baseline %>% filter(trip_mainmode != (filter(lt, names == 'trip_mainmode' & val == 'Bicycle') %>% distinct(id) %>% as.integer()) & census_id %in% sample(unique(census_id), 100))
# Convert those trips to cycling
sc$trip_mainmode <- filter(lt, names == 'trip_mainmode' & val == 'Bicycle') %>% distinct(id) %>% as.integer()
# Create trip_cycletime_hr by dividing trip_cycletime_min by 60
sc$trip_cycletime_hr <- sc$trip_durationraw_min / 60
# Create a copy of baseline var
nbaseline <- baseline
# Update selected rows by sc
nbaseline[nbaseline$id %in% sc$id, ] <- sc

# Sum all mmets for individuals
individual_mmet_sc <- sqldf('select census_id, female, agecat, sum(trip_cycletime_hr) as cycleNTS_wkhr,
                         sum(trip_walktime_hr) as walkNTS_wkhr,
                         sport_wkmmets
                         from nbaseline group by census_id')

# Calculate individual sum of mmets for the scenario
individual_mmet_sc$total_mmet <- ((METCycling - 1) * individual_mmet_sc$cycleNTS_wkhr) + 
  ((METWalking - 1) * individual_mmet_sc$walkNTS_wkhr) + individual_mmet_sc$sport_wkmmets

# Replace NAs with 0
individual_mmet_sc$total_mmet <- ifelse(is.na(individual_mmet_sc$total_mmet), 0, individual_mmet_sc$total_mmet)
# Create a summary of mmets by gender and age
sc_mmet <- individual_mmet_sc  %>% group_by(female, agecat) %>% summarise(mean = mean(total_mmet))
