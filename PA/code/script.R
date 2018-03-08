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
# baseline <- read.csv("PA/data/180219_Metahit10000_v2_nolabel.csv", header = T, stringsAsFactors = F)
raw_data <- haven::read_dta("PA/data/SPtrip_CensusNTSAPS_E06000001.dta")

raw_data$id <- 1:nrow(raw_data)

lt <- create.lookups(raw_data, c("female", "agecat", "agecat_det", "trip_mainmode"))

# Sample 10k unique IDs
# Select trips for the 10k people
baseline <- raw_data %>% filter(census_id %in% sample(unique(census_id), 1000)) 

# Remove labels
baseline <- clear.labels(baseline)

baseline$trip_cycletime_min <- ifelse(is.na(baseline$trip_cycletime_min), 0, baseline$trip_cycletime_min)

baseline$trip_walktime_min <- ifelse(is.na(baseline$trip_walktime_min), 0, baseline$trip_walktime_min)

baseline$trip_cycletime_hr <- baseline$trip_cycletime_min / 60

baseline$trip_walktime_hr <- baseline$trip_walktime_min / 60

individual_mmet <- sqldf('select census_id, female, agecat, sum(trip_cycletime_hr) as cycleNTS_wkhr,
                         sum(trip_walktime_hr) as walkNTS_wkhr,
                         sport_wkmmets
                         from baseline group by census_id')

individual_mmet$total_mmet <- ((METCycling - 1) * individual_mmet$cycleNTS_wkhr) + 
                          ((METWalking - 1) * individual_mmet$walkNTS_wkhr) + individual_mmet$sport_wkmmets

individual_mmet$total_mmet <- ifelse(is.na(individual_mmet$total_mmet), 0, individual_mmet$total_mmet)

b_mmet <- individual_mmet  %>% group_by(female, agecat) %>% summarise(mean = mean(total_mmet))

# Define a scenario
# Switch modes
# Create cycling specifics distances

# Sample 10k unique IDs
# Select trips for the 10k people
sc <- baseline %>% filter(trip_mainmode != (filter(trip_mode, mode == 'Bicycle') %>% distinct(val) %>% as.integer()) & census_id %in% sample(unique(census_id), 100))
sc[sc$nts_tripid %in% sc$nts_tripid, ]$trip_cycletime_hr <- sc[sc$nts_tripid %in% sc$nts_tripid, ]$trip_durationraw_min / 60

nbaseline <- baseline
nbaseline[nbaseline$id %in% sc$id, ] <- sc


individual_mmet_sc <- sqldf('select census_id, female, agecat, sum(trip_cycletime_hr) as cycleNTS_wkhr,
                         sum(trip_walktime_hr) as walkNTS_wkhr,
                         sport_wkmmets
                         from nbaseline group by census_id')

individual_mmet_sc$total_mmet <- ((METCycling - 1) * individual_mmet_sc$cycleNTS_wkhr) + 
  ((METWalking - 1) * individual_mmet_sc$walkNTS_wkhr) + individual_mmet_sc$sport_wkmmets

individual_mmet_sc$total_mmet <- ifelse(is.na(individual_mmet_sc$total_mmet), 0, individual_mmet_sc$total_mmet)

sc_mmet <- individual_mmet_sc  %>% group_by(female, agecat) %>% summarise(mean = mean(total_mmet))
