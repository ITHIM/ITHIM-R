# Load packages
library(tidyverse)
library(sqldf)
# Read combined individual travel survey and Physical Activity data
# baseline <- read.csv("PA/data/180219_Metahit10000_v2_nolabel.csv", header = T, stringsAsFactors = F)

baseline <- readstata13::read.dta13("PA/data/SPtrip_CensusNTSAPS_E06000001.dta", convert.factors = T, generate.factors = F)

baseline$trip_cycletime_hr <- baseline$trip_cycletime_min / 60

baseline$trip_walktime_hr <- baseline$trip_walktime_min / 60

# Initialize  energy expenditure constants - taken from ICT
METCycling <- 5.63
METWalking <- 3.53
METEbikes <- 4.50

individual_mmet <- sqldf('select census_id, female, agecat, sum(trip_cycletime_hr) as cycleNTS_wkhr,
                         sum(trip_walktime_hr) as walkNTS_wkhr, sport_wkmmets
                         from baseline group by census_id')

individual_mmet$total_mmet <- ((METCycling - 1) * individual_mmet$cycleNTS_wkhr) + 
                          ((METWalking - 1) * individual_mmet$walkNTS_wkhr) + individual_mmet$sport_wkmmets

individual_mmet$total_mmet <- ifelse(is.na(individual_mmet$total_mmet), 0, individual_mmet$total_mmet)

individual_mmet  %>% group_by(female, agecat) %>% summarise(mean = mean(total_mmet))

