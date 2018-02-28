# Load packages
library(tidyverse)
library(sqldf)
# Read combined individual travel survey and Physical Activity data
baseline <- read.csv("PA/data/180219_Metahit10000_v2_nolabel.csv", header = T, stringsAsFactors = F)

# Initialize  energy expenditure constants - taken from ICT
METCycling <- 5.63
METWalking <- 3.53
METEbikes <- 4.50

#calculate mmets in baseline
# Add minutes from both APS (walking, cycling and total sports) and NTS trips (walking and cycling)
baseline$METh <- (METWalking * baseline$trip_walktime_min / 60) + (METWalking * baseline$walkrec10_wkhr) +
  (METCycling * baseline$trip_cycletime_min / 60) + (METWalking * baseline$cyclerec_wkhr / 60) + baseline$sport_wkmets
baseline$MMETh <- ((METWalking - 1) * baseline$trip_walktime_min / 60) + ((METWalking - 1) * baseline$walkrec10_wkhr) +
((METCycling - 1) * baseline$trip_cycletime_min / 60) + ((METCycling - 1) * baseline$cyclerec_wkhr) + baseline$sport_wkmets

individual_mmet <- sqldf('select f.census_id, sum(f.MMETh) as mmets FROM baseline as f GROUP BY f.census_id')