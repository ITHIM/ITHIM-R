# Load packages
library(tidyverse)
library(sqldf)
# Read combined individual travel survey and Physical Activity data
# baseline <- read.csv("PA/data/180219_Metahit10000_v2_nolabel.csv", header = T, stringsAsFactors = F)

baseline <- readstata13::read.dta13("PA/data/SPtrip_CensusNTSAPS_E06000001.dta")


## Convert factors to non-factors
baseline$female <- as.integer(baseline$female)
baseline$agecat <- as.character(baseline$agecat)

baseline$trip_cycletime_hr <- baseline$trip_cycletime_min / 60

baseline$trip_walktime_hr <- baseline$trip_walktime_min / 60

# Initialize  energy expenditure constants - taken from ICT
METCycling <- 5.63
METWalking <- 3.53
METEbikes <- 4.50

individual_mmet <- sqldf('select census_id, female, agecat, sum(trip_cycletime_hr) as cycleNTS_wkhr,
                         sum(trip_walktime_hr) as walkNTS_wkhr, 
                         sport_wkmmets
                         from baseline group by census_id')

individual_mmet$total_mmet <- ((METCycling - 1) * individual_mmet$cycleNTS_wkhr) + 
                          ((METWalking - 1) * individual_mmet$walkNTS_wkhr) + individual_mmet$sport_wkmmets

individual_mmet$total_mmet <- ifelse(is.na(individual_mmet$total_mmet), 0, individual_mmet$total_mmet)

individual_mmet  %>% group_by(female, agecat) %>% summarise(mean = mean(total_mmet))

# # A tibble: 12 x 3
# # Groups:   female [?]
# female agecat    mean
# <fct>  <fct>    <dbl>
#   1 No     16 to 24 28.5 
# 2 No     25 to 34 27.0 
# 3 No     35 to 49 14.3 
# 4 No     50 to 64  8.88
# 5 No     65 to 74  7.05
# 6 No     75 plus   4.68
# 7 Yes    16 to 24 13.9 
# 8 Yes    25 to 34 11.6 
# 9 Yes    35 to 49  9.81
# 10 Yes    50 to 64  7.19
# 11 Yes    65 to 74  5.35
# 12 Yes    75 plus   3.60
