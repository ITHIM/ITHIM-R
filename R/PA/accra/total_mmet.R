# Clear workspace
rm (list = ls())
library(tidyverse)


# Read raw_data
rd <- read_csv("data/scenarios/accra/baseline_and_three_scenarios.csv")

# Multiply df by 6 times

# Filter people with trips
rd_pwt <- filter(rd, trip_id != 0)

rd_pwot <- filter(rd, trip_id == 0)

rd_pwt <- bind_rows(replicate(6, rd_pwt, simplify = FALSE))

rd <- rbind(rd_pwt, rd_pwot)

# Constants
# Initialize  energy expenditure constants - taken from ICT
MMETCycling <- 5.63 - 1
MMETWalking <- 3.53 - 1
MMETEbikes <- 4.50 - 1


# Convert baseline's trip duration from mins to hours
rd$trip_duration_hrs <- rd$trip_duration / 60


# Get total individual level walking and cycling and sport mmets 
ind <- rd %>% group_by(participant_id) %>% summarise ( sex = first(sex), 
                                                       age_cat = first(age_cat), 
                                                       cycling_mmet_base = (sum(trip_duration_hrs[trip_mode == 'Bicycle']) * MMETCycling),
                                                       walking_mmet_base = ((sum(trip_duration_hrs[trip_mode == 'Walking']) + 
                                                                               sum(trip_duration_hrs[trip_mode == 'Short Walking'])) * MMETCycling),
                                                       cycling_mmet_scen1 = (sum(trip_duration_hrs[scen1_mode == 'Bicycle']) * MMETCycling),
                                                       walking_mmet_scen1 = ((sum(trip_duration_hrs[scen1_mode == 'Walking']) + 
                                                                                sum(trip_duration_hrs[scen1_mode == 'Short Walking'])) * MMETCycling),
                                                       cycling_mmet_scen2 = (sum(trip_duration_hrs[scen2_mode == 'Bicycle']) * MMETCycling),
                                                       walking_mmet_scen2 = ((sum(trip_duration_hrs[scen2_mode == 'Walking']) + 
                                                                                sum(trip_duration_hrs[scen2_mode == 'Short Walking'])) * MMETCycling),
                                                       cycling_mmet_scen3 = (sum(trip_duration_hrs[scen3_mode == 'Bicycle']) * MMETCycling),
                                                       walking_mmet_scen3 = ((sum(trip_duration_hrs[scen3_mode == 'Walking']) + 
                                                                                sum(trip_duration_hrs[scen3_mode == 'Short Walking'])) * MMETCycling),
                                                       work_ltpa_mmet = first(work_ltpa_marg_met),
                                                       base_mmet = first(work_ltpa_marg_met) +  cycling_mmet_base + walking_mmet_base,
                                                       scen1_mmet = first(work_ltpa_marg_met) +  cycling_mmet_scen1 + walking_mmet_scen1,
                                                       scen2_mmet = first(work_ltpa_marg_met) +  cycling_mmet_scen2 + walking_mmet_scen2,
                                                       scen3_mmet = first(work_ltpa_marg_met) +  cycling_mmet_scen3 + walking_mmet_scen3
                                                       
)
