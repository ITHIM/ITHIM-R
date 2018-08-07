# Clear workspace
rm (list = ls())
library(tidyverse)


# Read raw_data
rd <- read_csv("data/scenarios/accra/baseline_and_scenarios.csv")

# Constants
# Initialize  energy expenditure constants - taken from ICT
MMETCycling <- 5.63 - 1
MMETWalking <- 3.53 - 1
MMETEbikes <- 4.50 - 1


# Convert baseline's trip duration from mins to hours
rd$trip_duration_hrs <- rd$trip_duration / 60

# Get total individual level walking and cycling and sport mmets 
ind <- rd %>% group_by(participant_id) %>% summarise ( sex = first(sex), 
                                                       age = first(age),
                                                       age_cat = first(age_cat), 
                                                       cycling_mmet_base = (sum(trip_duration_hrs[trip_mode == 'Bicycle' & scenario == 'Baseline']) * MMETCycling),
                                                       walking_mmet_base = ((sum(trip_duration_hrs[trip_mode == 'Walking'  & scenario == 'Baseline']) + 
                                                                               sum(trip_duration_hrs[trip_mode == 'Short Walking'  & scenario == 'Baseline'])) * MMETCycling),
                                                       
                                                       cycling_mmet_scen1 = (sum(trip_duration_hrs[trip_mode == 'Bicycle'  & scenario == 'Scenario 1']) * MMETCycling),
                                                       walking_mmet_scen1 = ((sum(trip_duration_hrs[trip_mode == 'Walking'  & scenario == 'Scenario 1']) + 
                                                                                sum(trip_duration_hrs[trip_mode == 'Short Walking'   & scenario == 'Scenario 1'])) * MMETCycling),
                                                       
                                                       cycling_mmet_scen2 = (sum(trip_duration_hrs[trip_mode == 'Bicycle'   & scenario == 'Scenario 2']) * MMETCycling),
                                                       walking_mmet_scen2 = ((sum(trip_duration_hrs[trip_mode == 'Walking'   & scenario == 'Scenario 2']) + 
                                                                                sum(trip_duration_hrs[trip_mode == 'Short Walking'   & scenario == 'Scenario 2'])) * MMETCycling),
                                                       
                                                       cycling_mmet_scen3 = (sum(trip_duration_hrs[trip_mode == 'Bicycle'   & scenario == 'Scenario 3']) * MMETCycling),
                                                       walking_mmet_scen3 = ((sum(trip_duration_hrs[trip_mode == 'Walking'   & scenario == 'Scenario 3']) + 
                                                                                sum(trip_duration_hrs[trip_mode == 'Short Walking'  & scenario == 'Scenario 3'])) * MMETCycling),
                                                       
                                                       cycling_mmet_scen4 = (sum(trip_duration_hrs[trip_mode == 'Bicycle'   & scenario == 'Scenario 4']) * MMETCycling),
                                                       walking_mmet_scen4 = ((sum(trip_duration_hrs[trip_mode == 'Walking'   & scenario == 'Scenario 4']) + 
                                                                                sum(trip_duration_hrs[trip_mode == 'Short Walking'  & scenario == 'Scenario 4'])) * MMETCycling),
                                                       
                                                       cycling_mmet_scen5 = (sum(trip_duration_hrs[trip_mode == 'Bicycle'   & scenario == 'Scenario 5']) * MMETCycling),
                                                       walking_mmet_scen5 = ((sum(trip_duration_hrs[trip_mode == 'Walking'   & scenario == 'Scenario 5']) + 
                                                                                sum(trip_duration_hrs[trip_mode == 'Short Walking'  & scenario == 'Scenario 5'])) * MMETCycling),
                                                       
                                                       work_ltpa_mmet = first(work_ltpa_marg_met),
                                                       base_mmet = first(work_ltpa_marg_met) +  cycling_mmet_base + walking_mmet_base,
                                                       scen1_mmet = first(work_ltpa_marg_met) +  cycling_mmet_scen1 + walking_mmet_scen1,
                                                       scen2_mmet = first(work_ltpa_marg_met) +  cycling_mmet_scen2 + walking_mmet_scen2,
                                                       scen3_mmet = first(work_ltpa_marg_met) +  cycling_mmet_scen3 + walking_mmet_scen3,
                                                       scen4_mmet = first(work_ltpa_marg_met) +  cycling_mmet_scen4 + walking_mmet_scen4,
                                                       scen5_mmet = first(work_ltpa_marg_met) +  cycling_mmet_scen5 + walking_mmet_scen5
                                                       
)


ind <- select(ind, participant_id, sex, age, age_cat, base_mmet, scen1_mmet, scen2_mmet, scen3_mmet, scen4_mmet, scen5_mmet)

write_csv(ind,'data/synth_pop_data/accra/pa/pa_total_mmet_weekly.csv')