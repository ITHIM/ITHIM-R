require(tidyverse)
require(ithimr)
require(data.table)
io <- readRDS("results/multi_city/io.rds")

cities <- names(io)[!names(io) %in% 'scen_prop']

cities <- c("arica", "belo_horizonte")

DAY_TO_WEEK_TRAVEL_SCALAR = 7

# constant parameters for MMET_CYCLING
MMET_CYCLING <- 4.63
# constant parameters for MMET_WALKING
MMET_WALKING <- 2.53

atlist <- list()

for (city in cities){
  
  arica <- io[[city]]$trip_scen_sets %>% mutate(scenario = case_when(
    scenario == "Scenario 1" ~ "CYC_SC", 
    scenario == "Scenario 2" ~ "CAR_SC",
    scenario == "Scenario 3" ~ "BUS_SC",
    scenario == "Baseline" ~ "Baseline"))
  
  SYNTHETIC_POPULATION <- io[[city]]$synth_pop
  
  SCEN <- SCEN_SHORT_NAME <- unique(arica$scenario)
  
  BACKGROUND_PA_SCALAR <- 1
  
  mmets <- ithimr::total_mmet(arica)
  
  arica_bl <- arica %>% filter(scenario == "Baseline")
  arica_sc <- arica %>% filter(scenario == "BUS_SC")
  bmtrids <- arica_sc %>% filter(trip_mode == "bus") %>% dplyr::select(trip_id) %>% distinct()
  blatids <- arica_bl %>% filter(trip_mode %in% c('cycle', 'pedestrian')) %>% dplyr::select(trip_id, participant_id)# %>% distinct()
  print(city)
  print(blatids %>% filter(trip_id %in% bmtrids$trip_id) %>% nrow())
  
  # idset <- blatids %>% filter(trip_id %in% bmtrids$trip_id) %>% dplyr::select(trip_id)
  
  idset <- blatids %>% filter(trip_id %in% bmtrids$trip_id) %>% dplyr::select(participant_id)
  
  tdf <- arica_bl %>% filter(participant_id %in% idset$participant_id) %>% mutate(city = city) %>% arrange(participant_id)
  
  mmets_sdf <- mmets %>% dplyr::select(participant_id, Baseline_mmet, BUS_SC_mmet)
  
  atlist[[city]] <- left_join(tdf, mmets_sdf)
  
  # atlist[[city]] <- arica_bl %>% filter(trip_id %in% idset$trip_id) %>% mutate(city = city) %>% group_by(participant_id) %>% 
  #   summarise(age, sex, age_cat, total_trips = n_distinct(trip_id), city) %>% distinct(participant_id, .keep_all = T)
    #dplyr::select(participant_id, age, sex, age_cat, city) %>% distinct(participant_id, .keep_all = T)
}

atdf <- data.table::rbindlist(atlist)

write_csv(atdf, "results/multi_city/scen_distr/bus_sc_AT_demo_by_cities.csv")


atlist <- list()

for (city in cities){
  arica <- io[[city]]$trip_scen_sets %>% mutate(scenario = case_when(
    scenario == "Scenario 1" ~ "CYC_SC", 
    scenario == "Scenario 2" ~ "CAR_SC",
    scenario == "Scenario 3" ~ "BUS_SC",
    scenario == "Baseline" ~ "Baseline"))
  
  SYNTHETIC_POPULATION <- io[[city]]$synth_pop
  
  SCEN <- SCEN_SHORT_NAME <- unique(arica$scenario)
  
  BACKGROUND_PA_SCALAR <- 1
  
  mmets <- ithimr::total_mmet(arica)
  
  
  arica_bl <- arica %>% filter(scenario == "Baseline")
  arica_sc <- arica %>% filter(scenario == "BUS_SC")
  bmtrids <- arica_sc %>% filter(trip_mode == "bus") %>% dplyr::select(trip_id) %>% distinct()
  blatids <- arica_bl %>% filter(!trip_mode %in% c('cycle', 'pedestrian')) %>% dplyr::select(trip_id, participant_id)
  print(city)
  print(blatids %>% filter(trip_id %in% bmtrids$trip_id) %>% nrow())
  
  idset <- blatids %>% filter(trip_id %in% bmtrids$trip_id) %>% dplyr::select(participant_id)
  
  tdf <- arica_bl %>% filter(participant_id %in% idset$participant_id) %>% mutate(city = city) %>% arrange(participant_id)
  
  mmets_sdf <- mmets %>% dplyr::select(participant_id, Baseline_mmet, BUS_SC_mmet)
  
  atlist[[city]] <- left_join(tdf, mmets_sdf)
  
  # idset <- blatids %>% filter(trip_id %in% bmtrids$trip_id) %>% dplyr::select(trip_id)
  # 
  # atlist[[city]] <- arica_bl %>% filter(trip_id %in% idset$trip_id) %>% mutate(city = city) %>% group_by(participant_id) %>% 
  #   summarise(age, sex, age_cat, total_trips = n_distinct(trip_id), city) %>% distinct(participant_id, .keep_all = T)
  #dplyr::select(participant_id, age, sex, age_cat, city) %>% distinct(participant_id, .keep_all = T)
}

ptdf <- data.table::rbindlist(atlist)

write_csv(ptdf, "results/multi_city/scen_distr/bus_sc_PT_demo_by_cities.csv")
