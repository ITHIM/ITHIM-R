rm (list = ls())
library(tidyverse)

io <- readRDS("results/multi_city/io.rds")
# Assumes that multi_city_script.R has been run till 
cities <- c('accra','sao_paulo','delhi','bangalore')

round_to <- 1

total_distance <- list()
total_duration <- list()

index <- 1
  
groups <- c('All', "Male", "Female")

for(city in cities){
  for (groupings in groups){
    if (groupings == 'All')
      count_people <- length(unique(io[[city]]$synth_pop$participant_id))
    else if (groupings %in% c('Male', 'Female'))
      count_people <- filter(io[[city]]$synth_pop, sex == groupings) %>% distinct(participant_id) %>% nrow()
    if (groupings == 'All'){
    td <- io[[city]]$trip_scen_sets %>% filter(scenario == 'Baseline') %>% 
      group_by(stage_mode, scenario) %>% 
      summarise(dur = round(sum(stage_distance) / count_people, round_to)) %>% 
      mutate(pop = count_people) %>%
      spread(key = scenario, value = dur)
    }else{
      td <- io[[city]]$trip_scen_sets %>% filter(scenario == 'Baseline' & sex == groupings) %>% 
        group_by(stage_mode, scenario) %>% 
        summarise(dur = round(sum(stage_distance) / count_people, round_to)) %>% 
        mutate(pop = count_people) %>%
        spread(key = scenario, value = dur)
      
    }
    
    names(td)[3] <- paste0(city, "_", groupings)
    
    if (index == 1){
      total_distance <- td
    }else{
      total_distance <- full_join(total_distance, td, by = "stage_mode")
    }
    
    if (groupings == 'All'){
      td <- io[[city]]$trip_scen_sets %>% filter(scenario == 'Baseline') %>% 
        group_by(stage_mode, scenario) %>% 
        summarise(dur = round(sum(stage_duration) / count_people, round_to)) %>% #
        mutate(pop = count_people) %>%
        spread(key = scenario, value = dur)  
      
    }else{
    
    td <- io[[city]]$trip_scen_sets %>% filter(scenario == 'Baseline' & sex == groupings) %>% 
      group_by(stage_mode, scenario) %>% 
      summarise(dur = round(sum(stage_duration) / count_people, round_to)) %>% #/ count_people
      mutate(pop = count_people) %>%
      spread(key = scenario, value = dur)
    }
    
    names(td)[3] <- paste0(city, "_", groupings)
    
    if (index == 1){
      total_duration <- td
    }else{
      total_duration <- full_join(total_duration, td, by = "stage_mode")
    }
    
    index <- index + 1
    
  }
  
}



total_distance_trips <- list()
total_duration_trips <- list()

index <- 1

# total_male_distance <- 

groups <- c('All', "Male", "Female")

for(city in cities){
  for (groupings in groups){
    if (groupings == 'All')
      count_people <- length(unique(io[[city]]$trip_scen_sets$participant_id))
    else if (groupings %in% c('Male', 'Female'))
      count_people <- filter(io[[city]]$trip_scen_sets, sex == groupings) %>% distinct(participant_id) %>% nrow()
    if (groupings == 'All'){
      td <- io[[city]]$trip_scen_sets %>% filter(scenario == 'Baseline') %>% 
        group_by(stage_mode, scenario) %>% 
        summarise(dur = round(sum(stage_distance) / count_people, round_to)) %>% 
        mutate(pop = count_people) %>%
        spread(key = scenario, value = dur)
    }else{
      td <- io[[city]]$trip_scen_sets %>% filter(scenario == 'Baseline' & sex == groupings) %>% 
        group_by(stage_mode, scenario) %>% 
        summarise(dur = round(sum(stage_distance) / count_people , round_to)) %>% 
        mutate(pop = count_people) %>%
        spread(key = scenario, value = dur)
    }
    
    names(td)[3] <- paste0(city, "_", groupings)
    
    if (index == 1){
      total_distance_trips <- td
    }else{
      total_distance_trips <- full_join(total_distance_trips, td, by = "stage_mode")
    }
    
    if (groupings == 'All'){
    
    td <- io[[city]]$trip_scen_sets %>% filter(scenario == 'Baseline') %>% 
      group_by(stage_mode, scenario) %>% 
      summarise(dur = round(sum(stage_duration) / count_people, round_to)) %>% #/ count_people
      mutate(pop = count_people) %>%
      spread(key = scenario, value = dur)
    
    }else{
      
      td <- io[[city]]$trip_scen_sets %>% filter(scenario == 'Baseline' & sex == groupings) %>% 
        group_by(stage_mode, scenario) %>% 
        summarise(dur = round(sum(stage_duration) / count_people, round_to)) %>% #/ count_people
        mutate(pop = count_people) %>%
        spread(key = scenario, value = dur)
      
    }
    
    names(td)[3] <- paste0(city, "_", groupings)
    
    if (index == 1){
      total_duration_trips <- td
    }else{
      total_duration_trips <- full_join(total_duration_trips, td, by = "stage_mode")
    }
    
    index <- index + 1
    
  }
  
}

m <- matrix("", ncol = 25, nrow = 1)
m[1, 1] <- "TRIP DISTANCE - TOTAL"
mdf <- data.frame(m)
names(mdf) <- names(total_distance)

local_distance_df <- plyr::rbind.fill(mdf, total_distance)

m <- matrix("", ncol = 25, nrow = 1)
m[1, 1] <- "TRIP DURATION - TOTAL"
mdf <- data.frame(m)
names(mdf) <- names(total_duration)

local_duration_df <- plyr::rbind.fill(mdf, total_duration)

m <- matrix("", ncol = 25, nrow = 1)
m[1, 1] <- "TRIP DISTANCE - PEOPLE WITH TRIPS"
mdf <- data.frame(m)
names(mdf) <- names(total_distance)

local_distance_trips_df <- plyr::rbind.fill(mdf, total_distance_trips)

m <- matrix("", ncol = 25, nrow = 1)
m[1, 1] <- "TRIP DURATION - PEOPLE WITH TRIPS"
mdf <- data.frame(m)
names(mdf) <- names(total_distance)

local_duration_trips_df <- plyr::rbind.fill(mdf, total_duration_trips)

final_df <- do.call("rbind", list(local_distance_df, local_distance_trips_df, local_duration_df, local_duration_trips_df))

final_df <- final_df %>% mutate_all(na_if,"")

names(final_df)[seq(2, ncol(final_df), by = 2)] <- "population"

write_csv(final_df, "results/multi_city/baseline_trips/trips_distance_duration.csv", na = "")
