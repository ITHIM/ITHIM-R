rm (list = ls())
library(tidyverse)

io <- readRDS("results/multi_city/io.rds")
# Assumes that multi_city_script.R has been run till 
cities <- c('accra','sao_paulo','delhi','bangalore')

total_distance <- list()
total_duration <- list()

index <- 1

# total_male_distance <- 
  
groups <- c('All', "Male", "Female")

for(city in cities){
  for (groupings in groups){
    if (groupings == 'All')
      count_people <- length(unique(io[[city]]$synth_pop$participant_id))
    else if (groupings %in% c('Male', 'Female'))
      count_people <- filter(io[[city]]$synth_pop, sex == groupings) %>% distinct(participant_id) %>% nrow()
    
    td <- io[[city]]$trip_scen_sets %>% filter(scenario == 'Baseline') %>% 
      group_by(stage_mode, scenario) %>% 
      summarise(dur = round(sum(stage_distance) / count_people, 1)) %>% 
      #mutate(grouping = groupings) %>%
      spread(key = scenario, value = dur)
    
    names(td)[2] <- paste0(city, "_", groupings)
    
    if (index == 1){
      total_distance <- td
    }else{
      total_distance <- full_join(total_distance, td, by = "stage_mode")
    }
    
    td <- io[[city]]$trip_scen_sets %>% filter(scenario == 'Baseline') %>% 
      group_by(stage_mode, scenario) %>% 
      summarise(dur = round(sum(stage_duration) / count_people, 1)) %>% #/ count_people
      #mutate(grouping = groupings) %>%
      spread(key = scenario, value = dur)
    
    names(td)[2] <- paste0(city, "_", groupings)
    
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
    
    td <- io[[city]]$trip_scen_sets %>% filter(scenario == 'Baseline') %>% 
      group_by(stage_mode, scenario) %>% 
      summarise(dur = round(sum(stage_distance) / count_people, 1)) %>% 
      #mutate(grouping = groupings) %>%
      spread(key = scenario, value = dur)
    
    names(td)[2] <- paste0(city, "_", groupings)
    
    if (index == 1){
      total_distance_trips <- td
    }else{
      total_distance_trips <- full_join(total_distance_trips, td, by = "stage_mode")
    }
    
    td <- io[[city]]$trip_scen_sets %>% filter(scenario == 'Baseline') %>% 
      group_by(stage_mode, scenario) %>% 
      summarise(dur = round(sum(stage_duration) / count_people, 1)) %>% #/ count_people
      #mutate(grouping = groupings) %>%
      spread(key = scenario, value = dur)
    
    names(td)[2] <- paste0(city, "_", groupings)
    
    if (index == 1){
      total_duration_trips <- td
    }else{
      total_duration_trips <- full_join(total_duration_trips, td, by = "stage_mode")
    }
    
    index <- index + 1
    
  }
  
}