# Clear workspace
rm (list = ls())

# load libraries
require(tidyverse)
require(drpa)


# Read scenario data
rd <- read_csv("data/scenarios/accra/baseline_and_three_scenarios.csv")

# Create dummy ind pop
ind <- rd %>% group_by(participant_id) %>% summarise(sex = first(sex),
                                                     age_cat = first(age_cat))

# Read disease lt
disease_lt <- read_csv("data/dose_response/disease_outcomes_lookup.csv")

disease_lt[is.na(disease_lt)] <- 0

for (i in 1:nrow(disease_lt)){
  
  for (scen in c('base', 'scen1', 'scen2', 'scen3')){
    # i <- 1
    if (disease_lt$air_pollution[i] == 1 & disease_lt$physical_activity[i] ){
      
      ind[[paste0(scen, '_rr_ap_pa_', disease_lt$acronym[i])]] <- runif(nrow(ind), min = 0, max = 1)
      
    }
    else if (disease_lt$air_pollution[i] == 1 & disease_lt$physical_activity[i] != 1){
      
      ind[[paste0(scen, '_rr_ap_', disease_lt$acronym[i])]] <- runif(nrow(ind), min = 0, max = 1)
      
    } else if (disease_lt$air_pollution[i] != 1 & disease_lt$physical_activity[i] == 1){
      
      ind[[paste0(scen, '_rr_pa_', disease_lt$acronym[i])]] <- runif(nrow(ind), min = 0, max = 1)
    }
    
  }
}


