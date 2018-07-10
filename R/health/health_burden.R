# Clear workspace
rm (list = ls())

# load libraries
require(tidyverse)
require(drpa)

# source
source("R/PA/code/functions.R")


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

pif <- data.frame(PAF(pop = ind, attr = c('sex', 'age_cat'), cn = c('base_rr_ap_pa_cvd', 'scen1_rr_ap_pa_cvd')))
pif <- arrange(pif, age.band, gender)


# Redefine non-factor based column classes
pif[,c("age.band", "gender")] <- lapply(pif[,c("age.band", "gender")], as.character)
pif[,c("base_rr_ap_pa_cvd", "scen1_rr_ap_pa_cvd")] <- lapply(pif[,c("base_rr_ap_pa_cvd", "scen1_rr_ap_pa_cvd")], as.double)


# Read gbd data
gbd_data <- read_csv("data/demographics/gbd/accra/GBD Accra.csv")

yll_dfs <- combine_health_and_pif(
  pop = pif,
  hc = gbd_data,
  hm = "YLLs (Years of Life Lost)",
  cn = c("base_rr_ap_pa_cvd", "scen1_rr_ap_pa_cvd"),
  hm_cause <- "All causes",
  hm_cn <- 'val_accra')


# Subset to get yll
yll <- as.data.frame(yll_dfs[1])
# Subset to get yll_reductions
yll_red <- as.data.frame(yll_dfs[2])


death_dfs <- combine_health_and_pif(
  pop = pif,
  hc = gbd_data,
  hm = "Deaths",
  cn = c("base_rr_ap_pa_cvd", "scen1_rr_ap_pa_cvd"),
  hm_cause <- "All causes",
  hm_cn <- 'val_accra')


# Subset to get yll
deaths <- as.data.frame(death_dfs[1])
# Subset to get yll_reductions
deaths_red <- as.data.frame(death_dfs[2])
