source("R/scenarios/accra/setup.R")

# Read the ind file with pa mmet
ind_pa <- read_csv("data/synth_pop_data/accra/pa/pa_total_mmet_weekly.csv")

# Read disease lt
disease_lt <- read.csv("data/dose_response/disease_outcomes_lookup.csv")

disease_lt[is.na(disease_lt)] <- 0

