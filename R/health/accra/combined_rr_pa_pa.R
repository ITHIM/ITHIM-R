source("R/scenarios/accra/setup.R")

# Read the ind file with rr for pa
ind_pa <- read_csv("data/synth_pop_data/accra/RR/RR_PA_calculations.csv")

# Read the ind file with rr for ap
ind_ap <- read_csv("data/synth_pop_data/accra/RR/RR_AP_calculations.csv")

# Data frame's function which replaces NaNs
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

# Replace NaNs with 1
ind_ap[is.nan(ind_ap)] <- 1

# Replace Na with 1
ind_pa[is.na(ind_pa)] <- 1

# remove common columns from ap
ind_ap <- select(ind_ap, -c(sex, age, age_cat))

# join pa and ap ind datasets
ind <- left_join(ind_pa, ind_ap, by = "participant_id")

# Read disease lt
disease_lt <- read.csv("data/dose_response/disease_outcomes_lookup.csv")

disease_lt[is.na(disease_lt)] <- 0

### iterating over all all disease outcomes
for ( j in 1:nrow(disease_lt)){
  ## checking whether to calculate this health outcome for PA
  if (disease_lt$physical_activity[j] == 1 & disease_lt$air_pollution[j] == 1){
    for (scen in c('base', 'scen1', 'scen2', 'scen3')){
      ac <- disease_lt$acronym[j] %>% as.character()
      ind[[paste('RR_pa_ap', scen, ac, sep = '_')]] <- ind[[paste('RR_pa', scen, ac, sep = '_')]] * ind[[paste('RR_ap', scen, ac, sep = '_')]]
      
      #ind[[paste('RR_pa', scen, ac, sep = '_')]] <- NULL
      #ind[[paste('RR_ap', scen, ac, sep = '_')]] <- NULL
    }
  }
}

write_csv(ind, 'data/synth_pop_data/accra/RR/RR_PA_AP_calculations.csv')