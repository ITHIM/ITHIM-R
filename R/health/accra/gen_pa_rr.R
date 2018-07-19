source("R/scenarios/accra/setup.R")

# Read the ind file with pa mmet
ind <- read_csv("data/synth_pop_data/accra/pa/pa_total_mmet_weekly.csv")

# Read disease lt
disease_lt <- read.csv("data/dose_response/disease_outcomes_lookup.csv")

disease_lt[is.na(disease_lt)] <- 0


# Names
# RR_ap_base_cvd	RR_ap_scen1_cvd	RR_ap_scen2_cvd	RR_ap_scen3_cvd



### iterating over all all disease outcomes
for ( j in 1:nrow(disease_lt)){
  ## checking whether to calculate this health outcome for PA
  if (disease_lt$physical_activity[j] == 1){
    pa_dn <- disease_lt$pa_acronym[j] %>% as.character()
    pa_n <- disease_lt$acronym[j] %>% as.character()
    
    for (scen in c('base', 'scen1', 'scen2', 'scen3')){
      ind[[paste('RR_pa', scen, pa_n, sep = '_')]] <- NULL
      ## iterating over all individuals
      for (i in 1:nrow(ind)){
        
        if (pa_dn == 'all-cause-mortality'){
          
          ind[[paste('RR_pa', scen, pa_n, sep = '_')]][i] <- dose_response(cause = pa_dn, outcome_type = 'mortality',
                                                              dose = ind[[paste0(scen, '_mmet')]][i], use_75_pert = T)$rr %>% as.numeric()

          # CVD: 35 mmeth per week use mortality
          # Lung cancer: 10 mmeth per week use incidence
          # Breast cancer: 35 mmeth per week use incidence
          # Colon cancer : 35 mmeth per week use incidence
          # stroke 75 pert


        }else if(pa_dn == 'cardiovascular-disease'){
          ind[[paste('RR_pa', scen, pa_n, sep = '_')]][i] <- dose_response(cause = pa_dn, outcome_type = 'mortality',
                                                                 dose = ifelse(ind[[paste0(scen, '_mmet')]][i] <= 35, ind[[paste0(scen, '_mmet')]][i], 35),
                                                                 use_75_pert = F)$rr %>% as.numeric()
        }else if(pa_dn == 'lung-cancer'){
          ind[[paste('RR_pa', scen, pa_n, sep = '_')]][i] <- dose_response(cause = pa_dn, outcome_type = 'incidence',
                                                                 dose = ifelse(ind[[paste0(scen, '_mmet')]][i] <= 10, ind[[paste0(scen, '_mmet')]][i], 10),
                                                                 use_75_pert = F)$rr %>% as.numeric()

        }else if(pa_dn == 'breast-cancer'){
          ind[[paste('RR_pa', scen, pa_n, sep = '_')]][i] <- dose_response(cause = pa_dn, outcome_type = 'incidence',
                                                                 dose = ifelse(ind[[paste0(scen, '_mmet')]][i] <= 35, ind[[paste0(scen, '_mmet')]][i], 35),
                                                                 use_75_pert = F)$rr %>% as.numeric()

        }else if(pa_dn == 'colon-cancer'){
          ind[[paste('RR_pa', scen, pa_n, sep = '_')]][i] <- dose_response(cause = pa_dn, outcome_type = 'incidence',
                                                                 dose = ifelse(ind[[paste0(scen, '_mmet')]][i] <= 35, ind[[paste0(scen, '_mmet')]][i], 35),
                                                                 use_75_pert = F)$rr %>% as.numeric()

        }else if(pa_dn == 'stroke'){
          ind[[paste('RR_pa', scen, pa_n, sep = '_')]][i] <- dose_response(cause = pa_dn, outcome_type = 'incidence',
                                                                 dose = ind[[paste0(scen, '_mmet')]][i], use_75_pert = T)$rr %>% as.numeric()
        }else if(pa_dn == 'diabetes'){
          ind[[paste('RR_pa', scen, pa_n, sep = '_')]][i] <- dose_response(cause = pa_dn, outcome_type = 'mortality', 
                                                                           dose = ind[[paste0(scen, '_mmet')]][i], use_75_pert = F)$rr %>% as.numeric()
          
        }
        
        
      }
      
    }
    
  }
  
}

write_csv(ind, "data/synth_pop_data/accra/RR/RR_PA_calculations.csv")