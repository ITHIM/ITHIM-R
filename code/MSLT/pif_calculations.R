rm (list = ls())

# load dr for pa objects
source("code/drpa/dose_response.R")

require(dplyr)
require(tidyverse)

mmets <- read_csv("code/MSLT/data/england/mmets_london.csv")

## Remove all scenario but one for simplicity

MS_cols <- names(mmets %>% dplyr::select(starts_with("MS")))

mmets <- select(mmets, -c(MS_cols[-1]))

disease_short_names <- data.frame(disease = c("all-cause-mortality", 
                                              "diabetes",
                                              "lung-cancer",
                                              "breast-cancer", 
                                              "colon-cancer",
                                              "coronary-heart-disease",
                                              "stroke"),
                                  sname = c("ac", "dm", "tblc", "bc",
                                            "cc", "ihd", "is"))

# 
# 'all-cause-mortality', 'breast-cancer', 'cardiovascular-disease',
# 'colon-cancer', 'coronary-heart-disease', 'diabetes', 'endometrial-cancer',
# 'heart-failure', 'lung-cancer', 'stroke', 'total-cancer'

pa_certainty <- T

### iterating over all all disease outcomes
for ( j in 1:nrow(disease_short_names)){
  ## checking whether to calculate this health outcome for PA
  pa_dn <- disease_short_names$sname[j] %>% as.character()
  local_cause <- disease_short_names$disease[j] %>% as.character()
  
  for (scen in append('baseline', names(mmets %>% dplyr::select(starts_with("MS"))))){
    
    mmets[[paste('RR_pa', scen, pa_dn, sep = '_')]] <- -1
    
    
    for (i in 1:nrow(mmets)){
      
      
      if (local_cause == 'all-cause-mortality'){
        
        mmets[[paste('RR_pa', scen, pa_dn, sep = '_')]][i] <- dose_response(cause = local_cause, outcome_type = 'mortality', certainty = pa_certainty, 
                                                                            dose = mmets[[scen]][i], use_75_pert = T)$rr %>% as.numeric()
        
      }else if(local_cause == 'coronary-heart-disease'){
        mmets[[paste('RR_pa', scen, pa_dn, sep = '_')]][i] <- dose_response(cause = local_cause, outcome_type = 'mortality', certainty = pa_certainty, 
                                                                            dose = ifelse(mmets[[scen]][i] <= 35, mmets[[scen]][i], 35),
                                                                            use_75_pert = F)$rr %>% as.numeric()
      }else if(local_cause == 'breast-cancer'){
        mmets[[paste('RR_pa', scen, pa_dn, sep = '_')]][i] <- dose_response(cause = local_cause, outcome_type = 'incidence', certainty = pa_certainty, 
                                                                            dose = ifelse(mmets[[scen]][i] <= 10, mmets[[scen]][i], 10),
                                                                            use_75_pert = F)$rr %>% as.numeric()
        
      }else if(local_cause == 'colon-cancer'){
        mmets[[paste('RR_pa', scen, pa_dn, sep = '_')]][i] <- dose_response(cause = local_cause, outcome_type = 'incidence', certainty = pa_certainty, 
                                                                            dose = ifelse(mmets[[scen]][i] <= 35, mmets[[scen]][i], 35),
                                                                            use_75_pert = F)$rr %>% as.numeric()
        
      }
      
      else if(local_cause == 'lung-cancer'){
        mmets[[paste('RR_pa', scen, pa_dn, sep = '_')]][i] <- dose_response(cause = local_cause, outcome_type = 'incidence', certainty = pa_certainty, 
                                                                            dose = ifelse(mmets[[scen]][i] <= 10, mmets[[scen]][i], 10),
                                                                            use_75_pert = F)$rr %>% as.numeric()
        
      }else if(local_cause == 'stroke'){
        mmets[[paste('RR_pa', scen, pa_dn, sep = '_')]][i] <- dose_response(cause = local_cause, outcome_type = 'incidence',certainty = pa_certainty, 
                                                                            dose = ifelse(mmets[[scen]][i] <= 13.37, mmets[[scen]][i], 13.37),
                                                                            use_75_pert = F)$rr %>% as.numeric()
      }else if(local_cause == 'diabetes'){
        mmets[[paste('RR_pa', scen, pa_dn, sep = '_')]][i] <- dose_response(cause = local_cause, outcome_type = 'mortality', certainty = pa_certainty, 
                                                                         dose = mmets[[scen]][i], use_75_pert = F)$rr %>% as.numeric()
        
      }else if(local_cause == 'total-cancer'){
        mmets[[paste('RR_pa', scen, pa_dn, sep = '_')]][i] <- dose_response(cause = local_cause, outcome_type = 'mortality', certainty = pa_certainty, 
                                                                            dose = ifelse(mmets[[scen]][i] <= 35, mmets[[scen]][i], 35),
                                                                            use_75_pert = F)$rr %>% as.numeric()
        
      }
    }
  }
  
}