rm (list = ls())

require(dplyr)
require(tidyverse)

# load dr for pa objects
source("code/drpa/dose_response.R")

# Load all health related functions
source("code/PA/code/functions.R")



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

pif_list <- list()
index <- 1

for ( j in 1:nrow(disease_short_names)){
  ## checking whether to calculate this health outcome for PA
  pa_dn <- disease_short_names$sname[j] %>% as.character()
  local_cause <- disease_short_names$disease[j] %>% as.character()
  
  for (scen in names(mmets %>% dplyr::select(starts_with("MS")))){
    
    qb <- paste('RR_pa_baseline', pa_dn, sep = '_')
    
    qscen <- paste('RR_pa', scen, pa_dn, sep = '_')
    
    # browser()
    
    if (nrow(mmets[is.na(mmets[[qb]]),]) > 0)
      mmets[is.na(mmets[[qb]]),][[qb]] <- 0
    if (nrow(mmets[is.na(mmets[[qscen]]),]) > 0) 
      mmets[is.na(mmets[[qscen]]),][[qscen]]
    
    # Calculate PIFs for baseline and selected scenario
    pif <- data.frame(PAF(pop = mmets, attr = c('Sex_B01ID', 'age_group'), cn = c(qb , qscen)))
    pif <- arrange(pif, age.band, gender)
    
    pif[, 3] <- NULL
    
    pif$gender <- as.integer(pif$gender)
    pif$age.band <- (as.character(pif$age.band))
    pif[[qscen]] <- as.numeric(as.character(pif[[qscen]]))
    
    pif_interp <- data.frame(age = rep(seq(0, 100), 2), sex = c(rep("male", 101), rep("female", 101)))
    
    pif_interp$sex <- as.character(pif_interp$sex)
    
    for (i in 1:nrow(pif)){
      
      pif$age_group[i] <- round(mean(as.numeric(str_extract_all(pif$age.band[i],"\\(?[0-9,.]+\\)?")[[1]])))
      
    }
    
    pif_interp$v <- -1
    
    for (i in 1:(nrow(pif) / 2)){
      for (j in unique(pif$gender)){
        
        local_pif <- filter(pif, gender == j)
        
        sex <- ifelse (j == 1, "male", "female")
        val <- local_pif[i, 3]
        
        if (i == 1){
          pif_interp[pif_interp$sex == sex &
                       pif_interp$age <= local_pif$age_group[i],]$v <- val
        }
        
        if (i > 1 && i != (nrow(local_pif))){
          pif_interp[pif_interp$sex == sex &
                       (pif_interp$age <= local_pif$age_group[i] &
                          pif_interp$age > local_pif$age_group[i - 1]),]$v <- val
          
        }
        
        if (i == nrow(local_pif)){
          pif_interp[pif_interp$sex == sex &
                       pif_interp$age > local_pif$age_group[i],]$v <- val
        }
        
      }
      
    }
    
    
    # browser()
    pif_list[[index]] <- pif_interp
    index <- index + 1
    
    
    
  }
}

# mmets[is.na(mmets$RR_pa_baseline_ac),]$RR_pa_baseline_ac <- 0
# mmets[is.na(mmets$RR_pa_MS0.05_ebik0_eq0_ac),]$RR_pa_MS0.05_ebik0_eq0_ac <- 0



