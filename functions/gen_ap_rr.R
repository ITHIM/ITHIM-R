gen_ap_rr <- function(rd,ind_pm){
  # Create dummy ind pop
  ind <-  summarise(group_by(rd,participant_id),sex = first(sex),
                    age = first(age),
                    age_cat = first(age_cat))
  
  ### combining PM2.5 concentration data (scenario_pm_calculations.R) and PA data (total_mmet.R) at the individual level (n=732)
  #ind<- read.csv("data/synth_pop_data/accra/processed_data/indiv_mmet/pa_total_mmet_weekly.csv") ### PA 
  ind_pm$participant_id <- as.integer(ind_pm$participant_id)
  
  ind <-  left_join(ind,ind_pm, by = "participant_id")
  ## assigning air pollution age band to the individual_level data
  min_ages <- c(seq(24,94,by=5),200)
  ind$ap_age <-
    sapply(ind$age, function(x)
      if(x > min_ages[1])
        min_ages[which(min_ages > x)[1] - 1] + 1
      else
        0)
  
  ## for every individual average of all parameter draws within the age and disease-outcome
  
  pm_indices <- sapply(SCEN_SHORT_NAME,function(x)which(colnames(ind)==paste0("pm_conc_",x)))
  ### iterating over all all disease outcomes
  for ( j in 1:nrow(DISEASE_OUTCOMES)){
    ## checking whether to calculate this health outcome for air pollution
    if (DISEASE_OUTCOMES$air_pollution[j] == 1){ 
      # initialise lists
      for (x in 1:length(SCEN_SHORT_NAME))
        ind[[paste0("RR_ap_", SCEN_SHORT_NAME[x])]] <- 0
      cause <- as.character(DISEASE_OUTCOMES$ap_acronym[j])
      dr_ap_disease <- subset(DR_AP, cause_code == cause)
      # apply by age groups
      ages <- unique(dr_ap_disease$age_code)
      for(age in ages){
        dr_ap_sub <- subset(dr_ap_disease,age_code == age )
        if(age==99){
          i <-1:nrow(ind)
        }else{
          i <- which(ind$ap_age==age)
        }
        # get parameters
        alpha <- DR_AP_LIST[[cause]][[as.character(age)]]$alpha
        beta <- DR_AP_LIST[[cause]][[as.character(age)]]$beta
        gamma <- DR_AP_LIST[[cause]][[as.character(age)]]$gamma
        tmrel <- DR_AP_LIST[[cause]][[as.character(age)]]$tmrel
        # calculate AP and apply to all in age group
        for(x in 1: length(SCEN_SHORT_NAME)) 
          ind[[paste0("RR_ap_", SCEN_SHORT_NAME[x])]][i] <-
          as.numeric(1 + alpha * (1 - exp(-beta * (ind[[pm_indices[x]]][i] - tmrel) ^ gamma )))
      }
      ## change the names of the columns as per the disease
      for (n in 1: length(SCEN_SHORT_NAME)){
        col <- which(names(ind)== paste0("RR_ap_",SCEN_SHORT_NAME[n]))
        names(ind)[col]<- paste0("RR_ap_",SCEN_SHORT_NAME[n],"_",DISEASE_OUTCOMES$acronym[j])
      }
    }
  }
  ind
}
