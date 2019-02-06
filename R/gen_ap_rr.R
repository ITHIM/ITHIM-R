#' @export
gen_ap_rr <- function(pm_conc_pp){
  
  ### combining PM2.5 concentration data (scenario_pm_calculations.R) and PA data (total_mmet.R) at the individual level (n=732)
  
  pm_rr_pp <- pm_conc_pp ## PM2.5 relative risk per person
  
  ## assigning air pollution age band to the individual_level data
  min_ages <- c(seq(24,94,by=5),200)
  pm_rr_pp$ap_age <-
    sapply(pm_rr_pp$age, function(x)
      if(x > min_ages[1])
        min_ages[which(min_ages > x)[1] - 1] + 1
      else
        0)
  
  pm_indices <- sapply(SCEN_SHORT_NAME,function(x)which(colnames(pm_rr_pp)==paste0("pm_conc_",x)))
  ### iterating over all all disease outcomes
  for ( j in c(1:nrow(DISEASE_INVENTORY))[DISEASE_INVENTORY$air_pollution == 1]){
    # initialise lists
    for (x in 1:length(SCEN_SHORT_NAME))
      pm_rr_pp[[paste0("RR_ap_", SCEN_SHORT_NAME[x])]] <- 0
    cause <- as.character(DISEASE_INVENTORY$ap_acronym[j])
    dr_ap_disease <- subset(DR_AP, cause_code == cause)
    # apply by age groups
    ages <- unique(dr_ap_disease$age_code)
    for(age in ages){
      dr_ap_sub <- subset(dr_ap_disease,age_code == age )
      if(age==99){
        i <-1:nrow(pm_rr_pp)
      }else{
        i <- which(pm_rr_pp$ap_age==age)
      }
      # get parameters
      alpha <- DR_AP_LIST[[cause]][[as.character(age)]]$alpha
      beta <- DR_AP_LIST[[cause]][[as.character(age)]]$beta
      gamma <- DR_AP_LIST[[cause]][[as.character(age)]]$gamma
      tmrel <- DR_AP_LIST[[cause]][[as.character(age)]]$tmrel
      # calculate AP and apply to all in age group
      for(x in 1: length(SCEN_SHORT_NAME)) 
        pm_rr_pp[[paste0("RR_ap_", SCEN_SHORT_NAME[x])]][i] <-
        as.numeric(1 + alpha * (1 - exp(-beta * (pm_rr_pp[[pm_indices[x]]][i] - tmrel) ^ gamma )))
    }
    ## change the names of the columns as per the disease
    for (n in 1: length(SCEN_SHORT_NAME)){
      col <- which(names(pm_rr_pp)== paste0("RR_ap_",SCEN_SHORT_NAME[n]))
      names(pm_rr_pp)[col]<- paste0("RR_ap_",SCEN_SHORT_NAME[n],"_",DISEASE_INVENTORY$acronym[j])
    }
  }
  pm_rr_pp
}