#' Get RR for diseases given AP exposure
#' 
#' Computes the RR for individuals for each disease given AP exposure
#' 
#' @param pm_conc_pp individual AP exposures
#' 
#' @return data frame of relative risks per person per disease
#' 
#' @export
gen_ap_rr <- function(pm_conc_pp){
  
  pm_rr_pp <- pm_conc_pp 
  
  ## assigning air pollution age band to the individual_level data
  min_ages <- c(seq(24,94,by=5),200)
  pm_rr_pp$age <- as.numeric(pm_rr_pp$age)
  pm_rr_pp$ap_age <- 0
  for(i in 1:length(min_ages)) pm_rr_pp$ap_age[pm_rr_pp$age>min_ages[i]] <- min_ages[i]+1
  
  pm_indices <- sapply(SCEN_SHORT_NAME,function(x)which(colnames(pm_rr_pp)==paste0("pm_conc_",x)))
  ### iterating over all all disease outcomes
  for ( j in c(1:nrow(DISEASE_INVENTORY))[DISEASE_INVENTORY$air_pollution == 1]){
    # initialise lists
    for (x in 1:length(SCEN_SHORT_NAME))
      pm_rr_pp[[paste0("RR_ap_", SCEN_SHORT_NAME[x])]] <- 1
    cause <- as.character(DISEASE_INVENTORY$ap_acronym[j])
    dr_ap_disease <- DR_AP[DR_AP$cause_code == cause,]
    # apply by age groups
    ages <- unique(dr_ap_disease$age_code)
    for(age in ages){
      dr_ap_sub <- dr_ap_disease[dr_ap_disease$age_code == age,]
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
        pm_rr_pp[[paste0("RR_ap_", SCEN_SHORT_NAME[x])]][i] <- ap_dose_response_curve(pm_rr_pp[[pm_indices[x]]][i],alpha,beta,gamma,tmrel)
    }
    ## change the names of the columns as per the disease
    for (n in 1: length(SCEN_SHORT_NAME)){
      col <- which(names(pm_rr_pp)== paste0("RR_ap_",SCEN_SHORT_NAME[n]))
      names(pm_rr_pp)[col]<- paste0("RR_ap_",SCEN_SHORT_NAME[n],"_",DISEASE_INVENTORY$acronym[j])
    }
  }
  pm_rr_pp
}

#' Computes RR as a DR relationship
#' 
#' Computes RR as a DR relationship given four parameters and the PM2.5 exposure
#' 
#' @param pm PM2.5 exposure
#' @param alpha DR parameter
#' @param beta DR parameter
#' @param gamma DR parameter
#' @param tmrel DR parameter
#' 
#' @return RR
#' 
#' @export
ap_dose_response_curve <- function(pm,alpha,beta,gamma,tmrel){
  as.numeric(1 + alpha * (1 - exp(-beta * (pm - tmrel) ^ gamma )))
}
