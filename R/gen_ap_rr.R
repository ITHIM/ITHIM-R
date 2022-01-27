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
  min_ages <- c(seq(24, 94, by = 5), 200)
  pm_rr_pp$age <- as.numeric(pm_rr_pp$age)
  pm_rr_pp$ap_age <- 0
  for (i in 1:length(min_ages)) 
    pm_rr_pp$ap_age[pm_rr_pp$age > min_ages[i]] <- min_ages[i] + 1
  
  pm_indices <- sapply(SCEN_SHORT_NAME,
                       function(x)
                         which(colnames(pm_rr_pp) == paste0("pm_conc_",x))
  )
  
  ### iterating over all all disease outcomes
  for (j in c(1:nrow(DISEASE_INVENTORY))[DISEASE_INVENTORY$air_pollution == 1]) {
    # initialise lists
    
    for (x in 1:length(SCEN_SHORT_NAME)) # create columns for future results
      pm_rr_pp[[paste0("RR_ap_", SCEN_SHORT_NAME[x])]] <- 1
    
    cause <- as.character(DISEASE_INVENTORY$ap_acronym[j])
    #dr_ap_disease <- DR_AP[DR_AP$cause_code == cause,]
    
    # apply by age groups depending on the cause
    if (cause %in% c("cvd_ihd", "cvd_stroke")){
      ages <- seq(25,95,5)
      }else {ages <- 99}
    
    for (age in ages) {
      #dr_ap_sub <- dr_ap_disease[dr_ap_disease$age_code == age,]
      # Look for index of people in the age category
      if (age == 99) {
        i <- 1:nrow(pm_rr_pp)
      }else{
        i <- which(pm_rr_pp$ap_age == age)
      }
      cause_age <- ifelse(length(ages) != 1,
                          paste(cause, age, sep = "_"),
                          cause)
      
      # get parameters
      #agechar <- as.character(age)
      #alpha <- DR_AP_LIST[[cause]][[age]]$alpha
      #beta <- DR_AP_LIST[[cause]][[age]]$beta
      #gamma <- DR_AP_LIST[[cause]][[age]]$gamma
      #tmrel <- DR_AP_LIST[[cause]][[age]]$tmrel
      
      # get name of disease
      ap_n <- as.character(DISEASE_INVENTORY$acronym[j])
     
      # Set quantile to the default value
      quant <- 0.5
      
      # Check if quantile for the the specific cause has been declared. 
      # If yes, then use it instead
      if (exists(paste0('AP_DOSE_RESPONSE_QUANTILE_',cause)))
        quant <- get(paste0('AP_DOSE_RESPONSE_QUANTILE_',cause))

      # calculate AP and apply to all in age group
      for (x in 1:length(SCEN_SHORT_NAME)) {
        #pm_rr_pp[[paste0("RR_ap_", SCEN_SHORT_NAME[x])]][i] <- ap_dose_response_curve(pm_rr_pp[[pm_indices[x]]][i],alpha,beta,gamma,tmrel)
        # pm_rr_pp[[paste0("RR_ap_", SCEN_SHORT_NAME[x])]][i] <-
        # AP_dose_response(cause = cause_age, 
        #                  dose = pm_rr_pp[[pm_indices[x]]][i])
      
        return_vector <- AP_dose_response(cause = cause_age, 
                                          dose = pm_rr_pp[[pm_indices[x]]][i], quantile = quant)
        pm_rr_pp[[paste0("RR_ap_", SCEN_SHORT_NAME[x])]][i] <- return_vector$rr
      
      }
      
      
    } # End loop ages
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
