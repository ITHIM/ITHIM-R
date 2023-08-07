#' Combine relative risks from AP and PA
#' 
#' Combine relative risks (RR) from AP and PA through multiplication for crossover diseases
#' 
#' This function performs the following steps:
#' - join the ap and pa relative risk datasets 
#' - loop through all disease outcomes that are affected by both PA and AP
#'   - for each scenario multiply the relative risks for PA and AP and store in a new column
#'   - if confidence intervals are required, multiply the upper and lower RR for AP
#'     and PA respectively wherever possible, otherwise use the given RR values instead
#' 
#' @param ind_pa data frame of individual RRs for diseases affected by PA
#' @param ind_ap data frame of individual RRs for diseases affected by AP
#' @param conf_int logic: whether to include confidence interval from dose response relationships or not
#' 
#' @return dataframe giving the RR risk for AP, PA and combined AP and PA exposure levels for every person in the synthetic population and for each scenario
#' 
#' @export
combined_rr_ap_pa <- function(ind_pa, ind_ap, conf_int = FALSE){
  
  # Replace NaNs with 1
  ind_ap[is.na(ind_ap)] <- 1
  
  # Replace Na with 1
  ind_pa[is.na(ind_pa)] <- 1
  
  # join pa and ap datasets (all data.frames)
  ind_ap_pa <- dplyr::left_join(ind_pa, ind_ap, by = c('participant_id','age','sex','age_cat'))
  
  ### iterating over all all disease outcomes that are affected by both PA and AP
  for ( j in c(1:nrow(DISEASE_INVENTORY))[DISEASE_INVENTORY$physical_activity == 1 & DISEASE_INVENTORY$air_pollution == 1]){
    
    ac <- as.character(DISEASE_INVENTORY$acronym[j])
   
     for (scen in SCEN_SHORT_NAME){ # loop through scenarios
      ind_ap_pa[[paste('RR_pa_ap', scen, ac, sep = '_')]] <- ind_ap_pa[[paste('RR_pa', scen, ac, sep = '_')]] * ind_ap_pa[[paste('RR_ap', scen, ac, sep = '_')]]
      
      # for confidence intervals, multiply the upper and lower RR for AP and PA respectively wherever possible,
      # otherwise use the given RR values instead
      if (conf_int){
        column_pa_lb <- ifelse(paste('RR_pa', scen, ac, 'lb', sep = '_') %in% colnames(ind_ap_pa), paste('RR_pa', scen, ac, 'lb', sep = '_'), paste('RR_pa', scen, ac, sep = '_'))
        column_pa_ub <- ifelse(paste('RR_pa', scen, ac, 'ub', sep = '_') %in% colnames(ind_ap_pa), paste('RR_pa', scen, ac, 'ub', sep = '_'), paste('RR_pa', scen, ac, sep = '_'))
        
        column_ap_lb <- ifelse(paste('RR_ap', scen, ac, 'lb', sep = '_') %in% colnames(ind_ap_pa), paste('RR_ap', scen, ac, 'lb', sep = '_'), paste('RR_ap', scen, ac, sep = '_'))
        column_ap_ub <- ifelse(paste('RR_ap', scen, ac, 'ub', sep = '_') %in% colnames(ind_ap_pa), paste('RR_ap', scen, ac, 'ub', sep = '_'), paste('RR_ap', scen, ac, sep = '_'))
        
        
        ind_ap_pa[[paste('RR_pa_ap', scen, ac, 'lb', sep = '_')]] <- ind_ap_pa[[column_pa_lb]] * ind_ap_pa[[column_ap_lb]]
        
        ind_ap_pa[[paste('RR_pa_ap', scen, ac, 'ub', sep = '_')]] <- ind_ap_pa[[column_pa_ub]] * ind_ap_pa[[column_ap_ub]]
        
      }
    }
  }
  
  ind_ap_pa
}
