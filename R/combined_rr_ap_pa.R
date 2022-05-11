#' Combine relative risks from AP and PA
#' 
#' Combine relative risks from AP and PA through multiplication for crossover diseases
#' 
#' @param ind_pa data frame of individual RRs for diseases affected by PA
#' @param ind_ap data frame of individual RRs for diseases affected by AP
#' @param conf_int logic: whether to include confidence interval from dose response relationships or not
#' 
#' @return combined RR for diseases after accounted for AP and PA exposures
#' 
#' @export
combined_rr_ap_pa <- function(ind_pa, ind_ap, conf_int = FALSE){
  
  # Replace NaNs with 1
  ind_ap[is.na(ind_ap)] <- 1
  
  # Replace Na with 1
  ind_pa[is.na(ind_pa)] <- 1
  
  # join pa and ap datasets (all data.frames)
  ind_ap_pa <- dplyr::left_join(ind_pa, ind_ap, by = c('participant_id','age','sex','age_cat'))
  
  ### iterating over all all disease outcomes
  for ( j in c(1:nrow(DISEASE_INVENTORY))[DISEASE_INVENTORY$physical_activity == 1 & DISEASE_INVENTORY$air_pollution == 1]){
    ac <- as.character(DISEASE_INVENTORY$acronym[j])
    for (scen in SCEN_SHORT_NAME){
      ind_ap_pa[[paste('RR_pa_ap', scen, ac, sep = '_')]] <- ind_ap_pa[[paste('RR_pa', scen, ac, sep = '_')]] * ind_ap_pa[[paste('RR_ap', scen, ac, sep = '_')]]
      
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
