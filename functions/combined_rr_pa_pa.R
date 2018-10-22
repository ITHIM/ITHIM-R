combined_rr_pa_pa <- function(ind_pa,ind_ap){
  
  # Replace NaNs with 1
  ind_ap[is.na(ind_ap)] <- 1
  
  # Replace Na with 1
  ind_pa[is.na(ind_pa)] <- 1
  
  # remove common columns from ap
  ind_ap <- dplyr::select(ind_ap, -c(sex, age, age_cat))
  
  # join pa and ap ind datasets
  ind <- left_join(ind_pa, ind_ap, by = "participant_id")
  
  ### iterating over all all disease outcomes
  for ( j in 1:nrow(DISEASE_OUTCOMES)){
    ## checking whether to calculate this health outcome for PA
    if (DISEASE_OUTCOMES$physical_activity[j] == 1 & DISEASE_OUTCOMES$air_pollution[j] == 1){
      for (scen in SCEN_SHORT_NAME){
        ac <- as.character(DISEASE_OUTCOMES$acronym[j])
        ind[[paste('RR_pa_ap', scen, ac, sep = '_')]] <- ind[[paste('RR_pa', scen, ac, sep = '_')]] * ind[[paste('RR_ap', scen, ac, sep = '_')]]
        
      }
    }
  }
  
  ind
}
