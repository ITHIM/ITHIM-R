gen_pa_rr <- function(ind){
  ### iterating over all all disease outcomes
  dose_columns <- match(paste0(SCEN_SHORT_NAME, '_mmet'),colnames(ind))
  doses_clean <- ind[,dose_columns]
  for ( j in 1:nrow(DISEASE_OUTCOMES)){
    ## checking whether to calculate this health outcome for PA
    if (DISEASE_OUTCOMES$physical_activity[j] == 1){
      pa_dn <- as.character(DISEASE_OUTCOMES$pa_acronym[j])
      pa_n <- as.character(DISEASE_OUTCOMES$acronym[j])
      outcome_type <- ifelse(pa_dn%in%c('lung-cancer','stroke'), 'incidence' , 'mortality')
      # CHD: 35 mmeth per week use mortality
      # Lung cancer: 10 mmeth per week use incidence
      # stroke 75 pert: 13.37
      # Diabetes no limits
      # total cancer: 35 mmeths per week use mortality
      doses <- doses_clean
      if(pa_dn %in% c('total-cancer','coronary-heart-disease')) doses[doses>35] <- 35
      else if(pa_dn == 'lung-cancer') doses[doses>10] <- 10
      else if(pa_dn == 'stroke') doses[doses>13.37] <- 13.37
      else if(pa_dn == 'all-cause-mortality') doses[doses>16.08] <- 16.08
      ##RJ apply function to all doses as one long vector
      return_vector <- PA_dose_response(cause = pa_dn, outcome_type = outcome_type, 
                                        dose = unlist(data.frame(doses)))
      ##RJ take segments of returned vector corresponding to scenario
      for (i in 1:length(SCEN_SHORT_NAME)){
        scen <- SCEN_SHORT_NAME[i]
        ind[[paste('RR_pa', scen, pa_n, sep = '_')]] <- return_vector$rr[(1+(i-1)*nrow(doses)):(i*nrow(doses))]
      }
    }
  }
  ind 
}
