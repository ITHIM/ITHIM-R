#' @export
gen_pa_rr <- function(mmets_pp){
  ### iterating over all all disease outcomes
  dose_columns <- match(paste0(SCEN_SHORT_NAME, '_mmet'),colnames(mmets_pp))
  doses_clean <- mmets_pp[,dose_columns]
  for ( j in c(1:nrow(DISEASE_INVENTORY))[DISEASE_INVENTORY$physical_activity == 1]){
    pa_dn <- as.character(DISEASE_INVENTORY$pa_acronym[j])
    pa_n <- as.character(DISEASE_INVENTORY$acronym[j])
    outcome_type <- ifelse(pa_dn%in%c('lung_cancer','stroke'), 'incidence' , 'mortality')
    # CHD: 35 mmeth per week use mortality
    # Lung cancer: 10 mmeth per week use incidence
    # stroke 75 pert: 13.37
    # Diabetes no limits
    # total cancer: 35 mmeths per week use mortality
    doses <- doses_clean
    if(pa_dn %in% c('total_cancer','coronary_heart_disease')) doses[doses>35] <- 35
    else if(pa_dn == 'lung_cancer') doses[doses>10] <- 10
    else if(pa_dn == 'stroke') doses[doses>13.37] <- 13.37
    else if(pa_dn == 'all_cause') doses[doses>16.08] <- 16.08
    ##RJ apply function to all doses as one long vector
    return_vector <- PA_dose_response(cause = pa_dn, outcome_type = outcome_type, 
                                      dose = unlist(data.frame(doses)))
    ##RJ take segments of returned vector corresponding to scenario
    for (i in 1:length(SCEN_SHORT_NAME)){
      scen <- SCEN_SHORT_NAME[i]
      mmets_pp[[paste('RR_pa', scen, pa_n, sep = '_')]] <- return_vector$rr[(1+(i-1)*nrow(doses)):(i*nrow(doses))]
    }
  }
  mmets_pp 
}
