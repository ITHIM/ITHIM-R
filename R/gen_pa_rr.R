#' @export
gen_pa_rr <- function(mmets_pp){
  ### iterating over all all disease outcomes
  dose_columns <- match(paste0(SCEN_SHORT_NAME, '_mmet'),colnames(mmets_pp))
  doses_clean <- mmets_pp[,dose_columns]
  for ( j in c(1:nrow(DISEASE_INVENTORY))[DISEASE_INVENTORY$physical_activity == 1]){
    # get name of PA DR curve
    pa_dn <- as.character(DISEASE_INVENTORY$pa_acronym[j])
    # get name of disease
    pa_n <- as.character(DISEASE_INVENTORY$acronym[j])
    # decide whether to use "all" or "mortality"
    outcome_type <- ifelse(pa_dn%in%c('lung_cancer','breast_cancer','endometrial_cancer','colon_cancer'), 'all' , 'mortality')
    doses <- doses_clean
    # apply disease-specific thresholds
    if(pa_dn %in% c('total_cancer','coronary_heart_disease','breast_cancer','endometrial_cancer','colon_cancer')) doses[doses>35] <- 35
    else if(pa_dn == 'lung_cancer') doses[doses>10] <- 10
    else if(pa_dn == 'stroke') doses[doses>32] <- 32
    else if(pa_dn == 'all_cause') doses[doses>16.08] <- 16.08
    ##RJ apply PA DR function to all doses as one long vector
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
