#' Get RR for diseases given mMETs
#' 
#' Computes the RR for individuals for each disease given mMETs (PA exposure)
#' 
#' @param mmets_pp individual mMETs
#' 
#' @return data frame of relative risks per person per disease
#' 
#' @export
gen_pa_rr <- function(mmets_pp, conf_int = F){
  ### iterating over all all disease outcomes
  dose_columns <- match(paste0(SCEN_SHORT_NAME, '_mmet'),colnames(mmets_pp))
  doses_vector <- unlist(data.frame(mmets_pp[,dose_columns]))
  for (j in c(1:nrow(DISEASE_INVENTORY))[DISEASE_INVENTORY$physical_activity == 1]) {
    # get name of PA DR curve
    pa_dn <- as.character(DISEASE_INVENTORY$pa_acronym[j])
    outcome <- ifelse(DISEASE_INVENTORY$outcome[j] == "deaths", "fatal",
                      "fatal-and-non-fatal")
    # get name of disease
    pa_n <- as.character(DISEASE_INVENTORY$acronym[j])
    ##RJ apply PA DR function to all doses as one long vector
    #return_vector <- PA_dose_response(cause = pa_dn,dose = doses_vector)
    
    # Set quantile to the default value
    quant <- 0.5
    # Check if quantile for the the specific cause has been declared. 
    # If yes, then use it instead
    if (exists(paste0('PA_DOSE_RESPONSE_QUANTILE_',pa_dn)))
      quant <- get(paste0('PA_DOSE_RESPONSE_QUANTILE_',pa_dn))
    
    # Add quantile as the parameter
    return_vector <- drpa::dose_response(cause = pa_dn, outcome_type = outcome,
                                         dose = doses_vector, quantile = quant, confidence_intervals = conf_int)
    ##RJ take segments of returned vector corresponding to scenario
    for (i in 1:length(SCEN_SHORT_NAME)) {
      scen <- SCEN_SHORT_NAME[i]
      mmets_pp[[paste('RR_pa', scen, pa_n, sep = '_')]] <- return_vector$rr[(1 + (i - 1)*nrow(mmets_pp)):(i*nrow(mmets_pp))]
      
      if (conf_int){
        mmets_pp[[paste('RR_pa', scen, pa_n, 'lb', sep = '_')]] <- return_vector$lb[(1 + (i - 1)*nrow(mmets_pp)):(i*nrow(mmets_pp))]
        mmets_pp[[paste('RR_pa', scen, pa_n, 'ub', sep = '_')]] <- return_vector$ub[(1 + (i - 1)*nrow(mmets_pp)):(i*nrow(mmets_pp))]
      }
    }
    
  }
  mmets_pp 
}
