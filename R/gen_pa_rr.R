#' Get RR for diseases given mMETs
#' 
#' Computes the RR for individuals for each disease given mMETs (PA exposure)
#' 
#' @param mmets_pp individual mMETs
#' 
#' @return data frame of relative risks per person per disease
#' 
#' @export
gen_pa_rr <- function(mmets_pp){
  ### iterating over all all disease outcomes
  dose_columns <- match(paste0(SCEN_SHORT_NAME, '_mmet'),colnames(mmets_pp))
  doses_vector <- unlist(data.frame(mmets_pp[,dose_columns]))
  for ( j in c(1:nrow(DISEASE_INVENTORY))[DISEASE_INVENTORY$physical_activity == 1]){
    # get name of PA DR curve
    pa_dn <- as.character(DISEASE_INVENTORY$pa_acronym[j])
    # get name of disease
    pa_n <- as.character(DISEASE_INVENTORY$acronym[j])
    ##RJ apply PA DR function to all doses as one long vector
    return_vector <- PA_dose_response(cause = pa_dn,dose = doses_vector)
    ##RJ take segments of returned vector corresponding to scenario
    for (i in 1:length(SCEN_SHORT_NAME)){
      scen <- SCEN_SHORT_NAME[i]
      mmets_pp[[paste('RR_pa', scen, pa_n, sep = '_')]] <- return_vector$rr[(1+(i-1)*nrow(mmets_pp)):(i*nrow(mmets_pp))]
    }
  }
  mmets_pp 
}
