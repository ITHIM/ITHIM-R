#' Get relative risk for diseases given mMETs
#' 
#' Computes the relative risks (RR) for individuals in the synthetic population for each disease given their mMETs (PA exposure)
#' 
#' This function performs the following steps:
#' 
#' \itemize{
#' \item loop through all diseases that are related to physical activity levels:
#'    \itemize{
#'    \item set the quantile of the value of the dose response curves to be extracted. If running in 
#'      constant mode, the quantile is usually set to 0.5, i.e. to the median of the dose response curves. 
#'      If running in sample mode, then the quantile can be set to be sampled from a distribution in the 
#'      input parameters.
#'    \item create one vector containing all the mMET values for all scenarios
#'    \item assign the relative risk for the given disease and quantile to the 
#'      given mMET values for all people in the synthetic population for all 
#'      scenarios by calling the drpa::dose_response function
#'    \item extract the RR for each scenario from the vector containing all RR for all scenarios
#'    \item if confidence intervals are required, also extract the RR upper and lower
#'      confidence values for each scenario
#'    }
#' }
#' 
#' @param mmets_pp individual mMETs
#' 
#' @return data frame of relative risks per person per disease
#' 
#' @export


gen_pa_rr <- function(mmets_pp, conf_int = F){
  
  
  dose_columns <- match(paste0(SCEN_SHORT_NAME, '_mmet'),colnames(mmets_pp))
  
  # create one long vector containing all mMET values for the synthetic population for all scenarios
  doses_vector <- unlist(data.frame(mmets_pp[,dose_columns]))
  
  ### iterate over all all disease outcomes that are related to physical activity levels
  for (j in c(1:nrow(DISEASE_INVENTORY))[DISEASE_INVENTORY$physical_activity == 1]) {
    
    # get name of PA DR curve
    pa_dn <- as.character(DISEASE_INVENTORY$pa_acronym[j])
    # define the potential outcomes as either fatal or fatal-and-non-fatal
    outcome <- ifelse(DISEASE_INVENTORY$outcome[j] == "deaths", "fatal",
                      "fatal-and-non-fatal")
    
    # get name of disease
    pa_n <- as.character(DISEASE_INVENTORY$acronym[j])
   
    
    
    # Set quantile to the default value
    quant <- 0.5
    # Check if quantile for the the specific cause has been declared. 
    # If yes, then use the declared value instead
    if (exists(paste0('PA_DOSE_RESPONSE_QUANTILE_',pa_dn)))
      quant <- get(paste0('PA_DOSE_RESPONSE_QUANTILE_',pa_dn))
    
    #Apply PA DR function to all doses as one long vector
    #Use a hard censor method of WHO-QRL which flattens the relationship after 35 MMETs per week
    return_vector <- drpa::dose_response(cause = pa_dn, outcome_type = outcome,
                                         dose = doses_vector, quantile = quant, confidence_intervals = conf_int, 
                                         censor_method = 'WHO-QRL')
    
    ##take segments of returned vector corresponding to each scenario
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
