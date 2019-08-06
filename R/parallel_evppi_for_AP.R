#' Compute AP EVPPI 
#' 
#' For use to compute AP EVPPI in parallel
#' 
#' @param disease disease name
#' @param parameter_samples data frame of parameter samples
#' @param outcome data frame of outcomes
#' @param NSCEN number of scenarios
#' 
#' @return vector of EVPPI values (one per scenario)
#' 
#' @export
parallel_evppi_for_AP <- function(disease,parameter_samples,outcome,NSCEN){
  AP_DOSE_RESPONSE_QUANTILE <- c()
  x1 <- parameter_samples[,which(colnames(parameter_samples)==paste0('AP_DOSE_RESPONSE_QUANTILE_ALPHA_',disease))];
  x2 <- parameter_samples[,which(colnames(parameter_samples)==paste0('AP_DOSE_RESPONSE_QUANTILE_BETA_',disease))];
  x3 <- parameter_samples[,which(colnames(parameter_samples)==paste0('AP_DOSE_RESPONSE_QUANTILE_GAMMA_',disease))];
  x4 <- parameter_samples[,which(colnames(parameter_samples)==paste0('AP_DOSE_RESPONSE_QUANTILE_TMREL_',disease))];
  for(j in 1:(NSCEN)){
    y <- rowSums(outcome[,seq(j,ncol(outcome),by=NSCEN)])
    vary <- var(y)
    model <- gam(y ~ te(x1,x2,x3,x4))
    AP_DOSE_RESPONSE_QUANTILE[j] <- (vary - mean((y - model$fitted) ^ 2)) / vary * 100 
  }
  AP_DOSE_RESPONSE_QUANTILE
}
