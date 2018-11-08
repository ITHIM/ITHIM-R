parallel_evppi_for_AP <- function(disease,parameter_samples,outcome){
  AP_DOSE_RESPONSE_QUANTILE <- c()
  x1 <- parameter_samples[,which(colnames(parameter_samples)==paste0('AP_DOSE_RESPONSE_QUANTILE_ALPHA_',disease))];
  x2 <- parameter_samples[,which(colnames(parameter_samples)==paste0('AP_DOSE_RESPONSE_QUANTILE_BETA_',disease))];
  x3 <- parameter_samples[,which(colnames(parameter_samples)==paste0('AP_DOSE_RESPONSE_QUANTILE_GAMMA_',disease))];
  x4 <- parameter_samples[,which(colnames(parameter_samples)==paste0('AP_DOSE_RESPONSE_QUANTILE_TMREL_',disease))];
  for(j in 1:(NSCEN)){
    y <- outcome[, j+5] ## +5 means we choose ihd outcome for each scenario
    vary <- var(y)
    model <- gam(y ~ te(x1,x2,x3,x4))
    AP_DOSE_RESPONSE_QUANTILE[j] <- (vary - mean((y - model$fitted) ^ 2)) / vary * 100 
  }
  AP_DOSE_RESPONSE_QUANTILE
}
