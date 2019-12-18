#' Compute evppi, designed to be run in parallel
#' 
#' Creates a list of EVPPI values one parameter (set) at a time across one or more outcomes
#' 
#' @param jj index for input and (potentially) outcome
#' @param sources list of inputs
#' @param outcome list of outcomes
#' @param nscen number of scenarios (which constitute columns of outcome)
#' @param all whether or not to compute EVPPI for source against all outcomes. Default=F, so sources[[jj]] relates to outcome[[jj]]
#' @param multi_city_outcome whether the last outcome corresponds to all cities combine
#' 
#' @return list of EVPPI vectors
#' 
#' @export
compute_evppi <- function(jj,sources,outcome,nscen=1,all=F,multi_city_outcome=T){
  # initialise vector
  voi <- rep(0,length(outcome)*nscen)
  # extract one source
  sourcesj <- sources[[jj]]
  max_degree <- ifelse(is.vector(sourcesj),1,ncol(sourcesj))
  # compute number of cities in outcome
  ncities <- length(outcome) - as.numeric(multi_city_outcome)
  # if computing for all outcomes, include all indices
  indices <- jj
  if(all==T) indices <- 1:ncities
  # if there is a multi-city outcome, include in indices
  if(multi_city_outcome==T) indices <- c(indices,length(outcome))
  # loop over included indices
  for(j in indices){
    # extract one outcome
    case <- outcome[[j]]
    # loop over all scenarios
    for(k in 1:nscen){
      # extract scenario values and sum
      scen_case <- case[,seq(k,ncol(case),by=nscen)]
      y <- rowSums(scen_case)
      # compute outcome variance
      vary <- var(y)
      # model outcome as a function of input(s)
      model <- earth(y ~ sourcesj, degree=min(4,max_degree))
      # compute evppi as percentage
      voi[(j-1)*nscen + k] <- (vary - mean((y - model$fitted) ^ 2)) / vary * 100
    }
  }
  voi
}
