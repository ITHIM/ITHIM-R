#' Function to calculate the EVPPI values for given input parameters and outcome values
#' 
#' This function calculates the expected value of partially perfect information, i.e. the percentage by which we could
#' reduce the variance of the final outcome if we knew this one parameter (or several interdependent parameters) perfectly.
#'
#' The function performs the following steps:
#' 
#'\itemize{
#'\item extract the parameter / parameters of interest for which the EVPPI value is to be calculated
#'
#'\item loop through the various outcomes :
#'  \itemize{
#'    \item calculate the EVPPI value for each outcome using the evppivar() function from https://github.com/chjackson/voi
#'    \item calculate the percentage of variance in the outcome we could reduce were we to know the 
#'          input parameter / parameters perfectly.
#'    } 
#' }  
#' 
#' 
#' @param p input parameter index
#' @param global_para list of global input parameters that are the same across all cities
#' @param city_para list of city specific input parameters
#' @param city_outcome list of outcomes for a specific city
#' @param nsamples number of samples
#' @param individual_para whether each parameter is to be considered individually or not
#
#' @return list of of EVPPI values for specific city
#' 
#' @export


compute_evppi <- function(p, global_para,city_para,city_outcomes, nsamples, individual_para = TRUE){
  
  ncol_gen <- ncol(global_para) 

  #if (is.null(ncol_gen)) ncol_gen <- length(global_para) # in case of DR functions were several parameters are considered at the same time
  
  voi <- rep(0,length(city_outcomes)) # create empty output list
  
  if (individual_para == TRUE){
    if(p <= ncol_gen){# first loop through general parameters
      sourcesj <- global_para[[p]]  # look at each parameter at a time
    } else {
      p2 <- p - ncol_gen
      sourcesj <- city_para[[p2]] # then loop through city specific parameters
    }
  } else { # this assumes that either city_para or global_para is empty
    if (nrow(global_para)==0){
      sourcesj <- city_para
    } else {
      sourcesj <- global_para
    }
  }
  
  for(o in 1:length(city_outcomes)){ # loop through all outcomes
    
    y <- as.numeric(city_outcomes[[o]])
    # extract one outcome     
    vary <- var(y) #compute outcome variance
    
    # model outcome as a function of input(s)
    if (nsamples >= 8){ # use Chris Jackson's VoI R package if sample size large enough
      if(is.vector(sourcesj)){ # if only one parameter is considered at a time
        evppi_jj <- evppivar(y,sourcesj) # uses Chris Jackson's VoI package
      }
      else { # if several input parameters are considered together, e.g. for CO2 and PM emission inventories
        evppi_jj <- evppivar(y,sourcesj, par= c(colnames(sourcesj)), method="earth")
      }
      
      # compute evppi as percentage, i.e. percentage of variance we can reduce if we knew a certain input parameter
      voi[o] <- evppi_jj$evppi / vary * 100
    } else { # calculate EVPPI directly if sample size too small to use C Jackson's VoI package
      model <- earth(y ~ sourcesj, degree=4)
      voi[o] <- (vary - mean((y - model$fitted) ^ 2)) / vary * 100 # compute evppi as percentage
    }
    
  }  
  voi   # return evppi list
}




