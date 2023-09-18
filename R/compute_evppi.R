#' Compute evppi, designed to be run in parallel - ONLY USED in SAMPLING MODE
#' 
#' Creates a list of EVPPI values one parameter (set) at a time across some pre-defined outcomes
#' Uses Chris Jackson's VoI Github package https://github.com/chjackson/voi
#' @param p input parameter index
#' @param global_para list of global input parameters that are the same across all cities
#' @param city_para list of city specific input parameters
#' @param city_outcome list of outcomes for a specific city
#
#' @return list of EVPPI vectors for specific city
#' 
#' @export


compute_evppi <- function(p, global_para,city_para,city_outcomes, nsamples){
  
  ncol_gen <- ncol(global_para) 

  if (is.null(ncol_gen)) ncol_gen <- length(global_para) # in case of DR functions were several parameters are considered at the same time
  
  voi <- rep(0,length(city_outcomes)) # create empty output list
  
  if(p <= ncol_gen){# first loop through general parameters
    sourcesj <- global_para[[p]]  # look at each parameter at a time
  } else {
    p2 <- p - ncol_gen
    sourcesj <- city_para[[p2]] # then loop through city specific parameters
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
      else { # if several input parameters are considered together, e.g. dose response alpha, beta, gamma, trml parameters
        evppi_jj <- evppivar(y,sourcesj, par= c(colnames(sourcesj)))
      }
      
      # compute evppi as percentage, i.e. percentage of variance we can reduce if we knew a certain input parameter
      voi[o] <- evppi_jj$evppi / vary * 100
    } 
    else { # calculate EVPPI directly if sample size too small to use C Jackson's VoI package
      model <- earth(y ~ sourcesj, degree=4)
      voi[o] <- (vary - mean((y - model$fitted) ^ 2)) / vary * 100 # compute evppi as percentage
    }
    
  }  
  voi   # return evppi list
}




