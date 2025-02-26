#' Function to calculate the EVPPI values for given input parameters and outcome values
#' 
#' This function calculates the expected value of partially perfect information, i.e. the percentage by which we could
#' reduce the variance of the final outcome if we knew this one parameter (or several interdependent parameters) perfectly.
#'
#' The function performs the following steps:
#' 
#'\itemize{
#'\item create empty vector to be filled with EVPPI values
#'\item depending on the input parameter considered (depends on value of p), set the paramter(s) for which the evppi value is to be calculated
#'      Note that the calculations are slightly different for the dose response input parameters, denoted by the flag call_dr_rr
#'\item if the input parameter are not related to any dose response function loop through the various outcomes :
#'  \itemize{
#'    \item calculate the EVPPI value for each outcome using the evppivar() function from https://github.com/chjackson/voi
#'    \item calculate the percentage of standard deviation in the outcome we could reduce were we to know the 
#'          input parameter / parameters perfectly.
#'    } 
#'\item if the input parameter is related to the dose response functions:
#'  \itemize{
#'    \item loop through the scenarios and different outcomes as the evppi values are calculated by matching the dose response
#'          functions to the respective outcomes
#'    \item calculate the EVPPI value for each outcome and scenario using the evppivar() function from https://github.com/chjackson/voi
#'    \item calculate the percentage of standard deviation in the outcome we could reduce were we to know the 
#'          input parameter / parameters perfectly.
#'    } 
#' }  
#' 
#' 
#' @param p input parameter index
#' @param global_para list of global input parameters that are the same across all cities
#' @param city_para list of city specific input parameters
#' @param dr_rr dose response RR values for each scenario
#' @param outcome_voi_list list containing the outcomes of interest
#' @param SCEN_SHORT_NAME scenario names including baseline
#' @param city_outcomes list of outcomes for a specific city
#' @param nsamples number of samples
#' @param individual_para whether each parameter is to be considered individually or not
#
#' @return list of of EVPPI standard variation values for specific city
#' 
#' @export


compute_evppi <- function(p, global_para,city_para,dr_rr, outcome_voi_list,
                          SCEN_SHORT_NAME, city_outcomes,  nsamples, individual_para = TRUE){

  
  ncol_gen <- ncol(global_para) 
  ncol_city <- ncol(city_para)

  #if (is.null(ncol_gen)) ncol_gen <- length(global_para) # in case of DR functions were several parameters are considered at the same time
  
  voi <- rep(0,length(city_outcomes)) # create empty output list
  
  # set up flag
  call_dr_rr <- FALSE
  
  if (individual_para == TRUE){
    if(p <= ncol_gen){# first loop through general parameters
      sourcesj <- global_para[[p]]  # look at each parameter at a time
    } else if (p > ncol_gen & p <= (ncol_gen + ncol_city)){
      p2 <- p - ncol_gen 
      sourcesj <- city_para[[p2]] # then loop through city specific parameters
    } else if (p == ncol_gen + ncol_city + 1){
      call_dr_rr <- TRUE
    }
  } else if (individual_para == FALSE & nrow(dr_rr)==0) { # this assumes that either city_para and dr_rr or global_para and dr_rr is empty
    if (nrow(global_para)==0 ){
      sourcesj <- city_para
    } else if (nrow(city_para)==0 ) {
      sourcesj <- global_para
    }
  }
  
  if (call_dr_rr == FALSE){ # for all parameters except the dose response parameters
    for(o in 1:length(city_outcomes)){ # loop through all outcomes (for all parameters except the dose response functions)
      
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
        
        # compute evppi as percentage, i.e. percentage of the standard variation we can reduce if we knew a certain input parameter
        voi[o] <- sqrt(evppi_jj$evppi / vary * 100)
      } else { # calculate EVPPI directly if sample size too small to use C Jackson's VoI package
        model <- earth(y ~ sourcesj, degree=4)
        voi[o] <- sqrt((vary - mean((y - model$fitted) ^ 2)) / vary * 100 ) # compute evppi as percentage of standard deviation
      }
    }  
  } else { # repeat for Dose response function RRs, where RRs respond to specific scenario and outcome
    # need to match DR scenario and outcomes to model outcomes
   
    for (s in SCEN_SHORT_NAME[SCEN_SHORT_NAME != 'base']){ # loop through scenarios
      for (v in outcome_voi_list){ # loop through outcomes
        if (v == 'inj'){ # injury results are not effect by dose response function
          voi_loc <-  which(colnames(city_outcomes)==paste0(s,'_yll_',v))
          voi[voi_loc] <- 0
          
        }else {
          rr_match <- sapply(colnames(dr_rr),function(x)grepl(paste0('DR_RR_',v),x))
          sourcesj <- dr_rr[,rr_match]
          
          # find scenario specific outcomes 
          voi_loc <-  which(colnames(city_outcomes)==paste0(s,'_ylls_',v))
          city_outcomes_loc <- city_outcomes[[voi_loc]]
          
          # get into correct format for voi 
          y <- as.numeric(city_outcomes_loc)
          # extract one outcome     
          vary <- var(y) #compute outcome variance
          
          # model outcome as a function of input()
          if (nsamples >= 8){ # use Chris Jackson's VoI R package if sample size large enough
            if(is.vector(sourcesj)){ # if only one parameter is considered at a time
              evppi_jj <- evppivar(y,sourcesj) # uses Chris Jackson's VoI package
            }
            else { # if several input parameters are considered together, e.g. for CO2 and PM emission inventories
              evppi_jj <- evppivar(y,sourcesj, par= c(colnames(sourcesj)), method="earth")
            }
  
            # compute evppi as percentage, i.e. percentage of the standard variation we can reduce if we knew a certain input parameter
            voi[voi_loc] <- sqrt(evppi_jj$evppi / vary * 100)
          } else { # calculate EVPPI directly if sample size too small to use C Jackson's VoI package
            model <- earth(y ~ sourcesj, degree=4)
            voi[voi_loc] <- sqrt((vary - mean((y - model$fitted) ^ 2)) / vary * 100 ) # compute evppi as percentage of standard deviation
          }
        } # end of 'inj' exception
      } # end of outcome_voi_list
    } # end of scenario list
    
  }
  
  
  voi   # return evppi list
}




