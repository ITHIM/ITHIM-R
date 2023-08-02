#' Calculate relative risk given PM exposure level
#' 
#' Calculate the relative risk (RR) for each disease related to air pollution and each
#' scenario based on the individual PM exposure levels
#' 
#' This function performs the following steps:
#' - various checks to ensure the correct PM exposure levels (doses) and disease are read in
#' - get the lookup table for the required dose response functions which contains for each dose
#'   the median RR and the upper and lower confidence interval RR values
#' - for the PM exposure doses in the synthetic population find the need RR by extrapolating the 
#'   dose responses given in the lookup table
#' - if a confidence interval is required, repeat this interpolation using the upper and lower
#'   confidence RR values
#' - if the required quantile is not 0.5, i.e. the median, then find the required RR value by
#'   defining a normal function with mean the median RR value for that dose and standard deviations
#'   defined by the upper and lower confidence RR values. Use the correct quantile from this
#'   normal function as your RR value.  
#'   
#' 
#' @param cause name of disease
#' @param dose vector of PM exposure levels from individuals for a given age range and scenario
#' @param quantile quantile of the dose response functions to be used as output value
#' @param confidence_intervals logic: whether or not to return confidence intervals
#' 
#' @return data frame of relative risks
#' 
#' @export
AP_dose_response <- function(cause, dose, quantile, confidence_intervals = F) {
 
  
   # Check there are NAs in dose or the classes is not numeric
  if (sum(is.na(dose)) > 0 || class(dose) != "numeric") {
    stop('Please provide dose in numeric')
  }
  # check that the correct disease name is used
  if (!cause %in% c('all_cause_ap', 'cvd_ihd', 'neo_lung', 'resp_copd', 
                    'cvd_stroke', 't2_dm', 'lri', 'respiratory', 'cvd',
                    'cvd_ihd_25', 'cvd_ihd_30', 'cvd_ihd_35', 'cvd_ihd_40',
                    'cvd_ihd_45', 'cvd_ihd_50', 'cvd_ihd_55', 'cvd_ihd_60',
                    'cvd_ihd_65', 'cvd_ihd_70', 'cvd_ihd_75', 'cvd_ihd_80',
                    'cvd_ihd_85', 'cvd_ihd_90', 'cvd_ihd_95',
                    'cvd_stroke_25', 'cvd_stroke_30', 'cvd_stroke_35',
                    'cvd_stroke_40', 'cvd_stroke_45', 'cvd_stroke_50',
                    'cvd_stroke_55', 'cvd_stroke_60', 'cvd_stroke_65',
                    'cvd_stroke_70', 'cvd_stroke_75', 'cvd_stroke_80',
                    'cvd_stroke_85', 'cvd_stroke_90', 'cvd_stroke_95'
  )) {stop('Unsupported cause/disease. Please select from \n
         cvd_ihd \n
         neo_lung \n
         resp_copd \n
         cvd_stroke \n
         t2_dm \n
         lri \n
         respiratory \n
         cvd')
  } # End if unsupported causes
  
  # read in dose response functions for disease cause 
  # contains relative risk with upper and lower limits for a variety of doses
  lookup_table <- get(cause) 
  lookup_df <- setDT(lookup_table) 
  
  # interpolate the values in the lookup table to get RR values for all PM exposure doses
  # in the synthetic population
  rr <- approx(x = lookup_df$dose, y = lookup_df$RR, 
               xout = dose, yleft = 1, yright = min(lookup_df$RR))$y
  
  # if a confidence interval is to be returned or if the quantile is not 0.5, 
  # i.e. the median define an upper or lower band
  # interpolate the upper and lower band values in the lookup table to get upper and lower band
  # RR values for all PM exposure doses in the synthetic population
  if (confidence_intervals || quantile != 0.5) {
    lb <-
      approx(
        x = lookup_df$dose,
        y = lookup_df$lb,
        xout = dose,
        yleft = 1,
        yright = min(lookup_df$lb)
      )$y
    ub <-
      approx(
        x = lookup_df$dose,
        y = lookup_df$ub,
        xout = dose,
        yleft = 1,
        yright = min(lookup_df$ub)
      )$y
  }
  
  # if the quantile is not 0.5, i.e. the median, then find the RR value by finding the
  # right quantile of a normal function with mean = the median RR value and standard deviation
  # defined by the upper and lower RR confidence interval values
  if (quantile != 0.5){
    rr <- qnorm(quantile, mean = rr, sd = (ub-lb)/1.96)
    rr[rr<0] <- 0
  }
  
  # if confidence values are required return the RR and the lower and upper bounds
  # otherwise just return the RR
  if (confidence_intervals) {
    return(data.frame (rr = rr, lb = lb, ub = ub))
  }else{
    return(data.frame(rr = rr))
  }

  #return(rr)
}