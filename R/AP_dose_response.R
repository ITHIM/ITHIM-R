#' Calculate RR given AP
#' 
#' Calculate RR for a disease given AP
#' 
#' @param cause name of disease
#' @param dose vector of doses of AP from individuals
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
  if (!cause %in% c('all_cause_ap', 'cvd_ihd', 'neo_lung', 'resp_copd', 
                    'cvd_stroke', 't2_dm', 'lri', 'cvd',
                    'cvd_ihd_25', 'cvd_ihd_30', 'cvd_ihd_35', 'cvd_ihd_40',
                    'cvd_ihd_45', 'cvd_ihd_50', 'cvd_ihd_55', 'cvd_ihd_60',
                    'cvd_ihd_65', 'cvd_ihd_70', 'cvd_ihd_75', 'cvd_ihd_80',
                    'cvd_ihd_85', 'cvd_ihd_90', 'cvd_ihd_95',
                    'cvd_stroke_25', 'cvd_stroke_30', 'cvd_stroke_35',
                    'cvd_stroke_40', 'cvd_stroke_45', 'cvd_stroke_50',
                    'cvd_stroke_55', 'cvd_stroke_60', 'cvd_stroke_65',
                    'cvd_stroke_70', 'cvd_stroke_75', 'cvd_stroke_80',
                    'cvd_stroke_85', 'cvd_stroke_90', 'cvd_stroke_95'
  )) {
    stop('Unsupported cause/disease. Please select from \n
         cvd_ihd \n
         neo_lung \n
         resp_copd \n
         cvd_stroke \n
         t2_dm \n
         lri')
  } # End if unsupported causes
  lookup_table <- get(cause)
  lookup_df <- setDT(lookup_table)
  rr <- approx(x = lookup_df$dose, y = lookup_df$RR, 
               xout = dose, yleft = 1, yright = min(lookup_df$RR))$y
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
  
  if (quantile != 0.5){
    rr <- qnorm(quantile, mean = rr, sd = (ub-lb)/1.96)
    rr[rr<0] <- 0
  }
  
  if (confidence_intervals) {
    return(data.frame (rr = rr, lb = lb, ub = ub))
  }else{
    return(data.frame(rr = rr))
  }

  #return(rr)
}