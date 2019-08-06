#' Calculate RR given PA
#' 
#' Calculate RR for a disease given PA
#' 
#' @param cause name of disease
#' @param dose vector of doses of PA from individuals
#' @param confidence_intervals logic: whether or not to return confidence intervals
#' 
#' @return data frame of relative risks
#' 
#' @export
PA_dose_response <- function (cause, dose, confidence_intervals = F){
  
  if (sum(is.na(dose))>0 || class(dose)!= "numeric"){
    stop ('Please provide dose in numeric')
  }
  if (!cause %in% c('all_cause', 'breast_cancer', 'cardiovascular_disease',
                    'colon_cancer', 'coronary_heart_disease', 'diabetes', 'endometrial_cancer',
                    'heart_failure', 'lung_cancer', 'stroke', 'total_cancer')){
    stop('Unsupported cause/disease. Please select from \n
         all_cause \n
         breast_cancer\n
         cardiovascular_disease \n
         colon_cancer \n
         coronary_heart_disease \n
         endometrial_cancer \n
         heart_failure \n
         lung_cancer \n
         stroke \n
         total_cancer')
  }
  # decide whether to use "all" or "mortality"
  outcome_type <- ifelse(cause%in%c('lung_cancer','breast_cancer','endometrial_cancer','colon_cancer'), 'all' , 'mortality')
  
  # apply disease-specific thresholds
  if(cause %in% c('total_cancer','coronary_heart_disease','breast_cancer','endometrial_cancer','colon_cancer')) dose[dose>35] <- 35
  else if(cause == 'lung_cancer') dose[dose>10] <- 10
  else if(cause == 'stroke') dose[dose>32] <- 32
  else if(cause == 'all_cause') dose[dose>16.08] <- 16.08
  
  ## this function assumes the existence of a file with a name such as 'stroke_mortality.csv'
  ## and column names 'dose', 'RR', 'lb' and 'ub'.
  fname <- paste(cause, outcome_type, sep = "_")
  lookup_table <- get(fname)
  lookup_df <- setDT(lookup_table)
  rr <- approx(x=lookup_df$dose,y=lookup_df$RR,xout=dose,yleft=1,yright=min(lookup_df$RR))$y
  if (confidence_intervals || PA_DOSE_RESPONSE_QUANTILE==T) {
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
  ## we assume that the columns describe a normal distribution with standard deviation defined by the upper and lower bounds.
  if (PA_DOSE_RESPONSE_QUANTILE==T){
    rr <- qnorm(get(paste0('PA_DOSE_RESPONSE_QUANTILE_',cause)), mean=rr, sd=(ub-lb)/1.96)
    rr[rr<0] <- 0
  }
  if (confidence_intervals) {
    return(data.frame (rr = rr, lb = lb, ub = ub))
  }else{
    return(data.frame(rr = rr))
  }
}
