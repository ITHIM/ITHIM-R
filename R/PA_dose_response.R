#' @export
PA_dose_response <- function (cause, outcome_type, dose, confidence_intervals = F){
  
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
  if (!outcome_type %in% c('mortality', 'incidence','all')){
    stop('Unsupported outcome_type. Please select from \n
         mortality \n
         incidence \n
         all')
  }
  if (cause == 'all_cause' && outcome_type == 'incidence'){
    stop('Incidence does not exist for all_cause')
  }
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
  if (PA_DOSE_RESPONSE_QUANTILE==T){
    #rr <- truncnorm::qtruncnorm(get(paste0('PA_DOSE_RESPONSE_QUANTILE_',cause)), rr, sd=rr-lb,a=0, b=1)
    rr <- qnorm(get(paste0('PA_DOSE_RESPONSE_QUANTILE_',cause)), mean=rr, sd=(ub-lb)/1.96)
    rr[rr<0] <- 0
  }
  if (confidence_intervals) {
    return(data.frame (rr = rr, lb = lb, ub = ub))
  }else{
    return(data.frame(rr = rr))
  }
}
