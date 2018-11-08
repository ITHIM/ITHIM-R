PA_dose_response <- function (cause, outcome_type, dose, confidence_intervals = F){
  
  if (sum(is.na(dose))>0 || class(dose)!= "numeric"){
    stop ('Please provide dose in numeric')
  }
  if (!cause %in% c('all-cause-mortality', 'breast-cancer', 'cardiovascular-disease',
                    'colon-cancer', 'coronary-heart-disease', 'diabetes', 'endometrial-cancer',
                    'heart-failure', 'lung-cancer', 'stroke', 'total-cancer')){
    stop('Unsupported cause/disease. Please select from \n
         all-cause-mortality \n
         breast-cancer\n
         cardiovascular-disease \n
         colon-cancer \n
         coronary-heart-disease \n
         endometrial-cancer \n
         heart-failure \n
         lung-cancer \n
         stroke \n
         total-cancer')
  }
  if (!outcome_type %in% c('mortality', 'incidence')){
    stop('Unsupported outcome_type. Please select from \n
         mortality \n
         incidence')
  }
  if (cause == 'all-cause-mortality' && outcome_type == 'incidence'){
    stop('Incidence does not exist for all-cause-mortality')
  }
  fname <- paste(cause, outcome_type, sep = "-")
  if (cause == 'all-cause-mortality')
    fname <- cause
  lookup_table <- get(paste0(fname))
  lookup_df <- as.data.frame(lookup_table)
  #pert_75 <- stringr::str_sub(basename(list_of_files[[1]]), end = -5)
  ##RJ previously:
  ## cond <- ifelse(use_75_pert, abs(lookup_table$dose - dose), which.min(abs(lookup_table$dose - dose)))
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
    ##RJ question for AA: this function has standard deviation = 1. Is that right?
    rr <- truncnorm::qtruncnorm(get(paste0('PA_DOSE_RESPONSE_QUANTILE_',cause)), rr, a=lb, b=ub)
  }
  if (confidence_intervals) {
    return(data.frame (rr = rr, lb = lb, ub = ub))
  }else{
    return(data.frame(rr = rr))
  }
  }
