list_of_files <- list.files(path = "data/drpa/extdata/", recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)
for (i in 1:length(list_of_files)){
  assign(stringr::str_sub(basename(list_of_files[[i]]), end = -5), read_csv(list_of_files[[i]]))
}


dose_response <- function (cause, outcome_type, dose, confidence_intervals = F, certainty = T, use_75_pert = T){
  
  if (is.na(dose) || class(dose) != "numeric")
    stop ('Please provide dose in numeric')
  
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
  
  if (cause == 'all-cause-mortality' && outcome_type == 'incidence')
    stop('Incidence does not exist for all-cause-mortality')
  
  fname <- paste(cause, outcome_type, sep = "-")
  
  if (cause == 'all-cause-mortality')
    fname <- cause
  
  #print(fname)
  
  lookup_table <- get(paste0(fname))
  
  pert_75 <- stringr::str_sub(basename(list_of_files[[1]]), end = -5)
  
  
  cond <- ifelse(use_75_pert,
                 abs(lookup_table$dose - dose),
                 which.min(abs(lookup_table$dose - dose)))
  
  if (confidence_intervals){
    
    if (certainty){
      
      
      rr <- lookup_table[cond, "RR"] %>% as.numeric()
      lb <- lookup_table[cond, "lb"] %>% as.numeric()
      ub <- lookup_table[cond, "ub"] %>% as.numeric()
      
      return (data.frame (rr = rr, lb = lb, ub = ub))
      
    }else{
      
      
      lb <- lookup_table[cond, "lb"] %>% as.numeric()
      ub <- lookup_table[cond, "ub"] %>% as.numeric()
      rr <- truncnorm::rtruncnorm(n = 1, a = lb, b = ub, mean = lookup_table[cond, "RR"] %>% as.numeric())
      
      return (data.frame (rr = rr, lb = lb, ub = ub))
      
    }
    
  }else{
    
    
    if (certainty){
      
      rr = lookup_table[cond, "RR"] %>% as.numeric()
      return(data.frame(rr = rr))
      
    }else{
      
      lb <- lookup_table[cond, "lb"] %>% as.numeric()
      ub <- lookup_table[cond, "ub"] %>% as.numeric()
      
      rr <- truncnorm::rtruncnorm(n = 1, a = lb, b = ub, mean = lookup_table[cond, "RR"] %>% as.numeric())
      return(data.frame(rr = rr))
      
    }
    
  }
  
  # if (certainty){
  #   
  #   
  #   lb <- lookup_table[cond, "lb"] %>% as.numeric()
  #   ub <- lookup_table[cond, "ub"] %>% as.numeric()
  #   
  #   return(stats::runif(1, min=lb, max=ub))
  # }
  # else{
  #   
  #   return(lookup_table[cond, "RR"] %>% as.numeric())
  # }
  
  
  
}
