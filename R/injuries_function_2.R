#' Predict injuries
#' 
#' Predict injuries based on regression model from baseline and scenario travel
#' 
#' @param true_distances data frame to set up results
#' @param injuries_list list of data frames to supply to regression model for prediction
#' @param reg_model regression glm object
#' @param constant_mode whether or not we are in constant (vs sampling) mode
#' 
#' @return list of injury prediction data frames and whw matrices
#' 
#' @export
injuries_function_2 <- function(true_distances,injuries_list,reg_model,constant_mode=F){
  ## For predictive uncertainty, we could sample a number from the predicted distribution
  cas_modes <- unique(as.character(injuries_list[[1]][[1]]$cas_mode))
  if(length(injuries_list[[1]])==2)
    cas_modes <- unique(c(cas_modes,as.character(injuries_list[[1]]$nov$cas_mode)))
  demographic <- DEMOGRAPHIC
  demographic$dem_index <- 1:nrow(demographic)
  demographic <- demographic[,-which(colnames(demographic)=='population')]
  colnames(demographic)[which(colnames(demographic)=='age')] <- 'age_cat'
  injuries <- true_distances
  injuries <- left_join(injuries,demographic,by=c('age_cat','sex'))
  injuries$bus_driver <- 0
  colnames(demographic)[which(colnames(demographic)=='sex')] <- 'cas_gender'
  whw_temp <- list()
  for(scen in SCEN){
    whw_temp[[scen]] <- list()
    for(type in INJURY_TABLE_TYPES){
      injuries_list[[scen]][[type]]$injury_reporting_rate <- INJURY_REPORTING_RATE
      injuries_list[[scen]][[type]]$weight <- 1
      injuries_list[[scen]][[type]]$pred <- predict(reg_model[[type]],newdata = remove_missing_levels(reg_model[[type]],injuries_list[[scen]][[type]]),type='response')
      if(constant_mode){
        whw_temp[[scen]][[type]] <- sapply(unique(injuries_list[[scen]][[type]]$cas_mode),function(x)
          sapply(unique(injuries_list[[scen]][[type]]$strike_mode),function(y)sum(subset(injuries_list[[scen]][[type]],cas_mode==x&strike_mode==y)$pred,na.rm=T)))
        if(type=='whw'){
          colnames(whw_temp[[scen]][[type]]) <- unique(injuries_list[[scen]][[type]]$cas_mode)
          rownames(whw_temp[[scen]][[type]]) <- unique(injuries_list[[scen]][[type]]$strike_mode)
        }else{
          names(whw_temp[[scen]][[type]]) <- unique(injuries_list[[scen]][[type]]$cas_mode)
        }
      }
      suppressWarnings(
        injuries_list[[scen]][[type]] <- left_join(injuries_list[[scen]][[type]],demographic,by=c('age_cat','cas_gender'))
      )
    }
    
    for(injured_mode in cas_modes)
      for(index in unique(injuries$dem_index))
        injuries[injuries$scenario==scen&injuries$dem_index==index,match(injured_mode,colnames(injuries))] <- 0
    
    for(injured_mode in cas_modes)
      for(index in unique(injuries$dem_index))
        for(type in INJURY_TABLE_TYPES)
          injuries[injuries$scenario==scen&injuries$dem_index==index,match(injured_mode,colnames(injuries))] <- 
            injuries[injuries$scenario==scen&injuries$dem_index==index,match(injured_mode,colnames(injuries))] + 
              sum(injuries_list[[scen]][[type]][injuries_list[[scen]][[type]]$cas_mode==injured_mode&
                                                injuries_list[[scen]][[type]]$dem_index==index,]$pred) 
      
  }
  
  injuries$Deaths <- rowSums(injuries[,match(unique(injuries_list[[1]]$whw$cas_mode),colnames(injuries))]) +
    rowSums(injuries[,match(unique(injuries_list[[1]]$nov$cas_mode),colnames(injuries))])
  list(injuries,whw_temp)
  ##TODO add in uncaptured fatalities as constant
}


# @title remove_missing_levels
# @description Accounts for missing factor levels present only in test data
# but not in train data by setting values to NA
#
# @import magrittr
# @importFrom gdata unmatrix
# @importFrom stringr str_split
#
# @param fit fitted model on training data
#
# @param test_data data to make predictions for
#
# @return data.frame with matching factor levels to fitted model
#
# @keywords internal
#
##!! temporary fix for missing (age) factors
#' @export
remove_missing_levels <- function(fit, test_data) {
  
  # https://stackoverflow.com/a/39495480/4185785
  
  # drop empty factor levels in test data
  test_data <- as.data.frame(droplevels(test_data))
  
  # 'fit' object structure of 'lm' and 'glmmPQL' is different so we need to
  # account for it
  if (any(class(fit) == "glmmPQL")) {
    # Obtain factor predictors in the model and their levels
    factors <- (gsub("[-^0-9]|as.factor|\\(|\\)", "",
                     names(unlist(fit$contrasts))))
    # do nothing if no factors are present
    if (length(factors) == 0) {
      return(test_data)
    }
    
    map(fit$contrasts, function(x) names(unmatrix(x))) %>%
      unlist() -> factor_levels
    factor_levels %>% str_split(":", simplify = TRUE) %>%
      extract(, 1) -> factor_levels
    
    model_factors <- as.data.frame(cbind(factors, factor_levels))
  } else {
    # Obtain factor predictors in the model and their levels
    factors <- (gsub("[-^0-9]|as.factor|\\(|\\)", "",
                     names(unlist(fit$xlevels))))
    # do nothing if no factors are present
    if (length(factors) == 0) {
      return(test_data)
    }
    
    factor_levels <- unname(unlist(fit$xlevels))
    model_factors <- as.data.frame(cbind(factors, factor_levels))
  }
  
  # Select column names in test data that are factor predictors in
  # trained model
  
  predictors <- names(test_data[names(test_data) %in% factors])
  
  # For each factor predictor in your data, if the level is not in the model,
  # set the value to NA
  
  for (i in 1:length(predictors)) {
    found <- test_data[, predictors[i]] %in% model_factors[
      model_factors$factors == predictors[i], ]$factor_levels
    if (any(!found)) {
      # track which variable
      var <- predictors[i]
      # set to NA
      test_data[!found, predictors[i]] <- NA
      # drop empty factor levels in test data
      test_data <- droplevels(test_data) 
      # issue warning to console
      message(sprintf(paste0("Setting missing levels in '%s', only present",
                             " in test data but missing in train data,",
                             " to 'NA'."),
                      var))
    }
  }
  return(test_data)
}