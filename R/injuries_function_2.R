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
  # both tibbles, return tibble
  injuries <- dplyr::left_join(injuries,demographic,by=c('age_cat','sex'))
  injuries$bus_driver <- 0
  colnames(demographic)[which(colnames(demographic)=='sex')] <- 'cas_gender'
  whw_temp <- list()
  for(scen in SCEN){
    whw_temp[[scen]] <- list()
    for(type in INJURY_TABLE_TYPES){
      injuries_list[[scen]][[type]]$injury_reporting_rate <- 1
      
      # Set weight to 1 for prediction
      injuries_list[[scen]][[type]]$weight <- 1
      
      ## Use link type and use linkinv function to account for positive confidence interval for the poisson distr.
      # Ref: https://fromthebottomoftheheap.net/2018/12/10/confidence-intervals-for-glms/
      
      # grad the inverse link function
      ilink <- family(reg_model[[type]])$linkinv
      # add fit and se.fit on the **link** scale
      injuries_list[[scen]][[type]] <- bind_cols(injuries_list[[scen]][[type]], setNames(as_tibble(predict(reg_model[[type]], newdata = remove_missing_levels(reg_model[[type]],
                                                                                                                                                              injuries_list[[scen]][[type]]),
                                                                                                           type='link', se.fit = TRUE)[1:2]),
                                                                                         c('fit_link','se_link')))



      pred_val <- predict(reg_model[[type]],newdata = remove_missing_levels(reg_model[[type]],injuries_list[[scen]][[type]]), type='response', se.fit = TRUE) %>% as.data.frame()
      injuries_list[[scen]][[type]]$pred_ub_am <- pred_val$fit + (2 * pred_val$se.fit)

      ## create the interval and back-transform
      injuries_list[[scen]][[type]] <- mutate(injuries_list[[scen]][[type]],
                                              pred  = ilink(fit_link),
                                              pred_ub = ifelse(is.infinite(ilink(fit_link + (2 * se_link))), pred_ub_am,
                                                               ilink(fit_link + (2 * se_link))),
                                              pred_lb = ilink(fit_link - (2 * se_link)))
   
      # pred_val <- predict(reg_model[[type]],newdata = remove_missing_levels(reg_model[[type]],injuries_list[[scen]][[type]]), type='response', se.fit = TRUE) %>% as.data.frame()
      # injuries_list[[scen]][[type]] <- mutate(injuries_list[[scen]][[type]],
      #                                         pred  = pred_val$fit,
      #                                         pred_ub = pred_val$fit + (2 * pred_val$se.fit),
      #                                         pred_lb = pred_val$fit - (2 * pred_val$se.fit))
      # 
      
      
      if(constant_mode){
        whw_temp[[scen]][[type]] <- sapply(unique(injuries_list[[scen]][[type]]$cas_mode),function(x)
          sapply(unique(injuries_list[[scen]][[type]]$strike_mode),function(y)sum(subset(injuries_list[[scen]][[type]], cas_mode == x & strike_mode == y)$pred, na.rm = T)))
        if(type=='whw'){
          colnames(whw_temp[[scen]][[type]]) <- unique(injuries_list[[scen]][[type]]$cas_mode)
          rownames(whw_temp[[scen]][[type]]) <- unique(injuries_list[[scen]][[type]]$strike_mode)
        }else{
          names(whw_temp[[scen]][[type]]) <- unique(injuries_list[[scen]][[type]]$cas_mode)
        }
      }
      
      if(constant_mode){
        for(conf in c("ub", "lb")){
          var_name <- paste0(type, "_", conf)
          whw_temp[[scen]][[var_name]] <- sapply(unique(injuries_list[[scen]][[type]]$cas_mode),function(x)
            sapply(unique(injuries_list[[scen]][[type]]$strike_mode),function(y)sum(subset(injuries_list[[scen]][[type]], 
                                                                                           cas_mode == x & strike_mode == y)[[paste0("pred_", conf)]], na.rm = T)))
          if(type=='whw'){
            colnames(whw_temp[[scen]][[var_name]]) <- unique(injuries_list[[scen]][[type]]$cas_mode)
            rownames(whw_temp[[scen]][[var_name]]) <- unique(injuries_list[[scen]][[type]]$strike_mode)
          }else{
            names(whw_temp[[scen]][[var_name]]) <- unique(injuries_list[[scen]][[type]]$cas_mode)
          }
        }
      }
      
      # data.frame and tibble, returns data.frame
      suppressWarnings(
        injuries_list[[scen]][[type]] <- dplyr::left_join(injuries_list[[scen]][[type]],demographic,by=c('age_cat','cas_gender'))
      )
    }
    
    for(injured_mode in cas_modes)
      for(index in unique(injuries$dem_index))
        injuries[injuries$scenario==scen&injuries$dem_index==index,match(injured_mode,colnames(injuries))] <- 0
    
    
    injuries_lb <- injuries_ub <- injuries
    
    for(injured_mode in cas_modes)
      for(index in unique(injuries$dem_index))
        for(type in INJURY_TABLE_TYPES)
          injuries[injuries$scenario==scen&injuries$dem_index==index,match(injured_mode,colnames(injuries))] <- 
      injuries[injuries$scenario==scen&injuries$dem_index==index,match(injured_mode,colnames(injuries))] + 
      sum(injuries_list[[scen]][[type]][injuries_list[[scen]][[type]]$cas_mode==injured_mode&
                                          injuries_list[[scen]][[type]]$dem_index==index,]$pred) 
    if(constant_mode){
      for(injured_mode in cas_modes)
        for(index in unique(injuries_lb$dem_index))
          for(type in INJURY_TABLE_TYPES)
            injuries_lb[injuries_lb$scenario==scen&injuries_lb$dem_index==index,match(injured_mode,colnames(injuries_lb))] <- 
              injuries_lb[injuries_lb$scenario==scen&injuries_lb$dem_index==index,match(injured_mode,colnames(injuries_lb))] + 
              sum(injuries_list[[scen]][[type]][injuries_list[[scen]][[type]]$cas_mode==injured_mode&
                                                  injuries_list[[scen]][[type]]$dem_index==index,]$pred_lb)
      
      for(injured_mode in cas_modes)
        for(index in unique(injuries_ub$dem_index))
          for(type in INJURY_TABLE_TYPES)
            injuries_ub[injuries_ub$scenario==scen&injuries_ub$dem_index==index,match(injured_mode,colnames(injuries_ub))] <- 
              injuries_ub[injuries_ub$scenario==scen&injuries_ub$dem_index==index,match(injured_mode,colnames(injuries_ub))] + 
              sum(injuries_list[[scen]][[type]][injuries_list[[scen]][[type]]$cas_mode==injured_mode&
                                                  injuries_list[[scen]][[type]]$dem_index==index,]$pred_ub)
    }
    
  }
  
  # This piece of code is duplicating the sum of deaths for most of modes (bus, car, cycle, motorcycle), because it looks for casualty modes in both nov and whw. I fixed it by getting unique values in casualty modes from both nov and whw
  
  # injuries$Deaths <- rowSums(injuries[,match(unique(injuries_list[[1]]$whw$cas_mode),colnames(injuries))]) +
  #   rowSums(injuries[,match(unique(injuries_list[[1]]$nov$cas_mode),colnames(injuries))])
  cas_names <- unique(c(unique(injuries_list[[1]]$whw$cas_mode), 
                        unique(injuries_list[[1]]$nov$cas_mode)))
  
  # Assume injuries as tibble and use dplyr instead
  # Also remove NAs
  injuries <- injuries %>% ungroup() %>% mutate(Deaths = rowSums(dplyr::select(., cas_names %>% as.character()), na.rm = T))
  
  if(constant_mode){
    injuries_lb <- injuries_lb %>% ungroup() %>% mutate(Deaths_lb = rowSums(dplyr::select(., cas_names %>% as.character()), na.rm = T))
    injuries_ub <- injuries_ub %>% ungroup() %>% mutate(Deaths_ub = rowSums(dplyr::select(., cas_names %>% as.character()), na.rm = T))
    
    injuries <- dplyr::left_join(injuries, injuries_lb %>% dplyr::select(age_cat, sex, dem_index, scenario, Deaths_lb), by = c('age_cat', 'sex', 'dem_index', 'scenario'))
    injuries <- dplyr::left_join(injuries, injuries_ub %>% dplyr::select(age_cat, sex, dem_index, scenario, Deaths_ub), by = c('age_cat', 'sex', 'dem_index', 'scenario'))
  }
  list(injuries, whw_temp)
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