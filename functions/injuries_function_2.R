injuries_function_2 <- function(true_distances,injuries_list,reg_model){
  ## For predictive uncertainty, we could sample a number from the predicted distribution
  injuries <- true_distances
  injuries$Bus_driver <- 0
  for(scen in SCEN){
    for(type in c('whw','noov')){
      injuries_list[[scen]][[type]]$injury_reporting_rate <- INJURY_REPORTING_RATE
      injuries_list[[scen]][[type]]$pred <- predict(reg_model[[type]],newdata = injuries_list[[scen]][[type]],type='response')
    }
    for(injured_mode in unique(injuries_list[[1]]$whw$cas_mode))
      for(age_gen in unique(injuries$sex_age))
        injuries[injuries$scenario==scen&injuries$sex_age==age_gen,match(injured_mode,colnames(injuries))] <- 
          sum(subset(injuries_list[[scen]]$whw,cas_mode==injured_mode&injury_gen_age==age_gen)$pred) + 
          sum(subset(injuries_list[[scen]]$noov,cas_mode==injured_mode&injury_gen_age==age_gen)$pred)
  }
  
  injuries$Deaths <- rowSums(injuries[,match(unique(injuries_list[[1]]$whw$cas_mode),colnames(injuries))])
  injuries
  ##TODO add in uncaptured fatalities as constant
}