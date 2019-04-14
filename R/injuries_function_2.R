#' @export
injuries_function_2 <- function(true_distances,injuries_list,reg_model){
  ## For predictive uncertainty, we could sample a number from the predicted distribution
  cas_modes <- unique(c(as.character(injuries_list[[1]]$whw$cas_mode),as.character(injuries_list[[1]]$noov$cas_mode)))
  injuries <- true_distances[,colnames(true_distances)%in%c(cas_modes,'sex_age','scenario','dem_index')]
  injuries$bus_driver <- 0
  for(scen in SCEN){
    for(type in c('whw','noov')){
      injuries_list[[scen]][[type]]$injury_reporting_rate <- INJURY_REPORTING_RATE
      injuries_list[[scen]][[type]]$pred <- predict(reg_model[[type]],newdata = injuries_list[[scen]][[type]],type='response')
    }
    for(injured_mode in cas_modes)
      for(age_gen in unique(injuries$sex_age))
        injuries[injuries$scenario==scen&injuries$sex_age==age_gen,match(injured_mode,colnames(injuries))] <- 
          sum(subset(injuries_list[[scen]]$whw,cas_mode==injured_mode&injury_gen_age==age_gen)$pred) + 
          sum(subset(injuries_list[[scen]]$noov,cas_mode==injured_mode&injury_gen_age==age_gen)$pred)
  }
  
  injuries$Deaths <- rowSums(injuries[,match(cas_modes,colnames(injuries))])
  injuries
  ##TODO add in uncaptured fatalities as constant
}
