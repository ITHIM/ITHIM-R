#' @export
distances_for_injury_function <- function(trip_scen_sets){
  # This function calculates distances used by two different injury functions.
  # At some point this function will be trimmed to calculate only what we need for injury_function_2
  
  ##!! RJ need to scale distances up for representativeness of survey population of total population
  
  ## for injury_function
  # get total distances
  journeys <- trip_scen_sets %>% 
    group_by (age_cat,sex,stage_mode, scenario) %>% 
    summarise(tot_dist = sum(stage_distance))
  distances <- spread(journeys,stage_mode, tot_dist,fill=0) 
  distances$pedestrian <- distances$walking 
  distances <- distances[, -which(names(distances) ==  "walking")]
  if(ADD_WALK_TO_BUS_TRIPS){
    distances$pedestrian <- distances$pedestrian + distances$walk_to_bus
    distances <- distances[, -which(names(distances) ==  "walk_to_bus")]
  }
  distances$car <- distances$taxi + distances$car
  distances <- distances[, -which(names(distances) ==  "taxi")]
  ## bus distance increases linearly with bus passenger distance
  if('bus_driver'%in%colnames(distances)){
    passenger <- sapply(SCEN,function(x)sum(subset(distances,scenario==x)$bus))
    distances$bus_driver <- distances$bus_driver * passenger[match(distances$scenario,SCEN)] / passenger[[1]]
  }
  true_distances_0 <- distances
  true_distances_0$sex_age <-  paste0(true_distances_0$sex,"_",true_distances_0$age_cat)
  if(ADD_BUS_DRIVERS) true_distances_0$bus <- true_distances_0$bus + true_distances_0$bus_driver
  true_distances <- true_distances_0[,-c(which(names(true_distances_0) == 'sex'))]
  
  # get distances relative to baseline scenario
  scen_dist <- sapply(1:(NSCEN+1),function(x)c(colSums(subset(distances,scenario == SCEN[x])[,colnames(distances)%in%unique(journeys$stage_mode)])))
  colnames(scen_dist) <- SCEN_SHORT_NAME
  for(i in 2:ncol(scen_dist)) scen_dist[,i] <- scen_dist[,i]/scen_dist[,1] 
  if(CITY=='accra') scen_dist <- rbind(scen_dist,Tuktuk=1)
  
  # get distances as a proportion of travel across demographic groups
  mode_names <- names(distances)[names(distances)%in%c(unique(journeys$stage_mode),'pedestrian')]
  for (i in 1: length(mode_names))
    for (n in 1:(NSCEN+1))
      distances[[mode_names[i]]][which(distances$scenario == SCEN[n])] <- 
    distances[[mode_names[i]]][which(distances$scenario == SCEN[n])]/ sum(distances[[mode_names[i]]][which(distances$scenario == SCEN[n])],na.rm=T)
  relative_distances <- distances
  relative_distances$sex_age <-  paste0(relative_distances$sex,"_",relative_distances$age_cat)
  relative_distances <- relative_distances[,-c(which(names(relative_distances) == 'sex'))]
  
  ## for injury_function_2
  mode_names <- names(true_distances)[!names(true_distances)%in%c('age_cat','scenario','sex_age')]
  # divide injuries into those for which we can write a WHW (who hit whom) matrix, i.e. we know distances of both striker and casualty, 
  ## and those for which we don't know striker distance: no or other vehicle (NOOV)
  ## we can only model casualties for which we know distance travelled 
  ## we include truck and bus travel via the flags used in run_ithim_setup, if they are missing from the survey, as in accra
  injury_table <- INJURY_TABLE
  
  ## add distance columns
  injuries_for_model <- add_distance_columns(injury_table,mode_names,true_distances_0,scenarios=SCEN[1])
  
  scenario_injury_table <- list()
  for(type in c('whw','noov')) 
    scenario_injury_table[[type]] <- expand.grid(age_cat=unique(DEMOGRAPHIC$age),
                                                 cas_gender=unique(DEMOGRAPHIC$sex),
                                       cas_mode=unique(injuries_for_model[[1]][[type]]$cas_mode),
                                       strike_mode=unique(injuries_for_model[[1]][[type]]$strike_mode))
  injuries_list <- add_distance_columns(scenario_injury_table,mode_names,true_distances_0)
  
  # run regression model on baseline data
  reg_model <- list()
  ## Injury regression. This needs a lot of work to make it generalisable to different settings, data qualities, etc.
  ##TODO write formulae without prior knowledge of column names
  ##TODO use all ages with ns(age,...).
  ##RJ linearity in group rates
  CAS_EXPONENT <<- INJURY_LINEARITY * CASUALTY_EXPONENT_FRACTION
  STR_EXPONENT <<- INJURY_LINEARITY - CAS_EXPONENT
  forms <- list(whw='count~cas_mode+strike_mode+offset(log(cas_distance)+log(strike_distance)-CAS_EXPONENT*log(cas_distance_sum)+(1-STR_EXPONENT)*log(strike_distance_sum))',
                noov='count~cas_mode+strike_mode+offset(log(cas_distance))')
  if('age_cat'%in%names(injuries_for_model[[1]][[1]]))
    for(type in c('whw','noov'))
      forms[[type]] <- paste0(c(forms[[type]],'age_cat'),collapse='+')
  if('cas_gender'%in%names(injuries_for_model[[1]][[1]]))
    for(type in c('whw','noov'))
      forms[[type]] <- paste0(c(forms[[type]],'cas_gender'),collapse='+')

  ## catch for when regression fails: if fail, run simpler model: no interactions.
  for(type in c('whw','noov')){
    injuries_for_model[[1]][[type]]$injury_reporting_rate <- 1
    reg_model[[type]] <- tryCatch({
      suppressWarnings(glm(as.formula(forms[[type]]),data=injuries_for_model[[1]][[type]],family='poisson',
                                              offset=-log(injury_reporting_rate),control=glm.control(maxit=100)))
    }, error = function(e){
      temp_form <- gsub('*','+',forms[[type]],fixed=T)
      suppressWarnings(glm(as.formula(temp_form),data=injuries_for_model[[1]][[type]],family='poisson',
                           offset=-log(injury_reporting_rate),control=glm.control(maxit=100)))
    }
    )
    #reg_model[[type]] <- trim_glm_object(reg_model[[type]])
  }
  ##
  ## For predictive uncertainty, we could sample a number from the predicted distribution
  if('year'%in%colnames(injuries_list[[1]][[1]])){
    # the injury burden at baseline is the prediction for the most recent year
    most_recent_year <- max(injuries_list[[1]][[1]]$year)
    for(scen in SCEN)
      for(type in c('whw','noov'))
        injuries_list[[scen]][[type]] <- subset(injuries_list[[scen]][[type]],year==most_recent_year)
  }
  
  return(list(relative_distances=relative_distances,scen_dist=scen_dist,true_distances=true_distances,injuries_list=injuries_list,reg_model=reg_model))
}
