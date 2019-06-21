#' @export
distances_for_injury_function <- function(pp_summary,dist){
  # This function calculates distances used by two different injury functions.
  # At some point this function will be trimmed to calculate only what we need for injury_function_2
  
  dem_indices <- unique(pp_summary[[1]]$dem_index)
  
  true_dur <- lapply(1:(NSCEN+1),function(x) cbind(rep(SCEN[x],length(dem_indices)),setDT(pp_summary[[x]])[,-'participant_id',with=F][, lapply(.SD, sum), by = c("dem_index")]) )
  true_dur <- data.frame(do.call('rbind',true_dur))
  colnames(true_dur)[1] <- 'scenario'
  mode_cols <- which(sapply(colnames(true_dur),function(x) grepl('_dur',x)))
  modes <- sapply(colnames(true_dur)[mode_cols],function(x) gsub('_dur','',x))
  colnames(true_dur)[mode_cols] <- modes
  mode_speeds <- VEHICLE_INVENTORY$speed[match(modes,VEHICLE_INVENTORY$stage_mode)]
  true_dist <- true_dur
  for(i in 1:length(mode_cols)) true_dist[,mode_cols[i]] <- true_dur[,mode_cols[i]]*mode_speeds[i]/60
  
  true_dist$pedestrian <- true_dist$walking 
  if('walk_to_pt'%in%colnames(true_dist)){
    true_dist$pedestrian <- true_dist$pedestrian + true_dist$walk_to_pt

  }
  ## car is car, taxi, shared auto, shared taxi
  true_dist$car <- rowSums(true_dist[,colnames(true_dist)%in%c('car','taxi','shared_auto','shared_taxi')])
  true_dist <- true_dist[, -which(names(true_dist) %in% c('taxi','shared_auto','shared_taxi','walk_to_pt','walking'))]
  ##!! bus distance increases linearly with bus passenger distance
  ##!! driving allocation to demographic group is the same as passenger
  if(ADD_BUS_DRIVERS){
    passenger <- sapply(SCEN,function(x)sum(subset(true_dist,scenario==x)$bus))
    true_dist$bus_driver <- passenger * BUS_TO_PASSENGER_RATIO
  }
  if(ADD_TRUCK_DRIVERS){
    car <- sapply(SCEN,function(x)sum(subset(true_dist,scenario==x)$car))
    true_dist$truck <- car * TRUCK_TO_CAR_RATIO
  }
  true_distances_0 <- true_dist
  true_distances_0 <- left_join(true_distances_0,DEMOGRAPHIC,by='dem_index')
  names(true_distances_0)[which(names(true_distances_0)=='age')] <- 'age_cat'
  true_distances_0$sex_age <-  paste0(true_distances_0$sex,"_",true_distances_0$age_cat)
  #if(ADD_BUS_DRIVERS) true_distances_0$bus <- true_distances_0$bus + true_distances_0$bus_driver
  true_distances <- true_distances_0[,-c(which(names(true_distances_0) == 'sex'))]
  
  # get distances relative to baseline scenario
  mode_indices <- 3:ncol(true_dist)
  mode_names <- names(true_dist)[mode_indices]
  #scen_dist <- sapply(0:NSCEN+1,function(x)c(colSums(subset(true_dist,scenario == SCEN_SHORT_NAME[x])[,mode_indices])))
  #colnames(scen_dist) <- SCEN_SHORT_NAME
  #for(i in 2:ncol(scen_dist)) scen_dist[,i] <- scen_dist[,i]/scen_dist[,1] 
  #if(CITY=='accra') scen_dist <- rbind(scen_dist,Tuktuk=1)
  
  
  ## for injury_function_2
  mode_names <- names(true_distances)[!names(true_distances)%in%c('age_cat','scenario','sex_age','dem_index')]
  # divide injuries into those for which we can write a WHW (who hit whom) matrix, i.e. we know distances of both striker and casualty, 
  ## and those for which we don't know striker distance: no or other vehicle (NOOV)
  ## we can only model casualties for which we know distance travelled 
  ## we include truck and bus travel via the flags used in run_ithim_setup, if they are missing from the survey, as in accra
  injury_table <- INJURY_TABLE
  
  ## add distance columns
  injuries_for_model <- add_distance_columns(injury_table,mode_names,true_distances_0,dist,scenarios=SCEN[1])
  
  scenario_injury_table <- list()
  for(type in INJURY_TABLE_TYPES) 
    scenario_injury_table[[type]] <- expand.grid(age_cat=unique(DEMOGRAPHIC$age_cat),
                                                 cas_gender=unique(DEMOGRAPHIC$sex),
                                                 cas_mode=unique(injuries_for_model[[1]][[type]]$cas_mode),
                                                 strike_mode=unique(injuries_for_model[[1]][[type]]$strike_mode))
  injuries_list <- add_distance_columns(scenario_injury_table,mode_names,true_distances_0,dist)
  for (n in 1:(NSCEN+1))
    for(type in INJURY_TABLE_TYPES) 
      injuries_list[[n]][[type]]$injury_gen_age <- apply(cbind(as.character(injuries_list[[n]][[type]]$cas_gender),as.character(injuries_list[[n]][[type]]$age_cat)),1,function(x)paste(x,collapse='_'))
  
  # run regression model on baseline data
  reg_model <- list()
  ## Injury regression. This needs a lot of work to make it generalisable to different settings, data qualities, etc.
  ##TODO write formulae without prior knowledge of column names
  ##TODO use all ages with ns(age,...).
  ##RJ linearity in group rates
  CAS_EXPONENT <<- INJURY_LINEARITY * CASUALTY_EXPONENT_FRACTION
  STR_EXPONENT <<- INJURY_LINEARITY - CAS_EXPONENT
  forms <- list(whw='count~cas_mode*strike_mode+offset(log(cas_distance)+log(strike_distance)-CAS_EXPONENT*log(cas_distance_sum)-STR_EXPONENT*log(strike_distance_sum)-log(injury_reporting_rate))',
                nov='count~cas_mode+offset(2*CAS_EXPONENT*log(cas_distance)-log(injury_reporting_rate))')
  # if('age_cat'%in%names(injuries_for_model[[1]][[1]]))
  #   for(type in INJURY_TABLE_TYPES)
  #     forms[[type]] <- paste0(c(forms[[type]],'age_cat'),collapse='+')
  # if('cas_gender'%in%names(injuries_for_model[[1]][[1]]))
  #   for(type in INJURY_TABLE_TYPES)
  #     forms[[type]] <- paste0(c(forms[[type]],'cas_gender'),collapse='+')

  ## catch for when regression fails: if fail, run simpler model: no interactions.
  for(type in INJURY_TABLE_TYPES){
    injuries_for_model[[1]][[type]]$injury_reporting_rate <- 1
    
    test <- 'try-error'
    # try 1: add age cat and gender
    if(any(c('age_cat','cas_gender')%in%names(injuries_for_model[[1]][[type]]))){
      new_form <- forms[[type]]
      if('age_cat'%in%names(injuries_for_model[[1]][[1]]))
        new_form <- paste0(c(new_form,'age_cat'),collapse='+')
      if('cas_gender'%in%names(injuries_for_model[[1]][[1]]))
        new_form <- paste0(c(new_form,'cas_gender'),collapse='+')
      test <- try(glm(as.formula(new_form),data=injuries_for_model[[1]][[type]],family='poisson'))
    }
    if(length(test)==1&&test == 'try-error')
      test <- try(glm(as.formula(forms[[type]]),data=injuries_for_model[[1]][[type]],family='poisson'))
    if(length(test)==1&&test == 'try-error')
      test <- try(glm('offset(2*CAS_EXPONENT*log(cas_distance)-log(injury_reporting_rate))',data=injuries_for_model[[1]][[type]],family='poisson'))
    #
    reg_model[[type]] <- trim_glm_object(test)
    test <- NULL
  }
  return(list(true_distances=true_distances,injuries_list=injuries_list,reg_model=reg_model))
}


# }


  
