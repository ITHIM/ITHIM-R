#' Get distances and model for injuries module
#' 
#' Computes exposures (distances) to parametrise the injury regression model, 
#' which is computed as a Poisson with various offsets and used later in prediction
#' 
#' @param trip_scen_sets list of synthetic trip sets for scenarios
#' @param dist table of (total) distances per mode per scenario
#' 
#' @return list of distances, injury table, and injury regression model
#' 
#' @export
distances_for_injury_function <- function(trip_scen_sets,dist){
  
  ##!! RJ need to scale distances up for representativeness of survey population of total population
  
  ## for injury_function
  # get total distances
  journeys <- trip_scen_sets %>% 
    group_by (age_cat,sex,stage_mode, scenario) %>% 
    summarise(tot_dist = sum(stage_distance))
  trip_scen_sets <- NULL
  
  distances <- spread(journeys,stage_mode, tot_dist,fill=0) 
  distances$pedestrian <- distances$walking 
  if('walk_to_pt'%in%names(distances)){
    distances$pedestrian <- distances$pedestrian + distances$walk_to_pt
  }
  ## car is car, taxi, shared auto, shared taxi
  distances$car <- rowSums(distances[,colnames(distances)%in%c('car','taxi','shared_auto','shared_taxi')])
  distances <- distances[, -which(names(distances) %in% c('taxi','shared_auto','shared_taxi',"walking","walk_to_pt"))]
  ## bus distance increases linearly with bus passenger distance
  if('bus_driver'%in%colnames(distances)){
    passenger <- sapply(SCEN,function(x)sum(subset(distances,scenario==x)$bus))
    distances$bus_driver <- distances$bus_driver * passenger[match(distances$scenario,SCEN)] / passenger[[1]]
  }
  true_distances_0 <- distances
  true_distances_0$sex_age <-  paste0(true_distances_0$sex,"_",true_distances_0$age_cat)
  #if(ADD_BUS_DRIVERS) true_distances_0$bus <- true_distances_0$bus + true_distances_0$bus_driver
  true_distances <- true_distances_0#[,-c(which(names(true_distances_0) == 'sex'))]
  
  ## for injury_function_2
  mode_names <- names(true_distances)[!names(true_distances)%in%c('age_cat','scenario','sex_age','sex')]
  # divide injuries into those for which we can write a WHW (who hit whom) matrix, i.e. we know distances of both striker and casualty, 
  ## and those for which we don't know striker distance: no or other vehicle (NOOV)
  ## we can only model casualties for which we know distance travelled 
  ## we include truck and bus travel via the flags used in run_ithim_setup, if they are missing from the survey, as in accra
  injury_table <- INJURY_TABLE
  
  ## add distance columns
  injuries_for_model <- add_distance_columns(injury_table,mode_names,true_distances_0,dist,scenarios=SCEN[1])
  
  scenario_injury_table <- list()
  for(type in INJURY_TABLE_TYPES) 
    scenario_injury_table[[type]] <- expand.grid(age_cat=unique(DEMOGRAPHIC$age),
                                                 cas_gender=unique(DEMOGRAPHIC$sex),
                                       cas_mode=unique(injuries_for_model[[1]][[type]]$cas_mode),
                                       strike_mode=unique(injuries_for_model[[1]][[type]]$strike_mode))
  injuries_list <- add_distance_columns(injury_table=scenario_injury_table,mode_names,true_distances_0,dist)
  for (n in 1:(NSCEN+1))
    for(type in INJURY_TABLE_TYPES) 
      injuries_list[[n]][[type]]$injury_gen_age <- apply(cbind(as.character(injuries_list[[n]][[type]]$cas_gender),as.character(injuries_list[[n]][[type]]$age_cat)),1,function(x)paste(x,collapse='_'))
  
  # run regression model on baseline data
  reg_model <- list()
  ## Injury regression. This needs a lot of work to make it generalisable to different settings, data qualities, etc.
  ##TODO write formulae without prior knowledge of column names
  ##TODO use all ages with ns(age,...).
  ##RJ linearity in group rates
  CAS_EXPONENT <<- SIN_EXPONENT_SUM * CASUALTY_EXPONENT_FRACTION
  STR_EXPONENT <<- SIN_EXPONENT_SUM - CAS_EXPONENT
  forms <- list(whw='count~cas_mode*strike_mode+offset(log(cas_distance)+log(strike_distance)+(CAS_EXPONENT-1)*log(cas_distance_sum)+(STR_EXPONENT-1)*log(strike_distance_sum)-log(injury_reporting_rate)+log(weight))',
                nov='count~cas_mode+offset(CAS_EXPONENT*log(cas_distance)-log(injury_reporting_rate)+log(weight))')
  if('age_cat'%in%names(injuries_for_model[[1]][[1]]))
    for(type in INJURY_TABLE_TYPES)
      forms[[type]] <- paste0(c(forms[[type]],'age_cat'),collapse='+')
  if('cas_gender'%in%names(injuries_for_model[[1]][[1]]))
    for(type in INJURY_TABLE_TYPES)
      forms[[type]] <- paste0(c(forms[[type]],'cas_gender'),collapse='+')

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
      test <- try(glm('offset(2*CAS_EXPONENT*log(cas_distance)-log(injury_reporting_rate)+log(weight))',data=injuries_for_model[[1]][[type]],family='poisson'))
    #
    reg_model[[type]] <- trim_glm_object(test)
    test <- NULL
    
    # reg_model[[type]] <- tryCatch({
    #   suppressWarnings(glm(as.formula(forms[[type]]),data=injuries_for_model[[1]][[type]],family='poisson',control=glm.control(maxit=100)))
    # }, error = function(e){
    #   temp_form <- gsub('*','+',forms[[type]],fixed=T)
    #   suppressWarnings(glm(as.formula(temp_form),data=injuries_for_model[[1]][[type]],family='poisson',control=glm.control(maxit=100)))
    # }
    # )
    # reg_model[[type]] <- trim_glm_object(reg_model[[type]])
  }
  ##
  ## For predictive uncertainty, we could sample a number from the predicted distribution
  # if('year'%in%colnames(injuries_list[[1]][[1]])){
  #   print('year')
  #   # the injury burden at baseline is the prediction for the most recent year
  #   most_recent_year <- max(injuries_list[[1]][[1]]$year)
  #   for(scen in SCEN)
  #     for(type in INJURY_TABLE_TYPES)
  #       injuries_list[[scen]][[type]] <- subset(injuries_list[[scen]][[type]],year==most_recent_year)
  # }
  
  return(list(true_distances=true_distances,injuries_list=injuries_list,reg_model=reg_model))
}

  ## old code:
# get distances relative to baseline scenario
# scen_dist <- sapply(1:(NSCEN+1),function(x)c(colSums(subset(distances,scenario == SCEN[x])[,colnames(distances)%in%unique(journeys$stage_mode)])))
# colnames(scen_dist) <- SCEN_SHORT_NAME
# for(i in 2:ncol(scen_dist)) scen_dist[,i] <- scen_dist[,i]/scen_dist[,1] 
# if(CITY=='accra') scen_dist <- rbind(scen_dist,Tuktuk=1)

  #for (i in 1: length(mode_names))
  #for (n in 1:(NSCEN+1))
  #  distances[[mode_names[i]]][which(distances$scenario == SCEN[n])] <- 
  #    distances[[mode_names[i]]][which(distances$scenario == SCEN[n])]/ sum(distances[[mode_names[i]]][which(distances$scenario == SCEN[n])],na.rm=T)
  #relative_distances <- distances
  #relative_distances$sex_age <-  paste0(relative_distances$sex,"_",relative_distances$age_cat)
  #relative_distances <- relative_distances[,-c(which(names(relative_distances) == 'sex'))]
  