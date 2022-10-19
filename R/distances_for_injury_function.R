#' Get distances and model for injuries module
#' 
#' Computes exposures (distances) to parametrise the injury regression model, 
#' which is computed as a Poisson with various offsets and used later in prediction
#' 
#' @param journeys df with sex and age_cat total distance (by total population) for scenarios
#' @param dist table of (total) distances per mode per scenario
#' 
#' @return list of distances, injury table, and injury regression model
#' 
#' @export
distances_for_injury_function <- function(journeys, dist){
  
  ##!! RJ need to scale distances up for representativeness of survey population of total population
  ##!! AA have added total distance by taking demographic into the calculations, and then scaling distance accordingly
  
  distances <- spread(journeys,stage_mode, tot_dist,fill=0) 
  
  if('walk_to_pt'%in%names(distances)){
    distances$pedestrian <- distances$pedestrian + distances$walk_to_pt
  }
  ## car is car, taxi, shared auto, shared taxi
  distances$car <- rowSums(distances[,colnames(distances)%in%c('car','taxi','shared_auto','shared_taxi')])
  distances <- distances[, -which(names(distances) %in% c('taxi','shared_auto','shared_taxi',"walk_to_pt"))]
  ## bus distance increases linearly with bus passenger distance
  # NOT needed any longer as Latam scenario definition re-calculates bus driver distances
  # if('bus_driver'%in%colnames(distances)){
  #   passenger <- sapply(SCEN,function(x)sum(subset(distances,scenario==x)$bus))
  #   distances$bus_driver <- distances$bus_driver * passenger[match(distances$scenario,SCEN)] / passenger[[1]]
  # }
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
  
  # add injury_reporting rate
  for(type in INJURY_TABLE_TYPES) 
    INJURY_TABLE[[type]]$injury_reporting_rate <- INJURY_REPORTING_RATE 
  
  INJURY_TABLE <<- INJURY_TABLE

    
  ## add distance columns
  injuries_for_model <- add_distance_columns(injury_table,mode_names,true_distances_0,dist,scenarios=SCEN[1])
  
  zero_dist <- list()
  zero_dist_pos_inj <- list()
  
  zero_dist_flag <- F # flag to highlight if some age and gender categories have at least one fatality but zero distance for a strike and casualty mode combination
  
  # finds cas and strike mode combinations for which there exist zero distances
  for(type in INJURY_TABLE_TYPES){ 
    zero_dist[[type]] <- subset(injuries_for_model$Baseline[[type]],strike_distance == 0 | cas_distance == 0)
    zero_dist_pos_inj[[type]] <- subset(zero_dist[[type]], count > 0)
    if (nrow(zero_dist_pos_inj[[type]])>0){
      zero_dist_flag <- T
    }
  }
  
  # zero_dist_inj[[city]] <<- list()
  # zero_dist_inj[[city]][['zero_dist']] <<- zero_dist
  # zero_dist_inj[[city]][['zero_dist_pos_inj']] <<- zero_dist_pos_inj
  
  # if there exists at least one age and gender category where there exists at least one fatality with zero distance for a strike and casualty mode combination,
  # aggregate by age and gender
  
  if (zero_dist_flag == T ){
    for(type in INJURY_TABLE_TYPES){ 
      injuries_df <- injuries_for_model$Baseline[[type]] #%>% dplyr::select(-c(age_cat, cas_gender))
      setDT(injuries_df)
      injuries_for_model$Baseline[[type]] <- as.data.frame(injuries_df[,.(count=sum(count),weight=mean(weight),strike_distance_sum=mean(strike_distance_sum),
                                                                          cas_distance_sum=mean(cas_distance_sum)),by=c('cas_mode','strike_mode')])
      injuries_for_model$Baseline[[type]]$strike_distance <- injuries_for_model$Baseline[[type]]$strike_distance_sum
      injuries_for_model$Baseline[[type]]$cas_distance <- injuries_for_model$Baseline[[type]]$cas_distance_sum
      injuries_for_model[[1]][[type]]$injury_reporting_rate <- as.numeric(INJURY_REPORTING_RATE)
    }
  }
  
  
  for(type in INJURY_TABLE_TYPES){ 
    injuries_for_model$Baseline[[type]] <- subset(injuries_for_model$Baseline[[type]],strike_distance>0&cas_distance>0)
  }
  

  
  scenario_injury_table <- list()
  for(type in INJURY_TABLE_TYPES) 
    scenario_injury_table[[type]] <- expand.grid(age_cat=unique(DEMOGRAPHIC$age),
                                                 cas_gender=unique(DEMOGRAPHIC$sex),
                                       cas_mode=unique(injuries_for_model[[1]][[type]]$cas_mode),
                                       strike_mode=unique(injuries_for_model[[1]][[type]]$strike_mode)) 
  
  
  injuries_list <- add_distance_columns(injury_table=scenario_injury_table,mode_names,true_distances_0,dist)
  
  
  for (n in 1:(NSCEN+1)){
    for(type in INJURY_TABLE_TYPES){ 
      # remove zero distances
      injuries_list[[n]][[type]] <- subset(injuries_list[[n]][[type]],strike_distance>0&cas_distance>0)
      injuries_list[[n]][[type]]$injury_gen_age <- apply(cbind(as.character(injuries_list[[n]][[type]]$cas_gender),as.character(injuries_list[[n]][[type]]$age_cat)),1,function(x)paste(x,collapse='_'))
      # remove strike and cas mode pairs where cas mode = strike mode
      injuries_list[[n]][[type]]$cas_strike_mode <- apply(cbind(as.character(injuries_list[[n]][[type]]$cas_mode),as.character(injuries_list[[n]][[type]]$strike_mode)),1,function(x)paste(x,collapse='_'))
      injuries_list[[n]][[type]] <- injuries_list[[n]][[type]] %>% filter(cas_strike_mode != 'car_car' & cas_strike_mode != 'bus_bus' & cas_strike_mode != 'motorcycle_motorcycle'
                                                                          & cas_strike_mode != 'cycle_cycle'& cas_strike_mode != 'truck_truck' )
    }
  }
  
  ## determine safety in number coefficients and add as columns to injuries_list and injuries_for_model
  if (CALL_INDIVIDUAL_SIN == F){
    CAS_EXPONENT <<- SIN_EXPONENT_SUM  * CASUALTY_EXPONENT_FRACTION
    STR_EXPONENT <<- SIN_EXPONENT_SUM - CAS_EXPONENT
    
    for(type in INJURY_TABLE_TYPES){
      injuries_for_model$Baseline[[type]]$cas_exponent_col <- CAS_EXPONENT
      injuries_for_model$Baseline[[type]]$str_exponent_col <- STR_EXPONENT
      
        for (n in 1:(NSCEN+1)){
          injuries_list[[n]][[type]]$cas_exponent_col <- CAS_EXPONENT
          injuries_list[[n]][[type]]$str_exponent_col <- STR_EXPONENT
      }
    }
  }
  
  
  if (CALL_INDIVIDUAL_SIN == T){ # assign coefficients depending on strike and victim modes

    injuries_for_model$Baseline$whw$cas_exponent_col <- SIN_EXPONENT_SUM_VEH  * CASUALTY_EXPONENT_FRACTION_VEH
    injuries_for_model$Baseline$whw$str_exponent_col <- SIN_EXPONENT_SUM_VEH  * CASUALTY_EXPONENT_FRACTION_VEH
    
    injuries_for_model$Baseline$whw$cas_exponent_col[injuries_for_model$Baseline$whw$cas_mode == 'cycle'] <- SIN_EXPONENT_SUM_CYCLE  * CASUALTY_EXPONENT_FRACTION_CYCLE
    injuries_for_model$Baseline$whw$str_exponent_col[injuries_for_model$Baseline$whw$cas_mode == 'cycle'] <- SIN_EXPONENT_SUM_CYCLE - SIN_EXPONENT_SUM_CYCLE  * CASUALTY_EXPONENT_FRACTION_CYCLE
    
    injuries_for_model$Baseline$whw$cas_exponent_col[injuries_for_model$Baseline$whw$cas_mode == 'pedestrian'] <- SIN_EXPONENT_SUM_PED  * CASUALTY_EXPONENT_FRACTION_PED
    injuries_for_model$Baseline$whw$str_exponent_col[injuries_for_model$Baseline$whw$cas_mode == 'pedestrian'] <- SIN_EXPONENT_SUM_PED - SIN_EXPONENT_SUM_PED  * CASUALTY_EXPONENT_FRACTION_PED
    
    
    for (n in 1:(NSCEN+1)){
      injuries_list[[n]]$whw$cas_exponent_col <- SIN_EXPONENT_SUM_VEH  * CASUALTY_EXPONENT_FRACTION_VEH
      injuries_list[[n]]$whw$str_exponent_col <- SIN_EXPONENT_SUM_VEH  * CASUALTY_EXPONENT_FRACTION_VEH
      
      injuries_list[[n]]$whw$cas_exponent_col[injuries_list[[n]]$whw$cas_mode == 'cycle'] <- SIN_EXPONENT_SUM_CYCLE  * CASUALTY_EXPONENT_FRACTION_CYCLE
      injuries_list[[n]]$whw$str_exponent_col[injuries_list[[n]]$whw$cas_mode == 'cycle'] <- SIN_EXPONENT_SUM_CYCLE - SIN_EXPONENT_SUM_CYCLE  * CASUALTY_EXPONENT_FRACTION_CYCLE
      
      injuries_list[[n]]$whw$cas_exponent_col[injuries_list[[n]]$whw$cas_mode == 'pedestrian'] <- SIN_EXPONENT_SUM_PED  * CASUALTY_EXPONENT_FRACTION_PED
      injuries_list[[n]]$whw$str_exponent_col[injuries_list[[n]]$whw$cas_mode == 'pedestrian'] <- SIN_EXPONENT_SUM_PED - SIN_EXPONENT_SUM_PED  * CASUALTY_EXPONENT_FRACTION_PED
    }
    
    injuries_for_model$Baseline$nov$cas_exponent_col <- SIN_EXPONENT_SUM_NOV
    injuries_for_model$Baseline$nov$str_exponent_col <- 1
    
    for (n in 1:(NSCEN+1)){
      injuries_list[[n]]$nov$cas_exponent_col <- SIN_EXPONENT_SUM_NOV 
      injuries_list[[n]]$nov$str_exponent_col <- 1
    }

  }
  
  # # save injuries_for_model as part of ithim_object
  # ithim_object$inj_distances <- list()
  # ithim_object$inj_distances$injuries_for_model <- injuries_for_model

  # run regression model on baseline data
  reg_model <- list()
  ## Injury regression. This needs a lot of work to make it generalisable to different settings, data qualities, etc.
  ##TODO write formulae without prior knowledge of column names
  ##TODO use all ages with ns(age,...).
  ##RJ linearity in group rates
  # CAS_EXPONENT <<- SIN_EXPONENT_SUM  * CASUALTY_EXPONENT_FRACTION
  # STR_EXPONENT <<- SIN_EXPONENT_SUM - CAS_EXPONENT

  forms <- list(whw='count~cas_mode+strike_mode+offset(log(cas_distance)+(cas_exponent_col-1)*log(cas_distance_sum)+log(strike_distance)+
                    (str_exponent_col-1)*log(strike_distance_sum)+log(injury_reporting_rate)+log(weight))',
                nov='count~cas_mode+offset(log(cas_distance)+(cas_exponent_col-1)*log(cas_distance_sum)+log(injury_reporting_rate)+log(weight))')
  
  if('age_cat'%in%names(injuries_for_model[[1]][[1]]))
    for(type in INJURY_TABLE_TYPES)
      forms[[type]] <- paste0(c(forms[[type]],'age_cat'),collapse='+')
  if('cas_gender'%in%names(injuries_for_model[[1]][[1]]))
    for(type in INJURY_TABLE_TYPES)
      forms[[type]] <- paste0(c(forms[[type]],'cas_gender'),collapse='+')

  max_std <- list() # store largest standard errors
  max_std_def <- 10
  
  
  # try most sophisticated model first and then successively simplify model
  for(type in INJURY_TABLE_TYPES){

    injuries_for_model[[1]][[type]]$injury_reporting_rate <- INJURY_REPORTING_RATE
    
    # remove strike and cas mode pairs where cas mode = strike mode
    injuries_for_model[[1]][[type]]$cas_strike_mode <- apply(cbind(as.character(injuries_for_model[[1]][[type]]$cas_mode),
                                                                   as.character(injuries_for_model[[1]][[type]]$strike_mode)),1,function(x)paste(x,collapse='_'))
    injuries_for_model[[1]][[type]] <- injuries_for_model[[1]][[type]] %>% filter(cas_strike_mode != 'car_car' & cas_strike_mode != 'bus_bus' & cas_strike_mode != 'motorcycle_motorcycle'
                                                                        & cas_strike_mode != 'cycle_cycle'& cas_strike_mode != 'truck_truck' )
    
    
    
    test <- 'try-error'
    

    # try poisson distribution
    #if (length(test) == 1 | max_std[[type]] > max_std_def | aic > 100000 | aic < 0 | theta > 100000){
      
    test <- try(glm(as.formula(forms[[type]]),data=injuries_for_model[[1]][[type]],family = 'poisson'))
    
    if (length(test) != 1){
      max_std[[type]] <- max(summary(test)$coefficients[,2])
    } else {
      max_std[[type]] <- 1000
    }


    # if either standard errors are high or if no model could be built, try building model without age and gender categories if not already done

    if (length(test) == 1 | max_std[[type]] > max_std_def ){

      # first remove age and gender if possible and fit model without age and gender categories
      if ('age_cat'%in%names(injuries_for_model[[1]][[1]])){

        injuries_df <- injuries_for_model$Baseline[[type]] #%>% dplyr::select(-c(age_cat, cas_gender))
        setDT(injuries_df)
        injuries_for_model$Baseline[[type]] <- as.data.frame(injuries_df[,.(count=sum(count),weight=mean(weight),strike_distance_sum=mean(strike_distance_sum),
                                                                            cas_distance_sum=mean(cas_distance_sum),
                                                                            cas_exponent_col = mean(cas_exponent_col),
                                                                            str_exponent_col = mean(str_exponent_col)
                                                                            ),by=c('cas_mode','strike_mode')])
        injuries_for_model$Baseline[[type]]$strike_distance <- injuries_for_model$Baseline[[type]]$strike_distance_sum
        injuries_for_model$Baseline[[type]]$cas_distance <- injuries_for_model$Baseline[[type]]$cas_distance_sum
        injuries_for_model[[1]][[type]]$injury_reporting_rate <- as.numeric(INJURY_REPORTING_RATE)

        # try model without age and gender categories
        forms_no_agecat <- list(whw='count~cas_mode+strike_mode+offset(log(cas_distance)+(cas_exponent_col-1)*log(cas_distance_sum)
                                            +log(strike_distance)+(str_exponent_col-1)*log(strike_distance_sum)+
                                            log(injury_reporting_rate)+log(weight))',
                                nov='count~cas_mode+offset(log(cas_distance)+(cas_exponent_col-1)*log(cas_distance_sum)+log(injury_reporting_rate)+log(weight))')
  
        ### re-run regression model
        test <- try(glm(as.formula(forms_no_agecat[[type]]),data=injuries_for_model[[1]][[type]], family = 'poisson'))

      }  
    }

    
    if (length(test) != 1){
      max_std[[type]] <- max(summary(test)$coefficients[,2])
    } else {
      max_std[[type]] <- 0
    }
    
    if (max_std[[type]]  > max_std_def){
      print(paste0("!! The ", type, " injury model for ", city, " has large standard errors!!!"))
    }
    
    
    
    

    reg_model[[type]] <- test
    test <- NULL
  }
    


  return(list(true_distances=true_distances,injuries_list=injuries_list,reg_model=reg_model, injuries_for_model = injuries_for_model))
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
  
