#' @export
get_synthetic_from_trips <- function(){
  
  ##!! to get the right order of trip columns; needed if trips are added
  raw_trip_set <- data.frame(trip_id=TRIP_SET$trip_id,
                             trip_mode=TRIP_SET$trip_mode,
                             trip_distance=TRIP_SET$trip_distance,
                             stage_mode=TRIP_SET$stage_mode,
                             stage_distance=TRIP_SET$stage_distance,
                             stage_duration=TRIP_SET$stage_duration,
                             participant_id=TRIP_SET$participant_id,
                             age=TRIP_SET$age,
                             sex=TRIP_SET$sex, 
                             stringsAsFactors = F)
  TRIP_SET <- NULL
  
  # TRAVEL_SUMMARY <- generate_travel_summary(raw_trip_set)
  # TRAVEL_SUMMARY <- scale_mode_probabilities(TRAVEL_SUMMARY)
  # TRAVEL_SUMMARY <- resample_mode_probabilities(TRAVEL_SUMMARY)
  
  ##!! number trips
  #raw_trip_set$trip_id[!is.na(raw_trip_set$trip_id)] <- 1:sum(!is.na(raw_trip_set$trip_id))
  
  ## add motorcycle trip to accra, and replicate set four times
  #if(CITY=='accra') raw_trip_set <- edit_accra_trips(raw_trip_set)
  #SURVEY_SCALAR <<- population/length(unique(TRIP_SET$participant_id))/survey_coverage
  
  ## add bus and truck trips
  #if(ADD_BUS_DRIVERS) raw_trip_set <- add_ghost_trips(raw_trip_set)
  #if(ADD_TRUCK_DRIVERS) raw_trip_set <- add_ghost_trips(raw_trip_set,trip_mode='truck',distance_ratio=TRUCK_TO_CAR_RATIO*DISTANCE_SCALAR_CAR_TAXI,reference_mode='car')

  # create synthetic population
  synth_pop <- create_synth_pop(raw_trip_set)
  raw_trip_set <- NULL
  SYNTHETIC_POPULATION <<- synth_pop$synthetic_population
  trip_set <- synth_pop$trip_set
  synth_pop <- NULL
  
  # create scenarios: either the walking test case, or the 5 hard-coded Accra scenarios
  trip_set <- ithim_setup_baseline_scenario(trip_set)
  ## update all distances and durations
  trip_set <- scale_trip_distances(trip_set)
  
  if(TEST_WALK_SCENARIO){
    SYNTHETIC_TRIPS <<- create_walk_scenario(trip_set)
  }else if(TEST_CYCLE_SCENARIO){
    SYNTHETIC_TRIPS <<- create_cycle_scenarios(trip_set)
  }else if(MAX_MODE_SHARE_SCENARIO){
    SYNTHETIC_TRIPS <<- create_max_mode_share_scenarios(trip_set)
  }else{
    SYNTHETIC_TRIPS <<- create_all_scenarios(CITY, trip_set)
  }
  
  #set_scenario_specific_variables()
  # some useful variables.
  NSCEN <<- length(SYNTHETIC_TRIPS) - 1
  SCEN <<- sapply(SYNTHETIC_TRIPS,function(x)x$scenario[1])
  SCEN_SHORT_NAME <<- c("base",paste0("scen", 1:NSCEN) )
  
}

#' @export
generate_travel_summary <- function(trip_set){
  demographic <- DEMOGRAPHIC
  trip_superset <- assign_age_groups(trip_set)
  trip_superset <- left_join(trip_superset,demographic,by=c('sex','age_cat'))
  
  modes_to_keep <- unique(trip_superset$stage_mode)
  modes_to_keep <- modes_to_keep[!is.na(modes_to_keep)]
  modes <- modes_to_keep#unlist(UNCERTAIN_TRAVEL_MODE_NAMES)
  
  missing_trips <- trip_superset[is.na(trip_superset$stage_mode)&!duplicated(trip_superset$participant_id),]
  all_trips <- setDT(trip_superset[!is.na(trip_superset$stage_mode),])
  rm(trip_superset)
  
  no_travel_people <- setDT(missing_trips)[,.(no_travel=.N),by='dem_index']
  rm(missing_trips)
  
  travel_data <- PP_TRAVEL_PROPENSITIES
  # get indices for mode random variables
  m_inds <- sapply(modes,function(modei) which(names(travel_data)==paste0(modei,'_p_rn')))
  travel_summary_template <- expand.grid(stage_mode=sort(modes),dem_index=sort(unique(demographic$dem_index)))
  dem_indices <- unique(travel_summary_template$dem_index)
  
  rm(demographic)
  
    
    ## get trips and clear memory
    superset <- all_trips
    
    ## get summary travel information (by mode and demography) and clear memory
    pp_travel_by_mode <- unique( superset[,.(num_trips=.N,dem_index=dem_index),by=c('stage_mode','participant_id')], by=c('stage_mode','participant_id'))
    travel_people <- unique(superset,by='participant_id')[,.(some_travel=.N),by='dem_index']
    rm(superset)
    travel_by_mode_and_demo <- pp_travel_by_mode[,.(travellers=.N),by=c('stage_mode','dem_index')]
    travel_by_mode_and_demo <- setorder(travel_by_mode_and_demo,dem_index,stage_mode)
    travel_summary <- travel_by_mode_and_demo[setDT(travel_summary_template),on=c('stage_mode','dem_index')]
    travel_summary <- travel_summary[travel_people,on=c('dem_index')]
    travel_summary <- travel_summary[no_travel_people,on=c('dem_index')]
    travel_summary <- travel_summary[,population:=some_travel+no_travel]
    travel_summary$travellers[is.na(travel_summary$travellers)] <- 0
    setnames(travel_summary,'stage_mode','mode')
    rm(pp_travel_by_mode,travel_by_mode_and_demo,travel_people)
    
    ## estimate smooth probabilities
    travel_summary$raw_probability <- travel_summary$travellers/travel_summary$population
    travel_summary$smooth_probability <- #raw_probability
      #smooth_probability[smooth_probability==0] <- 0.001
      suppressWarnings(glm(raw_probability~I(dem_index<(max(dem_index)/2))+I(dem_index%%(max(dem_index)/2))+mode,
                           family=binomial,data=travel_summary)$fitted.values)
    return(travel_summary)
}

#' @export
scale_mode_probabilities <- function(travel_summary){
  car_taxi_modes <- UNCERTAIN_TRAVEL_MODE_NAMES$car
  pt_modes <- UNCERTAIN_TRAVEL_MODE_NAMES$pt
  
  ## multiply mode probability scalars
  match_modes <- rep(1,nrow(travel_summary))
  modes <- travel_summary$mode
  match_modes[modes%in%car_taxi_modes] <- PROBABILITY_SCALAR_CAR_TAXI
  match_modes[modes%in%c('walking')] <- PROBABILITY_SCALAR_WALKING
  match_modes[modes%in%pt_modes] <- PROBABILITY_SCALAR_PT
  match_modes[modes%in%c('cycling')] <- PROBABILITY_SCALAR_CYCLING
  match_modes[modes%in%c('motorcycle')] <- PROBABILITY_SCALAR_MOTORCYCLE
  travel_summary$smooth_probability <- travel_summary$smooth_probability*match_modes
  travel_summary$raw_probability <- travel_summary$raw_probability*match_modes
  return(travel_summary)  
}

#' @export
resample_mode_probabilities <- function(travel_summary){
  if(is.list(PROPENSITY_TO_TRAVEL)) {
    pointiness <- 300
    beta_val <- (1/travel_summary$smooth_probability - 1)*pointiness/(1 + (1/travel_summary$smooth_probability - 1))
    #beta_val <- (1/raw_probability - 1)*pointiness/(1 + (1/raw_probability - 1))
    alpha_val <- pointiness - beta_val
    uncertain_rows <- travel_summary$mode%in%unlist(UNCERTAIN_TRAVEL_MODE_NAMES)
    propensities <- rep(0.5,nrow(travel_summary))
    propensities[uncertain_rows] <- sapply(travel_summary$mode[uncertain_rows],
                                           function(x)PROPENSITY_TO_TRAVEL[[which(sapply(UNCERTAIN_TRAVEL_MODE_NAMES,
                                                                                         function(y)x%in%y))]])
    travel_summary$probability <- qbeta(propensities,alpha_val,beta_val)
    if(any(!uncertain_rows)) travel_summary$probability[!uncertain_rows] <- travel_summary$raw_probability[!uncertain_rows]
    #travel_summary$probability <- sigmoid(sapply(1:nrow(travel_summary),function(x)dnorm(PROPENSITY_TO_TRAVEL[[travel_summary$mode[x]]],travel_summary$mu[x],travel_summary$sd[x])))
  }else{
    travel_summary$probability <- travel_summary$raw_probability#qbeta(PROPENSITY_TO_TRAVEL,travel_summary$alpha_val,travel_summary$beta_val)
    # 
  }
  return(travel_summary)
}