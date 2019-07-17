#' @export
get_synthetic_from_trips <- function(){
  
  # create synthetic population
  synth_pop <- create_synth_pop()
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
generate_no_travel_summary <- function(){
  trip_set <- TRIP_SET
  trip_superset <- assign_age_groups(trip_set)
  trip_superset <- left_join(trip_superset,DEMOGRAPHIC,by=c('sex','age_cat'))
  
  modes_to_keep <- intersect(MODE_SPEEDS$stage_mode,unique(trip_superset$trip_mode))
  #modes_to_keep <- modes_to_keep[!is.na(modes_to_keep)]
  modes <- modes_to_keep#unlist(UNCERTAIN_TRAVEL_MODE_NAMES)
  
  missing_trips <- trip_superset[!trip_superset$trip_mode%in%modes_to_keep&!duplicated(trip_superset$participant_id),]
  all_trips <- setDT(trip_superset[trip_superset$trip_mode%in%modes_to_keep,])
  trip_superset <- NULL
  
  NO_TRAVEL_PEOPLE <<- setDT(missing_trips)[,.(no_travel=.N),by='dem_index']
  missing_trips <- NULL
  
  TRAVEL_SUMMARY_TEMPLATE <<- expand.grid(trip_mode=sort(modes_to_keep),dem_index=sort(unique(DEMOGRAPHIC$dem_index)))
  
  #TRAVEL_SUMMARY <<- generate_travel_summary(all_trips)
}

#' @export
generate_travel_summary <- function(all_trips){
  dem_indices <- unique(TRAVEL_SUMMARY_TEMPLATE$dem_index)
  
  # summarise by demographic group those who have done some travel. The rest of the population is in no_travel_people
  travel_people <- all_trips[,.(some_travel=length(unique(participant_id))),by='dem_index']
  
  # number of travellers by demographic group (binomial)
  travel_by_mode_and_demo <- all_trips[,.(travellers=length(unique(participant_id))),by=.(trip_mode,dem_index)]#pp_travel_by_mode[,.(travellers=.N),by=.(trip_mode,dem_index)]
  all_trips <- NULL
  
  setorder(travel_by_mode_and_demo,dem_index,trip_mode)
  # fill travel_by_mode_and_demo (adds in missing groupings) 
  travel_summary <- travel_by_mode_and_demo[setDT(TRAVEL_SUMMARY_TEMPLATE),on=.(trip_mode,dem_index)]
  # add total number of people who complete some travel
  travel_summary <- travel_summary[travel_people,on=c('dem_index')]
  # add total number of people who complete no travel
  travel_summary <- travel_summary[NO_TRAVEL_PEOPLE,on=c('dem_index')]
  # add total number of people
  travel_summary <- travel_summary[,population:=some_travel+no_travel]
  # replace any NA with 0
  travel_summary$travellers[is.na(travel_summary$travellers)] <- 0
  setnames(travel_summary,'trip_mode','mode')
  travel_by_mode_and_demo <- travel_people <- NULL
  
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
