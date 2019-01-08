#' @export
scenario_pm_calculations <- function(dist,trip_scen_sets){
  
  # concentration contributed by non-transport share (remains constant across the scenarios)
  non_transport_pm_conc <- PM_CONC_BASE*(1 - PM_TRANS_SHARE)  
  
  ## adding in travel not covered in the synthetic trip set, based on distances travelled relative to car, set in VEHICLE_INVENTORY
  emission_dist <- dist
  for(mode_type in which(!VEHICLE_INVENTORY$trip_mode%in%emission_dist$trip_mode)){
    emission_dist <- rbind(emission_dist,emission_dist[which(emission_dist$trip_mode=='Private Car'),])
    emission_dist[nrow(emission_dist),1] <- VEHICLE_INVENTORY$trip_mode[mode_type]
    emission_dist[nrow(emission_dist),0:NSCEN+2] <- emission_dist[nrow(emission_dist),2]*VEHICLE_INVENTORY$distance_ratio_to_car[mode_type]
  }
  
  ## multiply distance by emission factor. (We don't need to scale to a whole year, as we are just scaling the background concentration.)
  ordered_efs <- VEHICLE_INVENTORY$emission_factor[match(emission_dist$trip_mode,VEHICLE_INVENTORY$trip_mode)]
  trans_emissions <- emission_dist[,0:NSCEN+2]*t(repmat(ordered_efs,NSCEN+1,1))#*SURVEY_SCALAR
  
  baseline_sum <- sum(trans_emissions[[SCEN[1]]])
  conc_pm <- c()
  for(i in 1:length(SCEN_SHORT_NAME))
    conc_pm[i] <- non_transport_pm_conc + PM_TRANS_SHARE*PM_CONC_BASE*sum(trans_emissions[[SCEN[i]]])/baseline_sum
  
  ##RJ rewriting ventilation as a function of MMET_CYCLING and MMET_WALKING, loosely following de Sa's SP model.
  vent_rates <- data.frame(trip_mode=VEHICLE_INVENTORY$trip_mode,stringsAsFactors = F) 
  vent_rates$vent_rate <- BASE_LEVEL_INHALATION_RATE # L / min
  vent_rates$vent_rate[vent_rates$trip_mode=='Bicycle'] <- BASE_LEVEL_INHALATION_RATE + 5.0*MMET_CYCLING
  vent_rates$vent_rate[vent_rates$trip_mode%in%c('Walking','Short Walking')] <- BASE_LEVEL_INHALATION_RATE + 5.0*MMET_WALKING
  
  ##RJ rewriting exposure ratio as function of ambient PM2.5, as in Goel et al 2015
  ##!! five fixed parameters: BASE_LEVEL_INHALATION_RATE (10), CLOSED_WINDOW_PM_RATIO (0.5), CLOSED_WINDOW_RATIO (0.5), ROAD_RATIO_MAX (3.216), ROAD_RATIO_SLOPE (0.379)
  ##RJ question for RG: should this function account for PM_TRANS_SHARE?
  on_road_off_road_ratio <- ROAD_RATIO_MAX - ROAD_RATIO_SLOPE*log(conc_pm)
  ##RJ question for RG: why is 'in car' twice better than 'away from road'?
  in_vehicle_ratio <- (1-CLOSED_WINDOW_RATIO)*on_road_off_road_ratio + CLOSED_WINDOW_RATIO*CLOSED_WINDOW_PM_RATIO # averaging over windows open and windows closed
  ratio_by_mode <- rbind(on_road_off_road_ratio,in_vehicle_ratio)
  
  vent_rates$vehicle_ratio_index <- sapply(vent_rates$trip_mode,function(x) ifelse(x%in%c('Walking','Short Walking','Bicycle'),1,2))
  
  trip_set <- left_join(trip_scen_sets,vent_rates,'trip_mode')
  trip_set$on_road_air <- trip_set$trip_duration*trip_set$vent_rate / 60 # L
  scen_index <- match(trip_set$scenario,SCEN)
  scen_pm <- as.numeric(conc_pm[scen_index])
  scen_ratio <- ratio_by_mode[cbind(trip_set$vehicle_ratio_index,scen_index)]
  trip_set$pm_dose <- trip_set$on_road_air * scen_ratio * scen_pm # mg
  
  synth_pop <- SYNTHETIC_POPULATION
  
  ### following code generates final_data
  for (i in 1:length(SCEN)){
    synth_pop[[paste0('pm_conc_',SCEN_SHORT_NAME[i])]] <- conc_pm[i]
    scen_trips <- subset(trip_set,scenario == SCEN[i]&participant_id%in%synth_pop$participant_id)
    
    individual_data <- setDT(scen_trips)[,.(on_road_dur = sum(trip_duration,na.rm=TRUE), 
                                            on_road_pm = sum(pm_dose,na.rm=TRUE), 
                                            air_inhaled = sum(on_road_air,na.rm=TRUE)),by='participant_id']
    
    ## PM2.5 inhalation = total mg inhaled / total volume inhaled
    non_transport_air_inhaled <- (24-individual_data$on_road_dur/60)*BASE_LEVEL_INHALATION_RATE
    pm_conc <- ((non_transport_air_inhaled * as.numeric(conc_pm[i])) + individual_data$on_road_pm)/(non_transport_air_inhaled+individual_data$air_inhaled)
    
    synth_pop[[paste0('pm_conc_',SCEN_SHORT_NAME[i])]][match(individual_data$participant_id,synth_pop$participant_id)] <- pm_conc
  }
  
  #####PM normalise
  ##RJ question for RG: why normalise?
  mean_conc <- rep(0,length(SCEN_SHORT_NAME))
  
  ## calculating means of individual-level concentrations
  for ( i in 1: length(SCEN_SHORT_NAME))
    mean_conc[i] <- mean(synth_pop[[paste0("pm_conc_", SCEN_SHORT_NAME[i])]])
  
  normalise <- as.numeric(conc_pm[1])/as.numeric(mean_conc[1])
  ###Lines which are normalising the concentrations
  
  for (i in 1: length(SCEN_SHORT_NAME))
    synth_pop[[paste0("pm_conc_", SCEN_SHORT_NAME[i])]] <- normalise*synth_pop[[paste0("pm_conc_", SCEN_SHORT_NAME[i])]]
  
  synth_pop$participant_id <- as.integer(synth_pop$participant_id)
  
  list(scenario_pm=conc_pm, pm_conc_pp=synth_pop)
  
}
