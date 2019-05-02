#' @export
scenario_pm_calculations <- function(dist,trip_scen_sets){
  
  # concentration contributed by non-transport share (remains constant across the scenarios)
  non_transport_pm_conc <- PM_CONC_BASE*(1 - PM_TRANS_SHARE)  
  
  ## adding in travel not covered in the synthetic trip set, based on distances travelled relative to car, set in VEHICLE_INVENTORY
  emission_dist <- dist
  
  ## get emission factor by dividing inventory by baseline distance. (We don't need to scale to a whole year, as we are just scaling the background concentration.)
  ordered_efs <- VEHICLE_INVENTORY$emission_inventory[match(emission_dist$stage_mode,VEHICLE_INVENTORY$stage_mode)]/emission_dist$Baseline
  ## get new emission by multiplying emission factor by scenario distance.
  trans_emissions <- emission_dist[,0:NSCEN+2]*t(repmat(ordered_efs,NSCEN+1,1))
  ## augment with travel emission contributions that aren't included in distance calculation
  for(mode_type in which(!VEHICLE_INVENTORY$stage_mode%in%emission_dist$stage_mode))
    trans_emissions[nrow(trans_emissions)+1,] <- VEHICLE_INVENTORY$emission_inventory[mode_type]
  
  ## scenario travel pm2.5 calculated as relative to the baseline
  baseline_sum <- sum(trans_emissions[[SCEN[1]]])
  conc_pm <- c()
  ## in this sum, the non-transport pm is constant; the transport emissions scale the transport contribution (PM_TRANS_SHARE) to the base level (PM_CONC_BASE)
  for(i in 1:length(SCEN_SHORT_NAME))
    conc_pm[i] <- non_transport_pm_conc + PM_TRANS_SHARE*PM_CONC_BASE*sum(trans_emissions[[SCEN[i]]])/baseline_sum
  
  ##RJ rewriting ventilation as a function of MMET_CYCLING and MMET_WALKING, loosely following de Sa's SP model.
  vent_rates <- data.frame(stage_mode=VEHICLE_INVENTORY$stage_mode,stringsAsFactors = F) 
  vent_rates$vent_rate <-1  # L / min
  vent_rates$vent_rate[vent_rates$stage_mode=='bicycle'] <- (BASE_LEVEL_INHALATION_RATE + 5.0*MMET_CYCLING)/BASE_LEVEL_INHALATION_RATE
  vent_rates$vent_rate[vent_rates$stage_mode%in%c('walking','walk_to_pt')] <- (BASE_LEVEL_INHALATION_RATE + 5.0*MMET_WALKING)/BASE_LEVEL_INHALATION_RATE
  
  ##RJ rewriting exposure ratio as function of ambient PM2.5, as in Goel et al 2015
  ##!! five fixed parameters: BASE_LEVEL_INHALATION_RATE (10), CLOSED_WINDOW_PM_RATIO (0.5), CLOSED_WINDOW_RATIO (0.5), ROAD_RATIO_MAX (3.216), ROAD_RATIO_SLOPE (0.379)
  ##RJ question for RG: should this function account for PM_TRANS_SHARE?
  on_road_off_road_ratio <- ROAD_RATIO_MAX - ROAD_RATIO_SLOPE*log(conc_pm)
  ##RJ question for RG: why is 'in car' twice better than 'away from road'?
  # averaging over windows open and windows closed
  in_vehicle_ratio <- (1-CLOSED_WINDOW_RATIO)*on_road_off_road_ratio + CLOSED_WINDOW_RATIO*CLOSED_WINDOW_PM_RATIO 
  # subway ratio is a constant
  subway_ratio <- rep(SUBWAY_PM_RATIO,length(conc_pm))
  # open vehicles experience the ``on_road_off_road_ratio'', and closed vehicles experience the ``in_vehicle_ratio''
  ratio_by_mode <- rbind(on_road_off_road_ratio,in_vehicle_ratio,subway_ratio)
  # assign rates according to the order of the ratio_by_mode array: 1 is open vehicle, 2 is closed vehicle, 3 is subway
  open_vehicles <- c('walking','walk_to_pt','bicycle','motorcycle','auto_rickshaw','shared_auto','cycle_rickshaw')
  rail_vehicles <- c('subway','rail')
  vent_rates$vehicle_ratio_index <- sapply(vent_rates$stage_mode,function(x) ifelse(x%in%rail_vehicles,3,ifelse(x%in%open_vehicles,1,2)))
  
  trip_set <- left_join(trip_scen_sets,vent_rates,'stage_mode')
  # litres of air inhaled are the product of the ventilation rate and the time (hours/60) spent travelling by that mode
  trip_set$on_road_air <- trip_set$stage_duration*trip_set$vent_rate / (60*24) # L
  # get indices for quick matching of values
  scen_index <- match(trip_set$scenario,SCEN)
  # ordered pm values
  scen_pm <- as.numeric(conc_pm[scen_index])
  # ordered ratios based on scenario and open/closed mode
  scen_ratio <- ratio_by_mode[cbind(trip_set$vehicle_ratio_index,scen_index)]
  # pm dose in mg as the product of the air inhaled, the background pm, and the exposure ratio
  trip_set$pm_dose <- trip_set$on_road_air * scen_pm * scen_ratio # mg
  
  # prepare individual-level dataset
  synth_pop <- SYNTHETIC_POPULATION
  # compute individual-level pm scenario by scenario
  for (i in 1:length(SCEN)){
    # initialise to background. This means persons who undertake zero travel get this value.
    synth_pop[[paste0('pm_conc_',SCEN_SHORT_NAME[i])]] <- conc_pm[i]
    # take trips from this scenario, and exclude trips by individuals not in the synthetic population (which might be truck trips)
    scen_trips <- subset(trip_set,scenario == SCEN[i]&participant_id%in%synth_pop$participant_id)
    # summarise individual-level time on road, pm inhaled, and air inhaled
    individual_data <- setDT(scen_trips)[,.(on_road_dur = sum(stage_duration,na.rm=TRUE), 
                                            on_road_pm = sum(pm_dose,na.rm=TRUE)),by='participant_id']#, 
                                            #air_inhaled = sum(on_road_air,na.rm=TRUE)),by='participant_id']
    
    # calculate non-travel air inhalation
    non_transport_air_inhaled <- (24-individual_data$on_road_dur/60)*1/24
    # concentration of pm inhaled = total pm inhaled / total air inhaled
    pm_conc <- ((non_transport_air_inhaled * as.numeric(conc_pm[i])) + individual_data$on_road_pm)#/(non_transport_air_inhaled+individual_data$air_inhaled)
    # match individual ids to set per person pm exposure
    synth_pop[[paste0('pm_conc_',SCEN_SHORT_NAME[i])]][match(individual_data$participant_id,synth_pop$participant_id)] <- pm_conc
  }
  
  #####PM normalise
  ##currently not normalising
  mean_conc <- rep(0,length(SCEN_SHORT_NAME))
  
  ## calculating means of individual-level concentrations
  mean_conc <- mean(synth_pop[[paste0("pm_conc_", SCEN_SHORT_NAME[1])]])
  
  normalise <- as.numeric(conc_pm[1])/as.numeric(mean_conc)
  ###Lines which are normalising the concentrations
  
  for (i in 1: length(SCEN_SHORT_NAME))
    ## Rahul made changes here/./-- no normalisation
    synth_pop[[paste0("pm_conc_", SCEN_SHORT_NAME[i])]] <- synth_pop[[paste0("pm_conc_", SCEN_SHORT_NAME[i])]]
  
    #synth_pop[[paste0("pm_conc_", SCEN_SHORT_NAME[i])]] <- normalise*synth_pop[[paste0("pm_conc_", SCEN_SHORT_NAME[i])]]
  
  synth_pop$participant_id <- as.integer(synth_pop$participant_id)
  
  list(scenario_pm=conc_pm, pm_conc_pp=synth_pop)
  
}
