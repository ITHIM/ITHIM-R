scenario_pm_calculations <- function(scen_dist,rd){
  
  # concentration contributed by non-transport share (remains constant across the scenarios)
  non_transport_pm_conc <- PM_CONC_BASE*(1 - PM_TRANS_SHARE)  
  
  ### Calculating number of scenarios besides the baseline
  trans_emissions <- TRANS_EMISSIONS_ORIGINAL
  ##RJ question for RG: looks like this is using bus travel distance to estimate emissions. Is this right?
  for (i in 2:6)  trans_emissions[[SCEN_SHORT_NAME[i]]] <- trans_emissions$base*c(scen_dist[[SCEN[i]]][c(4,4,3,5,2)]/scen_dist[[SCEN[1]]][c(4,4,3,5,2)],1,1,1)
  #for (i in 1:NSCEN){
  #  trans_emissions[1,p+i] <- trans_emissions$base_emissions[1]*scen_dist[4,n+i]/scen_dist[4,n] ## scenario emissions of 4W1
  #  trans_emissions[2,p+i] <- trans_emissions$base_emissions[2]*scen_dist[4,n+i]/scen_dist[4,n] ## scenario emissions of 4W2 (>2000cc engine size)
  #  trans_emissions[3,p+i] <- trans_emissions$base_emissions[3]*scen_dist[3,n+i]/scen_dist[3,n] ## scenario emissions of 2W
  #  trans_emissions[4,p+i] <- trans_emissions$base_emissions[4]*scen_dist[5,n+i]/scen_dist[5,n] ## scenario emissions of Taxi
  #  trans_emissions[5,p+i] <- trans_emissions$base_emissions[5]*scen_dist[2,n+i]/scen_dist[2,n] ## scenario emissions of bus
  #  trans_emissions[6,p+i] <- trans_emissions$base_emissions[6]*1 ## scenario emissions of trucks
  #  trans_emissions[7,p+i] <- trans_emissions$base_emissions[7]*1 ## scenario emissions of trucks
  #  trans_emissions[8,p+i] <- trans_emissions$base_emissions[8]*1 ## scenario emissions of trucks
  #  names(trans_emissions)[p+i] <-(paste("scen",i,"_emissions", sep=""))
  #}
  
  baseline_sum <- sum(trans_emissions[[SCEN_SHORT_NAME[1]]])
  conc_pm <- c()
  for(i in 1:length(SCEN_SHORT_NAME))
    conc_pm[i] <- non_transport_pm_conc + PM_TRANS_SHARE*PM_CONC_BASE*sum(trans_emissions[[SCEN_SHORT_NAME[i]]])/baseline_sum
  
  ##RJ rewriting ventilation as a function of MMET_CYCLING and MMET_WALKING, loosely following de Sa's SP model.
  vent_rates <- LOOKUP_RATIO_PM
  vent_rates$vent_rate[vent_rates$trip_mode=='Bicycle'] <- 10 + 5.0*MMET_CYCLING
  vent_rates$vent_rate[vent_rates$trip_mode%in%c('Walking','Short Walking')] <- 10 + 5.0*MMET_WALKING
  ### following code generates final_data
  for (i in 1:length(SCEN)){
    scen_index <- SCEN[i]
    rd_scen <- filter(rd, scenario == scen_index)
    rd_scen <- left_join(rd_scen,vent_rates, "trip_mode")  ## attaching the file with in-vehicle ratio and ventilation rate
    rd_scen$on_road_air <- rd_scen$trip_duration*rd_scen$vent_rate / 60
    rd_scen$pm_dose <- rd_scen$on_road_air * rd_scen$ratio * as.numeric(conc_pm[i])
    
    ##RJ need to retain ids
    #rd_scen$participant_id <- as.factor(rd_scen$participant_id)
    
    individual_data <- summarise(group_by(rd_scen,participant_id),on_road_dur = sum(trip_duration,na.rm=TRUE), 
                                 on_road_pm = sum(pm_dose,na.rm=TRUE), 
                                 air_inhaled = sum(on_road_air,na.rm=TRUE))
    ##RJ question for RG: can you write, in words or equations, what this calculation is doing?
    non_transport_pm_inhaled <- (24-individual_data$on_road_dur/60)*10
    individual_data$pm_conc <- ((non_transport_pm_inhaled * as.numeric(conc_pm[i])) + individual_data$on_road_pm)/(non_transport_pm_inhaled+individual_data$air_inhaled)
    individual_data <- subset(individual_data, select=c("participant_id", "pm_conc"))
    names(individual_data)[2] <- paste0('pm_conc_',SCEN_SHORT_NAME[i])
    
    if (i == 1 ){
      final_data <- individual_data 
    }else{
      final_data <- left_join(final_data,individual_data,by="participant_id")
    }
  }
  
  #####PM normalise
  
  mean_conc <- rep(0,length(SCEN_SHORT_NAME))
  
  ## calculating means of individual-level concentrations
  for ( i in 1: length(SCEN_SHORT_NAME))
    mean_conc[i] <- mean(final_data[[paste0("pm_conc_", SCEN_SHORT_NAME[i])]])
  
  normalise <- as.numeric(conc_pm[1])/as.numeric(mean_conc[1])
  ###Lines which are normalising the concentrations
  
  for (i in 1: length(SCEN_SHORT_NAME))
    final_data[[paste0("pm_conc_", SCEN_SHORT_NAME[i])]] <- normalise*final_data[[paste0("pm_conc_", SCEN_SHORT_NAME[i])]]
  
  list(scenario_pm=conc_pm, pm_conc_pp=as.data.frame(final_data))
  
}
