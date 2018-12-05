total_mmet <- function(trip_scen_sets){
  
  synth_pop <- SYNTHETIC_POPULATION
  rd_pa <- subset(trip_scen_sets,trip_mode%in%c('Walking','Short Walking','Bicycle')&participant_id>0) 
  # Convert baseline's trip duration from mins to hours
  rd_pa$trip_duration_hrs <- rd_pa$trip_duration / 60
  # Get total individual level walking and cycling and sport mmets 
  for (i in 1:length(SCEN)){
    synth_pop[[paste0(SCEN_SHORT_NAME[i],'_mmet')]] <- synth_pop$work_ltpa_marg_met * BACKGROUND_PA_SCALAR
    scen_trips <- subset(rd_pa,scenario == SCEN[i]&participant_id%in%synth_pop$participant_id)
    
    individual_data <- setDT(scen_trips)[,.(cycling_mmet_base = sum(trip_duration_hrs[trip_mode == 'Bicycle']) * MMET_CYCLING,
                                            walking_mmet_base = sum(trip_duration_hrs[trip_mode %in%c('Walking','Short Walking')]) * MMET_WALKING ),by='participant_id']
    
    part_id <- match(individual_data$participant_id,synth_pop$participant_id)
    synth_pop[[paste0(SCEN_SHORT_NAME[i],'_mmet')]][part_id] <- 
      synth_pop[[paste0(SCEN_SHORT_NAME[i],'_mmet')]][part_id] + individual_data$cycling_mmet_base + individual_data$walking_mmet_base
  }
  
  name_indices <- which(colnames(synth_pop)%in%c('participant_id', 'sex', 'age', 'age_cat', paste0(SCEN_SHORT_NAME,'_mmet')))
  mmets <- tbl_df(synth_pop)[,name_indices]
  mmets
  
}