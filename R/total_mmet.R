#' @export
total_mmet <- function(pp_summary){
  
  synth_pop <- SYNTHETIC_POPULATION
  
  ##!! maybe we don't need individual distance and can remove it from pp_summary?
  pp_summary2 <- lapply(pp_summary,function(y)y[,sapply(colnames(y),function(x)!grepl('_dist',x)),with=F])
  for(i in 1:length(pp_summary2)) colnames(pp_summary2[[i]]) <- sapply(colnames(pp_summary2[[i]]),function(x)gsub('_dur','',x))
  #rd_pa <- subset(trip_scen_sets,stage_mode%in%c('walking','walk_to_bus','bicycle')&participant_id>0) 
  # Convert baseline's trip duration from mins to hours
  #rd_pa$stage_duration_hrs <- rd_pa$stage_duration / 60 * DAY_TO_WEEK_TRAVEL_SCALAR
  # Get total individual level walking and cycling and sport mmets 
  for (i in 1:length(SCEN)){
    synth_pop[[paste0(SCEN_SHORT_NAME[i],'_mmet')]] <- synth_pop$work_ltpa_marg_met * BACKGROUND_PA_SCALAR
    #scen_trips <- subset(rd_pa,scenario == SCEN[i]&participant_id%in%synth_pop$participant_id)
    
    scen_travel <- subset(pp_summary2[[i]],participant_id%in%synth_pop$participant_id)
    scen_travel$cycling_mmet <- scen_travel$bicycle/60*DAY_TO_WEEK_TRAVEL_SCALAR * MMET_CYCLING
    if('walk_to_bus'%in%names(scen_travel)) scen_travel$walking <- scen_travel$walking+scen_travel$walk_to_bus
    scen_travel$walking_mmet <- scen_travel$walking/60*DAY_TO_WEEK_TRAVEL_SCALAR * MMET_WALKING
    
    #individual_data <- setDT(scen_trips)[,.(cycling_mmet = sum(trip_duration_hrs[trip_mode == 'Bicycle']) * MMET_CYCLING,
    #                                        walking_mmet = sum(trip_duration_hrs[trip_mode %in%c('Walking','Short Walking')]) * MMET_WALKING ),by='participant_id']
    
    individual_data <- scen_travel
    
    part_id <- match(individual_data$participant_id,synth_pop$participant_id)
    synth_pop[[paste0(SCEN_SHORT_NAME[i],'_mmet')]][part_id] <- 
      synth_pop[[paste0(SCEN_SHORT_NAME[i],'_mmet')]][part_id] + individual_data$cycling_mmet + individual_data$walking_mmet
  }
  
  # for (i in 1:length(SCEN)){
  #   synth_pop[[paste0(SCEN_SHORT_NAME[i],'_mmet')]] <- synth_pop$work_ltpa_marg_met * BACKGROUND_PA_SCALAR
  #   scen_trips <- subset(rd_pa,scenario == SCEN[i]&participant_id%in%synth_pop$participant_id)
  #   
  #   individual_data <- setDT(scen_trips)[,.(cycling_mmet_base = sum(stage_duration_hrs[stage_mode == 'bicycle']) * MMET_CYCLING,
  #                                           walking_mmet_base = sum(stage_duration_hrs[stage_mode %in%c('walking','walk_to_bus')]) * MMET_WALKING ),by='participant_id']
  #   
  #   part_id <- match(individual_data$participant_id,synth_pop$participant_id)
  #   synth_pop[[paste0(SCEN_SHORT_NAME[i],'_mmet')]][part_id] <- 
  #     synth_pop[[paste0(SCEN_SHORT_NAME[i],'_mmet')]][part_id] + individual_data$cycling_mmet_base + individual_data$walking_mmet_base
  # }
  # 
  name_indices <- which(colnames(synth_pop)%in%c('participant_id', 'sex', 'age', 'dem_index', paste0(SCEN_SHORT_NAME,'_mmet')))
  mmets <- tbl_df(synth_pop)[,name_indices]
  mmets
  
}
