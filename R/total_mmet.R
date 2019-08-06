#' Calculate total mMETs per person
#' 
#' Calculate total mMETs per person based on PA and active travel
#' 
#' @param trip_scen_sets data frame of all trips from all scenarios
#' 
#' @return total mMETs per week per person
#' 
#' @export
total_mmet <- function(trip_scen_sets){
  
  synth_pop <- setDT(SYNTHETIC_POPULATION)
  rd_pa <- setDT(trip_scen_sets)[trip_scen_sets$stage_mode%in%c('walking','walk_to_pt','bicycle')&trip_scen_sets$participant_id>0,]
  rd_pa$stage_mode[rd_pa$stage_mode=='walk_to_pt'] <- 'walking'
  # Convert baseline's trip duration from mins to hours
  rd_pa$stage_duration_hrs <- rd_pa$stage_duration / 60 * DAY_TO_WEEK_TRAVEL_SCALAR
  # Get total individual level walking and cycling and sport mmets 
  for (i in 1:length(SCEN)){
    synth_pop[[paste0(SCEN_SHORT_NAME[i],'_mmet')]] <- synth_pop$work_ltpa_marg_met * BACKGROUND_PA_SCALAR
    scen_trips <- rd_pa[rd_pa$scenario == SCEN[i],]
    
    individual_data <- scen_trips[,.(cycling_mmet_base = sum(stage_duration_hrs[stage_mode == 'bicycle']),
                                            walking_mmet_base = sum(stage_duration_hrs[stage_mode == 'walking']) ),by='participant_id']
    
    part_id <- match(individual_data$participant_id,synth_pop$participant_id)
    synth_pop[[paste0(SCEN_SHORT_NAME[i],'_mmet')]][part_id] <- 
      synth_pop[[paste0(SCEN_SHORT_NAME[i],'_mmet')]][part_id] + individual_data$cycling_mmet_base * MMET_CYCLING + individual_data$walking_mmet_base * MMET_WALKING
  }
  
  name_indices <- which(colnames(synth_pop)%in%c('participant_id', 'sex', 'age', 'age_cat', paste0(SCEN_SHORT_NAME,'_mmet')))
  mmets <- tbl_df(synth_pop)[,name_indices]
  mmets
  
}
