#' Calculate total mMETs per person
#' 
#' Calculate total mMETs per person in the synthetic population based on non-travel PA and active travel for each scenario
#' 
#' This function performs the following steps:
#' - extract all people from the trip set with an active travel (walk or cycle) stage mode (non ghost trips only)
#' - calculate the weekly time spent on active travel
#' - for each scenario:
#'    - scale the non-travel MMET value for the people in the synthetic population by the 
#'      BACKGROUND_PA_SCALAR to adjust for any biases in the PA data
#'    - calculate the total cycling and walking MMET values for each relevant person in the trip set
#'    - add the active travel MMET to the non-travel MMET values for each person in the synthetic population
#' - create one dataframe with total MMET for all people in the synthetic population for each scenario
#' 
#' @param trip_scen_sets data frame of all trips from all scenarios
#' 
#' @return total mMETs per week per person in each scenario
#' 
#' @export
total_mmet <- function(trip_scen_sets){
  
  synth_pop <- setDT(SYNTHETIC_POPULATION)
  
  # extract all people from the trip set with an active travel (walk or cycle) stage mode
  rd_pa <- setDT(trip_scen_sets)[trip_scen_sets$stage_mode%in%c('pedestrian','walk_to_pt','cycle')&trip_scen_sets$participant_id>0,]
  
  # rename any 'walk_to_pt' stages as 'pedestrian' stage
  rd_pa$stage_mode[rd_pa$stage_mode=='walk_to_pt'] <- 'pedestrian'
  
  # Convert baseline's trip duration from mins to hours, scale to entire week
  #rd_pa$stage_duration_hrs <- rd_pa$stage_duration / 60 * DAY_TO_WEEK_TRAVEL_SCALAR # day_to_week scalar has already been applied in the get_synthetic_from_trips.R function
  rd_pa$stage_duration_hrs <- rd_pa$stage_duration / 60 * 7
  
  # Get total individual level pedestrian and cycling and non-travel mmets 
  for (i in 1:length(SCEN)){ # loop through all scenarios
    # scale non-travel PA by BACKGROUND_PA_SCALAR which adjusts for biases in the data
    synth_pop[[paste0(SCEN_SHORT_NAME[i],'_mmet')]] <- synth_pop$work_ltpa_marg_met * BACKGROUND_PA_SCALAR
    
    scen_trips <- rd_pa[rd_pa$scenario == SCEN[i],] # extract trips from trip data for given scenario
    # calculate total cycle and total walking time per person
    individual_data <- scen_trips[,.(cycling_mmet_base = sum(stage_duration_hrs[stage_mode == 'cycle']), 
                                            walking_mmet_base = sum(stage_duration_hrs[stage_mode == 'pedestrian']) ),by='participant_id']
    # calculate total travel MMET for relevant people in synthetic population and add to non_travel MMET
    part_id <- match(individual_data$participant_id,synth_pop$participant_id)
    synth_pop[[paste0(SCEN_SHORT_NAME[i],'_mmet')]][part_id] <- 
      synth_pop[[paste0(SCEN_SHORT_NAME[i],'_mmet')]][part_id] + individual_data$cycling_mmet_base * MMET_CYCLING + individual_data$walking_mmet_base * MMET_WALKING
  }
  
  # create dataframe containing the total MMET (sum of travel and non_travel MMET) for each person in the synthetic population and for each scenario
  name_indices <- which(colnames(synth_pop)%in%c('participant_id', 'sex', 'age', 'age_cat', paste0(SCEN_SHORT_NAME,'_mmet')))
  mmets <- as.data.frame(synth_pop)[,name_indices]
  mmets
  
}
