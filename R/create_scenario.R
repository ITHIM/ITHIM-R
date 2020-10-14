#' Create individual scenario
#' 
#' Function to create individual scenario for the five prespecified scenarios from the baseline for Accra and Sao Paulo (create_all_scenarios)
#' 
#' @param rdr data frame of trips
#' @param scen_name name of scenario
#' @param source_modes which mode(s) to take trips from
#' @param combined_modes whether or not to combine source modes
#' @param target_modes mode to change to
#' @param source_distance_cats which categories to select trips from
#' @param source_trips how many trips to leave, or to take
#' @param target_trips 
#' 
#' @return list of scenarios
#' 
#' @export
create_scenario <- function(rdr, scen_name, source_modes, combined_modes = F, target_modes, source_distance_cats, 
                            source_trips, target_trips){
  ##!! RJ target_modes must be length 1
  if (!combined_modes){
    for (i in 1:length(source_modes)){
      local_source_trips <- sum(rdr$trip_mode == source_modes[i]) - source_trips[i]
      candidate_trips <- filter(rdr,trip_mode == source_modes[i] &
                                  trip_distance_cat %in% source_distance_cats)
      sample_trips <- candidate_trips[sample(1:nrow(candidate_trips),local_source_trips),]
      sample_trips$trip_mode <- target_modes
      sample_trips$stage_mode <- target_modes
      sample_trips$stage_distance <- sample_trips$trip_distance
      sample_trips$stage_duration <- sample_trips$stage_distance / MODE_SPEEDS$speed[MODE_SPEEDS$stage_mode==target_modes] * 60
      # Update selected rows for mode and duration
      ids <- match(sample_trips$trip_id,rdr$trip_id)
      rdr$trip_mode[ids] <- sample_trips$trip_mode
      rdr$trip_distance[ids] <- sample_trips$trip_distance
      rdr$stage_mode[ids] <- sample_trips$stage_mode
      rdr$stage_distance[ids] <- sample_trips$stage_distance
      rdr$stage_duration[ids] <- sample_trips$stage_duration
    } 
    rdr$scenario <- scen_name
    return(rdr)
  } else {
    
    candidate_trips <- filter(rdr,trip_mode %in% source_modes &
                                trip_distance_cat %in% source_distance_cats)
    if (CITY == "bogota_wb"){
      sample_trips <- candidate_trips[sample(1:nrow(candidate_trips),
                                             source_trips, replace = F,
                                             candidate_trips$w),]
    }
    else{
      # browser()
      sample_trips <- candidate_trips[sample(1:nrow(candidate_trips),source_trips),]
    }
    
    sample_trips$trip_mode <- target_modes
    sample_trips$stage_mode <- target_modes
    sample_trips$stage_distance <- sample_trips$trip_distance
    sample_trips$stage_duration <- sample_trips$stage_distance / MODE_SPEEDS$speed[MODE_SPEEDS$stage_mode==target_modes] * 60
    sample_trips$scenario <- scen_name
    
    return(sample_trips)
  }
}
