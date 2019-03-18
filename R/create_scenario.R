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
      rdr$trip_mode[match(sample_trips$trip_id,rdr$trip_id)] <- sample_trips$trip_mode
      rdr$trip_distance[match(sample_trips$trip_id,rdr$trip_id)] <- sample_trips$trip_distance
      rdr$stage_mode[match(sample_trips$trip_id,rdr$trip_id)] <- sample_trips$stage_mode
      rdr$stage_distance[match(sample_trips$trip_id,rdr$trip_id)] <- sample_trips$stage_distance
      rdr$stage_duration[match(sample_trips$trip_id,rdr$trip_id)] <- sample_trips$stage_duration
    } 
    rdr$scenario <- scen_name
    return(rdr)
  } else {
    
    candidate_trips <- filter(rdr,trip_mode %in% source_modes &
                                trip_distance_cat %in% source_distance_cats)
    sample_trips <- candidate_trips[sample(1:nrow(candidate_trips),source_trips),]
    sample_trips$trip_mode <- target_modes
    sample_trips$stage_mode <- target_modes
    sample_trips$stage_distance <- sample_trips$trip_distance
    sample_trips$stage_duration <- sample_trips$stage_distance / MODE_SPEEDS$speed[MODE_SPEEDS$stage_mode==target_modes] * 60
    sample_trips$scenario <- scen_name
    
    return(sample_trips)
  }
}
