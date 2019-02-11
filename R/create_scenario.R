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
      sample_trips$trip_duration <- (sample_trips$trip_distance * 60) / VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$trip_mode == target_modes]
      # Update selected rows for mode and duration
      rdr$trip_mode[match(sample_trips$row_id,rdr$row_id)] <- sample_trips$trip_mode
      rdr$trip_duration[match(sample_trips$row_id,rdr$row_id)] <- sample_trips$trip_duration
    } 
    rdr$scenario <- scen_name
    return(rdr)
  } else {
    
    candidate_trips <- filter(rdr,trip_mode %in% source_modes &
                                trip_distance_cat %in% source_distance_cats)
    sample_trips <- candidate_trips[sample(1:nrow(candidate_trips),source_trips),]
    sample_trips$trip_mode <- target_modes
    sample_trips$trip_duration <- (sample_trips$trip_distance * 60) / VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$trip_mode == target_modes]
    sample_trips$scenario <- scen_name
    
    return(sample_trips)
  }
}
