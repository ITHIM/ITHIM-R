create_scenario <- function(rdr, scen_name, source_modes, combined_modes = F, target_modes, source_distance_cats, 
                            source_trips, target_trips){
  local_source_trips <- list()
  if (!combined_modes){
    for (i in 1:length(source_modes)){
      local_source_trips[i] <- nrow(filter(rdr, trip_mode == source_modes[i])) - source_trips[i]
    }
    local_source_trips <- purrr::flatten_dbl(local_source_trips)
  }
  
  all_samples <- NULL
  
  if (!combined_modes){
    
    for (i in 1:length(source_modes)){
      
      sample <- filter(rdr,
                       trip_mode == source_modes[i] &
                         trip_distance_cat %in% source_distance_cats) %>% sample_n(local_source_trips[i]) %>%
        mutate(
          trip_mode = target_modes[1],
          trip_duration = (trip_distance * 60) / MODE_SPEEDS[MODE_SPEEDS$trip_mode == target_modes[i], ]$speed
        )
      
      
      # Update selected rows for mode and duration
      rdr[rdr$row_id %in% sample$row_id,]$trip_mode <- sample$trip_mode
      rdr[rdr$row_id %in% sample$row_id,]$trip_duration <- sample$trip_duration
      
      
      if (source_modes[i] == 'Bus'){
        # Remove bus associated short walking trips that have been changed to Private Car trips
        rdr <- rdr[!(rdr$trip_mode == 'Short Walking' & rdr$trip_id %in% sample$trip_id),]
      }
      
      
    } 
  }
  
  else {
    
    
    sample <- filter(rdr,
                     trip_mode %in% source_modes &
                       trip_distance_cat %in% source_distance_cats) %>% sample_n(source_trips[1]) %>%
      mutate(
        trip_mode = target_modes[1],
        trip_duration = (trip_distance * 60) / MODE_SPEEDS[MODE_SPEEDS$trip_mode == target_modes[1], ]$speed
      )
    
    sample$scenario <- scen_name
    
    return(sample)
  }
  
  
  rdr$scenario <- scen_name
  
  return(rdr)
  
}
