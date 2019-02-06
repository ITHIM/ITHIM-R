#' @export
dist_dur_tbls <- function(trip_scen_sets){
  
  bs <- trip_scen_sets
  
  ## calculate all distances & durations for each scenario
  l_dist <-  l_dur <- list()
  for (i in 1:length(SCEN)){
    # get scenario trips
    local <- group_by(filter(bs,scenario == SCEN[i]), trip_mode)
    
    # summarise total distances & durations
    local_dist <- summarise(local, sum_dist = sum(trip_distance))
    local_dur <- summarise(local, sum_dur = sum(trip_duration))
    
    # add Short Walking, if Short Walking has been added
    if(ADD_WALK_TO_BUS_TRIPS){
      local_dist$sum_dist[local_dist$trip_mode == "Walking"] <- 
        local_dist$sum_dist[local_dist$trip_mode == "Walking"] + 
        local_dist$sum_dist[local_dist$trip_mode == "Short Walking"]
      local_dur$sum_dur[local_dur$trip_mode == "Walking"] <- 
        local_dur$sum_dur[local_dur$trip_mode == "Walking"] + 
        local_dur$sum_dur[local_dur$trip_mode == "Short Walking"]
    }
    
    # store results
    colnames(local_dist)[2] <- SCEN[i]
    l_dist[[i]] <- local_dist
    colnames(local_dur)[2] <- SCEN[i]
    l_dur[[i]] <- local_dur
  }
  
  ## join distances & durations
  for (i in 1:length(l_dist)){
    if (i == 1){
      local_dist <- l_dist[[i]]
      local_dur <- l_dur[[i]]
    }else{
      local_dist <- left_join(local_dist, l_dist[[i]], by = "trip_mode")
      local_dur <- left_join(local_dur, l_dur[[i]], by = "trip_mode")
    }
  }
  
  # Remove short walking
  dist <- filter(local_dist, trip_mode != 'Short Walking')
  dur <- filter(local_dur, trip_mode != 'Short Walking')
  
  return(list(dist=dist,dur=dur))
}