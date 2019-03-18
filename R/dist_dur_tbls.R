#' @export
dist_dur_tbls <- function(trip_scen_sets){
  
  bs <- trip_scen_sets
  
  ## calculate all distances & durations for each scenario
  l_dist <-  l_dur <- list()
  for (i in 1:length(SCEN)){
    # get scenario trips
    local <- group_by(filter(bs,scenario == SCEN[i]), stage_mode)
    
    # summarise total distances & durations
    local_dist <- summarise(local, sum_dist = sum(stage_distance))
    local_dur <- summarise(local, sum_dur = sum(stage_duration))
    
    # add walk_to_bus, if walk_to_bus has been added
    if(ADD_WALK_TO_BUS_TRIPS){
      local_dist$sum_dist[local_dist$stage_mode == "walking"] <- 
        local_dist$sum_dist[local_dist$stage_mode == "walking"] + 
        local_dist$sum_dist[local_dist$stage_mode == "walk_to_bus"]
      local_dur$sum_dur[local_dur$stage_mode == "walking"] <- 
        local_dur$sum_dur[local_dur$stage_mode == "walking"] + 
        local_dur$sum_dur[local_dur$stage_mode == "walk_to_bus"]
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
      local_dist <- left_join(local_dist, l_dist[[i]], by = "stage_mode")
      local_dur <- left_join(local_dur, l_dur[[i]], by = "stage_mode")
    }
  }
  
  # Remove short walking
  dist <- filter(local_dist, stage_mode != 'walk_to_bus')
  dur <- filter(local_dur, stage_mode != 'walk_to_bus')
  
  ## bus travel is linear in bus passenger travel
  if('bus_driver'%in%dist$stage_mode){
    bus_driver_row <- which(dist$stage_mode=='bus_driver')
    bus_passenger_row <- which(dist$stage_mode=='bus')
    base_col <- which(colnames(dist)=='Baseline')
    dist[bus_driver_row,colnames(dist)%in%SCEN] <- as.numeric(dist[bus_driver_row,base_col] / dist[bus_passenger_row,base_col]) * dist[bus_passenger_row,colnames(dist)%in%SCEN] 
    dur[bus_driver_row,colnames(dur)%in%SCEN] <- as.numeric(dur[bus_driver_row,base_col] / dur[bus_passenger_row,base_col]) * dur[bus_passenger_row,colnames(dur)%in%SCEN] 
  }
  
  return(list(dist=dist,dur=dur))
}
