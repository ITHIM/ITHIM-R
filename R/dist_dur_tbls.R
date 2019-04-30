#' @export
dist_dur_tbls <- function(trip_scen_sets){
  
  bs <- trip_scen_sets
  
  stage_modes <- unique(bs$stage_mode)
  
  ## calculate all distances & durations for each scenario
  l_dist <-  l_dur <- list()
  for (i in 1:length(SCEN)){
    # get scenario trips
    local <- group_by(filter(bs,scenario == SCEN[i]), stage_mode)
    
    # summarise total distances & durations
    local_dist <- summarise(local, sum_dist = sum(stage_distance))
    local_dur <- summarise(local, sum_dur = sum(stage_duration))
    
    local <- subset(bs,scenario==SCEN[i])
    local_dist <- data.frame(stage_mode=stage_modes,sum_dist=sapply(stage_modes,function(x)sum(subset(local,stage_mode==x)$stage_distance)))
    local_dur <- data.frame(stage_mode=stage_modes,sum_dur=sapply(stage_modes,function(x)sum(subset(local,stage_mode==x)$stage_duration)))
    
    # add walk_to_bus, if walk_to_bus has been added
    if("walk_to_bus"%in%local_dist$stage_mode){
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
  #dist <- filter(local_dist, stage_mode != 'walk_to_bus')
  #dur <- filter(local_dur, stage_mode != 'walk_to_bus')
  dist <- local_dist[local_dist$stage_mode != 'walk_to_bus',]
  dur <- local_dur[local_dur$stage_mode != 'walk_to_bus',]
  
  dist$stage_mode <- as.character(dist$stage_mode)
  dur$stage_mode <- as.character(dur$stage_mode)
  
  ## bus travel is linear in bus passenger travel
  bus_passenger_row <- which(dist$stage_mode=='bus')
  if('bus_driver'%in%dist$stage_mode){
    bus_driver_row <- which(dist$stage_mode=='bus_driver')
    base_col <- which(colnames(dist)=='Baseline')
    dist[bus_driver_row,colnames(dist)%in%SCEN] <- as.numeric(dist[bus_driver_row,base_col] / dist[bus_passenger_row,base_col]) * dist[bus_passenger_row,colnames(dist)%in%SCEN] 
    dur[bus_driver_row,colnames(dur)%in%SCEN] <- as.numeric(dur[bus_driver_row,base_col] / dur[bus_passenger_row,base_col]) * dur[bus_passenger_row,colnames(dur)%in%SCEN] 
  }else{
    dist <- rbind(dist,dist[bus_passenger_row,])
    dist[nrow(dist),1] <- 'bus_driver'
    dur <- rbind(dur,dur[bus_passenger_row,])
    dur[nrow(dur),1] <- 'bus_driver'
  }
  
  return(list(dist=dist,dur=dur))
}
