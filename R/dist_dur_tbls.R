#' @export
dist_dur_tbls <- function(pp_summary){

  durations <- list()
  
  ## calculate all distances & durations for each scenario
  for (i in 1:length(SCEN)){
    
    scen_travel <- pp_summary[[SCEN_SHORT_NAME[i]]][,colnames(pp_summary[[SCEN_SHORT_NAME[i]]])%in%paste0(VEHICLE_INVENTORY$stage_mode,'_dur')]
    total_travel <- colSums(scen_travel)
    
    # add Short Walking, if Short Walking has been added
    if("walk_to_pt"%in%names(total_travel)){
      total_travel[['walking_dur']] <-  total_travel[['walking_dur']] +  total_travel[['walk_to_pt_dur']]
    }
    
    durations[[i]] <- total_travel
  }
  dur <- do.call('cbind',durations)
  colnames(dur) <- SCEN
  rownames(dur) <- sapply(rownames(dur),function(x)strsplit(x,'_dur')[[1]][1])
  dur <- dur[rownames(dur)!='walk_to_bus',]
  
<<<<<<< HEAD
  mode_indices <- match(rownames(dur),VEHICLE_INVENTORY$stage_mode)
  mode_speeds <- VEHICLE_INVENTORY$speed[mode_indices]
  mode_speeds[is.na(mode_speeds)] <- 0
  dist <- dur * matrix(rep(mode_speeds,NSCEN+1),ncol=NSCEN+1) / 60
=======
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
  #dist <- filter(local_dist, stage_mode != 'walk_to_pt')
  #dur <- filter(local_dur, stage_mode != 'walk_to_pt')
  dist <- local_dist[local_dist$stage_mode != 'walk_to_pt',]
  dur <- local_dur[local_dur$stage_mode != 'walk_to_pt',]
  
  dist$stage_mode <- as.character(dist$stage_mode)
  dur$stage_mode <- as.character(dur$stage_mode)
>>>>>>> master
  
  ## bus travel is linear in bus passenger travel
  bus_passenger_row <- which(rownames(dur)=='bus')
  dist <- rbind(dist,dist[bus_passenger_row,] * BUS_TO_PASSENGER_RATIO)
  dur <- rbind(dur,dur[bus_passenger_row,] * BUS_TO_PASSENGER_RATIO) 
  rownames(dist)[nrow(dist)] <- 'bus_driver'
  rownames(dur)[nrow(dur)] <- 'bus_driver'
  ## truck travel is linear in car travel
  if(ADD_TRUCK_DRIVERS){
    car_row <- which(rownames(dur)=='car')
    dist <- rbind(dist,dist[car_row,] * TRUCK_TO_CAR_RATIO)
    dur <- rbind(dur,dur[car_row,] * TRUCK_TO_CAR_RATIO) 
    rownames(dist)[nrow(dist)] <- 'truck'
    rownames(dur)[nrow(dur)] <- 'truck'
  }
  
  return(list(dist=dist,dur=dur))
}
  

  
