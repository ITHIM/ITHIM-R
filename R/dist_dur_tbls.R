#' @export
dist_dur_tbls <- function(pp_summary){

  durations <- list()
  
  ## calculate all distances & durations for each scenario
  for (i in 1:length(SCEN)){
    
    scen_travel <- pp_summary[[SCEN_SHORT_NAME[i]]][,colnames(pp_summary[[SCEN_SHORT_NAME[i]]])%in%paste0(VEHICLE_INVENTORY$stage_mode,'_dur')]
    total_travel <- colSums(scen_travel)
    
    # add Short Walking, if Short Walking has been added
    if(ADD_WALK_TO_BUS_TRIPS){
      total_travel[['walking_dur']] <-  total_travel[['walking_dur']] +  total_travel[['walk_to_bus_dur']]
    }
    
    durations[[i]] <- total_travel
  }
  dur <- do.call('cbind',durations)
  colnames(dur) <- SCEN
  rownames(dur) <- sapply(rownames(dur),function(x)strsplit(x,'_dur')[[1]][1])
  dur <- dur[rownames(dur)!='walk_to_bus',]
  
  mode_indices <- match(rownames(dur),VEHICLE_INVENTORY$stage_mode)
  mode_speeds <- VEHICLE_INVENTORY$speed[mode_indices]
  mode_speeds[is.na(mode_speeds)] <- 0
  dist <- dur * matrix(rep(mode_speeds,NSCEN+1),ncol=NSCEN+1) / 60
  
  ## bus travel is linear in bus passenger travel
  if('bus_driver'%in%rownames(dur)){
    bus_driver_row <- which(rownames(dur)=='bus_driver')
    bus_passenger_row <- which(rownames(dur)=='bus')
    base_col <- which(colnames(dist)=='Baseline')
    dist[bus_driver_row,colnames(dist)%in%SCEN] <- as.numeric(dist[bus_driver_row,base_col] / dist[bus_passenger_row,base_col]) * dist[bus_passenger_row,colnames(dist)%in%SCEN] 
    dur[bus_driver_row,colnames(dur)%in%SCEN] <- as.numeric(dur[bus_driver_row,base_col] / dur[bus_passenger_row,base_col]) * dur[bus_passenger_row,colnames(dur)%in%SCEN] 
  }
  
  return(list(dist=dist,dur=dur))
}
  

  
