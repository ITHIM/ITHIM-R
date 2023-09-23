#' Get distances and duration summaries by mode
#' 
#' Summaries of total distances and durations spent travelling per mode and per scenario, for the synthetic population
#' 
#' 
#' The function performs the following steps:
#' 
#' \itemize{
#' \item loop through the scenarios (incl Baseline):
#'  \itemize{
#'   \item using the trip data, sum across the distances by stage mode to get total
#'     distance by mode for each scenario for the synthetic population
#'     
#'   \item sum across the duration by stage mode to get total duration by mode for each scenario
#'   
#'   \item if 'walk_to_pt' stages exist, add them to the pedestrian stages for both 
#'     duration and distance
#'   }  
#' \item create one dataframe containing the total distances by mode for each scenario
#' 
#' \item create one dataframe containing the total duration by mode for each scenario
#' 
#' \item remove any 'walk_to_pt' stages as they have been added to the pedestrian stages
#' 
#' \item update the bus and car driver distances and duration in the scenarios using 
#'   the ratio of bus/car to bus_driver/car_driver in the baseline scenario (this
#'   is redundant for the GLOBAL, BOGOTA, AFRICA_INDIA and LATAM scenario definitions
#'   as bus and car driver distances are already updated during the scenario creation)
#' }
#' 
#' @param trip_scen_sets list of synthetic trip sets for each scenario including the baseline
#' 
#' @return list of table of (total) distances and durations per mode per scenario
#' 
#' @export



dist_dur_tbls <- function(trip_scen_sets){
  
  bs <- trip_scen_sets
  trip_scen_sets <- NULL
  
  stage_modes <- unique(bs$stage_mode) # find the unique stage modes
  
  ## calculate all distances & durations for each scenario
  l_dist <-  l_dur <- list()
  for (i in SCEN){ # loop through scenarios
    
    # get scenario trips
    local <- bs[bs$scenario == i,]
    local_dist <- local[,.(sum_dist=sum(stage_distance)),by='stage_mode'] # sum across distances by stage mode
    local_dur <- local[,.(sum_dur=sum(stage_duration)),by='stage_mode'] # sum across duration by stage mode
    
    
    # add walk_to_pt to pedestrian, if walk_to_pt has been added
    if("walk_to_pt"%in%local_dist$stage_mode){
      local_dist$sum_dist[local_dist$stage_mode == "pedestrian"] <-
        local_dist$sum_dist[local_dist$stage_mode == "pedestrian"] +
        local_dist$sum_dist[local_dist$stage_mode == "walk_to_pt"]
      local_dur$sum_dur[local_dur$stage_mode == "pedestrian"] <-
        local_dur$sum_dur[local_dur$stage_mode == "pedestrian"] +
        local_dur$sum_dur[local_dur$stage_mode == "walk_to_pt"]
    }
    
    # store results
    colnames(local_dist)[2] <- i
    l_dist[[i]] <- local_dist
    colnames(local_dur)[2] <- i
    l_dur[[i]] <- local_dur
  }
  bs <- NULL
  
  ## join distances & durations
  for (i in names(l_dist)){
    if (i == "baseline"){
      local_dist <- l_dist[[i]]
      local_dur <- l_dur[[i]]
    }else{
      local_dist <- local_dist[l_dist[[i]],on="stage_mode"]  #<- left_join(local_dist, , by = "stage_mode")
      local_dur <- local_dur[l_dur[[i]],on="stage_mode"]  # <- left_join(local_dur, l_dur[[i]], by = "stage_mode")
    }
  }
  
  # Remove walk to pt distances and durations as they were added to the pedestrian stages
  dist <- as.data.frame(local_dist[local_dist$stage_mode != 'walk_to_pt',])
  dur <- as.data.frame(local_dur[local_dur$stage_mode != 'walk_to_pt',])
  
  dist$stage_mode <- as.character(dist$stage_mode)
  dur$stage_mode <- as.character(dur$stage_mode)
  
  
  # Note the following lines for updating bus driver and car driver distances are 
  # redundant with the newer scenario definitions such as e.g. create_global_scenarios.R
  # as the bus driver and car driver distances are already 
  # re-defined within the scenario creation. 
  
  # update the bus_driver distances and duration based on the ratio of 
  # bus_driver to bus distance in the baseline scenario
  # bus travel is linear in bus passenger travel
  bus_passenger_row <- which(dist$stage_mode=='bus')
  if('bus_driver'%in%dist$stage_mode){
    bus_driver_row <- which(dist$stage_mode=='bus_driver')
    base_col <- which(colnames(dist)=='baseline')
    dist[bus_driver_row,colnames(dist)%in%SCEN] <- as.numeric(dist[bus_driver_row,base_col] / dist[bus_passenger_row,base_col]) * dist[bus_passenger_row,colnames(dist)%in%SCEN]
    dur[bus_driver_row,colnames(dur)%in%SCEN] <- as.numeric(dur[bus_driver_row,base_col] / dur[bus_passenger_row,base_col]) * dur[bus_passenger_row,colnames(dur)%in%SCEN]
  }else{
    dist <- rbind(dist,dist[bus_passenger_row,])
    dist[nrow(dist),1] <- 'bus_driver'
    dur <- rbind(dur,dur[bus_passenger_row,])
    dur[nrow(dur),1] <- 'bus_driver'
  }

  # update the car_driver distances and duration based on the ratio of 
  # car_driver to car distance in the baseline scenario
  # car travel is linear with regards to number of people in car
  car_passenger_row <- which(dist$stage_mode=='car')
  if('car_driver'%in%dist$stage_mode){
    car_driver_row <- which(dist$stage_mode=='car_driver')
    base_col <- which(colnames(dist)=='baseline')
    dist[car_driver_row,colnames(dist)%in%SCEN] <- as.numeric(dist[car_driver_row,base_col] / dist[car_passenger_row,base_col]) * dist[car_passenger_row,colnames(dist)%in%SCEN]
    dur[car_driver_row,colnames(dur)%in%SCEN] <- as.numeric(dur[car_driver_row,base_col] / dur[car_passenger_row,base_col]) * dur[car_passenger_row,colnames(dur)%in%SCEN]
  }else{
    dist <- rbind(dist,dist[car_passenger_row,])
    dist[nrow(dist),1] <- 'car_driver'
    dur <- rbind(dur,dur[car_passenger_row,])
    dur[nrow(dur),1] <- 'car_driver'
  }

 
  return(list(dist=dist,dur=dur))
}
