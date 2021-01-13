#' Add walk to PT
#' 
#' Adds a short walk stage to any PT trip if required.
#' Combines list of scenarios into one data frame
#' 
#' @param trip_set list of data frames, trips from all scenarios
#' 
#' @return data frame, all trips from all scenarios
#' 
#' @export
walk_to_pt_and_combine_scen <- function(SYNTHETIC_TRIPS){
  rd_list <- list()
  for(i in 1:length(SYNTHETIC_TRIPS)) rd_list[[i]] <- setDT(SYNTHETIC_TRIPS[[i]])
  SYNTHETIC_TRIPS <- NULL
  
  ## pt = public transport
  pt_modes <- c('bus')#,'minibus','subway','rail')
  
  ##!! this function and add_walk_trips need some attention
  
  if(ADD_WALK_TO_BUS_TRIPS)
    for(i in 1:length(rd_list)){
      
      
      rd_list[[i]] <- rd_list[[i]] %>% mutate(id = row_number())
      
      if (!any(names(rd_list[[i]]) %in% 'trip_mode')){
        print(CITY)
        break
      }
      # separate out bus trips
      pt_trips <- rd_list[[i]] %>% dplyr::filter(stage_mode %in% pt_modes)
      
      # further separate out bus trips WITHOUT pedestrian component
      pt_trips_wo_walk <- rd_list[[i]] %>% filter(trip_id %in% pt_trips$trip_id) %>% group_by(trip_id) %>% 
        mutate(ped = if(any(stage_mode == 'pedestrian')) 1 else 0) %>% 
        ungroup() %>% 
        filter(ped == 0) %>% dplyr::select(-ped)
      
      nrpt <- pt_trips %>% distinct(trip_id) %>% nrow
      nrptwp <- pt_trips_wo_walk %>% distinct(trip_id) %>% nrow
      
      # print(CITY)
      # print(paste(nrpt, " - ", round(nrptwp /  nrpt * 100,1)))
      
      # separate bus trips WITH pedestrian component
      pt_trips_w_walk <- pt_trips %>% filter(trip_id %in% setdiff(pt_trips$trip_id, pt_trips_wo_walk$trip_id))
      
      not_pt_trips <- subset(rd_list[[i]], !id %in% pt_trips$id)
      # divide bus trips into bus and pedestrian
      pt_walk_trips <- add_walk_trips(pt_trips_wo_walk)
      
      # recombine all trips
      rd_list[[i]] <- rbind(not_pt_trips, pt_trips_w_walk, pt_walk_trips[[1]],pt_walk_trips[[2]])
      
      rd_list[[i]]$id <- NULL
    }
  
  trip_df <- do.call('rbind',rd_list)
  rd_list <- NULL
  
  ## update all distances and durations
  trip_df <- scale_trip_distances(trip_df)
    
  return(trip_df)
  
}


#' Scale trip distances
#' 
#' Applies mode-specific distance scalars to all trips
#' 
#' @param trips data frame, all trips from all scenarios
#' 
#' @return data frame, all trips from all scenarios
#' 
#' @export
scale_trip_distances <- function(trips){
  car_taxi_modes <- c('car','taxi','auto_rickshaw','shared_auto')
  pt_modes <- c('bus','minibus','subway','rail','walk_to_pt')
  ## omit trip distance as it has already been used to create scenarios and has no other use
  #column_names <- c('stage_distance','stage_duration')
  match_modes <- rep(1,nrow(trips))
  stage_modes <- trips$stage_mode
  match_modes[stage_modes%in%car_taxi_modes] <- DISTANCE_SCALAR_CAR_TAXI
  match_modes[stage_modes=='pedestrian'] <- DISTANCE_SCALAR_WALKING
  match_modes[stage_modes%in%pt_modes] <- DISTANCE_SCALAR_PT
  match_modes[stage_modes=='cycle'] <- DISTANCE_SCALAR_CYCLING
  match_modes[stage_modes=='motorcycle'] <- DISTANCE_SCALAR_MOTORCYCLE
  trips$stage_distance <- trips$stage_distance*match_modes
  trips$stage_duration <- trips$stage_duration*match_modes
  
  ##!! do not need trip_distance as scaling after creating scenarios
  #trips$trip_distance <- trips$stage_distance
  #trip_ids <- trips$trip_id
  #n_stages <- sapply(trip_ids,function(x)sum(trip_ids==x))
  #if(any(n_stages>1)){
  #  stage_dist <- trips$stage_distance
  #  trips$trip_distance[n_stages>1] <- sapply(trip_ids[n_stages>1],function(x)sum(stage_dist[trip_ids==x]))
  #}
  
  trips
}
