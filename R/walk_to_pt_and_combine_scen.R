#' @export
walk_to_pt_and_combine_scen <- function(){
  rd_list <- SYNTHETIC_TRIPS
  
  ## pt = public transport
  pt_modes <- c('bus','minibus','subway','rail')
  
  if(ADD_WALK_TO_BUS_TRIPS)
    for(i in 1:length(rd_list)){
      # separate out bus trips
      pt_trips <- subset(rd_list[[i]],stage_mode%in%pt_modes)
      not_pt_trips <- subset(rd_list[[i]],!stage_mode%in%pt_modes)
      # divide bus trips into bus and walking
      pt_walk_trips <- add_walk_trips(pt_trips)
      # recombine all trips
      rd_list[[i]] <- rbind(not_pt_trips,pt_walk_trips[[1]],pt_walk_trips[[2]])
    }
  
  trip_df <- do.call('rbind',rd_list)
  
  ## update all distances and durations
  trip_df <- scale_trip_distances(trip_df)
    
  return(trip_df)
  
}


#' @export
scale_trip_distances <- function(trips){
  car_taxi_modes <- c('car','taxi','auto_rickshaw','shared_auto')
  pt_modes <- c('bus','minibus','subway','rail','walk_to_bus')
  ## omit trip distance as it has already been used to create scenarios and has no other use
  column_names <- c('stage_distance','stage_duration')
  match_modes <- rep(1,nrow(trips))
  stage_modes <- trips$stage_mode
  match_modes[stage_modes%in%car_taxi_modes] <- DISTANCE_SCALAR_CAR_TAXI
  match_modes[stage_modes%in%c('walking')] <- DISTANCE_SCALAR_WALKING
  match_modes[stage_modes%in%pt_modes] <- DISTANCE_SCALAR_PT
  match_modes[stage_modes%in%c('cycling')] <- DISTANCE_SCALAR_CYCLING
  match_modes[stage_modes%in%c('motorcycle')] <- DISTANCE_SCALAR_MOTORCYCLE
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