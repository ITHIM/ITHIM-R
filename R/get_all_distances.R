#' @export
get_all_distances <- function(pp_summary){
  
  # Generate distance and duration matrices
  dist_and_dur <- dist_dur_tbls(pp_summary)
  dist <- dist_and_dur$dist
  dur <- dist_and_dur$dur
  
  # distances for injuries calculation
  inj_distances <- distances_for_injury_function(pp_summary,dist)
  
  return(list(dist=dist,dur=dur,inj_distances=inj_distances))
  
  
}


#' @export
scale_trip_distances <- function(trips){
  car_taxi_modes <- UNCERTAIN_TRAVEL_MODE_NAMES$car
  pt_modes <- UNCERTAIN_TRAVEL_MODE_NAMES$pt
  
  match_modes <- rep(1,nrow(trips))
  stage_modes <- trips$stage_mode
  match_modes[stage_modes%in%car_taxi_modes] <- DISTANCE_SCALAR_CAR_TAXI
  match_modes[stage_modes%in%c('walking')] <- DISTANCE_SCALAR_WALKING
  match_modes[stage_modes%in%pt_modes] <- DISTANCE_SCALAR_PT
  match_modes[stage_modes%in%c('cycling')] <- DISTANCE_SCALAR_CYCLING
  match_modes[stage_modes%in%c('motorcycle')] <- DISTANCE_SCALAR_MOTORCYCLE
  trips[,'stage_distance':=stage_distance*..match_modes]
  trips[,'stage_duration':=stage_duration*..match_modes]
  #trips$stage_duration <- trips$stage_duration*match_modes
  
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
