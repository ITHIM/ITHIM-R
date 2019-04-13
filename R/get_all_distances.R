#' @export
get_all_distances <- function(ithim_object){
  
  # add walk-to-bus trips, as appropriate, and combines list of scenarios
  trip_scen_sets <- walk_to_pt()
  
  ## update all distances and durations
  for(i in 1:length(trip_scen_sets)) trip_scen_sets[[i]] <- scale_trip_distances(trip_scen_sets[[i]])
  
  ##!! might not want to save this object
  ithim_object$trip_scen_sets <- do.call(rbind,trip_scen_sets)
  
  ##RJ synthetic population
  ithim_object$pp_summary <- generate_synthetic_travel_data(trip_scen_sets)
  
  # Generate distance and duration matrices
  dist_and_dir <- dist_dur_tbls(ithim_object$pp_summary)
  ithim_object$dist <- dist_and_dir$dist
  ithim_object$dur <- dist_and_dir$dur
  
  # distances for injuries calculation
  ithim_object$inj_distances <- distances_for_injury_function(ithim_object$pp_summary)
  
  return(ithim_object)
  
  
  # add walk-to-bus trips, as appropriate, and combines list of scenarios
  #ithim_object$trip_scen_sets <- walk_to_pt_and_combine_scen()
  
  # Generate distance and duration matrices
  #dist_and_dir <- dist_dur_tbls(ithim_object$trip_scen_sets)
  #ithim_object$dist <- dist_and_dir$dist
  #ithim_object$dur <- dist_and_dir$dur
  
  # distances for injuries calculation
  #ithim_object$inj_distances <- distances_for_injury_function(ithim_object$trip_scen_sets)
  
  #return(ithim_object)
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
