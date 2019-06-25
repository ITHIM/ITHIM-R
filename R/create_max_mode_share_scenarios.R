#' @export
create_max_mode_share_scenarios <- function(trip_set){
  rdr <- trip_set
  trip_set <- NULL
  
  rd_list <- list()
  target_distances <- colnames(SCENARIO_PROPORTIONS)
  modes <- rownames(SCENARIO_PROPORTIONS)
  # Baseline scenario
  rd_list[[1]] <- rdr
  rdr_not_changeable <-  rdr[rdr$trip_mode%in%c('bus_driver','truck'),]
  rdr_changeable <-  rdr[!rdr$trip_mode%in%c('bus_driver','truck'),]
  rdr <- NULL
  ## add trip weights
  car_taxi_modes <- UNCERTAIN_TRAVEL_MODE_NAMES$car
  pt_modes <- UNCERTAIN_TRAVEL_MODE_NAMES$pt
  ## weight by mode probability scalars
  match_modes <- rep(1,nrow(rdr_changeable))
  travel_modes <- rdr_changeable$trip_mode
  match_modes[travel_modes%in%car_taxi_modes] <- PROBABILITY_SCALAR_CAR_TAXI
  match_modes[travel_modes%in%c('walking')] <- PROBABILITY_SCALAR_WALKING
  match_modes[travel_modes%in%pt_modes] <- PROBABILITY_SCALAR_PT
  match_modes[travel_modes%in%c('cycling')] <- PROBABILITY_SCALAR_CYCLING
  match_modes[travel_modes%in%c('motorcycle')] <- PROBABILITY_SCALAR_MOTORCYCLE
  rdr_changeable$trip_weight <- match_modes
  
  rdr_changeable_by_distance <- list()
  for(j in 1:ncol(SCENARIO_PROPORTIONS)){
    target_distance <- target_distances[j]
    rdr_changeable_by_distance[[j]] <- rdr_changeable[rdr_changeable$trip_distance_cat==target_distance,]
  }
  rdr_changeable <- NULL
  
  ###############################################################
  for(i in 1:nrow(SCENARIO_PROPORTIONS)){
    mode_name <- modes[i]
    rdr_copy <- list()
    for(j in 1:ncol(SCENARIO_PROPORTIONS)){
      rdr_copy[[j]] <- rdr_changeable_by_distance[[j]]
      potential_trip_ids <- unique(rdr_copy[[j]]$trip_id[!rdr_copy[[j]]$trip_mode%in%c(mode_name)])
      potential_trips <- rdr_copy[[j]][rdr_copy[[j]]$trip_id%in%potential_trip_ids&!duplicated(rdr_copy[[j]]$trip_id),]
      current_mode_trips <- sum(rdr_copy[[j]]$trip_mode==mode_name)#sum(rdr_copy[[j]]$trip_weight[rdr_copy[[j]]$trip_mode==mode_name])
      current_mode_weight <- sum(rdr_copy[[j]]$trip_weight[rdr_copy[[j]]$trip_mode==mode_name])
      total_weight <- sum(rdr_copy[[j]]$trip_weight[!duplicated(rdr_copy[[j]]$trip_id)])
      target_percent <- SCENARIO_PROPORTIONS[i,j]
      target_weight <- target_percent/100 * total_weight
      weight_to_gain <- target_weight - current_mode_weight
      # translate weight into number of trips for target mode
      #trips_to_gain <- round(weight_to_gain*current_mode_trips/total_weight)
      if(length(potential_trip_ids)>0&&weight_to_gain>0){
        # sample trips until weight is achieved
        cumulative_weight <- 0
        change_trip_ids <- c()
        weights <- potential_trips$trip_weight
        while(cumulative_weight<weight_to_gain){
          new_index <- base::sample(1:length(potential_trip_ids),size=1,prob=weights,replace=F)
          new_trip <- potential_trip_ids[new_index]
          change_trip_ids <- c(change_trip_ids,new_trip)
          weight_gained <- potential_trips$trip_weight[new_index]
          potential_trips$trip_distance[new_index] <- potential_trips$trip_distance[new_index]*weight_gained
          potential_trips$stage_distance[new_index] <- potential_trips$stage_distance[new_index]*weight_gained
          potential_trips$stage_duration[new_index] <- potential_trips$stage_duration[new_index]*weight_gained
          weights[new_index] <- 0
          cumulative_weight <- cumulative_weight + weight_gained
        }
        change_trips <- potential_trips[potential_trips$trip_id %in% change_trip_ids,]
        change_trips$trip_mode <- mode_name
        change_trips$stage_mode <- mode_name
        change_trips$stage_duration <- change_trips$stage_distance * 60 / MODE_SPEEDS$speed[MODE_SPEEDS$stage_mode == mode_name]
        ## if bus scenario: 
        if (mode_name == 'bus'){
          walk_trips <- change_trips;
          walk_trips$stage_mode <- 'walk_to_pt';
          walk_trips$stage_duration <- BUS_WALK_TIME;
          walk_trips$stage_distance <- walk_trips$stage_duration * MODE_SPEEDS$speed[MODE_SPEEDS$stage_mode == 'walking'] / 60
          change_trips <- rbind(change_trips, walk_trips)
        }
        rdr_copy[[j]] <- rbind(rdr_copy[[j]][!rdr_copy[[j]]$trip_id%in%change_trip_ids,],change_trips)
      }
    }
    rdr_scen <- do.call(rbind,rdr_copy)
    rdr_scen <- rbind(rdr_scen,rdr_not_changeable)
    rdr_scen$scenario <- paste0('Scenario ',i)
    rd_list[[i+1]] <- rdr_scen
  }
  ###############################################################
  
  return(rd_list)
}
