#' Create scenarios defined by maximum mode share
#' 
#' Creates five scenarios where, in each one, the mode share is elevated to the maximum observed across the cities.
#' The scenario-modes are pedestrian, cycle, car, motorcycle and bus
#' 
#' @param trip_set data frame, baseline scenario
#' 
#' @return list of baseline scenario and five mode scenarios
#' 
#' @export
create_max_mode_share_scenarios <- function(trip_set){
  rdr <- trip_set
  trip_set <- NULL
  
  rd_list <- list()
  target_distances <- colnames(SCENARIO_PROPORTIONS)
  modes <- rownames(SCENARIO_PROPORTIONS)
  # Baseline scenario
  rd_list[[1]] <- rdr
  rdr_not_changeable <-  rdr[rdr$trip_mode%in%c('bus_driver','truck', 'car_driver'),]
  rdr_changeable <-  rdr[!rdr$trip_mode%in%c('bus_driver','truck', 'car_driver'),]
  rdr <- NULL
  
  rdr_changeable_by_distance <- list()
  for(j in 1:ncol(SCENARIO_PROPORTIONS)){
    target_distance <- target_distances[j]
    rdr_changeable_by_distance[[j]] <- rdr_changeable[rdr_changeable$trip_distance_cat==target_distance,]
  }
  rdr_changeable <- NULL
  
  ###############################################################
  for(i in 1:nrow(SCENARIO_PROPORTIONS)) {
    mode_name <- modes[i]
    rdr_copy <- list()
    for(j in 1:ncol(SCENARIO_PROPORTIONS)){
      rdr_copy[[j]] <- rdr_changeable_by_distance[[j]]
      if (mode_name != "bus") {
        potential_trip_ids <- unique(rdr_copy[[j]][!rdr_copy[[j]]$trip_mode%in%c(mode_name),]$trip_id)
        ## AA: Fix proportion of current mode by counting unique IDs
        ## Remove counting rows by mode, as that would double count trips with multiple stages
        current_mode_trips <- rdr_copy[[j]] %>% 
          filter(trip_mode == mode_name) %>% distinct(trip_id) %>% nrow()
      } else {
        potential_trip_ids <- unique(rdr_copy[[j]][!rdr_copy[[j]]$trip_mode%in%c(mode_name ,"rail"),]$trip_id)
      ## AA: Fix proportion of current mode by counting unique IDs
      ## Remove counting rows by mode, as that would double count trips with multiple stages
      current_mode_trips <- rdr_copy[[j]] %>% 
        filter(trip_mode %in% c(mode_name, "rail")) %>% distinct(trip_id) %>% 
        nrow()
      }
      
      target_percent <- SCENARIO_PROPORTIONS[i,j]
      if(length(potential_trip_ids)>0&&round(length(unique(rdr_copy[[j]]$trip_id))/100*target_percent)-current_mode_trips>0){
        if(length(potential_trip_ids)==1){
          change_trip_ids <- potential_trip_ids
        }else{
          change_trip_ids <- base::sample(potential_trip_ids,size=round(length(unique(rdr_copy[[j]]$trip_id))/100*target_percent)-current_mode_trips)
          # t <- try(base::sample(potential_trip_ids,size=round(length(unique(rdr_copy[[j]]$trip_id))/100*target_percent)-current_mode_trips))
          # if ("try-error" %in% class(t))
          #   change_trip_ids <- base::sample(potential_trip_ids,size=round(length(unique(rdr_copy[[j]]$trip_id))/100*target_percent)-current_mode_trips,replace = T)
          # else 
          #   change_trip_ids <- base::sample(potential_trip_ids,size=round(length(unique(rdr_copy[[j]]$trip_id))/100*target_percent)-current_mode_trips)
          #print(c(CITY,mode_name,length(change_trip_ids)))
        }
        change_trips <- rdr_copy[[j]][rdr_copy[[j]]$trip_id %in% change_trip_ids,]
        change_trips$trip_mode <- mode_name
        change_trips$stage_mode <- mode_name
        change_trips$stage_duration <- change_trips$stage_distance * 60 / MODE_SPEEDS$speed[MODE_SPEEDS$stage_mode == mode_name]
        ## if bus scenario: 
        # if (mode_name == 'bus'){
        #   walk_trips <- change_trips;
        #   walk_trips$stage_mode <- 'walk_to_pt';
        #   walk_trips$stage_duration <- BUS_WALK_TIME;
        #   walk_trips$stage_distance <- walk_trips$stage_duration * MODE_SPEEDS$speed[MODE_SPEEDS$stage_mode == 'pedestrian'] / 60
        #   change_trips <- rbind(change_trips, walk_trips)
        # }
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
