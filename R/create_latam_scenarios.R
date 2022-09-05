#' Create scenarios defined for latam paper
#' 
#' Creates three scenarios where, in each one, the mode share is elevated to the
#' certain values in each distance band. The scenario-modes are cycle, car, and bus.
#' 
#' Motorcycle trips were added as well
#' 
#' @param trip_set data frame, baseline scenario
#' 
#' @return list of baseline scenario and three mode scenarios
#' 
#' @export
create_latam_scenarios <- function(trip_set){
  # I created this function based on "create_max_mode_share_scenarios" function.
  rdr <- trip_set
  trip_set <- NULL
  
  rd_list <- list()
  # In this function SCENARIO_PROPORTIONS has the increase in % percentage points
  # for each mode. These numbers came from computing the difference between
  # median propensity in each distance band for cycling and the top value.
  # We computed this numbers by hand, using the SCENARIO_PROPORTIONs that results
  # in the function "create_max_mode_share_scenarios" and the median was computed
  # by hand across cities. All of this is computed in the file "TablesAndFigures.xlsx"
  # in dropbox.
  scenario_proportions <- data.frame(c(5.6, 5.6, 0, 5.6),
                                     c(10.2, 10.2, 10.2, 10.2),
                                     c(2,2,2, 2))
  colnames(scenario_proportions) <- target_distances <- DIST_CAT
  rownames(scenario_proportions) <- modes <- c("cycle", "car", "bus", 'motorcycle')
  SCENARIO_PROPORTIONS <<- scenario_proportions
  # Baseline scenario
  rd_list[[1]] <- rdr
  modes_not_changeable <- c('bus_driver', 'truck', 'car_driver')
  rdr_not_changeable <-  rdr %>% filter(trip_mode %in% modes_not_changeable)
  rdr_changeable <-  rdr %>% filter(!trip_mode %in% modes_not_changeable) # Trips that can be reassigned to another mode
  rdr <- NULL
  
  # Split trips by distance band in a new list
  rdr_changeable_by_distance <- list()
  for (j in 1:ncol(SCENARIO_PROPORTIONS)) {
    target_distance <- target_distances[j]
    rdr_changeable_by_distance[[j]] <- rdr_changeable %>% 
      filter(trip_distance_cat == target_distance)
  }
  rdr_changeable <- NULL
  
  ###############################################################
  # Creation of scenarios
  for (i in 1:nrow(SCENARIO_PROPORTIONS)) { # Loop for each scenario
    mode_name <- modes[i] # mode of the scenario
    rdr_copy <- list()
    for (j in 1:ncol(SCENARIO_PROPORTIONS)) { # Loop for each distance band
      rdr_copy[[j]] <- rdr_changeable_by_distance[[j]] # Trips in the distance band
      if (mode_name != "bus") {
        # Identify the trips_id of trips that weren't made by the trip mode
        potential_trip_ids <- unique(rdr_copy[[j]][!rdr_copy[[j]]$trip_mode %in% c(mode_name),]$trip_id)
        
        # Count the number of trips that were made by the trip mode
        current_mode_trips <- rdr_copy[[j]] %>% 
          filter(trip_mode == mode_name) %>% distinct(trip_id) %>% nrow()
      } else {
        # Identify the trips_id of trips that weren't made by the trip mode
        potential_trip_ids <- unique(rdr_copy[[j]][!rdr_copy[[j]]$trip_mode %in% c(mode_name ,"rail"),]$trip_id)
        
        # Count the number of trips that were made by the trip mode
        current_mode_trips <- rdr_copy[[j]] %>% 
          filter(trip_mode %in% c(mode_name, "rail")) %>% distinct(trip_id) %>% 
          nrow()
      } # End else
      target_percent <- SCENARIO_PROPORTIONS[i,j]
      n_trips_to_change <- round(length(unique(rdr_copy[[j]]$trip_id)) * 
                                   target_percent / 100) # These trips will be reassigned
      if (length(potential_trip_ids) > 0 & n_trips_to_change > 0) {
        if (length(potential_trip_ids) == 1) {
          change_trip_ids <- potential_trip_ids
        } else {
          change_trip_ids <- base::sample(potential_trip_ids,
                                          size = n_trips_to_change)
        } 
        change_trips <- rdr_copy[[j]][rdr_copy[[j]]$trip_id %in% change_trip_ids,]
        change_trips$trip_mode <- mode_name
        change_trips$stage_mode <- mode_name
        change_trips$stage_duration <- change_trips$stage_distance * 60 /
          MODE_SPEEDS$speed[MODE_SPEEDS$stage_mode == mode_name]
        
        # Replace trips reassigned in the trip dataset and save them in a new list
        rdr_copy[[j]] <- 
          rbind(rdr_copy[[j]][!rdr_copy[[j]]$trip_id %in% change_trip_ids,],
                change_trips)
      }
    } # End loop for distance bands
    rdr_scen <- do.call(rbind,rdr_copy)
    rdr_scen <- rbind(rdr_scen,rdr_not_changeable)
    
    # Remove bus_driver from the dataset, to recalculate them
    rdr_scen <-  filter(rdr_scen, !trip_mode %in% 'bus_driver')
    rdr_scen <- add_ghost_trips(rdr_scen,
                                trip_mode = 'bus_driver',
                                distance_ratio = BUS_TO_PASSENGER_RATIO * DISTANCE_SCALAR_PT,
                                reference_mode = 'bus',
                                scenario = paste0('Scenario ',i))
    #print(paste("Scenario name: ", paste0('Scenario ',i)))
    bus_dr_dist <- sum(rdr_scen[rdr_scen$stage_mode=='bus_driver',]$stage_distance,na.rm=T)
    bus_dist <- sum(rdr_scen[rdr_scen$stage_mode=='bus',]$stage_distance,na.rm=T)
    
    #print(bus_dr_dist/bus_dist)
    
    
    # Remove car_driver from the dataset, to recalculate them
    rdr_scen <-  filter(rdr_scen, !trip_mode %in% 'car_driver')
    rdr_scen <- add_ghost_trips(rdr_scen,
                                trip_mode='car_driver',
                                distance_ratio=car_driver_scalar*DISTANCE_SCALAR_CAR_TAXI,
                                reference_mode='car',
                                scenario = paste0('Scenario ',i))
    #print(paste("Scenario name: ", paste0('Scenario ',i)))
    car_dr_dist <- sum(rdr_scen[rdr_scen$stage_mode=='car_driver',]$stage_distance,na.rm=T)
    car_dist <- sum(rdr_scen[rdr_scen$stage_mode=='car',]$stage_distance,na.rm=T)
    
    #print(car_dr_dist/car_dist)
    
    
    # browser()
    rdr_scen$scenario <- paste0('Scenario ',i)
    rd_list[[i + 1]] <- rdr_scen
  } # End loop for scenarios
  return(rd_list)
}