#' Create scenarios based on difference between median and max propensities for all cities
#' 
#' Creates four scenarios where, in each one, the mode share is elevated to the
#' certain values in each distance band. The scenario-modes are cycle, car, and bus and motorcycle.
#' 
#' Based on create_latam_scenarios
#' 
#' Add 5% of trips overall in such a way that the average mean mode share for each mode across the
#' three distance bands is preserved.
#' 
#' https://www.dropbox.com/home/ITHIM%20Global/Methods%20and%20Processes/ScenarioDefn/GlobalScenario
#' 
#' @param trip_set data frame, baseline scenario
#' 
#' @return list of baseline scenario and four mode scenarios
#' 
#' @export
create_africa_india_scenarios <- function(trip_set){
  
  rdr <- trip_set
  trip_set <- NULL
  
  rd_list <- list()
  
  # global modal split across the three distance categories for each mode
  # cycle, car, bus, motorcycle
  global_modeshares <- data.frame(c(36.8, 8.0, 1.7, 11.1), # distance category 0-2km
                                  c(47.2, 33.8, 25.8, 37.2), # distance category 2-6km
                                  c(16.0, 58.2, 72.7, 53.1))
  
  percentage_change <- 0.05
  
  
  rdr_baseline <- rdr %>% dplyr::select(c('trip_id', 'trip_distance_cat','scenario','trip_mode')) %>% filter() 
  rdr_baseline <- rdr_baseline %>% distinct()
  
  no_trips <- nrow(rdr_baseline)
  prop_0_2 <- nrow(rdr_baseline %>% filter(trip_distance_cat == "0-2km")) / no_trips
  prop_2_6 <- nrow(rdr_baseline %>% filter(trip_distance_cat == "2-6km")) / no_trips
  prop_6 <- nrow(rdr_baseline %>% filter(trip_distance_cat == "6+km")) / no_trips
  
  # initialise the proportions to be added in each scenario
  scenario_proportions <- data.frame(c(0, 0, 0, 0), # distance category 0-2km
                                     c(0, 0, 0, 0), # distance category 2-6km
                                     c(0, 0,0, 0))
  # add the correct values
  for (r in 1:3){
    for (c in 1:4){
      if (r == 1){
        percentage_trips <- prop_0_2
      } else if (r == 2){
        percentage_trips <- prop_2_6
      } else {
        percentage_trips <- prop_6
      }
      scenario_proportions[c,r] <- percentage_change * global_modeshares[c,r] / percentage_trips
    }
  }
  
  
  colnames(scenario_proportions) <- target_distances <- DIST_CAT
  rownames(scenario_proportions) <- modes <- c("cycle", "car", "bus", 'motorcycle')
  SCENARIO_PROPORTIONS <<- scenario_proportions
  
  #print(scenario_proportions)
  
  # Baseline scenario
  rd_list[[1]] <- rdr
  modes_not_changeable <- c('bus_driver', 'truck', 'car_driver')
  rdr_not_changeable <-  rdr %>% filter(trip_mode %in% modes_not_changeable)
  rdr_changeable <-  rdr %>% filter(!trip_mode %in% modes_not_changeable) # Trips that can be reassigned to another mode
  
  
  # Split trips by distance band in a new list
  rdr_changeable_by_distance <- list()
  for (j in 1:ncol(SCENARIO_PROPORTIONS)) {
    target_distance <- target_distances[j]
    rdr_changeable_by_distance[[j]] <- rdr_changeable %>% 
      filter(trip_distance_cat == target_distance)
  }
  rdr_changeable <- NULL
  
  # split all trips by distance band
  rdr_all_by_distance <- list()
  for (j in 1:ncol(SCENARIO_PROPORTIONS)) {
    target_distance <- target_distances[j]
    rdr_all_by_distance[[j]] <- rdr %>% 
      filter(trip_distance_cat == target_distance)
  }
  
  rdr <- NULL
  
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
      # n_trips_to_change <- round(length(unique(rdr_copy[[j]]$trip_id)) * 
      #                              target_percent / 100) # These trips will be reassigned
      n_trips_to_change <- round(length(unique(rdr_all_by_distance[[j]]$trip_id)) * 
                                   target_percent / 100) # These trips will be reassigned
      #print(n_trips_to_change)
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
  
  
  # check scenario defn:
  # bicycle
  # baseline_all <- rd_list[[1]] %>% dplyr::select(c('trip_id', 'trip_mode',"trip_distance_cat")) %>% distinct() 
  # base_count_all <- nrow(baseline_all)
  # baseline_bike <- baseline_all %>% filter(trip_mode == 'cycle')
  # base_count_bike <- nrow(baseline_bike)
  # base_prop_bike <- base_count_bike / base_count_all * 100
  # 
  # bike_all <- rd_list[[2]] %>% dplyr::select(c('trip_id', 'trip_mode',"trip_distance_cat")) %>% distinct() 
  # bike_bike <- bike_all %>% filter(trip_mode == 'cycle')
  # bike_count_bike <- nrow(bike_bike)
  # bike_prop_bike <- bike_count_bike / base_count_all * 100
  
  # bus
  # base_count_all <- nrow(baseline_all)
  # baseline_bus <- baseline_all %>% filter(trip_mode == 'bus')
  # base_count_bus <- nrow(baseline_bus)
  # base_prop_bus <- base_count_bus / base_count_all * 100
  # 
  # bus_all <- rd_list[[4]] %>% dplyr::select(c('trip_id', 'trip_mode',"trip_distance_cat")) %>% distinct() 
  # bus_bus <- bus_all %>% filter(trip_mode == 'bus')
  # bus_count_bus <- nrow(bus_bus)
  # bus_prop_bus <- bus_count_bus / base_count_all * 100
  
  return(rd_list)
}