create_all_scenarios <- function(trip_set){
  ###############################################################
  rdr <- trip_set
  tt <- nrow(rdr)
  rd_list <- list()
  rd_list[[1]] <- rdr
  # Scenario 1
  source_modes <- c('Bus', 'Walking')
  target_modes <- c('Private Car')
  source_percentages <- round(c(0.16, 0.49)* tt)
  rdr <- create_scenario(rdr, scen_name = 'Scenario 1', source_modes = source_modes, 
                         target_modes = target_modes, source_distance_cats = DIST_CAT, 
                         source_trips = source_percentages)
  rd_list[[2]] <- rdr
  ###############################################################
  # Scenario 2
  rdr <- rd_list[[2]]
  # 35 % of all trips are Bus.
  # These come from private car and taxi.
  # All car and taxi trips > 6 km go to Bus. Then 35 car and taxi trips 0--6 km go to bus.
  source_modes <- c('Private Car', 'Taxi')
  target_modes <- c('Bus')
  target_new_trips <- round(0.35 * tt - sum(rdr$trip_mode=='Bus'))
  total_car_trips <- filter(rdr, trip_mode %in% source_modes)
  long_trips <- sum(total_car_trips$trip_distance_cat!=DIST_CAT[1])
  long_car_trips_sample <- create_scenario(total_car_trips, scen_name = 'Scenario 2', source_modes = source_modes, combined_modes = T, 
                                           target_modes = target_modes, source_distance_cats = DIST_CAT[2:3],source_trips = long_trips)
  short_trips <- min( target_new_trips - long_trips, sum(total_car_trips$trip_distance_cat==DIST_CAT[1]))
  if(short_trips>0){
    short_car_trips_sample <- create_scenario(total_car_trips, scen_name = 'Scenario 2', source_modes = source_modes, combined_modes = T, 
                                              target_modes = target_modes, source_distance_cats = DIST_CAT[1],source_trips = short_trips) 
    long_car_trips_sample <- rbind(long_car_trips_sample, short_car_trips_sample)
  }
  bus_trips <- long_car_trips_sample
  
  # Update selected rows for mode and duration
  rdr$trip_mode[match(bus_trips$rid,rdr$rid)] <- bus_trips$trip_mode
  rdr$trip_duration[match(bus_trips$rid,rdr$rid)] <- bus_trips$trip_duration
  rdr$trip_distance[match(bus_trips$rid,rdr$rid)] <- bus_trips$trip_distance
  rdr$trip_distance_cat[match(bus_trips$rid,rdr$rid)] <- bus_trips$trip_distance_cat
  
  rdr$scenario <- "Scenario 2"
  rd_list[[3]] <- rdr
  ###############################################################
  # Scenario 3
  rdr <- rd_list[[2]]
  # 16 % Bus remain as is
  # 10 % Mcycle increase 
  # x decrease private car
  source_modes <- c('Private Car')
  target_modes <- c('Motorcycle')
  target_new_trips <- max(round(0.1 * tt) - sum(rdr$trip_mode=='Motorcycle'),1)
  mcycle_trips_sample <- create_scenario(rdr, scen_name = 'Scenario 3', source_modes = source_modes, 
                                         combined_modes = T, target_modes = target_modes, 
                                         source_distance_cats = DIST_CAT, source_trips = target_new_trips)
  # Update selected rows for mode and duration
  rdr$trip_mode[match(mcycle_trips_sample$row_id,rdr$row_id)] <- mcycle_trips_sample$trip_mode
  rdr$trip_duration[match(mcycle_trips_sample$row_id,rdr$row_id)] <- mcycle_trips_sample$trip_duration
  rdr$scenario <- "Scenario 3"
  rd_list[[4]] <- rdr
  #return(rd_list)
  ###############################################################
  # Scenario 4
  rdr <- rd_list[[2]]
  # 3.5 % Cycle
  source_modes <- c('Motorcycle', 'Private Car', 'Taxi')
  target_modes <- c('Bicycle')
  mtrips <- max(min(52,sum(rdr$trip_mode == 'Motorcycle')),1)
  btrips <- sum(rdr$trip_mode == 'Bicycle')
  ctrips <- max(min(round(0.035 * tt) - btrips - mtrips, sum(rdr$trip_mode %in% c('Private Car', 'Taxi')&rdr$trip_distance_cat==DIST_CAT[1])),1)
  target_new_trips <- c(mtrips, ctrips)
  mbike_trips <- create_scenario(rdr, scen_name = 'Scenario 4', source_modes = source_modes[1],combined_modes = T, 
                                 target_modes = target_modes,source_distance_cats = DIST_CAT,source_trips = target_new_trips[1])
  car_trips <- create_scenario(rdr, scen_name = 'Scenario 4', source_modes = c(source_modes[2], source_modes[3]),combined_modes = T, 
                               target_modes = target_modes,source_distance_cats = DIST_CAT[1],source_trips = target_new_trips[2])
  car_mbike_trips <- rbind(mbike_trips, car_trips)
  # Update selected rows for mode and duration
  rdr$trip_mode[match(car_mbike_trips$row_id,rdr$row_id)] <- car_mbike_trips$trip_mode
  rdr$trip_duration[match(car_mbike_trips$row_id,rdr$row_id)] <- car_mbike_trips$trip_duration
  rdr$scenario <- "Scenario 4"
  rd_list[[5]] <- rdr
  ###############################################################
  # Scenario 5
  rdr <- rd_list[[2]]
  # 3.5 % Cycle
  source_modes <- c('Private Car', 'Taxi')
  target_modes <- c('Walking')
  target_new_trips <- min(round(0.54 * tt) - sum(rdr$trip_mode == target_modes), sum(rdr$trip_mode%in%source_modes&rdr$trip_distance_cat==DIST_CAT[1]))
  motorised_trips <- create_scenario(rdr, scen_name = 'Scenario 4', source_modes = source_modes, combined_modes = T, 
                                     target_modes = target_modes,source_distance_cats = DIST_CAT[1],source_trips = target_new_trips)
  # Update selected rows for mode and duration
  rdr$trip_mode[match(motorised_trips$row_id,rdr$row_id)] <- motorised_trips$trip_mode
  rdr$trip_duration[match(motorised_trips$row_id,rdr$row_id)] <- motorised_trips$trip_duration
  rdr$scenario <- "Scenario 5"
  rd_list[[6]] <- rdr
  
  rd_list
}