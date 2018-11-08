create_all_scenarios <- function(rd){
  rd_list <- list()
  rd_list[[1]] <- rd
  # Scenario 1
  
  rdr <- rd
  
  source_modes <- c('Bus', 'Walking')
  target_modes <- c('Private Car')
  
  source_percentages <- c(0.16, 0.49)
  
  tt <- nrow(filter(rdr, ! trip_mode %in% c('99', 'Short Walking')))
  
  rdr <- create_scenario(rdr, scen_name = 'Scenario 1', source_modes = source_modes, 
                         target_modes = target_modes, source_distance_cats = DIST_CAT, 
                         source_trips = c(round(source_percentages[1] * tt), 
                                          round(source_percentages[2] * tt)))
  
  #rdfinal <- rbind(rd, rdr)
  rd_list[[2]] <- rdr
  
  ###############################################################
  # Scenario 2
  
  rdr <- rd_list[[2]]#filter(rdfinal, scenario == 'Scenario 1')
  
  # 35 % Bus
  
  tt <- nrow(filter(rdr,! trip_mode %in% c('99', 'Short Walking')))
  
  source_modes <- c('Private Car', 'Taxi')
  target_modes <- c('Bus')
  
  target_new_trips <- c(round(0.35 * tt) - nrow(filter(rdr, trip_mode == 'Bus')))
  
  total_car_trips <- filter(rdr, (trip_mode %in% c(source_modes[1], source_modes[2])))
  
  
  t_dc <- total_car_trips %>% group_by(trip_distance_cat) %>% summarise(count = dplyr::n())
  
  long_trips <- sum(t_dc[t_dc$trip_distance_cat != DIST_CAT[1],]$count)
  
  long_car_trips_sample <- create_scenario(total_car_trips, scen_name = 'Scenario 2', source_modes = source_modes, combined_modes = T, 
                                           target_modes = target_modes, source_distance_cats = c(DIST_CAT[2], DIST_CAT[3]), 
                                           source_trips = c(long_trips))
  
  short_car_trips_sample <- create_scenario(total_car_trips, scen_name = 'Scenario 2', source_modes = source_modes, combined_modes = T, 
                                            target_modes = target_modes, source_distance_cats = DIST_CAT[1], 
                                            source_trips = c(target_new_trips[1] - long_trips))
  
  car_trips_sample <- rbind(long_car_trips_sample, short_car_trips_sample)
  
  ##  ADDING SHORT WALK TRIPS FOR NEW BUS TRIPS
  
  # Divide bus trips into bus and walk trips
  bus_trips <- car_trips_sample
  
  bus_walk_trips <- add_walk_trips(bus_trips, ln_mean = MEAN_BUS_WALK_TIME, ln_sd = 1.2)
  
  # Update selected rows for mode and duration
  rdr[rdr$row_id %in% bus_walk_trips[[1]]$row_id,]$trip_mode <- bus_walk_trips[[1]]$trip_mode
  rdr[rdr$trip_mode == 'Bus' & rdr$rid %in% bus_walk_trips[[1]]$rid,]$trip_duration <- bus_walk_trips[[1]]$trip_duration
  rdr[rdr$trip_mode == 'Bus' & rdr$rid %in% bus_walk_trips[[1]]$rid,]$trip_distance <- bus_walk_trips[[1]]$trip_distance
  rdr[rdr$trip_mode == 'Bus' & rdr$rid %in% bus_walk_trips[[1]]$rid,]$trip_distance_cat <- bus_walk_trips[[1]]$trip_distance_cat
  
  rdr %>% group_by(trip_mode) %>% summarise(c = dplyr::n(), p = dplyr::n() / nrow(rdr) * 100)
  
  rdr <- rbind(rdr, bus_walk_trips[[2]])
  
  rdr$scenario <- "Scenario 2"
  
  #rdfinal <- rbind(rdfinal, rdr)
  rd_list[[3]] <- rdr
  ###############################################################
  # Scenario 3
  
  rdr <- rd_list[[2]]#filter(rdfinal, scenario == 'Scenario 1')
  
  # 16 % Bus remain as is
  # 10 % Mcycle increase 
  # x decrease private car
  
  tt <- nrow(filter(rdr,! trip_mode %in% c('99', 'Short Walking')))
  source_modes <- c('Private Car')
  target_modes <- c('Motorcycle')
  
  target_new_trips <- c(round(0.1 * tt) - 
                          nrow(filter(rdr, trip_mode == 'Motorcycle')))
  
  mcycle_trips_sample <- create_scenario(rdr, scen_name = 'Scenario 3', source_modes = source_modes, 
                                         combined_modes = T, target_modes = target_modes, 
                                         source_distance_cats = DIST_CAT, source_trips = c(round(0.1 * tt) - 
                                                                                             nrow(filter(rdr, trip_mode == 'Motorcycle'))))
  
  # Update selected rows for mode and duration
  rdr[rdr$row_id %in% mcycle_trips_sample$row_id,]$trip_mode <- mcycle_trips_sample$trip_mode
  rdr[rdr$row_id %in% mcycle_trips_sample$row_id,]$trip_duration <- mcycle_trips_sample$trip_duration
  
  rdr$scenario <- "Scenario 3"
  
  #rdfinal <- rbind(rdfinal, rdr)
  rd_list[[4]] <- rdr
  ###############################################################
  # Scenario 4
  
  rdr <- rd_list[[2]]#filter(rdfinal, scenario == 'Scenario 1')
  
  # 3.5 % Cycle
  
  tt <- nrow(filter(rdr, ! trip_mode %in% c('99', 'Short Walking')))
  
  source_modes <- c('Motorcycle', 'Private Car', 'Taxi')
  target_modes <- c('Bicycle')
  
  target_new_trips <- c(52, round(0.035 * tt) - nrow(filter(rdr, trip_mode == 'Bicycle')) - 52)
  
  mbike_trips <- create_scenario(rdr, scen_name = 'Scenario 4', source_modes = source_modes[1], 
                                 combined_modes = T, target_modes = target_modes, 
                                 source_distance_cats = DIST_CAT, 
                                 source_trips = target_new_trips[1])
  
  car_trips <- create_scenario(rdr, scen_name = 'Scenario 4', source_modes = c(source_modes[2], source_modes[3]), 
                               combined_modes = T, target_modes = target_modes, 
                               source_distance_cats = DIST_CAT[1], 
                               source_trips = target_new_trips[2])
  
  car_mbike_trips <- rbind(mbike_trips, car_trips)
  
  # Update selected rows for mode and duration
  rdr[rdr$row_id %in% car_mbike_trips$row_id,]$trip_mode <- car_mbike_trips$trip_mode
  rdr[rdr$row_id %in% car_mbike_trips$row_id,]$trip_duration <- car_mbike_trips$trip_duration
  
  rdr %>% group_by(trip_mode) %>% summarise(c = dplyr::n(), p = dplyr::n() / nrow(rdr) * 100)
  
  rdr$scenario <- "Scenario 4"
  
  #rdfinal <- rbind(rdfinal, rdr)
  rd_list[[5]] <- rdr
  ###############################################################
  # Scenario 5
  
  rdr <- rd_list[[2]]#filter(rdfinal, scenario == 'Scenario 1')
  
  # 3.5 % Cycle
  
  tt <- nrow(filter(rdr, ! trip_mode %in% c('99', 'Short Walking')))
  
  source_modes <- c('Private Car', 'Taxi')
  target_modes <- c('Walking')
  
  target_new_trips <- c(round(0.54 * tt) - nrow(filter(rdr, trip_mode == 'Walking')))
  
  motorised_trips <- create_scenario(rdr, scen_name = 'Scenario 4', source_modes = c(source_modes[1], source_modes[2]), 
                                     combined_modes = T, target_modes = target_modes, 
                                     source_distance_cats = DIST_CAT[1], 
                                     source_trips = target_new_trips[1])
  
  # Update selected rows for mode and duration
  rdr[rdr$row_id %in% motorised_trips$row_id,]$trip_mode <- motorised_trips$trip_mode
  rdr[rdr$row_id %in% motorised_trips$row_id,]$trip_duration <- motorised_trips$trip_duration
  
  rdr %>% group_by(trip_mode) %>% summarise(c = dplyr::n(), p = dplyr::n() / nrow(rdr) * 100)
  
  rdr$scenario <- "Scenario 5"
  
  #rdfinal <- rbind(rdfinal, rdr)
  rd_list[[6]] <- rdr
  
  
  do.call('rbind',rd_list)
}
