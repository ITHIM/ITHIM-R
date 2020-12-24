#' Creates specific scenarios for Accra and Sao Paulo
#' 
#' Creates five prespecified scenarios from the baseline for Accra and Sao Paulo
#' 
#' @param trip_set data frame of baseline trips
#' 
#' @return list of scenarios
#' 
#' @export
create_all_scenarios <- function(trip_set){
  
  # Default city is set to accra
  
  if(CITY == 'accra'){
    
    ###############################################################
    rdr <- trip_set
    tt <- length(unique(rdr$trip_id))
    rd_list <- list()
    rd_list[[1]] <- rdr
    # Scenario 1
    source_modes <- c('bus', 'pedestrian')
    target_modes <- c('car')
    source_percentages <- round(c(0.16, 0.49)* tt)
    rdr <- create_scenario(rdr, scen_name = 'Scenario 1', source_modes = source_modes, 
                           target_modes = target_modes, source_distance_cats = DIST_CAT, 
                           source_trips = source_percentages)
    rd_list[[2]] <- rdr
    ###############################################################
    # Scenario 2
    rdr <- rd_list[[2]]
    # 35 % of all trips are bus.
    # These come from car and taxi.
    # All car and taxi trips > 6 km go to bus. Then 35 car and taxi trips 0--6 km go to bus.
    source_modes <- c('car', 'taxi')
    target_modes <- c('bus')
    target_new_trips <- round(0.35 * tt - sum(rdr$trip_mode=='bus'))
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
    rdr$trip_mode[match(bus_trips$trip_id,rdr$trip_id)] <- bus_trips$trip_mode
    rdr$trip_distance[match(bus_trips$trip_id,rdr$trip_id)] <- bus_trips$trip_distance
    rdr$stage_mode[match(bus_trips$trip_id,rdr$trip_id)] <- bus_trips$stage_mode
    rdr$stage_distance[match(bus_trips$trip_id,rdr$trip_id)] <- bus_trips$stage_distance
    rdr$stage_duration[match(bus_trips$trip_id,rdr$trip_id)] <- bus_trips$stage_duration
    rdr$trip_distance_cat[match(bus_trips$trip_id,rdr$trip_id)] <- bus_trips$trip_distance_cat
    
    rdr$scenario <- "Scenario 2"
    rd_list[[3]] <- rdr
    ###############################################################
    # Scenario 3
    rdr <- rd_list[[2]]
    # 16 % bus remain as is
    # 10 % Mcycle increase 
    # x decrease car
    source_modes <- c('car')
    target_modes <- c('motorcycle')
    target_new_trips <- max(round(0.1 * tt) - sum(rdr$trip_mode=='motorcycle'),1)
    mcycle_trips_sample <- create_scenario(rdr, scen_name = 'Scenario 3', source_modes = source_modes, 
                                           combined_modes = T, target_modes = target_modes, 
                                           source_distance_cats = DIST_CAT, source_trips = target_new_trips)
    # Update selected rows for mode and duration
    rdr$trip_mode[match(mcycle_trips_sample$trip_id,rdr$trip_id)] <- mcycle_trips_sample$trip_mode
    rdr$trip_distance[match(mcycle_trips_sample$trip_id,rdr$trip_id)] <- mcycle_trips_sample$trip_distance
    rdr$stage_mode[match(mcycle_trips_sample$trip_id,rdr$trip_id)] <- mcycle_trips_sample$stage_mode
    rdr$stage_distance[match(mcycle_trips_sample$trip_id,rdr$trip_id)] <- mcycle_trips_sample$stage_distance
    rdr$stage_duration[match(mcycle_trips_sample$trip_id,rdr$trip_id)] <- mcycle_trips_sample$stage_duration
    rdr$scenario <- "Scenario 3"
    rd_list[[4]] <- rdr
    #return(rd_list)
    ###############################################################
    # Scenario 4
    rdr <- rd_list[[2]]
    # 3.5 % Cycle
    source_modes <- c('motorcycle', 'car', 'taxi')
    target_modes <- c('cycle')
    mtrips <- max(min(52,sum(rdr$trip_mode == 'motorcycle')),1)
    btrips <- sum(rdr$trip_mode == 'cycle')
    ctrips <- max(min(round(0.035 * tt) - btrips - mtrips, sum(rdr$trip_mode %in% c('car', 'taxi')&rdr$trip_distance_cat==DIST_CAT[1])),1)
    target_new_trips <- c(mtrips, ctrips)
    mbike_trips <- create_scenario(rdr, scen_name = 'Scenario 4', source_modes = source_modes[1],combined_modes = T, 
                                   target_modes = target_modes,source_distance_cats = DIST_CAT,source_trips = target_new_trips[1])
    car_trips <- create_scenario(rdr, scen_name = 'Scenario 4', source_modes = c(source_modes[2], source_modes[3]),combined_modes = T, 
                                 target_modes = target_modes,source_distance_cats = DIST_CAT[1],source_trips = target_new_trips[2]) # todo: source_modes has 2 elements, so source_trips should too, otherwise it'll return an error?
    car_mbike_trips <- rbind(mbike_trips, car_trips)
    # Update selected rows for mode and duration
    rdr$trip_mode[match(car_mbike_trips$trip_id,rdr$trip_id)] <- car_mbike_trips$trip_mode
    rdr$trip_distance[match(car_mbike_trips$trip_id,rdr$trip_id)] <- car_mbike_trips$trip_distance
    rdr$stage_mode[match(car_mbike_trips$trip_id,rdr$trip_id)] <- car_mbike_trips$stage_mode
    rdr$stage_distance[match(car_mbike_trips$trip_id,rdr$trip_id)] <- car_mbike_trips$stage_distance
    rdr$stage_duration[match(car_mbike_trips$trip_id,rdr$trip_id)] <- car_mbike_trips$stage_duration
    rdr$scenario <- "Scenario 4"
    rd_list[[5]] <- rdr
    ###############################################################
    # Scenario 5
    rdr <- rd_list[[2]]
    # 3.5 % Cycle
    source_modes <- c('car', 'taxi')
    target_modes <- c('pedestrian')
    target_new_trips <- min(round(0.54 * tt) - sum(rdr$trip_mode == target_modes), sum(rdr$trip_mode%in%source_modes&rdr$trip_distance_cat==DIST_CAT[1]))
    motorised_trips <- create_scenario(rdr, scen_name = 'Scenario 4', source_modes = source_modes, combined_modes = T, 
                                       target_modes = target_modes,source_distance_cats = DIST_CAT[1],source_trips = target_new_trips)
    # Update selected rows for mode and duration
    rdr$trip_mode[match(motorised_trips$trip_id,rdr$trip_id)] <- motorised_trips$trip_mode
    rdr$trip_distance[match(motorised_trips$trip_id,rdr$trip_id)] <- motorised_trips$trip_distance
    rdr$stage_mode[match(motorised_trips$trip_id,rdr$trip_id)] <- motorised_trips$stage_mode
    rdr$stage_distance[match(motorised_trips$trip_id,rdr$trip_id)] <- motorised_trips$stage_distance
    rdr$stage_duration[match(motorised_trips$trip_id,rdr$trip_id)] <- motorised_trips$stage_duration
    rdr$scenario <- "Scenario 5"
    rd_list[[6]] <- rdr
    
    
    return(rd_list)
  }
  
  if(CITY == 'sao_paulo'){
    
    ###############################################################
    rdr <- trip_set
    tt <- length(unique(rdr$trip_id))
    rd_list <- list()
    rd_list[[1]] <- rdr
    # Scenario 1
    source_modes <- c('bus', 'pedestrian')
    target_modes <- c('car')
    source_percentages <- round(c(0.15, 0.20)* tt)
    rdr <- create_scenario(rdr, scen_name = 'Scenario 1', source_modes = source_modes, 
                           target_modes = target_modes, source_distance_cats = DIST_CAT, 
                           source_trips = source_percentages)
    rd_list[[2]] <- rdr
    ###############################################################
    # Scenario 2
    rdr <- rd_list[[2]]
    # 35 % of all trips are bus.
    # These come from car and taxi.
    # All car and taxi trips > 6 km go to bus. Then 35 car and taxi trips 0--6 km go to bus.
    source_modes <- c('car', 'taxi')
    target_modes <- c('bus')
    target_new_trips <- round(0.35 * tt - sum(rdr$trip_mode=='bus'))
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
    rdr$trip_mode[match(bus_trips$trip_id,rdr$trip_id)] <- bus_trips$trip_mode
    rdr$trip_distance[match(bus_trips$trip_id,rdr$trip_id)] <- bus_trips$trip_distance
    rdr$stage_mode[match(bus_trips$trip_id,rdr$trip_id)] <- bus_trips$stage_mode
    rdr$stage_distance[match(bus_trips$trip_id,rdr$trip_id)] <- bus_trips$stage_distance
    rdr$stage_duration[match(bus_trips$trip_id,rdr$trip_id)] <- bus_trips$stage_duration
    rdr$trip_distance_cat[match(bus_trips$trip_id,rdr$trip_id)] <- bus_trips$trip_distance_cat
    
    rdr$scenario <- "Scenario 2"
    rd_list[[3]] <- rdr
    
    ###############################################################
    # Scenario 3
    rdr <- rd_list[[2]]
    # 16 % bus remain as is
    # 10 % Mcycle increase 
    # x decrease car
    source_modes <- c('car')
    target_modes <- c('motorcycle')
    target_new_trips <- max(round(0.1 * tt) - sum(rdr$trip_mode=='motorcycle'),1)
    mcycle_trips_sample <- create_scenario(rdr, scen_name = 'Scenario 3', source_modes = source_modes, 
                                           combined_modes = T, target_modes = target_modes, 
                                           source_distance_cats = DIST_CAT, source_trips = target_new_trips)
    # Update selected rows for mode and duration
    rdr$trip_mode[match(mcycle_trips_sample$trip_id,rdr$trip_id)] <- mcycle_trips_sample$trip_mode
    rdr$trip_distance[match(mcycle_trips_sample$trip_id,rdr$trip_id)] <- mcycle_trips_sample$trip_distance
    rdr$stage_mode[match(mcycle_trips_sample$trip_id,rdr$trip_id)] <- mcycle_trips_sample$stage_mode
    rdr$stage_distance[match(mcycle_trips_sample$trip_id,rdr$trip_id)] <- mcycle_trips_sample$stage_distance
    rdr$stage_duration[match(mcycle_trips_sample$trip_id,rdr$trip_id)] <- mcycle_trips_sample$stage_duration
    rdr$scenario <- "Scenario 3"
    rd_list[[4]] <- rdr
    
    
    ###############################################################
    # Scenario 4
    rdr <- rd_list[[2]]
    # 3.5 % Cycle
    source_modes <- c('motorcycle', 'car', 'taxi')
    target_modes <- c('cycle')
    mtrips <- max(min(52,sum(rdr$trip_mode == 'motorcycle')),1)
    btrips <- sum(rdr$trip_mode == 'cycle')
    ctrips <- max(min(round(0.035 * tt) - btrips - mtrips, sum(rdr$trip_mode %in% c('car', 'taxi')&rdr$trip_distance_cat==DIST_CAT[1])),1)
    target_new_trips <- c(mtrips, ctrips)
    mbike_trips <- create_scenario(rdr, scen_name = 'Scenario 4', source_modes = source_modes[1],combined_modes = T, 
                                   target_modes = target_modes,source_distance_cats = DIST_CAT,source_trips = target_new_trips[1])
    car_trips <- create_scenario(rdr, scen_name = 'Scenario 4', source_modes = c(source_modes[2], source_modes[3]),combined_modes = T, 
                                 target_modes = target_modes,source_distance_cats = DIST_CAT[1],source_trips = target_new_trips[2])
    car_mbike_trips <- rbind(mbike_trips, car_trips)
    # Update selected rows for mode and duration
    rdr$trip_mode[match(car_mbike_trips$trip_id,rdr$trip_id)] <- car_mbike_trips$trip_mode
    rdr$trip_distance[match(car_mbike_trips$trip_id,rdr$trip_id)] <- car_mbike_trips$trip_distance
    rdr$stage_mode[match(car_mbike_trips$trip_id,rdr$trip_id)] <- car_mbike_trips$stage_mode
    rdr$stage_distance[match(car_mbike_trips$trip_id,rdr$trip_id)] <- car_mbike_trips$stage_distance
    rdr$stage_duration[match(car_mbike_trips$trip_id,rdr$trip_id)] <- car_mbike_trips$stage_duration
    rdr$scenario <- "Scenario 4"
    rd_list[[5]] <- rdr
    ###############################################################
    # Scenario 5
    rdr <- rd_list[[2]]
    # 3.5 % Cycle
    source_modes <- c('car', 'taxi')
    target_modes <- c('pedestrian')
    target_new_trips <- min(round(0.54 * tt) - sum(rdr$trip_mode == target_modes), sum(rdr$trip_mode%in%source_modes&rdr$trip_distance_cat==DIST_CAT[1]))
    motorised_trips <- create_scenario(rdr, scen_name = 'Scenario 4', source_modes = source_modes, combined_modes = T, 
                                       target_modes = target_modes,source_distance_cats = DIST_CAT[1],source_trips = target_new_trips)
    # Update selected rows for mode and duration
    rdr$trip_mode[match(motorised_trips$trip_id,rdr$trip_id)] <- motorised_trips$trip_mode
    rdr$trip_distance[match(motorised_trips$trip_id,rdr$trip_id)] <- motorised_trips$trip_distance
    rdr$stage_mode[match(motorised_trips$trip_id,rdr$trip_id)] <- motorised_trips$stage_mode
    rdr$stage_distance[match(motorised_trips$trip_id,rdr$trip_id)] <- motorised_trips$stage_distance
    rdr$stage_duration[match(motorised_trips$trip_id,rdr$trip_id)] <- motorised_trips$stage_duration
    rdr$scenario <- "Scenario 5"
    rd_list[[6]] <- rdr
    
    return(rd_list)
  }
  
  #----
  if (CITY == 'bogota_wb') {
    
    ###############################################################
    # Scenario 1: women's mode share is equal to men's
    rd_list <- list()
    
    rdr_full <- trip_set
    
    rd_list[[1]] <- rdr_full # Baseline
    
    # names(rdr)
    # cbind(table(trip_set$trip_mode, trip_set$sex),
    #       prop.table(table(trip_set$trip_mode, trip_set$sex),margin = 2)) #Proportion of each sex
    # table(trip_set$trip_distance_cat, trip_set$trip_mode)
    
    # subset of trips that are going to be changed
    rdr <- trip_set[trip_set$sex == "female",]
    
    # Total number of trips made by female
    tt <- nrow(rdr)
    
    # Define new proportions
    # This is the order of proportions: bicycle, bus, car, motorcycle, taxi, walk
    new_trips <- round(tt * c(#0.08327304572973, # cycle instead of bicycle
                              0.31468225259871,
                              0.12532840878803,
                              0.08327304572973,
                              0.01913338156342,
                              0.40378098465522, # Pedestrian instead of walk
                              0.05380192666489))
                              # 0.05380192666489,
                              # 0.40378098465522))
    
    # Difference of trips between baseline and scenario 1
    diff_trips <- new_trips - table(rdr$trip_mode)
    
    # Create new id to avoid duplicates at the end of the redistribution
    rdr$id <- 1:nrow(rdr)
    
    # Subset dataset with restrictions
    rdr2 <- subset(rdr, !(trip_motive %in% c(4,7,8)) &
                     (age >= 16 & age <= 62) &
                     (strptime(trip_start_time, "%H:%M") >= strptime("05:30", "%H:%M") &
                        strptime(trip_start_time, "%H:%M") <= strptime("23:30", "%H:%M")) &
                     (limitation == 0))
    
    # Redistribution of bus trips to bicycle on any distance
    source_modes <- c('bus')
    target_modes <- c('cycle')
    b2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 1',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[1]*-1)
    
    # Redistribution of car trips to bicycle on any distance
    source_modes <- c('car')
    target_modes <- c('cycle')
    c2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 1',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[2]*-1)
    
    # Redistribution of motorcycle trips to bicycle on any distance
    source_modes <- c('motorcycle')
    target_modes <- c('cycle')
    m2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 1',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[4]*-1)
    
    # Redistribution of taxi trips to bicycle on any distance
    source_modes <- c('taxi')
    target_modes <- c('cycle')
    t2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 1',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[6]*-1)
    
    # Redistribution of walking trips to bicycle on any distance
    source_modes <- c('pedestrian')
    target_modes <- c('cycle')
    w2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 1',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[5]*-1)
    
    redistribute_trips <- rbind(b2bb_trips, c2bb_trips, m2bb_trips,
                                t2bb_trips, w2bb_trips)
    
    # Update selected rows for mode and duration
    rdr$trip_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_mode
    rdr$trip_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_distance
    rdr$stage_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_mode
    rdr$stage_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_distance
    rdr$stage_duration[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_duration
    
    rdr_full <- rbind(rdr_full[rdr_full$sex == "male",], rdr[,-ncol(rdr)])
    rdr_full$scenario <- "Scenario 1"
    rd_list[[2]] <- rdr_full
    
    rm(rdr_full, rdr, tt, new_trips, diff_trips, rdr2, b2bb_trips, c2bb_trips,
       m2bb_trips, t2bb_trips, w2bb_trips, redistribute_trips, source_modes,
       target_modes)
    
    # ###############################################################
    # Scenario 2
    # To proportion of bicycle is the same for each socio-economical stratum. 
    # The value assigned is the stratum that have the highest proportion, which in
    # this case is stratum 2.
    rdr_full <- trip_set
    
    # cbind(table(trip_set$trip_mode, trip_set$strata),
    #       prop.table(table(trip_set$trip_mode, trip_set$strata),margin = 2)) #Proportion of each sex
    # table(trip_set$trip_distance_cat, trip_set$trip_mode)
    
    # strata 1
    # subset of trips that are going to be changed
    rdr <- trip_set[trip_set$strata == 1,]
    
    # Total number of trips made by strata 1
    tt <- nrow(rdr)
    
    # Define new proportions
    # This is the order of proportions: bicycle, bus, car, motorcycle, taxi, walk
    new_trips <- round(tt * c(#0.06476578411405, # cycle instead of bicycle
                              0.38932790224033,
                              0.03934826883910,
                              0.06476578411405,
                              0.05409368635438,
                              0.43560081466395, # Pedestrian instead of walk
                              0.01694501018330))
                              # 0.01694501018330,
                              # 0.43560081466395))
    
    # Difference of trips between baseline and scenario 2
    diff_trips <- new_trips - table(rdr$trip_mode)
    
    # Create new id to avoid duplicates at the end of the redistribution
    rdr$id <- 1:nrow(rdr)
    
    # Subset dataset with restrictions
    rdr2 <- subset(rdr, !(trip_motive %in% c(4,7,8)) &
                     (age >= 16 & age <= 62) &
                     (strptime(trip_start_time, "%H:%M") >= strptime("05:30", "%H:%M") &
                        strptime(trip_start_time, "%H:%M") <= strptime("23:30", "%H:%M")) &
                     (limitation == 0))
    
    # Redistribution of bus trips to bicycle on any distance
    source_modes <- c('bus')
    target_modes <- c('cycle')
    b2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[1]*-1)
    
    # Redistribution of car trips to bicycle on any distance
    source_modes <- c('car')
    target_modes <- c('cycle')
    c2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[2]*-1)
    
    # Redistribution of motorcycle trips to bicycle on any distance
    source_modes <- c('motorcycle')
    target_modes <- c('cycle')
    m2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[4]*-1)
    
    # Redistribution of taxi trips to bicycle on any distance
    source_modes <- c('taxi')
    target_modes <- c('cycle')
    t2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[6]*-1)
    
    # Redistribution of walking trips to bicycle on any distance
    source_modes <- c('pedestrian')
    target_modes <- c('cycle')
    w2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[5]*-1)
    
    redistribute_trips <- rbind(b2bb_trips, c2bb_trips, m2bb_trips, t2bb_trips,
                                w2bb_trips)
    
    # Update selected rows for mode and duration
    rdr$trip_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_mode
    rdr$trip_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_distance
    rdr$stage_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_mode
    rdr$stage_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_distance
    rdr$stage_duration[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_duration
    
    # Save updated trips in other object
    rdr_1 <- rdr
    
    #####
    ### strata 3
    # subset of trips that are going to be changed
    rdr <- trip_set[trip_set$strata == 3,]
    
    # Total number of trips made by strata 3
    tt <- nrow(rdr)
    
    # Define new proportions
    # This is the order of proportions: bicycle, bus, car, motorcycle, taxi, walk
    new_trips <- round(tt * c(#0.06474470699163, # cycle instead of bicycle
                              0.31338038229664,
                              0.15077573644961,
                              0.06474470699163,
                              0.04405840166862,
                              0.37515357581645, # Pedestrian instead of walk
                              0.05188719677705))
                              # 0.05188719677705,
                              # 0.37515357581645))
    
    # Difference of trips between baseline and scenario 2
    diff_trips <- new_trips - table(rdr$trip_mode)
    
    # Create new id to avoid duplicates at the end of the redistribution
    rdr$id <- 1:nrow(rdr)
    
    # Subset dataset with restrictions
    rdr2 <- subset(rdr, !(trip_motive %in% c(4,7,8)) &
                     (age >= 16 & age <= 62) &
                     (strptime(trip_start_time, "%H:%M") >= strptime("05:30", "%H:%M") &
                        strptime(trip_start_time, "%H:%M") <= strptime("23:30", "%H:%M")) &
                     (limitation == 0))
    
    # Redistribution of bus trips to bicycle on any distance
    source_modes <- c('bus')
    target_modes <- c('cycle')
    b2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[1]*-1)
    
    # Redistribution of car trips to bicycle on any distance
    source_modes <- c('car')
    target_modes <- c('cycle')
    c2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[2]*-1)
    
    # Redistribution of motorcycle trips to bicycle on any distance
    source_modes <- c('motorcycle')
    target_modes <- c('cycle')
    m2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[4]*-1)
    
    # Redistribution of taxi trips to bicycle on any distance
    source_modes <- c('taxi')
    target_modes <- c('cycle')
    t2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[6]*-1)
    
    # Redistribution of walking trips to bicycle on any distance
    source_modes <- c('pedestrian')
    target_modes <- c('cycle')
    w2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[5]*-1)
    
    redistribute_trips <- rbind(b2bb_trips, c2bb_trips, m2bb_trips, t2bb_trips,
                                w2bb_trips)
    
    # Update selected rows for mode and duration
    rdr$trip_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_mode
    rdr$trip_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_distance
    rdr$stage_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_mode
    rdr$stage_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_distance
    rdr$stage_duration[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_duration
    
    # Save updated trips in other object
    rdr_3 <- rdr
    
    ####
    # strata 4
    # subset of trips that are going to be changed
    rdr <- trip_set[trip_set$strata == 4,]
    
    # Total number of trips made by strata 4
    tt <- nrow(rdr)
    
    # Define new proportions
    # This is the order of proportions: bicycle, bus, car, motorcycle, taxi, walk
    new_trips <- round(tt * c(#0.06471421823335, # cycle instead of bicycle
                              0.25980160604629,
                              0.30278696268304,
                              0.06471421823335,
                              0.01747756258857,
                              0.26337809568797, # Pedestrian instead of walk
                              0.09177407382414))
                              # 0.09177407382414,
                              # 0.26337809568797))
    
    # Difference of trips between baseline and scenario 2
    diff_trips <- new_trips - table(rdr$trip_mode)
    
    # Create new id to avoid duplicates at the end of the redistribution
    rdr$id <- 1:nrow(rdr)
    
    # Subset dataset with restrictions
    rdr2 <- subset(rdr, !(trip_motive %in% c(4,7,8)) &
                     (age >= 16 & age <= 62) &
                     (strptime(trip_start_time, "%H:%M") >= strptime("05:30", "%H:%M") &
                        strptime(trip_start_time, "%H:%M") <= strptime("23:30", "%H:%M")) &
                     (limitation == 0))
    
    # Redistribution of bus trips to bicycle on any distance
    source_modes <- c('bus')
    target_modes <- c('cycle')
    b2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[1]*-1)
    
    # Redistribution of car trips to bicycle on any distance
    source_modes <- c('car')
    target_modes <- c('cycle')
    c2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[2]*-1)
    
    # Redistribution of motorcycle trips to bicycle on any distance
    source_modes <- c('motorcycle')
    target_modes <- c('cycle')
    m2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[4]*-1)
    
    # Redistribution of taxi trips to bicycle on any distance
    source_modes <- c('taxi')
    target_modes <- c('cycle')
    t2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[6]*-1)
    
    # Redistribution of walking trips to bicycle on any distance
    source_modes <- c('pedestrian')
    target_modes <- c('cycle')
    w2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[5]*-1)
    
    redistribute_trips <- rbind(b2bb_trips, c2bb_trips, m2bb_trips, t2bb_trips,
                                w2bb_trips)
    
    # Update selected rows for mode and duration
    rdr$trip_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_mode
    rdr$trip_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_distance
    rdr$stage_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_mode
    rdr$stage_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_distance
    rdr$stage_duration[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_duration
    
    # Save updated trips in other object
    rdr_4 <- rdr
    
    #####
    # strata 5
    # subset of trips that are going to be changed
    rdr <- trip_set[trip_set$strata == 5,]
    
    # Total number of trips made by strata 5
    tt <- nrow(rdr)
    
    # Define new proportions
    # This is the order of proportions: bicycle, bus, car, motorcycle, taxi, walk
    new_trips <- round(tt * c(#0.06476545842217, # cycle instead of bicycle
                              0.17937100213220,
                              0.42057569296375,
                              0.06476545842217,
                              0.01918976545842,
                              0.22547974413646, # Pedestrian instead of walk
                              0.09035181236674))
                              # 0.09035181236674,
                              # 0.22547974413646))
    
    # Difference of trips between baseline and scenario 2
    diff_trips <- new_trips - table(rdr$trip_mode)
    
    # Create new id to avoid duplicates at the end of the redistribution
    rdr$id <- 1:nrow(rdr)
    
    # Subset dataset with restrictions
    rdr2 <- subset(rdr, !(trip_motive %in% c(4,7,8)) &
                     (age >= 16 & age <= 62) &
                     (strptime(trip_start_time, "%H:%M") >= strptime("05:30", "%H:%M") &
                        strptime(trip_start_time, "%H:%M") <= strptime("23:30", "%H:%M")) &
                     (limitation == 0))
    
    # Redistribution of bus trips to bicycle on any distance
    source_modes <- c('bus')
    target_modes <- c('cycle')
    b2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[1]*-1)
    
    # Redistribution of car trips to bicycle on any distance
    source_modes <- c('car')
    target_modes <- c('cycle')
    c2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[2]*-1)
    
    # Redistribution of motorcycle trips to bicycle on any distance
    source_modes <- c('motorcycle')
    target_modes <- c('cycle')
    m2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[4]*-1)
    
    # Redistribution of taxi trips to bicycle on any distance
    source_modes <- c('taxi')
    target_modes <- c('cycle')
    t2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[6]*-1)
    
    # Redistribution of walking trips to bicycle on any distance
    source_modes <- c('pedestrian')
    target_modes <- c('cycle')
    w2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[5]*-1)
    
    redistribute_trips <- rbind(b2bb_trips, c2bb_trips, m2bb_trips, t2bb_trips,
                                w2bb_trips)
    
    # Update selected rows for mode and duration
    rdr$trip_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_mode
    rdr$trip_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_distance
    rdr$stage_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_mode
    rdr$stage_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_distance
    rdr$stage_duration[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_duration
    
    # Save updated trips in other object
    rdr_5 <- rdr
    
    # strata 6
    # subset of trips that are going to be changed
    rdr <- trip_set[trip_set$strata == 6,]
    
    # Total number of trips made by strata 6
    tt <- nrow(rdr)
    
    # Define new proportions
    # This is the order of proportions: bicycle, bus, car, motorcycle, taxi, walk
    new_trips <- round(tt * c(#0.06485484867202, # cycle instead of bicycle
                              0.10809141445337,
                              0.44379246448425,
                              0.06485484867202,
                              0.01142680667078,
                              0.28752316244595, # Pedestrian instead of walk
                              0.08431130327363))
                              # 0.08431130327363,
                              # 0.28752316244595))
    
    # Difference of trips between baseline and scenario 2
    diff_trips <- new_trips - table(rdr$trip_mode)
    
    # Create new id to avoid duplicates at the end of the redistribution
    rdr$id <- 1:nrow(rdr)
    
    # Subset dataset with restrictions
    rdr2 <- subset(rdr, !(trip_motive %in% c(4,7,8)) &
                     (age >= 16 & age <= 62) &
                     (strptime(trip_start_time, "%H:%M") >= strptime("05:30", "%H:%M") &
                        strptime(trip_start_time, "%H:%M") <= strptime("23:30", "%H:%M")) &
                     (limitation == 0))
    
    # Redistribution of bus trips to bicycle on any distance
    source_modes <- c('bus')
    target_modes <- c('cycle')
    b2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[1]*-1)
    
    # Redistribution of car trips to bicycle on any distance
    source_modes <- c('car')
    target_modes <- c('cycle')
    c2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[2]*-1)
    
    # Redistribution of motorcycle trips to bicycle on any distance
    source_modes <- c('motorcycle')
    target_modes <- c('cycle')
    m2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[4]*-1)
    
    # Redistribution of taxi trips to bicycle on any distance
    source_modes <- c('taxi')
    target_modes <- c('cycle')
    t2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[6]*-1)
    
    # Redistribution of walking trips to bicycle on any distance
    source_modes <- c('pedestrian')
    target_modes <- c('cycle')
    w2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[5]*-1)
    
    redistribute_trips <- rbind(b2bb_trips, c2bb_trips, m2bb_trips, t2bb_trips,
                                w2bb_trips)
    
    # Update selected rows for mode and duration
    rdr$trip_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_mode
    rdr$trip_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_distance
    rdr$stage_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_mode
    rdr$stage_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_distance
    rdr$stage_duration[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_duration
    
    # Save updated trips in other object
    rdr_6 <- rdr
    
    # Join all trips in a single dataset
    rdr_full <- rbind(rdr_full[rdr_full$strata == 2,], rdr_1[,-ncol(rdr_1)],
                      rdr_3[,-ncol(rdr_3)], rdr_4[,-ncol(rdr_4)],
                      rdr_5[,-ncol(rdr_5)], rdr_6[,-ncol(rdr_6)])
    rdr_full$scenario <- "Scenario 2"
    rd_list[[3]] <- rdr_full
    
    rm(rdr_full, rdr, tt, new_trips, diff_trips, rdr2, b2bb_trips, c2bb_trips, m2bb_trips, t2bb_trips, w2bb_trips, redistribute_trips, source_modes,
       target_modes, rdr_1, rdr_3, rdr_4, rdr_5, rdr_6)
    
    ###############################################################
    # Scenario 3: Duplicar viajes en bici, todos vienen del carro
    rdr_full <- trip_set
    
    # cbind(table(trip_set$trip_mode), prop.table(table(trip_set$trip_mode)))
    # table(trip_set$trip_distance_cat, trip_set$trip_mode)
    
    # subset of trips that are going to be changed
    rdr <- rdr_full
    
    # Total number of trips made by female
    tt <- nrow(rdr)
    
    # Define new proportions
    # This is the order of proportions: bicycle, bus, car, motorcycle, taxi, walk
    new_trips <- round(tt * c(#0.10327060481031, # cycle instead of bicycle
                              0.31946261447117,
                              0.10407567676361,
                              0.10327060481031,
                              0.04491295159505,
                              0.37886686122572, # Pedestrian instead of walk
                              0.04941129113415))
                              # 0.04941129113415,
                              # 0.37886686122572))
    
    # Difference of trips between baseline and scenario 3
    diff_trips <- new_trips - table(rdr$trip_mode)
    
    # Create new id to avoid duplicates at the end of the redistribution
    rdr$id <- 1:nrow(rdr)
    
    # Subset dataset with restrictions
    rdr2 <- subset(rdr, !(trip_motive %in% c(4,7,8)) &
                     (age >= 16 & age <= 62) &
                     (strptime(trip_start_time, "%H:%M") >= strptime("05:30", "%H:%M") &
                        strptime(trip_start_time, "%H:%M") <= strptime("23:30", "%H:%M")) &
                     (limitation == 0))
    
    # Define weights to give priority to short and medium distances
    rdr2$w <- NA
    rdr2$w[rdr2$trip_distance_cat == "0-6 km"] <- 10
    rdr2$w[rdr2$trip_distance_cat == "7-15 km"] <- 10
    rdr2$w[rdr2$trip_distance_cat == "16+ km"] <- 1
    
    # Redistribution of car trips to bicycle on any distance
    source_modes <- c('car')
    target_modes <- c('cycle')
    c2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 3',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT,
                                  source_trips = diff_trips[2]*-1)
    
    redistribute_trips <- c2bb_trips
    
    # Update selected rows for mode and duration
    rdr$trip_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_mode
    rdr$trip_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_distance
    rdr$stage_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_mode
    rdr$stage_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_distance
    rdr$stage_duration[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_duration
    
    rdr_full <- rdr[,-ncol(rdr)]
    rdr_full$scenario <- "Scenario 3"
    rd_list[[4]] <- rdr_full
    
    rm(rdr_full, rdr, tt, new_trips, diff_trips, rdr2, c2bb_trips,
       redistribute_trips, source_modes, target_modes)
    # cbind(table(rd_list[[4]]$trip_mode),
    #       prop.table(table(rd_list[[4]]$trip_mode)))
    
    
    ###############################################################
    # Scenario 4: Duplicar viajes en bici, todos vienen de transporte privado,
    # Carro y moto
    rdr_full <- trip_set
    
    # cbind(table(trip_set$trip_mode), prop.table(table(trip_set$trip_mode)))
    # table(trip_set$trip_distance_cat, trip_set$trip_mode)
    
    # subset of trips that are going to be changed
    rdr <- rdr_full
    
    # Total number of trips made by female
    tt <- nrow(rdr)
    
    # Define new proportions
    # This is the order of proportions: bicycle, bus, car, motorcycle, taxi, walk
    new_trips <- round(tt * c(#0.10327060481031, # cycle instead of bicycle
                              0.31946261447117,
                              0.11651403844219,
                              0.10327060481031,
                              0.03247458991647,
                              0.37886686122572, # Pedestrian instead of walk
                              0.04941129113415))
                              # 0.04941129113415,
                              # 0.37886686122572))
    
    # Difference of trips between baseline and scenario 3
    diff_trips <- new_trips - table(rdr$trip_mode)
    
    # Create new id to avoid duplicates at the end of the redistribution
    rdr$id <- 1:nrow(rdr)
    
    # Subset dataset with restrictions
    rdr2 <- subset(rdr, !(trip_motive %in% c(4,7,8)) &
                     (age >= 16 & age <= 62) &
                     (strptime(trip_start_time, "%H:%M") >= strptime("05:30", "%H:%M") &
                        strptime(trip_start_time, "%H:%M") <= strptime("23:30", "%H:%M")) &
                     (limitation == 0))
    
    # Define weights to give priority to short and medium distances
    rdr2$w <- NA
    rdr2$w[rdr2$trip_distance_cat == "0-6 km"] <- 10
    rdr2$w[rdr2$trip_distance_cat == "7-15 km"] <- 10
    rdr2$w[rdr2$trip_distance_cat == "16+ km"] <- 1
    
    # Redistribution of car trips to bicycle on any distance
    source_modes <- c('car')
    target_modes <- c('cycle')
    c2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 4',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT,
                                  source_trips = diff_trips[2]*-1)
    
    # Redistribution of motorcycle trips to bicycle on any distance
    source_modes <- c('motorcycle')
    target_modes <- c('cycle')
    m2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 4',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[4]*-1)
    
    redistribute_trips <- rbind(c2bb_trips, m2bb_trips)
    
    # Update selected rows for mode and duration
    rdr$trip_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_mode
    rdr$trip_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_distance
    rdr$stage_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_mode
    rdr$stage_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_distance
    rdr$stage_duration[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_duration
    
    rdr_full <- rdr[,-ncol(rdr)]
    rdr_full$scenario <- "Scenario 4"
    rd_list[[5]] <- rdr_full
    
    rm(rdr_full, rdr, tt, new_trips, diff_trips, rdr2, c2bb_trips, m2bb_trips,
       redistribute_trips, source_modes, target_modes)
    # cbind(table(rd_list[[5]]$trip_mode),
    #       prop.table(table(rd_list[[5]]$trip_mode)))
    
    ###############################################################
    # Scenario 5: Duplicar viajes en bici, todos vienen del transporte publico
    rdr_full <- trip_set
    
    # cbind(table(trip_set$trip_mode), prop.table(table(trip_set$trip_mode)))
    # table(trip_set$trip_distance_cat, trip_set$trip_mode)
    
    # subset of trips that are going to be changed
    rdr <- rdr_full
    
    # Total number of trips made by female
    tt <- nrow(rdr)
    
    # Define new proportions
    # This is the order of proportions: bicycle, bus, car, motorcycle, taxi, walk
    new_trips <- round(tt * c(#0.10327060481031, # cycle instead of bicycle
                              0.26782731206602,
                              0.15571097916876,
                              0.10327060481031,
                              0.04491295159505,
                              0.37886686122572, # Pedestrian instead of walk
                              0.04941129113415))
                              # 0.04941129113415,
                              # 0.37886686122572))
    
    # Difference of trips between baseline and scenario 3
    diff_trips <- new_trips - table(rdr$trip_mode)
    
    # Create new id to avoid duplicates at the end of the redistribution
    rdr$id <- 1:nrow(rdr)
    
    # Subset dataset with restrictions
    rdr2 <- subset(rdr, !(trip_motive %in% c(4,7,8)) &
                     (age >= 16 & age <= 62) &
                     (strptime(trip_start_time, "%H:%M") >= strptime("05:30", "%H:%M") &
                        strptime(trip_start_time, "%H:%M") <= strptime("23:30", "%H:%M")) &
                     (limitation == 0))
    
    # Define weights to give priority to short and medium distances
    rdr2$w <- NA
    rdr2$w[rdr2$trip_distance_cat == "0-6 km"] <- 10
    rdr2$w[rdr2$trip_distance_cat == "7-15 km"] <- 10
    rdr2$w[rdr2$trip_distance_cat == "16+ km"] <- 1
    
    # Redistribution of bus trips to bicycle on any distance
    source_modes <- c('bus')
    target_modes <- c('cycle')
    b2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 5',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT,
                                  source_trips = diff_trips[1]*-1)
    
    redistribute_trips <- b2bb_trips
    
    # Update selected rows for mode and duration
    rdr$trip_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_mode
    rdr$trip_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_distance
    rdr$stage_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_mode
    rdr$stage_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_distance
    rdr$stage_duration[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_duration
    
    rdr_full <- rdr[,-ncol(rdr)]
    rdr_full$scenario <- "Scenario 5"
    rd_list[[6]] <- rdr_full
    
    rm(rdr_full, rdr, tt, new_trips, diff_trips, rdr2, b2bb_trips,
       redistribute_trips, source_modes, target_modes)
    
    ###############################################################
    # Scenario 6: Duplicar viajes en bici, todos vienen a pie
    rdr_full <- trip_set
    
    # cbind(table(trip_set$trip_mode), prop.table(table(trip_set$trip_mode)))
    # table(trip_set$trip_distance_cat, trip_set$trip_mode)
    
    # subset of trips that are going to be changed
    rdr <- rdr_full
    
    # Total number of trips made by female
    tt <- nrow(rdr)
    
    # Define new proportions
    # This is the order of proportions: bicycle, bus, car, motorcycle, taxi, walk
    new_trips <- round(tt * c(#0.10327060481031, # cycle instead of bicycle
                              0.31946261447117,
                              0.15571097916876,
                              0.10327060481031,
                              0.04491295159505,
                              0.32723155882057, # Pedestrian instead of walk
                              0.04941129113415))
                              # 0.04941129113415,
                              # 0.32723155882057))
    
    # Difference of trips between baseline and scenario 3
    diff_trips <- new_trips - table(rdr$trip_mode)
    
    # Create new id to avoid duplicates at the end of the redistribution
    rdr$id <- 1:nrow(rdr)
    
    # Subset dataset with restrictions
    rdr2 <- subset(rdr, !(trip_motive %in% c(4,7,8)) &
                     (age >= 16 & age <= 62) &
                     (strptime(trip_start_time, "%H:%M") >= strptime("05:30", "%H:%M") &
                        strptime(trip_start_time, "%H:%M") <= strptime("23:30", "%H:%M")) &
                     (limitation == 0))
    
    # Redistribution of walking trips to bicycle on any distance
    source_modes <- c('pedestrian')
    target_modes <- c('cycle')
    w2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 6',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[5]*-1)
    
    redistribute_trips <- w2bb_trips
    
    # Update selected rows for mode and duration
    rdr$trip_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_mode
    rdr$trip_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_distance
    rdr$stage_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_mode
    rdr$stage_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_distance
    rdr$stage_duration[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_duration
    
    rdr_full <- rdr[,-ncol(rdr)]
    rdr_full$scenario <- "Scenario 6"
    rd_list[[7]] <- rdr_full
    
    rm(rdr_full, rdr, tt, new_trips, diff_trips, rdr2, w2bb_trips,
       redistribute_trips, source_modes, target_modes)
    
    ###############################################################
    # Scenario 7: ciudad expandida y dependiente del carro
    rdr_full <- trip_set
    
    # cbind(table(trip_set$trip_mode), prop.table(table(trip_set$trip_mode)))
    # table(trip_set$trip_distance_cat, trip_set$trip_mode)
    
    # subset of trips that are going to be changed
    rdr <- rdr_full
    
    # Total number of trips made by female
    tt <- nrow(rdr)
    
    # Define new proportions
    # This is the order of proportions: bicycle, bus, car, motorcycle, taxi, walk
    new_trips <- round(tt * c(#0.05121642758680, # cycle instead of bicycle
                              0.28518396888884,
                              0.18533738859104,
                              0.05121642758680,
                              0.05345833162812,
                              0.37579342541177, # Pedestrian instead of walk
                              0.04901045789343))
                              # 0.04901045789343,
                              # 0.37579342541177))
    
    # Difference of trips between baseline and scenario 7
    diff_trips <- new_trips - table(rdr$trip_mode)
    
    # Create new id to avoid duplicates at the end of the redistribution
    rdr$id <- 1:nrow(rdr)
    
    # Redistribution of walking trips to motorcycle on medium and large distance
    source_modes <- c('pedestrian')
    target_modes <- c('motorcycle')
    w2m_trips <- create_scenario(rdr, scen_name = 'Scenario 7',
                                 source_modes = source_modes,
                                 combined_modes = T,
                                 target_modes = target_modes,
                                 source_distance_cats = DIST_CAT[-1],
                                 source_trips = diff_trips[5]*-1)
    
    # Redistribution of taxi trips to motorcycle on any distance
    source_modes <- c('taxi')
    target_modes <- c('motorcycle')
    t2m_trips <- create_scenario(rdr, scen_name = 'Scenario 7',
                                 source_modes = source_modes,
                                 combined_modes = T,
                                 target_modes = target_modes,
                                 source_distance_cats = DIST_CAT,
                                 source_trips = diff_trips[6]*-1)
    
    # Redistribution of bicycle trips to motorcycle on any distance
    source_modes <- c('cycle')
    target_modes <- c('motorcycle')
    bb2m_trips <- create_scenario(rdr, scen_name = 'Scenario 7',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT,
                                  source_trips = diff_trips[3]*-1)
    
    # Redistribution of bus trips to car on any distance
    source_modes <- c('bus')
    target_modes <- c('car')
    b2c_trips <- create_scenario(rdr, scen_name = 'Scenario 7',
                                 source_modes = source_modes,
                                 combined_modes = T,
                                 target_modes = target_modes,
                                 source_distance_cats = DIST_CAT,
                                 source_trips = diff_trips[2])
    
    # Redistribution of bus trips to motorcycle on any distance
    source_modes <- c('bus')
    target_modes <- c('motorcycle')
    remaining_m <- diff_trips[4] + diff_trips[6] + diff_trips[5] + diff_trips[3]
    rdr2 <- rdr[-match(b2c_trips$id,rdr$id),]
    b2m_trips <- create_scenario(rdr2, scen_name = 'Scenario 7',
                                 source_modes = source_modes,
                                 combined_modes = T,
                                 target_modes = target_modes,
                                 source_distance_cats = DIST_CAT,
                                 source_trips = remaining_m)
    
    
    redistribute_trips <- rbind(w2m_trips, t2m_trips, bb2m_trips, b2c_trips,
                                b2m_trips)
    
    # Update selected rows for mode and duration
    rdr$trip_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_mode
    rdr$trip_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_distance
    rdr$stage_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_mode
    rdr$stage_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_distance
    rdr$stage_duration[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_duration
    
    rdr_full <- rdr[,-ncol(rdr)]
    rdr_full$scenario <- "Scenario 7"
    rd_list[[8]] <- rdr_full
    
    rm(rdr_full, rdr, tt, new_trips, diff_trips, rdr2, w2m_trips, t2m_trips,
       bb2m_trips, b2c_trips, b2m_trips, remaining_m,
       redistribute_trips, source_modes, target_modes)
    
    ###############################################################
    # Scenario 8: ciudad densa y transporte público
    rdr_full <- trip_set
    
    # cbind(table(trip_set$trip_mode), prop.table(table(trip_set$trip_mode)))
    # table(trip_set$trip_distance_cat, trip_set$trip_mode)
    
    # subset of trips that are going to be changed
    rdr <- rdr_full
    
    # Total number of trips made by female
    tt <- nrow(rdr)
    
    # Define new proportions
    # This is the order of proportions: bicycle, bus, car, motorcycle, taxi, walk
    new_trips <- round(tt * c(#0.05500569514238, # cycle instead of bicycle
                              0.34031490787270,
                              0.12440603015075,
                              0.05500569514238,
                              0.03588341708543,
                              0.40359664991625, # Pedestrian instead of walk
                              0.04079329983250))
                              # 0.04079329983250,
                              # 0.40359664991625))
    
    # Difference of trips between baseline and scenario 7
    diff_trips <- new_trips - table(rdr$trip_mode)
    
    # Create new id to avoid duplicates at the end of the redistribution
    rdr$id <- 1:nrow(rdr)
    
    # Redistribution of car trips to bicycle on any distance
    source_modes <- c('car')
    target_modes <- c('cycle')
    
    # Subset dataset with restrictions
    rdr2 <- subset(rdr, !(trip_motive %in% c(4,7,8)) &
                     (age >= 16 & age <= 62) &
                     (strptime(trip_start_time, "%H:%M") >= strptime("05:30", "%H:%M") &
                        strptime(trip_start_time, "%H:%M") <= strptime("23:30", "%H:%M")) &
                     (limitation == 0))
    
    c2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 8',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[3])
    
    # Redistribution of car trips to bus on any distance
    source_modes <- c('car')
    target_modes <- c('bus')
    # bus trips shouldnt be the same as those chosen to be bicycle
    rdr3 <- rdr[-match(c2bb_trips$id,rdr$id),]
    c2b_trips <- create_scenario(rdr3, scen_name = 'Scenario 8',
                                 source_modes = source_modes,
                                 combined_modes = T,
                                 target_modes = target_modes,
                                 source_distance_cats = DIST_CAT,
                                 source_trips = diff_trips[1])
    
    # Redistribution of car trips to walk on short distance
    source_modes <- c('car')
    target_modes <- c('pedestrian')
    remaining_c <- (diff_trips[2] + diff_trips[3] + diff_trips[1])*-1
    # walking trips shouldnt be the same as those chosen to be bicycle
    rdr4 <- rdr3[-match(c2b_trips$id,rdr3$id),]
    c2w_trips <- create_scenario(rdr4, scen_name = 'Scenario 8',
                                 source_modes = source_modes,
                                 combined_modes = T,
                                 target_modes = target_modes,
                                 source_distance_cats = DIST_CAT[1],
                                 source_trips = remaining_c)
    
    # Redistribution of motorcycle trips to walk on short distance
    source_modes <- c('motorcycle')
    target_modes <- c('pedestrian')
    
    # Define weights to give priority to short and medium distances
    rdr5 <- rdr
    rdr5$w <- NA
    rdr5$w[rdr2$trip_distance_cat == "0-6 km"] <- 100
    rdr5$w[rdr2$trip_distance_cat == "7-15 km"] <- 10
    rdr5$w[rdr2$trip_distance_cat == "16+ km"] <- 1
    
    m2w_trips <- create_scenario(rdr5, scen_name = 'Scenario 8',
                                 source_modes = source_modes,
                                 combined_modes = T,
                                 target_modes = target_modes,
                                 source_distance_cats = DIST_CAT[-3],
                                 source_trips = diff_trips[4]*-1)
    
    # Redistribution of taxi trips to walk on short distance
    source_modes <- c('taxi')
    target_modes <- c('pedestrian')
    t2w_trips <- create_scenario(rdr5, scen_name = 'Scenario 8',
                                 source_modes = source_modes,
                                 combined_modes = T,
                                 target_modes = target_modes,
                                 source_distance_cats = DIST_CAT[-3],
                                 source_trips = diff_trips[6]*-1)
    
    redistribute_trips <- rbind(c2b_trips, c2bb_trips, c2w_trips,
                                m2w_trips[,-ncol(m2w_trips)],
                                t2w_trips[,-ncol(t2w_trips)])
    
    # Update selected rows for mode and duration
    rdr$trip_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_mode
    rdr$trip_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_distance
    rdr$stage_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_mode
    rdr$stage_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_distance
    rdr$stage_duration[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_duration
    
    rdr_full <- rdr[,-ncol(rdr)]
    rdr_full$scenario <- "Scenario 8"
    rd_list[[9]] <- rdr_full
    
    rm(rdr_full, rdr, tt, new_trips, diff_trips, c2b_trips, c2bb_trips,
       c2w_trips, m2w_trips, t2w_trips, remaining_c, rdr2, rdr3, rdr4,
       redistribute_trips, source_modes, target_modes)
    
    ###############################################################
    # Scenario 9: ciudad compartida post COVID
    rdr_full <- trip_set
    
    # cbind(table(trip_set$trip_mode), prop.table(table(trip_set$trip_mode)))
    # table(trip_set$trip_distance_cat, trip_set$trip_mode)
    
    # subset of trips that are going to be changed
    rdr <- rdr_full
    
    # Total number of trips made by female
    tt <- nrow(rdr)
    
    # Define new proportions
    # This is the order of proportions: bicycle, bus, car, motorcycle, taxi, walk
    new_trips <- round(tt * c(#0.05606484443955, # cycle instead of bicycle
                              0.33138259403709,
                              0.14189691819549,
                              0.05606484443955,
                              0.04092845252417,
                              0.38565751013329, # Pedestrian instead of walk
                              0.04406968067042))
                              # 0.04406968067042,
                              # 0.38565751013329))
    
    # Difference of trips between baseline and scenario 7
    diff_trips <- new_trips - table(rdr$trip_mode)
    
    # Create new id to avoid duplicates at the end of the redistribution
    rdr$id <- 1:nrow(rdr)
    
    # Subset dataset with restrictions
    rdr2 <- subset(rdr, !(trip_motive %in% c(4,7,8)) &
                     (age >= 16 & age <= 62) &
                     (strptime(trip_start_time, "%H:%M") >= strptime("05:30", "%H:%M") &
                        strptime(trip_start_time, "%H:%M") <= strptime("23:30", "%H:%M")) &
                     (limitation == 0))
    
    # Redistribution of motorcycle trips to bicycle on short and medium distance
    source_modes <- c('motorcycle')
    target_modes <- c('cycle')
    m2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 9',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[4]*-1)
    
    # Redistribution of taxi trips to bicycle on short and medium distance
    source_modes <- c('taxi')
    target_modes <- c('cycle')
    t2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 9',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[3] + diff_trips[4])
    
    # Redistribution of taxi trips to bus on any distance
    source_modes <- c('taxi')
    target_modes <- c('bus')
    remaining_t <- (diff_trips[6] + diff_trips[3] + diff_trips[4])*-1
    # bus trips shouldnt be the same as those chosen to be bicycle
    rdr3 <- rdr[-match(t2bb_trips$id,rdr$id),]
    t2b_trips <- create_scenario(rdr3, scen_name = 'Scenario 9',
                                 source_modes = source_modes,
                                 combined_modes = T,
                                 target_modes = target_modes,
                                 source_distance_cats = DIST_CAT,
                                 source_trips = remaining_t)
    
    # Redistribution of car trips to walking on short distance
    source_modes <- c('car')
    target_modes <- c('pedestrian')
    c2w_trips <- create_scenario(rdr, scen_name = 'Scenario 9',
                                 source_modes = source_modes,
                                 combined_modes = T,
                                 target_modes = target_modes,
                                 source_distance_cats = DIST_CAT[1],
                                 source_trips = diff_trips[5])
    
    # Redistribution of car trips to bus on any distance
    source_modes <- c('car')
    target_modes <- c('bus')
    # bus trips shouldnt be the same as those chosen to be walking
    rdr4 <- rdr[-match(c2w_trips$id,rdr$id),]
    c2b_trips <- create_scenario(rdr4, scen_name = 'Scenario 9',
                                 source_modes = source_modes,
                                 combined_modes = T,
                                 target_modes = target_modes,
                                 source_distance_cats = DIST_CAT,
                                 source_trips = (diff_trips[2] + diff_trips[5])*-1)
    
    redistribute_trips <- rbind(c2w_trips, c2b_trips, t2b_trips, t2bb_trips,
                                m2bb_trips)
    
    # Update selected rows for mode and duration
    rdr$trip_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_mode
    rdr$trip_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_distance
    rdr$stage_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_mode
    rdr$stage_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_distance
    rdr$stage_duration[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_duration
    
    rdr_full <- rdr[,-ncol(rdr)]
    rdr_full$scenario <- "Scenario 9"
    rd_list[[10]] <- rdr_full
    
    rm(rdr_full, rdr, tt, new_trips, diff_trips, c2w_trips, c2b_trips,
       t2b_trips, t2bb_trips, rdr2, rdr3, rdr4, remaining_t, m2bb_trips,
       redistribute_trips, source_modes, target_modes)
    
    ###############################################################
    # Scenario 10: Escenario de Javier Peña
    rdr_full <- trip_set
    
    # cbind(table(trip_set$trip_mode), prop.table(table(trip_set$trip_mode)))
    # table(trip_set$trip_distance_cat, trip_set$trip_mode)
    
    # subset of trips that are going to be changed
    rdr <- rdr_full
    
    # Total number of trips made by female
    tt <- nrow(rdr)
    
    # Define new proportions
    # This is the order of proportions: bicycle, bus, car, motorcycle, taxi, walk
    new_trips <- round(tt * c(#0.10359627867295, # cycle instead of bicycle
                              0.30565187390953,
                              0.13983792956414,
                              0.10359627867295,
                              0.04048315192109,
                              0.36660415642052, # Pedestrian instead of walk
                              0.04382660951177))
                              # 0.04382660951177,
                              # 0.36660415642052))
    
    # Difference of trips between baseline and scenario 3
    diff_trips <- new_trips - table(rdr$trip_mode)
    
    # Create new id to avoid duplicates at the end of the redistribution
    rdr$id <- 1:nrow(rdr)
    
    # Subset dataset with restrictions
    rdr2 <- subset(rdr, !(trip_motive %in% c(4,7,8)) &
                     (age >= 16 & age <= 62) &
                     (strptime(trip_start_time, "%H:%M") >= strptime("05:30", "%H:%M") &
                        strptime(trip_start_time, "%H:%M") <= strptime("23:30", "%H:%M")) &
                     (limitation == 0))
    
    # Redistribution of bus trips to bicycle on short and medium distance
    source_modes <- c('bus')
    target_modes <- c('cycle')
    b2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 10',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[1]*-1)
    
    # Redistribution of car trips to bicycle on short and medium distance
    source_modes <- c('car')
    target_modes <- c('cycle')
    c2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 10',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[2]*-1)
    
    # Redistribution of motorcycle trips to bicycle on short and medium distance
    source_modes <- c('motorcycle')
    target_modes <- c('cycle')
    m2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 10',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[4]*-1)
    
    # Redistribution of taxi trips to bicycle on short and medium distance
    source_modes <- c('taxi')
    target_modes <- c('cycle')
    t2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 10',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[6]*-1)
    
    # Redistribution of walk trips to bicycle on short and medium distance
    source_modes <- c('pedestrian')
    target_modes <- c('cycle')
    w2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 10',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[5]*-1)
    
    redistribute_trips <- rbind(b2bb_trips, c2bb_trips, m2bb_trips, t2bb_trips,
                                w2bb_trips)
    
    # Update selected rows for mode and duration
    rdr$trip_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_mode
    rdr$trip_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_distance
    rdr$stage_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_mode
    rdr$stage_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_distance
    rdr$stage_duration[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_duration
    
    rdr_full <- rdr[,-ncol(rdr)]
    rdr_full$scenario <- "Scenario 10"
    rd_list[[11]] <- rdr_full
    
    rm(rdr_full, rdr, tt, new_trips, diff_trips, rdr2, b2bb_trips, c2bb_trips,
       m2bb_trips, t2bb_trips, w2bb_trips, redistribute_trips, source_modes,
       target_modes)
    
    return(rd_list)
  }
}
