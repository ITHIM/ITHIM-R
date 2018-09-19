## global variables (i.e. those needed by all functions) are assigned to all environments, using 
## <<- and assign(...,pos=1). A better method might be to make an object (list) that contains 
## all inputs, variables, and intermediate objects
ithim_setup_global_values <- function(plotFlag = F,
                                      SAMPLEMODE = F,
                                      NSAMPLES = 1,
                                      what_if_scenarios = 2,
                                      PA_MULT = c(1, 2),
                                      pa_certainty = T,
                                      RSEED = c(1:100),
                                      MEAN_BUS_WALK_TIME= 5,
                                      modes = c("Bus", "Private Car", "Taxi", "Walking","Short Walking", "Bicycle", "Motorcycle"),
                                      speeds = c(15, 21, 21, 4.8, 4.8, 14.5, 25),
                                      DIST_CAT = c("0-6 km", "7-9 km", "10+ km"),
                                      AGE_CATEGORY = c("15-49", "50-69", "70+")){
  
  ## PROGRAMMING VARIABLES
  plotFlag <<- plotFlag
  SAMPLEMODE <<- SAMPLEMODE
  NSAMPLES <<- NSAMPLES
  what_if_scenarios <<- what_if_scenarios
  PA_MULT <<- PA_MULT
  pa_certainty <<- pa_certainty
  RSEED <<- RSEED
  
  ## MODEL VARIABLES
  MEAN_BUS_WALK_TIME <<- MEAN_BUS_WALK_TIME
  MODE_SPEEDS <<- data.frame(trip_mode=modes, speed=speeds,stringsAsFactors=F)
  DIST_CAT <<- DIST_CAT
  DIST_LOWER_BOUNDS <<- as.numeric(sapply(strsplit(DIST_CAT,"[^0-9]+"),function(x)x[1]))
  AGE_CATEGORY <<- AGE_CATEGORY
  AGE_LOWER_BOUNDS <<- as.numeric(sapply(strsplit(AGE_CATEGORY,"[^0-9]+"),function(x)x[1]))
}

ithim_setup_parameters <- function(MMETCycling = 4.63,
                                   MMETWalking = 2.53,
                                   MMETEbikes = 3.50,
                                   PM_CONC_BASE = 50,  
                                   PM_TRANS_SHARE = 0.225 ){
  ## PARAMETERS
  parameters <- list()
  if(length(MMETCycling)==1 || SAMPLEMODE == F) {
    MMETCycling <<- MMETCycling
  }else{
    parameters$MMETCycling <- Lnorm(MMETCycling[1],MMETCycling[2])
  }
  if(length(MMETWalking)==1 || SAMPLEMODE == F) {
    MMETWalking <<- MMETWalking
  }else{
    parameters$MMETWalking <- Lnorm(MMETWalking[1],MMETWalking[2])
  }
  if(length(MMETEbikes)==1 || SAMPLEMODE == F) {
    MMETEbikes <<- MMETEbikes
  }else{
    parameters$MMETEbikes <- Lnorm(MMETEbikes[1],MMETEbikes[2])
  }
  if(length(PM_CONC_BASE)==1 || SAMPLEMODE == F) {
    PM_CONC_BASE <<- PM_CONC_BASE
  }else{
    parameters$PM_CONC_BASE <- Lnorm(PM_CONC_BASE[1],PM_CONC_BASE[2])
  }
  if(length(PM_TRANS_SHARE)==1 || SAMPLEMODE == F) {
    PM_TRANS_SHARE <<- PM_TRANS_SHARE
  }else{
    parameters$PM_TRANS_SHARE <- Beta(PM_TRANS_SHARE[1],PM_TRANS_SHARE[2])
  }
  parameters
}

sample_parameters <- function(parameters){
  for(i in 1:length(parameters))
    assign(names(parameters)[i],r(parameters[[i]])(1),pos=1)
}

## this function requires path specification, so that it may differ for different case studies
ithim_load_data <- function(){
  ## DATA FILES FOR MODEL  
  DR_AP <<- read.csv("data/dose_response/AP/dose_response_AP.csv")
  DISEASE_OUTCOMES <<- read.csv("data/dose_response/disease_outcomes_lookup.csv")
  S.I.N <<- read_csv('code/injuries/data/sin_coefficients_pairs.csv')
  list_of_files <- list.files(path = "data/drpa/extdata/", recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)
  for (i in 1:length(list_of_files)){
    assign(stringr::str_sub(basename(list_of_files[[i]]), end = -5), read_csv(list_of_files[[i]]),pos=1)
  }
  
  ## DATA FILES FOR ACCRA
  RD <<- read_csv("data/synth_pop_data/accra/travel_survey/synthetic_population_with_trips.csv")
  trans_emissions_file <- read_csv("data/emission calculations accra/transport_emission_inventory_accra.csv")
  names(trans_emissions_file) <- c("vehicle_type", "delhi_fleet_2011", "delhi_fleet_perHH", "accra_fleet_2010", "PM2_5_emiss_fact", "base_emissions")
  TRANS_EMISSIONS_ORIGINAL <<- trans_emissions_file
  lookup_ratio_pm_file <-  read_csv('data/synth_pop_data/accra/pollution/pm_exposure_ratio_look_up.csv')
  lookup_ratio_pm_file <- rename(lookup_ratio_pm_file, trip_mode = Mode)
  LOOKUP_RATIO_PM <<- lookup_ratio_pm_file
  WHW_MAT <<- read_csv('code/injuries/accra/who_hit_who_accra.csv')
  GBD_DATA <<- read_csv('data/demographics/gbd/accra/GBD Accra.csv')
  gbd_injuries <- GBD_DATA[which(GBD_DATA$cause=="Road injuries"),]
  gbd_injuries$sex_age <- paste0(gbd_injuries$sex,"_",gbd_injuries$age)
  ## calculating the ratio of YLL to deaths for each age and sex group
  gbd_injuries <- arrange(gbd_injuries, measure)
  gbd_inj_yll <- gbd_injuries[which(gbd_injuries$measure=="YLLs (Years of Life Lost)"),]
  gbd_inj_dth <- gbd_injuries[which(gbd_injuries$measure=="Deaths"),]
  gbd_inj_yll$yll_dth_ratio <- gbd_inj_yll$value_gama/gbd_inj_dth$value_gama 
  GBD_INJ_YLL <<- gbd_inj_yll
}

ithim_setup_baseline_scenario <- function(){
  ## SET UP TRAVEL DATA
  rd <- RD
  # Create a row id
  rd$rid <- 1:nrow(rd)
  
  # Define trip_distances (in km)
  # Based on travel mode and trip duration, calculate distances
  
  rd <- left_join(rd, MODE_SPEEDS, by = "trip_mode")
  rd$speed[is.na(rd$speed)] <- 0
  rd$trip_distance <- (rd$trip_duration / 60) * rd$speed
  
  # Initialize them
  rd$trip_distance_cat <- 0
  rd$trip_distance_cat[rd$trip_distance > 0 & rd$trip_distance < DIST_LOWER_BOUNDS[2]] <- DIST_CAT[1]
  rd$trip_distance_cat[rd$trip_distance >= DIST_LOWER_BOUNDS[2] & rd$trip_distance < DIST_LOWER_BOUNDS[3]] <- DIST_CAT[2]
  rd$trip_distance_cat[rd$trip_distance >= DIST_LOWER_BOUNDS[3]] <- DIST_CAT[3]
  
  # Make age category
  rd$age_cat[rd$age >= AGE_LOWER_BOUNDS[1] & rd$age < AGE_LOWER_BOUNDS[2]] <- AGE_CATEGORY[1]
  rd$age_cat[rd$age >= AGE_LOWER_BOUNDS[2] & rd$age <= AGE_LOWER_BOUNDS[3]] <- AGE_CATEGORY[2]
  rd$age_cat[rd$age > AGE_LOWER_BOUNDS[3]] <- AGE_CATEGORY[3]
  
  # Remove all participants greater than 70 years of age
  rd <- filter(rd, age_cat != AGE_CATEGORY[3])
  
  rd$scenario <- "Baseline"
  
  bus_walk_trips <- add_walk_trips(filter(rd, trip_mode == "Bus"), ln_mean = MEAN_BUS_WALK_TIME, ln_sd = 1.2)
  
  rd[rd$trip_mode == 'Bus' & rd$rid %in% bus_walk_trips[[1]]$rid,]$trip_duration <- bus_walk_trips[[1]]$trip_duration
  rd[rd$trip_mode == 'Bus' & rd$rid %in% bus_walk_trips[[1]]$rid,]$trip_distance <- bus_walk_trips[[1]]$trip_distance
  rd[rd$trip_mode == 'Bus' & rd$rid %in% bus_walk_trips[[1]]$rid,]$trip_distance_cat <- bus_walk_trips[[1]]$trip_distance_cat
  
  rd <- rbind(rd, bus_walk_trips[[2]])
  
  rd$row_id <- 1:nrow(rd)

  rd
}

set_scenario_specific_variables <- function(rd){
  NSCEN <<- length(unique(rd$scenario)) - 1
  SCEN <<- unique(rd$scenario)
  SCEN_SHORT_NAME <<- c("base",paste0("scen", 1:NSCEN) )
}

add_walk_trips <- function(bus_trips, ln_mean, ln_sd){
  
  # bus_trips
  # ln_mean = 5
  # ln_sd = 1.2
  
  bus_trips <- arrange(bus_trips, trip_duration)
  
  walk_trips <- bus_trips
  
  walk_trips$trip_mode <- 'Short Walking'
  
  walk_trips$trip_duration <- sort(rlnorm(n = nrow(bus_trips), meanlog = log(ln_mean), sdlog = log(ln_sd)))
  
  # Replace walk trips with duration greater than that of bus needs to be set to 0
  if (nrow(walk_trips[(walk_trips$trip_duration - bus_trips$trip_duration)  > 0,]) > 0)
    walk_trips[(walk_trips$trip_duration - bus_trips$trip_duration)  > 0,]$trip_duration <- 0
  
  bus_trips$trip_duration <- bus_trips$trip_duration - walk_trips$trip_duration
  
  # print(summary(bus_trips$trip_duration))
  
  # print(summary(walk_trips$trip_duration))
  
  # Corrrect walk trips distance
  walk_trips$trip_distance <- (walk_trips$trip_duration / 60) * 4.8
  
  bus_trips$trip_distance <- (bus_trips$trip_duration / 60 ) * 15
  
  # Recategories trip_distance_cat for both bus and walk trips
  bus_trips$trip_distance_cat[bus_trips$trip_distance > 0 & bus_trips$trip_distance < DIST_LOWER_BOUNDS[2]] <- DIST_CAT[1]
  bus_trips$trip_distance_cat[bus_trips$trip_distance >= DIST_LOWER_BOUNDS[2] & bus_trips$trip_distance < DIST_LOWER_BOUNDS[3]] <- DIST_CAT[2]
  bus_trips$trip_distance_cat[bus_trips$trip_distance >= DIST_LOWER_BOUNDS[3]] <- DIST_CAT[3]
  
  
  walk_trips$trip_distance_cat[walk_trips$trip_distance > 0 & walk_trips$trip_distance < DIST_LOWER_BOUNDS[2]] <- DIST_CAT[1]
  walk_trips$trip_distance_cat[walk_trips$trip_distance >= DIST_LOWER_BOUNDS[2] & walk_trips$trip_distance < DIST_LOWER_BOUNDS[3]] <- DIST_CAT[2]
  walk_trips$trip_distance_cat[walk_trips$trip_distance >= DIST_LOWER_BOUNDS[3]] <- DIST_CAT[3]
  
  return(list(bus_trips, walk_trips))
  
}

create_scenario <- function(rdr, scen_name, source_modes, combined_modes = F, target_modes, source_distance_cats, 
                            source_trips, target_trips){
  local_source_trips <- list()
  if (!combined_modes){
    for (i in 1:length(source_modes)){
      local_source_trips[i] <- nrow(filter(rdr, trip_mode == source_modes[i])) - source_trips[i]
    }
    local_source_trips <- purrr::flatten_dbl(local_source_trips)
  }
  
  all_samples <- NULL
  
  if (!combined_modes){
    
    for (i in 1:length(source_modes)){
      
      sample <- filter(rdr, trip_mode == source_modes[i] & 
                         trip_distance_cat %in% source_distance_cats) %>% sample_n(local_source_trips[i]) %>% 
        mutate(trip_mode = target_modes[1],
               trip_duration = (trip_distance * 60 ) / MODE_SPEEDS[MODE_SPEEDS$trip_mode == target_modes[i],]$speed)
      
      
      # Update selected rows for mode and duration
      rdr[rdr$row_id %in% sample$row_id,]$trip_mode <- sample$trip_mode
      rdr[rdr$row_id %in% sample$row_id,]$trip_duration <- sample$trip_duration
      
      
      if (source_modes[i] == 'Bus'){
        # Remove bus associated short walking trips that have been changed to Private Car trips
        rdr <- rdr[!(rdr$trip_mode == 'Short Walking' & rdr$trip_id %in% sample$trip_id),]
      }
      
      
    } 
  }
  
  else {
    
    
    sample <- filter(rdr, trip_mode %in% source_modes & 
                       trip_distance_cat %in% source_distance_cats) %>% sample_n(source_trips[1]) %>% 
      mutate(trip_mode = target_modes[1],
             trip_duration = (trip_distance * 60 ) / MODE_SPEEDS[MODE_SPEEDS$trip_mode == target_modes[1],]$speed)
    
    sample$scenario <- scen_name
    
    return(sample)
  }
  
  
  rdr$scenario <- scen_name
  
  return(rdr)
  
}

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

dist_dur_tbls <- function(bs){
  # Remove short walking, 99, Train, Other and Unspecified modes
  dataset <- filter(bs, ! trip_mode %in% c('Short Walking', "99", "Train", "Other", "Unspecified"))
  
  if(plotFlag){
    # Unique number of ind
    total_ind <- length(unique(bs$participant_id))
    
    l <- list()
    for (i in 1:length(unique(dataset$scenario))){
      bd <- filter(dataset, scenario == unique(dataset$scenario)[i])
      bdnr <- nrow(bd)
      bd <- bd %>% group_by(trip_mode) %>%  summarise(pert = dplyr::n())
      bd <- bd %>%  select(trip_mode, pert) %>% 
        setNames(c("trip_mode",unique(dataset$scenario)[i])) 
      l[[i]] <- bd
      
    }
    
    bd <- l[[1]]
    if(length(l)>1)
      for (i in 2:length(l))
        bd <- left_join(bd, l[[i]], by = "trip_mode")
    
    l <- list()
    for (i in 1:length(unique(dataset$scenario))){
      bd <- filter(dataset, scenario == unique(dataset$scenario)[i])
      bdnr <- nrow(bd)
      bd <- bd %>% group_by(trip_mode) %>%  summarise(pert = round(dplyr::n()/bdnr * 100, 1))
      bd <- bd %>%  select(trip_mode, pert) %>% 
        setNames(c("trip_mode",unique(dataset$scenario)[i])) 
      l[[i]] <- bd
    }
    
    bd <- l[[1]]
    if(length(l)>1)
      for (i in 2:length(l))
        bd <- left_join(bd, l[[i]], by = "trip_mode")
    
    bd <- reshape2::melt(bd)
    
    plotly::ggplotly(ggplot(data = bd, aes(x = trip_mode, y = value, fill = variable)) + 
                       geom_bar(stat = 'identity', position = "dodge", color = "black") + 
                       theme_minimal() + xlab('Mode') + ylab('Percentage (%)') + labs(title = "Mode distribution per week"))
    # Calculate trip distance for baseline and three scenarios
    
  }
  
  l_dist <- list()
  l_dur <- list()
  for (i in 1:length(unique(dataset$scenario))){
    local <- bs %>% filter(scenario == unique(dataset$scenario)[i]) %>% 
      group_by(trip_mode)
    
    local_dur <- local %>% summarise(sum_dur = sum(trip_duration))
    local_dur <- filter(local_dur, !is.na(trip_mode))
    local_dur$sum_dur[local_dur$trip_mode == "Walking"] <- 
      local_dur$sum_dur[local_dur$trip_mode == "Walking"] + 
      local_dur$sum_dur[local_dur$trip_mode == "Short Walking"]
    local_dur <- local_dur %>%  select(trip_mode, sum_dur) %>% 
      setNames(c("trip_mode",unique(dataset$scenario)[i])) 
    l_dur[[i]] <- local_dur
    
    local_dist <- local %>% summarise(sum_dist = sum(trip_distance))
    local_dist <- filter(local_dist, !is.na(trip_mode))
    local_dist$sum_dist[local_dist$trip_mode == "Walking"] <- 
      local_dist$sum_dist[local_dist$trip_mode == "Walking"] + 
      local_dist$sum_dist[local_dist$trip_mode == "Short Walking"]
    local_dist <- local_dist %>%  select(trip_mode, sum_dist) %>% 
      setNames(c("trip_mode",unique(dataset$scenario)[i])) 
    l_dist[[i]] <- local_dist
  }
  
  for (i in 1:length(l_dist)){
    if (i == 1){
      local_dist <- l_dist[[i]]
      local_dur <- l_dur[[i]]
    }else{
      local_dist <- left_join(local_dist, l_dist[[i]], by = "trip_mode")
      local_dur <- left_join(local_dur, l_dur[[i]], by = "trip_mode")
    }
  }
  
 
  # Remove short walking, 99, Train, Other and Unspecified modes
  local_dist <- filter(local_dist, ! trip_mode %in% c('Short Walking', "99", "Train", "Other", "Unspecified"))
  local_dur <- filter(local_dur, ! trip_mode %in% c('Short Walking', "99", "Train", "Other", "Unspecified"))
  
  dist <- local_dist
  dur <- local_dur
  
  if(plotFlag){
    
    dist_melted <- reshape2::melt(dist, by = trip_mode)
    # Plot
    plotly::ggplotly(ggplot(data = dist_melted, aes(x = trip_mode, y = value / total_ind, fill = variable)) + 
                       geom_bar(stat = 'identity', position = 'dodge', color = "black") + 
                       theme_minimal() + xlab('Mode') + ylab('Distance (km)') + labs(title = "Mode distance  per person per week (km)")
    )
    
    dur_melted <- reshape2::melt(dur, by = trip_mode)
    
    dur_melted$value <- round(dur_melted$value / (60 * total_ind), 2)
    
    # Plot
    plotly::ggplotly(ggplot(data = dur_melted, aes(x = trip_mode, y = value, fill = variable)) + 
                       geom_bar(stat = 'identity', position = 'dodge', color = 'black') + 
                       theme_minimal() + xlab('Mode') + ylab('Duration (hours)') + labs(title = 
                                                                                          "Mode Duration per person per week (hours)")
    )
  }
  
  list(dist,dur)
  
}

total_mmet <- function(rd,INDEX){
  rd_pa <- subset(rd,trip_mode%in%c('Bicycle','Walking','Short Walking'))
  # Convert baseline's trip duration from mins to hours
  rd_pa$trip_duration_hrs <- rd_pa$trip_duration / 60
  # Get total individual level walking and cycling and sport mmets 
  
  pa_ind <- setDT(rd_pa)[,.(sex = first(sex), age = first(age),  age_cat = first(age_cat), 
                              cycling_mmet_base = (sum(trip_duration_hrs[trip_mode == 'Bicycle' & scenario == 'Baseline']) ),
                              walking_mmet_base = (sum(trip_duration_hrs[trip_mode %in%c('Walking','Short Walking')  & scenario == 'Baseline']) ),
                              cycling_mmet_scen1 = (sum(trip_duration_hrs[trip_mode == 'Bicycle'  & scenario == 'Scenario 1']) ),
                              walking_mmet_scen1 = (sum(trip_duration_hrs[trip_mode %in%c('Walking','Short Walking')  & scenario == 'Scenario 1']) ),
                              cycling_mmet_scen2 = (sum(trip_duration_hrs[trip_mode == 'Bicycle'   & scenario == 'Scenario 2']) ),
                              walking_mmet_scen2 = (sum(trip_duration_hrs[trip_mode  %in%c('Walking','Short Walking')   & scenario == 'Scenario 2']) ), 
                              cycling_mmet_scen3 = (sum(trip_duration_hrs[trip_mode == 'Bicycle'   & scenario == 'Scenario 3']) ),
                              walking_mmet_scen3 = (sum(trip_duration_hrs[trip_mode  %in%c('Walking','Short Walking')  & scenario == 'Scenario 3']) ),
                              cycling_mmet_scen4 = (sum(trip_duration_hrs[trip_mode == 'Bicycle'   & scenario == 'Scenario 4']) ),
                              walking_mmet_scen4 = (sum(trip_duration_hrs[trip_mode  %in%c('Walking','Short Walking')   & scenario == 'Scenario 4']) ), 
                              cycling_mmet_scen5 = (sum(trip_duration_hrs[trip_mode == 'Bicycle'   & scenario == 'Scenario 5']) ),
                              walking_mmet_scen5 = (sum(trip_duration_hrs[trip_mode  %in%c('Walking','Short Walking')   & scenario == 'Scenario 5']) ),
                              work_ltpa_mmet = first(work_ltpa_marg_met)),by='participant_id']
  
  pa_ind$base_mmet <- pa_ind$work_ltpa_mmet / PA_MULT[INDEX] +  pa_ind$cycling_mmet_base* MMETCycling + pa_ind$walking_mmet_base * MMETWalking
  pa_ind$scen1_mmet <- pa_ind$work_ltpa_mmet / PA_MULT[INDEX] +  pa_ind$cycling_mmet_scen1* MMETCycling + pa_ind$walking_mmet_scen1 * MMETWalking
  pa_ind$scen2_mmet <- pa_ind$work_ltpa_mmet / PA_MULT[INDEX] +  pa_ind$cycling_mmet_scen2* MMETCycling + pa_ind$walking_mmet_scen2 * MMETWalking
  pa_ind$scen3_mmet <- pa_ind$work_ltpa_mmet / PA_MULT[INDEX] +  pa_ind$cycling_mmet_scen3* MMETCycling + pa_ind$walking_mmet_scen3 * MMETWalking
  pa_ind$scen4_mmet <- pa_ind$work_ltpa_mmet / PA_MULT[INDEX] +  pa_ind$cycling_mmet_scen4* MMETCycling + pa_ind$walking_mmet_scen4 * MMETWalking
  pa_ind$scen5_mmet <- pa_ind$work_ltpa_mmet / PA_MULT[INDEX] +  pa_ind$cycling_mmet_scen5* MMETCycling + pa_ind$walking_mmet_scen5 * MMETWalking
  name_indices <- which(colnames(pa_ind)%in%c('participant_id', 'sex', 'age', 'age_cat', 'base_mmet', 'scen1_mmet', 'scen2_mmet', 'scen3_mmet', 'scen4_mmet', 'scen5_mmet'))
  mmets <- tbl_df(pa_ind)[,name_indices]
  mmets$id <- INDEX
  mmets
  
}

scenario_pm_calculations <- function(dist,rd){
  
  scen_dist <- dist 

  ### Calculating number of scenarios besides the baseline
  dataset <- filter(rd, ! trip_mode %in% c('Short Walking', "99", "Train", "Other", "Unspecified"))
  trans_emissions <- TRANS_EMISSIONS_ORIGINAL
  n <- which(names(scen_dist)=="Baseline") ## the column where baseline distances are in the scenario distance file
  p <- ncol(trans_emissions)
  for (i in 1:NSCEN){
    trans_emissions[1,p+i] <- trans_emissions$base_emissions[1]*scen_dist[4,n+i]/scen_dist[4,n] ## scenario emissions of 4W1
    trans_emissions[2,p+i] <- trans_emissions$base_emissions[2]*scen_dist[4,n+i]/scen_dist[4,n] ## scenario emissions of 4W2 (>2000cc engine size)
    trans_emissions[3,p+i] <- trans_emissions$base_emissions[3]*scen_dist[3,n+i]/scen_dist[3,n] ## scenario emissions of 2W
    trans_emissions[4,p+i] <- trans_emissions$base_emissions[4]*scen_dist[5,n+i]/scen_dist[5,n] ## scenario emissions of Taxi
    trans_emissions[5,p+i] <- trans_emissions$base_emissions[5]*scen_dist[2,n+i]/scen_dist[2,n] ## scenario emissions of bus
    trans_emissions[6,p+i] <- trans_emissions$base_emissions[6]*1 ## scenario emissions of trucks
    trans_emissions[7,p+i] <- trans_emissions$base_emissions[7]*1 ## scenario emissions of trucks
    trans_emissions[8,p+i] <- trans_emissions$base_emissions[8]*1 ## scenario emissions of trucks
    names(trans_emissions)[p+i] <-(paste("scen",i,"_emissions", sep=""))
  }
  
  
  trans_emissions[nrow(trans_emissions)+1,2:ncol(trans_emissions)] <- colSums(trans_emissions[,2:ncol(trans_emissions)],na.rm=T)
  
  trans_emissions[nrow(trans_emissions),1] <-"Total"  
  total_row <- nrow(trans_emissions)
  
  trans_emissions[nrow(trans_emissions)+1,1] <-"pm_conc_total"
  trans_emissions[nrow(trans_emissions),ncol(trans_emissions)-NSCEN] <- PM_CONC_BASE
  trans_emissions[nrow(trans_emissions)+1,1]<-"pm_conc_transport"
  trans_emissions[nrow(trans_emissions), ncol(trans_emissions)-NSCEN]<- PM_TRANS_SHARE*PM_CONC_BASE
  
  for(i in NSCEN:1)
    trans_emissions[nrow(trans_emissions), ncol(trans_emissions)-(i-1)] <- PM_TRANS_SHARE*PM_CONC_BASE*trans_emissions[total_row, ncol(trans_emissions)-(i-1)]/trans_emissions[total_row, ncol(trans_emissions)-i]
  
  non_transport_pm_conc <- PM_CONC_BASE -  (PM_TRANS_SHARE*PM_CONC_BASE)  ### this is the concentration contributed by non-transport share (remains constant across the scenarios)
  
  for(i in NSCEN:1)
    trans_emissions[nrow(trans_emissions)-1,ncol(trans_emissions)-(i-1)] <-  non_transport_pm_conc + trans_emissions[nrow(trans_emissions),ncol(trans_emissions)-(i-1)]
  
  conc_pm <- trans_emissions[nrow(trans_emissions)-1, 6:(6+NSCEN)] ## background concentrations for baseline and all scenarios
  
  ### following code generates final_data
  for (i in 1:length(SCEN)){
    scen_index <- SCEN[i]
    rd_scen <- filter(rd, scenario == scen_index)
    rd_scen <- left_join(rd_scen,LOOKUP_RATIO_PM, "trip_mode")  ## attaching the file with in-vehicle ratio and ventilation rate
    rd_scen$on_road_air <- rd_scen$trip_duration*rd_scen$vent_rate / 60
    rd_scen$pm_dose <- rd_scen$on_road_air * rd_scen$ratio * as.numeric(conc_pm[i])
    
    ##RJ need to retain ids
    #rd_scen$participant_id <- as.factor(rd_scen$participant_id)
    
    individual_data <- summarise(group_by(rd_scen,participant_id),on_road_dur = sum(trip_duration,na.rm=TRUE), 
                                 on_road_pm = sum(pm_dose,na.rm=TRUE), 
                                 on_road_pm= sum(pm_dose,na.rm=TRUE), 
                                 air_inhaled = sum(on_road_air,na.rm=TRUE))
    temp_vec <- (24-individual_data$on_road_dur/60)*10
    individual_data$pm_conc <- ((temp_vec * as.numeric(conc_pm[i])) + individual_data$on_road_pm)/(temp_vec+individual_data$air_inhaled)
    individual_data <- subset(individual_data, select=c("participant_id", "pm_conc"))
    names(individual_data)[2] <- paste0('pm_conc_',SCEN_SHORT_NAME[i])
    
    if (i == 1 )
    {
      final_data <- individual_data 
    }else{
      final_data <- left_join(final_data,individual_data,by="participant_id")
    }
  }
  
  #####PM normalise
  
  mean_conc <- rep(0,length(SCEN_SHORT_NAME))
  
  ## calculating means of individual-level concentrations
  for ( i in 1: length(SCEN_SHORT_NAME))
    mean_conc[i] <- mean(final_data[[paste0("pm_conc_", SCEN_SHORT_NAME[i])]])
  
  normalise <- as.numeric(conc_pm[1])/as.numeric(mean_conc[1])
  ###Lines which are normalising the concentrations
  
  for (i in 1: length(SCEN_SHORT_NAME))
    final_data[[paste0("pm_conc_", SCEN_SHORT_NAME[i])]] <- normalise*final_data[[paste0("pm_conc_", SCEN_SHORT_NAME[i])]]
  
  as.data.frame(final_data)
  
}

gen_ap_rr <- function(rd,ind_pm){
  # Create dummy ind pop
  ind <-  summarise(group_by(rd,participant_id),sex = first(sex),
                    age = first(age),
                    age_cat = first(age_cat))
  ## number of scenarios
  dataset <- filter(rd, ! trip_mode %in% c('Short Walking', "99", "Train", "Other", "Unspecified"))
  
  ### combining PM2.5 concentration data (scenario_pm_calculations.R) and PA data (total_mmet.R) at the individual level (n=732)
  #ind<- read.csv("data/synth_pop_data/accra/processed_data/indiv_mmet/pa_total_mmet_weekly.csv") ### PA 
  ind_pm$participant_id <- as.integer(ind_pm$participant_id)
  
  ind <-  left_join(ind,ind_pm, by = "participant_id")
  ## assigning air pollution age band to the individual_level data
  min_ages <- c(seq(24,94,by=5),200)
  ind$ap_age <- sapply(ind$age,function(x)if(x>min_ages[1])min_ages[which(min_ages>x)[1]-1]+1 else 0)

  ## for every individual average of all parameter draws within the age and disease-outcome
  
  iters <- 1:5
  pm_indices <- sapply(SCEN_SHORT_NAME,function(x)which(colnames(ind)==paste0("pm_conc_",x)))
  ### iterating over all all disease outcomes
  for ( j in 1:nrow(DISEASE_OUTCOMES)){
    ## checking whether to calculate this health outcome for air pollution
    if (DISEASE_OUTCOMES$air_pollution[j] == 1){ 
      for(x in 1:length(SCEN_SHORT_NAME)) ind[[paste0("RR_ap_",SCEN_SHORT_NAME[x])]] <- 0
      dr_ap_disease <- subset(DR_AP,cause_code==as.character(DISEASE_OUTCOMES$ap_acronym[j]))
      ## iterating over all individuals
      ages <- unique(dr_ap_disease$age_code)
      for(age in ages){
        dr_ap_sub <- subset(dr_ap_disease,age_code == age )
        if(age==99){
          i <-1:nrow(ind)
        }else{
          i <- which(ind$ap_age==age)
        }
        alpha <- repmat(dr_ap_sub[iters,2],length(i),1)
        beta <- repmat(dr_ap_sub[iters,3],length(i),1)
        gamma <- repmat(dr_ap_sub[iters,4],length(i),1)
        tmrel <- repmat(dr_ap_sub[iters,5],length(i),1)
        pm <- repmat(dr_ap_sub[iters,2],length(i),1)
        for(x in 1: length(SCEN_SHORT_NAME)) ind[[paste0("RR_ap_",SCEN_SHORT_NAME[x])]][i] <- rowMeans(1 + (alpha * (1-exp(-beta*((repmat(as.matrix(ind[i,pm_indices[x]]),1,length(iters)) - tmrel)^gamma)))))
      }
      ## change the names of the columns as per the disease
      for (n in 1: length(SCEN_SHORT_NAME)){
        col <- which(names(ind)== paste0("RR_ap_",SCEN_SHORT_NAME[n]))
        names(ind)[col]<- paste0("RR_ap_",SCEN_SHORT_NAME[n],"_",DISEASE_OUTCOMES$acronym[j])
      }
    }
  }
  ind
}

dose_response <- function (cause, outcome_type, dose, confidence_intervals = F, certainty = T, use_75_pert = T){
  
  if (sum(is.na(dose))>0 || class(dose)!= "numeric"){
    stop ('Please provide dose in numeric')
  }
  if (!cause %in% c('all-cause-mortality', 'breast-cancer', 'cardiovascular-disease',
                    'colon-cancer', 'coronary-heart-disease', 'diabetes', 'endometrial-cancer',
                    'heart-failure', 'lung-cancer', 'stroke', 'total-cancer')){
    stop('Unsupported cause/disease. Please select from \n
         all-cause-mortality \n
         breast-cancer\n
         cardiovascular-disease \n
         colon-cancer \n
         coronary-heart-disease \n
         endometrial-cancer \n
         heart-failure \n
         lung-cancer \n
         stroke \n
         total-cancer')
  }
  if (!outcome_type %in% c('mortality', 'incidence')){
    stop('Unsupported outcome_type. Please select from \n
         mortality \n
         incidence')
  }
  if (cause == 'all-cause-mortality' && outcome_type == 'incidence'){
    stop('Incidence does not exist for all-cause-mortality')
  }
  fname <- paste(cause, outcome_type, sep = "-")
  if (cause == 'all-cause-mortality')
    fname <- cause
  lookup_table <- get(paste0(fname))
  lookup_df <- as.data.frame(lookup_table)
  #pert_75 <- stringr::str_sub(basename(list_of_files[[1]]), end = -5)
  rr <- approx(x=lookup_df$dose,y=lookup_df$RR,xout=dose,yleft=1,yright=min(lookup_df$RR))$y
  if (confidence_intervals|| !certainty){
    lb <- approx(x=lookup_df$dose,y=lookup_df$lb,xout=dose,yleft=1,yright=min(lookup_df$lb))$y
    ub <- approx(x=lookup_df$dose,y=lookup_df$ub,xout=dose,yleft=1,yright=min(lookup_df$ub))$y
  }
  if (!certainty){
    rr <- truncnorm::rtruncnorm(n = length(rr), a = lb, b = ub, mean = rr)
  }
  if (confidence_intervals) {
    return(data.frame (rr = rr, lb = lb, ub = ub))
  }else{
    return(data.frame(rr = rr))
  }
}

gen_pa_rr <- function(ind,INDEX){
  ### iterating over all all disease outcomes
  dose_columns <- match(paste0(SCEN_SHORT_NAME, '_mmet'),colnames(ind))
  doses_clean <- ind[,dose_columns]
  for ( j in 1:nrow(DISEASE_OUTCOMES)){
    ## checking whether to calculate this health outcome for PA
    if (DISEASE_OUTCOMES$physical_activity[j] == 1){
      pa_dn <- as.character(DISEASE_OUTCOMES$pa_acronym[j])
      pa_n <- as.character(DISEASE_OUTCOMES$acronym[j])
      outcome_type <- ifelse(pa_dn%in%c('lung-cancer','stroke'), 'incidence' , 'mortality')
      use_75_pert <- F#ifelse(pa_dn == 'all-cause-mortality',T,F)
      ##RJ what is use_75_pert?
      # CHD: 35 mmeth per week use mortality
      # Lung cancer: 10 mmeth per week use incidence
      # stroke 75 pert: 13.37
      # Diabetes no limits
      # total cancer: 35 mmeths per week use mortality
      doses <- doses_clean
      if(pa_dn %in% c('total-cancer','coronary-heart-disease')) doses[doses>=35] <- 35
      else if(pa_dn == 'lung-cancer') doses[doses>=10] <- 10
      else if(pa_dn == 'stroke') doses[doses>=13.37] <- 13.37
      return_vector <- dose_response(cause = pa_dn, outcome_type = outcome_type, certainty = pa_certainty, 
                                   dose = unlist(data.frame(doses)), use_75_pert = use_75_pert)
      for (i in 1:length(SCEN_SHORT_NAME)){
        scen <- SCEN_SHORT_NAME[i]
        ind[[paste('RR_pa', scen, pa_n, sep = '_')]] <- return_vector$rr[(1+(i-1)*nrow(doses)):(i*nrow(doses))]
      }
    }
  }
  ind 
}

combined_rr_pa_pa <- function(ind_pa,ind_ap){
  
  # Replace NaNs with 1
  ind_ap[is.na(ind_ap)] <- 1
  
  # Replace Na with 1
  ind_pa[is.na(ind_pa)] <- 1
  
  # remove common columns from ap
  ind_ap <- select(ind_ap, -c(sex, age, age_cat))
  
  # join pa and ap ind datasets
  ind <- left_join(ind_pa, ind_ap, by = "participant_id")

  ### iterating over all all disease outcomes
  for ( j in 1:nrow(DISEASE_OUTCOMES)){
    ## checking whether to calculate this health outcome for PA
    if (DISEASE_OUTCOMES$physical_activity[j] == 1 & DISEASE_OUTCOMES$air_pollution[j] == 1){
      for (scen in SCEN_SHORT_NAME){
        ac <- as.character(DISEASE_OUTCOMES$acronym[j])
        ind[[paste('RR_pa_ap', scen, ac, sep = '_')]] <- ind[[paste('RR_pa', scen, ac, sep = '_')]] * ind[[paste('RR_ap', scen, ac, sep = '_')]]
        
      }
    }
  }
  
  ind
}

accra_injuries <- function(rd,scen_dist){
  ### injury code
  ### This is the script for distance-based injury model for Accra using safety-in-numbers
  ## PREPROCESSING
  if(exists('SCEN_DIST_FOR_INJURY')){
    scen_dist <- SCEN_DIST_FOR_INJURY
  }else{
    names(scen_dist)[1] <- c("mode")
    names(scen_dist)[2:(length(SCEN_SHORT_NAME)+1)] <- SCEN_SHORT_NAME
    scen_dist[nrow(scen_dist)+1, 1] <- "Car"
    scen_dist[nrow(scen_dist),1:(NSCEN+1)+1] <- colSums(scen_dist[4:5,1:(NSCEN+1)+1]) ## summing Private Car and Taxi as Car
    scen_dist <- scen_dist[-c(4,5),]  ## removing Private Car and Taxi rows
    scen_dist[,1] <- c("Bicycle", "Bus", "Motorcycle", "Pedestrian", "Car")
    ## adding truck as one of the vehicle types
    scen_dist[nrow(scen_dist)+1, 1] <-"Truck"  
    ## adding tuktuk as one of the vehicle types
    scen_dist[nrow(scen_dist)+1, 1] <-"Tuktuk"
    ## allocating dummy distance of 1 for trucks as these will not be changed across the scenarios
    scen_dist[nrow(scen_dist)-1,1:(NSCEN+1)+1] <- 1   
    ## allocating dummy distance of 1 for tuk-tuks as these will not be changed 
    scen_dist[nrow(scen_dist),1:(NSCEN+1)+1] <- 1  
    ## columns as striking and rows as victim
    ## calculating the ratio of distances for each mode in each scenario
    for (i in 1:NSCEN ) scen_dist[,(2+i)] <- scen_dist[,(2+i)]/scen_dist[,2]
    SCEN_DIST_FOR_INJURY <<- scen_dist
  }
  
  
  
  whw_mat2 <- list()
  for (k in 3:(1+length(SCEN_SHORT_NAME))) whw_mat2[[k-2]] <- WHW_MAT
  for (j in 2: ncol(WHW_MAT)){
    ncol_sin <- which(names(S.I.N)==names(WHW_MAT)[j])
    nrow_strk_dist <- which(scen_dist[,1]== names(WHW_MAT)[j])
    for (i in 1: nrow(WHW_MAT)) {
      nrow_vic_dist <- which(scen_dist[,1]== as.character(WHW_MAT[i,1]))
      nrow_sin <-  which(S.I.N[,1]==as.character(WHW_MAT[i,1])) 
      vic_exp <- S.I.N[nrow_sin[1],ncol_sin]
      str_exp <- S.I.N[nrow_sin[1]+6,ncol_sin]
      for (k in 3:(1+length(SCEN_SHORT_NAME))){ 
        victim_dist <- scen_dist[nrow_vic_dist,k] 
        strk_dist <- scen_dist[nrow_strk_dist,k]
        whw_mat2[[k-2]][i, j] <- WHW_MAT[i, j]*(victim_dist^vic_exp)*(strk_dist^str_exp)  
      }
    }
  }
  ## names of victim types
  victim_deaths <- as.data.frame(WHW_MAT[,1])  
  ## number of deaths in baseline by victim type
  victim_deaths <- cbind(victim_deaths, scen=as.data.frame(rowSums(WHW_MAT[,3:8])))  
  for (k in 3:(1+length(SCEN_SHORT_NAME))) victim_deaths <- cbind(victim_deaths, as.data.frame(rowSums(whw_mat2[[k-2]][,3:8],na.rm=T))) 
  names(victim_deaths)[1] <- c("victim_type")
  names(victim_deaths)[2:(length(SCEN_SHORT_NAME)+1)] <- SCEN_SHORT_NAME
  
  if(exists('RELATIVE_DIST_FOR_INJURY')){
    relative_distances <- RELATIVE_DIST_FOR_INJURY
  }else{
    journeys <- filter(rd,!is.na(trip_id), !trip_mode%in%c(99,"Train","Unspecified","Other")) %>% group_by (age_cat,sex,trip_mode, scenario) %>% summarise(tot_dist=sum(trip_distance))
    distances <- spread(journeys,trip_mode, tot_dist) 
    distances$Pedestrian <- distances$Walking+distances$`Short Walking`
    distances <- distances[,-which(names(distances)== "Walking")]
    distances <- distances[,-which(names(distances)== "Short Walking")]
    distances$Car<- distances$Taxi+distances$`Private Car`
    distances <- distances[,-which(names(distances)== "Private Car")]
    distances <- distances[,-which(names(distances)== "Taxi")]
    
    mode_names <- names(distances)[4:8]
    for (i in 1: length(mode_names))
      for (n in 1:(NSCEN+1))
        distances[[mode_names[i]]][which(distances$scenario==unique(rd$scenario)[n])] <- distances[[mode_names[i]]][which(distances$scenario==unique(rd$scenario)[n])]/ sum(distances[[mode_names[i]]][which(distances$scenario==unique(rd$scenario)[n])],na.rm=T)
    relative_distances <- distances
    relative_distances$sex_age <-  paste0(relative_distances$sex,"_",relative_distances$age_cat)
    relative_distances <- relative_distances[,-c(which(names(relative_distances)=='sex'))]
    RELATIVE_DIST_FOR_INJURY <<- relative_distances
  }
  
  dist_scen_indices <- match(relative_distances$scenario,SCEN)
  vic_scen_indices <- match(SCEN_SHORT_NAME[dist_scen_indices],colnames(victim_deaths))
  vic_mode_indices <- match(names(relative_distances)[3:7],victim_deaths[,1])
  injuries <- relative_distances
  injuries[,3:7] <- injuries[,3:7]*t(victim_deaths[vic_mode_indices,vic_scen_indices])
  
  injuries[,9] <- rowSums(injuries[,3:7],na.rm=T)
  names(injuries)[9] <-"Deaths"
  
  joined_injury <- left_join(injuries, GBD_INJ_YLL, by="sex_age")
  
  joined_injury$YLL <- joined_injury$Deaths*joined_injury$yll_dth_ratio
  death_and_yll <- select(joined_injury, c('age_cat','sex','scenario','Deaths','YLL'))
  
  x_deaths <- select(death_and_yll, -YLL)
  x_deaths <- spread(x_deaths,scenario, Deaths)
  x_yll <- select(death_and_yll, -Deaths)
  x_yll <- spread(x_yll,scenario, YLL)
  
  for (n in c(1:length(SCEN_SHORT_NAME))[-2]){
      x_deaths[,n+2]<-x_deaths[,n+2] - x_deaths[,4] 
      x_yll[,n+2] <- x_yll[,n+2] - x_yll[,4] 
  }
  
  deaths_yll_injuries <- cbind(x_deaths, x_yll)
  deaths_yll_injuries <- deaths_yll_injuries[,-c((2+NSCEN+2),(2+NSCEN+3))]
  deaths_yll_injuries <- as.data.frame(deaths_yll_injuries)
  names(deaths_yll_injuries)[1:2]<- c("age_cat", "sex")
  
  metric <- c("deaths", "yll")
  k <- 1
  for  (i in 1: 2)
    for (j in 1:(NSCEN+1)){
      names(deaths_yll_injuries)[2+k] <- paste0(SCEN_SHORT_NAME[j],"_",metric[i],"_inj")
      k<-k+1
    }
  
  deaths_yll_injuries[,3:ncol(deaths_yll_injuries)] <- -1 * deaths_yll_injuries[,3:ncol(deaths_yll_injuries)] 
  
  #inj <- deaths_by_mode #read_csv("data/synth_pop_data/accra/injuries/deaths_by_mode.csv")
  #inj <- select(inj, -c(sex_age))
  #inj <- rename(inj, total = Deaths)
  #inj <- reshape2::melt(inj)
  #inj[is.na(inj)] <- 0
  
  deaths_yll_injuries
}

health_burden <- function(ind,inj){
  
  
  cols = c(3, 4) 
  ### iterating over all all disease outcomes
  for ( j in 1:nrow(DISEASE_OUTCOMES)){
    ## checking whether to calculate this health outcome for PA
    # Disease acronym
    ac <- DISEASE_OUTCOMES$acronym[j] %>% as.character()
    # GBD's disease name
    gbd_dn <- DISEASE_OUTCOMES$GBD_name[j] %>% as.character()
    middle_bit <- paste0(ifelse(DISEASE_OUTCOMES$physical_activity[j]==1,'pa_',''),ifelse(DISEASE_OUTCOMES$air_pollution[j]==1,'ap_',''))
    base_var <- paste0('RR_',middle_bit,'scen1_', ac)
    scen_vars <- paste0('RR_',middle_bit, SCEN_SHORT_NAME, '_', ac)
    base_scen_vars <- lapply(scen_vars,function(x)c(base_var,x))
    # Loop through all three scenarios
    for (index in 1:length(SCEN_SHORT_NAME)){
      scen <- SCEN_SHORT_NAME[index]
      scen_var <- scen_vars[index]
      # Calculate PIFs for baseline and selected scenario
      pif <- data.frame(PAF(pop = ind, attr = c('sex', 'age_cat'), cn = c(base_var, scen_var)))
      pif <- arrange(pif, age.band, gender)
      # Redefine non-factor based column classes
      pif[,c("age.band", "gender")] <- lapply(pif[,c("age.band", "gender")], as.character)   
      pif[,cols] = apply(pif[,cols], 2, function(x) as.numeric(as.character(x)))
      # Calculate ylls (total and red)
      yll_dfs <- combine_health_and_pif(
        pop = pif,
        hc = GBD_DATA,
        hm = "YLLs (Years of Life Lost)",
        cn = base_scen_vars[[index]],
        hm_cause = gbd_dn,
        hm_cn = 'value_gama')
      # Subset to get yll
      local_ylls <- as.data.frame(yll_dfs[[1]])
      # Subset to get yll_reductions
      local_ylls_red <- as.data.frame(yll_dfs[[2]])
      # Calculate deaths (total and red)
      death_dfs <- combine_health_and_pif(
        pop = pif,
        hc = GBD_DATA,
        hm = "Deaths",
        cn = base_scen_vars[[index]],
        hm_cause = gbd_dn,
        hm_cn = 'value_gama')
      # Subset to get yll
      local_deaths <- as.data.frame(death_dfs[[1]])
      # Subset to get yll_reductions
      local_deaths_red <- as.data.frame(death_dfs[[2]])
      # Remove baseline vars
      # Rename var names
      local_deaths <- rename(local_deaths, !! paste0(scen, '_deaths_',middle_bit,ac) := scen_var)
      local_deaths_red <- rename(local_deaths_red, !! paste0(scen, '_deaths_red_',middle_bit,ac) := scen_var)
      local_ylls <- rename(local_ylls, !! paste0(scen, '_ylls_',middle_bit,ac) := scen_var)
      local_ylls_red <- rename(local_ylls_red, !! paste0(scen, '_ylls_red_',middle_bit,ac) := scen_var)
      local_deaths <- select(local_deaths, -contains("RR_"))
      local_deaths_red <- select(local_deaths_red, -contains("RR_"))
      local_ylls <- select(local_ylls, -contains("RR_"))
      local_ylls_red <- select(local_ylls_red, -contains("RR_"))
      if (index == 1 && j==1){
        # If global vars are not initiliazed, copy vars
        gdeaths <- local_deaths
        gdeaths_red <- local_deaths_red
        gylls <- local_ylls
        gylls_red <- local_ylls_red
      }else{
        # global vars are already initialized. Join new datasets with old ones.
        gdeaths <- left_join(gdeaths, local_deaths, by = c("age.band", "gender"))
        gdeaths_red <- left_join(gdeaths_red, local_deaths_red, by = c("age.band", "gender"))
        gylls <- left_join(gylls, local_ylls, by = c("age.band", "gender"))
        gylls_red <- left_join(gylls_red, local_ylls_red, by = c("age.band", "gender"))
      }
    }
  }
  gdeaths[,names(select(gdeaths, contains("scen1_")))] <- 0
  # rename columns
  inj <- rename(inj, age.band = age_cat, gender = sex)
  # Select deaths columns
  inj_deaths <- select(inj, c(age.band, gender, contains("deaths")))
  # Select yll columns
  inj_ylls <- select(inj, c(age.band, gender, contains("yll")))
  # Join injuries data to global datasets
  gdeaths <- left_join(gdeaths, inj_deaths, by = c("age.band", "gender"))
  gylls <- left_join(gylls, inj_ylls, by = c("age.band", "gender"))
  list(deaths=gdeaths,deaths_red=gdeaths_red,ylls=gylls,ylls_red=gylls_red)
}

PAF <- function(pop, attr, cn){
  unique_gender <- unique(pop[[attr[1]]])
  unique_age_group <- unique(pop[[attr[2]]])
  combinations <- length(unique_age_group)*length(unique_gender)
  m <- expand.grid("age band"=unique_age_group, "gender"=unique_gender)
  
  for(i in cn) m[[i]] <- 0
  mi <- 1
  for (j in 1:length(unique_gender)){
    reduced_pop_gen <- filter(pop, UQ(as.name(attr[1])) == unique_gender[j])
    for (i in 1:length(unique_age_group)){
      reduced_pop <- filter(reduced_pop_gen, UQ(as.name(attr[2])) == unique_age_group[i])
      sumPRR <- sum (reduced_pop[[cn[1]]])
      m[mi, 3] <- sumPRR
      for (k in 2:length(cn)){
        sumPRRi <- sum (reduced_pop[[cn[k]]])
        PRA <- (sumPRR - sumPRRi) / sumPRR
        m[mi, 2 + k] = round(PRA, digits = 6)
      }
      mi <- mi + 1
    }
  }
  m
}

combine_health_and_pif <- function(pop, hc, hm, hm_cause, hm_cn, cn){
  
  # combine_health_and_pif(pif, GBD_DATA, "YLLs (Years of Life Lost)")
  # pop <- pif
  # hc <- GBD_DATA
  # hm <- "YLLs (Years of Life Lost)"
  write_indices <- unique(match(cn,colnames(pop))) ##RJ odd hack for when cn=c(scen1,scen1)
  m <- pop
  n <- pop
  for (new_row in 1:nrow(m)){
    gen <- m$gender[new_row]
    ageband <- m$age.band[new_row]
    sub <- filter(hc, sex == gen & age ==  ageband & measure == hm & metric == "Number" & cause == hm_cause)
    hm_cn_val <- as.numeric(sub[[hm_cn]])
    m_sub <- filter(m, gender == gen & age.band ==  ageband)
    baseline_val <- as.double(m_sub[write_indices[1]])# %>% select(cn[1]) %>% as.double()
    if (length(hm_cn_val) > 0){
      vals <- as.double(m_sub[write_indices]) * hm_cn_val #sapply(cn,function(x)m_sub %>% select(x) %>% as.double() * hm_cn_val)
    }else{ vals <- c(0,0) }
    
    n[n$gender == gen & n$age.band ==  ageband, write_indices] <- round(vals / baseline_val, 5)
    m[m$gender == gen & m$age.band == ageband, write_indices] <- vals
    
    #  for (i in 1:length(cn)){
    #      val <- m_sub %>% select(cn[i]) %>% as.double() * hm_cn_val
    #      print(val)
    #      n[[cn[i]]][n$gender == gen & n$age.band ==  ageband] <- round(val / baseline_val, 5)
    #      m[[cn[i]]][m$gender == gen & m$age.band == ageband] <- val
    #  }
    #}else{
    #  for (i in 1:length(cn))
    #    n[[cn[i]]][n$gender == gen & n$age.band ==  ageband] <- 
    #      m[[cn[i]]][m$gender == gen & m$age.band == ageband] <- 0
    #}
  }
  list(m, n)
}

run_ithim <- function(seed=1){ 
  INDEX <- 1
  set.seed(seed)#Sys.getpid()+seed+as.numeric(Sys.time())
  return_list <- list()
  if(SAMPLEMODE==T) {
    sample_parameters(parameters)
    parameter_samples <- sapply(names(parameters),function(x)get(x))
    return_list$parameter_samples <- parameter_samples
  }
  #cat(paste0(nsample,' out of ',NSAMPLES,'\n'))
  # Generate distance and duration matrices
  dist_and_dur <- dist_dur_tbls(bs)
  dist <- dist_and_dur[[1]]
  dur <- dist_and_dur[[2]]
  mmets <- total_mmet(bs,INDEX)
  pm_conc <- scenario_pm_calculations(dist,bs)
  RR_AP_calculations <- gen_ap_rr(bs,pm_conc)
  RR_PA_calculations <- gen_pa_rr(mmets,INDEX)
  RR_PA_AP_calculations <- combined_rr_pa_pa(RR_PA_calculations,RR_AP_calculations)
  deaths_yll_injuries <- accra_injuries(bs,dist)
  hb <- health_burden(RR_PA_AP_calculations,deaths_yll_injuries)
  #  deaths[[nsample]] <- hb$deaths
  #  deaths_red[[nsample]] <- hb$deaths_red
  ylls <- hb$ylls
  #  ylls_red[[nsample]] <- hb$ylls_red
  return_list$outcome <- colSums(ylls[,c(10,11,13:16)]) ## return ihd
  return(return_list)
}



