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
