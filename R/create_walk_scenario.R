#' @export
create_walk_scenario <- function(trip_set){
  ###############################################################
  rdr <- trip_set
  tt <- nrow(rdr)
  rd_list <- list()
  rd_list[[1]] <- rdr
  # Scenario 1: walk
  walk_scen <- SYNTHETIC_POPULATION[,colnames(SYNTHETIC_POPULATION)%in%colnames(trip_set)]
  walk_scen$trip_id <- max(rdr$trip_id) + walk_scen$participant_id
  walk_scen$trip_mode <- 'Walking'
  walk_scen$trip_duration <- 10
  walk_scen$rid <- walk_scen$row_id <- walk_scen$trip_id ## redundant
  walk_scen$trip_distance <- walk_scen$trip_duration * VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$trip_mode=='Walking'] / 60
  walk_scen$trip_distance_cat <- DIST_CAT[1] ## redundant
  walk_scen$scenario <- 'Walking'
  rdr$scenario <- 'Walking'
  walk_scen <- walk_scen[,match(colnames(rdr),colnames(walk_scen))]
  rdr <- rbind(rdr,walk_scen)

  rd_list[[2]] <- rdr
  ###############################################################
  
  rd_list
}
