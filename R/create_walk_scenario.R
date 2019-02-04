#' @export
create_walk_scenario <- function(trip_set){
  rdr <- trip_set
  rd_list <- list()
  
  # Baseline scenario
  rd_list[[1]] <- rdr
  ###############################################################
  
  # Scenario 1: walk
  walk_scen <- SYNTHETIC_POPULATION[,colnames(SYNTHETIC_POPULATION)%in%colnames(trip_set)]
  walk_scen$trip_id <- max(rdr$trip_id) + walk_scen$participant_id
  walk_scen$trip_mode <- 'Walking'
  walk_scen$trip_duration <- 10
  walk_scen$trip_distance <- walk_scen$trip_duration * VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$trip_mode=='Walking'] / 60
  walk_scen$rid <- walk_scen$row_id <- walk_scen$trip_id ## redundant
  walk_scen$trip_distance_cat <- DIST_CAT[1] ## redundant
  
  # set scenario name, `Walking'
  walk_scen$scenario <- 'Walking'
  rdr$scenario <- 'Walking'
  
  print(colnames(rdr))
  print("--------")
  print(colnames(walk_scen))
  
  
  # join new walks to existing trips
  ##!! RJ hack: to run with this scenario, we trim extra columns from the raw trip set so they match the above
  rd_list[[1]] <- rd_list[[1]][,colnames(rd_list[[1]])%in%colnames(walk_scen)]
  rdr <- rdr[,colnames(rdr)%in%colnames(walk_scen)]
  walk_scen <- walk_scen[,match(colnames(rdr),colnames(walk_scen))]
  rdr <- rbind(rdr,walk_scen)

  rd_list[[2]] <- rdr
  ###############################################################
  
  return(rd_list)
}
