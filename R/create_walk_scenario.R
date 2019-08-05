#' @export
create_walk_scenario <- function(trip_set){
  rdr <- trip_set
  rdr <- add_trip_weights(rdr)
  rd_list <- list()
  
  # Baseline scenario
  rd_list[[1]] <- rdr
  ###############################################################
  
  # Scenario 1: walk
  walk_scen <- RAW_TRIP_DEMOGRAPHICS[,colnames(RAW_TRIP_DEMOGRAPHICS)%in%colnames(trip_set),with=F]
  walk_scen$trip_id <- max(rdr$trip_id) + 1:nrow(RAW_TRIP_DEMOGRAPHICS)
  walk_scen$trip_mode <- 'walking'
  walk_scen$trip_distance <- 1
  walk_scen$stage_mode <- 'walking'
  walk_scen$stage_distance <- 1
  walk_scen$stage_duration <- 12.5
  walk_scen$trip_weight <- 1
  walk_scen$rid <- walk_scen$row_id <- walk_scen$trip_id ## redundant
  walk_scen$trip_distance_cat <- DIST_CAT[1] ## redundant
  
  # set scenario name, `walking'
  walk_scen$scenario <- 'walking'
  rdr$scenario <- 'walking'
  
  # join new walks to existing trips
  ##!! RJ hack: to run with this scenario, we trim extra columns from the raw trip set so they match the above
  rd_list[[1]] <- rd_list[[1]][,colnames(rd_list[[1]])%in%colnames(walk_scen),with=F]
  rdr <- rdr[,colnames(rdr)%in%colnames(walk_scen),with=F]
  walk_scen <- walk_scen[,match(colnames(rdr),colnames(walk_scen)),with=F]
  rdr <- rbind(rdr,walk_scen)

  rd_list[[2]] <- rdr
  ###############################################################
  
  return(rd_list)
}
