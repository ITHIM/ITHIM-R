#' @export
walk_to_bus_and_combine_scen <- function(){
  rd_list <- SYNTHETIC_TRIPS
  for(i in 1:length(rd_list)){
    bus_trips <- subset(rd_list[[i]],trip_mode=='Bus')
    not_bus_trips <- subset(rd_list[[i]],trip_mode!='Bus')
    bus_walk_trips <- add_walk_trips(bus_trips)
    rd_list[[i]] <- rbind(not_bus_trips,bus_walk_trips[[1]],bus_walk_trips[[2]])
  }
  
  do.call('rbind',rd_list)
}
