#' @export
walk_to_bus_and_combine_scen <- function(){
  rd_list <- SYNTHETIC_TRIPS
  
  if(ADD_WALK_TO_BUS_TRIPS)
    for(i in 1:length(rd_list)){
      # separate out bus trips
      bus_trips <- subset(rd_list[[i]],stage_mode=='bus')
      not_bus_trips <- subset(rd_list[[i]],stage_mode!='bus')
      # divide bus trips into bus and walking
      bus_walk_trips <- add_walk_trips(bus_trips)
      # recombine all trips
      rd_list[[i]] <- rbind(not_bus_trips,bus_walk_trips[[1]],bus_walk_trips[[2]])
    }
    
  return(do.call('rbind',rd_list))
  
}
