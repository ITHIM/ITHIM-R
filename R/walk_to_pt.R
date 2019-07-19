#' @export
walk_to_pt <- function(rd_list){
  
  ## pt = public transport
  pt_modes <- c('bus','minibus','subway','rail')
  
  if(ADD_WALK_TO_BUS_TRIPS)
    for(i in 1:length(rd_list)){
      # separate out bus trips
      pt_trips <- rd_list[[i]][rd_list[[i]]$stage_mode%in%pt_modes,]
      not_pt_trips <- rd_list[[i]][!rd_list[[i]]$stage_mode%in%pt_modes,]
      # divide bus trips into bus and walking
      pt_walk_trips <- add_walk_trips(pt_trips)
      # recombine all trips
      rd_list[[i]] <- rbind(not_pt_trips,pt_walk_trips[[1]],pt_walk_trips[[2]])
    }
  
  return(rd_list)
  
}

