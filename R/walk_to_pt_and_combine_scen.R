#' Add walk to PT stages 
#' 
#' Adds a short walk stage to any PT trip if required.
#'
#' 
#' This function performs the following steps:
#' 
#' - create a list containing the dataframes of the synthetic trips for each scenario
#' - if ADD_WALK_TO_PT_TRIPS == T, i.e if additional 'walk to pt' stages are to be added:
#'   - filter out all trips with a public transport stage mode
#'   - filter out public transport trips with and without a 'walk to pt' stage
#'   - filter out the trips without a public transport stage
#'   - add a 'walk to pt' stage to those public transport trips without a walking stage
#'     (add_walk_trips.R)
#' - combine all trips from all scenarios into one dataframe
#' - scale the stage distances and durations by calling the scale_trip_distances.R function
#' 
#' 
#' @param trip_set list of data frames, trips from all scenarios
#' 
#' @return data frame, all trips from all scenarios
#' 
#' @export


walk_to_pt_and_combine_scen <- function(SYNTHETIC_TRIPS){
  
  # create a list containing all the SYNTHETIC_TRIPS dataframes
  rd_list <- list()
  for(i in 1:length(SYNTHETIC_TRIPS)) rd_list[[i]] <- setDT(SYNTHETIC_TRIPS[[i]])
  SYNTHETIC_TRIPS <- NULL
  
  ## pt = public transport
  pt_modes <- c('bus', 'rail','minibus','subway')
  
  
  if(ADD_WALK_TO_PT_TRIPS){
    for(i in 1:length(rd_list)){
      
      
      rd_list[[i]] <- rd_list[[i]] %>% dplyr::mutate(id = row_number())
      
      # check that all scenario synthetic trips dataframes contain a trip_mode columns
      if (!any(names(rd_list[[i]]) %in% 'trip_mode')){
        print(paste0(CITY," There are issues with the synthetic trips which do NOT a trip_mode column"))
        break
      }
      
      # separate out PT trips
      pt_trips <- rd_list[[i]] %>% dplyr::filter(stage_mode %in% pt_modes)
      
      # further separate out public transport trips WITHOUT pedestrian component
      pt_trips_wo_walk <- rd_list[[i]] %>% dplyr::filter(trip_id %in% pt_trips$trip_id) %>% group_by(trip_id) %>% 
        dplyr::mutate(ped = if(any(stage_mode == 'walk_to_pt')) 1 else 0) %>% 
        ungroup() %>% 
        filter(ped == 0) %>% dplyr::select(-ped)
      
      # check number of pt trips with and without walking stages 
      #nrpt <- pt_trips %>% distinct(trip_id) %>% nrow
      #nrptwp <- pt_trips_wo_walk %>% distinct(trip_id) %>% nrow
      
      # print(CITY)
      # print(paste(nrpt, " - ", round(nrptwp /  nrpt * 100,1)))
      
      # separate pt trips WITH pedestrian component
      pt_trips_w_walk <- pt_trips %>% filter(trip_id %in% setdiff(pt_trips$trip_id, pt_trips_wo_walk$trip_id))
      
      # find the trips without a public transport stage
      not_pt_trips <- subset(rd_list[[i]], !id %in% pt_trips$id)
      
      # add a walking stage component to those pt trips without such a walking stage
      pt_walk_trips <- add_walk_trips(pt_trips_wo_walk)
      
      # recombine all trips
      rd_list[[i]] <- rbind(not_pt_trips, pt_trips_w_walk, pt_walk_trips[[1]],pt_walk_trips[[2]])
      
      rd_list[[i]]$id <- NULL
    }
  }
  
  trip_df <- do.call('rbind',rd_list)
  rd_list <- NULL
  
  ## update all distances and duration
  trip_df <- scale_trip_distances(trip_df)
    
  return(trip_df)
  
}


#' Scale trip distances
#' 
#' Applies mode-specific distance scalars to all trips
#' 
#' The function is used to multiply all trip stages belonging to a certain mode
#' by a city specific scalar. Note that walk to pt stages are counted as
#' public transport stages and are multiplied by the DISTANCE_SCALAR_PT 
#' 
#' @param trips data frame, all trips from all scenarios
#' 
#' @return data frame, all trips from all scenarios with scaled distances
#' 
#' @export
scale_trip_distances <- function(trips){
  car_taxi_modes <- c('car','taxi','auto_rickshaw','shared_auto')
  pt_modes <- c('bus','minibus','subway','rail','walk_to_pt')

  ## omit trip distance as it has already been used to create scenarios and has no other use
  match_modes <- rep(1,nrow(trips))
  stage_modes <- trips$stage_mode
  match_modes[stage_modes%in%car_taxi_modes] <- DISTANCE_SCALAR_CAR_TAXI
  match_modes[stage_modes=='pedestrian'] <- DISTANCE_SCALAR_WALKING
  match_modes[stage_modes%in%pt_modes] <- DISTANCE_SCALAR_PT
  match_modes[stage_modes=='cycle'] <- DISTANCE_SCALAR_CYCLING
  match_modes[stage_modes=='motorcycle'] <- DISTANCE_SCALAR_MOTORCYCLE
  trips$stage_distance <- trips$stage_distance*match_modes
  trips$stage_duration <- trips$stage_duration*match_modes
  
  
  return(trips)
}
