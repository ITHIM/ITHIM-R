#' Add walk to public transport stages
#'
#' Adds a short walk stage to any public transport (PT) trip if required.
#'
#'
#' This function performs the following steps:
#'
#' \itemize{
#' \item create a list containing the dataframes of the synthetic trips for each scenario
#'
#' \item if ADD_WALK_TO_PT_TRIPS == T, i.e if additional 'walk to pt' stages are to be added:
#'   \itemize{
#'   \item filter out all trips with a public transport stage mode
#'
#'   \item divide public transport trips into those with and without a 'walk to pt' stage
#'
#'  \item add a 'walk to pt' stage to those public transport trips without a walking stage
#'     (\code{\link{add_walk_trips()}})
#'     }
#' \item combine all trips from all scenarios into one dataframe
#'
#' \item scale the stage distances and durations by calling the scale_trip_distances.R function
#' }
#'
#' @param trip_set list of data frames, trips from all scenarios
#'
#' @return data frame, all trips from all scenarios
#'
#' @export


walk_to_pt_and_combine_scen <- function(SYNTHETIC_TRIPS) {
  # create a list containing all the SYNTHETIC_TRIPS dataframes
  rd_list <- list()

  for (i in names(SYNTHETIC_TRIPS)) rd_list[[i]] <- setDT(SYNTHETIC_TRIPS[[i]])
  SYNTHETIC_TRIPS <- NULL

  # define PT modes
  pt_modes <- c("bus", "rail", "minibus", "subway")

  # if walk to pt stages are to be added:
  if (ADD_WALK_TO_PT_TRIPS) {
    for (i in names(rd_list)) { # loop through all scenarios

      rd_list[[i]] <- rd_list[[i]] %>% dplyr::mutate(id = row_number())

      # check that all scenario synthetic trips dataframes contain a trip_mode column
      if (!any(names(rd_list[[i]]) %in% "trip_mode")) {
        print(paste0(CITY, " There are issues with the synthetic trips which do NOT a trip_mode column"))
        break
      }

      # separate out PT trips
      pt_trips <- rd_list[[i]] %>% dplyr::filter(stage_mode %in% pt_modes)

      # further separate out public transport trips WITHOUT pedestrian component
      pt_trips_wo_walk <- rd_list[[i]] %>%
        dplyr::filter(trip_id %in% pt_trips$trip_id) %>%
        group_by(trip_id) %>%
        dplyr::mutate(ped = if (any(stage_mode == "walk_to_pt")) 1 else 0) %>%
        ungroup() %>%
        filter(ped == 0) %>%
        dplyr::select(-ped)

      # check number of pt trips with and without walking stages
      # nrpt <- pt_trips %>% distinct(trip_id) %>% nrow
      # nrptwp <- pt_trips_wo_walk %>% distinct(trip_id) %>% nrow

      # print(CITY)
      # print(paste(nrpt, " - ", round(nrptwp /  nrpt * 100,1)))

      # separate out pt trips WITH pedestrian component
      pt_trips_w_walk <- pt_trips %>%
        filter(trip_id %in% setdiff(pt_trips$trip_id, pt_trips_wo_walk$trip_id))

      # find the trips without a public transport stage
      not_pt_trips <- subset(rd_list[[i]], !id %in% pt_trips$id)

      # add a walking stage component to those pt trips without such a walking stage
      pt_walk_trips <- add_walk_trips(pt_trips_wo_walk)

      # recombine all trips
      rd_list[[i]] <- rbind(not_pt_trips, pt_trips_w_walk, pt_walk_trips[[1]], pt_walk_trips[[2]])

      rd_list[[i]]$id <- NULL
    }
  }

  trip_df <- do.call("rbind", rd_list)
  rd_list <- NULL

  # update all distances and duration
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
#' The function performs the following steps:
#'
#' \itemize{
#' \item define all car and public transport modes
#'
#' \item multiply all stage distances and stage durations by the corresponding distance scalars
#' }
#'
#' @param trips data frame, all trips from all scenarios
#'
#' @return data frame, all trips from all scenarios with scaled distances
#'
#' @export
scale_trip_distances <- function(trips) {
  car_taxi_modes <- c("car", "taxi", "auto_rickshaw", "shared_auto")
  pt_modes <- c("bus", "minibus", "subway", "rail", "walk_to_pt")

  # ignore trip distance as it has already been used to create scenarios and has no other use
  # set-up the scalars for all stage modes
  match_modes <- rep(1, nrow(trips))
  stage_modes <- trips$stage_mode
  match_modes[stage_modes %in% car_taxi_modes] <- DISTANCE_SCALAR_CAR_TAXI
  match_modes[stage_modes == "pedestrian"] <- DISTANCE_SCALAR_WALKING
  match_modes[stage_modes %in% pt_modes] <- DISTANCE_SCALAR_PT
  match_modes[stage_modes == "cycle"] <- DISTANCE_SCALAR_CYCLING
  match_modes[stage_modes == "motorcycle"] <- DISTANCE_SCALAR_MOTORCYCLE

  # scale stage distance and stage duration
  trips$stage_distance <- trips$stage_distance * match_modes
  trips$stage_duration <- trips$stage_duration * match_modes

  return(trips)
}
