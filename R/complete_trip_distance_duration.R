#' Add missing trip information
#'
#' Adds any missing trip information such as stage duration or stage distance.
#'
#'
#' This function performs to following steps to calculate stage duration or stage distance. Note that
#' we need at least trip duration or stage distance or stage stage duration:
#'
#' \itemize{
#' \item add speed column
#'
#' \item if there is no stage duration:
#'  \itemize{
#'  \item calculate the stage duration if the stage distance is given using the mode speeds
#'
#'  \item if the stage duration sum is 'NA' (i.e. if not all stage distances exist or if there are no stage distances at all):
#'      \itemize{
#'      \item if trip duration exists and its sum is not NA, then find the trip ids with 'NA' stage duration and replace those
#'        stage duration with the trip duration divided by the number of stages of that particular trip
#'
#'      \item remove any remaining trips with 'NA' stage distance
#'
#'      \item update the stage speed column
#'      }
#'    }
#'
#' \item if there is no stage distance:
#'   \itemize{
#'   \item calculate stage distance using the stage duration and mode speeds
#'    }
#' \item if there is no trip distance:
#'  \itemize{
#'  \item calculate trip distance by summing over the stage distances
#' }
#' }
#'
#' @export


complete_trip_distance_duration <- function() {
  trip_set <- TRIP_SET

  # add a speed column
  stage_speed <- sapply(trip_set$stage_mode, function(x) {
    speed <- MODE_SPEEDS$speed[MODE_SPEEDS$stage_mode == x]
    ifelse(length(speed) == 0, 0, speed)
  })


  ## if distance but no duration, add duration
  ## duration = distance / speed * 60
  if ("stage_distance" %in% colnames(trip_set) && !"stage_duration" %in% colnames(trip_set)) {
    trip_set$stage_duration <- trip_set$stage_distance / stage_speed * 60
  }

  # check if the sum of stage durations is 'na'
  na_stage <- is.na(sum(trip_set$stage_duration[!is.na(trip_set$stage_mode)]))
  if (na_stage) {
    cat("NA in stage duration in trip set.\n")

    ### The following bit of code seems redundant as stage duration is already updated with stage distance (and speed) above
    # # if stage distance exists and the sum of stage distance is not 'na', then update stage duration by using stage distance and speed
    # if('stage_distance'%in%colnames(trip_set)&&!is.na(sum(trip_set$stage_distance[!is.na(trip_set$stage_mode)]))){
    #   cat('Populating stage duration from stage distance\n')
    #   trip_set$stage_duration <- trip_set$stage_distance / stage_speed * 60

    # else if trip duration exists, then find the trip ids with 'na' stage duration and update the corresponding trip id stage durations
    # with the trip duration divided by the number of stages for that trip
    # }else if('trip_duration'%in%colnames(trip_set)&&!is.na(sum(trip_set$trip_duration[!is.na(trip_set$stage_mode)]))){

    if ("trip_duration" %in% colnames(trip_set) && !is.na(sum(trip_set$trip_duration[!is.na(trip_set$stage_mode)]))) {
      cat("Populating stage duration from trip duration\n")
      na_stage_ids <- trip_set$trip_id[is.na(trip_set$stage_duration) & !is.na(trip_set$stage_mode)]
      na_stage_set <- setDT(subset(trip_set[trip_set$trip_id %in% na_stage_ids, ]))
      na_stage_set[, nstages := .N, by = "trip_id"]
      na_stage_set[, stage_duration := trip_duration / nstages]
      other_set <- subset(trip_set[!trip_set$trip_id %in% na_stage_ids, ])
      trip_set <- rbind(other_set, as.data.frame(na_stage_set)[, colnames(na_stage_set) %in% colnames(other_set)])
    }

    na_stage_ids <- trip_set$trip_id[is.na(trip_set$stage_duration) & !is.na(trip_set$stage_mode)]
    # remove any stages with zero stage duration
    if (length(na_stage_ids) > 0) {
      cat(paste0("Removing ", length(unique(na_stage_ids)), " trips (", length(na_stage_ids), " stages) with NA stage duration from trip set.\n"))
      trip_set <- subset(trip_set, !trip_id %in% na_stage_ids)
    }
    stage_speed <- sapply(trip_set$stage_mode, function(x) {
      speed <- MODE_SPEEDS$speed[MODE_SPEEDS$stage_mode == x]
      ifelse(length(speed) == 0, 0, speed)
    })
  }

  ## if duration but no distance, add distance
  ## distance = speed * duration / 60
  if ("stage_duration" %in% colnames(trip_set) && !"stage_distance" %in% colnames(trip_set)) {
    trip_set$stage_distance <- trip_set$stage_duration * stage_speed / 60
  }

  # compute trip distance by summing over the stage distances
  ## depending on the situation there might be other (faster) ways to compute trip_distance,
  ## e.g. as a function of trip duration or it might just be the same as stage_distance but
  ## to allow for all eventualities we just sum the stages of each trip.
  if (!"trip_distance" %in% colnames(trip_set)) {
    distances <- setDT(trip_set)[, sum(stage_distance), by = "trip_id"]
    colnames(distances)[2] <- "trip_distance"
    # joins two data.tables, returns data.table
    trip_set <- dplyr::left_join(trip_set, distances, by = "trip_id")
  }


  TRIP_SET <<- as.data.frame(trip_set)
}
