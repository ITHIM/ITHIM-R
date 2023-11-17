#' Additional trips for trip data set
#'
#' Creates a data frame with given characteristics to be added to an existing trip data set.
#'
#' This function is used to create new trips with certain characteristics that can be added to an existing trip data set.
#' The input distance given is the upper limit of trip distances added and the function takes the number of trips per
#' person (nTrips) random samples between 1 and the input distance given. Age and sex of each participant are also
#' sampled from a range of input age and sex values.
#'
#' The function performs the following steps:
#' \itemize{
#' \item create nTrips new trips sampling from distances, ages and sexes
#' }
#'
#' @param trip_ids ids for new trips
#' @param new_mode mode for new trips
#' @param distance distances to sample from
#' @param participant_id participant id for new trips
#' @param age age for participant
#' @param sex sex for participant
#' @param nTrips number of trips for each participant
#' @param speed speed for new trips
#'
#' @return data frame of trips
#'
#' @export
add_trips <- function(trip_ids = 0, new_mode = "pedestrian", distance = 1, participant_id = 0, age = 20, sex = "male", nTrips = 3, speed = 4.8) {
  dist <- sample(distance, nTrips, replace = T) # sample nTrips values between 1 and distance
  return(data.frame(
    trip_id = trip_ids,
    trip_mode = new_mode,
    trip_distance = dist,
    stage_mode = new_mode,
    stage_distance = dist,
    stage_duration = 60 * dist / speed,
    participant_id = participant_id,
    age = sample(age, 1, replace = T),
    sex = sample(sex, 1, replace = T)
  ))
}
