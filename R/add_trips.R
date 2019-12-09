#' Add trips to trip set
#' 
#' Creates a data frame of the same description as a trip set to append
#' 
#' @param trip_ids ids for new trips
#' @param new_mode mode for new trips
#' @param distance distances to sample from
#' @param participant_id participant id for new trips
#' @param age age for participant
#' @param sex sex for participant
#' @param nTrips number of trips for participant
#' @param speed speed for new trips
#' 
#' @return data frame of trips
#' 
#' @export
add_trips <- function(trip_ids=0,new_mode='walking',distance=1,participant_id=0,age=20,sex='male',nTrips=3,speed=4.8){
  dist <- sample(distance,nTrips,replace=T)
  return(data.frame(trip_id   = trip_ids, 
             trip_mode = new_mode, 
             trip_distance = dist, 
             stage_mode = new_mode, 
             stage_distance = dist, 
             stage_duration = 60 * dist / speed, 
             participant_id = participant_id,
             age = sample(age,1,replace=T),
             sex = sample(sex,1,replace=T)))
}
