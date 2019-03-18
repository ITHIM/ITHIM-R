#' @export
complete_trip_distance_duration <- function(){
  
  trip_set <- TRIP_SET
  trip_speed <- sapply(trip_set$trip_mode,function(x){speed <- MODE_SPEEDS$speed[MODE_SPEEDS$stage_mode==x]; ifelse(length(speed)==0,0,speed)})
  stage_speed <- sapply(trip_set$stage_mode,function(x){speed <- MODE_SPEEDS$speed[MODE_SPEEDS$stage_mode==x]; ifelse(length(speed)==0,0,speed)})
  
  
  ## if distance but no duration, add duration
  ## duration = distance / speed * 60
  if('stage_distance'%in%colnames(trip_set)&&!'stage_duration'%in%colnames(trip_set))
    trip_set$stage_duration <- trip_set$stage_distance / stage_speed * 60
  ## if duration but no distance, add distance
  ## distance = speed * duration / 60
  if('stage_duration'%in%colnames(trip_set)&&!'stage_distance'%in%colnames(trip_set))
    trip_set$stage_distance <- trip_set$stage_duration * stage_speed / 60
  ## depending on the situation there might be other (faster) ways to compute trip_distance,
  ## e.g. as a function of trip duration or it might just be the same as stage_distance but
  ## to allow for all eventualities we just sum the stages of each trip.
  if(!'trip_distance'%in%colnames(trip_set)){
    trip_set$trip_distance <- sapply(trip_set$trip_id,function(x)sum(subset(trip_set,trip_id==x)$stage_distance))
  }
  
  TRIP_SET <<- trip_set
    
}