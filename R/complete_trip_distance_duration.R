#' Data harmonisation: add columns to trip set if missing
#' 
#' Creates any columns needed and missing from the trip set, then writes the trip set to the global enviroment
#' 
#' 
#' @export
complete_trip_distance_duration <- function(){
  
  trip_set <- TRIP_SET
  #trip_speed <- sapply(trip_set$trip_mode,function(x){speed <- MODE_SPEEDS$speed[MODE_SPEEDS$stage_mode==x]; ifelse(length(speed)==0,0,speed)})
  stage_speed <- sapply(trip_set$stage_mode,function(x){speed <- MODE_SPEEDS$speed[MODE_SPEEDS$stage_mode==x]; ifelse(length(speed)==0,0,speed)})
  
  
  ## if distance but no duration, add duration
  ## duration = distance / speed * 60
  if('stage_distance'%in%colnames(trip_set)&&!'stage_duration'%in%colnames(trip_set))
    trip_set$stage_duration <- trip_set$stage_distance / stage_speed * 60
  
  na_stage <- is.na(sum(trip_set$stage_duration[!is.na(trip_set$stage_mode)]))
  if(na_stage){
    cat('NA in stage duration in trip set.\n')
    ## populate trip durations by summing stage durations
    if('stage_distance'%in%colnames(trip_set)&&!is.na(sum(trip_set$stage_distance[!is.na(trip_set$stage_mode)]))){
      cat('Populating stage duration from stage distance\n')
      trip_set$stage_duration <- trip_set$stage_distance / stage_speed * 60
    }else if('trip_duration'%in%colnames(trip_set)&&!is.na(sum(trip_set$trip_duration[!is.na(trip_set$stage_mode)]))){
      ## populate trip durations by summing stage durations
      cat('Populating stage duration from trip duration\n')
      na_stage_ids <- trip_set$trip_id[is.na(trip_set$stage_duration)&!is.na(trip_set$stage_mode)]
      na_stage_set <- setDT(subset(trip_set[trip_set$trip_id%in%na_stage_ids,]))
      na_stage_set[,nstages:=.N,by='trip_id']
      na_stage_set[,stage_duration:=trip_duration/nstages]
      other_set <- subset(trip_set[!trip_set$trip_id%in%na_stage_ids,])
      trip_set <- rbind(other_set,as.data.frame(na_stage_set)[,colnames(na_stage_set)%in%colnames(other_set)])
    }
    na_stage_ids <- trip_set$trip_id[is.na(trip_set$stage_duration)&!is.na(trip_set$stage_mode)]
    if(length(na_stage_ids)>0){
      cat(paste0("Removing ",length(unique(na_stage_ids))," trips (",length(na_stage_ids)," stages) with NA stage duration from trip set.\n"))
      trip_set <- subset(trip_set,!trip_id%in%na_stage_ids)
    }
    stage_speed <- sapply(trip_set$stage_mode,function(x){speed <- MODE_SPEEDS$speed[MODE_SPEEDS$stage_mode==x]; ifelse(length(speed)==0,0,speed)})
  }
  
  ## if duration but no distance, add distance
  ## distance = speed * duration / 60
  if('stage_duration'%in%colnames(trip_set)&&!'stage_distance'%in%colnames(trip_set))
    trip_set$stage_distance <- trip_set$stage_duration * stage_speed / 60
  ## depending on the situation there might be other (faster) ways to compute trip_distance,
  ## e.g. as a function of trip duration or it might just be the same as stage_distance but
  ## to allow for all eventualities we just sum the stages of each trip.
  if(!'trip_distance'%in%colnames(trip_set)){
    distances <- setDT(trip_set)[,sum(stage_distance),by='trip_id']
    colnames(distances)[2] <- 'trip_distance'
    trip_set <- left_join(trip_set,distances,by='trip_id')
  }
  
  
  TRIP_SET <<- as.data.frame(trip_set)
    
}