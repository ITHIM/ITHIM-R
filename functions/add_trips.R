add_trips <- function(trip_ids=0,new_mode='Walking',duration=10,participant_id=0,age=20,sex='Male',nTrips=3){
  data.frame(trip_id   = trip_ids, 
             trip_mode = new_mode, 
             trip_duration = sample(duration,nTrips,replace=T), 
             participant_id = participant_id,
             age = sample(age,1,replace=T),
             sex = sample(sex,1,replace=T))
}
