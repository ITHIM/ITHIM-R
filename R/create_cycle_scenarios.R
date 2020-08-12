#' Creates cycling scenarios
#' 
#' Creates five scenarios with 10-50% cycling
#' 
#' @param trip_set data frame of baseline trips
#' 
#' @return list of scenarios
#' 
#' @export
create_cycle_scenarios <- function(trip_set){
  rdr <- trip_set
  rd_list <- list()
  target_distance <- '2-5 km'
  # Baseline scenario
  rd_list[[1]] <- rdr
  print(c(sum(rdr$trip_distance_cat==target_distance),sum(rdr$trip_distance_cat==target_distance&rdr$trip_mode=='cycle')))
  ###############################################################
  for(i in 1:5){
    # Scenario i: i*10% cycle
    short_trips <- subset(rdr,trip_distance_cat==target_distance)
    potential_trip_ids <- unique(subset(short_trips,trip_mode!='cycle')$trip_id)
    current_cycle_trips <- sum(short_trips$trip_mode=='cycle')
    target_percent <- 10*i
    change_trip_ids <- base::sample(potential_trip_ids,size=max(1,round(length(unique(short_trips$trip_id))/100*target_percent)-current_cycle_trips))
    change_trips <- subset(short_trips,trip_id%in%change_trip_ids)
    change_trips$trip_mode <- 'cycle'
    change_trips$stage_mode <- 'cycle'
    change_trips$stage_duration <- change_trips$stage_distance * 60 / MODE_SPEEDS$speed[MODE_SPEEDS$stage_mode=='cycle']
    
    rdr <- rbind(subset(rdr,!trip_id%in%change_trip_ids),change_trips)
    print(c(sum(rdr$trip_distance_cat==target_distance),sum(rdr$trip_distance_cat==target_distance&rdr$trip_mode=='cycle')))
    rdr$scenario <- paste0('Scenario ',i)
    rd_list[[i+1]] <- rdr
  }
  ###############################################################
  
  return(rd_list)
}
