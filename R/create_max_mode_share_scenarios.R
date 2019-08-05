#' @export
create_max_mode_share_scenarios <- function(trip_set){
  
  rdr <- as.data.frame(trip_set)			
  trip_set <- NULL			
  
  rd_list <- list()			
  target_distances <- colnames(SCENARIO_PROPORTIONS)			
  modes <- rownames(SCENARIO_PROPORTIONS)		
  
  rdr <- add_trip_weights(rdr)
  
  # Baseline scenario			
  rd_list[[1]] <- rdr
  
  rdr_not_changeable <-  setDT(rdr[rdr$trip_mode%in%c('bus_driver','truck'),])			
  rdr_changeable <-  rdr[!rdr$trip_mode%in%c('bus_driver','truck'),]			
  rdr <- NULL			
  	
  
  rdr_changeable_by_distance <- list()			
  for(j in 1:ncol(SCENARIO_PROPORTIONS)){			
    target_distance <- target_distances[j]			
    rdr_changeable_by_distance[[j]] <- rdr_changeable[rdr_changeable$trip_distance_cat==target_distance,]			
  }			
  rdr_changeable <- NULL			
  
  ###############################################################			
  for(i in 1:nrow(SCENARIO_PROPORTIONS)){			
    mode_name <- modes[i]			
    rdr_copy <- list()			
    for(j in 1:ncol(SCENARIO_PROPORTIONS)){			
      # get all trips and trips that can be changed			
      rdr_copy[[j]] <- rdr_changeable_by_distance[[j]]			
      rdr_copy_j_trip_id <- rdr_copy[[j]]$trip_id			
      unique_trips <- rdr_copy[[j]][!duplicated(rdr_copy_j_trip_id),]		
      potential_index <- unique_trips$trip_mode!=mode_name
      #potential_trips <- unique_trips[unique_trips$trip_mode!=mode_name,]	
      potential_trip_ids <- unique_trips$trip_id[potential_index]	
      potential_trip_weights <- unique_trips$trip_weight[potential_index]	
      # calculate			
      current_number <- sum(unique_trips$trip_mode%in%mode_name)			
      total_number <- nrow(unique_trips)			
      current_number_fraction <- current_number / total_number			
      current_weight <- sum(unique_trips$trip_weight[unique_trips$trip_mode==mode_name])			
      total_weight <- sum(unique_trips$trip_weight)			
      current_weight_fraction <- current_weight / total_weight			
      target_number_fraction <- SCENARIO_PROPORTIONS[i,j] /100			
      target_weight <- target_number_fraction*total_weight#
      #ifelse(current_weight_fraction==0,target_number_fraction*total_weight, current_number_fraction/current_weight_fraction *target_number_fraction*total_weight)	
      weight_to_gain <- target_weight -  current_weight	
      #if(weight_to_gain >= sum(potential_trip_weights)){
      #  change_trip_ids <- potential_trip_ids
      #}else 
        if(length(potential_trip_ids)>0&&weight_to_gain>0){			
        # sample trips until weight is achieved			
        cumulative_weight <- 0			
        change_trip_ids <- c()			
        weights <- potential_trip_weights			
        if(weight_to_gain>30){			
          # staircase sample for unequal weights			
          sample_size <- round(weight_to_gain)-20			
          stair_weights <- cumsum(weights/sum(weights))*sample_size + runif(1)			
          first_weight <- sum(stair_weights<1)+1	
          floor_weights <- floor(stair_weights)			
          sample_indicies <- (first_weight+1):length(floor_weights)			
          new_index <- first_weight+c(0,which(floor_weights[sample_indicies]>floor_weights[sample_indicies-1]))			
          # too slow: new_index <- base::sample.int(length(potential_trip_ids),size=sample_size,prob=weights,replace=F)			
          new_trip <- potential_trip_ids[new_index]			
          change_trip_ids <- c(change_trip_ids,new_trip)			
          weight_gained <- potential_trip_weights[new_index]			
          weights[new_index] <- 0#max(0,weight_gained-1)			
          cumulative_weight <- cumulative_weight + sum(weight_gained)			
        }			
        while(cumulative_weight<weight_to_gain){			
          stair_weights <- cumsum(weights/sum(weights)) + runif(1)			
          new_index <- sum(stair_weights<1)+1		
          #new_index <- base::sample(1:length(potential_trip_ids),size=1,prob=weights,replace=F)			
          new_trip <- potential_trip_ids[new_index]			
          change_trip_ids <- c(change_trip_ids,new_trip)			
          weight_gained <- potential_trip_weights[new_index]			
          weights[new_index] <- 0#max(0,weight_gained-1)			
          cumulative_weight <- cumulative_weight + weight_gained			
        }			
        change_trips <- unique_trips[unique_trips$trip_id %fin% change_trip_ids,]		
        unique_trips <- NULL
        change_trips$trip_mode <- mode_name			
        change_trips$stage_mode <- mode_name			
        change_trips$stage_duration <- change_trips$stage_distance * 60 / MODE_SPEEDS$speed[MODE_SPEEDS$stage_mode == mode_name]			
        ## if bus scenario, add walk to bus: 			
        if (mode_name == 'bus'){			
          walk_trips <- change_trips;			
          walk_trips$stage_mode <- 'walk_to_pt';			
          walk_trips$stage_duration <- BUS_WALK_TIME;			
          walk_trips$stage_distance <- walk_trips$stage_duration * MODE_SPEEDS$speed[MODE_SPEEDS$stage_mode == 'walking'] / 60			
          change_trips <- rbind(change_trips, walk_trips)			
        }			
        rdr_copy[[j]] <- rbindlist(list(rdr_copy[[j]][!rdr_copy_j_trip_id%fin%change_trip_ids,],change_trips))		
        rdr_copy_j_trip_id <- NULL
      }			
    }			
    rdr_scen <- rbindlist(rdr_copy)			
    rdr_copy <- NULL
    if(nrow(rdr_not_changeable)>0) rdr_scen <- rbind(rdr_scen,rdr_not_changeable)			
    rdr_scen[,'scenario':=paste0('Scenario ',i)]
    rd_list[[i+1]] <- rdr_scen		
    rdr_scen <- NULL
  }
  
  
  ###############################################################
  
  return(rd_list)
}

#' @export
`%fin%` <- function(x, table) {
  stopifnot(require(fastmatch))
  fmatch(x, table, nomatch = 0L) > 0L
}

#' @export
add_trip_weights <- function(rdr){
  
  
  ## add trip weights			
  car_taxi_modes <- UNCERTAIN_TRAVEL_MODE_NAMES$car			
  pt_modes <- UNCERTAIN_TRAVEL_MODE_NAMES$pt			
  ## weight by mode probability scalars			
  match_modes <- rep(1,nrow(rdr))			
  travel_modes <- rdr$trip_mode			
  match_modes[travel_modes%in%car_taxi_modes] <- PROBABILITY_SCALAR_CAR_TAXI			
  match_modes[travel_modes%in%c('walking')] <- PROBABILITY_SCALAR_WALKING			
  match_modes[travel_modes%in%pt_modes] <- PROBABILITY_SCALAR_PT			
  match_modes[travel_modes%in%c('cycling')] <- PROBABILITY_SCALAR_CYCLING			
  match_modes[travel_modes%in%c('motorcycle')] <- PROBABILITY_SCALAR_MOTORCYCLE			
  rdr$trip_weight <- match_modes			
  return(rdr)
  
}

# rdr <- setDT(trip_set)
# setkey(rdr,trip_id)
# trip_set <- NULL
# 
# rd_list <- list()
# target_distances <- colnames(SCENARIO_PROPORTIONS)
# modes <- rownames(SCENARIO_PROPORTIONS)
# # Baseline scenario
# rd_list[[1]] <- rdr
# rdr_not_changeable <-  rdr[trip_mode%in%c('bus_driver','truck')]
# rdr_changeable <-  rdr[!trip_mode%in%c('bus_driver','truck')]
# rdr <- NULL
# ## add trip weights
# car_taxi_modes <- UNCERTAIN_TRAVEL_MODE_NAMES$car
# pt_modes <- UNCERTAIN_TRAVEL_MODE_NAMES$pt
# ## weight by mode probability scalars
# match_modes <- rep(1,nrow(rdr_changeable))
# travel_modes <- rdr_changeable$trip_mode
# match_modes[travel_modes%in%car_taxi_modes] <- PROBABILITY_SCALAR_CAR_TAXI
# match_modes[travel_modes%in%c('walking')] <- PROBABILITY_SCALAR_WALKING
# match_modes[travel_modes%in%pt_modes] <- PROBABILITY_SCALAR_PT
# match_modes[travel_modes%in%c('cycling')] <- PROBABILITY_SCALAR_CYCLING
# match_modes[travel_modes%in%c('motorcycle')] <- PROBABILITY_SCALAR_MOTORCYCLE
# rdr_changeable[, 'trip_weight' := match_modes]
# 
# rdr_changeable_by_distance <- list()
# for(j in 1:ncol(SCENARIO_PROPORTIONS)){
#   target_distance <- target_distances[j]
#   rdr_changeable_by_distance[[j]] <- rdr_changeable[rdr_changeable$trip_distance_cat==target_distance,]
# }
# rdr_changeable <- NULL
# 
# ###############################################################
# for(i in 1:nrow(SCENARIO_PROPORTIONS)){
#   mode_name <- modes[i]
#   rdr_copy <- list()
#   for(j in 1:ncol(SCENARIO_PROPORTIONS)){
#     # get all trips and trips that can be changed
#     rdr_copy[[j]] <- rdr_changeable_by_distance[[j]]
#     rdr_copy_j_trip_id <- rdr_copy[[j]]$trip_id
#     unique_ids <- unique(rdr_copy_j_trip_id)
#     unique_trips <- rdr_copy[[j]][!duplicated(rdr_copy_j_trip_id)]
#     potential_trips <- unique_trips[trip_mode!=mode_name]
#     potential_trip_ids <- potential_trips$trip_id
#     #unique_trips[,.(potential=trip_id%fin%..potential_trip_ids)]
#     #unique_trips[, 'potential' := trip_id%fin%..potential_trip_ids]
#     # calculate
#     current_number <- sum(unique_trips$trip_mode%in%mode_name)
#     total_number <- nrow(unique_trips)
#     current_number_fraction <- current_number / total_number
#     current_weight <- sum(unique_trips$trip_weight[unique_trips$trip_mode==mode_name])
#     total_weight <- sum(unique_trips$trip_weight)
#     unique_trips <- NULL
#     current_weight_fraction <- current_weight / total_weight
#     target_number_fraction <- SCENARIO_PROPORTIONS[i,j] /100
#     target_weight <- current_number_fraction/current_weight_fraction*target_number_fraction*total_weight
#     weight_to_gain <- target_weight -  current_weight
#     if(length(potential_trip_ids)>0&&weight_to_gain>0){
#       # sample trips until weight is achieved
#       cumulative_weight <- 0
#       change_trip_ids <- c()
#       weights <- potential_trips$trip_weight
#       if(weight_to_gain>30){
#         # staircase sample for unequal weights
#         sample_size <- round(weight_to_gain)-20
#         stair_weights <- cumsum(weights/sum(weights))*sample_size + runif(1)
#         first_weight <- which(stair_weights>1)[1]
#         floor_weights <- floor(stair_weights)
#         sample_indicies <- (first_weight+1):length(floor_weights)
#         new_index <- first_weight+c(0,which(floor_weights[sample_indicies]>floor_weights[sample_indicies-1]))
#         # too slow: new_index <- base::sample.int(length(potential_trip_ids),size=sample_size,prob=weights,replace=F)
#         new_trip <- potential_trip_ids[new_index]
#         change_trip_ids <- c(change_trip_ids,new_trip)
#         weight_gained <- potential_trips$trip_weight[new_index]
#         weights[new_index] <- 0#max(0,weight_gained-1)
#         cumulative_weight <- cumulative_weight + sum(weight_gained)
#       }
#       while(cumulative_weight<weight_to_gain){
#         stair_weights <- cumsum(weights/sum(weights)) + runif(1)
#         new_index <- which(stair_weights>1)[1]
#         #new_index <- base::sample(1:length(potential_trip_ids),size=1,prob=weights,replace=F)
#         new_trip <- potential_trip_ids[new_index]
#         change_trip_ids <- c(change_trip_ids,new_trip)
#         weight_gained <- potential_trips$trip_weight[new_index]
#         weights[new_index] <- 0#max(0,weight_gained-1)
#         cumulative_weight <- cumulative_weight + weight_gained
#       }
#       change_trips <- potential_trips[potential_trips$trip_id %in% change_trip_ids,]
#       mode_speed <- MODE_SPEEDS$speed[MODE_SPEEDS$stage_mode == mode_name]
#       change_trips[,`:=`(trip_mode=..mode_name,stage_mode=..mode_name,stage_duration=stage_distance*60/..mode_speed)]
#       #change_trips$stage_mode <- mode_name
#       #change_trips$stage_duration <- change_trips$stage_distance * 60 / MODE_SPEEDS$speed[MODE_SPEEDS$stage_mode == mode_name]
#       ## if bus scenario, add walk to bus: 
#       if (mode_name == 'bus'){
#         walk_trips <- copy(change_trips)
#         mode_speed <- MODE_SPEEDS$speed[MODE_SPEEDS$stage_mode == 'walk_to_pt']
#         walk_trips[,`:=`(trip_mode='walk_to_pt',stage_mode='walk_to_pt',stage_duration=BUS_WALK_TIME,stage_distance=stage_duration/60*..mode_speed)]
#         #walk_trips$stage_mode <- 'walk_to_pt';
#         #walk_trips$stage_duration <- BUS_WALK_TIME;
#         #walk_trips$stage_distance <- walk_trips$stage_duration * MODE_SPEEDS$speed[MODE_SPEEDS$stage_mode == 'walking'] / 60
#         change_trips <- rbind(change_trips, walk_trips)
#       }
#       kept_trips <- rdr_copy[[j]][!rdr_copy_j_trip_id%fin%change_trip_ids,]
#       rdr_copy[[j]] <- rbind(kept_trips,change_trips)
#     }
#   }
#   rdr_scen <- rbindlist(rdr_copy)
#   rdr_scen <- rbind(rdr_scen,rdr_not_changeable)
#   #scenario_name <- paste0('Scenario ',i)
#   #rdr_scen[,.(scenario=..scenario_name)]
#   rd_list[[i+1]] <- as.data.frame(rdr_scen)
#   rd_list[[i+1]]$scenario <- paste0('Scenario ',i)
# }