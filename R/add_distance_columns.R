#' @export
add_distance_columns <- function(injury_table,mode_names,true_distances_0,scenarios=SCEN){
  
  injury_temp <- injury_table
  
  by_age <- 'age_cat'%in%names(injury_table[[1]])
  by_gender <- 'cas_gender'%in%names(injury_table[[1]])
  for(type in c('whw','noov')){
    if(!by_age) injury_temp[[type]]$age_cat <- 1
    if(!by_gender) injury_temp[[type]]$cas_gender <- 1
  }
  u_gen <- unique(injury_temp[[1]]$cas_gender)
  u_age <- unique(injury_temp[[1]]$age_cat)
  dem_index_table <- expand.grid(cas_gender=u_gen,age_cat=u_age)
  
  if(!by_age) true_distances_0$age_cat <- 1
  if(!by_gender) true_distances_0$sex <- 1
  true_distances_0$dem_index <- length(u_gen)*(match(true_distances_0$age_cat,u_age)-1) + match(true_distances_0$sex,u_gen)
  
  cas_mode_indices <- list()
  dem_index <- list()
  # initialise tables and store indices
  for(type in c('whw','noov')){
    ##TODO make contingency table without prior knowledge of column names
    gen_index <- match(injury_temp[[type]]$cas_gen,u_gen)
    age_index <- match(injury_temp[[type]]$age_cat,u_age)
    dem_index[[type]] <- length(u_gen)*(age_index-1)+gen_index
    cas_mode_indices[[type]] <- match(injury_table[[type]]$cas_mode,mode_names)
  }
  ## group 2W and 3W striking vehicles
  strike_distances <- true_distances_0
  strike_distances$motorcycle <- rowSums(strike_distances[,colnames(strike_distances)%in%c('motorcycle','auto_rickshaw'),drop=F])
  strike_mode_indices <- match(injury_table$whw$strike_mode,mode_names)
  
  ## Calculated distances
  ## true distances should be the total for the whole population for a whole year. 
  ##TODO precalculate and save distances (for uncertainty use case)
  injuries_list <- list()
  for(scen in scenarios){
    injuries_list[[scen]] <- list()
    true_scen_dist <- subset(true_distances_0,scenario==scen)
    dist_summary <- as.data.frame(t(sapply(unique(true_scen_dist$dem_index),function(x)
      colSums(subset(true_scen_dist,dem_index==x)[,!colnames(true_scen_dist)%in%c('age_cat','sex','scenario','sex_age','dem_index')]))))
    strike_true_scen_dist <- subset(strike_distances,scenario==scen)
    strike_dist_summary <- as.data.frame(t(sapply(unique(strike_true_scen_dist$dem_index),function(x)
      colSums(subset(strike_true_scen_dist,dem_index==x)[,!colnames(strike_true_scen_dist)%in%c('age_cat','sex','scenario','sex_age','dem_index')]))))
    for(type in c('whw','noov')){
      injuries_list[[scen]][[type]] <- injury_table[[type]]
      ##TODO get distances without prior knowledge of column names
      ##TODO differentiate between driver and passenger for casualty and striker distances
      
      # initialise all strike distances as 1
      injuries_list[[scen]][[type]]$strike_distance <- 1
      injuries_list[[scen]][[type]]$strike_distance_sum <- 1
      
      # apply casualty distance sums
      distance_sums <- sapply(mode_names,function(x)sum(dist_summary[[x]]))
      strike_distance_sums <- sapply(mode_names,function(x)sum(strike_dist_summary[[x]]))
      injuries_list[[scen]][[type]]$cas_distance_sum <- distance_sums[cas_mode_indices[[type]]]
      
      # apply group-level casualty distances
      injuries_list[[scen]][[type]]$cas_distance <- as.numeric(as.data.frame(dist_summary)[cbind(dem_index[[type]],cas_mode_indices[[type]])])
      
      # apply striker distances for whw
      if(type=='whw'){
        injuries_list[[scen]][[type]]$strike_distance <- strike_distance_sums[strike_mode_indices]
        injuries_list[[scen]][[type]]$strike_distance_sum <- injuries_list[[scen]][[type]]$strike_distance
      }
      
      # omit any rows with zero travel
      injuries_list[[scen]][[type]] <- subset(injuries_list[[scen]][[type]],strike_distance>0&cas_distance>0)
    }
  }
  
  return(injuries_list)
}
