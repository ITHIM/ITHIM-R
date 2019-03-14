#' @export
add_distance_columns <- function(injury_table,mode_names,true_distances,scenarios=SCEN){
  
  by_age <- 'age_cat'%in%names(injury_table[[1]])
  by_gender <- 'cas_gender'%in%names(injury_table[[1]])
  age_gender <- by_age&&by_gender
  u_gen <- unique(injury_table[[1]]$cas_gender)
  u_age <- unique(injury_table[[1]]$age_cat)
  age_gen_labels <- apply(expand.grid(u_gen,u_age),1,function(x)paste(x,collapse='_'))
  cas_mode_indices <- list()
  injury_gen_age <- list()
  # initialise tables and store indices
  for(type in c('whw','noov')){
    ##TODO make contingency table without prior knowledge of column names
    gen_index <- match(injury_table[[type]]$cas_gen,u_gen)
    age_index <- match(injury_table[[type]]$age_cat,u_age)
    injury_gen_age[[type]] <- age_gen_labels[length(u_gen)*(age_index-1)+gen_index]
    if(age_gender) injury_table[[type]]$injury_gen_age <- injury_gen_age[[type]]
    cas_mode_indices[[type]] <- match(injury_table[[type]]$cas_mode,mode_names)
  }
  strike_mode_indices <- match(injury_table$whw$strike_mode,mode_names)
  
  ## Calculated distances
  ## true distances should be the total for the whole population for a whole year. 
  ##TODO precalculate and save distances (for uncertainty use case)
  injuries_list <- list()
  for(scen in scenarios){
    injuries_list[[scen]] <- list()
    true_scen_dist <- subset(true_distances,scenario==scen)
    for(type in c('whw','noov')){
      injuries_list[[scen]][[type]] <- injury_table[[type]]
      ##TODO get distances without prior knowledge of column names
      ##TODO differentiate between driver and passenger for casualty and striker distances
      
      # initialise all strike distances as 1
      injuries_list[[scen]][[type]]$strike_distance <- 1
      injuries_list[[scen]][[type]]$strike_distance_sum <- 1
      
      # apply casualty distance sums
      distance_sums <- sapply(mode_names,function(x)sum(true_scen_dist[[x]]))
      injuries_list[[scen]][[type]]$cas_distance_sum <- distance_sums[cas_mode_indices[[type]]]
      
      # apply group-level casualty distances
      if(age_gender){
        cas_demo_indices <- match(injury_gen_age[[type]],true_scen_dist$sex_age)
        injuries_list[[scen]][[type]]$cas_distance <- as.numeric(as.data.frame(true_scen_dist)[cbind(cas_demo_indices,cas_mode_indices[[type]]+2)])
      }else{
        injuries_list[[scen]][[type]]$cas_distance <- injuries_list[[scen]][[type]]$cas_distance_sum 
      }
      
      # apply striker distances for whw
      if(type=='whw'){
        injuries_list[[scen]][[type]]$strike_distance <- distance_sums[strike_mode_indices]
        injuries_list[[scen]][[type]]$strike_distance_sum <- injuries_list[[scen]][[type]]$strike_distance
      }
      
      # omit any rows with zero travel
      injuries_list[[scen]][[type]] <- subset(injuries_list[[scen]][[type]],strike_distance>0&cas_distance>0)
    }
  }
  
  return(injuries_list)
}
