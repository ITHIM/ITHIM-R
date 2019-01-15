#' @export
distances_for_injury_function <- function(trip_scen_sets){
  ##!! RJ need to scale distances up for representativeness of survey population of total population
  
  journeys <- trip_scen_sets %>% 
    group_by (age_cat,sex,trip_mode, scenario) %>% 
    summarise(tot_dist = sum(trip_distance))
  distances <- spread(journeys,trip_mode, tot_dist,fill=0) 
  distances$Pedestrian <- distances$Walking 
  distances <- distances[, -which(names(distances) ==  "Walking")]
  if(ADD_WALK_TO_BUS_TRIPS){
    distances$Pedestrian <- distances$Pedestrian + distances$`Short Walking`
    distances <- distances[, -which(names(distances) ==  "Short Walking")]
  }
  distances$Car <- distances$Taxi + distances$`Private Car`
  distances <- distances[, -which(names(distances) ==  "Private Car")]
  distances <- distances[, -which(names(distances) ==  "Taxi")]
  true_distances <- distances
  true_distances$sex_age <-  paste0(true_distances$sex,"_",true_distances$age_cat)
  if(ADD_BUS_DRIVERS) true_distances$Bus <- true_distances$Bus + true_distances$Bus_driver
  true_distances <- true_distances[,-c(which(names(true_distances) == 'sex'))]
  
  scen_dist <- sapply(1:(NSCEN+1),function(x)c(colSums(subset(distances,scenario == SCEN[x])[,3+1:(length(unique(journeys$trip_mode))-2)])))
  colnames(scen_dist) <- SCEN_SHORT_NAME
  for(i in 2:ncol(scen_dist)) scen_dist[,i] <- scen_dist[,i]/scen_dist[,1] 
  if(CITY=='accra') scen_dist <- rbind(scen_dist,Tuktuk=1)
  
  mode_names <- names(distances)[3+1:(length(unique(journeys$trip_mode))-2)]
  for (i in 1: length(mode_names))
    for (n in 1:(NSCEN+1))
      distances[[mode_names[i]]][which(distances$scenario == SCEN[n])] <- 
    distances[[mode_names[i]]][which(distances$scenario == SCEN[n])]/ sum(distances[[mode_names[i]]][which(distances$scenario == SCEN[n])],na.rm=T)
  relative_distances <- distances
  relative_distances$sex_age <-  paste0(relative_distances$sex,"_",relative_distances$age_cat)
  relative_distances <- relative_distances[,-c(which(names(relative_distances) == 'sex'))]
  
  ##RJ set up injury regression 
  mode_names <- names(true_distances)[!names(true_distances)%in%c('age_cat','scenario','sex_age')]
  # divide injuries into those for which we can write a WHW matrix, i.e. we know distances of both striker and casualty, 
  ## and those for which we don't know striker distance: no or other vehicle (noov)
  ## we can only model casualties for which we know distance travelled (i.e. no Truck casualties for Accra)
  injury_table <- INJURY_TABLE
  u_gen <- unique(injury_table[[1]]$cas_gender)
  u_age <- unique(injury_table[[1]]$cas_age)
  age_gen_labels <- apply(expand.grid(u_gen,u_age),1,function(x)paste(x,collapse='_'))
  cas_mode_indices <- list()
  injury_gen_age <- list()
  for(type in c('whw','noov')){
    ##TODO make contingency table without prior knowledge of column names
    gen_index <- match(injury_table[[type]]$cas_gen,u_gen)
    age_index <- match(injury_table[[type]]$cas_age,u_age)
    injury_gen_age[[type]] <- age_gen_labels[length(u_gen)*(age_index-1)+gen_index]
    injury_table[[type]]$injury_gen_age <- injury_gen_age[[type]]
    cas_mode_indices[[type]] <- match(injury_table[[type]]$cas_mode,mode_names)
  }
  strike_mode_indices <- match(injury_table$whw$strike_mode,mode_names)
  
  ## Calculated distances
  ## true distances should be the total for the whole population for a whole year. 
  ##TODO precalculate and save distances (for uncertainty use case)
  injuries_list <- list()
  for(scen in SCEN){
    injuries_list[[scen]] <- list()
    true_scen_dist <- subset(true_distances,scenario==scen)
    for(type in c('whw','noov')){
      injuries_list[[scen]][[type]] <- injury_table[[type]]
      ##TODO get distances without prior knowledge of column names
      ##TODO differentiate between driver and passenger for casualty and striker distances
      injuries_list[[scen]][[type]]$strike_distance <- 1
      injuries_list[[scen]][[type]]$strike_distance_sum <- 1
      distance_sums <- sapply(mode_names,function(x)sum(true_scen_dist[[x]]))
      injuries_list[[scen]][[type]]$cas_distance_sum <- distance_sums[cas_mode_indices[[type]]]
      
      cas_demo_indices <- match(injury_gen_age[[type]],true_scen_dist$sex_age)
      injuries_list[[scen]][[type]]$cas_distance <- as.numeric(as.data.frame(true_scen_dist)[cbind(cas_demo_indices,cas_mode_indices[[type]]+2)])
      
      if(type=='whw'){
        injuries_list[[scen]][[type]]$strike_distance <- distance_sums[strike_mode_indices]
        injuries_list[[scen]][[type]]$strike_distance_sum <- injuries_list[[scen]][[type]]$strike_distance
      }
      injuries_list[[scen]][[type]] <- subset(injuries_list[[scen]][[type]],strike_distance>0&cas_distance>0)
      
    }
  }
  
  reg_model <- list()
  ##TODO write formulae without prior knowledge of column names
  ##TODO use all ages. ns.
  ##TODO different formulae for whw and noov
  ##!! need a catch for when regression fails. E.g., if fail, run simpler model.
  for(type in c('whw','noov')){
    injuries_list[[1]][[type]]$injury_reporting_rate <- 1
    suppressWarnings(reg_model[[type]] <- glm(count~cas_mode+strike_mode+cas_age+cas_gender,data=injuries_list[[1]][[type]],family='poisson',
                                              offset=0.5*log(cas_distance)+0.5*log(strike_distance)-log(injury_reporting_rate),control=glm.control(maxit=100)))
    reg_model[[type]] <- trim_glm_object(reg_model[[type]])
  }
  ##
  ## For predictive uncertainty, we could sample a number from the predicted distribution
  for(scen in SCEN)
    for(type in c('whw','noov'))
      injuries_list[[scen]][[type]] <- subset(injuries_list[[scen]][[type]],year==2016)
  
  list(relative_distances=relative_distances,scen_dist=scen_dist,true_distances=true_distances,injuries_list=injuries_list,reg_model=reg_model)
}
