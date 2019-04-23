#' @export
generate_synthetic_travel_data <- function(trip_scen_sets){
  trip_superset <- assign_age_groups(TRIP_SET)
  
  modes_to_keep <- unique(trip_scen_sets[[1]]$stage_mode)
  modes <- modes_to_keep#unlist(UNCERTAIN_TRAVEL_MODE_NAMES)
  
  missing_trips <- subset(trip_superset,!stage_mode%in%modes_to_keep&!duplicated(participant_id))
  demographic <- DEMOGRAPHIC
  colnames(demographic)[which(colnames(demographic)=='age')] <- 'age_cat'
  
  missing_trips <- left_join(missing_trips,demographic,by=c('sex','age_cat'))
  
  
  pp_summary <- list()#PP_TRAVEL_PROPENSITIES[,colnames(PP_TRAVEL_PROPENSITIES)%in%c('participant_id','dem_index')]
  
  for(scen in 1:length(trip_scen_sets)){
    
    superset <- left_join(trip_scen_sets[[scen]],demographic,by=c('sex','age_cat'))
    
    travel_summary <- expand.grid(dem_index=unique(demographic$dem_index),mode=modes)
    travel_summary$non_travellers <- sapply(travel_summary$dem_index,function(x)sum(missing_trips$dem_index==x))
    travel_summary$travellers <- apply(travel_summary,1,function(x)length(unique(subset(superset,stage_mode==x[2]&dem_index==x[1])$participant_id)))
    ##!! remove zero-travel demographic groups
    travel_summary <- travel_summary[travel_summary$non_travellers>0|travel_summary$travellers>0,]
    raw_probability <- travel_summary$travellers/(travel_summary$travellers+travel_summary$non_travellers)
    smooth_probability <- suppressWarnings(
      glm(raw_probability~I(dem_index<(max(dem_index)/2))+I(dem_index%%(max(dem_index)/2))+mode,
          family=binomial,offset=log(travellers+non_travellers),data=travel_summary)$fitted.values)
    #travel_summary$n_trips <- apply(travel_summary,1,function(x)nrow(subset(superset,trip_mode==x[2]&dem_index==x[1])))
    #travel_summary$trips_pp <- travel_summary$n_trips/travel_summary$travellers
    #travel_summary$duration <- apply(travel_summary,1,function(x)sum(subset(superset,trip_mode==x[2]&dem_index==x[1])$trip_duration))
    #travel_summary$duration_per_trip <- travel_summary$duration/travel_summary$n_trips
    pointiness <- 200
    beta <- (1/smooth_probability - 1)*pointiness/(1 + (1/smooth_probability - 1))
    #beta <- (1/raw_probability - 1)*pointiness/(1 + (1/raw_probability - 1))
    alpha <- pointiness - beta
    #travel_summary$mu <- logit(travel_summary$smooth_probability)
    #travel_summary$sd <- 0.5#abs(travel_summary$mu)
    if(is.list(PROPENSITY_TO_TRAVEL)) {
      uncertain_rows <- travel_summary$mode%in%unlist(UNCERTAIN_TRAVEL_MODE_NAMES)
      propensities <- rep(0.5,nrow(travel_summary))
      propensities[uncertain_rows] <- sapply(travel_summary$mode[uncertain_rows],
                                             function(x)PROPENSITY_TO_TRAVEL[[which(sapply(UNCERTAIN_TRAVEL_MODE_NAMES,
                                                                     function(y)x%in%y))]])
      travel_summary$probability <- qbeta(propensities,alpha,beta)
      if(any(!uncertain_rows)) travel_summary$probability[!uncertain_rows] <- raw_probability[!uncertain_rows]
      #travel_summary$probability <- sigmoid(sapply(1:nrow(travel_summary),function(x)dnorm(PROPENSITY_TO_TRAVEL[[travel_summary$mode[x]]],travel_summary$mu[x],travel_summary$sd[x])))
    }else{
      travel_summary$probability <- raw_probability#qbeta(PROPENSITY_TO_TRAVEL,travel_summary$alpha,travel_summary$beta)
      # 
    }
    
    dem_indices <- unique(travel_summary$dem_index)
    dist_densities <- dur_densities <- list()
    pp_summary_scen <- PP_TRAVEL_PROPENSITIES[,colnames(PP_TRAVEL_PROPENSITIES)%in%c('participant_id','dem_index')]
    
    ##### mode density
    individual_data <- setDT(superset)[,.(dist = sum(stage_distance) , dur = sum(stage_duration) ),by=c('stage_mode','participant_id')]
    individual_data <- left_join(individual_data,pp_summary_scen,by='participant_id')
    for(m in modes){
      subtab <- subset(individual_data,stage_mode==m)
      dist_densities[[m]] <- dur_densities[[m]] <- list()
      for(d in dem_indices){
        subtab2 <- subset(subtab,dem_index==d)
        dist_densities[[m]][[d]] <- subtab2$dist
        dur_densities[[m]][[d]] <- subtab2$dur
      }
    }
    
    
    densities <- list(dist_densities,dur_densities)
    ### start
    
    ##!! not extrapolating travel_data <- extrapolate_travel_data(travel_summary,modes,densities,repetitiveness=repetitiveness)
    #travel_data <- sample_travel_data(travel_summary,modes,densities)
    
    dist_densities <- densities[[1]]
    dur_densities <- densities[[2]]
    pop_densities <- lapply(densities,unlist)
    pop_dist_densities <- pop_densities[[1]]
    pop_dur_densities <- pop_densities[[2]]
    
    dem_indices <- unique(travel_summary$dem_index)
    # initialise durations to 0
    travel_data <- PP_TRAVEL_PROPENSITIES
    for(m in modes) travel_data[[paste0(m,'_dist')]] <- 0
    for(m in modes) travel_data[[paste0(m,'_dur')]] <- 0
    # get indices for mode random variables
    m_inds <- sapply(modes,function(m) which(names(travel_data)==paste0(m,'_p_rn')))
    for(d in dem_indices){
      sub2 <- subset(travel_summary,dem_index==d)
      travellers <- which(travel_data$dem_index==d)
      for(i in 1:length(modes)){
        m <- modes[i]
        probability <- sub2$probability[sub2$mode==m]
        if(length(dist_densities[[m]][[d]])<2|length(dur_densities[[m]][[d]])<2){
          dist_density <- pop_dist_densities
          dur_density <- pop_dur_densities
        }else{
          dist_density <- dist_densities[[m]][[d]]
          dur_density <- dur_densities[[m]][[d]]
        }
        m_ind <- m_inds[i]
        propensities <- travel_data[travellers,m_ind]
        
        travelled <- 0
        travel_given_probability <- propensities<probability
        if(sum(travel_given_probability)>0){
          non_zero_travellers <- travellers[travel_given_probability]
          traveller_propensities <- propensities[travel_given_probability]/probability
          dists <- sort(dist_density,decreasing = T)[ceiling(traveller_propensities*length(dist_density))]
          durs <- sort(dur_density,decreasing = T)[ceiling(traveller_propensities*length(dur_density))]
          
          travel_data[[paste0(m,'_dist')]][non_zero_travellers] <- dists
          travel_data[[paste0(m,'_dur')]][non_zero_travellers] <- durs
        }
      }
    }
    
    ### stop
    
    for(m in modes_to_keep){
      pp_summary_scen[[paste0(m,'_dist')]] <- travel_data[[paste0(m,'_dist')]]
      pp_summary_scen[[paste0(m,'_dur')]] <- travel_data[[paste0(m,'_dur')]]
    }
    # 
    pp_summary[[SCEN_SHORT_NAME[scen]]] <- pp_summary_scen
  }
  return(pp_summary)
}

#' @export
sample_travel_data <- function(travel_summary,modes,densities){
  dist_densities <- densities[[1]]
  dur_densities <- densities[[2]]
  pop_densities <- lapply(densities,unlist)
  pop_dist_densities <- pop_densities[[1]]
  pop_dur_densities <- pop_densities[[2]]
  
  dem_indices <- unique(travel_summary$dem_index)
  # initialise durations to 0
  travel_data <- PP_TRAVEL_PROPENSITIES
  for(m in modes) travel_data[[paste0(m,'_dist')]] <- 0
  for(m in modes) travel_data[[paste0(m,'_dur')]] <- 0
  # get indices for mode random variables
  m_inds <- sapply(modes,function(m) which(names(travel_data)==paste0(m,'_p_rn')))
  for(d in dem_indices){
    sub2 <- subset(travel_summary,dem_index==d)
    travellers <- which(travel_data$dem_index==d)
    for(i in 1:length(modes)){
      m <- modes[i]
      probability <- sub2$probability[sub2$mode==m]
      if(length(dist_densities[[m]][[d]])<2|length(dur_densities[[m]][[d]])<2){
        dist_density <- pop_dist_densities
        dur_density <- pop_dur_densities
      }else{
        dist_density <- dist_densities[[m]][[d]]
        dur_density <- dur_densities[[m]][[d]]
      }
      m_ind <- m_inds[i]
      propensities <- travel_data[travellers,m_ind]
      
      travelled <- 0
      travel_given_probability <- propensities<probability
      if(sum(travel_given_probability)>0){
        non_zero_travellers <- travellers[travel_given_probability]
        traveller_propensities <- propensities[travel_given_probability]/probability
        dists <- sort(dist_density,decreasing = T)[ceiling(traveller_propensities*length(dist_density))]
        durs <- sort(dur_density,decreasing = T)[ceiling(traveller_propensities*length(dur_density))]
        
        travel_data[[paste0(m,'_dist')]][non_zero_travellers] <- dists
        travel_data[[paste0(m,'_dur')]][non_zero_travellers] <- durs
      }
    }
  }
  travel_data
}


#' @export
extrapolate_travel_data <- function(travel_summary,modes,densities,repetitiveness=1,days=7){
  self_days <- days*repetitiveness
  same_days <- 1-repetitiveness
  dem_indices <- unique(travel_summary$dem_index)
  # initialise durations to 0
  trip_data <- PP_TRAVEL_PROPENSITIES
  for(m in modes) trip_data[[paste0(m,'_dur')]] <- 0
  # get indices for mode random variables
  m_inds <- sapply(modes,function(m) which(names(trip_data)==paste0(m,'_p_rn')))
  for(d in dem_indices){
    sub2 <- subset(travel_summary,dem_index==d)
    travellers <- which(trip_data$dem_index==d)
    for(i in 1:length(modes)){
      m <- modes[i]
      probability <- sub2$probability[sub2$mode==m]
      if(length(densities[[d]][[m]])==0||probability < 1e-7) {
        trip_data[[paste0(m,'_dur')]][travellers] <- 0
      }else{
        dens <- rep(densities[[d]][[m]],times=2)
        zeros <- max(2,round(length(dens)*(1-probability)/probability))
        sample_density <- c(dens,rep(0,times=zeros))
        m_ind <- m_inds[i]
        self <- sort(sample_density)[ceiling(trip_data[travellers,m_ind]*length(sample_density))]*self_days
        week_density <- rowSums(matrix(sample_density[ceiling(EXTRAP_RN*length(sample_density))],ncol=days,byrow=F))
        same <- sort(week_density)[ceiling(trip_data[travellers,m_ind]*length(week_density))]*same_days
        trip_data[[paste0(m,'_dur')]][travellers] <- self + same
      }
    }
  }
  trip_data
}



