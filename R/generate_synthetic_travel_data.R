#' @export
generate_synthetic_travel_data <- function(trip_scen_sets){
  demographic <- DEMOGRAPHIC
  trip_superset <- assign_age_groups(TRIP_SET)
  trip_superset <- left_join(trip_superset,demographic,by=c('sex','age_cat'))
  raw_trip_demographics <- trip_superset[,colnames(trip_superset)%in%c('participant_id','dem_index')]
  
  modes_to_keep <- unique(trip_scen_sets[[1]]$stage_mode)
  modes <- modes_to_keep#unlist(UNCERTAIN_TRAVEL_MODE_NAMES)
  
  missing_trips <- subset(trip_superset,!stage_mode%in%modes&!duplicated(participant_id))
  rm(trip_superset)
  
  no_travel_people <- setDT(missing_trips)[,.(no_travel=.N),by='dem_index']
  rm(missing_trips)
  
  participant_demographics <- PP_TRAVEL_PROPENSITIES[,colnames(PP_TRAVEL_PROPENSITIES)%in%c('participant_id','dem_index')]
  pp_summary <- list()#PP_TRAVEL_PROPENSITIES[,colnames(PP_TRAVEL_PROPENSITIES)%in%c('participant_id','dem_index')]
  pp_summary_scen <- participant_demographics
  
  travel_data <- PP_TRAVEL_PROPENSITIES
  # get indices for mode random variables
  m_inds <- sapply(modes,function(modei) which(names(travel_data)==paste0(modei,'_p_rn')))
  travel_summary_template <- expand.grid(stage_mode=sort(modes),dem_index=sort(unique(demographic$dem_index)))
  dem_indices <- unique(travel_summary_template$dem_index)
  
  for(scen in 1:length(trip_scen_sets)) trip_scen_sets[[scen]] <- setDT(left_join(trip_scen_sets[[scen]],demographic,by=c('sex','age_cat')))
  rm(demographic)
  
  for(scen in 1:length(trip_scen_sets)){
    
    ## get trips and clear memory
    superset <- trip_scen_sets[[scen]]
    trip_scen_sets[[scen]] <- 0
    
    ## get mode--demography-specific raw densities
    individual_data <- superset[,.(dur = sum(stage_duration) ),by=c('stage_mode','participant_id')]
    individual_data <- left_join(individual_data,raw_trip_demographics,by='participant_id')
    
    dur_densities <- list()
    for(modei in modes){
      subtab <- individual_data[individual_data$stage_mode==modei,]
      dur_densities[[modei]] <- list()
      for(di in dem_indices){
        dur_densities[[modei]][[di]] <- subtab$dur[subtab$dem_index==di]
      }
    }
    rm(individual_data,subtab)
    
    ## get summary travel information (by mode and demography) and clear memory
    pp_travel_by_mode <- unique( superset[,.(num_trips=.N,dem_index=dem_index),by=c('stage_mode','participant_id')], by=c('stage_mode','participant_id'))
    travel_people <- unique(superset,by='participant_id')[,.(some_travel=.N),by='dem_index']
    rm(superset)
    travel_by_mode_and_demo <- pp_travel_by_mode[,.(travellers=.N),by=c('stage_mode','dem_index')]
    travel_by_mode_and_demo <- setorder(travel_by_mode_and_demo,dem_index,stage_mode)
    travel_summary <- travel_by_mode_and_demo[setDT(travel_summary_template),on=c('stage_mode','dem_index')]
    travel_summary <- travel_summary[travel_people,on=c('dem_index')]
    travel_summary <- travel_summary[no_travel_people,on=c('dem_index')]
    travel_summary <- travel_summary[,population:=some_travel+no_travel]
    travel_summary$travellers[is.na(travel_summary$travellers)] <- 0
    setnames(travel_summary,'stage_mode','mode')
    rm(pp_travel_by_mode,travel_by_mode_and_demo,travel_people)
    
    ## estimate smooth probabilities
    raw_probability <- travel_summary$travellers/travel_summary$population
    smooth_probability <- #raw_probability
    #smooth_probability[smooth_probability==0] <- 0.001
      suppressWarnings(glm(raw_probability~I(dem_index<(max(dem_index)/2))+I(dem_index%%(max(dem_index)/2))+mode,
          family=binomial,offset=log(population),data=travel_summary)$fitted.values)
    pointiness <- 300
    beta_val <- (1/smooth_probability - 1)*pointiness/(1 + (1/smooth_probability - 1))
    #beta_val <- (1/raw_probability - 1)*pointiness/(1 + (1/raw_probability - 1))
    alpha_val <- pointiness - beta_val
    if(is.list(PROPENSITY_TO_TRAVEL)) {
      uncertain_rows <- travel_summary$mode%in%unlist(UNCERTAIN_TRAVEL_MODE_NAMES)
      propensities <- rep(0.5,nrow(travel_summary))
      propensities[uncertain_rows] <- sapply(travel_summary$mode[uncertain_rows],
                                             function(x)PROPENSITY_TO_TRAVEL[[which(sapply(UNCERTAIN_TRAVEL_MODE_NAMES,
                                                                     function(y)x%in%y))]])
      travel_summary$probability <- qbeta(propensities,alpha_val,beta_val)
      if(any(!uncertain_rows)) travel_summary$probability[!uncertain_rows] <- raw_probability[!uncertain_rows]
      #travel_summary$probability <- sigmoid(sapply(1:nrow(travel_summary),function(x)dnorm(PROPENSITY_TO_TRAVEL[[travel_summary$mode[x]]],travel_summary$mu[x],travel_summary$sd[x])))
    }else{
      travel_summary$probability <- raw_probability#qbeta(PROPENSITY_TO_TRAVEL,travel_summary$alpha_val,travel_summary$beta_val)
      # 
    }
    
    ### start
    
    ##!! not extrapolating travel_data <- extrapolate_travel_data(travel_summary,modes,densities,repetitiveness=repetitiveness)
    #travel_data <- sample_travel_data(travel_summary,modes,densities)
    
    ## use population travel data if there are no demographic-mode travel samples
    pop_mode_dur_densities <- lapply(dur_densities,unlist)
    ## use population travel data if there are no mode travel samples
    pop_dur_densities <- unlist(pop_mode_dur_densities)
    
    # initialise durations to 0
    for(modei in modes) travel_data[[paste0(modei,'_dur')]] <- 0
    for(di in dem_indices){
      sub2 <- travel_summary[travel_summary$dem_index==di,]
      travellers <- which(travel_data$dem_index==di)
      for(i in 1:length(modes)){
        modei <- modes[i]
        probability <- sub2$probability[sub2$mode==modei]
        ## use population travel data if there are no demographic-mode travel samples
        if(length(dur_densities[[modei]][[di]])<2){#length(dist_densities[[modei]][[di]])<2|
          ## use population travel data if there are no mode travel samples
          if(length(pop_mode_dur_densities[[modei]])<2){#length(pop_mode_dist_densities[[modei]])<2|
            dur_density <- pop_dur_densities
          }else{
            dur_density <- pop_mode_dur_densities[[modei]]
          }
        }else{
          dur_density <- dur_densities[[modei]][[di]]
        }
        m_ind <- m_inds[i]
        propensities <- travel_data[travellers,m_ind]

        travelled <- 0
        travel_given_probability <- propensities<probability
        if(sum(travel_given_probability)>0){
          non_zero_travellers <- travellers[travel_given_probability]
          traveller_propensities <- propensities[travel_given_probability]/probability
          durs <- sort(dur_density,decreasing = T)[ceiling(traveller_propensities*length(dur_density))]

          travel_data[[paste0(modei,'_dur')]][non_zero_travellers] <- durs
        }
      }
    }
    rm(travel_summary)

    ### stop

    for(modei in modes){
      pp_summary_scen[[paste0(modei,'_dur')]] <- travel_data[[paste0(modei,'_dur')]]
    }

    pp_summary[[scen]] <- pp_summary_scen#[[SCEN_SHORT_NAME[scen]]] <- pp_summary_scen
  }
  rm(participant_demographics,pp_summary_scen,travel_data)
  names(pp_summary) <- SCEN_SHORT_NAME
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



