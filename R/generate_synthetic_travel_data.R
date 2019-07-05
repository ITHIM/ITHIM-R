#' @export
generate_synthetic_travel_data <- function(trip_scen_sets){
  trip_superset <- setDT(TRIP_SET)
  setDT(DEMOGRAPHIC)
  keycols = c("sex","age_cat")
  setkeyv(DEMOGRAPHIC,keycols)
  
  trip_superset[DEMOGRAPHIC,on=c('sex','age_cat'),dem_index := i.dem_index]
  raw_trip_demographics <- unique(trip_superset[,.(participant_id=participant_id,dem_index=dem_index)],by=c('participant_id'))
  setkey(raw_trip_demographics,participant_id)
  
  modes_to_keep <- unique(trip_scen_sets[[1]]$stage_mode)
  modes <- modes_to_keep#unlist(UNCERTAIN_TRAVEL_MODE_NAMES)
  
  participant_demographics <- PP_TRAVEL_PROPENSITIES[,colnames(PP_TRAVEL_PROPENSITIES)%in%c('participant_id','dem_index')]
  pp_summary <- list()#PP_TRAVEL_PROPENSITIES[,colnames(PP_TRAVEL_PROPENSITIES)%in%c('participant_id','dem_index')]
  pp_summary_scen <- participant_demographics
  
  travel_data <- PP_TRAVEL_PROPENSITIES
  # get indices for mode random variables
  m_inds <- sapply(modes,function(modei) which(names(travel_data)==paste0(modei,'_p_rn')))
  dem_indices <- unique(DEMOGRAPHIC$dem_index)
  
  for(scen in 1:length(trip_scen_sets)) setDT(trip_scen_sets[[scen]])[DEMOGRAPHIC,on=.(sex,age_cat),dem_index := i.dem_index]
  # for(scen in 1:length(trip_scen_sets)) {
  #   setDT(trip_scen_sets[[scen]])
  #   setkeyv(trip_scen_sets[[scen]],keycols)
  #   trip_scen_sets[[scen]][DEMOGRAPHIC,dem_index := i.dem_index]
  # }
  
  for(scen in 1:length(trip_scen_sets)){
    
    ## get trips and clear memory
    superset <- trip_scen_sets[[scen]]
    trip_scen_sets[[scen]] <- 0
    ## get summary travel information (by mode and demography) and clear memory
    travel_summary <- generate_travel_summary(superset,raw_trip_demographics)
    travel_summary <- scale_mode_probabilities(travel_summary)
    travel_summary <- resample_mode_probabilities(travel_summary)
    
    ## get mode--demography-specific raw duration densities
    superset[,'weighted_duration' := stage_duration*trip_weight]
    individual_data <- superset[,.(dur = sum(weighted_duration) ),by=.(stage_mode,participant_id)]
    superset <- NULL
    setkey(individual_data,participant_id)
    individual_data[raw_trip_demographics,dem_index := i.dem_index,on='participant_id']
    
    dur_densities <- list()#individual_data[,.(dur=list(c(dur))),by=.(stage_mode,dem_index)]
    pop_dur_densities <- individual_data$dur
    ind_modes <- individual_data$stage_mode
    for(modei in modes){
    #subtab <- individual_data[ind_modes==modei,]
     data_indices <- ind_modes==modei
     dur_densities[[modei]] <- list()
     durs <- individual_data$dur[data_indices]
     dems <- individual_data$dem_index[data_indices]
     for(di in dem_indices){
       dur_densities[[modei]][[di]] <- durs[dems==di]
     }
    }
    individual_data <- subtab <- NULL
    
    ### start
    
    ##!! not extrapolating travel_data <- extrapolate_travel_data(travel_summary,modes,densities,repetitiveness=repetitiveness)
    #travel_data <- sample_travel_data(travel_summary,modes,densities)
    
    ## use population travel data if there are no demographic-mode travel samples
    pop_mode_dur_densities <- lapply(dur_densities,unlist)
    #pop_mode_dur_densities <- individual_data[,.(dur=list(c(dur))),by=stage_mode]
    ## use population travel data if there are no mode travel samples
    #pop_dur_densities <- unlist(pop_mode_dur_densities)
    
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
    travel_summary <- NULL

    ### stop

    for(modei in modes){
      pp_summary_scen[[paste0(modei,'_dur')]] <- travel_data[[paste0(modei,'_dur')]]
    }

    pp_summary[[scen]] <- pp_summary_scen#[[SCEN_SHORT_NAME[scen]]] <- pp_summary_scen
  }
  participant_demographics <- pp_summary_scen <- travel_data <- NULL
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



