#' @export
generate_synthetic_travel_data <- function(synthetic_trips){
  ## get raw trips and demographic information
  setDT(DEMOGRAPHIC)
  keycols = c("sex","age_cat")
  setkeyv(DEMOGRAPHIC,keycols)
  
  modes_to_keep <- unique(synthetic_trips[[1]]$stage_mode)
  modes <- modes_to_keep#unlist(UNCERTAIN_TRAVEL_MODE_NAMES)
  
  ## get demographic information for synthetic population
  participant_demographics <- PP_TRAVEL_PROPENSITIES[,colnames(PP_TRAVEL_PROPENSITIES)%in%c('participant_id','dem_index')]
  pp_summary <- list()#PP_TRAVEL_PROPENSITIES[,colnames(PP_TRAVEL_PROPENSITIES)%in%c('participant_id','dem_index')]
  pp_summary_scen <- participant_demographics
  
  ## initialise travel data for synthetic population
  travel_data <- PP_TRAVEL_PROPENSITIES
  # get indices for mode random variables
  m_inds <- sapply(modes,function(modei) which(names(travel_data)==paste0(modei,'_p_rn')))
  dem_indices <- unique(DEMOGRAPHIC$dem_index)
  
  ## set scenario trips to DT and add demographic information
  for(scen in 1:length(synthetic_trips)) setDT(synthetic_trips[[scen]])#[DEMOGRAPHIC,on=.(sex,age_cat),dem_index := i.dem_index]
  
  for(scen in 1:length(synthetic_trips)){
    
    ## get trips and clear memory
    superset <- synthetic_trips[[scen]]
    synthetic_trips[[scen]] <- 0
    
    ## travel_summary gives the probabilities to travel; densities (from individual data) give the distribution of travel, given travel occurs.
    
    ## get summary travel information (by mode and demography) for scenario
    travel_summary <- generate_travel_summary(superset)
    #travel_summary <- scale_mode_probabilities(travel_summary)
    travel_summary <- resample_mode_probabilities(travel_summary)
    
    ## get mode--demography-specific raw duration densities
    #superset[,weighted_duration := stage_duration*trip_weight]
    individual_data <- superset[,.(dur = sum(stage_duration) ),by=.(stage_mode,participant_id)]
    superset <- NULL
    #setkey(individual_data,participant_id)
    #individual_data[RAW_TRIP_DEMOGRAPHICS,dem_index := i.dem_index,on='participant_id']
    dem_index_order <- RAW_TRIP_DEMOGRAPHICS$dem_index[match(individual_data$participant_id,RAW_TRIP_DEMOGRAPHICS$participant_id)]
    
    dur_densities <- list()#individual_data[,.(dur=list(c(dur))),by=.(stage_mode,dem_index)]
    pop_dur_densities <- individual_data$dur
    ind_modes <- individual_data$stage_mode
    for(modei in modes){
    #subtab <- individual_data[ind_modes==modei,]
     data_indices <- ind_modes==modei
     dur_densities[[modei]] <- list()
     durs <- individual_data$dur[data_indices]
     dems <- dem_index_order[data_indices]
     for(di in dem_indices){
       dur_densities[[modei]][[di]] <- durs[dems==di]
     }
    }
    individual_data <- dem_index_order <- subtab <- NULL
    
    ### start: draw from zero & duration mixture distribution
    
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
      travel_data_travellers <- travel_data[travellers,]
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
        propensities <- travel_data_travellers[,m_ind]

        travelled <- 0
        travel_given_probability <- propensities<probability
        if(sum(travel_given_probability)>0){
          non_zero_travellers <- travellers[travel_given_probability]
          traveller_propensities <- propensities[travel_given_probability]/probability
          durs <- sort(dur_density,method='quick',decreasing=T)[ceiling(traveller_propensities*length(dur_density))]

          travel_data[[paste0(modei,'_dur')]][non_zero_travellers] <- durs
        }
      }
    }
    travel_summary <- dur_densities <- pop_mode_dur_densities <- pop_dur_densities  <- NULL

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
generate_no_travel_summary <- function(){
  trip_set <- TRIP_SET
  trip_superset <- assign_age_groups(trip_set)
  trip_superset <- left_join(trip_superset,DEMOGRAPHIC,by=c('sex','age_cat'))
  
  modes_to_keep <- intersect(MODE_SPEEDS$stage_mode,unique(trip_superset$trip_mode))
  #modes_to_keep <- modes_to_keep[!is.na(modes_to_keep)]
  modes <- modes_to_keep#unlist(UNCERTAIN_TRAVEL_MODE_NAMES)
  
  missing_trips <- trip_superset[!trip_superset$trip_mode%in%modes_to_keep&!duplicated(trip_superset$participant_id),]
  all_trips <- setDT(trip_superset[trip_superset$trip_mode%in%modes_to_keep,])
  trip_superset <- NULL
  
  NO_TRAVEL_PEOPLE <<- setDT(missing_trips)[,.(no_travel=.N),by='dem_index']
  missing_trips <- NULL
  
  TRAVEL_SUMMARY_TEMPLATE <<- expand.grid(trip_mode=sort(modes_to_keep),dem_index=sort(unique(DEMOGRAPHIC$dem_index)))
  
  #TRAVEL_SUMMARY <<- generate_travel_summary(all_trips)
}

#' @export
generate_travel_summary <- function(all_trips){
  dem_indices <- unique(TRAVEL_SUMMARY_TEMPLATE$dem_index)
  
  # summarise by demographic group those who have done some travel. The rest of the population is in no_travel_people
  travel_people <- all_trips[,.(some_travel=length(unique(participant_id))),by='dem_index']
  
  # number of travellers by demographic group (binomial)
  travel_by_mode_and_demo <- all_trips[,.(travellers=length(unique(participant_id))),by=.(trip_mode,dem_index)]#pp_travel_by_mode[,.(travellers=.N),by=.(trip_mode,dem_index)]
  mode_weights <- as.data.frame(all_trips[,.(weight=mean(trip_weight)),by=trip_mode])
  all_trips <- NULL
  
  setorder(travel_by_mode_and_demo,dem_index,trip_mode)
  # fill travel_by_mode_and_demo (adds in missing groupings) 
  travel_summary <- travel_by_mode_and_demo[setDT(TRAVEL_SUMMARY_TEMPLATE),on=.(trip_mode,dem_index)]
  # add total number of people who complete some travel
  travel_summary <- travel_summary[travel_people,on=c('dem_index')]
  # add total number of people who complete no travel
  travel_summary <- travel_summary[NO_TRAVEL_PEOPLE,on=c('dem_index')]
  # add total number of people
  travel_summary[,population:=some_travel+no_travel]
  # replace any NA with 0
  travel_summary[is.na(travellers),travellers:=0]
  setnames(travel_summary,'trip_mode','mode')
  travel_by_mode_and_demo <- travel_people <- NULL
  
  ## estimate smooth probabilities
  travel_summary[,raw_probability:=travellers/population]
  travel_summary$smooth_probability <- #raw_probability
    #smooth_probability[smooth_probability==0] <- 0.001
    suppressWarnings(glm(raw_probability~I(dem_index<(max(dem_index)/2))+I(dem_index%%(max(dem_index)/2))+mode,
                         family=binomial,data=travel_summary)$fitted.values)
#  return(travel_summary)
#}

# @export
#scale_mode_probabilities <- function(travel_summary){
  car_taxi_modes <- UNCERTAIN_TRAVEL_MODE_NAMES$car
  pt_modes <- UNCERTAIN_TRAVEL_MODE_NAMES$pt
  
  ## multiply mode probability scalars
  match_modes <- rep(1,nrow(travel_summary))
  modes <- travel_summary$mode
  
  mode_order <- sapply(UNCERTAIN_TRAVEL_MODE_NAMES,function(x)which(mode_weights$trip_mode%in%x[1]) )
  
  ##!! not the probabilities, but the sum of the weights over the number of rows. or the sum of the weights relative to the other modes??
  for(i in 1:length(UNCERTAIN_TRAVEL_MODE_NAMES))
    match_modes[modes%in%UNCERTAIN_TRAVEL_MODE_NAMES[[i]]] <- mode_weights[mode_order[i],2]
  #match_modes[modes%in%c('walking')] <- PROBABILITY_SCALAR_WALKING
  #match_modes[modes%in%pt_modes] <- PROBABILITY_SCALAR_PT
  #match_modes[modes%in%c('cycling')] <- PROBABILITY_SCALAR_CYCLING
  #match_modes[modes%in%c('motorcycle')] <- PROBABILITY_SCALAR_MOTORCYCLE
  travel_summary[,smooth_probability:=smooth_probability*..match_modes]
  travel_summary[,raw_probability:=raw_probability*..match_modes]
  return(travel_summary)  
}

#' @export
resample_mode_probabilities <- function(travel_summary){
  if(is.list(PROPENSITY_TO_TRAVEL)) {
    pointiness <- 300
    beta_val <- (1/travel_summary$smooth_probability - 1)*pointiness/(1 + (1/travel_summary$smooth_probability - 1))
    #beta_val <- (1/raw_probability - 1)*pointiness/(1 + (1/raw_probability - 1))
    alpha_val <- pointiness - beta_val
    uncertain_rows <- travel_summary$mode%in%unlist(UNCERTAIN_TRAVEL_MODE_NAMES)
    propensities <- rep(0.5,nrow(travel_summary))
    propensities[uncertain_rows] <- sapply(travel_summary$mode[uncertain_rows],
                                           function(x)PROPENSITY_TO_TRAVEL[[which(sapply(UNCERTAIN_TRAVEL_MODE_NAMES,
                                                                                         function(y)x%in%y))]])
    travel_summary$probability <- qbeta(propensities,alpha_val,beta_val)
    if(any(!uncertain_rows)) travel_summary$probability[!uncertain_rows] <- travel_summary$raw_probability[!uncertain_rows]
    #travel_summary$probability <- sigmoid(sapply(1:nrow(travel_summary),function(x)dnorm(PROPENSITY_TO_TRAVEL[[travel_summary$mode[x]]],travel_summary$mu[x],travel_summary$sd[x])))
  }else{
    travel_summary$probability <- travel_summary$raw_probability#qbeta(PROPENSITY_TO_TRAVEL,travel_summary$alpha_val,travel_summary$beta_val)
    # 
  }
  return(travel_summary)
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



