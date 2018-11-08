dist_dur_tbls <- function(bs){
  
  bs <- filter(bs, !trip_mode %in% c("99", "Train", "Other", "Unspecified")&!is.na(trip_mode))
  
  ##RJ only calculate if plotFlag==T
  if(plotFlag){
    # Remove short walking, 99, Train, Other and Unspecified modes
    dataset <- filter(bs, ! trip_mode %in% c('Short Walking', "99", "Train", "Other", "Unspecified"))
    # Unique number of ind
    total_ind <- length(unique(bs$participant_id))
    
    l <- list()
    for (i in 1:length(unique(dataset$scenario))){
      bd <- filter(dataset, scenario == unique(dataset$scenario)[i])
      bdnr <- nrow(bd)
      bd <- bd %>% group_by(trip_mode) %>%  summarise(pert = dplyr::n())
      bd <- bd %>%  dplyr::select(trip_mode, pert) %>% 
        setNames(c("trip_mode",unique(dataset$scenario)[i])) 
      l[[i]] <- bd
      
    }
    
    bd <- l[[1]]
    if(length(l)>1)
      for (i in 2:length(l))
        bd <- left_join(bd, l[[i]], by = "trip_mode")
    
    l <- list()
    for (i in 1:length(unique(dataset$scenario))){
      bd <- filter(dataset, scenario == unique(dataset$scenario)[i])
      bdnr <- nrow(bd)
      bd <- bd %>% group_by(trip_mode) %>%  summarise(pert = round(dplyr::n()/bdnr * 100, 1))
      bd <- bd %>%  dplyr::select(trip_mode, pert) %>% 
        setNames(c("trip_mode",unique(dataset$scenario)[i])) 
      l[[i]] <- bd
    }
    
    bd <- l[[1]]
    if(length(l)>1)
      for (i in 2:length(l))
        bd <- left_join(bd, l[[i]], by = "trip_mode")
    
    bd <- reshape2::melt(bd)
    
    plotly::ggplotly(ggplot(data = bd, aes(x = trip_mode, y = value, fill = variable)) + 
                       geom_bar(stat = 'identity', position = "dodge", color = "black") + 
                       theme_minimal() + xlab('Mode') + ylab('Percentage (%)') + labs(title = "Mode distribution per week"))
    # Calculate trip distance for baseline and three scenarios
    
  }
  
  ## calculate all distances & durations
  l_dist <-  l_dur <- list()
  for (i in 1:length(SCEN)){
    local <- group_by(filter(bs,scenario == SCEN[i]), trip_mode)
    
    local_dist <- summarise(local, sum_dist = sum(trip_distance))
    local_dist$sum_dist[local_dist$trip_mode == "Walking"] <- 
      local_dist$sum_dist[local_dist$trip_mode == "Walking"] + 
      local_dist$sum_dist[local_dist$trip_mode == "Short Walking"]
    colnames(local_dist)[2] <- SCEN[i]
    l_dist[[i]] <- local_dist
    
    local_dur <- summarise(local, sum_dur = sum(trip_duration))
    local_dur$sum_dur[local_dur$trip_mode == "Walking"] <- 
      local_dur$sum_dur[local_dur$trip_mode == "Walking"] + 
      local_dur$sum_dur[local_dur$trip_mode == "Short Walking"]
    colnames(local_dur)[2] <- SCEN[i]
    l_dur[[i]] <- local_dur
  }
  
  ## join distances & durations
  for (i in 1:length(l_dist)){
    if (i == 1){
      local_dist <- l_dist[[i]]
      local_dur <- l_dur[[i]]
    }else{
      local_dist <- left_join(local_dist, l_dist[[i]], by = "trip_mode")
      local_dur <- left_join(local_dur, l_dur[[i]], by = "trip_mode")
    }
  }
  
  # Remove short walking
  dist <- filter(local_dist, trip_mode != 'Short Walking')
  dur <- filter(local_dur, trip_mode != 'Short Walking')
  
  if(plotFlag){
    
    dist_melted <- reshape2::melt(dist, by = trip_mode)
    # Plot
    plotly::ggplotly(ggplot(data = dist_melted, aes(x = trip_mode, y = value / total_ind, fill = variable)) + 
                       geom_bar(stat = 'identity', position = 'dodge', color = "black") + 
                       theme_minimal() + xlab('Mode') + ylab('Distance (km)') + labs(title = "Mode distance  per person per week (km)")
    )
    
    dur_melted <- reshape2::melt(dur, by = trip_mode)
    
    dur_melted$value <- round(dur_melted$value / (60 * total_ind), 2)
    
    # Plot
    plotly::ggplotly(ggplot(data = dur_melted, aes(x = trip_mode, y = value, fill = variable)) + 
                       geom_bar(stat = 'identity', position = 'dodge', color = 'black') + 
                       theme_minimal() + xlab('Mode') + ylab('Duration (hours)') + labs(title = 
                                                                                          "Mode Duration per person per week (hours)")
    )
  }
  
  #list(dist,dur)
  list(dist=dist,dur=dur)
}
