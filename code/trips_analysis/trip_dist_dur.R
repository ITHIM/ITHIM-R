# Load library
require(tidyverse)
library(dplyr, warn.conflicts = FALSE)

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)


# Clear workspace
rm(list=ls())

# Cities names
cities <- c('accra', 'bangalore', 'belo_horizonte', 'bogota', 'buenos_aires', 'cape_town',
            'delhi', 'mexico_city', 'santiago', 'sao_paulo', 'vizag')
i <- 1

# List to hold trip datasets
l <- list()

# List to save all stage and trip speed data
speed_df <- list()
for (city in cities){
  trip_set <- read.csv(paste0('data/local/',city,'/',city,'_trip.csv'), stringsAsFactors = F)
  l[[city]] <- trip_set
  
  # Check if distance column exists
  dist <- trip_set %>% dplyr::select(ends_with('distance')) %>% length()
  # Check if duration column exists
  dur <- trip_set %>% dplyr::select(ends_with('duration')) %>% length()
  
  if(dist && dur){
    
    # Local df for saving speed data
    lsdf <- NULL
    
    print(paste(i, city , 'has both distance and duration'))
    
    # Subset trip data with both dist and duration
    # calculate speed
    # group by mode
    # calculate mean and median
    
    # If stage data is available
    if (any(names(trip_set) %in% 'stage_duration') && (any(names(trip_set) %in% 'stage_distance'))){
      lsdf <- trip_set %>% dplyr::filter(stage_distance > 0 & stage_duration > 0 & !is.na(stage_mode)) %>%  
        mutate(speed = stage_distance / ( stage_duration / 60)) %>% group_by(stage_mode) %>% 
        summarise(median_speed = median(speed, na.rm = T), mean_speed = mean(speed, na.rm = T))
      lsdf$city <- city
      
      # If not, then use trip data
    }else if (any(names(trip_set) %in% 'trip_duration') && (any(names(trip_set) %in% 'trip_distance'))){
      lsdf <- trip_set %>% dplyr::filter(trip_distance > 0 & trip_duration > 0 & !is.na(trip_mode)) %>%  mutate(speed = trip_distance / ( trip_duration / 60)) %>% group_by(trip_mode) %>% 
        summarise(median_speed = median(speed, na.rm = T), mean_speed = mean(speed, na.rm = T))
      lsdf$city <- city
      
      lsdf <- rename(lsdf, stage_mode = trip_mode)
    }
    
    if (!is.null(lsdf)){
      
      # Bind speed data
      if (length(speed_df) > 0) {
        speed_df <- plyr::rbind.fill(speed_df, lsdf)
        
      }else{
        
        # Initial speed data
        speed_df <- lsdf
        
      }
    }
    
    
  }else if(dist && !dur){
    
    print(paste(i, city , 'has ONLY distance'))
    
  }else if(!dist && dur){
    
    print(paste(i, city , 'has ONLY duration'))
    
  }
  
  i <- i + 1
  
}

write_csv(speed_df, 'data/analysis/mode_speed/speed_df.csv')
