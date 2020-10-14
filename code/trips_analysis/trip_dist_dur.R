require(tidyverse)
rm(list=ls())
# io <- readRDS("results/multi_city/io.rds")
# Assumes that multi_city_script.R has been run till 
# Get names of cities from the io object
cities <- c('accra', 'bangalore', 'belo_horizonte', 'bogota', 'buenos_aires', 'cape_town',
            'delhi', 'mexico_city', 'santiago', 'sao_paulo', 'vizag')
i <- 1

l <- list()

speed_df <- list()
for (city in cities){
  trip_set <- read.csv(paste0('inst/extdata/local/',city,'/trips_', city, '.csv'), stringsAsFactors = F)
  l[[city]] <- trip_set
  
  dist <- trip_set %>% dplyr::select(ends_with('distance')) %>% length()
  dur <- trip_set %>% dplyr::select(ends_with('duration')) %>% length()
  
    if(dist && dur){
      
      print(paste(i, city , 'has both distance and duration'))
      
      lsdf <- trip_set %>% mutate(speed = stage_distance / ( stage_duration / 60)) %>% group_by(stage_mode) %>% 
        summarise(median_speed = median(speed, na.rm = T), mean_speed = mean(speed, na.rm = T))
      lsdf$city <- city
      
      
      if (length(speed_df) > 0) {
        speed_df <- plyr::rbind.fill(speed_df, lsdf)
            
      }else{
        
        speed_df <- lsdf
        
      }
      
      
    }else if(dist && !dur){
      
      print(paste(i, city , 'has ONLY distance'))
      
    }else if(!dist && dur){
     
      print(paste(i, city , 'has ONLY duration'))
      
    }
  
  i <- i + 1
  
}