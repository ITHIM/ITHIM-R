library(tidyverse)
file_path <- file.path('data/local/buenos_aires/buenos_aires_injuries_NOV updated using bogota.csv')
whw <- read_csv(file_path)
injuries <- data.frame(cas_mode=character(),strike_mode=character(),stringsAsFactors = F)
for(i in 1:nrow(whw))
  for(j in 2:ncol(whw)){
    count <- ceiling(whw[i,j]) %>% as.numeric()
    if(count>0)
      for(k in 1:count){
        injuries[nrow(injuries)+1,] <- c(as.character(whw[i,1]),colnames(whw)[j])
      }
  }


unique(injuries$cas_mode)
unique(injuries$strike_mode)
head(injuries)
unique(injuries$cas_mode)
unique(injuries$strike_mode)

injuries$strike_mode[injuries$strike_mode=='Pedestrian'] <- 'pedestrian'
injuries$strike_mode[injuries$strike_mode=='Pedal cycle'] <- 'cycle'
injuries$strike_mode[injuries$strike_mode=='2/3 Wheeled'] <- 'motorcycle'
injuries$strike_mode[injuries$strike_mode=='Car/pick-up/van'] <- 'car'
injuries$strike_mode[injuries$strike_mode=='Bus'] <- 'bus'
injuries$strike_mode[injuries$strike_mode=='unspecified'] <- 'unknown'
injuries$strike_mode[injuries$strike_mode=='Trucks'] <- 'truck'

injury_file <- 'injuries_buenos_aires.csv'
write_csv(injuries,paste0('inst/extdata/local/buenos_aires/',injury_file))
