library(tidyverse)
path <- file.path('data/local/bogota/bogota_injuries.csv')
whw <- read.csv(path)
injuries <- data.frame(cas_mode=character(),strike_mode=character(), stringsAsFactors = F)
number_of_years <- 1
for(i in 1:nrow(whw))
  for(j in 2:ncol(whw)){
    count <- whw[i,j]
    if(count>0){
      weight <- number_of_years*ceiling(count)/count
      if(weight==0) print(c(i,j))
      for(k in 1:ceiling(count)){ ## three years of data
        #print(c(k,count))
        injuries[nrow(injuries)+1,] <- c(as.character(whw[i,1]),colnames(whw)[j],weight)
      }
    }
  }
## rahul recommends omitting pick up truck and 3wheeled strikers – both almost zero in the matrix.
unique(injuries$cas_mode)
unique(injuries$strike_mode)
injuries <- injuries[!injuries$cas_mode%in%c('3Wheeled','Pick-up truck/van'),]
injuries$strike_mode[injuries$strike_mode=='X2.3.Wheeled'] <- 'motorcycle'
injuries$strike_mode[injuries$strike_mode=='Car.pick.up.van'] <- 'car'
injuries$strike_mode[injuries$strike_mode=='Trucks'] <- 'truck'
injuries$strike_mode[injuries$strike_mode=='Non.collision'] <- 'NOV'
injuries$strike_mode[injuries$strike_mode=='Pedal.cycle'] <- 'bicycle'
injuries$cas_mode[injuries$cas_mode=='Pedal Cycle'] <- 'bicycle'
#injuries$cas_mode[injuries$cas_mode=='3Wheeled'] <- 'auto_rickshaw'
#injuries$cas_mode[injuries$cas_mode=='Pick-up truck/van'] <- 'minibus'
injuries$cas_mode[injuries$cas_mode=='Heavy transport'] <- 'truck'
injuries
head(injuries)
unique(injuries$cas_mode)
unique(injuries$strike_mode)
injury_file <- 'injuries_bogota.csv'
write.csv(injuries,paste0('inst/extdata/local/bogota/',injury_file))



