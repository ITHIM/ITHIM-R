whw_file <- 'injury_matrix_vizag_3_years.csv'
path <- 'data/local/vizag/'
whw <- read.csv(paste0(path,whw_file))
injuries <- data.frame(cas_mode=character(),strike_mode=character(),stringsAsFactors = F)
for(i in 1:nrow(whw))
  for(j in 2:ncol(whw)){
    count <- whw[i,j]
    if(count>0)
    for(k in 1:count){
      injuries[nrow(injuries)+1,] <- c(as.character(whw[i,1]),colnames(whw)[j])
    }
  }
injuries$strike_mode[injuries$strike_mode=='X2.3.Wheeled'] <- 'motorcycle'
injuries$strike_mode[injuries$strike_mode=='Car.pick.up.van'] <- 'car'
injuries$strike_mode[injuries$strike_mode=='Trucks'] <- 'truck'
injuries$strike_mode[injuries$strike_mode=='Bus'] <- 'bus'


injuries$cas_mode[injuries$cas_mode=='Pedal Cycle'] <- 'bicycle'
injuries$cas_mode[injuries$cas_mode=='3Wheeled'] <- 'auto_rickshaw'
injuries$cas_mode[injuries$cas_mode=='Heavy transport'] <- 'truck'

injuries$cas_mode <- tolower(injuries$cas_mode)
injuries$strike_mode <- tolower(injuries$strike_mode)

injuries
head(injuries)
unique(injuries$cas_mode)
unique(injuries$strike_mode)
injury_file <- 'injuries_vizag.csv'
write.csv(injuries,paste0('inst/extdata/local/vizag/',injury_file))



