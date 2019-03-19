path <- file.path(find.package('ithimr',lib.loc=.libPaths()), 'extdata/local/delhi/delhi_road_deaths_2010_2013_average.csv')
whw <- read.csv(path)
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
injuries
head(injuries)
write.csv(injuries,path)


