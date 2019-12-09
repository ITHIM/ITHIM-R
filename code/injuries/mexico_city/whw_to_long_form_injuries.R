library(ithimr)
setwd('ITHIM-R')
whw_file <- 'nov.csv'
path <- file.path('code/injuries/mexico_city/')
whw <- read.csv(paste0(path,whw_file))
colnames(whw)[2] <- 'NOV'
injuries <- data.frame(cas_mode=character(),strike_mode=character(),weight=numeric(),stringsAsFactors = F)
number_of_years <- 1
for(i in 1:nrow(whw))
  for(j in 2:ncol(whw)){
    count <- whw[i,j]
    if(count>0){
      weight <- number_of_years*ceiling(count)/count
      for(k in 1:ceiling(count)){ 
        #print(c(k,count))
        injuries[nrow(injuries)+1,] <- c(as.character(whw[i,1]),colnames(whw)[j],weight)
      }
    }
  }
## rahul recommends omitting pick up truck and 3wheeled strikers – both almost zero in the matrix.
unique(injuries$cas_mode)
unique(injuries$strike_mode)
injuries$cas_mode[injuries$cas_mode=='cycle'] <- 'bicycle'
injuries
head(injuries)
unique(injuries$cas_mode)
unique(injuries$strike_mode)
injury_file <- 'injuries_mexico_city.csv'
write.csv(injuries,paste0('inst/extdata/local/mexico_city/',injury_file))



