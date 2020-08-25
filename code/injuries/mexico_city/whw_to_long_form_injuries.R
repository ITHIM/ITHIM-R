library(ithimr)
file_path <- file.path('data/local/mexico_city/mexico city injury data updated using bogota.csv')
whw <- read_csv(file_path)
injuries <- data.frame(cas_mode=character(),strike_mode=character(),weight=numeric(),stringsAsFactors = F)
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


unique(injuries$cas_mode)
unique(injuries$strike_mode)
head(injuries)
unique(injuries$cas_mode)
unique(injuries$strike_mode)

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



