library(tidyverse)
file_path <- file.path('data/local/cape_town/injuries_cape_town.csv')
whw <- read_csv(file_path)
whw$X1 <- whw$year <- NULL

whw <- whw %>% rename(cas_gender = cas_sex)





injuries <- data.frame(cas_mode=character(),strike_mode=character(),weight=numeric(),stringsAsFactors = F)
number_of_years <- max(whw$year) - min(whw$year)
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
#injuries$cas_mode[injuries$cas_mode=='cycle'] <- 'bicycle'
injuries
head(injuries)
unique(injuries$cas_mode)
unique(injuries$strike_mode)
injury_file <- 'injuries_buenos_aires.csv'
write_csv(injuries, paste0('inst/extdata/local/buenos_aires/',injury_file))