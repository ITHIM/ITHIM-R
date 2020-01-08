whw_file <- 'santiago_fatality_matrix_4_years_2011_2014.csv'
path <- 'data/local/santiago/'
whw <- read.csv(paste0(path,whw_file))

injuries <- data.frame(cas_mode=character(),strike_mode=character(),weight=numeric(),stringsAsFactors = F)
number_of_years <- 4
for(i in 1:nrow(whw))
  for(j in 2:ncol(whw)){
    count <- whw[i,j]
    if(!is.na(count)&count>0){
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
injuries <- injuries[!injuries$cas_mode%in%c('3Wheeled','Pick-up truck/van'),]

injuries$cas_mode[injuries$cas_mode=='Cycle'] <- 'bicycle'
injuries$cas_mode[injuries$cas_mode=='NMV'] <- 'Other'
injuries$cas_mode[injuries$cas_mode=='MC'] <- 'motorcycle'

injuries$strike_mode[injuries$strike_mode=='None'] <- 'NOV'
injuries$strike_mode[injuries$strike_mode=='Cycle'] <- 'bicycle'
injuries$strike_mode[injuries$strike_mode=='MC'] <- 'motorcycle'
injuries
head(injuries)
unique(injuries$cas_mode)
unique(injuries$strike_mode)
injury_file <- 'injuries_santiago.csv'
write.csv(injuries,paste0('inst/extdata/local/santiago/',injury_file))



