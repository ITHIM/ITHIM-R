library(ithimr)
setwd('ITHIM-R')
whw_file <- 'nov.csv'
path <- file.path('code/injuries/buenos_aires/')
whw <- read.csv(paste0(path,whw_file))
colnames(whw)[2] <- 'NOV'
injuries <- data.frame(cas_mode=character(),strike_mode=character(),weight=numeric(),stringsAsFactors = F)
number_of_years <- 1
for(i in 1:nrow(whw))
  for(j in 2:ncol(whw)){
    count <- whw[i,j]
    if(count>0){
      weight <- number_of_years*ceiling(count)/count
      for(k in 1:ceiling(count)){ ## six years of data
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
injury_file <- 'injuries_buenos_aires.csv'
write.csv(injuries,paste0('inst/extdata/local/buenos_aires/',injury_file))

#####################################################################
## with weights
library(xlsx)
whw_file <- 'injuries_whw_weights.xlsx'
path <- file.path('code/injuries/buenos_aires/')
whw <- read.xlsx(paste0(path,whw_file),sheetIndex = 2,startRow=2,colClasses=c('character','character',rep('numeric',10)))
whw_gender <- weights <- list()
whw_gender$female <- whw[-c(12:23),-c(1)]
whw_gender$male <- whw[-c(1:11,23),-c(1)]
for(gen in 1:2){
  whw_gender[[gen]]$known <- whw_gender[[gen]]$Grand.Total - whw_gender[[gen]]$unknown
  whw_gender[[gen]]$weight <- whw_gender[[gen]]$Grand.Total / whw_gender[[gen]]$known
  whw_gender[[gen]]$weight[is.na( whw_gender[[gen]]$weight)] <- 1 
  weights[[gen]] <- whw_gender[[gen]]
  for(i in 2:9){
    weights[[gen]][1:10,i] <- weights[[gen]][1:10,i] * weights[[gen]][1:10,13]
    weights[[gen]][11,i] <- sum(weights[[gen]][1:10,i])
  }
  weights[[gen]][12,2:9] <-  weights[[gen]][11,2:9] - weights[[gen]][10,2:9]
  weights[[gen]][13,2:9] <- weights[[gen]][11,2:9] / weights[[gen]][12,2:9]
  weights[[gen]][13,2:9][is.na( weights[[gen]][13,2:9])] <- 1 
}

ages <- whw_gender$male[1:9,1]
age_cats <- sapply(ages,function(x)as.numeric(strsplit(as.character(x),'-')[[1]]))
injuries <- data.frame(cas_mode=character(),cas_gender=character(),cas_age=character(),strike_mode=character(),weight=numeric(),stringsAsFactors = F)
number_of_years <- 1
for(gen in 1:2){
for(i in 1:(nrow(whw_gender[[gen]])-2)) # age cat
  for(j in 2:(ncol(whw_gender[[gen]])-4)){ # mode
    count <- whw_gender[[gen]][i,j]
    if(count>0){
      weight <- weights[[gen]][13,j] * weights[[gen]][i,13]
      for(k in 1:round(count)){ 
        #print(c(k,count))
        injuries[nrow(injuries)+1,] <- c(colnames(whw_gender[[gen]])[j],names(whw_gender)[gen],sample(age_cats[1,i]:age_cats[2,i],1),'NOV',as.numeric(weight))
      }
    }
  }
}
injuries$weight <- as.numeric(injuries$weight)
sum(injuries$weight)
unique(injuries$cas_mode)
unique(injuries$strike_mode)
injuries$cas_mode[injuries$cas_mode=='motorbike'] <- 'motorcycle'
injuries$cas_mode[injuries$cas_mode=='bike'] <- 'bicycle'
injuries
head(injuries)
unique(injuries$cas_mode)
unique(injuries$strike_mode)
injury_file <- 'injuries_buenos_aires.csv'
write.csv(injuries,paste0('inst/extdata/local/buenos_aires/',injury_file))
