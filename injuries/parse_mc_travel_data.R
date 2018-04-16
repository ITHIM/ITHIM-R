setwd('~/overflow_dropbox/ITHIM-R/injuries/')
library(xlsx)

## read trip dataset
trips0 <- read.csv('tviaje.csv')
trips <- trips0[,c(1,2,26*2+6,26*3+3:4)]
names(trips) <- c('trip_id','person_id','passenger','gen','age')

## read stage dataset
stages0 <- read.csv('ttransporte.csv')
stages <- stages0[,c(1,2,3,5,7,8)]
names(stages) <- c('stage_id','trip_id','weekend','mode','hours','minutes')
stages$time <- stages$minutes + 60*stages$hours
stages$weekend <- stages$weekend - 1

## list modes
modes <- c('Car','Minibus','Taxi (Internet app)','Taxi (street)','Subway',
  'Bus','Bycicle','Bus','Motorcycle','Tram',
  'Bus Rapid Transit (BRT)','Light Rail Transit (LRT)','Train','Walk','Cable car',
  'Bycicle taxi','Motorcycle taxi','School bus','Charter bus','Other')
## define whether modes are by default passenger or not
passenger <- c(0,1,1,1,1,
  1,0,1,0,1,
  1,1,1,0,1,
  1,1,1,1,1)
## list modes to keep (exclude train, tram, subway, cable car, other)
keep_mode <- c(1,2,3,4,6,7,8,9,11,14,16,17,18,19)

stages <- subset(stages,mode%in%keep_mode)

trips$passenger <- trips$passenger - 1

stages$passenger <- stages$passenger_trip <- trips$passenger[match(stages$trip_id,trips$trip_id)]
stages$passenger[is.na(stages$passenger)] <- passenger[stages$mode[is.na(stages$passenger)]]
stages$passenger[!stages$mode%in%c(1,9)] <- passenger[stages$mode[!stages$mode%in%c(1,9)]]
stages$passenger[stages$age<16&stages$mode%in%c(1,9)] <- 1

stages$gen <- trips$gen[match(stages$trip_id,trips$trip_id)]
stages$age <- trips$age[match(stages$trip_id,trips$trip_id)]

for(i in keep_mode) for(j in 0:1) print(c(i,j,nrow(subset(stages,passenger==j&mode==i))))

mode_list <- list()
mode_list[['pedestrian']] <- 14
mode_list[['cyclist']] <- c(7,16)
mode_list[['motorcycle']] <- c(9,17)
mode_list[['car/taxi']] <- c(1,3,4)
mode_list[['bus']] <- c(2,6,8,11,18,19)

## get population
population <- read.xlsx('MexicoCityPopulationData.xlsx',sheetIndex=1,stringsAsFactors=FALSE,header=F,rowIndex=5:26,colIndex=c(1,4,5),colClasses=c('character','numeric','numeric'))
names(population) <- c('age','m','f')
population$m <- population$m + population$m[22]*population$m/sum(population$m[1:21])
population$f <- population$f + population$f[22]*population$f/sum(population$f[1:21])
population <- population[-22,]
population$age <- as.numeric(sapply(strsplit(as.character(population$age),"[^0-9]+"),function(x)x[1]))

travel_data <- expand.grid(age=population$age,gen=c('m','f'))
for(m in names(mode_list)){
  for(pass in 0:1){
    travel_data[[paste0(m,pass)]] <- 0
    for(i in 1:nrow(travel_data)){
      min_age <- travel_data[i,1]
      max_age <- 120; if(i<nrow(travel_data)) max_age <- travel_data[i+1,1]
      mf <- which(c('m','f')==travel_data[i,2])
      sub_stage <- subset(stages,mode%in%mode_list[[m]]&passenger==pass&gen==mf&age>=min_age&age<max_age)
      travel_data[[paste0(m,pass)]][i] <- sum(sub_stage$time*ifelse(sub_stage$weekend==0,5,2))
    }
  }
}

pop_temp <- population[,c(1,3)]; names(pop_temp)[2] <- 'm'
population_mult <- rbind(population[,1:2],pop_temp)
travel_data$population <- population_mult$m

## get participants
parts0 <- read.csv('tsdem.csv')
parts <- parts0[,c(1,5,6,11,12)]
names(parts)[2:3] <- c('gen','age')
travel_data$participants <- 0
for(i in 1:nrow(travel_data)){
  min_age <- travel_data[i,1]
  max_age <- 120; if(i<nrow(travel_data)) max_age <- travel_data[i+1,1]
  mf <- which(c('m','f')==travel_data[i,2])
  travel_data$participants[i] <- nrow(subset(parts,gen==mf&age>=min_age&age<max_age))
}


travel_data[,3:12] <- travel_data[,3:12] / travel_data$participants * travel_data$population * 52 / 60 * 1e-9
names(travel_data)[3:12] <- c('pedestrian','pedestrian passenger','cyclist','cyclist passenger','motorcycle','motorcycle passenger','car','car passenger','bus','bus passenger')
travel_data[is.na(travel_data)] <- 0
travel_data <- travel_data[,-c(which(colSums(travel_data[,3:12])==0)+2)]
write.csv(travel_data,'mexico_city_travel_data.csv')

