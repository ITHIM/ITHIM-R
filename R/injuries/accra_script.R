setwd('~/overflow_dropbox/ITHIM-R/R/injuries/')
library(tidyr)
library(dplyr)
library(splines)
library(sfsmisc)
library(readxl)
library(stringr)
library(ReIns)

## true population
true_pop <- read_xlsx(path='~/overflow_dropbox/ITHIM-R/R/injuries/data/accra_population.xlsx',sheet=2)
true_pop <- true_pop[-dim(true_pop)[1],c(1,3,5)]
age_bands <- str_split(string=true_pop$'Age bands',pattern='-',n=2,simplify=TRUE)
lower_ages <- as.numeric(age_bands[,1])
upper_ages <- as.numeric(age_bands[,2])
modes <- list(bus.passenger='Bus',car.passenger='Taxi',pedestrian='Walking',car='Private Car',cyclist='Bicycle',motorcycle='Motorcycle')
## speeds
speeds <- list(bus.passenger=15,car.passenger=21,pedestrian=4.8,car=21,cyclist=14.5,motorcycle=25,tuktuk=22)
## parse trip data
trips <- read.csv('data/accra_trip.csv')
## get population surveyed
unique_id <- subset(trips,!duplicated(participant_id))
pop_numbers <- matrix(0,nrow=length(lower_ages),ncol=2)
rownames(pop_numbers) <- lower_ages
colnames(pop_numbers) <- c('Female','Male')
for(i in 1:2)
  pop_numbers[,i] <- apply(cbind(lower_ages,upper_ages),1,function(x)nrow(subset(unique_id,age<=x[2]&age>=x[1]&sex==c('Female','Male')[i])))

if(file.exists('accra_processed_trips.Rds')){
  trips0 <- readRDS('accra_processed_trips.Rds')
}else{
  ## get distance data
  trips$trip_mode <- factor(trips$trip_mode,levels=c(levels(trips$trip_mode),'Motorcycle'))
  trips$trip_mode[trips$trip_mode=='Other'&trips$trip_duration<60] <- 'Motorcycle'
  trips0 <- subset(trips,trip_mode%in%unlist(modes)) # excludes 99, Other, Unspecified, Train
  ## bus wait and walk times
  min_wait_time <- c(0,5,10,15,20,30)#exclusive
  max_wait_time <- c(5,10,15,20,30,60)#inclusive
  wait_time_proportion <- c(51.3,20.3,7.6,6.3,8.3,6.2)
  wait_rate <- 0.11
  x11(); plot(c(1:60),pexp(c(1:60),rate=wait_rate),typ='l')
  points(max_wait_time,sapply(1:length(max_wait_time),function(x)sum(wait_time_proportion[1:x])/100))
  min_walk_time <- c(0,10,20,30,40,60)#exclusive
  max_walk_time <- c(10,20,30,40,60,max(subset(trips0,trip_mode=='Bus')$trip_duration)+1)#inclusive
  walk_time_proportion <- c(80.6,13.5,4.3,1.3,0.1,0)
  walk_rate <- 0.15
  x11(); plot(c(1:60),pexp(c(1:60),rate=walk_rate),typ='l')
  points(max_walk_time,sapply(1:length(max_walk_time),function(x)sum(walk_time_proportion[1:x])/100))
  min_bus_duration <- 5
  
  ## subtract wait and walk times. add new walk journeys
  bus_trips <- subset(trips0,trip_mode=='Bus')
  other_trips <- subset(trips0,trip_mode!='Bus')
  bus_trips$trip_duration <- sapply(bus_trips$trip_duration,function(x)x-rtexp(1,rate=wait_rate,endpoint=x-min_bus_duration))
  walk_trips <- bus_trips
  walk_trips$trip_mode <- 'Walking'
  walk_trips$trip_duration <- sapply(walk_trips$trip_duration,function(x)rtexp(1,rate=wait_rate,endpoint=x-min_bus_duration))
  bus_trips$trip_duration <- bus_trips$trip_duration - walk_trips$trip_duration
  trips0 <- rbind(other_trips,bus_trips,walk_trips)
  trips0$trip_distance <- apply(cbind(trips0$trip_mode,trips0$trip_duration),1,function(x)speeds[[which(unlist(modes)==levels(trips0$trip_mode)[x[1]])]]*x[2]/60)
  saveRDS(trips0,'accra_processed_trips.Rds')
}

## accra distance with population ages
distance_dataset <- expand.grid(age=lower_ages,gen=c('Female','Male'))
distance_dataset$upper_age <- upper_ages[match(distance_dataset$age,lower_ages)]
for(i in names(modes))
  distance_dataset[[i]] <- apply(distance_dataset,1,function(x)sum(subset(trips0,age>=as.numeric(x[1])&age<=as.numeric(x[3])&sex==x[2]&trip_mode%in%modes[[i]])$trip_duration))/60*speeds[[i]]*365
for(i in 1:2)
  for(j in 1:length(lower_ages))
    if(pop_numbers[j,i]>0)
      distance_dataset[distance_dataset$age==lower_ages[j]&distance_dataset$gen==c('Female','Male')[i],4:(length(modes)+3)] <- 
  distance_dataset[distance_dataset$age==lower_ages[j]&distance_dataset$gen==c('Female','Male')[i],4:(length(modes)+3)]*as.numeric(true_pop[j,4-i])/pop_numbers[j,i]

source('~/soft/colour.R')
cols <- spiral(length(modes)+1)[1:length(modes)+1]
x11(width=12,height=6); par(mar=c(5,5,1,1),mfrow=c(1,2))
for(g in c('Female','Male'))
  for(i in 1:length(names(modes)))
    if(i==1){
      plot(as.numeric(lower_ages),1e-6*as.numeric(distance_dataset[[names(modes)[i]]][distance_dataset$gen==g]),typ='b',col=cols[i],
        ylim=c(0,1e-6*max(distance_dataset[,4:8])),frame=F,xlab='Age',ylab='Million km',lwd=2,main=g,cex.lab=1.5,cex.axis=1.5)
    }else{
      lines(as.numeric(lower_ages),1e-6*as.numeric(distance_dataset[[names(modes)[i]]][distance_dataset$gen==g]),typ='b',col=cols[i],lwd=2)
    }
legend(x=60,y=1e-6*max(distance_dataset[,4:8]),lwd=2,col=cols,legend=names(modes),bty='n')

x11(width=12,height=6); par(mar=c(5,5,1,1),mfrow=c(1,2))
for(g in c('Female','Male'))
  for(i in 1:length(names(modes)))
    if(i==1){
      plot(as.numeric(lower_ages),as.numeric(distance_dataset[[names(modes)[i]]][distance_dataset$gen==g])/true_pop[[4-which(c('Female','Male')==g)]],typ='b',col=cols[i],
        ylim=c(0,max(distance_dataset[,4:8]/true_pop[[4-which(c('Female','Male')==g)]])),frame=F,xlab='Age',ylab='km',lwd=2,main=g,cex.lab=1.5,cex.axis=1.5)
    }else{
      lines(as.numeric(lower_ages),as.numeric(distance_dataset[[names(modes)[i]]][distance_dataset$gen==g])/true_pop[[4-which(c('Female','Male')==g)]],typ='b',col=cols[i],lwd=2)
    }
legend(x=60,y=max(distance_dataset[,4:8]/true_pop[[4-which(c('Female','Male')==g)]]),lwd=2,col=cols,legend=names(modes),bty='n')


## prepare new dataset
new_data <- expand.grid(cas_mode=c('pedestrian','cyclist','car/taxi','bus','motorcycle'),strike_mode=c('pedestrian','cyclist','car/taxi','motorcycle','bus'),cas_age=lower_ages,strike_age=lower_ages,
                        cas_male=c(0,1),strike_male=c(0,1),cas_severity=c('Serious','Fatal'))
new_data$cas_distance <- 0
new_data$strike_distance <- 0
casualty_mode <- list(pedestrian='pedestrian',cyclist='cyclist','car/taxi'=c('car.passenger','car'),bus='bus.passenger',motorcycle='motorcycle')
str_mode <- list(pedestrian='pedestrian',cyclist='cyclist','car/taxi'=c('car'),bus='bus',motorcycle='motorcycle')
distance_dataset$bus <- 0
for(i in 1:length(casualty_mode)){
  indices4C <- new_data$cas_mode%in%names(casualty_mode)[i]
  indices4S <- new_data$strike_mode%in%names(str_mode)[i]
  for(k in 1:length(c(0,1))){
    indices5C <- indices4C&new_data$cas_male==c(0,1)[k]
    indices5S <- indices4S&new_data$strike_male==c(0,1)[k]
    for(j in 1:length(lower_ages)){
      indicesC <- indices5C&new_data$cas_age==lower_ages[j]
      new_data$cas_distance[indicesC] <- sum(distance_dataset[distance_dataset$age==lower_ages[j]&distance_dataset$gen==c('Female','Male')[k],
                                                              which(names(distance_dataset)%in%casualty_mode[[i]])])
      indicesS <- indices5S&new_data$strike_age==lower_ages[j]
      new_data$strike_distance[indicesS] <- sum(distance_dataset[distance_dataset$age==lower_ages[j]&distance_dataset$gen==c('Female','Male')[k],
                                                                 which(names(distance_dataset)%in%str_mode[[i]])])
    }
  }
}
new_data$cas_distance <- new_data$cas_distance*1e-9
new_data$strike_distance <- new_data$strike_distance*1e-9
new_data_dist <- subset(new_data,cas_distance>0&strike_distance>0&strike_mode%in%c('pedestrian','cyclist','car/taxi','motorcycle'))
new_data_const <- rbind(subset(new_data,strike_mode=='bus'),mutate(subset(new_data,strike_mode=='bus'),strike_mode='heavy goods'),mutate(subset(new_data,strike_mode=='bus'),strike_mode='light goods'))
new_data_const$strike_distance <- 0.2

  
## london model
test_data <- readRDS('~/overflow_dropbox/ITHIM/InjuryModel/ssgYearSegmented_ldn.Rdata')
test_data <- subset(test_data,cas_distance>0)
test_data_dist <- droplevels(subset(test_data,strike_mode%in%c('pedestrian','cyclist','car/taxi','motorcycle')))
test_data_dist <- subset(test_data_dist,strike_distance>0)
test_data_const <- subset(test_data,strike_mode%in%c('heavy goods','light goods','bus'))
test_data_const$strike_distance <- 1
rm(test_data)

factors <- c('cas_mode','strike_mode','cas_severity','strike_male','cas_male','ns(cas_age,df=5)','ns(strike_age,df=5)')
form_dist <- 'count~offset(log(cas_distance))+offset(log(strike_distance))'
for(i in 1:length(factors)) form_dist <- paste(c(form_dist,factors[i]),collapse='+')
for(i in c(1:length(factors))[-c(1,6,7)]) form_dist <- paste(c(form_dist,paste(c(factors[i],factors[1]),collapse=':')),collapse='+')
for(i in c(1:length(factors))[-c(1,2,6,7)]) form_dist <- paste(c(form_dist,paste(c(factors[i],factors[2]),collapse=':')),collapse='+')
for(i in c(4,5)) form_dist <- paste(c(form_dist,paste(c(factors[i],factors[3]),collapse=':')),collapse='+')
fit_dist <- glm(as.formula(form_dist),data=test_data_dist,family=poisson(),control=glm.control(maxit=200))
form_const <- 'count~offset(log(cas_distance))+offset(log(strike_distance))'
for(i in c(1,2,3,5,6)) form_const <- paste(c(form_const,factors[i]),collapse='+')
for(i in c(2,3,5)) form_const <- paste(c(form_const,paste(c(factors[i],factors[1]),collapse=':')),collapse='+')
for(i in c(3,5)) form_const <- paste(c(form_const,paste(c(factors[i],factors[2]),collapse=':')),collapse='+')
for(i in c(5)) form_const <- paste(c(form_const,paste(c(factors[i],factors[3]),collapse=':')),collapse='+')
fit_const <- glm(as.formula(form_const),data=test_data_const,family=poisson(),control=glm.control(maxit=100))
test_data_dist$pred <- predict(fit_dist,newdata=test_data_dist,type='response')
test_data_const$pred <- predict(fit_const,newdata=test_data_const,type='response')
sum(c(test_data_const$pred[test_data_const$cas_severity=='Fatal'],test_data_dist$pred[test_data_dist$cas_severity=='Fatal']))/8
x11(); plot(test_data_dist$count,test_data_dist$pred)
x11(); plot(test_data_const$count,test_data_const$pred)

## apply to new dataset
new_data_dist$pred <- predict(fit_dist,newdata = new_data_dist,type='response')
c(sum(new_data_dist$pred[new_data_dist$cas_severity=='Fatal'])/1.6,sum(subset(test_data_dist,cas_severity=='Fatal'&year==2015)$pred)/8)
new_data_const$pred <- predict(fit_const,newdata = new_data_const,type='response')
c(sum(new_data_const$pred[new_data_const$cas_severity=='Fatal'])/1.6,sum(subset(test_data_const,cas_severity=='Fatal'&year==2015)$pred)/8)

c(sum(subset(test_data_const,cas_mode=='pedestrian'&strike_mode=='bus'&cas_severity=='Fatal'&cas_age>14&strike_age>14&cas_age<85&strike_age<85)$pred)/8,
  sum(subset(new_data_const,cas_mode=='pedestrian'&strike_mode=='bus'&cas_severity=='Fatal')$pred)/1.6)


for(sev in c('Fatal','Serious'))
  print(sapply(names(casualty_mode),function(x)(sum(subset(new_data_dist,cas_mode==x&cas_severity==sev)$pred)+sum(subset(new_data_const,cas_mode==x&cas_severity==sev)$pred))/
                   (sum(subset(new_data_dist,cas_severity==sev)$pred)+sum(subset(new_data_const,cas_severity==sev)$pred))*100))
print(sapply(c(0,1),function(x)(sum(subset(new_data_dist,cas_male==x&cas_severity=='Fatal')$pred)+sum(subset(new_data_const,cas_male==x&cas_severity=='Fatal')$pred)))/
        (sum(subset(new_data_const,cas_severity=='Fatal')$pred)+sum(subset(new_data_dist,cas_severity=='Fatal')$pred))*100)
for(x in c('Fatal','Serious'))
  print(c(sum(subset(new_data_dist,cas_severity==x)$pred),sum(subset(new_data_const,cas_severity==x&strike_mode=='light goods')$pred)))


