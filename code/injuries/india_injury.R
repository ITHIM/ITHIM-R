setwd('~/overflow_dropbox/ITHIM-R/code/injuries/')

cities <- c('bangalore','delhi')
injury_data <- list()
travel_data <- list()
pop_data <- list()
for(x in cities){
  injury_data[[x]] <- read.csv(paste0('data/',x,'_road_deaths_matrix.csv'))
  travel_data[[x]] <- readRDS(paste0('data/',x,'_travel_survey.RDS'))
  names(travel_data[[x]]) <- tolower(names(travel_data[[x]]))
  travel_data[[x]] <- travel_data[[x]][!is.na(travel_data[[x]]$age),]
  travel_data[[x]]$age <- floor(travel_data[[x]]$age)
  if(x=='bangalore'){
    names(travel_data[[x]])[which(names(travel_data[[x]])=='gender')] <- 'female'
    travel_data[[x]]$female <- travel_data[[x]]$female - 1
    travel_data[[x]]$hh_weights <- 1
  }
  travel_data[[x]] <- travel_data[[x]][!is.na(travel_data[[x]]$distance),]
  travel_data[[x]] <- travel_data[[x]][!is.na(travel_data[[x]]$hh_weights),]
  travel_data[[x]] <- travel_data[[x]][!is.na(travel_data[[x]]$mode_name),]
  travel_data[[x]] <- travel_data[[x]][!travel_data[[x]]$mode_name%in%c("Metro","Rail","Train"),]
  travel_data[[x]] <- travel_data[[x]][travel_data[[x]]$female%in%c(0,1),]
  pop_data[[x]] <- read.csv(paste0('data/single_year_age_sex_',x,'.csv'))[1:103,]
}

all_travel_ages <- sort(unique(unlist(sapply(travel_data,function(x)x$age))))
all_travel_genders <- 0:1
all_travel_passengers <- 0:1
all_modes <- unique(as.vector(sapply(travel_data,function(x)unique(x$mode_name))))
mode_passenger <- c(0,1,0,0,0,1,1,0,1,1,1)

delhi_weights <- readRDS('data/households_delhi_2.RDS')
delhi_weights <- delhi_weights[apply(delhi_weights,1,function(x) sum(is.na(x))==0),]
delhi_weights$age <- floor(delhi_weights$age)
no_journeys <- delhi_weights[which(!delhi_weights$person_id%in%unique(travel_data$delhi$person_id)),]
no_journeys_append <- data.frame(hh_id=delhi_weights$hh_id, person_id=delhi_weights$person_id, female=delhi_weights$female, age=delhi_weights$age,
                                 trip_id=NA, stage=NA,  mode=NA, mode_name=NA, distance=NA,  duration=NA, hh_weights=delhi_weights$hh_weights)

travel_data$delhi <- rbind(travel_data$delhi,no_journeys_append)

distance_array <- list()
for(x in cities){
  distance_array[[x]] <- expand.grid(age=all_travel_ages,female=all_travel_genders)
  for(i in 1:length(all_modes)){
    distance_array[[x]][[all_modes[i]]] <- 
      apply(distance_array[[x]],1,
            function(y)sum(subset(travel_data[[x]],female==y[2]&age==y[1]&mode_name==all_modes[i])$distance*
                             subset(travel_data[[x]],female==y[2]&age==y[1]&mode_name==all_modes[i])$hh_weights)/
              sum(subset(travel_data[[x]],female==y[2]&age==y[1])$hh_weights)*
              pop_data[[x]][which(pop_data[[x]][,1]==y[1]),4-y[2]])*365
  }
  distance_array[[x]][is.na(distance_array[[x]])] <- 0
}

casualty_modes <- levels(injury_data$delhi[,1])
strike_modes <- names(injury_data$delhi)

mode_map <- list()
mode_map$'Auto Rickshaw' <- c("3Wheeled","X2.3.Wheeled")
mode_map$Bus <- c("Bus","Bus")
mode_map$Car <- c("Car","Car.pick.up.van")
mode_map$Cycle <- c("Pedal Cycle","Pedal.cycle")
mode_map$MTW <- c("Motorcycle","X2.3.Wheeled")
mode_map$'Shared Taxi' <- c("Car","Car.pick.up.van")
mode_map$Taxi <- c("Car","Car.pick.up.van")
mode_map$'Shared Auto' <- c("Car","Car.pick.up.van")
mode_map$Walk <- c("Pedestrian","Pedestrian")
mode_map$'Cycle Rickshaw' <- c("Other","non.motor.vehicle")

casualty_modes <- casualty_modes[casualty_modes%in%sapply(mode_map,function(x)x[1])]
strike_modes <- strike_modes[strike_modes%in%sapply(mode_map,function(x)x[2])]


modes_to_subtract <- all_modes[mode_passenger==1]
drive_modes <- c('MTW','Car','Other')
overall_data <- list()
for(x in cities){
  overall_data[[x]] <- expand.grid(casualty_mode=casualty_modes,strike_mode=strike_modes)
  overall_data[[x]]$country <- x
}
overall_data$delhi$years <- 4
overall_data$bangalore$years <- 1
for(x in cities){
  overall_data[[x]]$injuries <- apply(overall_data[[x]],1,function(y)injury_data[[x]][injury_data[[x]][,1]==y[1],names(injury_data[[x]])==y[2]])
  overall_data[[x]]$cas_distance <- 1e-9*sapply(overall_data[[x]]$casualty_mode,
                                           function(y)sum(distance_array[[x]][,sapply(names(distance_array[[x]]),function(z)mode_map[[z]][1])==y]))
  strike_distances <- distance_array[[x]]
  strike_distances[,modes_to_subtract] <- 0
  strike_distances[strike_distances$age<15,drive_modes] <- 0
  overall_data[[x]]$strike_distance <- 1e-9*sapply(overall_data[[x]]$strike_mode,
                                           function(y)sum(strike_distances[,sapply(names(distance_array[[x]]),function(z)mode_map[[z]][2])==y]))
  overall_data[[x]]$rate <- overall_data[[x]]$injuries/overall_data[[x]]$cas_distance/overall_data[[x]]$strike_distance/overall_data[[x]]$years
}

data1 <- log(overall_data[[1]]$rate)
data2 <- log(overall_data[[2]]$rate)
data1[!is.finite(data1)] <- -15
data2[!is.finite(data2)] <- -15
x11(); plot(data1,data2)

cols <- spiral(7,offset=3)
x11(width=10,height=10); par(mar=c(5,5,1,1),mfrow=c(2,2))
for(x in cities){
  for(i in 0:1)
    matplot(subset(distance_array[[x]],female==i)$age,cbind(subset(distance_array[[x]],female==i)[,c(3,4,5,6,10,13)],rowSums(subset(distance_array[[x]],female==i)[,c(7:9,11,12)])),
            pch=16,col=cols,main=paste0(x,', ',c('M','F')[i+1]),frame=F,ylab='Distance',xlab='Age',cex.axis=1.5,cex.lab=1.5)
  legend(x=70,y=2e9,legend=c(names(distance_array[[x]])[c(3,4,5,6,10,13)],'Car'),fill=cols,bty='n')
}

x11(width=10,height=10); par(mar=c(5,5,1,1),mfrow=c(2,2))
for(x in cities){
  for(i in 0:1)
    matplot(subset(distance_array[[x]],female==i)$age,log(cbind(subset(distance_array[[x]],female==i)[,c(3,4,5,6,10,13)],rowSums(subset(distance_array[[x]],female==i)[,c(7:9,11,12)]))),
            pch=16,col=cols,main=paste0(x,', ',c('M','F')[i+1]),frame=F,ylab='Distance',xlab='Age',cex.axis=1.5,cex.lab=1.5)
  legend(x=70,y=2e9,legend=c(names(distance_array[[x]])[c(3,4,5,6,10,13)],'Car'),fill=cols,bty='n')
}

