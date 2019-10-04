library(dplyr)
#####Colombia--Bogota (Use this one)#### 
#(N persons = 91765)

##reading processed trips data of Bogota
write.csv(trips, 'V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Colombia/Bogota/Travel/bogota_trips.csv')

bogota_trips<- read.csv('raw_data/trips/bogota_trips.csv')
str(bogota_trips)
x<-bogota_trips %>% group_by(main_mode_eng) %>% summarise(sum(trip_weight,na.rm=T))
x$share<- x$`sum(trip_weight, na.rm = T)`/sum(x$`sum(trip_weight, na.rm = T)`)

bogota_trips$dur_weight<- bogota_trips$DURATION*bogota_trips$trip_weight
x<-bogota_trips %>% group_by(main_mode_eng) %>% summarise(dur_weight=sum(dur_weight,na.rm=T), sum_weights=sum(trip_weight,na.rm=T),n_trips=n())

##calculating weighted total duration by different modes
x$total_duration<-x$dur_weight*x$n_trips/x$sum_weights

##excluding the non-motorised modes
nmt<- c("Bicycle", "Walk","Other")
##only motorised modes
x<- x[which(!x$main_mode_eng %in% nmt),]
#combining cars and taxis
x[2,2:5]<-x[2,2:5] +x[4,2:5]
##removing taxi
x<- x[-4,]
#attaching speed values
x<- x %>% left_join(lookup_mode_name,by="main_mode_eng")
##removing repeating bus values
x<-x[!duplicated(x$main_mode_eng),]
##converting duration to distance
x$total_distance<- x$total_duration*x$speed/60
##converting distance to per capita (sample size)
x$dist_per_capita<- x$total_distance/91765
##calculating it for the whole population
x$total_distance_pop<- x$dist_per_capita*7776665
##multiplying by the duration scaling factor (as duration in Bogota data seems over)
x$total_distance_pop_corr<-x$total_distance_pop*0.7
##adding transport emissions for each mode from EDGAR data
x$emissions<-c(65.18, 267.38, 6.67) 
##calculating emissions per unit distance
x$emis_per_km<- x$emissions/x$total_distance_pop_corr
#write.csv(x,'V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Colombia/Bogota/Air pollution/emission_calculations.csv')



#####Chile Santiago#####
###added duration in the raw excel file(Viaje) as the column 'duration'
setwd("V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Chile/Travel Surveys/Santiago")
#import data
person <- read.csv("Persona.csv")
trip <- read.csv("Viaje.csv")
stage <- read.csv("stages.csv")
age<-read.csv('EdadPersonas.csv')
lookup_person_sex <- read.csv("lookup_person_sex.csv")
lookup_trip_purpose <- read.csv("lookup_trip_purpose.csv")
lookup_trip_mode <- read.csv("lookup_trip_mode.csv")
lookup_stage_mode <- read.csv("lookup_stage_mode.csv")

#add age and sex to person
person <- person %>% select(Hogar, Persona, Sexo) %>% left_join(age) %>% left_join(lookup_person_sex) %>% {.[,-3]}

#extract relevant variable in tripset
trip <- trip %>% select(Hogar, Persona, Viaje, PropositoAgregado,TiempoViaje , ModoAgregado) %>% left_join(lookup_trip_mode) %>% left_join(lookup_trip_purpose)%>% {.[,-c(4,6,8)]}

#extract relevant variable in stage
stage <- stage %>% select(Hogar, Persona, Viaje, Etapa, Modo) %>% left_join(lookup_stage_mode) %>% {.[,-c(5,7)]}

#add trip and stages to person
trip <- person %>% left_join(trip) %>% left_join(stage) %>% select(Persona, Edad, sex, Viaje, Etapa, stage_mode, trip_mode, TiempoViaje, trip_purpose)
names(trip) <- c("participant_id", "age", "sex", "trip_id", "stage_id", "stage_mode", "trip_mode", "trip_duration", "trip_purpose")

#explore data quality
#average trip per person
trip %>% distinct(trip_id) %>% nrow / trip %>% distinct(participant_id) %>% nrow
#modeshare
round(prop.table(table(factor(trip$trip_mode)))*100,2)
#average travel time per mode
trip %>% group_by(participant_id, age, sex, trip_id, trip_duration, trip_mode) %>% summarise(n_stage= n()) %>% group_by(trip_mode) %>% summarise(average_mode_duration = mean(trip_duration, na.rm = T))

nmt<- c("walk", "bicycle", "metro","other")
##only motorised modes
trip_mot<- trip[which(!trip$trip_mode %in% nmt),]

summary<- trip_mot %>% group_by(trip_mode) %>% summarise(total_duration=sum(trip_duration,na.rm=T))
#combining cars and taxis
summary[2,2]<-summary[2,2]+summary[3,2]
summary<-summary[-3,]
summary<-summary[-3,]

summary$speed<- c(18,18)
summary$total_distance<- summary$total_duration*summary$speed/60
summary$dist_per_capita<- summary$total_distance/60054
summary$total_distance_pop<- summary$dist_per_capita*6300000

#reading emission per km estimates for Bogota
bogota_emissions<- read.csv('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Colombia/Bogota/Air pollution/emission_calculations.csv')
summary$emissions[1]<-bogota_emissions$emis_per_km[1]*summary$total_distance_pop[1]
summary$emissions[2]<-bogota_emissions$emis_per_km[2]*summary$total_distance_pop[2]

summary$trip_mode<-as.character(summary$trip_mode)
summary[3:5,]<- NA
summary$trip_mode[3:5]<-c("ldv", "hdv", "mc")
summary$emissions[3:5]<-c(123.99,17.43,3.74)
summary$emission_share<- summary$emissions/sum(summary$emissions)
write.csv(summary,'V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Chile/Santiago/corrected_emissions_estimates_santiago_using_bogota.csv')
