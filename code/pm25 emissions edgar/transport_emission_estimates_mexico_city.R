##Mexico city ####
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Mexico/Travel surveys/Mexico City 2017/Databases/eod_2017_csv')
mexico_pp<-read.csv('tsdem_eod2017/conjunto_de_datos/tsdem.csv')  ## individual level
mexico_trip<-read.csv('tviaje_eod2017/conjunto_de_datos/tviaje.csv')
mexico_stages<-read.csv('ttransporte_eod2017/conjunto_de_datos/ttransporte.csv')
lookup<- read.csv('mode_names_lookup_spanish_english.csv')
library(dplyr)

##selecting only the relevant variables
mexico_pp<- subset(mexico_pp, select=c("id_soc", "id_hog"))  ## individual (soc) and household id
mexico_trip<- subset(mexico_trip, select=c("id_via","id_soc", "p5_13"))  ## individual and trip id and trip purpose



## joining with mexico_pp to attached the household id
mexico_trip<-left_join(mexico_trip, mexico_pp, by="id_soc")

head(mexico_stages)

mexico_stages$duration<-as.numeric(mexico_stages$p5_16_1_2 +(mexico_stages$p5_16_1_1*60)) 

mexico_stages$duration[which(mexico_stages$duration==6039)]<-NA ## cases where values of both minute and hour are 99-- probably unknown values
x<-as.data.frame(mexico_stages %>% group_by(id_via) %>% summarise(max(duration)))
x<-as.data.frame(x)

mexico_stages<-left_join(mexico_stages, x, by="id_via")
mexico_stages$if_main_mode<-0
mexico_stages$if_main_mode[which(mexico_stages$duration== mexico_stages$`max(duration)`)]<-1  ## allocating 1 to the row where maximum duration among stages 
#equals duration of the stage

mexico_stages<-mexico_stages[which(mexico_stages$if_main_mode==1),]  ## this removes the duplicate stages
head(mexico_stages)

#mexico_stages$trip_main_mode<- 0
#mexico_stages$trip_main_mode[which(mexico_stages$p5_14==7)]<-1

##attaching the english mode names

mexico_stages <- mexico_stages %>% left_join(lookup, by='p5_14')

mexico_stages<- left_join(mexico_stages, mexico_trip, by="id_via")

mexico_stages$dur_weight<- mexico_stages$duration*mexico_stages$factor
x<-mexico_stages %>% group_by(mode_english) %>% summarise(dur_weight=sum(dur_weight,na.rm=T), sum_weights=sum(factor,na.rm=T),n_trips=n())

##calculating weighted total duration by different modes
x$total_duration<-x$dur_weight*x$n_trips/x$sum_weights


##excluding the non-motorised modes
nmt<- c("Bike", "Light_rail","Metro", "Mexicable", "Other", "Personal", "Rickshaw", "Suburban_train", "Walk")
##only motorised modes
x<- x[which(!x$mode_english %in% nmt),]
x#combining cars and taxis, bus and minibus, and mototaxi and motorcycle
x[1,2:5]<-x[1,2:5] +x[3,2:5]
x[2,2:5]<-x[2,2:5] +x[6,2:5]
x[4,2:5]<-x[4,2:5] +x[5,2:5]
##removing taxi, minibus and mototaxi
x<- x[-c(3,5,6),]
#attaching speed values using Delhi estimates
x$speed<-c(15,21,25)

##converting duration to distance
x$total_distance<- x$total_duration*x$speed/60
##converting distance to per capita (sample size)
x$dist_per_capita<- x$total_distance/200117
##calculating it for the whole population
x$total_distance_pop<- x$dist_per_capita*8850000

#reading emission per km estimates for Bogota
bogota_emissions<- read.csv('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Colombia/Bogota/Air pollution/emission_calculations.csv')
x$emissions[1]<-bogota_emissions$emis_per_km[1]*x$total_distance_pop[1]
x$emissions[2]<-bogota_emissions$emis_per_km[2]*x$total_distance_pop[2]
x$emissions[3]<-bogota_emissions$emis_per_km[3]*x$total_distance_pop[3]

x$mode_english<-as.character(x$mode_english)
x[4:5,]<- NA
x$mode_english[4:5]<-c("ldv", "hdv")
x$emissions[4:5]<-c(42.72,62.27)
x$emission_share<- x$emissions/sum(x$emissions)
write.csv(x,'V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Mexico/Air pollution/mexico_city_transport_emissions_calculations.csv')
