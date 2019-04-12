#####LONDON#####
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/London/LTDS0514_Combined_V3U_v1.2')
trips<-read.table('Trip_ltds.txt', header = TRUE, sep=",")
stages<-read.table('Stage.txt', header=TRUE, sep=",")
trips_subset<- subset(trips, select=c("ttid", "tdbmmode", "tlenn"))
stages_subset<- subset(stages, select=c("ssid", "stid", "smode", "swalkdur"))
names(stages_subset)[2]<-"ttid"
stages_subset <- stages_subset %>% left_join(trips_subset, by="ttid")


##in case we want the walking duration by trip distance of bus
stages_subset$dist_cat[stages_subset$tlenn <=1 ] <-  "0 - 1 km"
stages_subset$dist_cat[stages_subset$tlenn >1 & stages_subset$tlenn <=5] <-  "2 - 5 km"
stages_subset$dist_cat[stages_subset$tlenn >5 & stages_subset$tlenn <=10] <-  "6 - 10 km"
stages_subset$dist_cat[stages_subset$tlenn >10 & stages_subset$tlenn <=20] <-  "11 - 20 km"
stages_subset$dist_cat[stages_subset$tlenn >20 & stages_subset$tlenn <=30] <-  "21 - 30 km"
stages_subset$dist_cat[stages_subset$tlenn >30 ] <-  "30+ km"


stages_bus<- stages_subset[which(stages_subset$swalkdur>0  & stages_subset$tdbmmode==13),]
stages_trains<- stages_subset[which(stages_subset$swalkdur>0  & stages_subset$tdbmmode==17),]

setwd("V:/Group/RG_PHM/ITHIM-R/code/PT_walk")

##trip wise total of walking duration for buses
bus_walk<- stages_bus %>% group_by(ttid) %>% summarise(walk_dur=sum(swalkdur))
write.csv(bus_walk, 'walking_duration_for_buses.csv')
plot(density(bus_walk$walk_dur), xlim=c(0,30))
summary(bus_walk$walk_dur)

##trip wise total of walking duration for underground
train_walk<- stages_trains %>% group_by(ttid) %>% summarise(walk_dur=sum(swalkdur))
write.csv(train_walk, 'walking_duration_for_subway.csv')
plot(density(train_walk$walk_dur), xlim=c(0,30))
summary(train_walk$walk_dur)
