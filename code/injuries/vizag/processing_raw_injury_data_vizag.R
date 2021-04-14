#### Visakhapatnam, India#####
path <- file.path('code/injuries/vizag/')
icd_codes<- read.csv(paste0(path,'/icd_code_look_up_table_row_column.csv'))
hier<-  read.csv(paste0(path,'/striking_vehicle_hierarchy_lookup.csv'))
accident<- read.csv(paste0(path,'/accident.csv'))
vehicle<- read.csv(paste0(path,'/vehicle.csv'))
victim<-read.csv(paste0(path,'/victim.csv'))
pedestrian<- read.csv(paste0(path,'/pedestrian.csv'))
str(victim)
## vehicle ID to do lookup with vehicle file
victim$victim_vehID<- paste(victim$Accident.ID, "_", victim$Vehicle.ID)

##unique ID to ensure repeated victims are given appropriate fraction of weights
victim$victim_uniqueID<- paste(victim$Accident.ID, "_", victim$Victim.ID)

victim<-subset(victim, select=c('Accident.ID', 'Road.User', 'Injury', 'Impacting.Vehicle.No', 'victim_vehID', 'victim_uniqueID'))
pedestrian$victim_vehID<- paste(pedestrian$Accident.ID, "_", pedestrian$Victim.No)
pedestrian$victim_uniqueID<- pedestrian$victim_vehID
pedestrian<-subset(pedestrian, select=c('Accident.ID', 'Road.User', 'Injury', 'Impacting.Vehicle.No', 'victim_vehID', 'victim_uniqueID'))
victim<- rbind(victim, pedestrian)
victim$striking_vehID<- paste(victim$Accident.ID, "_", victim$Impacting.Vehicle.No) 

str(vehicle)
vehicle<- subset(vehicle,select=c('Accident.ID', 'Vehicle.No', 'Vehicle.Type'))
vehicle$striking_vehID<- paste(vehicle$Accident.ID, "_", vehicle$Vehicle.No) 
vehicle<- subset(vehicle, select=c('striking_vehID', 'Vehicle.Type'))
victim<- victim %>% left_join(vehicle, by="striking_vehID")
names(victim)[8]<-"striking_veh_type"

vehicle<- read.csv(paste0(path,'/vehicle.csv'))
vehicle$victim_vehID<- paste(vehicle$Accident.ID, "_", vehicle$Vehicle.ID) 
vehicle<- subset(vehicle, select=c('victim_vehID', 'Vehicle.Type'))
victim<- victim %>% left_join(vehicle, by="victim_vehID")
head(victim)
names(victim)[9]<-"victim_veh_type"

lookup<- read.csv(paste0(path,'/vehicle_type_coding_full_names.csv'))
names(lookup)[1]<- "victim_veh_type"
victim <- victim %>% left_join(lookup, by = "victim_veh_type")
head(victim)
str(victim)
names(victim)[10]<-"victim_vehicle"
lookup<- read.csv(paste0(path,'/vehicle_type_coding_full_names.csv'))
names(lookup)[1]<- "striking_veh_type"
victim <- victim %>% left_join(lookup, by = "striking_veh_type")
str(victim)
names(victim)[11]<- "striking_vehicle"

victim$victim_vehicle<-as.character(victim$victim_vehicle)
for (i in 1: nrow(victim))
{
  
  if (!is.na(victim$Road.User[i]) & victim$Road.User[i]==3)
  {
    victim$victim_vehicle[i]="Pedestrian"
  }
}

for (i in 1: nrow(victim))
{
  
  victim$victim_striking_pair<- paste(victim$victim_vehicle, "-", victim$striking_vehicle)
  
}

#write.csv(as.data.frame(unique(victim$victim_striking_pair)), 'unique_victim_striking_pair.csv')
lookup<- read.csv(paste0(path,'/unique_victim_striking_pair_icd_codes.csv'))
victim<- victim %>% left_join(lookup, by="victim_striking_pair")
icd_codes<- read.csv(paste0(path,'/icd_code_look_up_table_row_column.csv'))
victim<- victim %>% left_join(icd_codes, by="ICD")
head(victim)
##selecting the fatality cases
victim<- victim[which(victim$Injury==3),]

##selecting only one striking vehicle when there are two
victim <- victim %>% left_join(hier, by="striking_vehicle")
x<- victim %>% group_by(victim_uniqueID) %>% summarise("max"=max(hier))
victim <- victim %>% left_join(x, by="victim_uniqueID")
victim<- victim[which(victim$hier==victim$max),]
##removing duplicate victim IDS
victim<- victim[!duplicated(victim$victim_uniqueID),]

##adding year
accident<- subset(accident, select=c("Accident.ID", "Date.of.Accident"))
accident$year<- substr(accident$Date.of.Accident, 7,10)
accident<- accident[, -2]
victim <- victim %>% left_join(accident, by="Accident.ID")


strik_list<- c('No other/fixed or stationary object'	,'Pedestrian',	'Pedal cycle',	'2/3 Wheeled',	'Car/pick-up/van'	,'Bus'	,'Railway train/railway vehicle',	'non-motor vehicle',	'unspecified',	'Non-collision', 'Unknown', 'Trucks')
vict_list<- c('Pedestrian'	,'Pedal Cycle',	'Motorcycle',	'3Wheeled',	'Car'	,'Pick-up truck/van',	'Heavy transport',	'Bus'	,'Other',	'Unknown',	'Railway')

victim$cas_type<- vict_list[victim$row]
victim$strk_type<- strik_list[victim$column]


victim<- subset(victim, select=c("year", "cas_type", "strk_type"))
injury_file<- 'vizag_injuries.csv'

write.csv(vic,paste0('inst/extdata/local/vizag/',injury_file))

