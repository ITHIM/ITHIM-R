setwd('code/injuries/bogota')

icd_codes<- read.csv('icd_code_look_up_table_row_column.csv')

#### Selecting dead road users from Victim file
year<-2017
vic<-paste('victims_bogota_',year,'.csv',sep="")
strike<-paste('striking_bogota_',year,'.csv',sep="")
vic<-read.csv(vic)
strike<-read.csv(strike)
vic$id<- seq(1, nrow(vic))
strike$id<- nrow(vic)+ seq(1, nrow(strike))

vic<-vic[which(vic$GRAVEDAD_PROCESADA=="MUERTA" ),]
vic<-subset(vic,selec=c('Formulario', 'EDAD_PROCESADA','Sexo','VEHICULO_VIAJABA', 'id'))
names(vic)<- c("accident_id", "vic_age", "vic_sex", "victim_vehicle", "victim_id")

vic_strike<-strike[which(strike$GRAVEDAD_PROCESADA=="MUERTA"),]  ## these are the victims from the striking vehicle file
vic_strike<-subset(vic_strike,selec=c('Formulario', 'EDAD_PROCESADA','Sexo','ClaseVehiculo', 'id'))
names(vic_strike)<- c("accident_id", "vic_age", "vic_sex", "victim_vehicle", 'victim_id')

### this is all the fatal victims
victim_all<- rbind(vic, vic_strike)

#reading the victim and striking files agains to get other parties involved in the death of victims
year<-2017
vic<-paste('victims_bogota_',year,'.csv',sep="")
strike<-paste('striking_bogota_',year,'.csv',sep="")
vic<-read.csv(vic)
strike<-read.csv(strike)
vic$id<- seq(1, nrow(vic))
strike$id<- nrow(vic)+ seq(1, nrow(strike))
vic<-subset(vic,selec=c('Formulario', 'EDAD_PROCESADA','Sexo','VEHICULO_VIAJABA', 'id'))
names(vic)<- c("accident_id", "strike_age", "strike_sex", "strike_vehicle", "strike_id")

strike<-subset(strike,selec=c('Formulario', 'EDAD_PROCESADA','Sexo','ClaseVehiculo', 'id'))
names(strike)<- c("accident_id", "strike_age", "strike_sex", "strike_vehicle", 'strike_id')

strike_all<- rbind(vic, strike)
victim_all<- victim_all %>% left_join(strike_all, by ='accident_id')
##accidents with more than one row
x<- victim_all %>% group_by(accident_id) %>% summarise('n'=n())
victim_all <- victim_all %>% left_join(x, by="accident_id")

victim_all<- victim_all[which(!(victim_all$victim_id == victim_all$strike_id & victim_all$n>1)),]

victim_all$strike_age[which(victim_all$victim_id == victim_all$strike_id)]<- NA
victim_all$strike_vehicle[which(victim_all$victim_id == victim_all$strike_id)]<- "NOV"
victim_all$strike_sex[which(victim_all$victim_id == victim_all$strike_id)]<- "NA"
victim_all$strike_id[which(victim_all$victim_id == victim_all$strike_id)]<- NA


vic<- victim_all

vic$vic_vehicl_eng<- "NA"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Buseta")]<-"Bus"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Pasajero")]<-"Unknown"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="PEATON")]<-"Pedestrian"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Peatón")]<-"Pedestrian"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Peaton")]<-"Pedestrian"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Automovil")]<-"Car"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Motocicleta")]<-"MC"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="MOTOCICLETA")]<-"MC"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Bus")]<-"Bus"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="BUS")]<-"Bus"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="TAXI")]<-"Car"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Camioneta")]<-"Van"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="CAMIONETA")]<-"Van"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Campero")]<-"Van"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Microbus")]<-"Bus"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="MICROBUS")]<-"Bus"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Camion, Furgon")]<-"Truck"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="CAMION, FURGON")]<-"Truck"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Volqueta")]<-"Van"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Bicicleta")]<-"Cycle"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Tractocamion")]<-"Bus"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="SIN INFORMATION")]<-"Unknown"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Motocarro")]<-"Car"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="AUTOMOVIL")]<-"Car"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Camion, Furgon Volqueta")]<-"Truck"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Acompañante")]<-"Unknown"

vic$strike_vehicle_eng<- "NA"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Buseta")]<-"Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="BUSETA")]<-"Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="PEATON")]<-"Pedestrian"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Automovil")]<-"Car"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="AUTOMOVIL")]<-"Car"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Automóvil")]<-"Car"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Motocicleta")]<-"MC"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="MOTOCICLETA")]<-"MC"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Bus")]<-"Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="BUS")]<-"Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Bus Articulado")]<-"Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Bus Alimentador")]<-"Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="TAXI")]<-"Car"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Camioneta")]<-"Van"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="CAMIONETA")]<-"Van"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Campero")]<-"Van"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Microbus")]<-"Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Camion, Furgon")]<-"Truck"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Camión, Furgón")]<-"Truck"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Volqueta")]<-"Van"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Bicicleta")]<-"Cycle"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Tractocamion")]<-"Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Tractocamión")]<-"Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="No identificado")]<-"Unknown"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Motocarro")]<-"Car"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Otro")]<-"Other"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Tracción animal")]<-"NMV"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="M. Industrial")]<-"Other"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="NOV")]<-"NOV"

# vic$strike_vehicle_eng[which(vic$strike_vehicle=="null")]<-"Unknown"
vic$strike_vehicle_eng[which(is.na(vic$strike_vehicle_eng))]<-"Unknown"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Camion, Furgon Volqueta")]<-"Truck"

vic<-vic[which(!(vic$vic_vehicl_eng== "Pedestrian" & vic$strike_vehicle_eng=="Pedestrian")),]
vic$strike_victim_id<- paste(vic$victim_id, "_", vic$strike_id)
vic <- arrange(vic, strike_victim_id)
vic <- vic[which(!duplicated(vic$strike_victim_id)),]

##assigning heierarchy to striking vehicles and select only one pair of victim and striking vehicle within a given accidents
hier<- as.data.frame(unique(vic$strike_vehicle_eng))
names(hier)<-"strike_vehicle_eng"
hier$rank<- 0
hier$rank<- c(8,4,1,5, 7,3,6, 2)

vic<- vic %>% left_join(hier, by="strike_vehicle_eng")
x<- vic %>% group_by(victim_id) %>% summarise("max"=max(rank))
vic <- vic %>% left_join(x, by="victim_id")
vic<- vic[which(vic$rank==vic$max),]

vic<- vic[which(!duplicated(vic$victim_id)),]


vic$combo<- paste(vic$vic_vehicl_eng, "-", vic$strike_vehicle_eng)
unique(vic$combo)

lookup<- read.csv('victim-striking-vehicle-pairs-bogota.csv') ## lookup table for unique victim-striking vehicle pairs and their corresponding ICD codes

vic<- vic %>% left_join(lookup,by="combo")

names(vic)[18] <-"ICD"
vic<- vic[,-17]
vic<- vic %>% left_join(icd_codes, by="ICD") ## row and column number of who-hit-who matrix, similar to Brazil analysis


strik_list<- c('No other/fixed or stationary object'	,'Pedestrian',	'Pedal cycle',	'2/3 Wheeled',	'Car/pick-up/van'	,'Bus'	,'Railway train/railway vehicle',	'non-motor vehicle',	'unspecified',	'Non-collision', 'Unknown', 'Trucks')
vict_list<- c('Pedestrian'	,'Pedal Cycle',	'Motorcycle',	'3Wheeled',	'Car'	,'Pick-up truck/van',	'Heavy transport',	'Bus'	,'Other',	'Unknown',	'Railway')

vic$cas_type<- vict_list[vic$row]
vic$strk_type<- strik_list[vic$column]

vic$year<- 2017
vic1<- vic

####Bogota 2016####
### Bogota 2016 and 2015 (next set of code) have been done
### the matrix differs from ITF matrix, need to check if the goods vehicles are being correctly categorised
### in 2016 data there is a column which says heavy load for vans, could that be 
###WHW_matrix in 2015 code is the sum of 2016 and 2015

setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Colombia/Bogota/Injuries')
setwd('C:/Users/goelr/Dropbox/sneha')

#### Selecting dead road users from Victim file
year<-2016
vic<-paste('victims_bogota_',year,'.csv',sep="")
strike<-paste('striking_bogota_',year,'.csv',sep="")
vic<-read.csv(vic)
strike<-read.csv(strike)
vic$id<- seq(1, nrow(vic))
strike$id<- 328+ seq(1, nrow(strike))

vic<-vic[which(vic$GRAVEDAD=="Muerta" | vic$GRAVEDAD=="MUERTA" ),]
vic<-subset(vic,selec=c('Accidente', 'EDAD','Sexo','VIAJABA_EN.', 'id'))
names(vic)<- c("accident_id", "vic_age", "vic_sex", "victim_vehicle", "victim_id")

vic_strike<-strike[which(strike$GRAVEDAD_PROCESADA=="Muerta"),]  ## these are the victims from the striking vehicle file
vic_strike<-subset(vic_strike,selec=c('Accidente', 'EDAD_PROCESADA','Sexo','ClaseVehiculo', 'id'))
names(vic_strike)<- c("accident_id", "vic_age", "vic_sex", "victim_vehicle", 'victim_id')

### this is all the fatal victims
victim_all<- rbind(vic, vic_strike)

#reading the victim and striking files agains to get other parties involved in the death of victims
year<-2016
vic<-paste('victims_bogota_',year,'.csv',sep="")
strike<-paste('striking_bogota_',year,'.csv',sep="")
vic<-read.csv(vic)
strike<-read.csv(strike)
vic$id<- seq(1, nrow(vic))
strike$id<- 328+ seq(1, nrow(strike))
vic<-subset(vic,selec=c('Accidente', 'EDAD','Sexo','VIAJABA_EN.', 'id'))
names(vic)<- c("accident_id", "strike_age", "strike_sex", "strike_vehicle", "strike_id")

strike<-subset(strike,selec=c('Accidente', 'EDAD_PROCESADA','Sexo','ClaseVehiculo', 'id'))
names(strike)<- c("accident_id", "strike_age", "strike_sex", "strike_vehicle", 'strike_id')

strike_all<- rbind(vic, strike)
victim_all<- victim_all %>% left_join(strike_all, by ='accident_id')

##accidents with more than one row
x<- victim_all %>% group_by(accident_id) %>% summarise('n'=n())
victim_all <- victim_all %>% left_join(x, by="accident_id")

victim_all<- victim_all[which(!(victim_all$victim_id == victim_all$strike_id & victim_all$n>1)),]

victim_all$strike_age[which(victim_all$victim_id == victim_all$strike_id)]<- NA
victim_all$strike_vehicle[which(victim_all$victim_id == victim_all$strike_id)]<- "NOV"
victim_all$strike_sex[which(victim_all$victim_id == victim_all$strike_id)]<- "NA"
victim_all$strike_id[which(victim_all$victim_id == victim_all$strike_id)]<- NA

vic<- victim_all

vic$vic_vehicl_eng<- "NA"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Buseta")]<-"Bus"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Pasajero")]<-"Unknown"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="PEATON")]<-"Pedestrian"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Peatón")]<-"Pedestrian"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Peaton")]<-"Pedestrian"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Automovil")]<-"Car"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Motocicleta")]<-"MC"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Bus")]<-"Bus"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="TAXI")]<-"Car"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Camioneta")]<-"Van"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Campero")]<-"Van"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Microbus")]<-"Bus"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Camion, Furgon")]<-"Truck"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Volqueta")]<-"Van"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Bicicleta")]<-"Cycle"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Tractocamion")]<-"Bus"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="SIN INFORMATION")]<-"Unknown"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Motocarro")]<-"Car"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Camion, Furgon Volqueta")]<-"Truck"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Acompañante")]<-"Unknown"

vic$strike_vehicle_eng<- "NA"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Buseta")]<-"Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="PEATON")]<-"Pedestrian"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Automovil")]<-"Car"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Automóvil")]<-"Car"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Motocicleta")]<-"MC"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Bus")]<-"Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Bus Articulado")]<-"Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Bus Alimentador")]<-"Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="TAXI")]<-"Car"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Camioneta")]<-"Van"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Campero")]<-"Van"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Microbus")]<-"Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Camion, Furgon")]<-"Truck"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Camión, Furgón")]<-"Truck"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Volqueta")]<-"Van"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Bicicleta")]<-"Cycle"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Tractocamion")]<-"Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Tractocamión")]<-"Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="No identificado")]<-"Unknown"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Motocarro")]<-"Car"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Otro")]<-"Other"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Tracción animal")]<-"NMV"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="M. Industrial")]<-"Other"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="NOV")]<-"NOV"


# vic$strike_vehicle_eng[which(vic$strike_vehicle=="null")]<-"Unknown"
vic$strike_vehicle_eng[which(is.na(vic$strike_vehicle_eng))]<-"Unknown"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Camion, Furgon Volqueta")]<-"Truck"


##assigning NA as the striking vehicle for cases where pedestrian is the striking vehicle of pedestrian, there is no ICD code for it
vic[which((vic$vic_vehicl_eng== "Pedestrian" & vic$strike_vehicle_eng=="Pedestrian")),]$strike_vehicle_eng<- "NA"
vic$strike_victim_id<- paste(vic$victim_id, "_", vic$strike_id)
vic <- arrange(vic, strike_victim_id)
vic <- vic[which(!duplicated(vic$strike_victim_id)),]

##assigning heierarchy to striking vehicles and select only one pair of victim and striking vehicle within a given accidents
hier<- as.data.frame(unique(vic$strike_vehicle_eng))
names(hier)<-"strike_vehicle_eng"
hier$rank<- 0
hier$rank<- c(8,6,7,5,2,9,1,3,4)


vic<- vic %>% left_join(hier, by="strike_vehicle_eng")
x<- vic %>% group_by(victim_id) %>% summarise("max"=max(rank))
vic <- vic %>% left_join(x, by="victim_id")
vic<- vic[which(vic$rank==vic$max),]

vic<- vic[which(!duplicated(vic$victim_id)),]


vic$combo<- paste(vic$vic_vehicl_eng, "-", vic$strike_vehicle_eng)
unique(vic$combo)

#write.csv(lookup_icd_code,'victim-striking-vehicle-pairs-bogota_2.csv')
lookup<- read.csv('victim-striking-vehicle-pairs-bogota.csv') ## lookup table for unique victim-striking vehicle pairs and their corresponding ICD codes

vic<- vic %>% left_join(lookup,by="combo")

names(vic)[18] <-"ICD"
vic<- vic[,-17]
vic<- vic %>% left_join(icd_codes, by="ICD") ## row and column number of who-hit-who matrix, similar to Brazil analysis


strik_list<- c('No other/fixed or stationary object'	,'Pedestrian',	'Pedal cycle',	'2/3 Wheeled',	'Car/pick-up/van'	,'Bus'	,'Railway train/railway vehicle',	'non-motor vehicle',	'unspecified',	'Non-collision', 'Unknown', 'Trucks')
vict_list<- c('Pedestrian'	,'Pedal Cycle',	'Motorcycle',	'3Wheeled',	'Car'	,'Pick-up truck/van',	'Heavy transport',	'Bus'	,'Other',	'Unknown',	'Railway')

vic$cas_type<- vict_list[vic$row]
vic$strk_type<- strik_list[vic$column]
vic$year<- 2016
vic2<- vic

####Bogota 2015####
#### Selecting dead road users from Victim file
year<-2015
vic<-paste('victims_bogota_',year,'.csv',sep="")
strike<-paste('striking_bogota_',year,'.csv',sep="")
vic<-read.csv(vic)
strike<-read.csv(strike)
vic$id<- seq(1, nrow(vic))
strike$id<- nrow(vic)+ seq(1, nrow(strike))

vic<-vic[which(vic$GRAVEDAD=="Muerta" | vic$GRAVEDAD=="MUERTA" ),]
vic<-subset(vic,selec=c('Accidente', 'EDAD','Sexo','ClaseVehiculo', 'id'))
names(vic)<- c("accident_id", "vic_age", "vic_sex", "victim_vehicle", "victim_id")

vic_strike<-strike[which(strike$GRAVEDAD_PROCESADA=="Muerta"),]  ## these are the victims from the striking vehicle file
vic_strike<-subset(vic_strike,selec=c('Accidente', 'edad','Sexo','ClaseVehiculo', 'id'))
names(vic_strike)<- c("accident_id", "vic_age", "vic_sex", "victim_vehicle", 'victim_id')

### this is all the fatal victims
victim_all<- rbind(vic, vic_strike)

#reading the victim and striking files agains to get other parties involved in the death of victims
year<-2015
vic<-paste('victims_bogota_',year,'.csv',sep="")
strike<-paste('striking_bogota_',year,'.csv',sep="")
vic<-read.csv(vic)
strike<-read.csv(strike)
vic$id<- seq(1, nrow(vic))
strike$id<- nrow(vic)+ seq(1, nrow(strike))
vic<-subset(vic,selec=c('Accidente', 'EDAD','Sexo','ClaseVehiculo', 'id'))
names(vic)<- c("accident_id", "strike_age", "strike_sex", "strike_vehicle", "strike_id")

strike<-subset(strike,selec=c('Accidente', 'edad','Sexo','ClaseVehiculo', 'id'))
names(strike)<- c("accident_id", "strike_age", "strike_sex", "strike_vehicle", 'strike_id')

strike_all<- rbind(vic, strike)
victim_all<- victim_all %>% left_join(strike_all, by ='accident_id')
##accidents with more than one row
x<- victim_all %>% group_by(accident_id) %>% summarise('n'=n())
victim_all <- victim_all %>% left_join(x, by="accident_id")

victim_all<- victim_all[which(!(victim_all$victim_id == victim_all$strike_id & victim_all$n>1)),]

victim_all$strike_age[which(victim_all$victim_id == victim_all$strike_id)]<- NA
victim_all$strike_vehicle[which(victim_all$victim_id == victim_all$strike_id)]<- "NOV"
victim_all$strike_sex[which(victim_all$victim_id == victim_all$strike_id)]<- "NA"
victim_all$strike_id[which(victim_all$victim_id == victim_all$strike_id)]<- NA


vic<- victim_all

vic$vic_vehicl_eng<- "NA"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Buseta")]<-"Bus"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Pasajero")]<-"Unknown"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="PEATON")]<-"Pedestrian"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Peatón")]<-"Pedestrian"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Peaton")]<-"Pedestrian"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Automovil")]<-"Car"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Motocicleta")]<-"MC"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Bus")]<-"Bus"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="TAXI")]<-"Car"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Camioneta")]<-"Van"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Campero")]<-"Van"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Microbus")]<-"Bus"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Camion, Furgon")]<-"Truck"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Volqueta")]<-"Van"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Bicicleta")]<-"Cycle"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Tractocamion")]<-"Bus"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="SIN INFORMATION")]<-"Unknown"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Motocarro")]<-"Car"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Camion, Furgon Volqueta")]<-"Truck"
vic$vic_vehicl_eng[which(vic$victim_vehicle=="Acompañante")]<-"Unknown"

vic$strike_vehicle_eng<- "NA"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Buseta")]<-"Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="PEATON")]<-"Pedestrian"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Automovil")]<-"Car"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Automóvil")]<-"Car"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Motocicleta")]<-"MC"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Bus")]<-"Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Bus Articulado")]<-"Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Bus Alimentador")]<-"Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="TAXI")]<-"Car"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Camioneta")]<-"Van"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Campero")]<-"Van"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Microbus")]<-"Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Camion, Furgon")]<-"Truck"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Camión, Furgón")]<-"Truck"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Volqueta")]<-"Van"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Bicicleta")]<-"Cycle"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Tractocamion")]<-"Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Tractocamión")]<-"Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="No identificado")]<-"Unknown"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Motocarro")]<-"Car"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Otro")]<-"Other"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Tracción animal")]<-"NMV"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="M. Industrial")]<-"Other"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="NOV")]<-"NOV"


# vic$strike_vehicle_eng[which(vic$strike_vehicle=="null")]<-"Unknown"
vic$strike_vehicle_eng[which(is.na(vic$strike_vehicle_eng))]<-"Unknown"
vic$strike_vehicle_eng[which(vic$strike_vehicle=="Camion, Furgon Volqueta")]<-"Truck"

vic[which((vic$vic_vehicl_eng== "Pedestrian" & vic$strike_vehicle_eng=="Pedestrian")),]$strike_vehicle_eng<- "NA"
vic$strike_victim_id<- paste(vic$victim_id, "_", vic$strike_id)
vic <- arrange(vic, strike_victim_id)
vic <- vic[which(!duplicated(vic$strike_victim_id)),]

##assigning hierarchy to striking vehicles and select only one pair of victim and striking vehicle within a given accidents
hier<- as.data.frame(unique(vic$strike_vehicle_eng))
names(hier)<-"strike_vehicle_eng"
hier$rank<- 0
hier$rank<- c(7,9,6,1,8,5,4,2,3)

vic<- vic %>% left_join(hier, by="strike_vehicle_eng")
x<- vic %>% group_by(victim_id) %>% summarise("max"=max(rank))
vic <- vic %>% left_join(x, by="victim_id")
vic<- vic[which(vic$rank==vic$max),]

vic<- vic[which(!duplicated(vic$victim_id)),]


vic$combo<- paste(vic$vic_vehicl_eng, "-", vic$strike_vehicle_eng)
unique(vic$combo)

lookup<- read.csv('victim-striking-vehicle-pairs-bogota.csv') ## lookup table for unique victim-striking vehicle pairs and their corresponding ICD codes

vic<- vic %>% left_join(lookup,by="combo")

names(vic)[18] <-"ICD"
vic<- vic[,-17]
vic<- vic %>% left_join(icd_codes, by="ICD") ## row and column number of who-hit-who matrix, similar to Brazil analysis


strik_list<- c('No other/fixed or stationary object'	,'Pedestrian',	'Pedal cycle',	'2/3 Wheeled',	'Car/pick-up/van'	,'Bus'	,'Railway train/railway vehicle',	'non-motor vehicle',	'unspecified',	'Non-collision', 'Unknown', 'Trucks')
vict_list<- c('Pedestrian'	,'Pedal Cycle',	'Motorcycle',	'3Wheeled',	'Car'	,'Pick-up truck/van',	'Heavy transport',	'Bus'	,'Other',	'Unknown',	'Railway')

vic$cas_type<- vict_list[vic$row]
vic$strk_type<- strik_list[vic$column]
vic$year<- 2015
vic<- rbind(vic, vic2, vic1)

