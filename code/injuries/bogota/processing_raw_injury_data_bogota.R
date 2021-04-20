library(tidyverse)
library(mice)

path <- file.path('code/injuries/bogota/')
icd_codes <- read.csv(paste0(path,'/icd_code_look_up_table_row_column.csv'))

#### Selecting dead road users from Victim file
year <- 2017
vic <- paste(path, '/victims_bogota_', year, '.csv', sep = "")
strike <- paste(path,'/striking_bogota_', year, '.csv', sep = "")
vic <- read.csv(vic)
strike <- read.csv(strike)
vic$id <- seq(1, nrow(vic))
strike$id <- nrow(vic) + seq(1, nrow(strike))

vic <- vic[which(vic$GRAVEDAD_PROCESADA == "MUERTA" ),]
vic <- subset(vic, select = c('Formulario', 'EDAD_PROCESADA','Sexo',
                              'VEHICULO_VIAJABA', 'id'))
names(vic) <- c("accident_id", "vic_age", "vic_sex", "victim_vehicle", "victim_id")

vic_strike <- strike[which(strike$GRAVEDAD_PROCESADA == "MUERTA"),]  ## these are the victims from the striking vehicle file
vic_strike <- subset(vic_strike, select = c('Formulario', 'EDAD_PROCESADA',
                                            'Sexo','ClaseVehiculo', 'id'))
names(vic_strike) <- c("accident_id", "vic_age", "vic_sex", "victim_vehicle",
                       'victim_id')

### this is all the fatal victims
victim_all <- rbind(vic, vic_strike)

#reading the victim and striking files agains to get other parties involved in the death of victims
year <- 2017
vic <- paste(path, '/victims_bogota_', year, '.csv', sep = "")
strike <- paste(path, '/striking_bogota_', year, '.csv', sep = "")
vic <- read.csv(vic)
strike <- read.csv(strike)
vic$id <- seq(1, nrow(vic))
strike$id <- nrow(vic) + seq(1, nrow(strike))
vic <- subset(vic, select = c('Formulario', 'EDAD_PROCESADA','Sexo',
                              'VEHICULO_VIAJABA', 'id'))
names(vic) <- c("accident_id", "strike_age", "strike_sex", "strike_vehicle",
                "strike_id")

strike <- subset(strike, select = c('Formulario', 'EDAD_PROCESADA','Sexo',
                                    'ClaseVehiculo', 'id'))
names(strike) <- c("accident_id", "strike_age", "strike_sex", "strike_vehicle", 'strike_id')

strike_all <- rbind(vic, strike)
victim_all <- victim_all %>% left_join(strike_all, by = 'accident_id')
##accidents with more than one row
x <- victim_all %>% group_by(accident_id) %>% summarise('n' = n())
victim_all <- victim_all %>% left_join(x, by = "accident_id")

victim_all <- victim_all[which(!(victim_all$victim_id == victim_all$strike_id &
                                   victim_all$n > 1)),]

victim_all$strike_age[which(victim_all$victim_id == victim_all$strike_id)] <- NA
victim_all$strike_vehicle[which(victim_all$victim_id == victim_all$strike_id)] <-  "NOV"
victim_all$strike_sex[which(victim_all$victim_id == victim_all$strike_id)] <- "NA"
victim_all$strike_id[which(victim_all$victim_id == victim_all$strike_id)] <- NA


vic <- victim_all

vic$vic_vehicl_eng <- "NA"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Buseta")] <- "Bus"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Pasajero")] <- "Unknown"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "PEATON")] <- "Pedestrian"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Peat?n")] <- "Pedestrian"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Peaton")] <- "Pedestrian"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Automovil")] <- "Car"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Motocicleta")] <- "MC"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "MOTOCICLETA")] <- "MC"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Bus")] <- "Bus"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "BUS")] <- "Bus"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "TAXI")] <- "Car"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Camioneta")] <- "Van"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "CAMIONETA")] <- "Van"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Campero")] <- "Van"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Microbus")] <- "Bus"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "MICROBUS")] <- "Bus"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Camion, Furgon")] <- "Truck"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "CAMION, FURGON")] <- "Truck"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Volqueta")] <- "Van"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Bicicleta")] <- "Cycle"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Tractocamion")] <- "Bus"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "SIN INFORMATION")] <- "Unknown"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Motocarro")] <- "Car"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "AUTOMOVIL")] <- "Car"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Camion, Furgon Volqueta")] <- "Truck"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Acompa?ante")] <- "Unknown"

vic$strike_vehicle_eng <- "NA"
vic$strike_vehicle_eng[which(vic$strike_vehicle  == "Buseta")] <- "Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "BUSETA")] <- "Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "PEATON")] <- "Pedestrian"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Automovil")] <- "Car"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "AUTOMOVIL")] <- "Car"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Autom?vil")] <- "Car"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Motocicleta")] <- "MC"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "MOTOCICLETA")] <- "MC"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Bus")] <- "Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "BUS")] <- "Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Bus Articulado")] <- "Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Bus Alimentador")] <- "Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "TAXI")] <- "Car"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Camioneta")] <- "Van"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "CAMIONETA")] <- "Van"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Campero")] <- "Van"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Microbus")] <- "Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Camion, Furgon")] <- "Truck"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Cami?n, Furg?n")] <- "Truck"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Volqueta")] <- "Van"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Bicicleta")] <- "Cycle"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Tractocamion")] <- "Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Tractocami?n")] <- "Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "No identificado")] <- "Unknown"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Motocarro")] <- "Car"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Otro")] <- "Other"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Tracci?n animal")] <- "NMV"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "M. Industrial")] <- "Other"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "NOV")] <- "NOV"

# vic$strike_vehicle_eng[which(vic$strike_vehicle == "null")] <- "Unknown"
vic$strike_vehicle_eng[which(is.na(vic$strike_vehicle_eng))] <- "Unknown"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Camion, Furgon Volqueta")] <- "Truck"

vic <- vic[which(!(vic$vic_vehicl_eng == "Pedestrian" & vic$strike_vehicle_eng == "Pedestrian")),]
vic$strike_victim_id <- paste(vic$victim_id, "_", vic$strike_id)
vic <- arrange(vic, strike_victim_id)
vic <- vic[which(!duplicated(vic$strike_victim_id)),]

##assigning heierarchy to striking vehicles and select only one pair of victim and striking vehicle within a given accidents
hier <- as.data.frame(unique(vic$strike_vehicle_eng))
names(hier) <- "strike_vehicle_eng"
hier$rank <- 0
hier$rank <- c(8,4,1,5,7,3,6,2)

vic <- vic %>% left_join(hier, by = "strike_vehicle_eng")
x <- vic %>% group_by(victim_id) %>% summarise("max" = max(rank))
vic <- vic %>% left_join(x, by = "victim_id")
vic <- vic[which(vic$rank == vic$max),]

vic <- vic[which(!duplicated(vic$victim_id)),]


vic$combo <- paste(vic$vic_vehicl_eng, "-", vic$strike_vehicle_eng)
unique(vic$combo)

lookup <- read.csv(paste0(path,'/victim-striking-vehicle-pairs-bogota.csv')) ## lookup table for unique victim-striking vehicle pairs and their corresponding ICD codes

vic <- vic %>% left_join(lookup,by = "combo")

names(vic)[18] <- "ICD"
vic <- vic[,-17]
vic <- vic %>% left_join(icd_codes, by = "ICD") ## row and column number of who-hit-who matrix, similar to Brazil analysis


strik_list <- c('No other/fixed or stationary object'	,'Pedestrian',	
                'Pedal cycle', '2/3 Wheeled',	'Car/pick-up/van', 'Bus', 
                'Railway train/railway vehicle', 'non-motor vehicle',	
                'unspecified', 'Non-collision', 'Unknown', 'Trucks')
vict_list <- c('Pedestrian', 'Pedal Cycle',	'Motorcycle',	'3Wheeled',	'Car',
               'Pick-up truck/van',	'Heavy transport', 'Bus', 'Other', 
               'Unknown',	'Railway')

vic$cas_type <- vict_list[vic$row]
vic$strk_type <- strik_list[vic$column]

vic$year <- 2017
vic1 <- vic

####Bogota 2016####
### Bogota 2016 and 2015 (next set of code) have been done
### the matrix differs from ITF matrix, need to check if the goods vehicles are being correctly categorised
### in 2016 data there is a column which says heavy load for vans, could that be 
###WHW_matrix in 2015 code is the sum of 2016 and 2015

#### Selecting dead road users from Victim file
year <- 2016
vic <- paste(path, '/victims_bogota_',year,'.csv',sep = "")
strike <- paste(path,'/striking_bogota_',year,'.csv',sep = "")
vic <- read.csv(vic)
strike <- read.csv(strike)
vic$id <- seq(1, nrow(vic))
strike$id <- 328 + seq(1, nrow(strike))

vic <- vic[which(vic$GRAVEDAD == "Muerta" | vic$GRAVEDAD == "MUERTA" ),]
vic <- subset(vic, select = c('Accidente', 'EDAD','Sexo','VIAJABA_EN.', 'id'))
names(vic) <- c("accident_id", "vic_age", "vic_sex", "victim_vehicle", "victim_id")

vic_strike <- strike[which(strike$GRAVEDAD_PROCESADA == "Muerta"),]  ## these are the victims from the striking vehicle file
vic_strike <- subset(vic_strike,select = c('Accidente', 'EDAD_PROCESADA',
                                           'Sexo','ClaseVehiculo', 'id'))
names(vic_strike) <- c("accident_id", "vic_age", "vic_sex", "victim_vehicle",
                       'victim_id')

### this is all the fatal victims
victim_all <- rbind(vic, vic_strike)

#reading the victim and striking files agains to get other parties involved in the death of victims
year <- 2016
vic <- paste(path, '/victims_bogota_',year,'.csv',sep = "")
strike <- paste(path,'/striking_bogota_',year,'.csv',sep = "")
vic <- read.csv(vic)
strike <- read.csv(strike)
vic$id <- seq(1, nrow(vic))
strike$id <- 328 + seq(1, nrow(strike))
vic <- subset(vic, select = c('Accidente', 'EDAD','Sexo','VIAJABA_EN.', 'id'))
names(vic) <- c("accident_id", "strike_age", "strike_sex", "strike_vehicle",
                "strike_id")

strike <- subset(strike, select = c('Accidente', 'EDAD_PROCESADA','Sexo',
                                    'ClaseVehiculo', 'id'))
names(strike) <- c("accident_id", "strike_age", "strike_sex", "strike_vehicle",
                   'strike_id')

strike_all <- rbind(vic, strike)
victim_all <- victim_all %>% left_join(strike_all, by = 'accident_id')

##accidents with more than one row
x <- victim_all %>% group_by(accident_id) %>% summarise('n'=n())
victim_all <- victim_all %>% left_join(x, by="accident_id")

victim_all<- victim_all[which(!(victim_all$victim_id == victim_all$strike_id & victim_all$n>1)),]

victim_all$strike_age[which(victim_all$victim_id == victim_all$strike_id)] <-  NA
victim_all$strike_vehicle[which(victim_all$victim_id == victim_all$strike_id)] <-  "NOV"
victim_all$strike_sex[which(victim_all$victim_id == victim_all$strike_id)] <-  "NA"
victim_all$strike_id[which(victim_all$victim_id == victim_all$strike_id)] <-  NA

vic<- victim_all

vic$vic_vehicl_eng<- "NA"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Buseta")] <- "Bus"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Pasajero")] <- "Unknown"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "PEATON")] <- "Pedestrian"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Peat?n")] <- "Pedestrian"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Peaton")] <- "Pedestrian"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Automovil")] <- "Car"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Motocicleta")] <- "MC"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Bus")] <- "Bus"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "TAXI")] <- "Car"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Camioneta")] <- "Van"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Campero")] <- "Van"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Microbus")] <- "Bus"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Camion, Furgon")] <- "Truck"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Volqueta")] <- "Van"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Bicicleta")] <- "Cycle"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Tractocamion")] <- "Bus"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "SIN INFORMATION")] <- "Unknown"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Motocarro")] <- "Car"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Camion, Furgon Volqueta")] <- "Truck"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Acompa?ante")] <- "Unknown"

vic$strike_vehicle_eng<- "NA"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Buseta")] <- "Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "PEATON")] <- "Pedestrian"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Automovil")] <- "Car"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Autom?vil")] <- "Car"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Motocicleta")] <- "MC"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Bus")] <- "Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Bus Articulado")] <- "Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Bus Alimentador")] <- "Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "TAXI")] <- "Car"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Camioneta")] <- "Van"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Campero")] <- "Van"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Microbus")] <- "Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Camion, Furgon")] <- "Truck"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Cami?n, Furg?n")] <- "Truck"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Volqueta")] <- "Van"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Bicicleta")] <- "Cycle"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Tractocamion")] <- "Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Tractocami?n")] <- "Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "No identificado")] <- "Unknown"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Motocarro")] <- "Car"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Otro")] <- "Other"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Tracci?n animal")] <- "NMV"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "M. Industrial")] <- "Other"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "NOV")] <- "NOV"


# vic$strike_vehicle_eng[which(vic$strike_vehicle == "null")] <- "Unknown"
vic$strike_vehicle_eng[which(is.na(vic$strike_vehicle_eng))] <- "Unknown"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Camion, Furgon Volqueta")] <- "Truck"


##assigning NA as the striking vehicle for cases where pedestrian is the striking vehicle of pedestrian, there is no ICD code for it
vic[which((vic$vic_vehicl_eng== "Pedestrian" & vic$strike_vehicle_eng == "Pedestrian")),]$strike_vehicle_eng<- "NA"
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
vic<-paste(path, '/victims_bogota_',year,'.csv',sep="")
strike<-paste(path,'/striking_bogota_',year,'.csv',sep="")
vic<-read.csv(vic)
strike<-read.csv(strike)
vic$id<- seq(1, nrow(vic))
strike$id<- nrow(vic)+ seq(1, nrow(strike))

vic<-vic[which(vic$GRAVEDAD == "Muerta" | vic$GRAVEDAD == "MUERTA" ),]
vic<-subset(vic,selec=c('Accidente', 'EDAD','Sexo','ClaseVehiculo', 'id'))
names(vic)<- c("accident_id", "vic_age", "vic_sex", "victim_vehicle", "victim_id")

vic_strike<-strike[which(strike$GRAVEDAD_PROCESADA == "Muerta"),]  ## these are the victims from the striking vehicle file
vic_strike<-subset(vic_strike,selec=c('Accidente', 'edad','Sexo','ClaseVehiculo', 'id'))
names(vic_strike)<- c("accident_id", "vic_age", "vic_sex", "victim_vehicle", 'victim_id')

### this is all the fatal victims
victim_all<- rbind(vic, vic_strike)

#re-reading the victim and striking files agains to get other parties involved in the death of victims
year<-2015
vic<-paste(path, '/victims_bogota_',year,'.csv',sep="")
strike<-paste(path,'/striking_bogota_',year,'.csv',sep="")
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

victim_all$strike_age[which(victim_all$victim_id == victim_all$strike_id)] <-  NA
victim_all$strike_vehicle[which(victim_all$victim_id == victim_all$strike_id)] <-  "NOV"
victim_all$strike_sex[which(victim_all$victim_id == victim_all$strike_id)] <-  "NA"
victim_all$strike_id[which(victim_all$victim_id == victim_all$strike_id)] <-  NA


vic<- victim_all

vic$vic_vehicl_eng<- "NA"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Buseta")] <- "Bus"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Pasajero")] <- "Unknown"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "PEATON")] <- "Pedestrian"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Peat?n")] <- "Pedestrian"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Peaton")] <- "Pedestrian"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Automovil")] <- "Car"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Motocicleta")] <- "MC"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Bus")] <- "Bus"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "TAXI")] <- "Car"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Camioneta")] <- "Van"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Campero")] <- "Van"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Microbus")] <- "Bus"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Camion, Furgon")] <- "Truck"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Volqueta")] <- "Van"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Bicicleta")] <- "Cycle"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Tractocamion")] <- "Bus"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "SIN INFORMATION")] <- "Unknown"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Motocarro")] <- "Car"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Camion, Furgon Volqueta")] <- "Truck"
vic$vic_vehicl_eng[which(vic$victim_vehicle == "Acompa?ante")] <- "Unknown"

vic$strike_vehicle_eng<- "NA"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Buseta")] <- "Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "PEATON")] <- "Pedestrian"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Automovil")] <- "Car"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Autom?vil")] <- "Car"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Motocicleta")] <- "MC"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Bus")] <- "Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Bus Articulado")] <- "Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Bus Alimentador")] <- "Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "TAXI")] <- "Car"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Camioneta")] <- "Van"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Campero")] <- "Van"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Microbus")] <- "Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Camion, Furgon")] <- "Truck"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Cami?n, Furg?n")] <- "Truck"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Volqueta")] <- "Van"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Bicicleta")] <- "Cycle"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Tractocamion")] <- "Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Tractocami?n")] <- "Bus"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "No identificado")] <- "Unknown"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Motocarro")] <- "Car"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Otro")] <- "Other"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Tracci?n animal")] <- "NMV"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "M. Industrial")] <- "Other"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "NOV")] <- "NOV"


# vic$strike_vehicle_eng[which(vic$strike_vehicle == "null")] <- "Unknown"
vic$strike_vehicle_eng[which(is.na(vic$strike_vehicle_eng))] <- "Unknown"
vic$strike_vehicle_eng[which(vic$strike_vehicle == "Camion, Furgon Volqueta")] <- "Truck"

vic[which((vic$vic_vehicl_eng== "Pedestrian" & vic$strike_vehicle_eng == "Pedestrian")),]$strike_vehicle_eng<- "NA"
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

##adding weight variable to reflect 
vic$weight<-3
vic<- subset(vic, select=c("vic_age", "vic_sex", "cas_type", "strk_type", "year", "weight"))
names(vic)[1:2] <- c("cas_age", "cas_sex")
vic$cas_sex[which(vic$cas_sex == "FEMENINO")] <- "female"
vic$cas_sex[which(vic$cas_sex == "MASCULINO")] <- "male"
vic$cas_sex[which(vic$cas_sex == "NO APLICA")] <- "NA"

# Multiple imputation using MICE
# https://stats.idre.ucla.edu/r/faq/how-do-i-perform-multiple-imputation-using-predictive-mean-matching-in-r/

# Read lookup table
smodes <- read_csv('data/global/modes/standardized_modes.csv')
# Separate rows 
smodes <- smodes %>% separate_rows(original, sep = ';')
# Trim
smodes <- smodes %>% mutate(across(where(is.character), str_trim))

# names(vic)
# unique(vic$cas_age)
# unique(vic$cas_sex)
# unique(vic$cas_type)
# unique(vic$strk_type)
# unique(vic$year)
# unique(vic$weight)

# Transforming "unknown" and "unspecified" to NA
vic2 <- vic %>% 
  mutate(cas_age = as.numeric(cas_age),
         cas_gender = factor(ifelse(cas_sex %in% c("female", "male"), 
                                    cas_sex, NA)),
         cas_mode = factor(ifelse(cas_type == "Unknown", NA, cas_type)),
         strike_mode = factor(ifelse(strk_type %in% c("Unknown", "unspecified"),
                                     NA, strk_type)))

#table(vic2$cas_sex, vic2$cas_gender, useNA = "always")
#table(vic2$strk_type, vic2$strike_mode, useNA = "always")

# There are 115 missing values in cas_age, 128 in cas_gender and 147 in strike mode
md.pattern(vic2)

# There are 115 rows with cas_age and cas_gender missing 
md.pairs(vic2)

# Imputation using mice. I imputed the dataset 5 times (this is why is multiple
# imputation). The idea is to make a sensitivity analysis in the results.
imp1 <- mice(vic2[,c("year","cas_age","cas_gender","cas_mode","strike_mode")], 
             m = 5, seed = 12345)
imp1

# Adding imputed rows to Bogota's dataset. The original columns (with missing
# values) are kept in the dataset with sufix "_original". And the first run of
# imputation is saved in cas_mode and strike_mode. From second to fifth
# imputation are also saved with the sufix "_2nd", to "5th".
vic3 <- vic2 %>% 
  rename(cas_age_original = cas_age,
         cas_gender_original = cas_gender, 
         #cas_mode_original = cas_mode, # cas_modes doesn't have missing values
         strike_mode_original = strike_mode) %>% 
  bind_cols(complete(imp1) %>% select(cas_age, cas_gender, strike_mode)) %>%
  bind_cols(complete(imp1, action = 2) %>% 
              select(cas_age, cas_gender, strike_mode) %>% 
              rename(cas_age_2nd = cas_age, cas_gender_2nd = cas_gender,
                     strike_mode_2nd = strike_mode)) %>%
  bind_cols(complete(imp1, action = 3) %>% 
              select(cas_age, cas_gender, strike_mode) %>% 
              rename(cas_age_3rd = cas_age, cas_gender_3rd = cas_gender,
                     strike_mode_3rd = strike_mode)) %>%   
  bind_cols(complete(imp1, action = 4) %>% 
              select(cas_age, cas_gender, strike_mode) %>% 
              rename(cas_age_4th = cas_age, cas_gender_4th = cas_gender,
                     strike_mode_4th = strike_mode)) %>%
  bind_cols(complete(imp1, action = 5) %>% 
              select(cas_age, cas_gender, strike_mode) %>% 
              rename(cas_age_5th = cas_age, cas_gender_5th = cas_gender,
                     strike_mode_5th = strike_mode))

# Comparing imputations 
# View(table(vic3$cas_age_original, vic3$cas_age, useNA = "always"))
# table(vic3$cas_gender_original, vic3$cas_gender, useNA = "always")
# table(vic3$strike_mode_original, vic3$strike_mode, useNA = "always")
# table(vic3$strike_mode_original, vic3$strike_mode_2nd, useNA = "always")

# Recode cas_mode and strike_mode
whw <- vic3 %>% 
  mutate(cas_mode = smodes$exhaustive_list[match(tolower(cas_mode),
                                                 smodes$original)],
         strike_mode = smodes$exhaustive_list[match(tolower(strike_mode),
                                                    smodes$original)],
         strike_mode_2nd = smodes$exhaustive_list[match(tolower(strike_mode_2nd),
                                                        smodes$original)],
         strike_mode_3rd = smodes$exhaustive_list[match(tolower(strike_mode_3rd),
                                                        smodes$original)],
         strike_mode_4th = smodes$exhaustive_list[match(tolower(strike_mode_4th),
                                                        smodes$original)],
         strike_mode_5th = smodes$exhaustive_list[match(tolower(strike_mode_5th),
                                                        smodes$original)])

# Comparing frequencies after recoding
table(vic3$cas_mode, useNA = "always")
table(whw$cas_mode, useNA = "always")
table(vic3$strike_mode, useNA = "always")
table(whw$strike_mode, useNA = "always")

# Check if all modes are correctly recoded
unique(whw$cas_mode) %in% smodes$exhaustive_list
unique(whw$strike_mode) %in% smodes$exhaustive_list
unique(whw$strike_mode_2nd) %in% smodes$exhaustive_list
unique(whw$strike_mode_3rd) %in% smodes$exhaustive_list
unique(whw$strike_mode_4th) %in% smodes$exhaustive_list
unique(whw$strike_mode_5th) %in% smodes$exhaustive_list


injury_file <- 'bogota_injuries.csv'
write.csv(whw, paste0('inst/extdata/local/bogota/', injury_file))

