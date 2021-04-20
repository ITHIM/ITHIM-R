#####Bangalore, India######
library(tidyverse)
library(mice)

path <- 'code/injuries/bangalore'
bangalore <- read.csv(paste0(path, '/bengaluru_city_2011.csv'))
head(bangalore)
bangalore <- subset(bangalore, select = c('Accident_Index', 'Accident_Severity',
                                          'Number_of_Casualties',
                                          'vic_Vehicle_Type','vic_Age',
                                          'vic_sex','strik_Vehicle_Type'))
unique(bangalore$vic_Vehicle_Type)
bangalore$vic_vehicl_eng <- 'NA'
bangalore$vic_vehicl_eng[which(bangalore$vic_Vehicle_Type == "2W")] <- "MC"
bangalore$vic_vehicl_eng[which(bangalore$vic_Vehicle_Type == "PED")] <- "Pedestrian"
bangalore$vic_vehicl_eng[which(bangalore$vic_Vehicle_Type == "CAR")] <- "Car"
bangalore$vic_vehicl_eng[which(bangalore$vic_Vehicle_Type == "2W PILLION")] <- "MC"
bangalore$vic_vehicl_eng[which(bangalore$vic_Vehicle_Type == "2W-ACTIVA")] <- "MC"
bangalore$vic_vehicl_eng[which(bangalore$vic_Vehicle_Type == "CYCLIST")] <- "Cycle"
bangalore$vic_vehicl_eng[which(bangalore$vic_Vehicle_Type == "2WPILLION RIDER")] <- "MC"
bangalore$vic_vehicl_eng[which(bangalore$vic_Vehicle_Type == "3W")] <- "3W"
bangalore$vic_vehicl_eng[which(bangalore$vic_Vehicle_Type == "TEMPO")] <- "3W"
bangalore$vic_vehicl_eng[which(bangalore$vic_Vehicle_Type == "TRACTOR")] <- "Other"
bangalore$vic_vehicl_eng[which(bangalore$vic_Vehicle_Type == "TANG VEHICLE/ TROLL PULLED BY HORSE")] <- "Other"
bangalore$vic_vehicl_eng[which(bangalore$vic_Vehicle_Type == "BUS PASSENGER")] <- "Bus"
bangalore$vic_vehicl_eng[which(bangalore$vic_Vehicle_Type == "BUS(KSRTC)")] <- "Bus"
bangalore$vic_vehicl_eng[which(bangalore$vic_Vehicle_Type == "LORRY")] <- "Truck"
bangalore$vic_vehicl_eng[which(bangalore$vic_Vehicle_Type == "BUS")] <- "Bus"
bangalore$vic_vehicl_eng[which(bangalore$vic_Vehicle_Type == "2W (PILLION RIDER)")] <- "MC"
bangalore$vic_vehicl_eng[which(bangalore$vic_Vehicle_Type == "LCV")] <- "Truck"
bangalore$vic_vehicl_eng[which(bangalore$vic_Vehicle_Type == "")] <- "Unknown"
bangalore$vic_vehicl_eng[which(bangalore$vic_Vehicle_Type=="BUS(BMTC)")]<-"Bus"
bangalore$vic_vehicl_eng[which(bangalore$vic_Vehicle_Type=="JEEP")]<-"Pick-up truck/van"
bangalore$vic_vehicl_eng[which(bangalore$vic_Vehicle_Type=="TRACTOR PASSENGER")]<-"Other"
bangalore$vic_vehicl_eng[which(bangalore$vic_Vehicle_Type=="LORRY PASSENGER")]<-"Truck"
bangalore$vic_vehicl_eng[which(bangalore$vic_Vehicle_Type=="CANTER")]<-"Truck"
bangalore$vic_vehicl_eng[which(bangalore$vic_Vehicle_Type=="3W PSSENGER")]<-"3W"
bangalore$vic_vehicl_eng[which(bangalore$vic_Vehicle_Type=="LCV PASSENGER")]<-"Truck"
bangalore$vic_vehicl_eng[which(bangalore$vic_Vehicle_Type=="3W PASSENGER")]<-"3W"

bangalore$strik_vehicl_eng<- 'NA'
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="TEMPO")]<-"3W"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="BMTC- BUS")]<-"Bus"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="AUTO")]<-"3W"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="2W")]<-"MC"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="2W ")]<-"MC"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="LORRY")]<-"Truck"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="CAR")]<-"Car"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="UNKNOWN")]<-"Unknown"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="TAXI CAR")]<-"Car"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="CAR/JEEP")]<-"Car"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="BUS(BMTC)")]<-"Bus"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="BUS (BMTC)")]<-"Bus"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="BUS(PRIVATE)")]<-"Bus"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="BUS(BMTC BUS)")]<-"Bus"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="3W")]<-"3W"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="MARUTI VAN")]<-"Car"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="TIPPER LORRY")]<-"Truck"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="TIPPER LORRY ")]<-"Truck"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="UNKWN")]<-"Unknown"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="BUS(KSRTC)")]<-"Bus"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="MAXI CAB")]<-"Car"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="UNKN")]<-"Unknown"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="LCV")]<-"Truck"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="BUS")]<-"Bus"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="BUS ")]<-"Bus"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="CANTER")]<-"Truck"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="MARUTHI VAN")]<-"Car"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="UNKNW")]<-"Unknown"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="SELF ACCIDENT")]<-"Self"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="PED")]<-"Pedestrian"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="TRACTOR")]<-"Other"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="LORRY(CONCRETE MIXER)")]<-"Truck"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="GOODS VEHICLE")]<-"Truck"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="TRACTOR TROLLY")]<-"Other"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="TRACTOR WATER TANKER")]<-"Other"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="MOBILE CRANE")]<-"Other"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="UNKNWN")]<-"Unknown"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="TANKER LORRY")]<-"Truck"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="BUS(PVT)")]<-"Bus"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="TRAILOR LORRY")]<-"Other"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="SCORPIO CAR")]<-"Car"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="TANKER/MULTI AXLE GOODS VEHICLE")]<-"Truck"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="GAS TANKER")]<-"Other"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="CAR(TAXI")]<-"Car"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="MINI LORRY")]<-"Truck"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="LOORY")]<-"Truck"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="SKIDDING")]<-"Self"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="MINI VAN")]<-"Van"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="MINI BUS")]<-"Bus"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="ROAD TURN")]<-"Self"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="JEEP")]<-"Car"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="UNKNN")]<-"Unknown"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="MILITARY TRUCK")]<-"Truck"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="TRUCK")]<-"Truck"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="BUSBMTC)")]<-"Bus"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="BUS(SCHOOL)")]<-"Bus"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="DRUNKEN DRIVING, SELF ACCIDENT")]<-"Self"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="JCB")]<-"Other"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="CYCLIST")]<-"Cycle"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="AMBULANCE")]<-"Van"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="SELF ACCIDENT, WHILE TURING")]<-"Self"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="BUS(bmtc)")]<-"Bus"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="VAN")]<-"Van"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="")]<-"Unknown"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="MOBILE CRANE ")]<-"Other"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="SCHOOL VAN")]<-"Van"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="CAR(TAXI)")]<-"Car"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="GAS TANKER ")]<-"Other"
bangalore$strik_vehicl_eng[which(bangalore$strik_Vehicle_Type=="TANKER")]<-"Other"
bangalore$strik_vehicl_eng[which(bangalore$strik_vehicl_eng=='NA')] <- "Stationary object"

bangalore<-bangalore[-c(181),]

bangalore$combo<- paste(bangalore$vic_vehicl_eng, "-", bangalore$strik_vehicl_eng)
#x<-unique(bangalore$combo)
#write.csv(x,'victim-striking-vehicle-unique-pairs.csv')
lookup<- read.csv('victim-striking-vehicle-unique-pairs-new.csv') ## lookup table for unique victim-striking vehicle pairs and their corresponding ICD codes
bangalore<- bangalore %>% left_join(lookup,by="combo")
icd_codes<- read.csv(paste0(path, '/icd_code_look_up_table_row_column.csv'))
bangalore<- bangalore %>% left_join(icd_codes,by="ICD")

strik_list<- c('No other/fixed or stationary object'	,'Pedestrian',	'Pedal cycle',	'2/3 Wheeled',	'Car/pick-up/van'	,'Bus'	,'Railway train/railway vehicle',	'non-motor vehicle',	'unspecified',	'Non-collision', 'Unknown', 'Trucks')
vict_list<- c('Pedestrian'	,'Pedal Cycle',	'Motorcycle',	'3Wheeled',	'Car'	,'Pick-up truck/van',	'Heavy transport',	'Bus'	,'Other',	'Unknown',	'Railway')
bangalore$cas_type<- vict_list[bangalore$row]
bangalore$strk_type<- strik_list[bangalore$column]
bangalore<- bangalore[which(bangalore$Accident_Severity=="FATAL"),]
bangalore<- subset(bangalore, select=c("vic_Age", "vic_sex", "cas_type", "strk_type"))
names(bangalore)[1:2]<-c("vic_age", "vic_sex")
injury_file <- 'injuries_bangalore.csv'
write.csv(bangalore,paste0('inst/extdata/local/bangalore/',injury_file))
