###Delhi, India######
library(tidyverse)
library(mice)

path<- 'code/injuries/delhi'
delhi1316<-read.csv(paste0(path,'/delhi_2013-2016.csv'))
delhi1012<-read.csv(paste0(path,'/delhi_2010-2012.csv'))
delhi0609<-read.csv(paste0(path,'/delhi_2006-2009.csv'))
delhi2005<-read.csv(paste0(path,'/delhi_2005.csv'))
delhi2004<-read.csv(paste0(path,'/delhi_2004.csv'))
delhi2003<-read.csv(paste0(path,'/delhi_2003.csv'))
delhi2002<-read.csv(paste0(path,'/delhi_2002.csv'))
delhi2001<-read.csv(paste0(path,'/delhi_2001.csv'))

str(delhi2002)

delhi2001$year<- as.numeric(substr(delhi2001$DATE_OCC,8,9))
delhi2001$year2<-as.numeric(paste(200, delhi2001$year, sep=""))
delhi2001<- delhi2001[which(delhi2001$KILLED>0),]
delhi2001<- subset(delhi2001, select=c('year2','VEHTYPE1','VEHTYPE2'))
names(delhi2001)<- c('year','striking_vehicle','victim_vehicle')


delhi2002$year<- as.numeric(substr(delhi2002$DATE_OCC,8,9))
delhi2002$year2<-as.numeric(paste(200, delhi2002$year, sep=""))
delhi2002<- delhi2002[which(delhi2002$KILLED>0),]
delhi2002<- subset(delhi2002, select=c('year2','VEHTYPE1','VEHTYPE2'))
names(delhi2002)<- c('year','striking_vehicle','victim_vehicle')

delhi2003$year<- as.numeric(substr(delhi2003$DATE_OCC,7,10))
delhi2003<- delhi2003[which(delhi2003$KILLED>0),]
delhi2003<- subset(delhi2003, select=c('year','VEHTYPE1','VEHTYPE2'))
names(delhi2003)<- c('year','striking_vehicle','victim_vehicle')

delhi2004$year<- as.numeric(substr(delhi2004$DATE_OCC,7,10))
delhi2004<- delhi2004[which(delhi2004$KILLED>0),]
delhi2004<- subset(delhi2004, select=c('year','VEHTYPE1','VEHTYPE2'))
names(delhi2004)<- c('year','striking_vehicle','victim_vehicle')

delhi2005<- delhi2005[which(delhi2005$KILLED>0),]
delhi2005<- subset(delhi2005, select=c('year','VEHTYPE1','VEHTYPE2'))
names(delhi2005)<- c('year','striking_vehicle','victim_vehicle')

## Delhi 2006 to 2009
delhi0609<- delhi0609[which(delhi0609$KILLED>0),]
delhi0609<- subset(delhi0609, select=c('YEAR','VEHDETL','VICTIM'))
names(delhi0609)<- c('year','striking_vehicle','victim_vehicle')

delhi1012<- subset(delhi1012, select=c('Year','VEHICLEATFAULT','VICTIM'))
names(delhi1012)<- c('year','striking_vehicle','victim_vehicle')

delhi1316<- subset(delhi1316, select=c('year','OFFENDING.VEHICLE','VICTIMS'))
names(delhi1316)<- c('year','striking_vehicle','victim_vehicle')

delhi<- rbind(delhi2001, delhi2002, delhi2003,delhi2004, delhi2005,delhi0609, delhi1012,delhi1316)

unique(delhi$victim_vehicle)

delhi$strik_vehicl_eng<-NA
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="BUS")]<-"Bus"
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="TSR")]<-"3W"
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="CAR")]<-"Car"
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="UNK")]<-"Unknown"
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="TMP")]<-"3W"
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="UDT")]<-"Unknown"
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="HTV")]<-"Truck"
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="TWW")]<-"2W"
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="BLB")]<-"Unknown"
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="TRC")]<-"Other"  ##assuming tractor
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="OSB")]<-"Other"  ##need to confirm
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="MIL")]<-"Truck"
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="TNK")]<-"Truck"
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="DLV")]<-"Car"  ##delivery vehicle
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="MBS")]<-"Bus"  ##need to confirm
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="SBS")]<-"Bus"  ##need to confirm
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="DTC")]<-"Bus"
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="STR")]<-"2W"
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="CRN")]<-"Other"   ##Crane
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="RTV")]<-"Van"
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="AMB")]<-"Van"
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="TAX")]<-"Car"
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="UNKNOWN")]<-"Unknown"
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="MTW")]<-"2W"
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="TRUCK")]<-"Truck"
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="NMV")]<-"NMV"
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="TCN")]<-"Other"  ##need to confirm
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="POV")]<-"Other"
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="HDC")]<-"Car"  ##need to comfirm
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="TNG")]<-"NMV" ##Tonga
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="MAT")]<-"Van" ##Matador
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="ANI")]<-"Other" ##Animal
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="CYR")]<-"NMV" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="HTV/GOODS")]<-"Truck" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="CAR PVT")]<-"Car" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="BUS MINI/RTV")]<-"Bus" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="TRAILER/CONTANR")]<-"Truck" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="TAXI")]<-"Car" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="SCOOTRIST/MC")]<-"2W" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="BUS BLUE LINE")]<-"Bus" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="BUS OTHER")]<-"Bus" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="BUS OTHER STATE")]<-"Bus" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="UN-KNOWN VEH.")]<-"Unknown" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="CRANE")]<-"Truck" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="TRACTOR")]<-"Truck" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="STEEM ROLLER")]<-"Truck" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="BULLOCK CARTS")]<-"NMV" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="TONGA/REHRA")]<-"NMV" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="MATADOR")]<-"Van"
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="POLICE VEHICLE")]<-"Car" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="ANIMAL")]<-"Other"
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="CLUSTER BUS")]<-"Bus"
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="GRAMIN SEWA")]<-"Van" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="HTV/GDS")]<-"Truck" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="CTR BUS")]<-"Bus" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="BUS SCL")]<-"Bus" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="STEEM R")]<-"Other" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="MILITRY")]<-"Truck" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="AMBULNC")]<-"Van" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="CAL CAB")]<-"Car" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="C.RICKW")]<-"NMV" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="B.CARTS")]<-"NMV" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="POL.VEH")]<-"Car" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="ERCAW")]<-"Other" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="RING ROAD")]<-"Unknown" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="CYCLE")]<-"Cycle" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="BUS DTC")]<-"Bus" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="BUS SCHOOL")]<-"Bus" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="TEMPO")]<-"Car" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="TANKER")]<-"Truck" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="Tractor")]<-"Truck" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="DELIVERY VAN")]<-"Van" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="CALL CENTER CAB")]<-"Car" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="CYCLE RICKSHOW")]<-"NMV" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="S/C&M/C")]<-"2W" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="MILITARY VEH.")]<-"Truck" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="DELIVRY")]<-"Car" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="PVT CAR")]<-"Car" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="DTC BUS")]<-"Bus" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="BUS O S")]<-"Bus" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="TRL/CON")]<-"Other" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="BUS OTR")]<-"Bus" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="AMBULANCE")]<-"Van" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="MIN. BUS")]<-"Bus" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="MIN.BUS")]<-"Bus" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="GRM.SEW")]<-"Van" 
delhi$strik_vehicl_eng[which(delhi$striking_vehicle=="BUL")]<-"Truck" ## assuming bulldozer 

delhi$vic_vehicl_eng<-NA
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="BUS")]<-"Bus"
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="TSR")]<-"3W"
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="CAR")]<-"Car"
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="UNK")]<-"Unknown"
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="TMP")]<-"3W"
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="UDT")]<-"Unknown"
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="HTV")]<-"Truck"
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="TWW")]<-"2W"
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="BLB")]<-"Unknown"
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="TRC")]<-"Other"  ## considering as tractor
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="OSB")]<-"Other"  ##need to confirm
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="MIL")]<-"Truck"
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="TNK")]<-"Other"
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="DLV")]<-"Other"  ##Delivery vehicle
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="MBS")]<-"Bus"  ##need to confirm
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="SBS")]<-"Bus"  ##need to confirm
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="DTC")]<-"Bus"
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="STR")]<-"2W"
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="CRN")]<-"Other"   ##need to confirm
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="RTV")]<-"Van"
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="AMB")]<-"Van"
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="TAX")]<-"Car"
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="UNKNOWN")]<-"Unknown"
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="MTW")]<-"2W"
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="TRUCK")]<-"Truck"
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="NMV")]<-"NMV"
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="TCN")]<-"Other"  ##need to confirm
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="POV")]<-"Other" ##probably police vehicle
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="HDC")]<-"Car"  ##need to comfirm
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="TNG")]<-"NMV" ##need to comfirm
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="MAT")]<-"Other" ##need to comfirm
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="ANI")]<-"Other" ##need to comfirm
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="CYR")]<-"NMV" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="HTV/GOODS")]<-"Truck" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="CAR PVT")]<-"Car" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="BUS MINI/RTV")]<-"Bus" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="TRAILER/CONTANR")]<-"Other" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="TAXI")]<-"Car" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="SCOOTRIST/MC")]<-"2W" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="BUS BLUE LINE")]<-"Bus" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="BUS OTHER")]<-"Bus" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="BUS OTHER STATE")]<-"Bus" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="UN-KNOWN VEH.")]<-"Unknown" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="CRANE")]<-"Other" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="TRACTOR")]<-"Other" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="STEEM ROLLER")]<-"Other" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="BULLOCK CARTS")]<-"NMV" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="TONGA/REHRA")]<-"NMV" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="MATADOR")]<-"Van"
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="POLICE VEHICLE")]<-"Car" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="ANIMAL")]<-"Other"
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="CLUSTER BUS")]<-"Bus"
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="GRAMIN SEWA")]<-"Van" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="HTV/GDS")]<-"Truck" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="CTR BUS")]<-"Bus" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="BUS SCL")]<-"Bus" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="STEEM R")]<-"Other" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="MILITRY")]<-"Truck" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="AMBULNC")]<-"Van" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="CAL CAB")]<-"Car" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="C.RICKW")]<-"NMV" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="B.CARTS")]<-"NMV" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="POL.VEH")]<-"Car" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="ERCAW")]<-"Other" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="RING ROAD")]<-"Unknown" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="CYCLE")]<-"Cycle" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="BUS DTC")]<-"Bus" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="BUS SCHOOL")]<-"Bus" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="TEMPO")]<-"Truck" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="TANKER")]<-"Truck" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="Tractor")]<-"Other" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="DELIVERY VAN")]<-"Van" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="CALL CENTER CAB")]<-"Car" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="CYCLE RICKSHOW")]<-"NMV" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="S/C&M/C")]<-"2W" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="MILITARY VEH.")]<-"Truck" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="DELIVRY")]<-"Other" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="PVT CAR")]<-"Car" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="DTC BUS")]<-"Bus" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="BUS O S")]<-"Bus" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="TRL/CON")]<-"Other" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="BUS OTR")]<-"Bus" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="AMBULANCE")]<-"Van" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="MIN. BUS")]<-"Bus" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="MIN.BUS")]<-"Bus" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="GRM.SEW")]<-"Van" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="SLF")]<-"Self" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="PED")]<-"Pedestrian" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="CYC")]<-"Cycle" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="WLL")]<-"Other" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="ELT")]<-"Other" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="Pas")]<-"Unknown" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="BUL")]<-"Other" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="PEDESTRIAN")]<-"Pedestrian" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="FIXED OBJECT")]<-"Fixed object" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="SCOOTER/M.CYCLE")]<-"2W" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="HAND CARTS")]<-"Other" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="SELF")]<-"Self" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="TRAILOR/CONTANR")]<-"Other" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="PEDSTRN")]<-"Pedestrian" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="PASSNGR")]<-"Unknown" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="PASSENGER")]<-"Unknown" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="PAS")]<-"Unknown" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="TONGA/R")]<-"NMV" 
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="HANDCRT")]<-"NMV"
delhi$vic_vehicl_eng[which(delhi$victim_vehicle=="TRAIL/C")]<-"NMV" 


## for the following cases, fixed object was in victim category
delhi[which(delhi$vic_vehicl_eng=="Fixed object"),]  ##  3673,3801,5109
delhi$vic_vehicl_eng[3673]<- "Truck"
delhi$strik_vehicl_eng[3673]<- "Fixed object"
delhi$vic_vehicl_eng[3801]<- "Truck"
delhi$strik_vehicl_eng[3801]<- "Fixed object"
delhi$vic_vehicl_eng[5109]<- "Car"
delhi$strik_vehicl_eng[5109]<- "Fixed object"


delhi$combo<- paste(delhi$vic_vehicl_eng, "-", delhi$strik_vehicl_eng)
#x<-unique(delhi$combo)
#write.csv(x,'victim-striking-vehicle-unique-pairs.csv')
lookup<- read.csv(paste0(path,'/victim-striking-vehicle-unique-pairs_new.csv')) ## lookup table for unique victim-striking vehicle pairs and their corresponding ICD codes

delhi<- delhi %>% left_join(lookup,by="combo")
icd_codes<- read.csv(paste0(path,'/icd_code_look_up_table_row_column.csv'))

delhi<- delhi %>% left_join(icd_codes,by="ICD")

strik_list<- c('No other/fixed or stationary object'	,'Pedestrian',	'Pedal cycle',	'2/3 Wheeled',	'Car/pick-up/van'	,'Bus'	,'Railway train/railway vehicle',	'non-motor vehicle',	'unspecified',	'Non-collision', 'Unknown', 'Trucks')
vict_list<- c('Pedestrian'	,'Pedal Cycle',	'Motorcycle',	'3Wheeled',	'Car'	,'Pick-up truck/van',	'Heavy transport',	'Bus'	,'Other',	'Unknown',	'Railway')

delhi$cas_type<- vict_list[delhi$row]
delhi$strk_type<- strik_list[delhi$column]

delhi<- subset(delhi, select=c("year", "cas_type", "strk_type"))

##selecting only three years (2012-14)
delhi<- delhi[which(delhi$year %in% c(2012, 2013, 2014)),]

# Multiple imputation using MICE
# https://stats.idre.ucla.edu/r/faq/how-do-i-perform-multiple-imputation-using-predictive-mean-matching-in-r/

# Multiple questions:
# 1) what happens with weight variable, shouldn't it be here. My understanding 
# is that year is not processed by the package
# 2) Shouldn't the names of the variables be "cas_mode" and "strike_mode". Do 
# the package work with different name variables?
# 3) shouldn't the modes be similar to what is in standardized file? 

# Read lookup table
smodes <- read_csv('data/global/modes/standardized_modes.csv')
# Separate rows 
smodes <- smodes %>% separate_rows(original, sep = ';')
# Trim
smodes <- smodes %>% mutate(across(where(is.character), str_trim))


#unique(delhi$cas_type)
#unique(delhi$strk_type)
# Transforming "unknown" and "unspecified" to NA
delhi2 <- delhi %>% 
  mutate(cas_mode = factor(ifelse(cas_type == "Unknown", NA, cas_type)),
         strike_mode = factor(ifelse(strk_type %in% c("Unknown", "unspecified"),
                                     NA, strk_type)))
#table(delhi2$cas_type, delhi2$cas_mode, useNA = "always")
#table(delhi2$strk_type, delhi2$strike_mode, useNA = "always")

# There are 38 missing values in cas_mode and 3019 in strike mode.
md.pattern(delhi2)

# In 38 rows both types are missing 
md.pairs(delhi2)

# Imputation using mice. I imputed the dataset 5 times (this is why is multiple
# imputation). The idea is to make a sensitivity analysis in the results.
imp1 <- mice(delhi2[,c("year","cas_mode", "strike_mode")], m = 5, seed = 12345)
imp1

# Adding imputed rows to delhi's dataset. The original columns (with missing
# values) are kept in the dataset with sufix "_original". And the first run of
# imputation is saved in cas_mode and strike_mode. From second to fifth
# imputation are also saved with the sufix "_2nd", to "5th".
delhi3 <- delhi2 %>% 
  rename(cas_mode_original = cas_mode,
         strike_mode_original = strike_mode) %>% 
  bind_cols(complete(imp1) %>% select(cas_mode, strike_mode)) %>%
  bind_cols(complete(imp1, action = 2) %>% select(cas_mode, strike_mode) %>% 
              rename(cas_mode_2nd = cas_mode, strike_mode_2nd = strike_mode)) %>%
  bind_cols(complete(imp1, action = 3) %>% select(cas_mode, strike_mode) %>% 
              rename(cas_mode_3rd = cas_mode, strike_mode_3rd = strike_mode)) %>%   bind_cols(complete(imp1, action = 4) %>% select(cas_mode, strike_mode) %>% 
              rename(cas_mode_4th = cas_mode, strike_mode_4th = strike_mode)) %>%
  bind_cols(complete(imp1, action = 5) %>% select(cas_mode, strike_mode) %>% 
              rename(cas_mode_5th = cas_mode, strike_mode_5th = strike_mode))
 
# Comparing imputations 
# table(delhi3$cas_mode_original, delhi3$cas_mode, useNA = "always")
# table(delhi3$strike_mode_original, delhi3$strike_mode, useNA = "always")
# table(delhi3$strike_mode_original, delhi3$strike_mode_2nd, useNA = "always")

# Recode cas_mode and strike_mode
whw <- delhi3 %>% 
  mutate(cas_mode = smodes$exhaustive_list[match(tolower(cas_mode),
                                                 smodes$original)],
         strike_mode = smodes$exhaustive_list[match(tolower(strike_mode),
                                                    smodes$original)],
         cas_mode_2nd = smodes$exhaustive_list[match(tolower(cas_mode_2nd),
                                                 smodes$original)],
         strike_mode_2nd = smodes$exhaustive_list[match(tolower(strike_mode_2nd),
                                                    smodes$original)],
         cas_mode_3rd = smodes$exhaustive_list[match(tolower(cas_mode_3rd),
                                                     smodes$original)],
         strike_mode_3rd = smodes$exhaustive_list[match(tolower(strike_mode_3rd),
                                                        smodes$original)],
         cas_mode_4th = smodes$exhaustive_list[match(tolower(cas_mode_4th),
                                                     smodes$original)],
         strike_mode_4th = smodes$exhaustive_list[match(tolower(strike_mode_4th),
                                                        smodes$original)],
         cas_mode_5th = smodes$exhaustive_list[match(tolower(cas_mode_5th),
                                                     smodes$original)],
         strike_mode_5th = smodes$exhaustive_list[match(tolower(strike_mode_5th),
                                                        smodes$original)],
         weight = 3) # Weight is 3 because these injuries are from 2012-2014

# Comparing frequencies after recoding
table(delhi3$cas_mode, useNA = "always")
table(whw$cas_mode, useNA = "always")
table(delhi3$strike_mode, useNA = "always")
table(whw$strike_mode, useNA = "always")

# Check if all modes are correctly recoded
unique(whw$cas_mode) %in% smodes$exhaustive_list
unique(whw$cas_mode_2nd) %in% smodes$exhaustive_list
unique(whw$cas_mode_3rd) %in% smodes$exhaustive_list
unique(whw$cas_mode_4th) %in% smodes$exhaustive_list
unique(whw$cas_mode_5th) %in% smodes$exhaustive_list
unique(whw$strike_mode) %in% smodes$exhaustive_list
unique(whw$strike_mode_2nd) %in% smodes$exhaustive_list
unique(whw$strike_mode_3rd) %in% smodes$exhaustive_list
unique(whw$strike_mode_4th) %in% smodes$exhaustive_list
unique(whw$strike_mode_5th) %in% smodes$exhaustive_list

injury_file <- 'injuries_delhi.csv'
write.csv(whw,paste0('inst/extdata/local/delhi/',injury_file))

