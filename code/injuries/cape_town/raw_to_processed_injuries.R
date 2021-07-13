# Load libraries
library(tidyverse)
library(mice)
library(magrittr)
library(readxl)
library(fuzzyjoin)
library(nnet)

rm(list=ls());gc()

# Since there's randomness in the sample function a seed is set to replicate
# results in the future
set.seed(12345)

# Lambed's code ----  
# First the code that Lambed did to preprocess these injuries

# Import Data
# route <- "V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Cape Town/Crash Data_CCT_190919"
route <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/SouthAfrica/CapeTown/Crash Data_CCT_190919"
load(paste0(route, "/data.RDATA"))

# Clean crash data
#mutate mode & year and rename  variables in crash data
all_injury <- 
  all_injury_0 %>% 
  mutate(mode= ifelse(is.na(`Vehicle Type`), "Pedestrian", `Vehicle Type`),
         `Vehicle Ref No.` = ifelse(is.na(`Vehicle Ref No.`), "N", `Vehicle Ref No.`),
         year = substring(Accdate, 1,4), sap = paste0(substring(Accdate, 6,7),substring(Accdate, 1,4) )) %>% 
  rename(sex=Gender, age=Age, race=`Population Group`)


all_injury <- 
  left_join(all_injury, data.frame(mode = levels(as.factor(all_injury$mode)), 
                                   mode1 = c("other","bicycle","bus","bus","other","bus","car","car","motorcycle","motorcycle","bus","other","car","other","car","pedestrian","other","other","motorcycle","truck","truck","unknown"),
                                   rank = c(6,5,2,2,6,2,3,3,4,4,2,6,3,6,3,8,6,6,4,1,1,7)))

# Explore fatal crashes
#select fatal crashes
fatal_crash <- 
  all_injury %>% 
  filter(Injuries=="Killed") %>% 
  distinct(`Accident No`) %>% 
  left_join(all_injury)

# Assign the largest vehilce as the strike mode in each accident
#strikers in crash data
fatal_crash %<>% group_by(`Accident No`, `Vehicle Ref No.`) %>% filter(!("Killed" %in% Injuries)) %>% ungroup()

#add strike mode
fatal_crash %<>% 
  group_by(`Accident No`, mode1, rank) %>%
  mutate(strike_mode = mode1[which.min(rank)]) %>% 
  ungroup() %>%
  distinct(`Accident No`, strike_mode) 

fatal_crash %>% 
  group_by(strike_mode) %>% 
  summarise(count = dplyr::n()) %>% 
  mutate(proportion = round(count*100/sum(count),2))

# Victim modes for those killed in fatal crashes
#determin victim mode in crash data
killed <- all_injury %>% 
  filter(Injuries == "Killed")

killed %>% 
  group_by(mode1) %>% 
  summarise(count = dplyr::n()) %>% 
  mutate(proportion = round(count*100/sum(count),2))

# In the hospital file (explored below), the victime modes for drivers and passengers are not differentiated (i.e., they are simply entered as driver or passenger). Since we are keeping the hospital file as the more accurate dataset, we are interested in using the victime modes distribution in the crash file to predict cause of vehicle types for drivers and passengers in the hospital file
killed %<>% 
  filter(!(mode1=="pedestrian"|mode1=="motorcycle"|mode1 == "bicycle")) %>% 
  dplyr::select(sex, age, race, mode1) %>% 
  sample_frac(1) #just to randomise

killed %>% 
  group_by(mode1) %>% 
  summarise(count = dplyr::n()) %>% 
  mutate(proportion = round(count*100/sum(count),2))

# Cleaning hospital data
#keep common varaible in fatality files
fatality_2012 <-  fatality_2012_0[,-c(5,12, 37:38)]
fatality_2012$year <- "2012"
fatality_2013 <-  fatality_2013_0[,-c(5,12, 37:38)]
fatality_2013$year <- "2013"
fatality_2014 <-  fatality_2014_0[,-c(5,12, 37:38)]
fatality_2014$year <- "2014"
fatality_2015 <-  fatality_2015_0[,-c(5,12, 17:18, 39:53)]
fatality_2015$year <- "2015"
fatality_2016 <-  fatality_2016_0[,-c(2,4:7,14, 19:23,25, 47:53)]
#change date of death to date
fatality_2016 %<>%
  mutate(DATE_OF_DEATH = as.Date(paste0(substring(DATE_OF_DEATH, 7,10),"-",
                                        substring(DATE_OF_DEATH, 4,5), "-",
                                        substring(DATE_OF_DEATH, 1,2))),
         CALL_DATE = as.Date(paste0(substring(CALL_DATE, 7,10),"-",
                                    substring(CALL_DATE, 4,5), "-",
                                    substring(CALL_DATE, 1,2))),
         TIME_OF_CALL = as.POSIXct(paste0(substring(TIME_OF_CALL, 7,10),"-",
                                          substring(TIME_OF_CALL, 4,5), "-",
                                          substring(TIME_OF_CALL, 1,2)," ",
                                          substring(TIME_OF_CALL, 12,13),":",
                                          substring(TIME_OF_CALL, 15,16),":", 
                                          "00")),
         year = "2016")  

#rename varaibles in  
fatality_2016 %<>% 
  rename( `Forensic Lab` = FORENSIC_LAB , `Case Number` = CORRECTED_POL_CASE_NO,
          timeinjury = TIME_OF_INJURY , race  = RACE, sex = GENDER ,
          datedeath = DATE_OF_DEATH, timedeath = TIME_OF_DEATH , 
          Age = AGE_IN_YEARS , provinceinjured = PROVINCE_OF_INJURY , 
          towninjury = TOWN_OF_INJURY , suburbinjury = SUBURB_OF_INJURY ,
          sceneinjur = SCENE_OF_INJURY , `Police Station` = NEAREST_SAPS ,
          Hospital = HOSPITAL_NAME , allegedcoddescr = ALLEGED_CAUSE_OF_DEATH ,
          allegedcodcat = ALLEGED_COD_CATEGORY ,
          officialcoddescr = OFFICIAL_CAUSE_OF_DEATH , 
          officialcodcat = OFFICIAL_COD_CATEGORY , 
          vehicletype = VEHICLE_TYPE , roadtype = ROAD_TYPE , 
          speedlimit = SPEED_LIMIT , numberofvictims = NUMBER_OF_VICTIMS ,
          numberofvehicles = NUMBER_OF_VEHICLES , 
          driverposition = DRIVER_POSITION , 
          passengerposition = PASSENGER_POSITION , 
          pedestrianposition = PEDESTRIAN_POSITION , 
          bodyposition = BODY_POSITION , 
          weathercondition = WEATHER_CONDITION , 
          oadcondition = ROAD_CONDITION , 
          incidentstreet = INCIDENT_STREET , incidentsuburb = INCIDENT_SUBURB , 
          incidenttown = INCIDENT_CITY , `Time Incident Logged` = TIME_OF_CALL , 
          `Date Incident Logged` = CALL_DATE )

#combine all fatality files
all_fatality <- rbind(fatality_2012,fatality_2013,fatality_2014,fatality_2015,
                      fatality_2016) 

#rename age
all_fatality <- all_fatality %>% rename(age = Age)

#Filtering fatalities from cape town only
all_fatality <- all_fatality %>% 
  filter(towninjury %in% c("BELLVILLE", "CAPE TOEN", "CAPE TOWM", 
                           "Cape Town", "CAPE TOWN", "ELSIES RIVER",
                           "MITCHELL'S PLAIN", "PAROW CAPE TOWN",
                           "PHILIPPI"))

#change levels of factor variabless to lower cases for comparison 
#all_fatality <- mutate_all(all_fatality, funs(tolower))
all_fatality <- mutate_all(all_fatality, list(tolower))

#relable victim mode in hospital data
all_fatality$allegedcoddescr[all_fatality$allegedcoddescr == "motor-cyclist"] <- "motorcycle"
all_fatality$allegedcoddescr[all_fatality$allegedcoddescr == "motorcycle pillion"] <- "motorcycle"
all_fatality$allegedcoddescr[all_fatality$allegedcoddescr == "cyclist"] <- "bicycle"
all_fatality$allegedcoddescr[all_fatality$allegedcoddescr == "railway pedestrian"] <- "pedestrian"

#relable strike mode in hospital data
all_fatality$vehicletype[all_fatality$vehicletype == "bakkie" | all_fatality$vehicletype == "sedan"] <- "car"
all_fatality$vehicletype[all_fatality$vehicletype == "bus" | all_fatality$vehicletype == "minibus taxi"] <- "bus"
all_fatality$vehicletype[all_fatality$vehicletype == "small pickup truck"] <- "truck"
all_fatality$vehicletype[all_fatality$vehicletype == "motorbike"] <- "motorcycle"
all_fatality$vehicletype[all_fatality$allegedcoddescr == "railway pedestrian"] <- "train"

# Exploring victim mode in hospital data. driver, passenger are not specified
all_fatality %>% 
  group_by(allegedcoddescr) %>% 
  summarise(count = dplyr::n()) %>% 
  mutate(proportion = round(count*100/sum(count),2))

# We consider that the reported victim mode for pedestrian, bicycle and motorcycle are correct; the rest are unknow and will be determined

#split dataset for victim mode assignment
unknown_victim_mode <- all_fatality %>% filter(!(allegedcoddescr == "pedestrian" | allegedcoddescr =="bicycle"| allegedcoddescr =="motorcycle"))
known_victim_mode <- setdiff(all_fatality, unknown_victim_mode)

# We use the victim modes in the crash file to assign unknown modes in the hospital file. Note that only mode destribution is used. Age and gender are largely under reported in the crash file and it does not seem to make sense using them

#add victim mode
victim_mode_crash <- bind_rows(killed, sample_n(killed,(nrow(unknown_victim_mode) - nrow(killed))))
unknown_victim_mode$allegedcoddescr <- victim_mode_crash$mode1

#reconstitue data set
all_fatality <- bind_rows(known_victim_mode, unknown_victim_mode)

all_fatality %>% 
  group_by(allegedcoddescr) %>% 
  summarise(count = dplyr::n()) %>% 
  mutate(proportion = round(count*100/sum(count),2))

# We now explore the strike modes in hospital data. Just about half cases have this reported. Taking a closer look, apart from pedestrians, where this seems to represent the strike mode, the rest seem to be a further specification of the victim mode.
all_fatality %>% 
  group_by(vehicletype) %>% 
  summarise(count = dplyr::n()) %>% 
  mutate(proportion = round(count*100/sum(count),2))

# We consider that the strike mode for pedestrian is accurate and the rest are unkown. We use the strike modes of the casuality file to impute unknown strike modes for the hospital file.

#split data to known and unknown victim and strike mode
known_strike_mode <- 
  all_fatality %>% 
  filter(!(is.na(vehicletype)|vehicletype == "unknown"|vehicletype == "other")) %>% 
  filter(allegedcoddescr =="pedestrian")

unknown_strike_mode <- setdiff(all_fatality, known_strike_mode) %>% sample_frac(1)

known_strike_mode %>% 
  group_by(vehicletype) %>%
  summarise(count_pedestrian = dplyr::n()) %>% 
  mutate(proportion = round(count_pedestrian*100/sum(count_pedestrian),2))

# We impute the strike modes in the hospital file based only on the distribution of the strike modes in the casualty file (other variable like age, and sex are poorly reported)
#add striking modes
x <- nrow(unknown_strike_mode) - nrow(fatal_crash)
y <- sample_n(fatal_crash,x, replace =FALSE)
z <- bind_rows(fatal_crash, y)
unknown_strike_mode$vehicletype <- z$strike_mode

all_fatality <- bind_rows(known_strike_mode, unknown_strike_mode)

# Number pedestrian hiting pedestrian is high
table(all_fatality$allegedcoddescr, all_fatality$vehicletype)

# We convert pedestrian striking modes to othe modes
a <- filter(all_fatality,allegedcoddescr=="pedestrian" & vehicletype == "pedestrian")
b <- a
a$vehicletype <-sample(c("bus","bus", "bus","car", "car", "car", "car", "car", "car", "car", "car", "car", "car","car", "car", "car","truck","motorcycle","unknown"),nrow(a),replace = TRUE)
all_fatality %<>%
  setdiff(b) %>% 
  bind_rows(a)


table(all_fatality$allegedcoddescr, all_fatality$vehicletype)

# Who hit who matrix
x <- all_fatality

cas_modes <- unique(x$allegedcoddescr)

strike_modes <- unique(x$vehicletype)

whw <- matrix(0,ncol=length(strike_modes),nrow=length(cas_modes))

for(i in 1:length(cas_modes))
  
  for(j in 1:length(strike_modes))
    
    whw[i,j] <- sum(x$allegedcoddescr==cas_modes[i]&x$vehicletype==strike_modes[j])

row.names(whw) <- cas_modes
colnames(whw) <- strike_modes

print(whw)

# Export data to different locations
cape_town_injury <- 
  dplyr::select(all_fatality, age, sex, allegedcoddescr, vehicletype, year) %>% 
  rename(cas_mode = allegedcoddescr, strike_mode = vehicletype, cas_age = age, cas_sex = sex) %>% 
  mutate(cas_sex= ifelse(cas_sex=="male", "Male", "Female"))

##Export for ithimr
# write.csv(cape_town_injury, file="V:/Studies/MOVED/HealthImpact/Clone/lambed/meelan_hia_africa/data/local/cape_town/injuries_cape_town.csv")
write.csv(cape_town_injury, file="data/local/cape_town/injuries_cape_town.csv",
          row.names = F)


# Imputation ----  
# Set filepath
file_path <- file.path('data/local/cape_town/injuries_cape_town.csv')

# Read
whw <- read_csv(file_path)
unique(whw$year)
# Remove unused columns
#whw$X1 <- whw$year <- NULL

# Rename column
whw <- whw %>% rename(cas_gender = cas_sex)

# Add weight column
#whw$weight <- 4
#whw$weight <- 5 # Weight is 5 because there's data from 2012 to 2016

# Read lookup table
smodes <- read_csv('data/global/modes/standardized_modes.csv')
# Separate rows 
smodes <- smodes %>% separate_rows(original, sep = ';')
# Trim
smodes <- smodes %>% 
  mutate(across(where(is.character), str_trim))

# Multiple imputation using MICE
# https://stats.idre.ucla.edu/r/faq/how-do-i-perform-multiple-imputation-using-predictive-mean-matching-in-r/
whw2 <- whw %>% mutate(
  cas_gender = factor(cas_gender),
  cas_mode = factor(ifelse(cas_mode == "unknown", NA, cas_mode)),
  strike_mode = factor(ifelse(strike_mode == "unknown", NA, strike_mode))
)

table(whw$cas_mode, useNA = "always")
table(whw2$cas_mode, useNA = "always")
table(whw$strike_mode, useNA = "always")
table(whw2$strike_mode, useNA = "always")

# There are 2 age missing, 40 cas_mode and 134 strike mode missing.
md.pattern(whw2)

# Out of the 40 and 134 cas and strike mode, in only 5 rows both modes are missing
md.pairs(whw2)

# imputation
imp1 <- mice(whw2, m = 5, seed = 12345)
imp1
# Adding imputed rows to Cape Town's dataset. The original columns (with missing
# values) are kept in the dataset with suffix "_original". And the first run of
# imputation is saved in cas_mode and strike_mode. From second to fifth
# imputation are also saved with the suffix "_2nd", to "5th".
whw3 <- whw2 %>% 
  rename(cas_mode_original = cas_mode,
         strike_mode_original = strike_mode) %>% 
  bind_cols(complete(imp1) %>% dplyr::select(cas_mode, strike_mode)) %>%
  bind_cols(complete(imp1, action = 2) %>% dplyr::select(cas_mode, strike_mode) %>% 
              rename(cas_mode_2nd = cas_mode, strike_mode_2nd = strike_mode)) %>%
  bind_cols(complete(imp1, action = 3) %>% dplyr::select(cas_mode, strike_mode) %>% 
              rename(cas_mode_3rd = cas_mode, strike_mode_3rd = strike_mode)) %>%   bind_cols(complete(imp1, action = 4) %>% dplyr::select(cas_mode, strike_mode) %>% 
                                                                                                rename(cas_mode_4th = cas_mode, strike_mode_4th = strike_mode)) %>%
  bind_cols(complete(imp1, action = 5) %>% dplyr::select(cas_mode, strike_mode) %>% 
              rename(cas_mode_5th = cas_mode, strike_mode_5th = strike_mode)) #%>% 
  # To avoid using age and sex in the model I renamed these variables
  #rename(cas_gender_notnow = cas_gender,
  #       cas_age_notnow = cas_age)

# Comparing imputations 
table(whw3$cas_mode_original, whw3$cas_mode, useNA = "always")
table(whw3$strike_mode_original, whw3$strike_mode, useNA = "always")
table(whw3$strike_mode_original, whw3$strike_mode_2nd, useNA = "always")

# Recode cas_mode and strike_mode
whw4 <- whw3 %>% 
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
                                                        smodes$original)])

# Comparing frequencies after recoding
table(whw3$cas_mode, useNA = "always")
table(whw4$cas_mode, useNA = "always")
table(whw3$strike_mode, useNA = "always")
table(whw4$strike_mode, useNA = "always")

# Check if all modes are correctly recoded
unique(whw4$cas_mode) %in% smodes$exhaustive_list
unique(whw4$cas_mode_2nd) %in% smodes$exhaustive_list
unique(whw4$cas_mode_3rd) %in% smodes$exhaustive_list
unique(whw4$cas_mode_4th) %in% smodes$exhaustive_list
unique(whw4$cas_mode_5th) %in% smodes$exhaustive_list
unique(whw4$strike_mode) %in% smodes$exhaustive_list
unique(whw4$strike_mode_2nd) %in% smodes$exhaustive_list
unique(whw4$strike_mode_3rd) %in% smodes$exhaustive_list
unique(whw4$strike_mode_4th) %in% smodes$exhaustive_list
unique(whw4$strike_mode_5th) %in% smodes$exhaustive_list

# Save file
injury_file <- 'injuries_cape_town.csv'
write.csv(whw4,paste0('inst/extdata/local/cape_town/',injury_file))


