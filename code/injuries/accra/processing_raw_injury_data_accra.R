# Load libraries
library(tidyverse)
library(reshape2)
library(mice)

# Specify path 
path<- file.path('data/local/accra/')

##reading dataframe with road deaths victims
whw<- readRDS(paste0(path, '/accra_injuries_dataset_rob.Rds'))

whw$victim_strike_pair<- paste(whw$cas_mode, "-", whw$strike_mode)
names(whw)[7]<- "combo"
#write.csv(unique(accra$victim_strike_pair), 'unique_victim_strike_pairs.csv')
lookup<- read.csv(paste0(path,'/unique_victim_strike_pairs.csv'))
icd_codes<-  read.csv(paste0(path,'/icd_code_look_up_table_row_column.csv'))
whw<- whw %>% left_join(lookup, "combo")
whw<- whw %>% left_join(icd_codes, by ="ICD")
strik_list <- c('No other/fixed or stationary object'	,'Pedestrian',	
                'Pedal cycle', '2/3 Wheeled',	'Car/pick-up/van', 'Bus',
                'Railway train/railway vehicle', 'non-motor vehicle',	
                'unspecified', 'Non-collision', 'Unknown', 'Trucks')
vict_list <- c('Pedestrian'	,'Pedal Cycle',	'Motorcycle',	'3Wheeled',	'Car',
               'Pick-up truck/van',	'Heavy transport',	'Bus'	,'Other',	
               'Unknown',	'Railway')
whw$cas_type <- vict_list[whw$row]
whw$strk_type <- strik_list[whw$column]

whw<- subset(whw, select=c("year","cas_age", "cas_gender", "cas_type", "strk_type"))
whw$weight<- 10


# Multiple imputation using MICE
# https://stats.idre.ucla.edu/r/faq/how-do-i-perform-multiple-imputation-using-predictive-mean-matching-in-r/
# Read lookup table
smodes <- read_csv('data/global/modes/standardized_modes.csv')
# Separate rows 
smodes <- smodes %>% separate_rows(original, sep = ';')
# Trim
smodes <- smodes %>% mutate(across(where(is.character), str_trim))

# unique(whw$cas_age)
# unique(whw$cas_gender)
# unique(whw$cas_type)
# unique(whw$strk_type)
# Transforming "unknown" and "unspecified" to NA
accra2 <- whw %>% 
  mutate(year = factor(year),
         cas_age = as.numeric(cas_age),
         cas_gender = factor(ifelse(cas_gender == "femal", "female", "male")),
         cas_mode = factor(ifelse(cas_type == "Unknown", NA, cas_type)),
         strike_mode = factor(ifelse(strk_type %in% c("Unknown", "unspecified"),
                                     NA, strk_type)))

# table(accra2$cas_type, accra2$cas_mode, useNA = "always")
# table(accra2$strk_type, accra2$strike_mode, useNA = "always")

# There are 258 missing values in strike_mode and 1 in cas_age
md.pattern(accra2)

# There are no pairs of missing values
md.pairs(accra2)

# Imputation using mice. I imputed the dataset 5 times (this is why is multiple
# imputation). The idea is to make a sensitivity analysis in the results.
imp1 <- mice(accra2[,c("year","cas_age", "cas_gender", "cas_mode", "strike_mode")],
             m = 5, seed = 12345)
imp1

# Adding imputed rows to Accras's dataset. The original columns (with missing
# values) are kept in the dataset with sufix "_original". And the first run of
# imputation is saved in cas_mode and strike_mode. From second to fifth
# imputation are also saved with the sufix "_2nd", to "5th".
accra3 <- accra2 %>% 
  rename(cas_age_original = cas_age,
         strike_mode_original = strike_mode) %>% 
  bind_cols(complete(imp1) %>% select(cas_age, strike_mode)) %>%
  bind_cols(complete(imp1, action = 2) %>% select(cas_age, strike_mode) %>% 
              rename(cas_age_2nd = cas_age, strike_mode_2nd = strike_mode)) %>%
  bind_cols(complete(imp1, action = 3) %>% select(cas_age, strike_mode) %>% 
              rename(cas_age_3rd = cas_age, strike_mode_3rd = strike_mode)) %>%  
  bind_cols(complete(imp1, action = 4) %>% select(cas_age, strike_mode) %>% 
              rename(cas_age_4th = cas_age, strike_mode_4th = strike_mode)) %>%
  bind_cols(complete(imp1, action = 5) %>% select(cas_age, strike_mode) %>% 
              rename(cas_age_5th = cas_age, strike_mode_5th = strike_mode)) %>% 
  select(year, cas_gender, cas_type, cas_mode, weight, strk_type, 
         cas_age_original, strike_mode_original, cas_age, strike_mode, 
         cas_age_2nd, strike_mode_2nd, cas_age_3rd, strike_mode_3rd,
         cas_age_4th, strike_mode_4th, cas_age_5th, strike_mode_5th)

# Comparing imputations 
table(accra3$cas_age_original, useNA = "always")
table(accra3$cas_age, useNA = "always")
table(accra3$strike_mode_original, accra3$strike_mode, useNA = "always")
table(accra3$strike_mode_original, accra3$strike_mode_2nd, useNA = "always")

# Recode cas_mode and strike_mode
accra4 <- accra3 %>% 
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
# Weight is 10 because these injuries are from 2007-2016

# Comparing frequencies after recoding
table(accra3$cas_mode, useNA = "always")
table(accra4$cas_mode, useNA = "always")
table(accra3$strike_mode, useNA = "always")
table(accra4$strike_mode, useNA = "always")

# Check if all modes are correctly recoded
unique(accra4$cas_mode) %in% smodes$exhaustive_list
unique(accra4$strike_mode) %in% smodes$exhaustive_list
unique(accra4$strike_mode_2nd) %in% smodes$exhaustive_list
unique(accra4$strike_mode_3rd) %in% smodes$exhaustive_list
unique(accra4$strike_mode_4th) %in% smodes$exhaustive_list
unique(accra4$strike_mode_5th) %in% smodes$exhaustive_list

# Specify name of output file
injury_file <- 'injuries_accra.csv'
write.csv(accra4, paste0('inst/extdata/local/accra/',injury_file))
