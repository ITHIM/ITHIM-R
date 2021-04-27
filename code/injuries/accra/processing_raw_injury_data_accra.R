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

####################

# Read lookup table for standardized modes
smodes <- read_csv('data/global/modes/standardized_modes.csv')
# Separate rows 
smodes <- smodes %>% separate_rows(original, sep = ';')
# Trim
smodes <- smodes %>% mutate(across(where(is.character), str_trim))

  # Specify name of output file
  injury_file <- paste0('injuries_', city, '.csv')
  
  # Write file to the right folder
  write_csv(whw, paste0('inst/extdata/local/',city, '/', injury_file))
#}