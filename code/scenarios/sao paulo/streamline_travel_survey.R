# Load libraries
library(tidyverse)

# Read sao paulo's travel survey
rd <- read_csv("data/scenarios/sao paulo/SP 2012 travel data.csv")

# Data Dictionary

# ID_PESS = participant ID
# IDADE = age
# SEXO = sex
# N_VIAG = trip ID
# TOT_VIAG = total number of trips per person
# ANDA_O = walking time at origin
# ANDA_D = walking time at destination
# DURACAO = trip duration in minutes
# MODOPRIN = main mode of transport
# 01 to 05 = bus
# 06 = car, driver
# 07 = car, passenger
# 08 = taxi
# 09 to 11 = van / minibus
# 12 = subway
# 13 = train
# 14 = motorbicycle
# 15 = bycicle
# 16 = walking
# 17 = others
# DISTANCIA = trip distance in meters
# ID_ORDEM = row ID

# Rename columns
rd <- rename(rd, participant_id = ID_PESS , 
             age  =  IDADE,
             sex = SEXO,
             trip_id = N_VIAG,
             total_trips = TOT_VIAG,
             walking_time_origin = ANDA_O,
             walking_time_dest = ANDA_D, 
             trip_duration = DURACAO, 
             mode = MODOPRIN,
             distance = DISTANCIA,
             row_id = ID_ORDEM
             
)

# Recode modes as strings
mode_df <- data.frame(
  mode_int = append(c(1:17), NA),
  mode_string = c(rep('bus', 5), 'car_driver', 
                       'car_passenger', 'taxi',
                       rep('van', 3), 'subway',
                       'train', 'motorbike',
                       'bicycle', 'walk', 'others', 'NAs')
                       
   
  
)

rd$mode_string <- as.character(mode_df$mode_string[match(rd$mode, mode_df$mode_int)])

ggplot(rd %>% 
         group_by(mode_string) %>% 
         summarise(count = n()) %>% 
         mutate(perc = round(count/sum(count) * 100, 1)), 
       aes(x = mode_string, y = perc)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = rel(0.8)))