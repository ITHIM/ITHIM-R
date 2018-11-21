library(tidyverse)
library(plotly)

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
             trip_mode = MODOPRIN,
             trip_distance = DISTANCIA,
             row_id = ID_ORDEM
             
)

# Recode modes as strings
mode_df <- data.frame(
  mode_int = append(c(1:17), NA),
  mode_string = c(rep('bus', 5), 'car_driver', 
                  'car_passenger', 'taxi',
                  rep('van', 3), 'subway',
                  'train', 'motorbike',
                  'bicycle', 'walk', 'others', NA)
  
  
  
)

# Convert numeric to string modes
rd$mode_string <- as.character(mode_df$mode_string[match(rd$trip_mode, mode_df$mode_int)])
rd$trip_mode <- rd$mode_string
rd$mode_string <- NULL

# Covert distance in meters to kms
rd$trip_distance <- rd$trip_distance / 1000

# plotly::ggplotly(
ggplot(rd %>% 
         filter(!is.na(trip_mode)) %>% 
         group_by(trip_mode) %>% 
         summarise(count = n()) %>% 
         mutate(perc = round(count/sum(count) * 100, 1)), 
       aes(x = trip_mode, y = perc)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label = perc), position = position_dodge(width=0.9), vjust=-0.25, color = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "", y = "percentage(%)", title = "Main Mode distribution")
# )


# Define distance categories
dist_cat <- c("0-6 km", "7-9 km", "10+ km")

# Initialize them
rd$trip_distance_cat <- NULL
rd$trip_distance_cat[rd$trip_distance > 0 & rd$trip_distance < 7] <- dist_cat[1]
rd$trip_distance_cat[rd$trip_distance >= 7 & rd$trip_distance < 10] <- dist_cat[2]
rd$trip_distance_cat[rd$trip_distance >= 10] <- dist_cat[3]

ggplot(rd %>% 
         filter(!is.na(trip_distance_cat)) %>% 
         group_by(trip_distance_cat) %>% 
         summarise(count = n()) %>% 
         mutate(perc = round(count/sum(count) * 100, 1)), 
       aes(x = trip_distance_cat, y = perc)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label = perc), position = position_dodge(width=0.9), vjust=-0.25, color = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "", y = "percentage(%)", title = "Main Mode Distance distribution")