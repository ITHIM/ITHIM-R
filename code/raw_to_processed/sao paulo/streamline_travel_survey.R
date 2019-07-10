# Clear workspace
rm (list = ls())

# Load libraries
library(tidyverse)
library(plotly)

# Read sao paulo's travel survey
rd <- read.csv("data/local/sao_paulo/trips_sao_paulo2.csv", stringsAsFactors = F)

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
             trip_id = ID_ORDEM, 
             total_trips = TOT_VIAG,
             walking_time_origin = ANDA_O,
             walking_time_dest = ANDA_D, 
             trip_duration = DURACAO, 
             trip_mode = MODOPRIN,
             trip_distance = DISTANCIA
             
)

# Recode modes as strings
mode_df <- data.frame(
  mode_int = append(c(1:17), NA),
  mode_string = c(rep('bus', 5), rep('car', 2), 
                  'taxi', rep('van', 3), 
                  'subway',
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
  labs(x = "", y = "percentage(%)", title = "Main Mode distribution - without weights")
# )

sum_total_trip_weight <- sum(rd$FE_VIA, na.rm = T)

# plotly::ggplotly(
ggplot(rd %>% 
         filter(!is.na(trip_mode) & 
                  (is.na(MODO2) & is.na(MODO3) & is.na(MODO4)) &
                  trip_mode != 'others') %>% 
         group_by(trip_mode) %>% 
         summarise(sum_trip_weights = sum(FE_VIA)) %>% 
         mutate(perc = round(sum_trip_weights/sum(sum_trip_weights) * 100, 1)), 
       aes(x = trip_mode, y = perc)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label = perc), position = position_dodge(width=0.9), vjust=-0.25, color = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "", y = "percentage(%)", title = "Main Mode distribution - with weights")
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

# Calculate mode speed from the dataset, by using mean distance and duration
# Remove all trips with multiple modes
# Using only commute mode as a proxy
mode_speed <- rd %>% filter(is.na(MODO2) & is.na(MODO3) & is.na(MODO4) &  
              ((MOTIVO_O %in% c(1, 2, 3) & MOTIVO_D %in% 8) | 
              (MOTIVO_D %in% c(1, 2, 3) & MOTIVO_O %in% 8))
             ) %>% group_by(trip_mode) %>% 
  summarise(mean (trip_distance), 
            speed = (mean(trip_distance)) / (mean(trip_duration) / 60))

# source_modes <- c('bus')
# target_modes <- c('car')
# 
# source_percentages <- c(0.4)
# 
# local_source_trips <- list()
# 
# for (i in 1:length(source_modes))
#   local_source_trips[i] <- nrow(filter(rd, trip_mode == source_modes[i])) - source_trips[i]
# 
# local_source_trips <- purrr::flatten_dbl(local_source_trips)

rd$person_weight <- round(rd$FE_PESS / 100)

# Select columns
rd <- dplyr::select(rd, participant_id, age, sex, trip_id, total_trips,
             walking_time_origin,
             walking_time_dest,
             trip_duration,
             trip_mode,
             trip_distance,
             trip_distance_cat,
             person_weight)

rd$pid <- -1
id <- 1
pid_list <- unique(rd$participant_id)
tid <- 1
rd$tid <- -1

new_rd <- list()

# bd <- rd
# rd <- bd
for(i in 1:length(pid_list)) {
  # i <- 1
  # require(profvis)
  # profvis({
  #   #your code here
  pg <- filter(rd, participant_id == pid_list[i])
  # Assign a new trip id
  rd[rd$participant_id == pid_list[i],]$tid <- seq(from = tid, to = tid + nrow(pg) - 1)
  # Advance the trip id index by pg
  tid <- tid + nrow(pg)
  
  count <- pg$person_weight[1]
  d <- bind_rows(replicate((count), pg, simplify = FALSE))
  d <- arrange(d, trip_id)
  rd[rd$participant_id == pid_list[i],]$pid <- id
  d$pid <- rep((id + 1):(id + count), nrow(pg))
  d$tid <- seq(from = tid, to = (tid) + (nrow(pg) * count) - 1 )
  
  tid <- max(d$tid) + 1
  
  id <- id + count
  
  if (length(new_rd) == 0){
    new_rd <- d
  }else{
    new_rd <- bind_rows(new_rd, d)
  }
  
  # number_unique_id <- rd %>% filter(participant_id == pid_list[i]) %>% 
  #   summarise(ua = length(unique(age))) %>% 
  #   as.numeric()
  # 
  # if (number_unique_id > 2){
  #   cat("id: ", pid_list[i], "/n")
  #   browser()
  #   
  # }
  
  # })
}

rd <- new_rd

rd$trip_id <- rd$tid
rd$participant_id <- rd$pid

rd$tid <- rd$pid <- NULL

#write_csv(rd, "data/local/sao_paulo/trips_sao_paulo_expanded.csv")

#require(tidyverse)

#rd <- read_csv("data/local/sao_paulo/trips_sao_paulo_expanded.csv")

# Rename van mode to mini-bus
rd$trip_mode[rd$trip_mode == "van"] <- "bus"

rd$stage_mode <- rd$trip_mode
rd$stage_distance <- rd$trip_distance
rd$stage_duration <- rd$trip_duration

rd <- mutate(rd, total_short_walk_time = walking_time_origin + walking_time_dest)
rd_pt <- filter(rd, trip_mode %in% c('subway', 'bus', 'train', 'van'))
rows_list <- list()
ind <- 1

for (i in 1:nrow(rd_pt)){
  nr <- rd_pt[i, ]
  if (!is.na(nr$total_short_walk_time) && nr$total_short_walk_time > 0){
    nr$stage_mode <- "walk_to_pt"
    nr$stage_duration <- nr$total_short_walk_time
    #nr$stage_distance <- nr$total_short_walk_time / 60 * 4.8
    rows_list[[ind]] <- nr
    ind <- ind + 1
  }
}

df <- plyr::ldply (rows_list, data.frame)
rd <- rbind(rd, df)

rd$walking_time_origin <- NULL
rd$walking_time_dest <- NULL
rd$trip_distance_cat <- NULL
rd$person_weight <- NULL
rd$total_short_walk_time <- NULL

rd[!is.na(rd$stage_mode) & rd$stage_mode == "walk_to_pt",]$stage_distance <-
  rd[!is.na(rd$stage_mode) & rd$stage_mode == "walk_to_pt",]$stage_duration / 60 * 4.8

rd$trip_distance <- ave(rd$stage_distance, rd$trip_id, FUN=sum)
rd$trip_duration <- ave(rd$stage_duration, rd$trip_id, FUN=sum)

rd$sex[rd$sex == 1] <- "Male"
rd$sex[rd$sex == 2] <- "Female"

# Save to a local var
b <- rd

write_csv(rd, "inst/extdata/local/sao_paulo/trips_sao_paulo.csv")

# Plot distance distribution
ggplot(sd %>% 
         group_by(trip_distance_cat) %>% 
         summarise(count = n()) %>% 
         mutate(perc = round(count/sum(count) * 100, 1)), 
       aes(x = trip_distance_cat, y = perc)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label = perc), position = position_dodge(width=0.9), vjust=-0.25, color = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "", y = "percentage(%)", title = "Main Mode Distance distribution")

# Plot mode distribution
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
  labs(x = "", y = "percentage(%)", title = "Main Mode distribution - without weights")


# Read raw pa for csv and store the filtered pa
{
  
  pa <- read_csv("data/local/sao_paulo/pa_raw_sao_paulo.csv")
  # Rename columns
  pa <- rename(pa, age = idade_socio)
  pa <- rename(pa, sex = sexo)
  
  # Convert numeric to string sex
  pa[pa$sex == 1, ]$sex <- "Male"
  pa[pa$sex == 2, ]$sex <- "Female"
  
  pa <- dplyr::select(pa, id, age, sex, ends_with("met"))
  
  pa[ is.na(pa) ] <- 0
  write_csv(pa, "data/local/sao_paulo/pa_sao_paulo.csv")
  
}