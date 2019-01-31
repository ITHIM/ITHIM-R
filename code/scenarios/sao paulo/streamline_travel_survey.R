# Clear workspace
rm (list = ls())

# Load libraries
library(tidyverse)
library(plotly)

# Read sao paulo's travel survey
rd <- read_csv("data/local/sao paulo/trips_sao_paulo.csv")

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
rd <- select(rd, participant_id, age, sex, trip_id, total_trips,
             walking_time_origin,
             walking_time_dest,
             trip_duration,
             trip_mode,
             trip_distance,
             trip_distance_cat,
             row_id,
             person_weight)

rd$pid <- -1
id <- 1
pid_list <- unique(rd$participant_id)
for(i in 1:length(pid_list)) {
  
  # require(profvis)
  # profvis({
  #   #your code here
  pg <- filter(rd, participant_id == pid_list[i])
  count <- pg$person_weight[1] - 1
  d <- bind_rows(replicate((count), pg, simplify = FALSE))
  d <- arrange(d, trip_id)
  rd[rd$participant_id == pid_list[i],]$pid <- id
  d$pid <- rep((id + 1):(id + count), nrow(pg))
  id <- id + count
  rd <- bind_rows(rd, d)
  
  # })
}


# Define distance categories
dist_cat <- c("0-6 km", "7-9 km", "10+ km")

# Initialize them
rd$trip_distance_cat <- NULL
rd$trip_distance_cat[rd$trip_distance > 0 & rd$trip_distance < 7] <- dist_cat[1]
rd$trip_distance_cat[rd$trip_distance >= 7 & rd$trip_distance < 10] <- dist_cat[2]
rd$trip_distance_cat[rd$trip_distance >= 10] <- dist_cat[3]

## Sample 1k rows for easier calculations
# set random seed
set.seed(40)
sd <- sample_n(rd, 1000)
sd %>% group_by(trip_mode) %>% summarise(round = (n() / nrow(.) * 100))

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
ggplot(sd %>% 
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


#sd's unique modes
# [1] NA          "bus"       "subway"    "car"       "motorbike" "bicycle"   "train"     "walk"      "van"      
# [10] "others"    "taxi"

#accra's unique modes
# [1] "99"          "Bus"         "Taxi"        "Walking"     "Train"       "Private Car" "Other"      
# [8] "Unspecified" "Bicycle"

sd[sd$trip_mode == "bus",]$trip_mode <- "Bus"
sd[is.na(sd$trip_mode),]$trip_mode <- "99"
sd[sd$trip_mode == "car",]$trip_mode <- "Private Car"
sd[sd$trip_mode == "bicycle",]$trip_mode <- "Bicycle"
sd[sd$trip_mode == "train",]$trip_mode <- "Train"
sd[sd$trip_mode == "walk",]$trip_mode <- "Walking"
sd[sd$trip_mode == "taxi",]$trip_mode <- "Taxi"
sd[sd$trip_mode == "motorcycle",]$trip_mode <- "Motorcycle"

write_csv(sd, "data/local/sao_paulo/trips_sao_paulo.csv")

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


# read sd
{
require(ithimr)
#require(tidyverse)

ithim_object <- run_ithim_setup(CITY = 'sao_paulo',
                      modes = c("Bus", "Private Car", "van", "Taxi", "Motorcycle", "Bicycle", "Walking"),
                      speeds = c(15, 21, 21, 21, 25, 15, 4.8),
                      # PATH_TO_LOCAL_DATA = 'data/local/sao_paulo/',
                      ADD_WALK_TO_BUS_TRIPS = F,
                      ADD_BUS_DRIVERS = F,
                      ADD_TRUCK_DRIVERS = F,
                      TEST_WALK_SCENARIO = F,
                      BUS_WALK_TIME= 5,
                      MMET_CYCLING = 4.63,
                      MMET_WALKING = 2.53,
                      PM_CONC_BASE = 50,  
                      PM_TRANS_SHARE = 0.225,
                      PA_DOSE_RESPONSE_QUANTILE = F,
                      AP_DOSE_RESPONSE_QUANTILE = F,
                      BACKGROUND_PA_SCALAR = 1,
                      INJURY_REPORTING_RATE = 1,
                      CHRONIC_DISEASE_SCALAR = 1,
                      RATIO_4W1_TO_4W2 = 10/12,
                      TAXI_TO_CAR_RATIO = 0.04,
                      BUS_TO_CAR_RATIO = 0.12,
                      TRUCK_TO_CAR_RATIO = 0.09,
                      MC_TO_CAR_RATIO = 0.2,
                      LDT_TO_CAR_RATIO = 0.21,
                      OTHER_TO_CAR_RATIO = 0.01)

ithim_object$outcomes <- run_ithim(ithim_object, seed = 1)

}

## plot results
{
  ## plot results
  result_mat <- colSums(ithim_object$outcome$hb$ylls[,3:ncol(ithim_object$outcome$hb$ylls)])
  columns <- length(result_mat)
  nDiseases <- columns/NSCEN
  ylim <- range(result_mat)
  x11(width = 8, height = 5); par(mfrow = c(2, 4))
  for(i in 1:nDiseases){
    if(i<5) {
      par(mar = c(1, 4, 4, 1))
      barplot(result_mat[1:NSCEN + (i - 1) * NSCEN], names.arg = '', ylim = ylim, las = 2, 
              main = paste0(last(strsplit(names(result_mat)[i * NSCEN], '_')[[1]])))
    }else{
      par(mar = c(5, 4, 4, 1))
      barplot(result_mat[1:NSCEN + (i - 1) * NSCEN], names.arg = SCEN_SHORT_NAME[c(1, 3:6)], ylim = ylim, las = 2, 
              main = paste0( last(strsplit(names(result_mat)[i * NSCEN], '_')[[1]])))
    }
  }
}

ithim_object$outcome <- list()

## what if: cleaner fleet !! not yet implemented
ithim_object$parameters <- ithim_setup_parameters()
ithim_object$outcome$cleaner_fleet <- run_ithim(ithim_object, seed = 1)

## what if: the roads are safer
ithim_object$parameters <- ithim_setup_parameters(INJURY_REPORTING_RATE = 2)
ithim_object$outcome$safety <- run_ithim(ithim_object, seed = 1)

## what if: the rate of chronic disease doubles
ithim_object$parameters <- ithim_setup_parameters(CHRONIC_DISEASE_SCALAR = 2)
ithim_object$outcome$chronic_disease <- run_ithim(ithim_object, seed = 1)

## what if: non-transport air pollution is half
ithim_object$parameters <- ithim_setup_parameters(PM_CONC_BASE = 30.625, PM_TRANS_SHARE = 0.3673469)
ithim_object$outcome$background_ap <- run_ithim(ithim_object, seed = 1)

## what if: non-transport physical activity is half
ithim_object$parameters <- ithim_setup_parameters(BACKGROUND_PA_SCALAR = 0.5)
ithim_object$outcome$background_pa <- run_ithim(ithim_object, seed = 1)


## plot results
result_list <- lapply(ithim_object$outcome,function(x)colSums(x$hb$ylls[,3:ncol(x$hb$ylls)]))
result_mat <- do.call(rbind, result_list)
columns <- ncol(result_mat)
nDiseases <- columns/NSCEN
for(i in 1:nDiseases){
  x11(width = 8, height = 5); par(mfrow = c(2, 3))
  ylim <- range(result_mat[, 1:NSCEN+(i-1)*NSCEN])
  for(j in 1:nrow(result_mat)){
    if(j<4) {
      par(mar = c(1, 4, 4, 1))
      barplot(result_mat[j, 1:NSCEN + (i - 1) * NSCEN], names.arg = '', ylim = ylim, las = 2, 
              main = paste0(rownames(result_mat)[j], ', ', last(strsplit(colnames(result_mat)[i * NSCEN], '_')[[1]])))
    }else{
      par(mar = c(5, 4, 4, 1))
      barplot(result_mat[j, 1:NSCEN + (i - 1) * NSCEN], names.arg = SCEN_SHORT_NAME[c(1, 3:6)], ylim = ylim, las = 2, 
              main = paste0(rownames(result_mat)[j], ',  ', last(strsplit(colnames(result_mat)[i * NSCEN], '_')[[1]])))
    }
  }
}
