#' ---
#' title: "Preprocessing of Mexico's travel dataset. Most of it comes from Lambed's code"
#' author: "Lambed and Daniel"
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#' ---

#' # **Understanding phase**
#+ warning=FALSE, message=FALSE, echo=FALSE
# Loading libraries
library(foreign) 
library(kableExtra)
library(readxl)
library(tidyverse)


#+ warning=FALSE, message=FALSE, echo=FALSE
# Cleaning workspace
rm(list = ls());gc()

# Printing options
options(scipen = 50)

#' This file is based on the script "travel_survey.R". It has the same code but
#' I added some comments.
#' 
#' 
#' ## Documentation 
#' Documentation is located in ".../Mexico/MexicoCity/Encuesta2017/Documentacion/"
#' . These files were found in https://www.inegi.org.mx/programas/eod/2017/
#'
#' From now on: 
#+ warning=FALSE, message=FALSE, echo=FALSE
data.frame(
  Reference = c("File1", "File2", "File3"),
  Description = c("Report with only final results (slides)",
                  "Methodology",
                  "Conceptual design"),
  Title = c("Encuesta Origen-Destino en hogares de la zona metropolitana del valle de Mexico",
            "Encuesta Origen-Destino en hogares de la zona metropolitana del valle de Mexico 2017 EOD - Documento metodologico",
            "Encuesta Origen-Destino en hogares de la zona metropolitana del valle de Mexico 2017 EOD - Diseño conceptual"),
  File = c("resultados_eod_2017.pdf",
           "Metodologia/metodologia_eod_2017.pdf",
           "Metodologia/conceptual_eod_2017.pdf")
) %>% kbl() %>% kable_classic()

#' ## Definition of a trip
#' 1. *Trip:* All trips with no constraints of duration.
#' Definition of trip in page 21 of **File3**: *Movement from one part to another made by one person with a specific reason/motive, using one or multiple modes of transport.**
#' 
#' 2. *Collection:* Trips collected in this survey correspond to those made the 
#' during the week (Tuesday, Wednesday, Thursday) and also the Saturday.
#' **Results presented are separately for weekdays and weekends**
#' 
#' 
#' ## Replicate main results from raw datasets
#' To create this report I have to set the full route of each file, regardless
#' the location of the working directory.
#'
#' Loading standardize_modes function:
#+ warning=FALSE, message=FALSE
#external_route <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/WorldBank/ITHIM-R/code/producing_trips_rd/"
#external_route <- "CAMBRIDGE_ROUTE V DRIVE"
#source(paste0(external_route, "used_functions.R")) 

#' It didn't work with knitr so I had to paste this function here. 
#' 
#' **Note: Before running this script, make sure this function is up to date**
standardize_modes <- function(trip, mode){
  # Read lookup table
  smodes <- read_csv('C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/data/global/modes/standardized_modes.csv')
  # Separate rows 
  smodes <- smodes %>% separate_rows(original, sep = ';')
  
  smodes <- smodes %>% 
    mutate(across(where(is.character), str_trim))
  
  if (length(mode) == 1) {
    if (mode == 'stage')
      trip$stage_mode <- smodes$exhaustive_list[match(trip$stage_mode, smodes$original)]
    else
      trip$trip_mode <- smodes$exhaustive_list[match(trip$trip_mode, smodes$original)]
  }else if (length(mode) == 2) {
    if (all(mode %in% c('stage', 'trip'))) {
      trip$trip_mode <- smodes$exhaustive_list[match(trip$trip_mode, smodes$original)]
      trip$stage_mode <- smodes$exhaustive_list[match(trip$stage_mode, smodes$original)]
    }
  }
  
  return(trip)
  
}

#' ### Importing datasets
#' 
#' #### Importing from V drive
#' I'm not importing directly from V drive because it takes too long. This is 
#' why this piece of code is commented.
# # data
# person_0 <- read_csv('J://Studies//MOVED//HealthImpact//Data//TIGTHAT//Mexico//Travel surveys//Mexico City 2017//Databases//eod_2017_csv//tsdem_eod2017//conjunto_de_datos//tsdem.csv')
# trip_0 <- read_csv('J://Studies//MOVED//HealthImpact//Data//TIGTHAT//Mexico//Travel surveys//Mexico City 2017//Databases//eod_2017_csv//tviaje_eod2017//conjunto_de_datos//tviaje.csv')
# stage_0 <- read_csv('J://Studies//MOVED//HealthImpact//Data//TIGTHAT//Mexico//Travel surveys//Mexico City 2017//Databases//eod_2017_csv//ttransporte_eod2017//conjunto_de_datos//ttransporte.csv')
# 
# #lookups
# trip_purpose <- read_excel("J://Studies//MOVED//HealthImpact//Data//TIGTHAT//Mexico//Travel surveys//Mexico City 2017//Databases//eod_2017_csv//lookup.xlsx",'trip_purpose')
# stage_mode <- read_excel("J://Studies//MOVED//HealthImpact//Data//TIGTHAT//Mexico//Travel surveys//Mexico City 2017//Databases//eod_2017_csv//lookup.xlsx",'stage_mode')
# sex <- bind_cols(sex=c("Male", "Female"), sexo = 1:2)

#' #### Importing from local directory
route <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/Mexico/MexicoCity/Encuesta2017/Microdatos/CSV/"

#+ warning=FALSE, message=FALSE, cache=TRUE
# People
person_0 <- read_csv(paste0(route, "tsdem_eod2017/conjunto_de_datos/tsdem.csv"),
                    guess_max = 100000)
# Trips
trip_0 <- read_csv(paste0(route, "tviaje_eod2017/conjunto_de_datos/tviaje.csv"),
                   guess_max = 100000)
# Stages
stage_0 <- read_csv(paste0(route, "ttransporte_eod2017/conjunto_de_datos/ttransporte.csv"), guess_max = 100000)

#lookups
trip_purpose <- read_excel(paste0(route, "lookup.xlsx"),'trip_purpose')
stage_mode <- read_excel(paste0(route, "lookup.xlsx"),'stage_mode')
sex <- bind_cols(sex = c("Male", "Female"), sexo = 1:2)

#' # **Preprocessing phase**
#' ## Filtering people from Mexico city only
#' Since the survey was conducted in multiple municipalities and injuries only 
#' come from the main city, then I will analyze only trips from households 
#' located in the main city.
person <- person_0 %>% 
  filter(ent == "09") %>%  # Filtering people that live only in the city 
  left_join(sex) %>%
  mutate(cluster_id = 1) %>% # select the appropriate cluster number
  select(cluster_id,id_hog, id_soc, sex, edad, factor) 
## all ind id's sex and age

#' ## Classification and translation of trip modes and purpose
#' Even though there's enough information at stage level, I still need to define
#' the main mode for each trip. So I made a hierarchy to get it and translate it.
#' This is going to be different from Lambed's code because he defined the main
#' mode based on stage duration.
#+ warning=FALSE, message=FALSE, cache=TRUE
main_mode <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/Mexico/Hierarchy.xlsx", sheet = "MexicoCity")
purpose <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/Mexico/Hierarchy.xlsx", sheet = "MexicoCity_purpose")

#' ## Row for each stage, translate trip_mode and create duration
#' First I create trip duration and then use the hierarchy to get the main mode.
trip <- trip_0 %>% 
  mutate(trip_duration_d = (as.numeric(p5_10_1) - as.numeric(p5_9_1))*60 + 
           (as.numeric(p5_10_2) - as.numeric(p5_9_2)),
         trip_duration_d = ifelse(trip_duration_d < (-450), 1440 + trip_duration_d, trip_duration_d),
         trip_duration_d = ifelse(trip_duration_d < 0, 0 - trip_duration_d, trip_duration_d),
         trip_duration_d = ifelse(trip_duration_d == 0, NA, trip_duration_d),
         # Defining hierarchy
         auto = ifelse(p5_14_01 == 1, 
                       main_mode$Hierarchy[main_mode$Code == "01"], 99),
         colectivo = ifelse(p5_14_02 == 1, 
                            main_mode$Hierarchy[main_mode$Code == "02"], 99),
         taxi_app = ifelse(p5_14_03 == 1, 
                           main_mode$Hierarchy[main_mode$Code == "03"], 99),
         taxi = ifelse(p5_14_04 == 1, 
                       main_mode$Hierarchy[main_mode$Code == "04"], 99),
         metro = ifelse(p5_14_05 == 1, 
                        main_mode$Hierarchy[main_mode$Code == "05"], 99),
         autobus_m1 = ifelse(p5_14_06 == 1, 
                             main_mode$Hierarchy[main_mode$Code == "06"], 99),
         bicicleta = ifelse(p5_14_07 == 1, 
                            main_mode$Hierarchy[main_mode$Code == "07"], 99),
         autobus = ifelse(p5_14_08 == 1, 
                          main_mode$Hierarchy[main_mode$Code == "08"], 99),
         moto = ifelse(p5_14_09 == 1, 
                       main_mode$Hierarchy[main_mode$Code == "09"], 99),
         trolebus = ifelse(p5_14_10 == 1, 
                           main_mode$Hierarchy[main_mode$Code == "10"], 99),
         metrobus = ifelse(p5_14_11 == 1, 
                           main_mode$Hierarchy[main_mode$Code == "11"], 99),
         tren_ligero = ifelse(p5_14_12 == 1, 
                              main_mode$Hierarchy[main_mode$Code == "12"], 99),
         tren_sub = ifelse(p5_14_13 == 1, 
                           main_mode$Hierarchy[main_mode$Code == "13"], 99),
         camina = ifelse(p5_14_14 == 1, 
                         main_mode$Hierarchy[main_mode$Code == "14"], 99),
         mexicable = ifelse(p5_14_15 == 1, 
                            main_mode$Hierarchy[main_mode$Code == "15"], 99),
         bicitaxi = ifelse(p5_14_16 == 1, 
                           main_mode$Hierarchy[main_mode$Code == "16"], 99),
         mototaxi = ifelse(p5_14_17 == 1, 
                           main_mode$Hierarchy[main_mode$Code == "17"], 99),
         escolar = ifelse(p5_14_18 == 1, 
                          main_mode$Hierarchy[main_mode$Code == "18"], 99),
         personal = ifelse(p5_14_19 == 1, 
                           main_mode$Hierarchy[main_mode$Code == "19"], 99),
         otro = ifelse(p5_14_20 == 1, 
                       main_mode$Hierarchy[main_mode$Code == "20"], 99),
         trip_purpose = purpose$ITHIM[
           match(p5_13, purpose$Code)]) %>% 
  # Now compute the main mode by looking at the hierarchy
  rowwise() %>% mutate(
    main_modes = min(auto, colectivo, taxi_app, taxi, metro, autobus_m1,
                     bicicleta, autobus, moto, trolebus, metrobus,
                     tren_ligero,tren_sub, camina, mexicable, bicitaxi,
                     mototaxi, escolar, personal, otro, na.rm = T),
    trip_mode = main_mode$ITHIM[match(main_modes, main_mode$Hierarchy)],
    trip_id_paste = paste(id_soc, id_via, sep = "-")) %>% 
  #left_join(trip_purpose) %>% 
  select(id_soc,id_via, trip_duration_d, trip_purpose, trip_mode, trip_id_paste)
# Variable p5_3 indicates whether the trip was made during the week (=1) or saturday (=2)

#' Translate stage mode and compute stage duration
stage <- stage_0 %>% 
  mutate(#p5_14 = as.numeric(p5_14), # make mode code as numeric for binding
    stage_mode = main_mode$ITHIM[match(p5_14, main_mode$Code)],
    stage_duration = as.numeric(p5_16_1_1)*60 + as.numeric(p5_16_1_2),
    stage_duration = ifelse(p5_16_1_1 == "99", NA,stage_duration)) %>%
  #left_join(stage_mode) %>%
  #mutate(mode = stage_mode) %>% 
  #left_join(mode_speed) %>% 
  #left_join(group_by(.,id_via) %>% 
  #              summarise(trip_mode = ifelse(is.na(stage_mode[which.is.max(stage_duration)]), 
  #                                           stage_mode[which.is.max(mode_speed)],
  #                                           stage_mode[which.is.max(stage_duration)] ))) %>% 
  select(id_via, id_tra, p5_3, stage_mode, stage_duration)
# Variable p5_3 indicates whether the trip was made during the week (=1) or saturday (=2)


#' ## Create variables for quick report
#' I need to create some variables to run the report that Lambed developed in 
#' the function *quality_check*.
#bind all datasets and retain only useful ones  
mexico_city <- person %>% 
  left_join(trip) %>% 
  left_join(stage) %>%
  rename(household_id = id_hog, 
         participant_id = id_soc,
         participant_wt = factor,
         trip_id = id_via, 
         age = edad, stage_id = id_tra) %>% 
  group_by(household_id, participant_id, trip_id, trip_mode) %>% 
  mutate(trip_duration = sum(stage_duration, na.rm =T)) %>% 
  ungroup()
# There are differences between the duration computed from the trip dataset and
# the duration computed from the stage dataset

#' Since we decided to use trips from both weekdays and weekends, then I create
#' new household ID, participant ID, trip ID and stage ID to those trips made
#' during the weekends. In this way, we avoid increasing the number of trips 
#' made by a person who had trips in weekdays and weekends.
# Replacing IDs from trips made on Saturdays
# View(mexico_city %>% group_by(p5_3) %>%
#   summarise(participant_id_min = min(participant_id),
#             participant_id_max = max(participant_id),
#             household_id_min = min(household_id),
#             household_id_max = max(household_id),
#             trip_id_min = min(trip_id),
#             trip_id_max = max(trip_id),
#             stage_id_min = min(stage_id),
#             stage_id_max = max(stage_id)))

mexico_city_v2 <- mexico_city %>% 
  mutate(
    participant_id = ifelse(!is.na(p5_3) & p5_3 == 2, 
                            participant_id + 1000000, participant_id),
    household_id = ifelse(!is.na(p5_3) & p5_3 == 2, 
                          household_id + 1000000, household_id),
    trip_id = ifelse(!is.na(p5_3) & p5_3 == 2, 
                     trip_id + 10000000, trip_id),
    stage_id = ifelse(!is.na(p5_3) & p5_3 == 2, 
                      stage_id + 10000000, stage_id))

# total_participant <-
#   mexico_city_v2 %>%
#   count(cluster_id, household_id, participant_id) %>%
#   nrow
# 
# people_with_trip <-
#   mexico_city_v2 %>%
#   filter(!is.na(trip_id)) %>%
#   count(cluster_id, household_id, participant_id) %>%
#   nrow
# 
# proportion_people_with_trips <- round(people_with_trip*100/total_participant)

trip <- mexico_city_v2
trip$meta_data <- NA
trip$meta_data[1] <- 20976700
trip$meta_data[2] <- 19239
trip$meta_data[3] <- "Travel Survey"
trip$meta_data[4] <- 2017
trip$meta_data[5] <- "1 day"
trip$meta_data[6] <- "Yes" #Stage level data available
trip$meta_data[7] <- "All purpose"#Overall trip purpose
trip$meta_data[8] <- "Yes" # Short walks to PT
trip$meta_data[9] <- "No" # Distance available
trip$meta_data[10] <- "" # missing modes

#' Export dataset to make the report
#quality_check(trip)
write.csv(trip, "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/data/local/mexico_city/mexico_city_trip.csv", row.names = F)

#' ## Standardize trip modes
#' There's already a function that standardize these modes so the package can use
#' these trips. I made sure to use translate trip modes so that the function
#' works perfectly (take a look at the *original* variable of *smodes* dataframe
#' in this function).
## Expand trip dataset using participant weight
#trip <- read_csv("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/data/local/mexico_city/mexico_city_trip.csv")

# Load helpful functions
#source("code/producing_trips_rd/used_functions.R")

# Standardized travel modes
trip <- standardize_modes(trip, mode = c('stage', 'trip'))

## Convert age as character to integer

trip$age <- as.integer(trip$age)

# Source functions
#source("code/producing_trips_rd/used_functions.R")

rd <- trip

#' ## Creating again IDs

# Remove extra columns
#rd$X1 <- NULL

rd$participant_id <- as.integer(as.factor(with(rd, paste(cluster_id, household_id, participant_id, sep = "_"))))

rd$trip_id <- as.integer(as.factor(with(rd, paste(cluster_id, household_id, participant_id, trip_id,  sep = "_"))))

#' # **Exporting phase**
#' ## Variables to export
#' Now I filter the columns I need
rd <- rd %>% dplyr::select(participant_id, age, sex, trip_id, trip_mode,
                           trip_duration, stage_id, stage_mode, stage_duration)

#' ## Export dataset
write_csv(rd, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/inst/extdata/local/mexico_city/trips_mexico_city.csv')
