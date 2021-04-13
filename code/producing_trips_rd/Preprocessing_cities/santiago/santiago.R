#' ---
#' title: "Preprocessing of Santiago's travel dataset. Most of it comes from Lambed's code"
#' author: "Lambed and Daniel"
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#' ---

#' # **Understanding phase**
#+ warning=FALSE, message=FALSE, echo=FALSE
# Loading libraries
library(haven) 
library(kableExtra)
library(readxl)
library(tidyverse)
library(nnet) # To use which.is.max function


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
#' Documentation is located in ".../Chile/Santiago/Trips/Reports/". I downloaded
#' these files from v drive and can be downloaded from http://www.sectra.gob.cl/encuestas_movilidad/encuestas_movilidad.htm
#'
#' From now on: 
#+ warning=FALSE, message=FALSE, echo=FALSE
data.frame(
  Reference = c("File1", "File2", "File3"),
  Description = c("Technical report and final results Vol. II",
                  "Report with only final results",
                  "Technical report and final results Vol. I"),
  Title = c("Encuesta Origen Destino Santiago 2021 Informe Final Volumen II",
            "Informe ejecutivo Origen Destino de Viajes 2012 ",
            "Encuesta Origen Destino Santiago 2021 Informe Final Volumen I"),
  File = c("Actualizacion_recolecc_STU_Santiago_IX Etapa_EOD Stgo 2012_Inf_Final vol 2.pdf",
           "Actualizacion_recolecc_STU_Santiago_IX Etapa_EOD Stgo 2012_Inf_Ejec.pdf",
           "Actualizacion_recolecc_STU_Santiago_IX Etapa_EOD Stgo 2012_Inf_Final vol I.pdf")
) %>% kbl() %>% kable_classic()

#' ## Definition of a trip (pendiente)
#' 1. *Trip:* All trips that are longer than 3 minutes for all modes except
#' walking, where trips should be longer or equal to 15 minutes. 
#' Definition of trip in page 120 of **File2**: *Movement from one part to another made by one person with a specific reason/motive, A definite hour of start and end, a mode of transport, and a duration greater than 3 minutes. Or a movement from one part to another with reason/motive work or study of any duration.*
#' 
#' 2. *Collection:* Trips collected in this survey correspond to those made the 
#' during the week (Dia Laboral Normal) and also the Saturday, Sunday and
#' Holidays.
#' **Results presented are separately for weekdays and weekends**
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


## mode_speed - pls add other modes and their average mode speeds if needed.
mode_speed <- data.frame(
  mode = c("bicycle","bus","car","metro", "motorcycle", "other", "rickshaw",
           "taxi","train","truck", "van","walk" ),
  mode_speed = c(15, 15, 25, 25, 25,21 ,25,25, 30,25,25, 5 ))

expand_using_weights <- function(trip, normalize_by = 10){
  # Save it in a local var
  rd <- trip
  # Expand by household IDs
  
  # Round participant weight
  rd <- rd %>% mutate(w = if_else(is.na(participant_wt), 0, round(participant_wt)))
  
  rd <- rd %>% mutate(w = round(w / normalize_by))
  
  # Subtract 1 from non-zero entries, and set weight to 1 for (0, 1, 2)
  rd <- rd %>% mutate(w = if_else(w > 1, w - 1, 1))
  
  # Expand it according to weights, and assign IDs to the newly expanded rows
  exp <- rd %>% uncount(w, .id = "pid")
  
  # Arrange df
  rd <- exp %>% arrange(cluster_id, household_id, participant_id, trip_id)
  
  # Create participant_id as a combination of cluster_id, household_id, participant_id, and pid (the newly expanded id)
  rd$participant_id <- as.integer(as.factor(with(rd, paste(cluster_id, household_id, participant_id, pid, sep = "_"))))
  
  # Create trip_id as a combination of cluster_id, household_id, participant_id, pid (the newly expanded id) and trip_id
  rd$trip_id <- as.integer(as.factor(with(rd, paste(cluster_id, household_id, participant_id, pid, trip_id, sep = "_"))))
  
  return(rd)
}

#' ### Importing datasets
#' 
#' #### Importing from V drive
#' I'm not importing directly from V drive because it takes too long. This is 
#' why this piece of code is commented.
# person_0 <- read.csv("J://Studies//MOVED//HealthImpact//Data//TIGTHAT//Chile//Travel Surveys//Santiago//Persona.csv")
# trip_0 <- read.csv("J://Studies//MOVED//HealthImpact//Data//TIGTHAT//Chile//Travel Surveys//Santiago//Viaje.csv")
# stage_0 <- read.csv("J://Studies//MOVED//HealthImpact//Data//TIGTHAT//Chile//Travel Surveys//Santiago//stages.csv")
# 
# age <- read.csv('J://Studies//MOVED//HealthImpact//Data//TIGTHAT//Chile//Travel Surveys//Santiago//EdadPersonas.csv')
# trip_purpose <- read.csv("J://Studies//MOVED//HealthImpact//Data//TIGTHAT//Chile//Travel Surveys//Santiago//lookup_trip_purpose.csv")
# trip_mode <- read.csv("J://Studies//MOVED//HealthImpact//Data//TIGTHAT//Chile//Travel Surveys//Santiago//lookup_trip_mode.csv")
# stage_mode <- read.csv("J://Studies//MOVED//HealthImpact//Data//TIGTHAT//Chile//Travel Surveys//Santiago//lookup_stage_mode.csv")
# sex <- bind_cols(sex= c("Male", "Female"), Sexo = c(1,2))

#' #### Importing from local directory
route <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/Chile/Santiago/Trips/CSV/"
hh_0 <- read_xlsx(paste0(route, "Hogar.xlsx"), guess_max = 100000)
person_0 <- read_xlsx(paste0(route, "Persona.xlsx"), guess_max = 100000)
trip_0 <- read_xlsx(paste0(route, "Viaje.xlsx"), guess_max = 100000)
stage_0 <- read_xlsx(paste0(route, "Etapa.xlsx"), guess_max = 100000)

age <- read_xlsx(paste0(route, "EdadPersonas.xlsx"), guess_max = 100000)

#lookups
sex <- bind_cols(sex = c("Male", "Female"), Sexo = c(1,2))

#' # **Preprocessing phase**
#' ## Filtering people from Santiago city
#' Since the survey was conducted in 45 comunas (page 3 of **File2**) and 
#' injuries information is at city level then I should filter only households
#' located within the city. To identify comunas that don't belong to Santiago
#' I took as reference "Tabla 2" page 14 of **File 3**.
person <- person_0 %>% 
  left_join(hh_0[, c("Hogar", "Comuna")], by = "Hogar") %>% 
  filter(!Comuna %in% c("COLINA", "LAMPA", "SAN BERNARDO", "CALERA DE TANGO",
                        "BUIN", "PUENTE ALTO", "PIRQUE", "MELIPILLA",
                        "TALAGANTE", "EL MONTE", "ISLA DE MAIPO", 
                        "PADRE HURTADO", "PEÑAFLOR")) %>% 
  mutate(cluster_id = 1,
         household_id = Hogar,
         participant_wt = Factor) %>% 
  left_join(rename(age, age = Edad)) %>% 
  left_join(sex) %>% 
  select(cluster_id, household_id, participant_wt, Persona, sex, age)

#' ## Classification and translation of trip modes and purpose
#' Even though there's enough information at stage level, I still need to define
#' the main mode for each trip. So I made a hierarchy to get it and translate it.
#' This is going to be different from Lambed's code because he defined the main
#' mode based on stage duration.
#+ warning=FALSE, message=FALSE, cache=TRUE
trip_purpose <- read_csv(paste0(route, "lookup_trip_purpose.csv"))
trip_mode <- read_csv(paste0(route, "lookup_trip_mode.csv"))
stage_mode <- read_csv(paste0(route, "lookup_stage_mode.csv"))

#' ## Row for each stage, translate trip_mode and create duration
#' First I create identify which trips were made during summer season and in
#' regular season as well as during the week or weekends. Then I translate trip
#' mode and trip purpose. Finally I filter trips on regular season
trip <- trip_0 %>% 
  mutate(workday = ifelse(!is.na(FactorLaboralNormal), 1, 0),
         saturday = ifelse(!is.na(FactorSabadoNormal), 1, 0),
         sunday = ifelse(!is.na(FactorDomingoNormal), 1, 0),
         workday_summer = ifelse(!is.na(FactorLaboralEstival), 1, 0),
         weekend_summer = ifelse(!is.na(FactorFindesemanaEstival), 1, 0),
         regular_season = ifelse(workday == 1, 1,
                                 ifelse(saturday == 1, 2,
                                        ifelse(sunday == 1, 3, 0)))) %>% 
  left_join(trip_purpose) %>% 
  left_join(trip_mode) %>% 
  # Filtering trips only in regular season
  filter(regular_season %in% c(1,2,3)) %>% 
  select(Persona, Viaje, TiempoViaje, trip_purpose, trip_mode, regular_season)

#' Translate stage mode and compute stage duration.
#' 
#' Here the trip mode is defined based on mode speed. This can be changed to a 
#' hierarchy
stage <- stage_0 %>% 
  left_join(stage_mode) %>% 
  select(Persona, Viaje, Etapa, stage_mode)

trip <- person %>%
  left_join(trip) %>% 
  left_join(stage) %>% 
  rename(participant_id = Persona, trip_id = Viaje, 
         stage_id = Etapa, trip_duration = TiempoViaje)

#trip with "other" mode
trip_1 <- 
  trip %>%
  filter(trip_mode == "other") %>% 
  mutate(mode = stage_mode) %>% 
  left_join(mode_speed) %>% 
  group_by(cluster_id, household_id, trip_id) %>% 
  mutate(trip_mode = stage_mode[which.is.max(mode_speed)]) %>% 
  select(-mode, -mode_speed) 

#trip with modes other than "other"
trip_2 <- 
  trip %>% 
  setdiff(
    trip %>% 
      filter(trip_mode == "other")
  )
#bind rows  
santiago <- bind_rows(trip_2, trip_1, .id = NULL)

#' ## Create variables for quick report
#' I need to create some variables to run the report that Lambed developed in 
#' the function *quality_check*.
#' 
#' Before doing this I have to make sure that trips in weekends have different
#' IDs to avoid increasing the daily number of trips per person.
# Replacing IDs from trips made on Saturdays
# View(santiago %>% group_by(regular_season) %>%
#        summarise(participant_id_min = min(participant_id, na.rm = T),
#                  participant_id_max = max(participant_id, na.rm = T),
#                  household_id_min = min(household_id, na.rm = T),
#                  household_id_max = max(household_id, na.rm = T),
#                  trip_id_min = min(trip_id, na.rm = T),
#                  trip_id_max = max(trip_id, na.rm = T),
#                  stage_id_min = min(stage_id, na.rm = T),
#                  stage_id_max = max(stage_id, na.rm = T)))

# There are no people with trips collected in weekdays and weekends, so I don't
# have to modify any id as in mexico city.
aux <- person_0 %>% 
  mutate(multiple_days = ifelse(!is.na(Factor_LaboralNormal) & 
                                  !is.na(Factor_SábadoNormal) & 
                                  !is.na(Factor_DomingoNormal), 1, 0))
sum(aux$multiple_days)

trip <- santiago
trip$meta_data <- NA
trip$meta_data[1] <- 7164400
trip$meta_data[2] <- 23929 
trip$meta_data[3] <- "Travel Survey"
trip$meta_data[4] <- 2012
trip$meta_data[5] <- "1 day"
trip$meta_data[6] <- "Yes" #Stage level data available
trip$meta_data[7] <- "All purpose"#Overall trip purpose
trip$meta_data[8] <- "Yes" # Short walks to PT
trip$meta_data[9] <- "No" # Distance available
trip$meta_data[10] <- "train" # missing modes


#' Export dataset to make the report
#quality_check(trip)
write.csv(trip, "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/data/local/santiago/santiago_trip.csv", row.names = F)

#' ## Standardize trip modes
#' There's already a function that standardize these modes so the package can use
#' these trips. I made sure to use translate trip modes so that the function
#' works perfectly (take a look at the *original* variable of *smodes* dataframe
#' in this function).
## Expand trip dataset using participant weight
#trip <- read_csv("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/data/local/santiago/santiago_trip.csv")

# Load helpful functions
#source("code/producing_trips_rd/used_functions.R")

# Standardized travel modes
trip <- standardize_modes(trip, mode = c('stage', 'trip'))

# Expand by household IDs
rd <- expand_using_weights(trip, normalize_by = 10)

#' ## Creating again IDs

# Remove extra columns
#rd$X1 <- NULL

rd$participant_id <- as.integer(as.factor(with(rd, paste(cluster_id, household_id, participant_id, pid, sep = "_"))))

rd$trip_id <- as.integer(as.factor(with(rd, paste(cluster_id, household_id, participant_id, pid, trip_id,  sep = "_"))))

#' # **Exporting phase**
#' ## Variables to export
#' Now I filter the columns I need
# Reorder and select columns
rd1 <- rd %>% dplyr::select(participant_id, age, sex, trip_id, trip_mode,
                            trip_duration, stage_id, stage_mode)

rd2 <- slice_sample(rd1, prop = 0.1)

#' ## Export dataset
write_csv(rd2, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/inst/extdata/local/santiago/trips_santiago.csv')
