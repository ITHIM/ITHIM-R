#' ---
#' title: "Preprocessing of Buenos Aires' travel dataset. Most of it comes from Lambed's code"
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
#' Documentation is located in ".../Argentina/BuenosAires/Trips/". I downloaded
#' these files from v drive.
#'
#' From now on: 
#+ warning=FALSE, message=FALSE, echo=FALSE
data.frame(
  Reference = c("File1"),
  Description = c("Final report"),
  Title = c("Resultados ENMODO"),
  File = c("Report.pdf")
) %>% kbl() %>% kable_classic()

#' ## Definition of a trip
#' 1. *Trip:* Moving from a origin to a destiny with a specific reason/motive 
#' (page 23, *File1*)
#' 
#' 2. *Collection:* Trips made in working days.
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

## mode_speed - pls add other modes and their average mode speeds if needed.
mode_speed <- data.frame(
  mode = c("bicycle","bus","car","metro", "motorcycle", "other", "rickshaw",
           "taxi","train","truck", "van","walk" ),
  mode_speed = c(15, 15, 25, 25, 25,21 ,25,25, 30,25,25, 5 ))

#' ### Importing datasets
#' 
#' #### Importing from V drive
#' I'm not importing directly from V drive because it takes too long. This is 
#' why this piece of code is commented.
# # data
# person_0 <- read_sav('J://Studies//MOVED//HealthImpact//Data//TIGTHAT//Argentina//WP1-TS//Buenos Aires//ENMODO_PERSONAS_pub_20121115.sav')
# trip_0 <- read_sav("J://Studies//MOVED//HealthImpact//Data//TIGTHAT//Argentina//WP1-TS//Buenos Aires//ENMODO_VIAJES_pub_20121115.sav")
# stage_0 <- read_sav("J://Studies//MOVED//HealthImpact//Data//TIGTHAT//Argentina//WP1-TS//Buenos Aires//ENMODO_ETAPAS_pub_20121115.sav")
# 
# #lookups
# trip_purpose <- read_excel("J://Studies//MOVED//HealthImpact//Data//TIGTHAT//Argentina//WP1-TS//Buenos Aires//lookup.xlsx",sheet = "trip_purpose", range = cell_cols("A:B"))
# stage_mode <- read_excel("J://Studies//MOVED//HealthImpact//Data//TIGTHAT//Argentina//WP1-TS//Buenos Aires//lookup.xlsx",sheet= "stage_mode", range = cell_cols("A:C"))# also ranks the modes

#' #### Importing from local directory
route <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/Argentina/BuenosAires/Trips/"

#+ warning=FALSE, message=FALSE, cache=TRUE
# People
person_0 <- read_sav(paste0(route, "ENMODO_PERSONAS_pub_20121115.sav"))
# Trips
trip_0 <- read_sav(paste0(route, "ENMODO_VIAJES_pub_20121115.sav"))
# Stages
stage_0 <- read_sav(paste0(route, "ENMODO_ETAPAS_pub_20121115.sav"))

#lookups
# trip_purpose <- read_excel(paste0(route, "lookup.xlsx"), sheet = 'trip_purpose',
#                            range = cell_cols("A:B"))
# stage_mode <- read_excel(paste0(route, "lookup.xlsx"), sheet = 'stage_mode',
#                          range = cell_cols("A:C"))

#' ### Number of people per partido
#' Compare this with what is mentioned in page 9 (Cuadro 2.1.1) of **File1**. 
names(person_0)
#sum(person_0$wt1) # Same as total in cuadro 2.1.1
#person_0 %>% group_by(PARTIDO) %>% summarise(total = sum(wt1)) %>% 
#    kbl() %>% kable_classic()
print(person_0 %>% group_by(PARTIDO) %>% summarise(total = sum(wt1)), n = 50)

#' # **Preprocessing phase**
#' ## Filtering people from Buenos Aires metropolitan area
#' Since the survey was conducted in the Autonomous City of Buenos Aires and 27
#' municipalities, and the jurisdiction of injuries is 
#' Greater BA (24 municipalities) + Autonomous BA then I have to filter out
#' "partidos" Escobar (252), Pilar (638) and Presidente Peron (648)
#keep relevant variables
person <- person_0[,c("PARTIDO","IDH","IDP", "EDAD", "SEXO","wt1")] %>% 
  filter(!PARTIDO %in% c(252, 638, 648))
#sum(person$wt1)

#' ## Classification and translation of trip modes and purpose
#' In the lookup file there's already a hierarchy defined by Lambed. The column
#' is called "rank"
#+ warning=FALSE, message=FALSE, cache=TRUE
trip_purpose <- read_excel(paste0(route, "lookup.xlsx"), sheet = 'trip_purpose',
                           range = cell_cols("A:B"))
stage_mode <- read_excel(paste0(route, "lookup.xlsx"), sheet = 'stage_mode',
                         range = cell_cols("A:C"))

#' ## Row for each stage, translate trip_mode and create duration
#' First I create trip duration and then select some variables
trip <- trip_0 %>% 
  mutate(trip_duration_1 = ((HORALLEG - HORASALI)%%24)*60 + 
           (MINLLEGA - MINSALID), 
         trip_duration_2 = difftime(HORAFIN,HORAINI,tz="GMT",units="mins"),
         trip_duration = ifelse(trip_duration_1 > 540, abs(trip_duration_2),
                                trip_duration_1)) %>% 
  left_join(trip_purpose) %>%
  select(IDP, IDV, trip_duration, trip_purpose)

#' Translate stage mode and compute stage duration.
#' 
#' Here the trip mode is defined based on a hierarchy (column "rank")
stage <- stage_0 %>% 
  left_join(stage_mode) %>%  #add mode names in english and ranks
  left_join(count(.,IDV)) %>%  # add number of stages for each trip --> to be used later
  left_join(trip[,c("IDV", "trip_duration")]) %>% #add trip duration
  mutate(stage_duration = ifelse(DURAMINU == 99 & n == 1, trip_duration, #get stage duration from trip duration
                                 ifelse(DURAMINU == 99 & n > 1, NA, 
                                        60*DURAHORA + DURAMINU))) %>% 
  left_join(group_by(., stage_mode) %>% 
              summarise(average_mode_time = mean(stage_duration, na.rm=T))) %>% 
  group_by(IDV) %>% 
  mutate(trip_mode = stage_mode[which.is.max(rank)], # add trip main mode
         average_mode_time = ifelse(DURAMINU == 99 & n > 1 & 
                                      "walk" %in% levels(as.factor(stage_mode)) &
                                      stage_mode == "walk", 1,
                                    ifelse(DURAMINU == 99 & n > 1 & 
                                      "walk" %in% levels(as.factor(stage_mode)) &
                                        stage_mode != "walk",
                                      5,average_mode_time))) %>% 
  left_join(group_by(.,IDV) %>% 
              summarise(sum_average = sum(average_mode_time))) %>% 
  mutate(stage_duration = ifelse(is.na(stage_duration), 
                                 round(trip_duration * average_mode_time / 
                                         sum_average),
                                 stage_duration)) %>% 
  {.[,c("IDP", "IDV", "IDE", "stage_mode", "stage_duration", "trip_mode")]}


#' ## Create variables for quick report
#' I need to create some variables to run the report that Lambed developed in 
#' the function *quality_check*.
#Join the three datasets and rename variables
trip <- person %>% 
  left_join(trip) %>% 
  left_join(stage) %>% 
  left_join(mode_speed %>% rename(trip_mode = mode)) %>%
  mutate(trip_distance = round(mode_speed*trip_duration/60)) %>% 
  select(-mode_speed) %>% 
  left_join(mode_speed %>% rename(stage_mode = mode)) %>% 
  mutate(stage_distance = round(mode_speed*stage_duration/60),
         sex = ifelse(SEXO == "Masculino", "Male", "Female")) %>% 
  rename(cluster_id = PARTIDO,
         household_id = IDH,
         participant_id = IDP,
         participant_wt = wt1,
         trip_id = IDV,
         age = EDAD,
         stage_id = IDE) %>% 
  select(cluster_id, household_id, participant_id, 
         participant_wt, age, sex, trip_id, trip_purpose, 
         trip_mode, trip_duration, stage_id,
         stage_mode, stage_duration)

trip$meta_data <- NA
trip$meta_data[1] <- 13381800
trip$meta_data[2] <- 23606 
trip$meta_data[3] <- "Travel Survey"
trip$meta_data[4] <- 2012
trip$meta_data[5] <- "1 day"
trip$meta_data[6] <- "Yes" #Stage level data available
trip$meta_data[7] <- "All purpose"#Overall trip purpose
trip$meta_data[8] <- "Yes" # Short walks to PT
trip$meta_data[9] <- "No" # Distance available
trip$meta_data[10] <- "" # missing modes


#' Export dataset to make the report
#quality_check(trip)
write.csv(trip, "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/data/local/buenos_aires/buenos_aires_trip.csv", row.names = F)

#' ## Standardize trip modes
#' There's already a function that standardize these modes so the package can use
#' these trips. I made sure to use translate trip modes so that the function
#' works perfectly (take a look at the *original* variable of *smodes* dataframe
#' in this function).
## Expand trip dataset using participant weight
#trip <- read_csv("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/data/local/buenos_aires/buenos_aires_trip.csv")

# Load helpful functions
#source("code/producing_trips_rd/used_functions.R")

# Standardized travel modes
trip <- standardize_modes(trip, mode = c('stage', 'trip'))

# Source functions
#source("code/producing_trips_rd/used_functions.R")

# Expand by household IDs
# rd <- expand_using_weights(trip, normalize_by = 20)
rd <- trip

# # Reduce filesize by slicing it to 10%
# rd <- slice_sample(rd, prop = 0.1)

#' ## Creating again IDs

# Remove extra columns
#rd$X1 <- NULL

rd$participant_id <- as.integer(as.factor(with(rd, paste(cluster_id, household_id, participant_id, sep = "_"))))

rd$trip_id <- as.integer(as.factor(with(rd, paste(cluster_id, household_id, participant_id, trip_id,  sep = "_"))))

#' # **Exporting phase**
#' ## Variables to export
#' Now I filter the columns I need
rd1 <- rd %>% dplyr::select(participant_id, age, sex, trip_id, trip_mode,
                            trip_duration, stage_id, stage_mode, stage_duration)

#' ## Export dataset
write_csv(rd1, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/inst/extdata/local/buenos_aires/trips_buenos_aires.csv')
