#' ---
#' title: "Preprocessing of Belo Horizonte's travel dataset. Most of it comes from Lambed's code"
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
#library(nnet) # To use which.is.max function


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
#' Documentation is located in ".../Brazil/BeloHorizonte/Trips/Documentation/". 
#' I downloaded these files from v drive.
#'
#' From now on: 
#+ warning=FALSE, message=FALSE, echo=FALSE
data.frame(
  Reference = c("File1", "File2"),
  Description = c("Codebook and protocol",
                  "Presentation"),
  Title = c("Manual das Pesquisas Origem e Destino 2002 e 2012",
            "Comparacao dos resultados das pesquisas Origem-Destino de 2002 e 2012"),
  File = c("Codebook and protocol.pdf",
           "Presentation.pdf")
) %>% kbl() %>% kable_classic()

#' ## Definition of a trip
#' 1. *Trip:* Moving from one part to another with a specific reason/motive,
#' using one or more modes of transport (page 15, *File1*)
#' 
#' 2. *Collection:* 
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
# trip_0 <- read.table('J://Studies//MOVED//HealthImpact//Data//TIGTHAT//Brazil//Belo Horizonte//Travel survey//dbo_TB_VIAGENS_INTERNAS_RMBH.txt', header = TRUE, sep=",")
# person_0 <- read.table('J://Studies//MOVED//HealthImpact//Data//TIGTHAT//Brazil//Belo Horizonte//Travel survey//dbo_TB_DOMICILIO_PESSOA_ENTREGA.txt', header = TRUE, sep=",")
#hh_weights_0<- read.table('dbo_TB_FATOR_EXPANS?O_DOMIC?LIO.txt', header = TRUE, sep=",")

#' #### Importing from local directory
route <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/Brazil/BeloHorizonte/Trips/Data/"

#+ warning=FALSE, message=FALSE, cache=TRUE
# People
person_0 <- read.table(paste0(route, "dbo_TB_DOMICILIO_PESSOA_ENTREGA.txt"),
                       sep = ",", header = T)
# Trips
trip_0 <- read.table(paste0(route, "dbo_TB_VIAGENS_INTERNAS_RMBH.txt"),
                     sep = ",", header = T)

# Lookup
sex <- data.frame(sex = c("Male", "Female"), DS_SEXO = c("Masculino", "Feminino"))

#' # **Preprocessing phase**
#' ## Filtering people from Belo Horizonte metropolitan area
#' Since the survey was conducted in the whole metropolitan area, there's no 
#' need to filter households. This goes in hand with the jurisdiction of injuries
#' dataset.
#keep relevant variables
#selecting relevant variables
person <-  person_0 %>%
  mutate(cluster_id = 1,
         household_id = ID_DOMICILIO, 
         participant_id = paste0(ID_DOMICILIO,"_",ID_PESSOA),
         participant_wt = 1) %>%
  left_join(sex) %>% 
  select(cluster_id, household_id, participant_id,participant_wt, sex, IDADE)

#' ## Classification and translation of trip modes and purpose
#' Even though there's enough information at stage level, I still need to define
#' the main mode for each trip. So I made a hierarchy to get it and translate it.
#' This is going to be different from Lambed's code because he defined the main
#' mode based on stage duration.
#+ warning=FALSE, message=FALSE, cache=TRUE
#lookup tables
trip_mode <- bind_cols(
  DS_SH_MEIO_TRANSPORTE = distinct(trip_0, DS_SH_MEIO_TRANSPORTE),
  trip_mode = factor(c("bus", "car", "walk", "metro", "van", "car", 
                       "motorcycle", "other", "car", "bicycle", "taxi"),
                     levels = c("bicycle","bus","car","metro","motorcycle",
                                "other" ,"taxi", "train","van","walk")))

trip_purpose <- data.frame(
  distinct(trip_0, motivo_origem), 
  trip_purpose = c("return", "school", "work", "other", "other", "other", 
                   "work", "work", "other", "other", "other", "other", "other",
                   "other"))

#' ## Row for each stage, translate trip_mode and create duration
#' First I create trip duration and trip purpose, then translate trip mode, then
#' select some variables.
trip <- trip_0 %>%
  mutate(participant_id = paste0(Domicilio,"_",Pessoa),
         trip_duration = (as.numeric(substr(trip_0$TEMPO.DE.DESLOCAMENTO, 
                                            12, 13)))*60 + 
           as.numeric(substr(trip_0$TEMPO.DE.DESLOCAMENTO, 15, 16))) %>% 
  left_join(trip_mode) %>%
  left_join(trip_purpose) %>% 
  select(participant_id, Viagem, trip_duration, trip_mode, trip_purpose)

#' ## Create variables for quick report
#' I need to create some variables to run the report that Lambed developed in 
#' the function *quality_check*.
trip <- person %>% 
  left_join(trip) %>%
  rename(age= IDADE, trip_id = Viagem)

trip$meta_data <- NA
trip$meta_data[1] <- 5595800
trip$meta_data[2] <- 15134 
trip$meta_data[3] <- "Travel Survey"
trip$meta_data[4] <- 2012
trip$meta_data[5] <- "1 day"
trip$meta_data[6] <- "No" #Stage level data available
trip$meta_data[7] <- "All purpose"#Overall trip purpose
trip$meta_data[8] <- "No" # Short walks to PT
trip$meta_data[9] <- "No" # Distance available
trip$meta_data[10] <- "rickshaw" # missing modes


#' Export dataset to make the report
#quality_check(trip)
write.csv(trip, "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/data/local/belo_horizonte/belo_horizonte_trip.csv", row.names = F)

#' ## Standardize trip modes
#' There's already a function that standardize these modes so the package can use
#' these trips. I made sure to use translate trip modes so that the function
#' works perfectly (take a look at the *original* variable of *smodes* dataframe
#' in this function).
## Expand trip dataset using participant weight
#trip <- read_csv("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/data/local/belo_horizonte/belo_horizonte_trip.csv")

# Load helpful functions
#source("code/producing_trips_rd/used_functions.R")

# Standardized travel modes
trip <- standardize_modes(trip, mode = c('trip'))

# Expand by household IDs
rd <- trip

#' ## Creating again IDs

# Remove extra columns
#rd$X1 <- NULL

rd$participant_id <- as.integer(as.factor(with(rd, paste(cluster_id, household_id, participant_id, sep = "_"))))

rd$trip_id <- as.integer(as.factor(with(rd, paste(cluster_id, household_id, participant_id, trip_id,  sep = "_"))))

#' # **Exporting phase**
#' ## Variables to export
#' Now I filter the columns I need
rd1 <- rd %>% dplyr::select(participant_id, age, sex, trip_id, trip_mode,
                            trip_duration)

#' ## Export dataset
write_csv(rd1, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/inst/extdata/local/belo_horizonte/trips_belo_horizonte.csv')
