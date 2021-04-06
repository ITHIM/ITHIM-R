#' ---
#' title: "Preprocessing of Delhi's travel dataset. Most of it comes from Lambed's code"
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
#' There's no documentation for this survey, at least not in the v drive. I
#' downloaded these files from v drive.
#'
#' From now on: 
#+ warning=FALSE, message=FALSE, echo=FALSE
data.frame(
  Reference = c("Not Available"),
  Description = c("Not Available"),
  Title = c("Not Available"),
  File = c("Not Available")
) %>% kbl() %>% kable_classic()

#' ## Definition of a trip
#' 1. *Trip:* Any movement carried out on public roads with a purpose determined,
#' between two places (origin and destination) at a certain time of day; It can
#' be carried out in several modes of transport and consist of one or more 
#' stages. This definition was given by Rahul in the 11 cities summary excel
#' sheet (on google drive).
#' 
#' 2. *Collection:* Trips made the day before the survey. This definition was
#' given by Rahul in the 11 cities summary excel sheet (on google drive).
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
#data
# person_0 <- read.csv('J://Studies//MOVED//HealthImpact//Data//TIGTHAT//India//Delhi//persons.csv', na.strings = c("", "NA"))
# stage_0 <- read.csv('J://Studies//MOVED//HealthImpact//Data//TIGTHAT//India//Delhi//trips_stages_delhi.csv')

#' #### Importing from local directory
route <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/India/Delhi/"

#+ warning=FALSE, message=FALSE, cache=TRUE
# People
person_0 <- read.csv(paste0(route, "persons.csv"),
                     na.strings = c("", "NA"))
# Stages
stage_0 <- read.csv(paste0(route, "trips_stages_delhi.csv"))

#lookups
sex <-  bind_cols(sex = c("Male", "Female"), female = c(0,1))

#' # **Preprocessing phase**
#' ## Filtering people from Delhi city
#' Since we don't have access to the documentation, we don't know the
#' jurisdiction of it. The jurisdiction of the injuries dataset is only the 
#' city. For this reason, I'm not filtering any row. However I will create some
#' variables that are needed for the package.
#select relevant variables
person <- person_0 %>% 
  rename(participant_id = Member.ID, 
         household_id = Form.No.,
         age = Age, 
         participant_wt = Weights_final) %>%
  left_join(sex) %>% 
  mutate(cluster_id = 1,
         participant_id = ifelse(is.na(participant_id), 
                                 paste0("U0", row_number()), 
                                 paste(participant_id))) %>% 
  {.[which(!duplicated(.$participant_id)),]} %>% 
  select(-Form_id_new, -Member.No., -female)


#' ## Classification and translation of trip modes and purpose
#' Since we don't have access to the documentation, we don't know anything
#' about the details of the survey nor the preprocessing done before.
#' These dataframes have been defined by Lambed.

#lookups
stage_mode <- bind_cols(
  stage_mode = c("walk", "bicycle", "motorcycle", "car", 
                 "rickshaw", "rickshaw", "car", "bus", "metro", "train", 
                 "taxi"), mode = 1:11)

trip_mode <- bind_cols(
  trip_mode = c("walk", "bicycle", "motorcycle", "car", "rickshaw", "rickshaw",
                "car", "bus", "metro", "train", "taxi"), Main.Mode = 1:11)

trip_purpose <- bind_cols(
  Trip.Purpose = 0:12,
  trip_purpose = c("return", "work", "school", "other","other","other","other",
                   "other","other","other","other","other","other"))

#' ## Row for each stage, translate trip_mode and create duration
#' First I create trip duration, then translate and rename some variables, and
#' then select some of them.
stage <- stage_0 %>% 
  mutate(participant_id = paste0(stage_0$Form.ID, stage_0$Member.No.), 
         stage_id = row_number(),
         trip_duration = Trip_Time_Duration*60,
         stage_duration = Travel.Time*60) %>%
  left_join(stage_mode) %>% 
  left_join(trip_mode) %>% 
  left_join((trip_purpose)) %>% 
  rename(trip_id = Trip.ID, 
         trip_distance = Trip_total_distance, 
         stage_distance = Distance) %>% 
  select(participant_id, trip_id, stage_id, trip_mode, trip_purpose, trip_duration, trip_distance, stage_mode, 
         stage_duration, stage_distance)


#' ## Create variables for quick report
#' I need to create some variables to run the report that Lambed developed in 
#' the function *quality_check*.
#combine dataframes 
trip <- person %>% 
  left_join(stage)

#replace NA trip modes with other
trip$trip_mode[which(!is.na(trip$trip_id) & is.na(trip$trip_mode) )] <- "other"

trip$meta_data <- NA
trip$meta_data[1] <- 23036600
trip$meta_data[2] <- 12747
trip$meta_data[3] <- "Travel Survey"
trip$meta_data[4] <- 2014
trip$meta_data[5] <- "1 day"
trip$meta_data[6] <- "Yes, but no stage duration" #Stage level data available
trip$meta_data[7] <- "All purpose"#Overall trip purpose
trip$meta_data[8] <- "Yes" # Short walks to PT
trip$meta_data[9] <- "No" # Distance available
trip$meta_data[10] <- "rickshaw" # missing modes


#' Export dataset to make the report
#quality_check(trip)
write.csv(trip, "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/data/local/delhi/delhi_trip.csv")

#' ## Standardize trip modes
#' There's already a function that standardize these modes so the package can use
#' these trips. I made sure to use translate trip modes so that the function
#' works perfectly (take a look at the *original* variable of *smodes* dataframe
#' in this function).
## Expand trip dataset using participant weight
#trip <- read_csv("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/data/local/delhi/delhi_trip.csv")

# Load helpful functions
#source("code/producing_trips_rd/used_functions.R")

# Standardized travel modes
trip <- standardize_modes(trip, mode = c('stage', 'trip'))

# Source functions
#source("code/producing_trips_rd/used_functions.R")

# Save it in a local var
rd <- trip

# Expand by household IDs
rd <- expand_using_weights(trip, normalize_by = 1)

#' ## Creating again IDs

# Remove extra columns
rd$X1 <- NULL

rd$participant_id <- as.integer(as.factor(with(rd, paste(cluster_id, household_id, participant_id, pid, sep = "_"))))

rd$trip_id <- as.integer(as.factor(with(rd, paste(cluster_id, household_id, participant_id, pid, trip_id,  sep = "_"))))

#' # **Exporting phase**
#' ## Variables to export
#' Now I filter the columns I need
rd1 <- rd %>% dplyr::select(participant_id, age, sex, trip_id, trip_mode,
                            trip_duration, trip_distance, stage_id, stage_mode,
                            stage_duration, stage_distance)

#' ## Export dataset
write_csv(rd1, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/inst/extdata/local/delhi/trips_delhi.csv')
