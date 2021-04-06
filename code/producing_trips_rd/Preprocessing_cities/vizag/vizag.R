#' ---
#' title: "Preprocessing of Vizag's travel dataset. Most of it comes from Lambed's code"
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

#' ### Importing datasets
#' 
#' #### Importing from V drive
#' In the travel_survey.r script, the data is loaded from the package repo.
#' So any preprocessing made before is not documented. 
#visakhapatnam_trips <- read_csv("data/local/vizag/visakhapatnam_trips.csv")

#' #### Importing from local directory
#' This file is the same that appears in the repo. I downloaded it from v drive.
route <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/India/Vizag/"

#+ warning=FALSE, message=FALSE, cache=TRUE
# Trips
trip <- read_csv(paste0(route, "visakhapatnam_trips.csv"))


#' # **Preprocessing phase**
#' ## Filtering people from Visakhapatnam city
#' Since we don't have access to the documentation, we don't know the
#' jurisdiction of it. The jurisdiction of the injuries dataset is only the 
#' city. For this reason, I'm not filtering any row.


#' ## Classification and translation of trip modes and purpose
#' Since we don't have access to the documentation, we don't know anything
#' about the details of the survey nor the preprocessing done before.
#' In the imported dataset everything is in English and with the right format.

#' ## Row for each stage, translate trip_mode and create duration
#' From the imported dataset is clear that there's no information at stage level.
#' Then I create trip duration and other variables needed for the package from
#' the available variablesm and then select some of them
trip %<>% 
  mutate(cluster_id = 1,
         household_id = 1,
         participant_id = ind_id,
         trip_id = ifelse(is.na(travel_time), NA, X1),
         sex = ifelse(sex == "M", "Male", "Female"),
         trip_duration = travel_time,
         trip_distance = distance,
         trip_purpose = "unknown",
         trip_mode = ifelse(mode == "autorickshaw" | 
                              mode == "shared_autorickshaw" | 
                              mode == "cyclerickshaw", 
                            "rickshaw",
                            ifelse(mode == "mtw", "motorcycle",
                                   ifelse(mode == "company_bus", "bus", mode))),
         trip_wkly_frequency = wk_freq) %>% 
  select(cluster_id, household_id, participant_id,age, sex, trip_id,trip_mode,
         trip_duration, trip_distance, trip_wkly_frequency, trip_purpose) 

#' Translate stage mode and compute stage duration.
#' 
#' As I mentioned before, there's no need to translate because it's already in 
#' English.


#' ## Create variables for quick report
#' I need to create some variables to run the report that Lambed developed in 
#' the function *quality_check*. Since this dataset was preprocessed before, 
#' there's no need to do anything more.

trip$meta_data <- NA
trip$meta_data[1] <- 1878000
trip$meta_data[2] <- 23162
trip$meta_data[3] <- "Travel Survey"
trip$meta_data[4] <- 2011
trip$meta_data[5] <- "7 days"
trip$meta_data[6] <- "No" #Stage level data available
trip$meta_data[7] <- "All purpose"#Overall trip purpose
trip$meta_data[8] <- "No" # Short walks to PT
trip$meta_data[9] <- "Yes" # Distance available
trip$meta_data[10] <- "" # missing modes

#' Export dataset to make the report
#quality_check(trip)
write.csv(trip, "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/data/local/vizag/vizag_trip.csv")

#' ## Standardize trip modes
#' There's already a function that standardize these modes so the package can use
#' these trips. I made sure to use translate trip modes so that the function
#' works perfectly (take a look at the *original* variable of *smodes* dataframe
#' in this function).
## Expand trip dataset using participant weight
#trip <- read_csv("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/data/local/vizag/vizag_trip.csv")

# Load helpful functions
#source("code/producing_trips_rd/used_functions.R")

# Standardized travel modes
trip <- standardize_modes(trip, mode = c('trip'))

# Source functions
#source("code/producing_trips_rd/used_functions.R")

# Expand by household IDs
# rd <- expand_using_weights(trip, normalize_by = 20)
rd <- trip

#' ## Creating again IDs

# Remove extra columns
rd$X1 <- NULL

rd$participant_id <- as.integer(as.factor(with(rd, paste(cluster_id, household_id, participant_id, sep = "_"))))

rd$trip_id <- as.integer(as.factor(with(rd, paste(cluster_id, household_id, participant_id, trip_id, sep = "_"))))

#' # **Exporting phase**
#' ## Variables to export
#' Now I filter the columns I need
rd1 <- rd %>% dplyr::select(participant_id, age, sex, trip_id, trip_mode,
                            trip_duration, trip_distance)

#' ## Export dataset
write_csv(rd1, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/inst/extdata/local/vizag/trips_vizag.csv')
