#' ---
#' title: "Preprocessing of Bangalore's travel dataset. Most of it comes from Lambed's code"
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
library(gsubfn) # To use gsubfn function
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
#' downloaded these files from v drive. I asked Rahul for the documentation
#' but hasn't upload anything.
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
#' 1. *Trip:* We don't know yet
#' 
#' 2. *Collection:* We don't know yet
#' 
#' 
#' ## Replicate main results from raw datasets
#' To create this report I have to set the full route of each file, regardless
#' the location of the working directory.
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
mode_speed <- data.frame(mode = c("bicycle","bus","car","metro", "motorcycle",
                                  "other", "rickshaw", "taxi","train","truck",
                                  "van","walk" ),
                         mode_speed = c(15, 15, 25, 25, 25,21 ,25,25, 30,25,25, 5 ))

#' ### Importing datasets
#' 
#' #### Importing from V drive
#' I'm not importing directly from V drive because it takes too long. This is 
#' why this piece of code is commented.
# # data
# person_0 <- read_excel("V://Studies//MOVED//HealthImpact//Data//TIGTHAT//India//Bangalore//HH information-urban bmr.xlsx",sheet = 1, range = cell_cols("A:AM"),col_types = c("text"))
# stage_0 <- read_excel("V://Studies//MOVED//HealthImpact//Data//TIGTHAT//India//Bangalore//COMPILED DATA final.xlsx",sheet = 1, range = cell_cols("A:AA"), col_types = c("text"))
 


#' #### Importing from local directory
route <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/India/Bangalore/"

#+ warning=FALSE, message=FALSE, cache=TRUE
# People
person_0 <- read_excel(paste0(route, "HH information-urban bmr.xlsx"),
                       sheet = 1, range = cell_cols("A:AM"),
                       col_types = c("text"))
# Stages
stage_0 <- read_excel(paste0(route, "COMPILED DATA final.xlsx"),
                    sheet = 1, range = cell_cols("A:AA"), 
                    col_types = c("text"))

#' # **Preprocessing phase**
#' ## Filtering people from Bangalore
#' Since we don't have access to the documentation, we don't know the
#' jurisdiction of it. The jurisdiction of the injuries dataset is only the 
#' city. For this reason, I'm not filtering any row. However I will create some
#' variables that are needed for the package.
person <- person_0 %>% 
  {.[-c(1,2),]} %>% 
  filter(is.na(...39)) %>% 
  rename(household_id = `Household Serial Number`,
         cluster_id = `WARD NO`,
         person = ...28,
         age = `Age(Years)`,
         sex = `Sex:\r\n1. Male,\r\n2. Female`,
         id = ...38) %>% 
  dplyr::select(household_id, cluster_id, person, age, sex, id) %>% 
  bind_rows(
    person_0 %>% 
      filter(!is.na(...39)) %>% 
      slice(1:186) %>% 
      rename(household_id = `Household Serial Number`,
             cluster_id = `WARD NO`,
             person = Name, 
             age = `Sex:\r\n1. Male,\r\n2. Female`, 
             sex = `Marital Status:\r\n1. Unmarried,\r\n2. Married,\r\n3. Others.`, 
             id = ...39) %>% 
      dplyr::select(household_id, cluster_id, person, age, sex, id) %>% 
      bind_rows(
        person_0 %>% 
          filter(!is.na(...39)) %>% 
          slice(187:715) %>% 
          rename(household_id = `Household Serial Number`,
                 cluster_id = `WARD NO`,
                 person = ...28,
                 age = `Sex:\r\n1. Male,\r\n2. Female`, 
                 sex = `Marital Status:\r\n1. Unmarried,\r\n2. Married,\r\n3. Others.`, 
                 id = ...39) %>% 
          dplyr::select(household_id, cluster_id, person, age, sex, id)  ) ) %>% 
  rename(participant_id = id) %>% 
  dplyr::select(cluster_id, household_id, participant_id, age, sex) %>% 
  mutate(participant_wt =1, sex = ifelse(sex==1,"Male", "Female"))

#add omitted cluster_id and household_id
for(i in 2:nrow(person)){
  if(is.na(person$cluster_id[i])){
    person$cluster_id[i] = person$cluster_id[i-1]
  }
}


for(i in 2:nrow(person)){
  if(is.na(person$household_id[i])){
    person$household_id[i] = person$household_id[i-1]
  }
}

#' ## Classification and translation of trip modes and purpose
#' Since we don't have access to the documentation, we don't know anything
#' about the details of the survey nor the preprocessing done before.
#' These dataframes have been defined by Lambed.
#lookup
trip_purpose <-  bind_cols(
  distinct(stage_0, `Purpose of travel`),
  trip_purpose = c("other", "work", "return", "school", "other","other",
                   "other","other","other", "other","other","other","other",
                   "other","other","other","other","other","other",
                   "other","other"))
stage_mode <-  bind_cols( 
  distinct(stage_0,`Mode of Travel`),
  mode = c("other", "walk", "bus", "motorcycle", "bicycle", "bus", "car",
           "bus","taxi", "rickshaw", "bus","other", "train","other", "taxi",
           "other","other"))

#' ## Row for each stage, translate trip_mode and create duration
#' First I create statge duration and distance, then translate and rename some variables, and
#' then select some of them.
stage <- stage_0 %>% 
  mutate(x = `Transfer time in Min     (Walk time+wait time for next mode)`,
         duration = ifelse(grepl("E-",x), as.numeric(x)*1440,x),
         duration = gsub("([A-z]|:00|:|;|\\s|-|\\.$)","", duration ),
         time_diff = ifelse(as.numeric(`Starting time`) < 1, 
                            (as.numeric(`Finishing Time`) - 
                               as.numeric(`Starting time`))*1440, 
                            (as.numeric(`Finishing Time`) - 
                               as.numeric(`Starting time`))),
         duration = ifelse((duration == "2" | duration == "3") & 
                             (grepl("m|M", x) | time_diff < 1), 
                           as.numeric(duration)/100, duration),
         duration =  ifelse((duration == "0" | is.na(duration)) & 
                              (time_diff < 180 & time_diff > 0), 
                            time_diff, duration ),
         stage_duration = ifelse(as.numeric(duration) < 4,
                                 gsubfn("([0-3])(\\.*\\d*)",
                                        ~as.numeric(x)*60 + 
                                          ifelse(is.na(as.numeric(y)),0,
                                                 as.numeric(y)*100), duration),
                                 duration),
         stage_duration = as.numeric(stage_duration),
         
         #stage distance
         y = `Stage Distance(Kms)`,
         distance = ifelse(grepl("^0\\.(\\d{3,})|E-",y), 
                           as.numeric(y)*1440, y),
         #Replace ":" at start of string assumed to be "."
         distance = gsub("::|:|,", ".", distance, ignore.case = TRUE),
         distance = gsub("(\\-1\\/2|1\\/2)", ".5", distance, ignore.case = TRUE),
         distance = gsub("(1\\/4)", ".25", distance, ignore.case = TRUE),
         distance = gsub("(3\\/4)", ".75", distance, ignore.case = TRUE),
         distance = gsub("(1\\/5)", ".2", distance, ignore.case = TRUE),
         distance = gsub("(\\.\\.)", ".", distance, ignore.case = TRUE),
         ##remove units from distances
         distance = gsub("([a-z]|\\s|]|:|\\/\\-|)", "", distance,
                         ignore.case = TRUE),
         distance = gsub("(^$|\\-)", "0", distance,ignore.case = TRUE),
         distance = gsub("\\.$", "", distance,ignore.case = TRUE),
         distance = as.numeric(distance),
         distance = ifelse((`Mode of Travel` == 1 & distance > 30) | 
                             distance > 90, distance/1000, distance)) %>%
  
  rename(`Purpose of travel` = `Purpose of travel`, 
         `Mode of Travel` = `Mode of Travel` ) %>% 
  #add purpose
  left_join(trip_purpose) %>% 
  #add mode names
  left_join(stage_mode) %>% 
  #add mode speed
  left_join(mode_speed) %>% 
  mutate(stage_duration  = ifelse(stage_duration <= 0 | stage_duration >= 200 |
                                     is.na(stage_duration),
                                   distance*60/mode_speed, stage_duration),
         stage_distance = ifelse(distance <= 0 | is.na(distance), 
                                 stage_duration * mode_speed / 60, distance)) %>%
  rename(hh = `House hold serial.No`, 
         ward = `Ward No.`, 
         person = `No. of Person`, 
         participant_id = ID ,
         trip_id = Trips, stage_id = Stage, age = AGE, stage_mode = mode ) %>%
  {.[-1,]} %>% 
  dplyr::select(participant_id, trip_id, stage_id, age, trip_purpose, 
                stage_mode, stage_distance, stage_duration, mode_speed) %>% 
  {.[!duplicated(.),]}

#' ## Create variables for quick report
#' I need to create some variables to run the report that Lambed developed in 
#' the function *quality_check*. Here trip mode is defined based on duration and
#' speed, we can change this using a hierarchy of modes.
trip <- 
  stage %>% 
  group_by(participant_id, trip_id) %>% 
  summarise(trip_mode = ifelse(is.na(stage_mode[which.is.max(stage_duration)]),
                               stage_mode[which.is.max(mode_speed)],
                               stage_mode[which.is.max(stage_duration)]),
            trip_duration = sum(stage_duration, na.rm = T))  

trip <- 
  person %>%
  left_join(trip) %>% 
  left_join(stage) %>% 
  dplyr::select(-mode_speed)

# Copy stage_duration from a single trip_duration
trip[is.na(trip$stage_mode) & (!is.na(trip$trip_duration)),]$stage_duration <- trip[is.na(trip$stage_mode) & (!is.na(trip$trip_duration)),]$trip_duration

# Copy stage_mode from a single trip_mode
trip[is.na(trip$stage_mode) & (!is.na(trip$trip_duration)),]$stage_mode <- trip[is.na(trip$stage_mode) & (!is.na(trip$trip_duration)),]$trip_mode

## Populate missing stage_distance when stage_duration is given
# Calculate temp speed column
trip <- trip %>% 
  left_join(trip %>% 
              group_by(stage_mode) %>% 
              summarise(
                speed = ((median(stage_distance, na.rm = T) * 60) / 
                           median(stage_duration, na.rm = T)))
            )

# Calculate distance - where missing, from speed and duration
trip[!is.na(trip$trip_duration) & is.na(trip$stage_distance) & !is.na(trip$stage_duration),]$stage_distance <- 
  (trip[!is.na(trip$trip_duration) & is.na(trip$stage_distance) & !is.na(trip$stage_duration),]$speed * 
     trip[!is.na(trip$trip_duration) & is.na(trip$stage_distance) & !is.na(trip$stage_duration),]$stage_duration) / 60

# Remove temp speed column
trip$speed <- NULL

trip$meta_data <- NA
trip$meta_data[1] <- 8971800
trip$meta_data[2] <- 5051
trip$meta_data[3] <- "Travel Survey"
trip$meta_data[4] <- 2011
trip$meta_data[5] <- "1 day"
trip$meta_data[6] <- "Yes" #Stage level data available
trip$meta_data[7] <- "Mainly commute"#Overall trip purpose
trip$meta_data[8] <- "Yes" # Short walks to PT
trip$meta_data[9] <- "Yes" # Distance available
trip$meta_data[10] <- "" # missing modes


#' Export dataset to make the report
#quality_check(trip)
write_csv(trip, "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/data/local/bangalore/bangalore_trip.csv")

#' ## Standardize trip modes
#' There's already a function that standardize these modes so the package can use
#' these trips. I made sure to use translate trip modes so that the function
#' works perfectly (take a look at the *original* variable of *smodes* dataframe
#' in this function).
## Expand trip dataset using participant weight
#trip <- read_csv("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/data/local/bangalore/bangalore_trip.csv")

# Load helpful functions
#source("code/producing_trips_rd/used_functions.R")

# Standardized travel modes
trip <- standardize_modes(trip, mode = c('stage', 'trip'))

#rd <- expand_using_weights(trip)
rd <- trip

# Arrange df
rd <- rd %>% arrange(cluster_id, household_id, participant_id, trip_id) %>% 
  mutate(
    participant_id = as.integer(as.factor(with(rd, 
                                               paste(cluster_id, household_id,
                                                     participant_id, 
                                                     sep = "_")))),
    trip_id = as.integer(as.factor(with(rd, paste(cluster_id, household_id,
                                                  participant_id, trip_id, 
                                                  sep = "_")))),
    trip_id = ifelse(is.na(trip_mode), NA, trip_id))

# Checking the number of missing values
sapply(trip, function(x) sum(is.na(x)))
sapply(rd, function(x) sum(is.na(x)))

#' # **Exporting phase**
#' ## Variables to export
#' Now I filter the columns I need
# Reorder and select columns
rd1 <- rd %>% dplyr::select(participant_id, age, sex, trip_id, trip_mode, trip_duration, stage_id, stage_mode, stage_duration, stage_distance)

#' ## Export dataset
write_csv(rd1, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/inst/extdata/local/bangalore/trips_bangalore.csv')
