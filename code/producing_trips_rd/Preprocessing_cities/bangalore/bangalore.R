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
#' 
#' This dataset is really poor in terms of quality, there's a lot of things
#' mixed: the ID has duplicates, in some rows the columns don't represent the
#' data that is supposed to represent, and so on.
#' 
#' The first thing to consists in getting the variables we need in the right
#' way and format, and then create variables for the model. For this I need to
#' split the dataset in 3 ways and create the correct variables.
person_0 <- person_0 %>% 
  # Remove the first two rows because they correspond to the variables from the
  # raw dataset. The first row or header of the dataset is merged in 2 or 3 
  # rows.
  {.[-c(1,2),]}

#' By looking at the datasets in excel, it can be seen that there are three
#' groups of rows: one where the values correspond to the variable labels, other
#' where the values are one column to the right (which is because of address
#' variable); and the last one because "member" variable is the same as in "age"
#' which is a mistake. For this reason I filter each group in a different 
#' dataset so I can correct them as it should.
#' 
#' The first group can be identified by those that are full of NA's in variable
#' "...39". This groups have most of the rows
person_1 <- person_0 %>% 
  # By filtering NA values in "...39" I get the first set of rows
  filter(is.na(...39)) %>% 
  # In this case the variables and the values make sense
  rename(household_id = `Household Serial Number`,
         cluster_id = `WARD NO`,
         person = ...28,
         age = `Age(Years)`,
         sex = `Sex:\r\n1. Male,\r\n2. Female`,
         id = ...38) %>% 
  dplyr::select(household_id, cluster_id, person, age, sex, id) %>% 
  # Filling NA values with the ones that come before. This applies under the
  # assumption that the order of the dataset is correct
  fill(household_id, .direction = "down") %>% 
  fill(cluster_id, .direction = "down") %>% 
  # There are 2 missing values in "person" variable I will keep them as it is
  # (for now).
  # Removing all rows that have cluster_id == "102a" because they have empty
  # values everywhere
  filter(cluster_id != "102a") %>% 
  # Since there are some problems with the variables I will try to fix them
  mutate(
    age = case_when(
      age %in% c(".", "1 MON", "10 month", "10 Month", "10 months", "10M",
                 "10mo", "11 mont", "11 month", "11 MONTH", "11mth",
                 "2 mont", "2 Month", "2M", "2MONTS", "3 MON", "3M", "4 mu",
                 "45 days", "5M", "6 months", "6M", "7 mont", "7 month",
                 "7 months", "7M", "7mo", "7Month", "8 mo", "8 month",
                 "8 months", "8 MONTHS", "8M", "9 MONTH", "9 months",
                 "9M", "9mo", "9MONTHS") ~ 0,
      age %in% c(":1", "1-1/2", "1-1/2 y", "1 1/2 yrs", "1,5", "1:05YRS",
                 "1year", "1YRS") ~ 1,
      age %in% c("2 1/2", "2,5") ~ 2,
      age %in% c(":10") ~ 10,
      age %in% c(":11") ~ 11,
      age %in% c(":8") ~ 8,
      age %in% c(":9") ~ 9,
      age %in% c("`33") ~ 33,
      age %in% c("21/2") ~ 21,
      age %in% c("31/2") ~ 31,
      age %in% c("5YRS") ~ 5,
      age %in% c("KUMAR") ~ as.numeric(NA),
      TRUE ~ as.numeric(age)
    ),
    age = floor(as.numeric(age)), # round numbers to the lowest integer
    sex = case_when(
      sex %in% c("1", "F") ~ "Female",
      sex %in% c("2", "M") ~ "Male",
      sex %in% c("0", "10", "11", "12", "20", "21", "22", "23", "3", "30",
                 "35", "4", "5", "6", "8") ~ as.character(NA),
      TRUE ~ sex
    ),
    participant_wt = 1,
    concat_id = paste(cluster_id, household_id, person, sep = "-")
  )

# It's not possible to know the real IDs because I didn't have access to raw datasets
#length(unique(person_1$concat_id)) == nrow(person_1) #False

#View(person_1 %>% group_by(concat_id) %>% filter(dplyr::n()>1))
#' The second group can be identified by those that have values in variable
#' "...39". This groups have in the column "Name of head of household" a number
#' instead of a name, and therefore the information doesn't correspond to the
#' variables. 
person_2 <- person_0 %>% 
  # By filtering NA values in "...39" I get the first set of rows
  filter(!is.na(...39)) %>% 
  slice(1:186) %>% # These are the rows from the second group
  rename(household_id = `Household Serial Number`,
         cluster_id = `WARD NO`,
         person = Name, 
         age = `Sex:\r\n1. Male,\r\n2. Female`, 
         sex = `Marital Status:\r\n1. Unmarried,\r\n2. Married,\r\n3. Others.`, 
         id = ...39) %>% 
  dplyr::select(household_id, cluster_id, person, age, sex, id) %>%
  mutate(age = as.numeric(age),
         sex = ifelse(sex == "1", "Male", "Female"),
         participant_wt = 1,
         concat_id = paste(cluster_id, household_id, person, sep = "-"))

#length(unique(person_2$concat_id)) == nrow(person_2) #OK

#' The third group can be identified by those that have values in variable
#' "...39". This groups have in the column "age" the same data that is
#' in column "members" and therefore the information doesn't correspond to the
#' variables. 
#' 
person_3 <- person_0 %>% 
  # By filtering NA values in "...39" I get the first set of rows
  filter(!is.na(...39)) %>% 
  slice(187:715) %>% # These are the rows from the third group
  rename(household_id = `Household Serial Number`,
         cluster_id = `WARD NO`,
         person = ...28,
         age = `Sex:\r\n1. Male,\r\n2. Female`, 
         sex = `Marital Status:\r\n1. Unmarried,\r\n2. Married,\r\n3. Others.`, 
         id = ...39) %>% 
  dplyr::select(household_id, cluster_id, person, age, sex, id) %>% 
  mutate(
    age = case_when(
      age %in% c("10 month", "11 MONTH", "6 MONTH", "6MONTH", "7 MONTH",
                 "9 MONTH") ~ 0,
      age %in% c(":11") ~ 11,
      age %in% c("1,5") ~ 1,
      age %in% c("2,5") ~ 2,
      TRUE ~ as.numeric(age)
    ),
    age = floor(as.numeric(age)), # round numbers to the lowest integer
    sex = ifelse(sex == "1", "Male", "Female"),
    participant_wt = 1,
    concat_id = paste(cluster_id, household_id, person, sep = "-")
  )

#length(unique(person_3$concat_id)) == nrow(person_3) #OK

person <- person_1 %>% bind_rows(person_2) %>% bind_rows(person_3) %>% 
  rename(participant_id = id)
  #rename(participant_id = concat_id)

# sum(is.na(person$household_id))
# sum(is.na(person$cluster_id))
# sum(is.na(person$person))
# sum(is.na(person$age))
# sum(is.na(person$sex))
# names(person)
# sort(unique(person$age))
# sort(unique(person$sex))


# rename(participant_id = id) %>% 
# dplyr::select(cluster_id, household_id, participant_id, age, sex) %>% 
# mutate(participant_wt = 1, sex = ifelse(sex == 1,"Male", "Female"))



#   #add omitted cluster_id and household_id
# for(i in 2:nrow(person)){
#   if(is.na(person$cluster_id[i])){
#     person$cluster_id[i] = person$cluster_id[i-1]
#   }
# }
# 
# 
# for(i in 2:nrow(person)){
#   if(is.na(person$household_id[i])){
#     person$household_id[i] = person$household_id[i-1]
#   }
# }

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
main_mode <-  bind_cols( 
  distinct(stage_0,`Mode of Travel`),
  mode = c("other", "walk", "bus", "motorcycle", "bicycle", "bus", "car",
           "bus","taxi", "rickshaw", "bus","other", "train","other", "taxi",
           "other","other"),
  # I created this hierarchy similar to the other cities
  hierarchy = c(17, 12, 4, 9, 11, 3, 8, 2, 7, 10, 5, 13, 1, 14, 6, 15, 16))

#' ## Row for each stage, translate trip_mode and create duration
#' First I create stage duration and distance, then translate and rename some variables, and
#' then select some of them.
stage <- stage_0 %>% 
  mutate(# Duration  
    x = `Transfer time in Min     (Walk time+wait time for next mode)`,
         # Transform duration in decimal number to minutes
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
                             distance > 90, distance/1000, distance)
         ) %>%
  rename(`Purpose of travel` = `Purpose of travel`, 
         `Mode of Travel` = `Mode of Travel` ) %>% 
  #add purpose
  left_join(trip_purpose) %>% 
  #add mode names
  left_join(main_mode) %>% 
  #add mode speed
  left_join(mode_speed) %>% 
  mutate(stage_duration  = ifelse(stage_duration <= 0 | stage_duration >= 200 |
                                    is.na(stage_duration),
                                  distance*60/mode_speed, stage_duration),
         stage_distance = ifelse(distance <= 0 | is.na(distance),
                                 stage_duration * mode_speed / 60, distance)
         ) %>%
  rename(hh = `House hold serial.No`, 
         ward = `Ward No.`, 
         person = `No. of Person`, 
         participant_id = ID ,
         trip_id = Trips, stage_id = Stage, age = AGE, stage_mode = mode ) %>%
  {.[-1,]} %>% 
  # Filling NA values with the ones that come before. This applies under the
  # assumption that the order of the dataset is correct
  fill(hh, .direction = "down") %>% 
  fill(ward, .direction = "down") %>% 
  mutate(age = case_when(
    age %in% c(".", "1 MON", "10 month", "10 Month", "10 months", "10M",
               "10mo", "11 mont", "11 month", "11 MONTH", "11mth",
               "2 mont", "2 Month", "2M", "2MONTS", "3 MON", "3M", "4 mu",
               "45 days", "5M", "6 months", "6M", "7 mont", "7 month",
               "7 months", "7M", "7mo", "7Month", "8 mo", "8 month",
               "8 months", "8 MONTHS", "8M", "9 MONTH", "9 months",
               "9M", "9mo", "9MONTHS") ~ 0,
    age %in% c(":1", "1-1/2", "1-1/2 y", "1 1/2 yrs", "1,5", "1:05YRS",
               "1year", "1YRS") ~ 1,
    age %in% c("2 1/2", "2,5") ~ 2,
    age %in% c(":10") ~ 10,
    age %in% c(":11") ~ 11,
    age %in% c(":8") ~ 8,
    age %in% c(":9") ~ 9,
    age %in% c("`33") ~ 33,
    age %in% c("21/2") ~ 21,
    age %in% c("31/2") ~ 31,
    age %in% c("5YRS") ~ 5,
    age %in% c("KUMAR") ~ as.numeric(NA),
    TRUE ~ as.numeric(age)),
    age = floor(as.numeric(age)) # round numbers to the lowest integer
  ) %>% 
  # mutate(concat_id = paste(ward, hh, person, sep = "-"),
  #        trip_id = paste(ward, hh, person, trip_id, sep = "-")) %>%
  dplyr::select(participant_id, trip_id, stage_id, age, trip_purpose, 
                stage_mode, stage_distance, stage_duration, mode_speed,
                hierarchy) %>% 
  {.[!duplicated(.),]}
#length(unique(stage$trip_id))


#' ## Create variables for quick report
#' I need to create some variables to run the report that Lambed developed in 
#' the function *quality_check*. Here trip mode is defined based on duration and
#' speed, we can change this using a hierarchy of modes.
# trip <- 
#   stage %>% 
#   group_by(participant_id, trip_id) %>% 
#   summarise(trip_mode = ifelse(is.na(stage_mode[which.is.max(stage_duration)]),
#                                stage_mode[which.is.max(mode_speed)],
#                                stage_mode[which.is.max(stage_duration)]),
#             trip_duration = sum(stage_duration, na.rm = T))

# Trip mode is created based on the hierarchy instaed of the speed or duration
trip1 <- stage %>% group_by(participant_id, trip_id) %>% 
  mutate(trip_mode = main_mode$mode[
    match(min(hierarchy), main_mode$hierarchy)],
    trip_duration = sum(stage_duration, na.rm = T)) %>% 
  ungroup() 

trip <- 
  person %>%
  left_join(trip1) %>% 
  #left_join(stage) %>% 
  dplyr::select(-mode_speed)


# Copy stage_duration from a single trip_duration
# trip[is.na(trip$stage_mode) & (!is.na(trip$trip_duration)),]$stage_duration <- trip[is.na(trip$stage_mode) & (!is.na(trip$trip_duration)),]$trip_duration

# Copy stage_mode from a single trip_mode
# trip[is.na(trip$stage_mode) & (!is.na(trip$trip_duration)),]$stage_mode <- trip[is.na(trip$stage_mode) & (!is.na(trip$trip_duration)),]$trip_mode

## Populate missing stage_distance when stage_duration is given
# Calculate temp speed column
# trip <- trip %>% 
#   left_join(trip %>% 
#               group_by(stage_mode) %>% 
#               summarise(
#                 speed = ((median(stage_distance, na.rm = T) * 60) / 
#                            median(stage_duration, na.rm = T)))
#   )

# Calculate distance - where missing, from speed and duration
# trip[!is.na(trip$trip_duration) & is.na(trip$stage_distance) & !is.na(trip$stage_duration),]$stage_distance <- 
#   (trip[!is.na(trip$trip_duration) & is.na(trip$stage_distance) & !is.na(trip$stage_duration),]$speed * 
#      trip[!is.na(trip$trip_duration) & is.na(trip$stage_distance) & !is.na(trip$stage_duration),]$stage_duration) / 60

# Remove temp speed column
#trip$speed <- NULL

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
    participant_id = as.integer(as.factor(paste(cluster_id, household_id,
                                                     participant_id, 
                                                     sep = "_"))),
    trip_id = as.integer(as.factor(paste(cluster_id, household_id,
                                                  participant_id, trip_id, 
                                                  sep = "_"))),
    trip_id = ifelse(is.na(trip_mode), NA, trip_id))

# Checking the number of missing values
sapply(trip, function(x) sum(is.na(x)))
sapply(rd, function(x) sum(is.na(x)))


##################################
# - Correction using speed: for every bus stage compute the speed distance/duration
#   In these case, compute distance assuming that time is right
#   
# - Correcting using trip mode: if trip mode is pedestrian and the stage mode is bus, then the stage_mode should be pedestrian
#   
# - Correcting: 2km limit

##################################

#' # **Exporting phase**
#' ## Variables to export
#' Now I filter the columns I need
# Reorder and select columns
rd1 <- rd %>% dplyr::select(participant_id, age, sex, trip_id, trip_mode, trip_duration, stage_id, stage_mode, stage_duration)#, stage_distance)

#' ## Export dataset
write_csv(rd1, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/inst/extdata/local/bangalore/trips_bangalore.csv')
