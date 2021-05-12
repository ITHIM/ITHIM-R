#' ---
#' title: "Preprocessing of Cape Town's travel dataset. Most of it comes from Lambed's code"
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
library(chron) # To use times function


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
#' Documentation is located in ".../SouthAfrica/CapeTown/Trips/reports/". I
#' downloaded these files from v drive.
#'
#' From now on: 
#+ warning=FALSE, message=FALSE, echo=FALSE
data.frame(
  Reference = c("File1"),
  Description = c("Household Survey Report"),
  Title = c("Household Survey Report"),
  File = c("Final Draft HH Survey Report.pdf")
) %>% kbl() %>% kable_classic()

#' ## Definition of a trip
#' 1. *Trip:* I haven't found it yet.
#' 
#' 2. *Collection:* Trips you undertook from yesterday morning at 4 o'clock to
#' this morning at 4 o'clock.
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
#original data
# household_0 <- read_excel("J://Studies//MOVED//HealthImpact//Data//TIGTHAT//Cape Town//2013 Household Survey Data Sets & Meta Data_RHDHV_230114//Final datasheets_incl metadata_05-12-2013//CT_Household_travel_survey_householdinfo//test.xlsx")
# 
# person_0 <- read_excel("J://Studies//MOVED//HealthImpact//Data//TIGTHAT//Cape Town//2013 Household Survey Data Sets & Meta Data_RHDHV_230114//Final datasheets_incl metadata_05-12-2013//CT_Household_travel_survey_person info//CT_household_travel_survey_person.xlsx",
#                        range = cell_cols("B:E"))
# trip_0 <- read_excel("J://Studies//MOVED//HealthImpact//Data//TIGTHAT//Cape Town//2013 Household Survey Data Sets & Meta Data_RHDHV_230114//Final datasheets_incl metadata_05-12-2013//CT_Trip diary//CT_household_travel_survey_diary.xlsx",
#                      sheet = "Trip Diary_copy")
# 
#' #### Importing from local directory
route <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/SouthAfrica/CapeTown/Trips/Data/"

#+ warning=FALSE, message=FALSE, cache=TRUE
# Households
household_0 <- read_excel(paste0(route, "test.xlsx"), guess_max = 100000)
# People
person_0 <- read_excel(paste0(route, "CT_household_travel_survey_person.xlsx"),
                       guess_max = 100000, range = cell_cols("B:E"))
# Trips
trip_0 <- read_excel(paste0(route, "CT_household_travel_survey_diary.xlsx"),
                    guess_max = 100000, sheet = "Trip Diary_copy")

#' # **Preprocessing phase**
#' ## Filtering people from Cape Town 
#' Since the survey was conducted in the Cape Town, Malmesbury and Paarl, I need 
#' tp filter only households from Cape Town. This goes in hand with the
#' jurisdiction of injuries dataset. For now I keep all households because
#' we don't know yet about the jurisdiction of injuries.
#get relevant household info
household <- household_0[-c(1:14),]
colnames(household) = household[1,]
household <- household[-1,]

#get person info (id, age, sex)
person <- person_0 %>% 
  mutate(cluster_id = 1, 
         age = 2013 - as.numeric(`Year of`), 
         participant_wt = 1,
         participant_id = as.numeric(`Number of`),
         sex = ifelse(`Gender:` == 1, "Male", 
                      ifelse(`Gender:` == 2, "Female", `Gender:`))) %>% 
  rename(household_id = `Unique number`) %>% 
  {.[-c(1:15),-c(2:4)]}


#' ## Classification and translation of trip modes and purpose
#+ warning=FALSE, message=FALSE, cache=TRUE

trip_mode <- data.frame(
  TD_Mode_Used = as.character(1:13), 
  trip_mode = c("walk", "car", "car", "train", "bus", "bus", 
                "bicycle", "motorcycle", "motorcycle", "bus", 
                "car", "bus", "other"))
#original tip modes 1=walk, 2=car-driver, 3=car-passenger, 4=train, 5=bus, 6=minibus taxi, 7=bicycle, 
#8=motorcycle driver, 9=motorcycle passenger, 10=MyCiti bus, 11=employer transport, 12=scholar trasnport, 13=othe

trip_purpose <- data.frame(
  TD_Trip_purpose = as.character(1:18), 
  trip_purpose = c("return", "work", "school", "school", 
                   "school", "other", "other", "work", 
                   "other", "other", "other ", "other", 
                   "other ", "other", "other ", "other", 
                   "other ", "other"))
#original trip purose: 1=home, 2=work, 3=school, 4=tertiary educ., 5=pick up/drop off children, 
#6=pick up/drop off other person, 7=transfer, 8=errand at work, 9=shopping, 10=recreation, 
#11=fuel station, 12=medicare, 13=post office/bank etc., 14=visit a person, 15=fetch water, 
#16=tend to animals, 17=other(1), 18=other(2),

#' ## Row for each stage, translate trip_mode and create duration
#' First I create trip duration and other variables and then select some of them
##mutate and rename trip variables 
trip <- trip_0 %>% 
  {.[-15495,]} %>% #remove the last row which is empty
  mutate(
    trip_duration = abs((times(Trip_end_Time) - times(Trip_start_Time))*24*60),
    Trip_no = ifelse(as.numeric(Trip_no) < 0, NA, Trip_no),
    Trip_no = ifelse(is.na(Trip_no) & !is.na(TD_Trip_purpose), 5, Trip_no)) %>% 
  left_join(trip_mode) %>% 
  left_join(trip_purpose) %>% 
  rename(household_id = Unique_Input,
         participant_id = Person_ID, 
         trip_id = Trip_no) %>% 
  dplyr::select(household_id, participant_id,trip_id, trip_mode, trip_purpose,
         trip_duration)

##remove rows indicating no trips for individuals with trips
a <- trip %>% filter(is.na(trip_id))# rows with no trips
b <- trip %>% filter(!is.na(trip_id)) # rows with trips
c <- semi_join(a, b, by = c("household_id", "participant_id"))# rows without trips and have trips elsewhere
trip <- anti_join(trip, c)


#' ## Create variables for quick report
#' I need to create some variables to run the report that Lambed developed in 
#' the function *quality_check*.
##add person characteristics to trip data
trip <- left_join(trip, person, by = c("household_id", "participant_id"))

##assign participant numbers to rows with trip numbers and no participant
trip$participant_id <- ifelse(is.na(trip$participant_id) & 
                                !is.na(trip$trip_id), 
                              10, trip$participant_id)

##remove rows with no participant number
trip <- trip[which(!is.na(trip$participant_id)),]


##add year of survey to trips
trip$meta_data <- NA
trip$meta_data[1] <- 4178700
trip$meta_data[2] <- 14086
trip$meta_data[3] <- "Travel Survey"
trip$meta_data[4] <- 2013
trip$meta_data[5] <- "1 day"
trip$meta_data[6] <- "Yes" #Stage level data available
trip$meta_data[7] <- "All purpose"#Overall trip purpose
trip$meta_data[8] <- "Yes" # Short walks to PT
trip$meta_data[9] <- "No" # Distance available
trip$meta_data[10] <- "rickshaw" # missing modes


#' Export dataset to make the report
#quality_check(trip)
write.csv(trip, "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/data/local/cape_town/cape_town_trip.csv")

#' ## Standardize trip modes
#' There's already a function that standardize these modes so the package can use
#' these trips. I made sure to use translate trip modes so that the function
#' works perfectly (take a look at the *original* variable of *smodes* dataframe
#' in this function).
## Expand trip dataset using participant weight
#trip <- read_csv("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/data/local/cape_town/cape_town_trip.csv")

# Load helpful functions
#source("code/producing_trips_rd/used_functions.R")

# Checking the number of missing values
sapply(trip, function(x) sum(is.na(x)))

# Standardized travel modes
trip <- standardize_modes(trip, mode = c('trip'))

rd <- expand_using_weights(trip, normalize_by = 1)


#' ## Creating again IDs
# Remove extra columns
rd$X1 <- NULL

rd$participant_id <- as.integer(as.factor(with(rd, paste(cluster_id, household_id, participant_id, pid, sep = "_"))))

rd$trip_id <- as.integer(as.factor(with(rd, paste(cluster_id, household_id, participant_id, pid, trip_id,  sep = "_"))))

# New id for duplicated trip
#View(rd[duplicated(rd$trip_id),])
#View(rd[rd$trip_id == 7194,]) 
rd[duplicated(rd$trip_id), "trip_id"] <- 99999

# Checking the number of missing values
#sapply(rd, function(x) sum(is.na(x)))

#' # **Exporting phase**
#' ## Variables to export
#' Now I filter the columns I need
# Reorder and select columns
rd1 <- rd %>% dplyr::select(participant_id, age, sex, trip_id, trip_mode,
                            trip_duration)

#' ## Export dataset
write_csv(rd1, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/inst/extdata/local/cape_town/trips_cape_town.csv')
