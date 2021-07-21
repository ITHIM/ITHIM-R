#' ---
#' title: "Preprocessing of Accra's time user survey dataset. Most of it comes from Lambed's code"
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
#' I added some comments. The processing of this dataset is totally different
#' because the survey is about time use not travel.
#' 
#' 
#' ## Documentation 
#' Documentation is located in ".../Ghana/Accra/Trips/Reports/". I downloaded
#' these files from v drive.
#'
#' From now on: 
#+ warning=FALSE, message=FALSE, echo=FALSE
data.frame(
  Reference = c("File1"),
  Description = c("Main report"),
  Title = c("How Ghanaian women and wem spend their time Ghana Time-Use Survey 2009 Main Report"),
  File = c("Ghana Time Use Survey (GTUS) 2009 Report.pdf")
) %>% kbl() %>% kable_classic()

#' ## Definition of a trip
#' 1. *Trip:* Not defined because of survey type. In processing, has been taken as a travel activity done by one of the listed modes. 
#' 
#' 2. *Collection:* All eligible household members were asked about their activities in the 24 hours beginning at 4am on the previous day.
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
# read data
# time_use_0 <- haven::read_spss("J://Studies//MOVED//HealthImpact//Data//TIGTHAT//Accra//Accra data and microdata//Time Use Survey//Data//GTUS 2009 24 Hours Individual Diary.sav")

#' #### Importing from local directory
route <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/Ghana/Accra/Trips/"

people <- haven::read_spss(paste0(route, "GTUS 2009 Individual Characteristics.sav"))

#+ warning=FALSE, message=FALSE, cache=TRUE
time_use_0 <- haven::read_spss(paste0(route, "GTUS 2009 24 Hours Individual Diary.sav"))
time_use_0 <- as_factor(time_use_0)

#lookup
trip_mode <- data.frame(
  distinct(time_use_0, ActLoc2), 
  #ActLoc2 = factor(unique(time_use_0$ActLoc2)),
  trip_mode = c(NA, "walk","bus","taxi",  "bicycle",  "car", 
                "other","train","other","other","other" )) %>% 
  mutate(ActLoc2 = factor(ActLoc2))


#' # **Preprocessing phase**
#' ## Filtering people from Accra 
#' Since the survey was conducted in the whole country, I need to filter only
#' the households that are located in the urban area of Greater Accra. This goes
#'  in hand with the jurisdiction of injuries dataset, which is the metropolitan
#'  area. 
#'  
# Adding hh id in people dataset
people2 <- people %>% 
  mutate(EANum2 = sprintf("%03d", people$EANum),
         HHNum2 = sprintf("%02d", people$HHNum),
         concat = paste(EANum2, HHNum2, sep = "-")) %>% 
  dplyr::select(EANum2, HHNum2, concat, URBRUR, region, district, Adj_hh_wt, 
                B04, B05) %>% distinct(concat, .keep_all = T)

# Merging people information to time_use_0 dataset
time_use_0_v2 <- time_use_0 %>% 
  mutate(concat = paste(EANum, HHNum, sep = "-")) %>% 
  left_join(people2, by = "concat") 

# Checking that region and urbrural variables are the same in both datasets
all(time_use_0_v2$region.x == time_use_0_v2$region.y) #ok
all(time_use_0_v2$Adj_hh_wt.x == time_use_0_v2$Adj_hh_wt.y) #ok
all(time_use_0_v2$URBRUR.x == time_use_0_v2$URBRUR.y) #ok

# Since both are ok, then I can assume that district is also ok

#keep relevant variables
#time_use_0 <- as_factor(time_use_0)

#time_use_0 %>% mutate_if(is.factor, as.character) -> time_use_0
# Create some variables and filter trips in the urban area of Greater Accra
dat <- time_use_0 %>% 
  # Paste district variable
  mutate(concat = paste(EANum, HHNum, sep = "-")) %>% 
  left_join(people2 %>% dplyr::select(concat, district), by = "concat") %>% 
  # filter dataset
  dplyr::filter(region == "Greater Accra" & URBRUR == "Urban" & 
                  district == 1) %>% 
  rename(sex = B102,
         age = B105,
         cluster_id = EANum,
         household_id = HHNum,
         participant_id = MemID,
         participant_wt = Adj_hh_wt) %>% 
  mutate(#separate start and end time,
    start = substr(Diary_hour, 1,2),
    end = substr(Diary_hour, 6,7),
    Duration = ActDur,
    Duplicate = "notDuplicate",
    Same = "notSame")

 #' ## Classification and translation of trip modes and purpose
#' The process to translate the purpose and mode is different to other surveys.
#' In this case we define the purpose in this way:
dat$ActCode1 <- ifelse(grepl("Work", dat$ActCode1), "work", 
                       ifelse(grepl("Learning",dat$ActCode1), "school", "other"))

dat$ActLoc22 <- dat$ActLoc2
#dat$ActLoc2 <- factor(dat$ActLoc2)
levels(dat$ActLoc2) <- c(levels(dat$ActLoc2), "missing")
#dat$ActLoc2[which(is.na(dat$ActLoc2))] <- "missing"
dat$ActLoc2[which(dat$ActLoc2 == "")] <- "missing"

#' ## Row for each stage, translate trip_mode and create duration
#' First I create trip duration and then translate the trip mode 
#for loop sums same activity
for (i in 2:nrow(dat)) {
  if (dat$cluster_id[i] == dat$cluster_id[i - 1] &
     dat$household_id[i] == dat$household_id[i - 1] &
     dat$participant_id[i] == dat$participant_id[i - 1] &
     dat$ActCode[i] == dat$ActCode[i - 1] & 
     dat$ActLoc2[i] == dat$ActLoc2[i - 1] &
     dat$start[i] == dat$end[i - 1]) {
    dat$Duration[i] <- `+`(dat$Duration[i], dat$Duration[i - 1])
    dat$Duplicate[i] <- "Duplicate"
    dat$Same[i - 1] <- "Same"		
  }
}

dat$ActLoc2[which(dat$ActLoc2 == "missing")] <- NA

dat <- dat %>% 
  #drop sub-activities that have been summed into one activity
  filter(!(Same == "Same")) %>% 
  #add modes
  left_join(trip_mode) 

trip <- 
  dat %>% 
  #filter trips only
  filter(ActLoc1 == "Travelling / Moving") %>% 
  mutate(#identify trip by row number
    trip_id = row_number(),
    trip_duration = Duration,
    trip_purpose = ActCode1)

no_trip <- dat %>% 
  anti_join(trip, by= c("cluster_id", "household_id", "participant_id")) %>% 
  mutate(trip_id = NA,
         trip_duration = NA,
         trip_purpose = NA)

#join datasets
trip <- bind_rows(trip, no_trip) %>% 
  dplyr::select(cluster_id, household_id, participant_id, participant_wt, age, 
                sex, 
         trip_id, trip_mode, trip_duration, trip_purpose) %>% 
  {.[!duplicated(.),]}

trip$trip_mode[which(!is.na(trip$trip_id) & is.na(trip$trip_mode))] <- "other"

# Make all trips with missing mode and duration as NA
trip <- trip %>% mutate(trip_mode = replace(trip_mode, !is.na(trip$trip_mode) & is.na(trip$trip_duration), NA)) %>%  as.data.frame()

#' Export dataset to make the report
#quality_check(trip)
write.csv(trip, "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/data/local/accra/accra_trip.csv")

#' ## Create motorcycle trips
#' The following code has as an input accra_trip.csv and as output accra_trip_with_mbike.csv
# Source processing code
source("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/code/raw_to_processed/accra/streamline_travel_survey.R")

#' ## Standardize trip modes
#' There's already a function that standardize these modes so the package can use
#' these trips. I made sure to use translate trip modes so that the function
#' works perfectly (take a look at the *original* variable of *smodes* dataframe
#' in this function).
## Expand trip dataset using participant weight
trip <- read_csv("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/data/local/accra/accra_trip_with_mbike.csv")

# Checking the number of missing values
sapply(trip, function(x) sum(is.na(x)))

# There's a discrepancy between the number of missing values in trip_id and
# trip_mode, trip_duration and trip purpose.
# min(trip$trip_id, na.rm = T); max(trip$trip_id, na.rm = T)
# trip <- trip %>% #arrange(trip_id) %>% 
#   mutate(participant_id2 = as.integer(as.factor(paste0(cluster_id, household_id,
#                                                       participant_id))),
#     trip_id2 = (1:dplyr::n()) + max(trip_id, na.rm = T),
#          trip_id2 = ifelse(!is.na(trip_id) & !is.na(trip_mode), trip_id,
#                            ifelse(is.na(trip_id) & !is.na(trip_mode), trip_id2,
#                                   NA)))
# 
# # Checking the number of missing values
# sapply(trip, function(x) sum(is.na(x)))

#' ## Create variables for quick report
#' I need to create some variables to run the report that Lambed developed in 
#' the function *quality_check*.
trip$meta_data <- NA
trip$meta_data[1] <- 2242000
trip$meta_data[2] <- 4000
trip$meta_data[3] <- "Time use"
trip$meta_data[4] <- 2009
trip$meta_data[5] <- "1 day"
trip$meta_data[6] <- "implied" #Stage level data available
trip$meta_data[7] <- "All purpose"#Overall trip purpose
trip$meta_data[8] <- "??" # Short walks to PT
trip$meta_data[9] <- "No" # Distance available
trip$meta_data[10] <- "rickshaw, motorcycle" # missing modes

write_csv(trip, "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/data/local/accra/accra_trip.csv")


# Load helpful functions
#source("code/producing_trips_rd/used_functions.R")

# Defining standardized_modes function again 
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

# Standardized travel modes
trip <- standardize_modes(trip, mode = c('trip'))

# Save it in a local var
rd <- trip

# Expand by household IDs

# # Round participant weight
# rd <- rd %>% mutate(w = if_else(is.na(participant_wt), 0, round(participant_wt)))
# 
# # Subtract 1 from non-zero entries
# rd <- rd %>% mutate(w = if_else(w > 0, w - 1, w))
# 
# # Expand it according to weights, and assign IDs to the newly expanded rows
# exp <- rd %>% filter(w > 0) %>% uncount(w, .id = "pid")

#' ## Creating again IDs
# Arrange df and Create participant_id as a combination of cluster_id, household_id,
rd <- rd %>% arrange(cluster_id, household_id, participant_id, trip_id) %>% 
  mutate(participant_id = as.integer(as.factor(participant_id)),
         trip_id = as.integer(as.factor(trip_id)))

# #  participant_id
# rd$participant_id <- as.integer(as.factor(with(rd, paste(cluster_id, household_id, participant_id, sep = "_"))))
# 
# # Create trip_id as a combination of cluster_id, household_id, participant_id and trip_id
# rd$trip_id <- as.integer(as.factor(with(rd, paste(cluster_id, household_id, participant_id, trip_id2, sep = "_"))))

#' # **Exporting phase**
#' ## Variables to export
#' Now I filter the columns I need
# Reorder and select columns
rd1 <- rd %>% dplyr::select(participant_id, age, sex, trip_id, trip_mode,
                            trip_mode, trip_duration)

#' ## Export dataset
write_csv(rd1, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/inst/extdata/local/accra/trips_accra.csv')
