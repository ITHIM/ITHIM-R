## packages

library(tidyverse)
library(magrittr)
library(readxl)
library(haven)
library(nnet)
library(foreign)
library(gsubfn)
library(knitr)
library(kableExtra)
library(summarytools)

    
## mode_speed - pls add other modes and their average mode speeds if needed.
mode_speed <- data.frame(mode = c("bicycle","bus","car","metro", "motorcycle",
                                "other", "rickshaw", "taxi","train","truck", "van","walk" ),
                              mode_speed = c(15, 15, 25, 25, 25,21 ,25,25, 30,25,25, 5 ))


## trip_summary
trip_summary <- data.frame(row.names =       c("Meta Data",
                                               "Population in 2014",
                                               "GDP/capita in 2014 ($)",
                                               "Type of survey",
                                               "Year of survey",
                                               "Diary type",
                                               "Trip stages captured",
                                               "Commute only?",
                                               "Short walks to PT",
                                               "Trip distance captured",
                                               "Missing modes",
                                               "",
                                               "Number of households",
                                               "Number of individuals",
                                               "Adults (% > 17 years)",
                                               "Household size",
                                               "Adults per household",
                                               "Male to female ratio (M:F)",
                                               "Trip Frequency",
                                               "People with trips (%)",
                                               "Prop. of sex with trips (M:F,%)",
                                               "Trip Distribution by sex (M:F,%)",
                                               "People with 1 trip (%)",
                                               "People with 2 trips (%)",
                                               "People with 3 trips (%)",
                                               "People with 4 trips (%)",
                                               "Trip per capita (overall)",
                                               "Trip per capita (adults)",
                                               "Trip duration (mins)",
                                               "travel time per capita",
                                               "travel time per capita (adults)",
                                               "Mean trip duration",
                                               "Median trip duration",
                                               "1st Quart of trip duration",
                                               "3rd Quart of trip duration",
                                               "IQR of trip duration",
                                               "Bicycle_duration",
                                               "Bus_duration",
                                               "Car_duration",
                                               "Metro_duration",
                                               "Motocycle_duration",
                                               "Other_duration",
                                               "Rickshaw_duration",
                                               "Taxi_duration",
                                               "Train_duration",
                                               "Truck_duration",
                                               "Van_duration",
                                               "Walk_duration",
                                               "Trip mode Shares (%)",
                                               "Bicycle_share",
                                               "Bus_share",
                                               "Car_share",
                                               "Metro_share",
                                               "Motocycle_share",
                                               "Other_share",
                                               "Rickshaw_share",
                                               "Taxi_share",
                                               "Train_share",
                                               "Truck_share",
                                               "Van_share",
                                               "Walk_share",
                                               "Trip Purpose (%)",
                                               "Work related",
                                               "School related",
                                               "Return home",
                                               "Other"))


## function_quality_check
quality_check <- function(trip){
        trip$trip_mode <- factor(trip$trip_mode, levels = c("bicycle","bus","car","metro", "motorcycle",
                                                            "other", "rickshaw", "taxi", "train", "truck", "van", "walk" ))
        
        total_household <-
            trip %>% 
            count(cluster_id, household_id) %>%
            nrow
        
        total_participant <- 
            trip %>% 
            count(cluster_id, household_id, participant_id) %>%
            nrow
        
        proportion_adult_participant <- 
            trip %>% 
            filter(age>17) %>% 
            count(cluster_id, household_id, participant_id) %>% 
            nrow*100/
            total_participant
        
        household_size <- 
            total_participant/total_household
        
        adult_per_house <- 
            trip %>% 
            filter(age>17) %>% 
            count(cluster_id, household_id, participant_id) %>% 
            nrow/
            total_household
        
        male_propotion <- 
            trip %>% 
            filter(sex == "Male") %>% 
            count(cluster_id, household_id, participant_id) %>%
            nrow /
            total_participant
        
        female_proportion <- 
            trip %>% 
            filter(sex == "Female") %>% 
            count(cluster_id, household_id, participant_id) %>%
            nrow /
            total_participant
        
        male_female_proportion <- 
            paste0(round(male_propotion*100),":",
                   round(female_proportion*100))
        
        people_with_trip <- 
            trip %>% 
            filter(!is.na(trip_id)) %>% 
            count(cluster_id, household_id, participant_id) %>% 
            nrow
        
        proportion_people_with_trips <- round(people_with_trip*100/total_participant)
        
        proportion_household_with_trip <-  
            trip %>% 
            group_by(cluster_id, household_id, participant_id) %>% 
            mutate(trip_status = ifelse(is.na(trip_id), 0, 1) ) %>% 
            count(cluster_id, household_id, participant_id, trip_status) %>% 
            group_by(cluster_id, household_id) %>% 
            summarise(mean_hh_trip = mean(trip_status)) %>%
            ungroup() %>% 
            summarise(value = mean(mean_hh_trip)) 
        
        male_trip_fraction <- 
            trip %>% 
            filter(!is.na(trip_id) & sex == "Male") %>%
            count(cluster_id, household_id, participant_id, trip_id) %>% 
            nrow/
            trip %>% 
            filter(!is.na(trip_id)) %>%
            count(cluster_id, household_id, participant_id, trip_id) %>% 
            nrow
        
        female_trip_fraction <- 
            trip %>% 
            filter(!is.na(trip_id) & sex == "Female") %>% 
            count(cluster_id, household_id, participant_id, trip_id) %>% 
            nrow/
            trip %>% 
            filter(!is.na(trip_id)) %>%
            count(cluster_id, household_id, participant_id, trip_id) %>% 
            nrow
        
        male_female_trip_fraction <- 
            paste0(round(male_trip_fraction*100), ":",
                   round(female_trip_fraction*100))
        
        proportion_male_with_trip <- 
            trip %>%
            filter(sex == "Male" & !is.na(trip_id)) %>% 
            count(cluster_id, household_id, participant_id) %>% 
            nrow /
            trip %>% 
            filter(sex == "Male") %>% 
            count(cluster_id, household_id, participant_id) %>% 
            nrow 
        
         proportion_female_with_trip <- 
             trip %>%
             filter(sex == "Female" & !is.na(trip_id)) %>% 
             count(cluster_id, household_id, participant_id) %>% 
             nrow /
             trip %>% 
             filter(sex == "Female") %>% 
             count(cluster_id, household_id, participant_id) %>% 
             nrow 
        
         trip_distribution_sex <- 
             paste0(round(proportion_male_with_trip*100),
                    ":",round(proportion_female_with_trip*100))
        
         
         trip_distribution_number <-
             trip %>% 
             filter(!is.na(trip_id)) %>% 
             count(cluster_id, household_id, participant_id, trip_id) %>% 
             count(cluster_id, household_id,participant_id) %>% 
             group_by(n) %>%
             summarise(number_of_trips = round(n()*100/nrow(.),1))
             
        trip_per_capita <- 
            trip %>% 
            filter(!is.na(trip_id)) %>% 
            count(cluster_id, household_id, participant_id, trip_id) %>% 
            nrow() / 
            trip %>% 
            count(cluster_id, household_id, participant_id) %>% 
            nrow()
        
        trip_per_capita_adult <- 
            trip %>% 
            filter(!is.na(trip_id) & age > 17) %>% 
            count(cluster_id, household_id, participant_id, trip_id) %>% 
            nrow() / 
            trip %>% 
            filter(age > 17) %>% 
            count(cluster_id, household_id, participant_id) %>%
            nrow()
       
         travel_time_per_person <-
             trip %>% 
             count(cluster_id, household_id, participant_id, trip_id, trip_duration) %$% 
             sum(trip_duration, na.rm = T)/
             count(trip, cluster_id, household_id, participant_id) %>% 
             nrow
        
         travel_time_per_adult <-
             trip %>% 
             filter(age>17) %>% 
             count(cluster_id, household_id, participant_id, trip_id, trip_duration) %$% 
             sum(trip_duration, na.rm = T) / 
             trip %>% 
             filter(age>17) %>% 
             count(cluster_id, household_id, participant_id) %>% 
             nrow
         
        average_trip_duration <-
            trip %>% 
            count(cluster_id, household_id, participant_id, trip_id, trip_duration) %$% 
            summary(trip_duration)
        
        iqr <-round((average_trip_duration[["3rd Qu."]] - average_trip_duration[["1st Qu."]]),1)
       
         avg_mode_time <- 
            trip %>% 
            filter(!is.na(trip_id)) %>% 
            count(cluster_id,household_id,participant_id, trip_id, trip_mode, trip_duration) %>% 
            group_by(trip_mode, .drop = FALSE) %>% 
            summarise(avg_mode_time = round(mean(trip_duration, na.rm = T),1))
        
        mode_share <- 
            trip %>% 
            filter(!is.na(trip_id)) %>% 
            count(cluster_id,household_id,participant_id, trip_id, trip_mode, trip_duration) %>% 
            group_by(trip_mode, .drop = F) %>% 
            summarise(mode_share = round(n()*100/nrow(.),1))
        
        trip_purpose <- 
            trip %>%
            filter(!is.na(trip_id)) %>% 
            count(cluster_id,household_id,participant_id, trip_id, trip_mode, trip_purpose) %>% 
            group_by(trip_purpose) %>%
            summarise(share = round(n()*100/nrow(.),1))
        
        
        value <<- c("",
                    trip$meta_data[1],
                    trip$meta_data[2],
                    trip$meta_data[3],
                    trip$meta_data[4],
                    trip$meta_data[5],
                    trip$meta_data[6],
                    trip$meta_data[7],
                    trip$meta_data[8],
                    trip$meta_data[9],
                    trip$meta_data[10],
                    "",
                   total_household, 
                   total_participant,
                   round(proportion_adult_participant),
                   round(household_size,1),
                   round(adult_per_house,1),
                   male_female_proportion,
                   "",
                   proportion_people_with_trips,
                   trip_distribution_sex,
                   male_female_trip_fraction,
                   ifelse(length(trip_distribution_number$number_of_trips[trip_distribution_number$n==1])==0,0,
                          trip_distribution_number$number_of_trips[trip_distribution_number$n==1]),
                   ifelse(length(trip_distribution_number$number_of_trips[trip_distribution_number$n==2])==0,0,
                          trip_distribution_number$number_of_trips[trip_distribution_number$n==2]),
                   ifelse(length(trip_distribution_number$number_of_trips[trip_distribution_number$n==3])==0,0,
                          trip_distribution_number$number_of_trips[trip_distribution_number$n==3]),
                   ifelse(length(trip_distribution_number$number_of_trips[trip_distribution_number$n==4])==0,0,
                          trip_distribution_number$number_of_trips[trip_distribution_number$n==4]),
                   round(trip_per_capita,1),
                   round(trip_per_capita_adult,1),
                   "",
                   round(travel_time_per_person),
                   round(travel_time_per_adult),
                   round(average_trip_duration[["Mean"]],1),
                   round(average_trip_duration[["Median"]],1),
                   round(average_trip_duration[["1st Qu."]],1),
                   round(average_trip_duration[["3rd Qu."]],1),
                   iqr,
                   avg_mode_time$avg_mode_time[1],
                   avg_mode_time$avg_mode_time[2],
                   avg_mode_time$avg_mode_time[3],
                   avg_mode_time$avg_mode_time[4],
                   avg_mode_time$avg_mode_time[5],
                   avg_mode_time$avg_mode_time[6],
                   avg_mode_time$avg_mode_time[7],
                   avg_mode_time$avg_mode_time[8],
                   avg_mode_time$avg_mode_time[9],
                   avg_mode_time$avg_mode_time[10],
                   avg_mode_time$avg_mode_time[11],
                   avg_mode_time$avg_mode_time[12],
                   "",
                   mode_share$mode_share[1],
                   mode_share$mode_share[2],
                   mode_share$mode_share[3],
                   mode_share$mode_share[4],
                   mode_share$mode_share[5],
                   mode_share$mode_share[6],
                   mode_share$mode_share[7],
                   mode_share$mode_share[8],
                   mode_share$mode_share[9],
                   mode_share$mode_share[10],
                   mode_share$mode_share[11],
                   mode_share$mode_share[12],
                   "",
                   trip_purpose$share[4],
                   trip_purpose$share[3],
                   trip_purpose$share[2],
                   trip_purpose$share[1]
                   )
     
        
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

standardize_modes <- function(trip, mode){
    # Read lookup table
    smodes <- read_csv('data/global/modes/standardized_modes.csv')
    # Separate rows 
    smodes <- smodes %>% separate_rows(original, sep = ';')
    
    smodes <- smodes %>% 
        mutate(across(where(is.character), str_trim))
    
    if (length(mode) == 1){
        if (mode == 'stage')
            trip$stage_mode <- smodes$exhaustive_list[match(trip$stage_mode, smodes$original)]
        else
            trip$trip_mode <- smodes$exhaustive_list[match(trip$trip_mode, smodes$original)]
    }else if (length(mode) == 2){
        if (all(mode %in% c('stage', 'trip'))){
            trip$trip_mode <- smodes$exhaustive_list[match(trip$trip_mode, smodes$original)]
            trip$stage_mode <- smodes$exhaustive_list[match(trip$stage_mode, smodes$original)]
        }
    }
    
    return(trip)
    
}
    


