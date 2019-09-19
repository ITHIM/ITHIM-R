package <- function(){
    #required packages
    library(tidyverse)
    library(readxl)
    library(haven)
    library(nnet)
    library(foreign)
    library(gsubfn)
    
    #mode speeds --> pls add other modes and their average mode speeds.
    mode_speed <<- data.frame(mode = c("bicycle","bus","car","metro", "motorcycle",
                                             "other", "rickshaw", "taxi", "train", "walk" ),
                              mode_speed = c(15, 15, 25, 25, 25,21 ,25,25, 30, 5 ))
    
    #function for evaluating data quality
    quality_check <<- function(trip){
        trip$trip_mode <- factor(trip$trip_mode, levels = c("bicycle","bus","car","metro", "motorcycle",
                                                            "other", "rickshaw", "taxi", "train", "walk" ))
        total_household <-trip %>% 
            count(cluster_id, household_id) %>%
            nrow
        total_participant <- trip %>% 
            count(cluster_id, household_id, participant_id) %>%
            nrow
        proportion_adult_participant <- trip %>% 
            filter(age>17) %>% 
            count(cluster_id, household_id, participant_id) %>% 
            nrow*100/
            total_participant
        household_size <- total_participant/total_household
        adult_per_house <- trip %>% 
            filter(age>17) %>% 
            count(cluster_id, household_id, participant_id) %>% 
            nrow/
            total_household
        male_propotion <- 
            trip %>% 
            filter(sex == "Male") %>% 
            count(cluster_id, household_id, participant_id) %>%
            nrow /
            trip %>% 
            count(cluster_id, household_id, participant_id) %>%
            nrow
        female_proportion <- 
            trip %>% 
            filter(sex == "Female") %>% 
            count(cluster_id, household_id, participant_id) %>%
            nrow /
            trip %>% 
            count(cluster_id, household_id, participant_id) %>%
            nrow
        proportion_people_with_trips <- trip %>% 
            filter(!is.na(trip_id)) %>% 
            count(cluster_id, household_id, participant_id) %>% 
            nrow/ 
            trip %>% 
            count(cluster_id, household_id, participant_id)  %>% 
            nrow
        proportion_household_with_trip <-  trip %>% 
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
        proportion_male_with_trip <- 
            trip %>%
            filter(sex == "Male" & !is.na(trip_id)) %>% 
            count(cluster_id, household_id, participant_id) %>% 
            nrow /
            trip %>% 
            filter(sex == "Male") %>% 
            count(cluster_id, household_id, participant_id) %>% 
            nrow 
         proportion_female_with_trip <- trip %>%
             filter(sex == "Female" & !is.na(trip_id)) %>% 
             count(cluster_id, household_id, participant_id) %>% 
             nrow /
             trip %>% 
             filter(sex == "Female") %>% 
             count(cluster_id, household_id, participant_id) %>% 
             nrow 
        trip_per_capita <- trip %>% 
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
        trip_purpose <- 
            trip %>%
            filter(!is.na(trip_id)) %>% 
            group_by(trip_purpose) %>%
            summarise(share = round(n()*100/nrow(.),1))
        mode_share <- 
            trip %>% 
            filter(!is.na(trip_id)) %>% 
            group_by(trip_mode, .drop =FALSE) %>% 
            summarise(mode_share = round(n()*100/nrow(.),1))
        avg_mode_time <- trip %>% 
            filter(!is.na(trip_id)) %>% 
            group_by(trip_mode, .drop = FALSE) %>% 
            summarise(avg_mode_time = round(mean(trip_duration, na.rm = T),1))
        
        
        #printed outputs
        cat("number of households                     =","\t",total_household,sep = "" , "\n")
        cat("number of individuals (adults> 17yrs)    =","\t", total_participant,
            "(", round(proportion_adult_participant),")",sep = "", "\n")
        cat("average household size                   = ","\t",round(household_size,1),sep = "", "\n")
        cat("avg number of adults per houshold        =","\t",round(adult_per_house,1),sep = "","\n")
        cat("proportion of males to females           =","\t",round(male_propotion*100),":",
            round(female_proportion*100),sep = "","\n")
        cat("proportion of people with trips          = ", "\t",round(proportion_people_with_trips*100),sep = "", "\n")
        cat("avg prop of houshold members with trips  =","\t",round(proportion_household_with_trip$value*100),sep = "","\n")
        cat("trip distribution by (male : female)     =", "\t", round(male_trip_fraction*100), ":",
            round(female_trip_fraction*100),sep = "","\n")
        cat("male trip rate cp with female            =","\t",round(proportion_male_with_trip*100),
            ":",round(proportion_female_with_trip*100),sep = "", "\n")
        cat("trip per capita (adults > 17)            = ","\t", round(trip_per_capita, 1),
            "(",round(trip_per_capita_adult,1),")",sep = "","\n")
        
        print(trip_purpose)
        print(mode_share)
        print(avg_mode_time)
        
        
        
        #for copying to excel
        write.table(trip_purpose)
        write.table(mode_share)
        write.table(avg_mode_time)
        # par(mfrow=c(2,2))
        # plot(trip$trip_duration)
        # boxplot(trip_duration ~ trip_mode, trip)
        # hist(trip$trip_duration[which(trip$trip_mode == "walk")], breaks = 50)
        # plot(density(trip$trip_duration[which(trip$trip_mode == "walk")], bw = 100))
    }
    
}