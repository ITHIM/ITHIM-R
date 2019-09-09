package <- function(){
    library(tidyverse)
    library(readxl)
    library(haven)
    library(nnet)
    quality_check <<- function(trip){
        proportion_people_with_trips <- trip %>% 
            filter(!is.na(trip_id)) %>% 
            distinct(participant_id) %>% 
            nrow() / 
            trip %>% 
            distinct(participant_id) %>% 
            nrow()
        trip_per_capita <- trip %>% 
            filter(!is.na(trip_id)) %>% 
            count(participant_id, trip_id) %>% 
            nrow() / 
            trip %>% 
            distinct(participant_id) %>% 
            nrow()
        mode_share <- trip %>% 
            filter(!is.na(trip_id)) %>% 
            group_by(trip_mode) %>% 
            summarise(mode_share = n()*100/nrow(.))
        avg_mode_time <- trip %>% 
            filter(!is.na(trip_id)) %>% 
            group_by(trip_mode) %>% 
            summarise(avg_mode_time = mean(trip_duration, na.rm = T))
        
        
        cat("proportion of people with trips = ", round(proportion_people_with_trips*100),"%", "\n")
        cat("trip per capita = ", round(trip_per_capita, 1),"trips/person", "\n")
        print(mode_share)
        print(avg_mode_time)
        
        # par(mfrow=c(2,2))
        # plot(trip$trip_duration)
        # boxplot(trip_duration ~ trip_mode, trip)
        # hist(trip$trip_duration[which(trip$trip_mode == "walk")], breaks = 50)
        # plot(density(trip$trip_duration[which(trip$trip_mode == "walk")], bw = 100))
    }
    
}