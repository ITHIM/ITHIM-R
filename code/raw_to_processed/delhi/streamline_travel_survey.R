# Clear workspace
rm (list = ls())

# Load libraries
library(tidyverse)
library(plotly)

# Read delhi travel survey with stages
rd <- read_csv("data/local/delhi/delhi_travel_survey.csv")

rd$pid <- -1
id <- 1
pid_list <- unique(rd$person_id)
for(i in 1:length(pid_list)) {
  pg <- filter(rd, person_id == pid_list[i])
  if (!is.na(pg) && !is.na(pg$hh_weights)){
    count <- round(pg$hh_weights[1])
    if (count == 0){
      rd[rd$person_id == pid_list[i],]$pid <- id
      id <- id + 1
    }
    if (count > 0){
      d <- bind_rows(replicate((count), pg, simplify = FALSE))
      d <- arrange(d, trip_id)
      rd[rd$person_id == pid_list[i],]$pid <- id
      d$pid <- rep((id + 1):(id + count), nrow(pg))
      id <- id + count
      rd <- bind_rows(rd, d)
    }
  }else{
    
    rd[rd$person_id == pid_list[i],]$pid <- id
    id <- id + 1
  }
}
