# Clear workspace
rm (list = ls())

# Load libraries
library(tidyverse)
library(plotly)

# Read bangalore travel survey with stages
raw_data <- read.csv("data/local/bangalore/bangalore_travel_survey_2011_March_14.csv", stringsAsFactors = F)

#####
## Remove duplicate rows
raw_data <- raw_data[!duplicated(raw_data), ]

## Remove duplicated rows
raw_data <- raw_data[(!duplicated(raw_data$uniqueid )) | (raw_data$uniqueid == "NA_NA"),]

## Remove uniqueid
raw_data$uniqueid <- NULL

# Save read data in a separate var
rd <- raw_data

#####
## Remove duplicate rows for people without trips
rd_pwt <- rd %>% filter(!is.na(trip_id))

rd_pwot <- rd %>% filter(is.na(trip_id))

rd_pwot <- rd_pwot[!duplicated(rd_pwot$person_id), ]

rd <- rbind(rd_pwt, rd_pwot)