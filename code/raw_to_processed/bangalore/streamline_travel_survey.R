# Clear workspace
rm (list = ls())

# Load libraries
library(tidyverse)
library(plotly)

# Read bangalore travel survey with stages
raw_data <- read.csv("data/local/bangalore/bangalore_travel_survey_2011_March_14.csv", stringsAsFactors = F)
