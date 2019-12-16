# Clear workspace
rm (list = ls())

# Load libraries
library(tidyverse)
library(summarytools)

# Read capetown's travel survey
raw_rd <- read_csv("data/local/cape_town/cape_town_trip.csv")

rd <- raw_rd
