# Clear workspace
rm (list = ls())

# Load libraries
library(tidyverse)
library(summarytools)

# Read santiago's travel survey
raw_rd <- read_csv("data/local/vizag/visakhapatnam_trips.csv")