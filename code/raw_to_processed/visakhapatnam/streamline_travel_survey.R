# Clear workspace
rm (list = ls())

# Load libraries
library(tidyverse)
library(summarytools)

# Read visakhapatnam's travel survey
raw_rd <- read_csv("data/local/vizag/visakhapatnam_trips.csv")

# Expand dataset by wk_freq
rd <- raw_rd %>% mutate(wk_freq = if_else(is.na(wk_freq), 1, wk_freq)) %>% uncount(wk_freq, .id = "trip_id")

# Print mode share
rd %>% filter(!is.na(mode)) %>% group_by(mode, .drop = F) %>% summarise(mode_share = round(n()*100/nrow(.),1))