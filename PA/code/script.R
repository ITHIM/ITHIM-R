# Load packages
library(tidyverse)
# Read combined individual travel survey and Physical Activity data
pop <- read.csv("PA/data/180219_Metahit10000_v2_nolabel.csv", header = T, stringsAsFactors = F)
# Initialize  energy expenditure constants
METcycling <- 5.63
METwalking <- 3.53
METebikes <- 4.50


#
