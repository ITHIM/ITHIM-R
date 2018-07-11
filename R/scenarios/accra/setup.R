# Clear workspace
rm (list = ls())

# Load base required libraries
pkgs <- c("tidyverse", "drpa")
lapply(pkgs, library, character.only = T)