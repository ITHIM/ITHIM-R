# Script to delete duplicates in GBD datasets
# Duplicates come from 95-99 age range in all causes
# Daniel Gil

#getwd()
library(readr)
library(dplyr)

rm(list=ls())

# Removing duplicates
cities <- c('accra', 'bangalore', 'belo_horizonte', 'bogota', 'buenos_aires', 'cape_town',
            'delhi', 'mexico_city', 'santiago', 'sao_paulo', 'vizag')

for (city in cities) {
  print(city)
  injuries <- read_csv(paste0("inst/extdata/local/", city, "/gbd_", city, ".csv"))
  injuries2 <- injuries %>% distinct() 
  print(paste0("Deleted rows: ", nrow(injuries) - nrow(injuries2)))
  write_csv(injuries2, paste0("inst/extdata/local/", city, "/gbd_", city, ".csv"))
}
