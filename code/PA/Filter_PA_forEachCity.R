# Script to filter the PA dataset for each city
# The file "combined_ltpa.csv" was sent by Lambed over slack
# I have to ask him to upload the code that created it

rm(list = ls())

# Loading libraries
library(readr)
library(dplyr)

# Reading the file
pa <- read_csv("code/PA/combined_ltpa.csv") %>% 
  rename(work_ltpa_marg_met = ltpa_marg_met)

# creating equivalence table
unique(pa$city)
cities <- data.frame(cities = c("accra", "bangalore", "belo_horizonte",
                                "bogota", "buenos_aires", "cape_town",
                                "delhi", "mexico_city", "santiago",
                                "sao_paulo", "vizag"),
                     pa_cities = c("Accra", "Bangalor_Vizag", "Belo_Horizonte",
                                   "Bogota", "Buenos_Aires", "Cape_Town",
                                   "Delhi", "Mexico", "Santiago", 
                                   "Sao_Paulo", "Bangalor_Vizag")
                    )
View(cities)

# Exporting to corresponding path
for (i in 1:nrow(cities)) {
for (i in 1:1) {
  #print(cities$pa_cities[i])
  temp <- pa %>% filter(city == cities$pa_cities[i]) %>% 
    dplyr::select(id, age, sex, work_ltpa_marg_met)
  write_csv(temp, paste0("inst/extdata/local/pa_", cities$cities[i],".csv"))
}

