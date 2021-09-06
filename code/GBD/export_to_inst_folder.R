# Script to read GBD files in each place and export them in the "inst" folder
# I do this city by city because there are some places (like US and Canada)
# that we are not going to use yet.
# Author: Daniel Gil

library(readr)

#country_results <- "V:/Studies/MOVED/HealthImpact/Projects/TIGTHAT/Case cities data/GBD 2019 Countries/"
# I do it locally because it is faster
country_results <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/GBD/2019/GBD 2019 Countries/"

# Accra
dataset <- read_csv(paste0(country_results, "Ghana_GBD_results.csv"))
write_csv(dataset, "code/GBD/Ghana_GBD_results.csv")
write_csv(dataset, "inst/extdata/local/accra/gbd_accra.csv")

# Bangalore, Delhi and Vizag
dataset <- read_csv(paste0(country_results, "India_GBD_results.csv"))
write_csv(dataset, "code/GBD/India_GBD_results.csv")
write_csv(dataset, "inst/extdata/local/bangalore/gbd_bangalore.csv")
write_csv(dataset, "inst/extdata/local/delhi/gbd_delhi.csv")
write_csv(dataset, "inst/extdata/local/vizag/gbd_vizag.csv")

# Belo Horizonte
dataset <- read_csv(paste0(country_results, "Minas Gerais_GBD_results.csv"))
write_csv(dataset, "code/GBD/Minas Gerais_GBD_results.csv")
write_csv(dataset, "inst/extdata/local/belo_horizonte/gbd_belo_horizonte.csv")

# Bogota
dataset <- read_csv(paste0(country_results, "Colombia_GBD_results.csv"))
write_csv(dataset, "code/GBD/Colombia_GBD_results.csv")
write_csv(dataset, "inst/extdata/local/bogota/gbd_bogota.csv")

# Buenos Aires
dataset <- read_csv(paste0(country_results, "Argentina_GBD_results.csv"))
write_csv(dataset, "code/GBD/Argentina_GBD_results.csv")
write_csv(dataset, "inst/extdata/local/buenos_aires/gbd_buenos_aires.csv")

# Cape Town
dataset <- read_csv(paste0(country_results, "South Africa_GBD_results.csv"))
write_csv(dataset, "code/GBD/South Africa_GBD_results.csv")
write_csv(dataset, "inst/extdata/local/cape_town/gbd_cape_town.csv")

# Mexico City
dataset <- read_csv(paste0(country_results, "Mexico City_GBD_results.csv"))
write_csv(dataset, "code/GBD/Mexico City_GBD_results.csv")
write_csv(dataset, "inst/extdata/local/mexico_city/gbd_mexico_city.csv")

# Santiago
dataset <- read_csv(paste0(country_results, "Chile_GBD_results.csv"))
write_csv(dataset, "code/GBD/Chile_GBD_results.csv")
write_csv(dataset, "inst/extdata/local/santiago/gbd_santiago.csv")

# Sao Paulo
dataset <- read_csv(paste0(country_results, "SÃ£o Paulo_GBD_results.csv"))
write_csv(dataset, "code/GBD/Sao Paulo_GBD_results.csv")
write_csv(dataset, "inst/extdata/local/sao_paulo/gbd_sao_paulo.csv")
