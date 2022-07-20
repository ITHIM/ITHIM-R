#This code matches the EDGAR geos-patial data sets to the 20 Latin American cities we are examining in ITHIM Global and produces and outputs the vehicle emissions inventory by each city in a text file
#SET UP
rm(list = ls())#clean work environment

#load libraries
library(dplyr)
library(sf)

######################
#PART 1: read EDGAR v6.1 emissions
######################
#set working directory, files needed for analysis are in our shared ITHIM Global Dropbox folder and also on Github at https://github.com/ITHIM/ITHIM-R/tree/latam_paper/code/pm25%20emissions%20edgar/EDGAR_v6.1_PM2.5_TRO_2018
setwd("C:/Users/hanee/Dropbox/ITHIM Global/Methods and Processes/Air Pollution/Latin American Cities Paper - Haneen/Emissions - sensitivity analysis/2018_EDGAR_PM2.5/EDGAR_v6.1_PM2.5_TRO_2018")

#read emissions from buses and view to check them
mode_bus <- read.table('./EDGAR_v6.1_PM2.5_EM_TRO_BS_PM2.5_2018.txt', header = T, sep=";") 
colnames(mode_bus)[3] <- "pm25_bus"
View(mode_bus)

#read emissions from heavy trucks and view to check them
mode_htruck <- read.table('./EDGAR_v6.1_PM2.5_EM_TRO_HD_PM2.5_2018.txt', header = T, sep=";") 
colnames(mode_htruck)[3] <- "pm25_htr"
View(mode_htruck)

#read emissions from light trucks and view to check them
mode_lttrucks <- read.table('./EDGAR_v6.1_PM2.5_EM_TRO_LD_PM2.5_2018.txt', header = T, sep=";") 
colnames(mode_lttrucks)[3] <- "pm25_ltt"
View(mode_lttrucks)

#read emissions from motorcycles and mopeds and view to check them
mode_mtw <- read.table('./EDGAR_v6.1_PM2.5_TRO_MC_MP_PM2.5_2018.txt', header = T, sep=";") 
colnames(mode_mtw)[3] <- "pm25_mtw"
View(mode_mtw)

#read emissions from passenger cars and view to check them
mode_car <- read.table('./EDGAR_v6.1_PM2.5_EM_TRO_PC_PM2.5_2018.txt', header = T, sep=";") 
colnames(mode_car)[3] <- "pm25_car"
View(mode_car)

#merge mode emissions in one data.frame called "modes"
modes <- merge(mode_bus, mode_htruck, by = c("lat", "lon"), all = TRUE)
modes <- merge(modes, mode_lttrucks, by = c("lat", "lon"), all = TRUE)
modes <- merge(modes, mode_mtw, by = c("lat", "lon"), all = TRUE)
modes <- merge(modes, mode_car, by = c("lat", "lon"), all = TRUE)

#create spatial points 'sf' object 
modes <- modes %>% st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84")

#check if all observations are contained and view the data frame
sum(!is.na(modes$pm25_bus))
sum(!is.na(modes$pm25_htr))
sum(!is.na(modes$pm25_ltt))
sum(!is.na(modes$pm25_mtw))
sum(!is.na(modes$pm25_car))
View(modes)

######################
#PART 2: add Latin American city names
######################
#read cities boundaries from the same directory in our shared ITHIM Global Dropbox folder. The shape file is also available on Github at https://github.com/ITHIM/ITHIM-R/blob/latam_paper/code/pm25%20emissions%20edgar/All%20Latin%20American%20Cities%20Boundaries.zip 
cities <- st_read(file.path("./cities.shp")) %>% st_zm() #drop Z coordinate
sf::sf_use_s2(FALSE)

#spatial join of attributes by location
modes <- st_join(modes, cities, join = st_intersects) 

#remove cities not in south america
modes <- modes[!is.na(modes$name),]

st_write(modes, dsn = file.path("./PM25_2018_All_Latin_American_Cities.shp"), driver = "ESRI Shapefile")


######################
#PART 3: get percentages of mode shares for each city
######################
#summarize mode shares found in same city (drop spatial geometry)
modes_analysis <- modes %>% group_by(name) %>% summarise(pm25_bus = sum(pm25_bus, na.rm = TRUE), pm25_car = sum(pm25_car, na.rm = TRUE), 
                                                         pm25_htr = sum(pm25_htr, na.rm = TRUE), pm25_ltt = sum(pm25_ltt, na.rm = TRUE),
                                                         pm25_mtw = sum(pm25_mtw, na.rm = TRUE)) %>% st_drop_geometry() 
#get percentage of mode shares
modes_analysis <- modes_analysis %>% group_by(name) %>% mutate(bus_share = (pm25_bus / sum(pm25_bus, pm25_car, pm25_htr, pm25_ltt, pm25_mtw)),
                                                               car_share = (pm25_car / sum(pm25_bus, pm25_car, pm25_htr, pm25_ltt, pm25_mtw)),
                                                               htr_share = (pm25_htr / sum(pm25_bus, pm25_car, pm25_htr, pm25_ltt, pm25_mtw)),
                                                               ltt_share = (pm25_ltt / sum(pm25_bus, pm25_car, pm25_htr, pm25_ltt, pm25_mtw)),
                                                               mtw_share = (pm25_mtw / sum(pm25_bus, pm25_car, pm25_htr, pm25_ltt, pm25_mtw)))
#write output as .txt file
write.table(modes_analysis, file.path("./PM25_2018_20cities.txt"), sep = ";", na = "NA", row.names = FALSE, col.names = TRUE)
#note that only 18 cities will be outputted because data is missing for two cities. We used the EDGAR data from the closest city to assign missing data for these two cities
#Iquique and Alto Hospicio: use values from Antofagasta
#Temuco: use values from Valdivia
