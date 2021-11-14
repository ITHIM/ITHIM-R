##code by Rahul Goel
##this code uses EDGAR PM2.5 emissions datasets with geocodes and extracts that data for case cities
##see code in code\co2 emissions edgar for CO2 emissions, that uses the same method
##the work to extract PM2.5 emissions was done by Nelson's group for some cities,
##for some cities we are using corresponding pmimary studies
##this particular code is extracting this data for Buenos Aires and Cape Town
library(dplyr)
library(tidyr)
library(rgdal)
library(sp)
library(sf)

rm(list = ls())
#setwd('code/pm25 emissions edgar/EDGARv4.3.2_PM2.5_TRO_TNR_data_2012')
path <- 'code/pm25 emissions edgar/EDGARv4.3.2_PM2.5_TRO_TNR_data_2012/'
bus <- read.table(paste0(path, 'EDGAR_v4.3.2_PM2.5_EM_TRO_BS_PM2.5_2012.txt'), 
                 header = T, sep = ";")
bus$mode <- "bus"
all_modes <- bus

htrucks <- read.table(paste0(path, 'EDGAR_v4.3.2_PM2.5_EM_TRO_HD_PM2.5_2012.txt'),
                      header = T, sep = ";")
htrucks$mode <- "htrucks"
all_modes <- rbind(all_modes, htrucks)

lttrucks <- read.table(paste0(path, 'EDGAR_v4.3.2_PM2.5_EM_TRO_LD_PM2.5_2012.txt'),
                       header = T, sep = ";")
lttrucks$mode <- "lttrucks"
all_modes <- rbind(all_modes, lttrucks)

mtw <- read.table(paste0(path, 'EDGAR_v4.3.2_PM2.5_TRO_MC_MP_PM2.5_2012.txt'), 
                  header = T, sep = ";")
mtw$mode <- "mtw"
all_modes <- rbind(all_modes, mtw)

car <- read.table(paste0(path, 'EDGAR_v4.3.2_PM2.5_EM_TRO_PC_PM2.5_2012.txt'), 
                  header = T, sep = ";")
car$mode <- "car"
all_modes <- rbind(all_modes, car)

other <- read.table(paste0(path, 
                           'EDGAR_v4.3.2_PM2.5_EM_TNR_OTH_RAI_PIP_2012.txt'), 
                    header = T, sep = ";")
other$mode <- "other"
all_modes <- rbind(all_modes, other)

names(all_modes)[3] <- "pm25emissions"

####joining Buenos Aires and Cape Town######
## for these two cities, we do not already have the selection of coordinates within their city boudnary
sp::coordinates(all_modes) <- ~lon +lat
proj4string(all_modes) <- CRS("+proj=longlat +datum=WGS84")
all_modes <- SpatialPointsDataFrame(all_modes,as.data.frame(all_modes))
##could be saved as a shape file
# writeOGR(all_modes,dsn='C:/Users/danie/Downloads',layer='bus_co2_global3', driver="ESRI Shapefile")

#select <- rgdal::readOGR('code/pm25 emissions edgar/buenos aires cape town.shp')
select <- rgdal::readOGR('code/pm25 emissions edgar/cities.shp')
latlong = "+proj=longlat +datum=WGS84"
select <- spTransform(select, CRS(latlong))
##intersection
x <- (over(all_modes,select))
x <- subset(x, select = "Name")
x <- cbind(x,all_modes)
pm25 <- x[which(!is.na(x$Name)),]

emissions <- pm25 %>% group_by(Name, mode) %>% 
  summarise("total" = sum(pm25emissions))
city_emissions <- spread(emissions, "mode", "total")
city_emissions$total <- rowSums(city_emissions[,2:7], na.rm = T)
city_emissions[,2:7] <- city_emissions[,2:7]/city_emissions$total
as.data.frame(city_emissions)
write.table(as.data.frame(city_emissions), "clipboard", sep = "\t", row.names = F)
