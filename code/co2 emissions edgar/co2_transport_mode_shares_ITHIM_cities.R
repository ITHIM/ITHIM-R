##code by Rahul Goel 
library(dplyr)
library(tidyr)
library(rgdal)
library(sp)
library(sf)
setwd('code/co2 emissions edgar/')

##reading CO2 files specific to each mode of transport
bus<- read.table('txt_TRO_TNR/monica_v5.0_EM_CO2_TRO_ROA_BS0_2012_CO2_2012.txt', header = T, sep=";")
bus$mode<- "bus"
all_modes<- bus

htrucks<- read.table('txt_TRO_TNR/monica_v5.0_EM_CO2_TRO_ROA_HD0_2012_CO2_2012.txt', header = T, sep=";")
htrucks$mode<- "htrucks"
all_modes<- rbind(all_modes, htrucks)

lttrucks<- read.table('txt_TRO_TNR/monica_v5.0_EM_CO2_TRO_ROA_LD0_2012_CO2_2012.txt', header = T, sep=";")
lttrucks$mode<- "lttrucks"
all_modes<- rbind(all_modes, lttrucks)

mtw<- read.table('txt_TRO_TNR/monica_v5.0_EM_CO2_TRO_ROA_MC0_MP0_2012_CO2_2012.txt', header = T, sep=";")
mtw$mode<- "mtw"
all_modes<- rbind(all_modes, mtw)

car<- read.table('txt_TRO_TNR/monica_v5.0_EM_CO2_TRO_ROA_PC0_2012_CO2_2012.txt', header = T, sep=";")
car$mode<- "car"
all_modes<- rbind(all_modes, car)

other<- read.table('txt_TRO_TNR/monica_v5.0_EM_CO2_TNR_OTH_2012_CO2_2012.txt', header = T, sep=";")
other$mode<- "other"
all_modes<- rbind(all_modes, other)

names(all_modes)[3]<-"co2emissions"

##converting this data with geo-coordinates to a point shape file
sp::coordinates(all_modes)<-~lon +lat
proj4string(all_modes)<-CRS("+proj=longlat +datum=WGS84")
all_modes<-SpatialPointsDataFrame(all_modes,as.data.frame(all_modes))
##could be saved as a shape file
##writeOGR(y,dsn='C:/Users/goelr/Work',layer='bus_co2_global3', driver="ESRI Shapefile")

####joining Buenos Aires and Cape Town######
## for these two cities, we do not already have the selection of coordinates within their city boundaries
select<-rgdal::readOGR('buenos aires cape town.shp')
latlong = "+proj=longlat +datum=WGS84"
select <- spTransform(select, CRS(latlong))

##intersecting emissions point shape file with city boundary polygon shape file of the two cities
x<-(over(all_modes,select))
x<- subset(x, select="Name")
x<- cbind(x,all_modes )
co2<- x[which(!is.na(x$Name)),]

emissions<-co2 %>% group_by(Name, mode) %>% summarise("total"=sum(co2emissions))
city_emissions<- spread(emissions,"mode","total" )
city_emissions$total<- rowSums(city_emissions[,2:7])
city_emissions[,2:7]<-round(city_emissions[,2:7]*100/city_emissions$total,2 )


all_modes<- all_modes@data
#all_modes$unique<- paste0(all_modes$lat, all_modes$lon)

###joining all other cities####
##reading the lookup table to join lat long with their respective cities (these geo-coordinates were identified by Nelson's team for PM2.5 emissions data)
lookup<- read.csv('lat_long_ithim_cities_edgar_emissions_lookup.csv')
lookup<- lookup[which(lookup$File=="Total"),]
lookup<- subset(lookup, select=c("lat", "lon","City"))

##attaching city names to emissions data using geo-coordinates
all_modes<- all_modes %>% left_join(lookup, by=c("lat", "lon"))

##removing all those rows with no city allocated
all_modes<- all_modes[which(!is.na(all_modes$City)),]

##renaming some city names
all_modes$City[which(substr(all_modes$City, 1,5) =="Bogot")]<-"Bogota"
all_modes$City[which(substr(all_modes$City, 4,9) ==" Paulo")]<-"Sao Paulo"


##the set of nine cities for which EDGAR PM2.5 was done earlier by Nelson's group
cities<- c("Delhi", "Bengaluru", "Belo Horizonte", "Accra", "Santiago", "Visakhapatnam","Mexico city", "Bogota", "Sao Paulo")

all_modes<- all_modes[which(all_modes$City %in% cities),]

emissions<-all_modes %>% group_by(City, mode) %>% summarise("total"=sum(co2emissions))
city_emissions_select<- spread(emissions,"mode","total" )
city_emissions_select$total<- rowSums(city_emissions_select[,2:7])
city_emissions_select[,2:7]<-round(city_emissions_select[,2:7]*100/city_emissions_select$total,2)

names(city_emissions)[1]<-"City"

####joining the emissions data of buenos aires and Cape Town with other nine cities
city_emissions<- rbind(as.data.frame(city_emissions),as.data.frame(city_emissions_select))

names(city_emissions)[4:5]<-c("heavytrucks", "lightdutytrucks")

write.csv(city_emissions, 'city_summary_co2.csv')


