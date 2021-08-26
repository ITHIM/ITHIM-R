#Extracts population data for selected countries

#Created in February-March 2019 by Marko Tainio
# Modified in August 2021 by Daniel Gil

rm(list = ls())

#Defining folder where the data is
#data_folder <- "V:/Studies/MOVED/HealthImpact/Data/Global/Global_Burden_Disease_2019/"
#temp_folder <- paste0(data_folder,"temp")
#result_folder <- "V:/Studies/MOVED/HealthImpact/Projects/TIGTHAT/Case cities data/GBD 2019 data extraction/"
#gbdfile_name <- "IHME_GBD_2019_POP_2010_2019_0.zip"

# I am running this locally because it is faster
# If we want to run from v-drive just uncomment lines 7-10
data_folder <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/GBD/2019/CountriesAndTerritoriesAndSubregions/"
temp_folder <- paste0(data_folder,"temp")
result_folder <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/GBD/2019/GBD 2019 data extraction/"
gbdfile_name <- "IHME_GBD_2019_POP_2010_2019_0.zip"

#Next two lines defines countries that will be extracted
countries <- read.csv((paste0(result_folder, "Countries to be extracted.csv")))
countries <- unlist(countries[,2])

file_select <- paste0(data_folder,gbdfile_name)

unzip(file_select, exdir=temp_folder)
  
data_read <- read.csv((paste0(temp_folder,"\\", "IHME_GBD_2019_POP_2019_Y2020M10D15.CSV")))
file.remove(paste0(temp_folder,"\\", "IHME_GBD_2019_POP_2019_Y2020M10D15.CSV"))
data_read <- subset(data_read, location_id %in% countries)
  

unlink(paste0(temp_folder), recursive = TRUE)
write.csv(data_read, 
          file = paste0(result_folder, "GBD2019_population_extracted.csv"), 
          row.names = FALSE)




#rm(data_folder, temp_folder, result_folder, gbdfile_name, countries, data_read, file_select)


