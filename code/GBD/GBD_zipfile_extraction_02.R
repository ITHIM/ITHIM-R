#This script extracts required Global Burden of Disease data from the zip files
#by first extracting zip-files, then reading csv file, adding required data to combined dataframe
#and finally deleiting extracted files. Resulting dataframe is then saved as csv-file.

#Created in February-March 2019 by Marko Tainio
# Modified in August 2021 by Daniel Gil

rm(list = ls())

#Defining folder where the data is
#data_folder <- "V:/Studies/MOVED/HealthImpact/Data/Global/Global_Burden_Disease_2019/"
#temp_folder <- paste0(data_folder,"temp")
#result_folder <- "V:/Studies/MOVED/HealthImpact/Projects/TIGTHAT/Case cities data/GBD 2019 data extraction/"
#gbdfile_name <- "IHME-GBD_2019_DATA-b1e8e391-"

# I am running this locally because it is faster
# If we want to run from v-drive just uncomment lines 9-12
data_folder <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/GBD/2019/CountriesAndTerritoriesAndSubregions/"
temp_folder <- paste0(data_folder,"temp")
result_folder <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/GBD/2019/GBD 2019 data extraction/"
gbdfile_name <- "IHME-GBD_2019_DATA-b1e8e391-"

#Next two lines defines countries that will be extracted
# Dan: In the file "countries to be extracted I added Uruguay, Canada, USA, Peru
# and New York because we have new data for these cities.
countries <- read.csv((paste0(result_folder, "Countries to be extracted.csv")))
countries <- unlist(countries[,2])

data_extracted <- NULL

for (i in 1:79) {
  print(paste0("folder: ", i))
  file_number <- i
  
  file_select <- paste0(data_folder,gbdfile_name, i,".zip")

  unzip(file_select, exdir=temp_folder)

  data_read <- read.csv((paste0(temp_folder,"\\", gbdfile_name, i, ".csv")))
  file.remove(paste0(temp_folder,"\\", gbdfile_name, i, ".csv"))
  data_read <- subset(data_read, location_id %in% countries)

  data_extracted <- rbind(data_extracted,data_read)
}

#unlink(paste0(temp_folder), recursive = TRUE)
write.csv(data_extracted, 
          file = paste0(result_folder, "GBD2019_countries_extracted.csv"), 
          row.names = FALSE)

rm(data_folder, temp_folder, result_folder, gbdfile_name, countries, data_extracted, file_number, i, data_read, file_select)


