#Extracts population data for selected countries

#Created in February-March 2019 by Marko Tainio

#Defining folder where the data is
data_folder <- "V:\\Studies\\MOVED\\HealthImpact\\Data\\Global_Burden_Disease_2017\\"
temp_folder <- paste0(data_folder,"temp")
result_folder <- "V:\\Studies\\MOVED\\HealthImpact\\Research\\TIGTHAT\\Case cities data\\GBD 2017 data extraction\\"
gbdfile_name <- "IHME_GBD_2017_POP_2015_2017.zip"

#Next two lines defines countries that will be extracted
countries <- read.csv((paste0(result_folder, "Countries to be extracted.csv")))
countries <- unlist(countries[,2])

file_select <- paste0(data_folder,gbdfile_name)

unzip(file_select, exdir=temp_folder)
  
data_read <- read.csv((paste0(temp_folder,"\\", "IHME_GBD_2017_POP_2015_2017_Y2018M11D08.CSV")))
file.remove(paste0(temp_folder,"\\", "IHME_GBD_2017_POP_2015_2017_Y2018M11D08.CSV"))
data_read <- subset(data_read, location_id %in% countries)
  

unlink(paste0(temp_folder), recursive = TRUE)
write.csv(data_read, file =paste0(result_folder, "GBD2017_population_extracted.csv"), row.names=FALSE)




rm(data_folder, temp_folder, result_folder, gbdfile_name, countries, data_read, file_select)


