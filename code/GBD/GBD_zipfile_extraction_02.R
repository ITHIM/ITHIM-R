#This script extracts required Global Burden of Disease data from the zip files
#by first extracting zip-files, then reading csv file, adding required data to combined dataframe
#and finally deleiting extracted files. Resulting dataframe is then saved as csv-file.

#Created in February-March 2019 by Marko Tainio

#Defining folder where the data is
data_folder <- "V:\\Studies\\MOVED\\HealthImpact\\Data\\Global_Burden_Disease_2017\\"
temp_folder <- paste0(data_folder,"temp")
result_folder <- "V:\\Studies\\MOVED\\HealthImpact\\Research\\TIGTHAT\\Case cities data\\GBD 2017 data extraction\\"
gbdfile_name <- "IHME-GBD_2017_DATA-2d3c1979-"

#Next two lines defines countries that will be extracted
countries <- read.csv((paste0(result_folder, "Countries to be extracted.csv")))
countries <- unlist(countries[,2])

data_extracted <- NULL

for (i in 1:91) {
  file_number <- i
  
  file_select <- paste0(data_folder,gbdfile_name, i,".zip")

  unzip(file_select, exdir=temp_folder)

  data_read <- read.csv((paste0(temp_folder,"\\", gbdfile_name, i, ".csv")))
  file.remove(paste0(temp_folder,"\\", gbdfile_name, i, ".csv"))
  data_read <- subset(data_read, location_id %in% countries)

  data_extracted <-rbind(data_extracted,data_read)
}

unlink(paste0(temp_folder), recursive = TRUE)
write.csv(data_extracted, file =paste0(result_folder, "GBD2017_countries_extracted.csv"), row.names=FALSE)

rm(data_folder, temp_folder, result_folder, gbdfile_name, countries, data_extracted, file_number, i, data_read, file_select)


