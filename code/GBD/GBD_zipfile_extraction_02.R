#' ---
#' title: "Extraction of GBD files"
#' author: "Created by Marko in February-March 2019, modified by Daniel in November 2021"
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#' ---
#' 
#' This script extracts required Global Burden of Disease data from the zipped files.
#' First we extract the zip-files, then we read the csv files, adding required 
#' data to combined dataframe, and finally we delete the extracted files. 
#' The result is a dataframe that is saved as csv-file.

rm(list = ls())

#' Defining folders for the data
#' data_folder is the path where the downloaded zip files are.
#' temp_folder is the path where temporal extracted files are going to be placed
#' result_folder is the path where the extracted information is going to be exported
#' gbdfile_name is the first characters of the name of the zip files that are common to all of them
#' 
#' I ran everything local because it is faster, but if someone wants to run this
#' script from V-drive or other laptop, then only the path needs to be changed.
#' V-Drive folder
#data_folder <- "V:/Studies/MOVED/HealthImpact/Data/Global/Global_Burden_Disease_2019/"
#temp_folder <- paste0(data_folder,"temp")
#result_folder <- "V:/Studies/MOVED/HealthImpact/Projects/TIGTHAT/Case cities data/GBD 2019 data extraction/"
#gbdfile_name <- "IHME-GBD_2019_DATA-cc0f4132-"

#' Local folder
data_folder <- "/home/danielgils_server/consultorias/cambridge/data/GBD/2019/CountriesAndTerritoriesAndSubregions/"
temp_folder <- paste0(data_folder,"temp")
result_folder <- "/home/danielgils_server/consultorias/cambridge/data/GBD/2019/GBD_2019_data_extraction/"
gbdfile_name <- "IHME-GBD_2019_DATA-cc0f4132-" # This name will change if data from another year is downloaded
os_sep <- '/'
# os_sep <- '\\'

#' Next two lines defines countries that will be extracted
#' Dan: In the file "countries to be extracted I added Uruguay, Canada, USA, Peru
#' and New York because we have new data for these cities.
#' 
#' This file can be imported from the result_folder or the ithim package. Just 
#' make sure that both files have the same information
#' From the result_folder:
countries <- read.csv((paste0(result_folder, "Countries to be extracted.csv")))
#' From the ithim package:
#countries <- read.csv("code/GBD/Countries to be extracted.csv")

countries <- unlist(countries[,2])

data_extracted <- NULL

for (i in 1:78) { # Loop for all zip files (79 in total)
  print(paste0("folder: ", i))
  file_number <- i
  
  file_select <- paste0(data_folder,gbdfile_name, i,".zip") # Zip Filename

  unzip(file_select, exdir = temp_folder) # Extracts zip in temporal folder

  data_read <- read.csv((paste0(temp_folder,os_sep, gbdfile_name, i, ".csv")))
  file.remove(paste0(temp_folder,os_sep, gbdfile_name, i, ".csv"))
  data_read <- subset(data_read, location_id %in% countries)

  data_extracted <- rbind(data_extracted,data_read)
}

#' Export the data extracted
#unlink(paste0(temp_folder), recursive = TRUE)
write.csv(data_extracted, 
          file = paste0(result_folder, "GBD2019_countries_extracted.csv"), 
          row.names = FALSE)

#' Clean workspace
rm(data_folder, temp_folder, result_folder, gbdfile_name, countries, data_extracted, file_number, i, data_read, file_select)


