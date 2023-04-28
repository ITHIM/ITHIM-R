#' ---
#' title: "Preprocessing of GBD and population data"
#' author: "Created by Marko in February-March 2019, modified by Daniel in November 2021. 
#' Further modified by Ali in April 2023"
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#' ---
#' 
#' This script preprocess the GBD data and merge the population data to it. The
#' result is a dataframe for each place in the right format for ITHIM-R package

rm(list = ls())
library(dplyr)

#' Defining folders for the data
#' result_folder is the path where the extracted information is 
#' country_results is the path where the GBD for each file is going to be exported
#' 
#' I ran everything local because it is faster, but if someone wants to run this
#' script from V-drive or other laptop, then only the path needs to be changed.
#' V-Drive folder
result_folder <- "GBD 2019 data extraction/"
country_results <- "GBD 2019 Countries/"


#' Local folder
# result_folder <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/GBD/2019/GBD 2019 data extraction/"
# country_results <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/GBD/2019/GBD 2019 Countries/"

#' The following files can be imported from the result_folder or the ithim package.
#' Just make sure that both files have the same information.
#' 
#' Select causes (health outcomes) to be extracted
#' 
# From the result_folder:
# causes <- read.csv((paste0(result_folder, "Causes to be extracted.csv")))
# causes <- unlist(causes[,2])
# 
# Read from the local list of causes
causes <- read.csv("code/GBD/Causes to be extracted.csv")
causes <- unlist(causes[,2])

# From the result_folder:
data_read <- read.csv(paste0(result_folder, "GBD2019_countries_extracted.csv"))

#Keep only causes defined in "Causes to be extracted.csv" file
data_read <- subset(data_read, cause_id %in% causes)

#Remove rates from the data
data_read <- subset(data_read, metric_name %in% "Number")

#Change the names of age groups
data_read$age_name <- gsub('Under 5', '0 to 4', data_read$age_name)
data_read$age_name <- gsub('95 plus', '95 to 99', data_read$age_name)

#Read age group data that defines what age groups are needed
age_groups <- read.csv("code/GBD/Age_groups.csv")
age_groups <- unlist(age_groups["age_name"])

data_read <- subset(data_read, age_name %in% age_groups)

#########
#Read and select population data
# From the result_folder:
popdata_read <- read.csv(paste0(result_folder, "GBD2019_population_extracted.csv"))

# Keep only year 2019 population estimates
popdata_read <- subset(popdata_read, year_id %in% 2019)
popdata_read <- subset(popdata_read, sex_name %in% c("male", "female"))

#Transform to upper case sex_name
popdata_read$sex_name <- ifelse(popdata_read$sex_name == 'female', 'Female',
                                'Male')

#Chanage names of age groups
popdata_read$age_group_name <- gsub('Under 5', '0 to 4', popdata_read$age_group_name)
popdata_read$age_group_name <- gsub('95 plus', '95 to 99', popdata_read$age_group_name)
#Remove extra age groups
popdata_read <- subset(popdata_read, age_group_name %in% age_groups)
#Change column name for age to allow left_join
names(popdata_read)[names(popdata_read) == 'age_group_name'] <- 'age_name'
#val to population, not to mix with the val of GBD data
names(popdata_read)[names(popdata_read) == 'val'] <- 'population'
# Change factor for "sex_name" to remove error message in join command
#popdata_read$sex_name <- factor(popdata_read$sex_name)

# Delete duplicates (in case they exist)
popdata_read <- popdata_read %>% distinct() 

############
#Combine burden and population data for one dataframe
#This works but gives error if burden data wont include all the countries.
#(due to differences in factors)

join_data <- left_join(data_read, popdata_read, 
                       by = c("age_name", "sex_name", "location_name"))
rm(popdata_read, data_read)

#Defines measures that are extracted
#measures <- read.csv((paste0(result_folder, "Measure_extracted.csv")))
#measures <- unlist(measures[,1])
measures <- c("Deaths", "YLLs (Years of Life Lost)")

#Columns to be extracted for further analyses, and measures needed
col_extracted <- c("measure_name.x", "location_name", "sex_name", "age_name", "cause_name", "val", "population")
join_data <- subset(join_data, measure_name.x %in% measures, select = col_extracted)

# Delete duplicates (in case they exist)
join_data <- join_data %>% distinct() 

############
#Save each country as separate csv-file
#First read the names of the countries
cname <- read.csv("code/GBD/Countries to be extracted.csv")
cname <- cname$Country_or_location


cities <- data.frame(
  country_region = c('Ghana', 'São Paulo', 'India', 'India', 'India', 'Minas Gerais', 'Colombia', 'Chile', 'Mexico City', 'Argentina', 'South Africa', 'Colombia', 'Colombia', 'Uruquay', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Kenya', 'Kenya', 'Mauritius'),
  city = c('accra', 'sao_paulo', 'delhi', 'bangalore', 'vizag', 'belo_horizonte', 'bogota', 'santiago', 'mexico_city', 'buenos_aires', 'cape_town', 'medellin', 'cali', 'montevideo', 'antofagasta', 'arica', 'copiapo', 'coquimbo_laserena', 'iquique_altohospicio', 'osorno', 'puerto_montt', 'san_antonio', 'temuco_padrelascasas', 'valdivia', 'gran_valparaiso', 'nairobi', 'kisumu', 'port_louis'),
  stringsAsFactors = FALSE
)

for (i in 1:length(cname)) { # Loop for each place (country or regior or city)
  
  # Filter rows for the place of the iteration
  a <- subset(join_data, location_name %in% cname[i])
  
  # Exports the dataset
  write.csv(a,
  file = paste0(country_results,cname[i], "_GBD_results.csv"),
  row.names = FALSE)
  
  # Get specific country for a group of cities
  mc <- cities |> filter(country_region == cname[i])
  
  # Update the GBD dataset in the relevant installation folder, 
  # such as inst/extdata/local/accra/gbd_accra.csv
  for (city in mc$city){
    readr::write_csv(a, paste0("inst/extdata/local/", city, "/gbd_", city, ".csv"))
  }
  rm(a)
}

#In the end remove all
rm(join_data, age_groups, causes, cname, col_extracted, country_results, i, measures, result_folder)