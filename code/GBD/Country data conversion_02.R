
library(dplyr)

result_folder <- "V:\\Studies\\MOVED\\HealthImpact\\Research\\TIGTHAT\\Case cities data\\GBD 2017 data extraction\\"
country_results <- "V:\\Studies\\MOVED\\HealthImpact\\Research\\TIGTHAT\\Case cities data\\"

#Select causes (health outcomes) to be extracted
causes <- read.csv((paste0(result_folder, "Causes to be extracted.csv")))
causes <- unlist(causes[,2])

data_read <- read.csv(paste0(result_folder, "GBD2017_countries_extracted.csv"))

#Keep only causes defined in "Causes to be extracted.csv" file
data_read <- subset(data_read, cause_id %in% causes)

#Remove rates from the data
data_read <- subset(data_read, metric_name %in% "Number")

#Change the names of age groups
data_read$age_name <- gsub('Under 5', '0 to 4', data_read$age_name)
data_read$age_name <- gsub('95 plus', '95 to 99', data_read$age_name)

#Read age group data that defines what age groups are needed
age_groups <- read.csv((paste0(result_folder, "Age_groups.csv")))
age_groups <- unlist(age_groups["age_name"])

data_read <- subset(data_read, age_name %in% age_groups)

#########
#Read and select population data
popdata_read <- read.csv(paste0(result_folder, "GBD2017_population_extracted.csv"))
# Keep only year 2017 population estimates
popdata_read <- subset(popdata_read, year_id %in% 2017)
popdata_read <- subset(popdata_read, sex_name %in% c("Male", "Female"))
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
popdata_read$sex_name <- factor(popdata_read$sex_name)

############
#Combine burden and population data for one dataframe
#This works but gives error if burden data wont include all the countries.
#(due to differences in factors)

join_data <- left_join (data_read, popdata_read, by=c("age_name", "sex_name", "location_name"))
rm(popdata_read, data_read)

#Defines measures that are extracted
measures <- read.csv((paste0(result_folder, "Measure_extracted.csv")))
measures <- unlist(measures[,1])

#Columns to be extracted for further analyses, and measures needed
col_extracted <- c("measure_name.x", "location_name", "sex_name", "age_name", "cause_name", "val", "population")
join_data <- subset(join_data, measure_name.x %in% measures, select = col_extracted)

############
#Save each country as separate csv-file
#First read the names of the countries
cname <- read.csv((paste0(result_folder, "Countries to be extracted.csv")))
cname <- cname$Country_or_location

for (i in 1:length(cname)) {
    a <- subset(join_data, location_name %in% cname[i])
    write.csv(a, file =paste0(country_results,cname[i], "_GBD_results.csv"), row.names=FALSE)
    rm(a)
}

#In the end remove all
rm(join_data, age_groups, causes, cname, col_extracted, country_results, i, measures, result_folder)




