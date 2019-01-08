# Created by Javad Rahimipour Anaraki on 22/10/18
# Ph.D. Candidate
# Department of Computer Science
# Memorial University of Newfoundland
# jra066 [AT] mun [DOT] ca | www.cs.mun.ca/~jra066

#   input: Trip data
#  output: Generate and add PA data

rm(list = ls())
#========================Libraries=========================
list.of.packages <-
  c("stringr",
    "data.table",
    "dplyr")
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)

library(stringr)
library(data.table)
library(dplyr)

#=========================Variables========================
OS <- Sys.info()
if (OS["sysname"] == "Windows") {
  path <-
    paste0("C:/Users/",
           OS["login"],
           "/Dropbox/ITHIM/RPackage/Ithim_r/data/local/montreal/data/")
} else {
  path <-
    paste0("/Users/",
           OS["login"],
           "/Dropbox/ITHIM/RPackage/Ithim_r/data/local/montreal/data/")
}
setwd(path)

#Timezone
timeZone <- "America/St_Johns"
Sys.setenv(TZ = timeZone)

#Data files to be processed
dataFiles <- dir(path = path, pattern = "trips_*")

#Read in average PA
#https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310033901&pickMembers%5B0%5D=3.1&pickMembers%5B1%5D=5.1
#Download the data and rename it to averagePA.csv
averagePA <- fread(paste0(path, "averagePA.csv"),
                   sep = ",",
                   data.table = FALSE)

#Filter out anything other than Estimates and Mean for Characteristics and Statistics, respectively
averagePA <- filter(averagePA, Statistics == "Mean" & Characteristics == "Estimate")

#Aggregate average PA
aggAveragePA <-
  aggregate(
    averagePA[, "VALUE"],
    by = list(Measures = averagePA[, "Measures"], Sex = averagePA[, "Sex"], AgeGroup = averagePA[, "Age group"]),
    FUN = mean
    )

#Remove NAs
aggAveragePA <- na.omit(aggAveragePA)

#Filter out anything other than Males and Females for Sex column
aggAveragePA <- filter(aggAveragePA, Sex == "Males" | Sex == "Females")

#Find age range and type
ageRange <- t(as.data.frame(str_extract_all(pattern = "([0-9])+", aggAveragePA[, "AgeGroup"])))
paType <- t(as.data.frame(str_extract_all(pattern = "(Light)|(Moderate)", aggAveragePA[, "Measures"])))

#Add, remove and rename column names
aggAveragePA[, "ageFrom"] <- ageRange[, 1]
aggAveragePA[, "ageTo"] <- ageRange[, 2]
aggAveragePA[, "AgeGroup"] <- NULL
aggAveragePA[, "Measures"] <- paType
colnames(aggAveragePA)[3] <- "mean"
aggAveragePA <- aggAveragePA[, c("Sex", "ageFrom", "ageTo", "Measures", "mean")]


#====================Read in data files====================
for (i in 1:length(dataFiles)) {
  data <-
    fread(dataFiles[i],
          sep = ",",
          data.table = FALSE)
  
  pa_Montreal <- cbind(c(1:nrow(data)), data[, c("sex", "age")])
  
  test <- filter(aggAveragePA, Sex == pa_Montreal[, "sex"] & pa_Montreal[, "age"] > ageFrom & pa_Montreal[, "age"] <= ageTo & Measures == "Moderate")
  pa_Montreal[, "work_mpa_duration"] <- dlnorm(x, meanlog = 0, sdlog = 1, log = FALSE)
  
  
  pa_Montreal[, "travel_min_week"] <- data[, "travel_min_week"]
  
  

  #Select required columns
  cols <-
    c("CD_GENRE_ACCDN", colnames(data)[grep(pattern = "(nb_)+", colnames(data))])
  tmpData <- data[, cols]
  
  #Calculate the number of vehicle and pedestrian involved
  tmpData[, "nb_SumAll"] <-
    rowSums(tmpData[, 2:ncol(tmpData)], dims = 1)
  
  #Merging filtered data with collision dictionary
  mrgData <-
    merge(x = tmpData,
          y = dictionary,
          by = "CD_GENRE_ACCDN",
          all.x = TRUE)
  
  #================Loop over wear locations==================
  for (i in 1:nrow(wearLocation)) {
    wear_loc <- wearLocation[i, "Location"]
    devID <- wearLocation[i, "ID"]
    
    #Reading accelerometer data
    acc <-
      read.csv(
        file = paste(path, uid, "/", devID, ".csv", sep = ""),
        header = TRUE,
        sep = ","
      )
    
    #Selecting required columns
    acc <- acc[, c("record_time", "x_axis", "y_axis", "z_axis")]
    
    #Rounding up the time to minutes
    acc[, "record_time"] <- substr(acc[, "record_time"], 0, 19)
    
    #Cut data based on start and end dates
    acc <-
      acc[(format.Date(acc[, "record_time"]) >= format.Date(startDate)),]
    acc <-
      acc[(format.Date(acc[, "record_time"]) <= format.Date(endDate)),]
    
    #Aggregating mData by record time and averaging of all values
    acc <-
      aggregate(
        acc[, c("x_axis", "y_axis", "z_axis")],
        by = list(record_time = acc$record_time),
        FUN = mean,
        na.rm = FALSE
      )
    
    #Adding corresponding labels to mData
    #To include all responses to the final aggregation dataset, change this to all=TRUE
    lData <- merge(acc, res, by = "record_time", all = FALSE)
    
    #Save the results as a CSV file
    #dateTime <- toString(format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))
    fileName <- paste(devID, "_labeled.csv", sep = "")
    write.csv(lData, paste(path, uid, "/", fileName, sep = ""), row.names = FALSE)
  }
}
