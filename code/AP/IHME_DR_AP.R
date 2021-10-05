#' ---
#' title: "Organizing IHME-GBD Dose-Response functions"
#' author: "Daniel"
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#' ---
#' 
# 
#' Each file has 1000 samples, mean and upper and lower bounds for a specific dose.
#' We only need the mean, upper and lower bounds. So in this script, this is done.

#+ warning=FALSE, message=FALSE, echo=FALSE
# Loading libraries
library(tidyverse)

#+ warning=FALSE, message=FALSE, echo=FALSE
# Cleaning workspace
rm(list = ls());gc()

# Printing options
options(scipen = 50)

# Listing of files
list_of_files <- list.files(path = "data/global/dose_response/drap/GBD 2019/IHME_GBD_2019_PM_RISK_DRAWS/",
                            recursive = TRUE, pattern = "\\.CSV$",
                             full.names = TRUE)

# Filter only the ones we're interested in
causes <- c("CVD_IHD", "CVD_STROKE", "LRI", "NEO_LUNG", "RESP_COPD", "T2_DM")
index <- c()
for (i in 1:length(causes)) {
  index <- c(index, grep(causes[i], list_of_files))
}

list_of_files <- list_of_files[index]

# Organizing datasets
for (i in 1:length(list_of_files)) {
  temp_df <- read_csv(list_of_files[i])
  cause <- unique(temp_df$cause)
  temp_df <- temp_df %>%dplyr::select(exposure_spline, mean, lower, upper) %>% 
    rename(dose = exposure_spline, RR = mean,	lb = lower,	ub = upper)
  write_csv(temp_df, paste0("inst/extdata/global/dose_response/drap/extdata/",
                            cause,".csv"))
}
