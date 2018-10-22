run_ithim_setup <- function(plotFlag = F,
                            NSAMPLES = 1,
                            modes = c("Bus", "Private Car", "Taxi", "Walking","Short Walking", "Bicycle", "Motorcycle"),
                            speeds = c(15, 21, 21, 4.8, 4.8, 14.5, 25),
                            DIST_CAT = c("0-6 km", "7-9 km", "10+ km"),
                            AGE_CATEGORY = c("15-49", "50-69", "70+"),
                            MEAN_BUS_WALK_TIME= 5,
                            MMET_CYCLING = 4.63,
                            MMET_WALKING = 2.53,
                            PM_CONC_BASE = 50,  
                            PM_TRANS_SHARE = 0.225,
                            PA_DOSE_RESPONSE_QUANTILE = F,
                            AP_DOSE_RESPONSE_QUANTILE = F,
                            BACKGROUND_PA_SCALAR = 1,
                            SAFETY_SCALAR = 1,
                            CHRONIC_DISEASE_SCALAR = 1 ){
  # Load packages
  library(tidyverse)
  library(haven)
  library(plotly)
  library(ReIns)
  library(dplyr)
  library(tidyverse)
  library(truncnorm)
  library(distr)
  library(pracma)
  library(data.table)
  library(mgcv)
  library(parallel)
  library(splines)
  
  #################################################
  
  ithim_object <- list()
  
  #ithim_setup_global_values(plotFlag,NSAMPLES,modes,speeds,DIST_CAT,AGE_CATEGORY)
  ## SET GLOBAL VALUES
  ## PROGRAMMING VARIABLES
  plotFlag <<- plotFlag
  NSAMPLES <<- NSAMPLES
  
  ## MODEL VARIABLES
  MODE_SPEEDS <<- data.frame(trip_mode = modes, speed = speeds, stringsAsFactors = F)
  DIST_CAT <<- DIST_CAT
  DIST_LOWER_BOUNDS <<- as.numeric(sapply(strsplit(DIST_CAT, "[^0-9]+"), function(x) x[1]))
  AGE_CATEGORY <<- AGE_CATEGORY
  AGE_LOWER_BOUNDS <<- as.numeric(sapply(strsplit(AGE_CATEGORY, "[^0-9]+"), function(x) x[1]))
  
  ithim_load_data()
  
  ithim_object$parameters <- ithim_setup_parameters(NSAMPLES,
                                                    MEAN_BUS_WALK_TIME,
                                                    MMET_CYCLING,
                                                    MMET_WALKING,
                                                    PM_CONC_BASE,  
                                                    PM_TRANS_SHARE,
                                                    PA_DOSE_RESPONSE_QUANTILE,
                                                    AP_DOSE_RESPONSE_QUANTILE,
                                                    BACKGROUND_PA_SCALAR,
                                                    SAFETY_SCALAR,
                                                    CHRONIC_DISEASE_SCALAR )
  ##RJ these distance calculations are currently not parameter dependent, which means we can make the calculation outside the function.
  ## We could either integrate them into another external function, or move them to the internal function, so that they can become variable.
  if(!'MEAN_BUS_WALK_TIME'%in%names(ithim_object$parameters)){
    rd <- ithim_setup_baseline_scenario()
    ithim_object$bs <- create_all_scenarios(rd)
    set_scenario_specific_variables(ithim_object$bs)
    # Generate distance and duration matrices
    dist_and_dir <- dist_dur_tbls(ithim_object$bs)
    ithim_object$dist <- dist_and_dir$dist
    ithim_object$dur <- dist_and_dir$dur
    # distances for injuries calculation
    ithim_object$inj_distances <- distances_for_injury_function(ithim_object$bs)
  }
  ######################
  return(ithim_object)
}
