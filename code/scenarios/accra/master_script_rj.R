setwd('~/overflow_dropbox/ITHIM-R/')
rm (list = ls())
source('ithim_r_functions.R')
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

#################################################
## Use case 1: not sampling:

##
ithim_setup_global_values()
parameters <- ithim_setup_parameters()
##

ithim_load_data()
rd <- ithim_setup_baseline_scenario()
INDEX <- 1
bs <- create_all_scenarios(rd)
set_scenario_specific_variables(bs)

## 
run_ithim(seed=1)
##

#################################################
## Use case 2: sampling:

##
ithim_setup_global_values(SAMPLEMODE = T, NSAMPLES = 16)
parameters <- ithim_setup_parameters(PM_CONC_BASE = c(log(50),1), PM_TRANS_SHARE = c(5,5)) 
##

ithim_load_data()
rd <- ithim_setup_baseline_scenario()
INDEX <- 1
bs <- create_all_scenarios(rd)
set_scenario_specific_variables(bs)

## to get EVPPI
numcores <- detectCores()
results <- mclapply(1:NSAMPLES,FUN=run_ithim,mc.cores = numcores)
parameter_samples <- t(sapply(results,function(x)x$parameter_samples))
outcome <- t(sapply(results,function(x)x$outcome))
evppi <- matrix(0,ncol=NSCEN,nrow=length(parameters))
for(j in 1:(NSCEN)){
  y <- outcome[,j]
  vary <- var(y)
  for(i in 1:length(parameters)){
    x <- parameter_samples[,i];
    model <- gam(y~s(x)); 
    evppi[i,j] <- (vary-mean((y-model$fitted)^2))/vary*100;
  }
}
print(evppi)
##

###################################################

#rm(list = setdiff(ls(), c("INDEX", "RSEED", "PA_MULT", "bs", "dist", "dur", "mmets", "pm_conc", 
#                          "RR_AP_calculations", "RR_PA_calculations",
#                          "RR_PA_AP_calculations", "deaths_by_mode",
#                          "deaths_yll_injuries", "MEAN_BUS_WALK_TIME",
#                          "deaths", "deaths_red", "ylls", "ylls_red",
#                          "pa_certainty"))) 

# # COPY THE SAME OBJECTS FROM 1:5, TO 6:10
# 
# for (INDEX in 2:6){
#   bs[[INDEX]] <- bs[[INDEX - 1]]
#   dist[[INDEX]] <- dist[[INDEX - 1]]
#   dur[[INDEX]] <- dur[[INDEX - 1]]
#   mmets[[INDEX]] <- mmets[[INDEX - 1]]
#   pm_conc[[INDEX]] <- pm_conc[[INDEX - 1]]
#   RR_AP_calculations[[INDEX]] <- RR_AP_calculations[[INDEX - 1]]
#   RR_PA_calculations[[INDEX]] <- RR_PA_calculations[[INDEX - 1]]
#   RR_PA_AP_calculations[[INDEX]] <- RR_PA_AP_calculations[[INDEX - 1]]
#   
# }
# 
# for (INDEX in 2:6){
#   
#   # INDEX <- 6
#   # Calculate RR PA
#   source("code/health/accra/gen_pa_rr.R")
#   
#   # Combine RR for PA and AP for common diseases
#   source("code/health/accra/combined_rr_pa_pa.R")
#   
#   # # Calculate disease burden for injuries
#   source("code/injuries/accra/accra_injuries.R")
#   # 
#   # # Calculate disease burden for AP, PA and Injuries
#   source("code/health/accra/health_burden.R")
#   
# }
# 
# 
# rm(list = setdiff(ls(), c("INDEX", "RSEED",  "bs", "dist", "dur", "mmets", "pm_conc", 
#                           "RR_AP_calculations", "RR_PA_calculations",
#                           "RR_PA_AP_calculations", "deaths_by_mode",
#                           "deaths_yll_injuries", "MEAN_BUS_WALK_TIME",
#                           "deaths", "deaths_red", "ylls", "ylls_red",
#                           "pa_certainty"))) 
