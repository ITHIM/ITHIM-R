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

ithim_object <- list()
## These what-if scenarios are hard coded. It isn't very generalisable.
ithim_object$what_if$now              <- list(cleaner_fleet=1.0,safety=1.0,chronic_disease=1.0,background_ap=1.0,background_pa=1.0)
ithim_object$what_if$cleaner_fleet    <- list(cleaner_fleet=0.5,safety=1.0,chronic_disease=1.0,background_ap=1.0,background_pa=1.0)
ithim_object$what_if$safety           <- list(cleaner_fleet=1.0,safety=0.5,chronic_disease=1.0,background_ap=1.0,background_pa=1.0)
ithim_object$what_if$chronic_disease  <- list(cleaner_fleet=1.0,safety=1.0,chronic_disease=2.0,background_ap=1.0,background_pa=1.0)
ithim_object$what_if$background_ap    <- list(cleaner_fleet=1.0,safety=1.0,chronic_disease=1.0,background_ap=0.5,background_pa=1.0)
ithim_object$what_if$background_pa    <- list(cleaner_fleet=1.0,safety=1.0,chronic_disease=1.0,background_ap=1.0,background_pa=0.5)

##
ithim_setup_global_values()
ithim_object$parameters <- ithim_setup_parameters()
##

ithim_load_data()
rd <- ithim_setup_baseline_scenario()
ithim_object$bs <- create_all_scenarios(rd)
set_scenario_specific_variables(ithim_object$bs)
# distances for injuries calculation
ithim_object$inj_distances <- distances_for_injury_function(ithim_object$bs)

## 
ithim_object$outcome <- run_ithim(ithim_object,seed=1)
result_mat <- do.call(rbind,ithim_object$outcome)
##

columns <- ncol(result_mat)
nDiseases <- (columns-1)/NSCEN
for(i in 1:nDiseases){
  x11(width=8,height=5); par(mfrow=c(2,3))
  ylim <- range(result_mat[,1:NSCEN+(i-1)*NSCEN])
  for(j in 1:nrow(result_mat)){
    if(j<4) {
      par(mar=c(1,4,4,1))
      barplot(result_mat[j,1:NSCEN+(i-1)*NSCEN],names.arg='',ylim=ylim,las=2,
              main=paste0(rownames(result_mat)[j],', ',last(strsplit(colnames(result_mat)[i*NSCEN],'_')[[1]])))
    }else{
      par(mar=c(5,4,4,1))
      barplot(result_mat[j,1:NSCEN+(i-1)*NSCEN],names.arg=SCEN_SHORT_NAME[c(1,3:6)],ylim=ylim,las=2,
              main=paste0(rownames(result_mat)[j],', ',last(strsplit(colnames(result_mat)[i*NSCEN],'_')[[1]])))
    }
  }
}

#################################################
## Use case 2: sampling:

##
ithim_setup_global_values(SAMPLEMODE = T, NSAMPLES = 16)
ithim_object$parameters <- ithim_setup_parameters(PM_CONC_BASE = c(log(50),1), PM_TRANS_SHARE = c(5,5)) 
##

ithim_load_data()
rd <- ithim_setup_baseline_scenario()
ithim_object$bs <- create_all_scenarios(rd)
set_scenario_specific_variables(ithim_object$bs)
# distances for injuries calculation
ithim_object$inj_distances <- distances_for_injury_function(ithim_object$bs)

## to get EVPPI
numcores <- detectCores()
results <- mclapply(1:NSAMPLES,FUN=run_ithim,ithim_object=ithim_object,mc.cores = numcores)
parameter_samples <- t(sapply(results,function(x)x$parameter_samples))
outcome <- t(sapply(results,function(x)x$now))
evppi <- matrix(0,ncol=NSCEN,nrow=length(ithim_object$parameters))
for(j in 1:(NSCEN)+5){
  y <- outcome[,j]
  vary <- var(y)
  for(i in 1:length(ithim_object$parameters)){
    x <- parameter_samples[,i];
    model <- gam(y~s(x)); 
    evppi[i,j-5] <- (vary-mean((y-model$fitted)^2))/vary*100;
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
