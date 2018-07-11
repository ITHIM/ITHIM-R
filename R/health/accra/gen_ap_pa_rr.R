# Clear workspace
rm (list = ls())

# load libraries
require(tidyverse)
require(drpa)
library(readr)
library(dplyr)

# source
source("R/PA/code/functions.R")

# ap_rr_pa_total_mmet_weekly <- read.csv("data/synth_pop_data/accra/processed_data/indiv_mmet/ap_rr_pa_total_mmet_weekly.csv")

# Read scenario data
rd <- read.csv("data/scenarios/accra/baseline_and_three_scenarios.csv")

# Create dummy ind pop
ind <- rd %>% group_by(participant_id) %>% summarise(sex = first(sex),
                                                     age = first(age),
                                                     age_cat = first(age_cat))
str(ind)
## disease outcome lookup table for PA and AP
disease_outcomes_lookup <- read.csv("data/dose_response/disease_outcomes_lookup.csv")
str(disease_outcomes_lookup)

## cvd_ihd and cvd_stroke are age dependent, therefore we need to map the age of individuals with the age in the dose-response file of AP 
dr_ap <- read.csv("data/dose_response/AP/dose_response_AP.csv")

### combining PM2.5 concentration data (scenario_pm_calculations.R) and PA data (total_mmet.R) at the individual level (n=732)
#ind<- read.csv("data/synth_pop_data/accra/processed_data/indiv_mmet/pa_total_mmet_weekly.csv") ### PA 
ind_pm <- read.csv("data/synth_pop_data/accra/pollution/individual_level_pm_conc_scenarios.csv")  ### PM2.5
ind <- ind %>% left_join(ind_pm, by = "participant_id")
## assigning air pollution age band to the individual_level data
ind$ap_age <- NA
for ( i in 1: nrow(ind))
{
  ind$ap_age[which(ind$age > 24 & ind$age < 30 )]<- 25
  ind$ap_age[which(ind$age > 29 & ind$age < 35 )]<- 30
  ind$ap_age[which(ind$age > 34 & ind$age < 40 )]<- 35
  ind$ap_age[which(ind$age > 39 & ind$age < 45 )]<- 40
  ind$ap_age[which(ind$age > 44 & ind$age < 50 )]<- 45
  ind$ap_age[which(ind$age > 49 & ind$age < 55 )]<- 50
  ind$ap_age[which(ind$age > 54 & ind$age < 60 )]<- 55
  ind$ap_age[which(ind$age > 59 & ind$age < 65 )]<- 60
  ind$ap_age[which(ind$age > 64 & ind$age < 70 )]<- 65
  ind$ap_age[which(ind$age > 69 & ind$age < 75 )]<- 70
  ind$ap_age[which(ind$age > 74 & ind$age < 80 )]<- 75
  ind$ap_age[which(ind$age > 79 & ind$age < 85 )]<- 80
  ind$ap_age[which(ind$age > 84 & ind$age < 90 )]<- 85
  ind$ap_age[which(ind$age > 89 & ind$age < 95 )]<- 90
  ind$ap_age[which(ind$age > 94 )]<- 95
  
}

ind$ap_age[is.na(ind$ap_age)] <- 0
## for every individual average of all parameter draws within the age and disease-outcome


for ( j in 1: nrow(disease_outcomes_lookup))  ### iterating over all all disease outcomes
{
  if (disease_outcomes_lookup$air_pollution[j] == 1)  ## checking whether to calculate this health outcome for air pollution
  { 
    
    for ( i in 1: nrow(ind) )  ## iterating over all individuals
    {
      
      ## for cvd_ihd and cvd_stroke- dose-response is age sensitive
      if (disease_outcomes_lookup$ap_acronym[j] == "cvd_ihd" | disease_outcomes_lookup$ap_acronym[j] == "cvd_stroke"  )
      {
        dr_ap_sub<- dr_ap[which(dr_ap$age_code == ind$ap_age[i] & dr_ap$cause_code==as.character(disease_outcomes_lookup$ap_acronym[j])),]
      }
      else
      {
        dr_ap_sub<- dr_ap[which(dr_ap$age_code== 99 & dr_ap$cause_code==as.character(disease_outcomes_lookup$ap_acronym[j])),] 
        
      }
      
      x<-NA
      x_scen1<- NA
      x_scen2<- NA
      x_scen3<- NA  
      for (k in 1: nrow(dr_ap_sub)) ## iterating over all the rows of the parameter draws of dose-response paramters
      {
        # browser()
        x<- sum(x, 1 + (dr_ap_sub[k,2] * (1-exp(-dr_ap_sub[k,3]*((ind$pm_conc_base[i] - dr_ap_sub[k,5])^dr_ap_sub[k,4])))),na.rm=T) 
        x_scen1<- sum(x_scen1, 1 + (dr_ap_sub[k,2] * (1-exp(-dr_ap_sub[k,3]*((ind$pm_conc_scen1[i]-dr_ap_sub[k,5])^dr_ap_sub[k,4])))),na.rm=T)
        x_scen2<- sum(x_scen2, 1 + (dr_ap_sub[k,2] * (1-exp(-dr_ap_sub[k,3]*((ind$pm_conc_scen2[i]-dr_ap_sub[k,5])^dr_ap_sub[k,4])))),na.rm=T)
        x_scen3<- sum(x_scen3, 1 + (dr_ap_sub[k,2] * (1-exp(-dr_ap_sub[k,3]*((ind$pm_conc_scen3[i]-dr_ap_sub[k,5])^dr_ap_sub[k,4])))),na.rm=T)
      }
      ind$RR_ap_base [i] <- x/nrow(dr_ap_sub)  
      ind$RR_ap_scen1[i]<- x_scen1/nrow(dr_ap_sub)
      ind$RR_ap_scen2[i]<- x_scen2/nrow(dr_ap_sub)
      ind$RR_ap_scen3[i]<- x_scen3/nrow(dr_ap_sub)
    }
    
    
    ## change the names of the columns as per the disease
    col<- which(names(ind)== "RR_ap_base")
    names(ind)[col]<- paste("RR_ap_base_", disease_outcomes_lookup$acronym[j], sep="")
    col<- which(names(ind)== "RR_ap_scen1")
    names(ind)[col]<- paste("RR_ap_scen1_", disease_outcomes_lookup$acronym[j], sep="")
    col<- which(names(ind)== "RR_ap_scen2")
    names(ind)[col]<- paste("RR_ap_scen2_", disease_outcomes_lookup$acronym[j], sep="")
    col<- which(names(ind)== "RR_ap_scen3")
    names(ind)[col]<- paste("RR_ap_scen3_", disease_outcomes_lookup$acronym[j], sep="")
    
  }
  
}

# Replace write.csv with write_csv
write_csv(ind, 'data/synth_pop_data/accra/combined_ap_pa_rr/RR_AP_calculations.csv')