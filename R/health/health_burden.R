# Clear workspace
rm (list = ls())

# load libraries
require(tidyverse)
require(drpa)
library(readr)
library(dplyr)
## disease outcome lookup table for PA and AP
disease_outcomes_lookup <- read_csv("~/GitHub/ITHIM-R/data/dose_response/disease_outcomes_lookup.csv")
str(disease_outcomes_lookup)

## cvd_ihd and cvd_stroke are age dependent, therefore we need to map the age of individuals with the age in the dose-response file of AP 
dr_ap<-read.csv("~/GitHub/ITHIM-R/data/dose_response/AP/dose_response_AP.csv")

## for every individual average of all parameter draws within the age and disease-outcome

x<-NA
x_scen1<- NA
x_scen2<- NA
x_scen3<- NA
for ( j in 1: nrow(disease_outcomes_lookup))
{

  
for ( i in 1: nrow(ind) )
{
  if (disease_outcomes_lookup$air_pollution==1)  ## checking whethet to calculate this health outcome for air pollution
  { 
    ## for cvd_ihd and cvd_stroke- dose-response is age sensitive
  
    dr_ap_sub<- dr_ap[which(dr_ap$age== ind$ap_age[i] & dr_ap$cause_code==disease_outcomes_lookup$ap_acronym),]
    
  }
for ( k in 1: nrow(dr_ap_sub)) ## this will calculate the RR for every individual and then average for all the parameter draws
{
  
  x_base<- sum(x, 1 + (rr_disease[i,2] * (1-exp(-rr_disease[k,3]*((individual_data_base$base_pm_conc-dr_ap_sub[k,5])^rr_disease[i,4])))),na.rm=T)
  x_scen1
  x_scen2
  x_scen3
}
  ind$RR_ap_base [i] <- x_base/nrow(dr_ap_sub)  
  ind$RR_ap_scen1[i]<- x_scen1/nrow(dr_ap_sub)
  ind$RR_ap_scen2[i]<- x_scen2/nrow(dr_ap_sub)
  ind$RR_ap_scen3[i]<- x_scen3/nrow(dr_ap_sub)
}


## change the names of the columns as per the disease
col<- which(names(ind)== "RR_ap_base")
names(ind)[col]<- paste("RR_ap_base_", disease_lookup_table$ap_acronym[j], sep="")
col<- which(names(ind)== "RR_ap_scen1")
names(ind)[col]<- paste("RR_ap_scen1_", disease_lookup_table$ap_acronym[j], sep="")
col<- which(names(ind)== "RR_ap_scen2")
names(ind)[col]<- paste("RR_ap_scen2_", disease_lookup_table$ap_acronym[j], sep="")
col<- which(names(ind)== "RR_ap_scen3")
names(ind)[col]<- paste("RR_ap_scen3_", disease_lookup_table$ap_acronym[j], sep="")

}
} 
}
# source
source("R/PA/code/functions.R")

### combining PM2.5 concentration data (scenario_pm_calculations.R) and PA data (total_mmet.R) at the individual level (n=732)
ind<- read_csv("~/GitHub/ITHIM-R/data/synth_pop_data/accra/processed_data/indiv_mmet/pa_total_mmet_weekly.csv") ### PA 
ind_pm<- read_csv("~/GitHub/ITHIM-R/data/synth_pop_data/accra/pollution/individual_level_pm_conc_scenarios.csv")  ### PM2.5
ind_pm<- ind_pm[,-1]
ind<- ind %>% left_join (ind_pm, by="participant_id")
## assigning air pollution age band to the individual_level data
ind$ap_age<- NA
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
  ind$ap_age[which(ind$age > 69 & ind$age < 55 )]<- 70
  ind$ap_age[which(ind$age > 74 & ind$age < 55 )]<- 75
  ind$ap_age[which(ind$age > 79 & ind$age < 55 )]<- 80
  ind$ap_age[which(ind$age > 84 & ind$age < 55 )]<- 85
  ind$ap_age[which(ind$age > 89 & ind$age < 55 )]<- 90
  ind$ap_age[which(ind$age > 94 )]<- 95
  
}


library(readr)
ap_rr_pa_total_mmet_weekly <- read_csv("~/GitHub/ITHIM-R/data/synth_pop_data/accra/processed_data/indiv_mmet/ap_rr_pa_total_mmet_weekly.csv")

# Read scenario data
rd <- read_csv("data/scenarios/accra/baseline_and_three_scenarios.csv")

# Create dummy ind pop
ind <- rd %>% group_by(participant_id) %>% summarise(sex = first(sex),
                                                     age_cat = first(age_cat))

# Read disease lt
disease_lt <- read_csv("data/dose_response/disease_outcomes_lookup.csv")

disease_lt[is.na(disease_lt)] <- 0

for (i in 1:nrow(disease_lt)){
  
  for (scen in c('base', 'scen1', 'scen2', 'scen3')){
    # i <- 1
    if (disease_lt$air_pollution[i] == 1 & disease_lt$physical_activity[i] ){
      
      ind[[paste0(scen, '_rr_ap_pa_', disease_lt$acronym[i])]] <- runif(nrow(ind), min = 0, max = 1)
      
    }
    else if (disease_lt$air_pollution[i] == 1 & disease_lt$physical_activity[i] != 1){
      
      ind[[paste0(scen, '_rr_ap_', disease_lt$acronym[i])]] <- runif(nrow(ind), min = 0, max = 1)
      
    } else if (disease_lt$air_pollution[i] != 1 & disease_lt$physical_activity[i] == 1){
      
      ind[[paste0(scen, '_rr_pa_', disease_lt$acronym[i])]] <- runif(nrow(ind), min = 0, max = 1)
    }
    
  }
}

pif <- data.frame(PAF(pop = ind, attr = c('sex', 'age_cat'), cn = c('base_rr_ap_pa_cvd', 'scen1_rr_ap_pa_cvd')))
pif <- arrange(pif, age.band, gender)


# Redefine non-factor based column classes
pif[,c("age.band", "gender")] <- lapply(pif[,c("age.band", "gender")], as.character)
pif[,c("base_rr_ap_pa_cvd", "scen1_rr_ap_pa_cvd")] <- lapply(pif[,c("base_rr_ap_pa_cvd", "scen1_rr_ap_pa_cvd")], as.double)


# Read gbd data
gbd_data <- read_csv("data/demographics/gbd/accra/GBD Accra.csv")

yll_dfs <- combine_health_and_pif(
  pop = pif,
  hc = gbd_data,
  hm = "YLLs (Years of Life Lost)",
  cn = c("base_rr_ap_pa_cvd", "scen1_rr_ap_pa_cvd"),
  hm_cause <- "All causes",
  hm_cn <- 'val_accra')


# Subset to get yll
yll <- as.data.frame(yll_dfs[1])
# Subset to get yll_reductions
yll_red <- as.data.frame(yll_dfs[2])


death_dfs <- combine_health_and_pif(
  pop = pif,
  hc = gbd_data,
  hm = "Deaths",
  cn = c("base_rr_ap_pa_cvd", "scen1_rr_ap_pa_cvd"),
  hm_cause <- "All causes",
  hm_cn <- 'val_accra')


# Subset to get yll
deaths <- as.data.frame(death_dfs[1])
# Subset to get yll_reductions
deaths_red <- as.data.frame(death_dfs[2])
