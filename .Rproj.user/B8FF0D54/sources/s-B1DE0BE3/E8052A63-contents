library(drpa)
library(tidyverse)
# Load all functions
source("R/PA/code/functions.R")

# Read processed data for ap and pa
ind <- read_csv("data/synth_pop_data/accra/processed_data/indiv_mmet/ap_rr_pa_mmet_weekly.csv")
ind2 <- read_csv("data/synth_pop_data/accra/processed_data/scenarios/baseline_and_three_scenarios.csv")

# Redefine age categories to match with GBD's
age_category <- c("15-49", "50-69", "70+")
ind$age_cat[ind$age >= 15 & ind$age < 50] <- age_category[1]
ind$age_cat[ind$age >= 50 & ind$age < 70] <- age_category[2]
ind$age_cat[ind$age >= 70] <- age_category[3]

# pif <- data.frame(PAF(pop = ind, attr = c('sex', 'age_cat'), cn = c('RR_pm_base', 'RR_pm_scen1', 'RR_pm_scen2', 'RR_pm_scen3')))
# pif <- arrange(pif, age.band, gender)
# 
# pif <- rename(pif, pif_AP_scen1 = RR_pm_scen1, pif_AP_scen2 = RR_pm_scen2, pif_AP_scen3 = RR_pm_scen3)
# pif$RR_pm_base <- NULL
str()
### List of diseases
##1) Cardiovascular disease: common to PA and AP
##3) Colon cancer: Only PA
##4) Lung cancer: common to PA and AP
##5) Chronic obstructive pulmonary disease (COPD): 

#AP diseases from dose-response function and GBD names
#cvd_ihd = IHD [B2.2 or B2.3.1]
#cvd_stroke = Stroke [B2.3 or B2.3.1]
#neo_lung = Tracheal, bronchus, and lung cancer [B.1.11]
#resp_copd = [B3.1]Chronic respiratory diseases is best guess in your excel. However, IHME have COPD as own category under the Chronic respiratory diseases category.
#IRI = lower respiratory infection [A.2.3]

## calculating RR for AP and PA (these should happen simulatenously)
rr_disease<- read.csv('pm_relative_risk.csv')
for (i in 1: 3)
{
  individual_data_base$RR_base<-  1 + (rr_disease[i,2] * (1-exp(-rr_disease[i,3]*((individual_data_base$base_pm_conc-rr_disease[i,5])^rr_disease[i,4]))))
  names(individual_data_base)[which( colnames(individual_data_base)=="RR_base" )]<- paste('RR_pm_',rr_disease[i,1],'_base',sep="")
  
  individual_data_base$RR_scen1<-  1 + (rr_disease[i,2] * (1-exp(-rr_disease[i,3]*((individual_data_base$scen1_conc-rr_disease[i,5])^rr_disease[i,4]))))
  names(individual_data_base)[which( colnames(individual_data_base)=="RR_scen1" )]<- paste('RR_pm_',rr_disease[i,1],'_scen1',sep="")
  
  individual_data_base$RR_scen2<-  1 + (rr_disease[i,2] * (1-exp(-rr_disease[i,3]*((individual_data_base$scen2_conc-rr_disease[i,5])^rr_disease[i,4]))))
  names(individual_data_base)[which( colnames(individual_data_base)=="RR_scen2" )]<- paste('RR_pm_',rr_disease[i,1],'_scen2',sep="")
  
  individual_data_base$RR_scen3<- 1 + (rr_disease[i,2] * (1-exp(-rr_disease[i,3]*((individual_data_base$scen3_conc-rr_disease[i,5])^rr_disease[i,4]))))
  names(individual_data_base)[which( colnames(individual_data_base)=="RR_scen3" )]<- paste('RR_pm_',rr_disease[i,1],'_scen3',sep="")
}

## add a loop to go through all disease outcomes


for (i in 1:nrow(ind)){
  ind$base_pa_all_cause_rr[i] <- dose_response(cause = 'all-cause-mortality', outcome_type = 'mortality', 
                                               dose = ifelse(ind$base_MET[i] <= 35, ind$base_MET[i], 35), 
                                               use_75_pert = F)$rr %>% as.numeric()
  ind$scen1_pa_all_cause_rr[i] <- dose_response(cause = 'all-cause-mortality', outcome_type = 'mortality', 
                                                dose = ifelse(ind$scen1_MET[i] <= 35, ind$scen1_MET, 35), 
                                                use_75_pert = F)$rr %>% as.numeric()
  
}

ind$base_pa_all_cause_rr[is.na(ind$base_pa_all_cause_rr)] <- 0
ind$scen1_pa_all_cause_rr[is.na(ind$scen1_pa_all_cause_rr)] <- 0

ind$base_pa_ap_combined_rr <- ind$RR_pm_base * ind$base_pa_all_cause_rr
ind$scen1_pa_ap_combined_rr <- ind$RR_pm_scen1 * ind$scen1_pa_all_cause_rr

pif <- data.frame(PAF(pop = ind, attr = c('sex', 'age_cat'), cn = c('base_pa_ap_combined_rr', 'scen1_pa_ap_combined_rr')))
pif <- arrange(pif, age.band, gender)


# Redefine non-factor based column classes
pif[,c("age.band", "gender")] <- lapply(pif[,c("age.band", "gender")], as.character)
pif[,c("base_pa_ap_combined_rr", "scen1_pa_ap_combined_rr")] <- lapply(pif[,c("base_pa_ap_combined_rr", "scen1_pa_ap_combined_rr")], as.double)


# Read gbd data
gbd_data <- read_csv("data/demographics/gbd/accra/GBD Accra.csv")

yll_dfs <- combine_health_and_pif(
  pop = pif,
  hc = gbd_data,
  hm = "YLLs (Years of Life Lost)",
  cn = c("base_pa_ap_combined_rr", "scen1_pa_ap_combined_rr"),
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
  cn = c("base_pa_ap_combined_rr", "scen1_pa_ap_combined_rr"),
  hm_cause <- "All causes",
  hm_cn <- 'val_accra')


# Subset to get yll
deaths <- as.data.frame(death_dfs[1])
# Subset to get yll_reductions
deaths_red <- as.data.frame(death_dfs[2])