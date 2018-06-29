library(drpa)
# Load all functions
source("R/PA/code/functions.R")

# Read processed data for ap and pa
ind <- read_csv("data/synth_pop_data/accra/processed_data/indiv_mmet/ap_rr_pa_mmet_weekly.csv")
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

for (i in 1:nrow(ind)){
  ind$base_pa_all_cause_rr[i] <- dose_response(cause = 'all-cause-mortality', outcome_type = 'mortality', dose = ind$base_MET[i])$rr %>% as.numeric()
  ind$scen1_pa_all_cause_rr[i] <- dose_response(cause = 'all-cause-mortality', outcome_type = 'mortality', dose = ind$scen1_MET[i])$rr %>% as.numeric()
  
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