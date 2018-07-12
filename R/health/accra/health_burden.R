source("R/scenarios/accra/setup.R")

# Example code for health burden calculations for RR_pa_ap_base_lc and RR_pa_ap_base_lc

source("R/PA/code/functions.R")

ind <- read_csv("data/synth_pop_data/accra/RR/RR_PA_AP_calculations.csv")

pif <- data.frame(PAF(pop = ind, attr = c('sex', 'age_cat'), cn = c('RR_pa_ap_base_lc', 'RR_pa_ap_scen1_lc')))
pif <- arrange(pif, age.band, gender)


# Redefine non-factor based column classes
pif[,c("age.band", "gender")] <- lapply(pif[,c("age.band", "gender")], as.character)
pif[,c("RR_pa_ap_base_lc", "RR_pa_ap_scen1_lc")] <- lapply(pif[,c("RR_pa_ap_base_lc", "RR_pa_ap_scen1_lc")], as.double)


# Read gbd data
gbd_data <- read.csv("data/demographics/gbd/accra/GBD Accra.csv")

yll_dfs <- combine_health_and_pif(
  pop = pif,
  hc = gbd_data,
  hm = "YLLs (Years of Life Lost)",
  cn = c("RR_pa_ap_base_lc", "RR_pa_ap_scen1_lc"),
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
  cn = c("RR_pa_ap_base_lc", "RR_pa_ap_scen1_lc"),
  hm_cause <- "All causes",
  hm_cn <- 'val_accra')


# Subset to get yll
deaths <- as.data.frame(death_dfs[1])
# Subset to get yll_reductions
deaths_red <- as.data.frame(death_dfs[2])
