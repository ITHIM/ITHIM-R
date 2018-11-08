ithim_load_data <- function(){
  ## DATA FILES FOR MODEL  
  DR_AP <<- read.csv("data/dose_response/AP/dose_response_AP.csv")
  DISEASE_OUTCOMES <<- read.csv("data/dose_response/disease_outcomes_lookup.csv")
  ##!! RJ question for AA/RG: this line creates a warning:
  ## Missing column names filled in: 'X1' [1] 
  ## Can it be fixed?
  S.I.N <<- suppressWarnings(read_csv('code/injuries/data/sin_coefficients_pairs.csv'))
  list_of_files <- list.files(path = "data/drpa/extdata/", recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)
  for (i in 1:length(list_of_files)){
    assign(stringr::str_sub(basename(list_of_files[[i]]), end = -5),
           read_csv(list_of_files[[i]]),
           pos = 1)
  }
  
  ## DATA FILES FOR ACCRA
  RD <<- read_csv("data/synth_pop_data/accra/travel_survey/synthetic_population_with_trips.csv")
  trans_emissions_file <- read_csv("data/emission calculations accra/transport_emission_inventory_accra.csv")
  names(trans_emissions_file) <- c("vehicle_type", "delhi_fleet_2011", "delhi_fleet_perHH", "accra_fleet_2010", "PM2_5_emiss_fact", "base")
  TRANS_EMISSIONS_ORIGINAL <<- trans_emissions_file
  lookup_ratio_pm_file <-  read_csv('data/synth_pop_data/accra/pollution/pm_exposure_ratio_look_up.csv')
  lookup_ratio_pm_file <- dplyr::rename(lookup_ratio_pm_file, trip_mode = Mode)
  LOOKUP_RATIO_PM <<- lookup_ratio_pm_file
  ##!! RJ question for AA/RG: this line creates a warning:
  ## Missing column names filled in: 'X1' [1] 
  ## Can it be fixed?
  WHW_MAT <<- suppressWarnings(read_csv('code/injuries/accra/who_hit_who_accra.csv'))
  GBD_DATA <<- read_csv('data/demographics/gbd/accra/GBD Accra.csv')
  gbd_injuries <- GBD_DATA[which(GBD_DATA$cause == "Road injuries"),]
  gbd_injuries$sex_age <- paste0(gbd_injuries$sex,"_",gbd_injuries$age)
  ## calculating the ratio of YLL to deaths for each age and sex group
  gbd_injuries <- arrange(gbd_injuries, measure)
  gbd_inj_yll <- gbd_injuries[which(gbd_injuries$measure == "YLLs (Years of Life Lost)"),]
  gbd_inj_dth <- gbd_injuries[which(gbd_injuries$measure == "Deaths"),]
  gbd_inj_yll$yll_dth_ratio <- gbd_inj_yll$value_gama/gbd_inj_dth$value_gama 
  GBD_INJ_YLL <<- gbd_inj_yll
  
  ##RJ suggestion to AA
  ## that our folder structure consists of two respositories for data: ITHIM-R/data/global and ITHIM-R/data/local
  ## in 'global', we have
  ##   Dose--response data (data/drpa/extdata/ and dose_response_AP.csv)
  ##   "disease dependencies" (disease_outcomes_lookup.csv)
  ##   Injury distance exponents (code/injuries/data/sin_coefficients_pairs.csv)
  ##   ventilation ratios (data/synth_pop_data/accra/pollution/pm_exposure_ratio_look_up.csv)
  ##   GBD (data/demographics/gbd/accra/GBD Accra.csv)
  ## these data are loaded automatically with library(ITHIMR), so we don't need to code them up here at all.
  ## in 'local', we have
  ##   accra, which contains
  ##       RD (data/synth_pop_data/accra/travel_survey/synthetic_population_with_trips.csv)
  ##       emissions (data/emission calculations accra/transport_emission_inventory_accra.csv)
  ##       WHW matrix (code/injuries/accra/who_hit_who_accra.csv)
  ## these files are loaded when ITHIM-R is run. 
  ## The user specifies either 'accra', if we have the folder 'accra', or the path to a repository containing named files, or a path per file...
}
