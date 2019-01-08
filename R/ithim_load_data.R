#' @export
ithim_load_data <- function(){
  ## this function requires path specification, so that it may differ for different case studies
  ## these datasets are all global, saved in global folder.
  global_path <- file.path(find.package('ithimr'), 'inst/extdata/global/')#'~/overflow_dropbox/ITHIM-R/data/global/'
  ## DATA FILES FOR MODEL  
  DISEASE_INVENTORY <<- read.csv(paste0(global_path,"dose_response/disease_outcomes_lookup.csv"))
  # DR_AP$cause_code matches DISEASE_INVENTORY$ap_acronym
  DR_AP <<- read.csv(paste0(global_path,"dose_response/drap/dose_response.csv"))
  #INJ_DIST_EXP <<- read_csv('code/injuries/data/sin_coefficients_pairs.csv') ## injury distance exponent
  # root of list_of_files matches DISEASE_INVENTORY$pa_acronym
  list_of_files <- list.files(path = paste0(global_path,"dose_response/drpa/extdata/"), recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)
  for (i in 1:length(list_of_files)){
    assign(stringr::str_sub(basename(list_of_files[[i]]), end = -5),
           readr::read_csv(list_of_files[[i]]),
           pos = 1)
  }
  ##!! Emission factors should depend on the regulatory standards of the setting at the time. This file applies to Accra, Delhi. Would not apply to current HI settings.
  EMISSION_FACTORS <<- readRDS(paste0(global_path,"emissions/emission_factors.Rds"))
  
  ## these datasets are all local, saved in local folder.
  local_path <- file.path(find.package('ithimr'), 'inst/extdata/local/',CITY,'/')
  ## DATA FILES FOR ACCRA
  # GBD file needs to have the following columns: 
  # age (=label, e.g. 15-49)
  # sex (=Male or Female)
  # measure
  # cause (GBD_DATA$cause matches DISEASE_INVENTORY$GBD_name)
  # metric
  # burden
  # min_age (=number, e.g. 15)
  # max_age (=number, e.g. 49)
  GBD_DATA <<- read_csv(paste0(local_path,'gbd_accra.csv'))
  gbd_injuries <- GBD_DATA[which(GBD_DATA$cause == "Road injuries"),]
  gbd_injuries$sex_age <- paste0(gbd_injuries$sex,"_",gbd_injuries$age)
  ## calculating the ratio of YLL to deaths for each age and sex group
  gbd_injuries <- arrange(gbd_injuries, measure)
  gbd_inj_yll <- gbd_injuries[which(gbd_injuries$measure == "YLLs (Years of Life Lost)"),]
  gbd_inj_dth <- gbd_injuries[which(gbd_injuries$measure == "Deaths"),]
  gbd_inj_yll$yll_dth_ratio <- gbd_inj_yll$burden/gbd_inj_dth$burden 
  GBD_INJ_YLL <<- gbd_inj_yll
  
  # get age-category details from GBD data
  AGE_CATEGORY <<- unique(GBD_DATA$age)
  AGE_LOWER_BOUNDS <<- sort(unique(GBD_DATA$min_age))
  MAX_AGE <<- max(GBD_DATA$max_age)
  
  trip_set <- read_csv(paste0(local_path,"trips_accra.csv"))
  trip_set$participant_id <- as.numeric(as.factor(trip_set$participant_id))
  TRIP_SET <<- trip_set
  PA_SET <<- read_csv(paste0(local_path,"pa_accra.csv"))
  WHW_MAT <<- read_csv(paste0(local_path,"who_hit_who_accra.csv"))
  injuries <- readRDS(paste0(local_path,"injuries_long_accra.Rds"))
  injuries <- assign_age_groups(injuries,age_label='cas_age')
  set_injury_contingency(injuries)
  ## DESCRIPTION OF INJURIES
  # has one row per event (fatality)
  # has colnames event_id, year, cas_mode, strike_mode, cas_age, cas_gender
  # classes are character for 'factors' and numeric for age and year
  # levels for cas_mode must match those modes used throughout, defined in TRAVEL_MODES. E.g. for Accra we re-label 'mini' as 'Bus'
  # levels for strike_mode that match TRAVEL_MODES will be used in a distance-based regression
  # levels for strike_mode that aren't in TRAVEL_MODES will be used in a distance-independent regression
  # levels in cas_gender must match the sex/gender levels provided elsewhere e.g. in TRIP_SET
  # colnames year, cas_mode, strike_mode, cas_age, cas_gender are used to form a contingency table
  # cas_mode, strike_mode, cas_age, cas_gender are used in the regression model
  # in future, we can add other covariates
  
  ##RJ suggestion to AA
  ## that our folder structure consists of two respositories for data: ITHIM-R/data/global and ITHIM-R/data/local
  ## in 'global', we have
  ##   Dose--response data (data/drpa/extdata/ and dose_response_AP.csv)
  ##   "disease dependencies" (disease_outcomes_lookup.csv)
  ##   Injury distance exponents (code/injuries/data/sin_coefficients_pairs.csv)
  ##   GBD (data/demographics/gbd/accra/GBD Accra.csv)
  ##   Emission factors ('data/emission calculations accra/emission_factors.Rds')
  ## these data are loaded automatically with library(ITHIMR), so we don't need to code them up here at all.
  ## in 'local', we have
  ##   Accra, which contains
  ##       Trip-level survey ("data/synth_pop_data/accra/raw_data/trips/trips_Accra.csv")
  ##       Physical activity data ("data/synth_pop_data/accra/raw_data/PA/pa_Accra.csv")
  ##       WHW matrix (code/injuries/accra/who_hit_who_accra.csv)
  ## these files are loaded when ITHIM-R is run. 
  ## The user specifies either 'accra', if we have the folder 'accra', or the path to a repository containing named files, or a path per file...
}
