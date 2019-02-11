#' @export
ithim_load_data <- function(){
  ## this function requires path specification, so that it may differ for different case studies
  
  ## these datasets are all global, saved in global folder.
  global_path <- file.path(find.package('ithimr',lib.loc=.libPaths()), 'extdata/global/')
  
  global_path <- paste0(global_path, "/")
  
  ## DATA FILES FOR MODEL  
  DISEASE_INVENTORY <<- read.csv(paste0(global_path,"dose_response/disease_outcomes_lookup.csv"))
  # DR_AP$cause_code matches DISEASE_INVENTORY$ap_acronym
  DR_AP <<- read.csv(paste0(global_path,"dose_response/drap/dose_response.csv"))
  #INJ_DIST_EXP <<- read_csv('code/injuries/data/sin_coefficients_pairs.csv') ## injury distance exponent
  # root of list_of_files matches DISEASE_INVENTORY$pa_acronym
  list_of_files <- list.files(path = paste0(global_path,"dose_response/drpa/extdata/"), recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)
  for (i in 1:length(list_of_files)){
    assign(stringr::str_sub(basename(list_of_files[[i]]), end = -5),
           readr::read_csv(list_of_files[[i]],col_types = cols()),
           pos = 1)
  }
  ##!! Emission factors should depend on the regulatory standards of the setting at the time. This file applies to Accra, Delhi. Would not apply to current HI settings.
  EMISSION_FACTORS <<- readRDS(paste0(global_path,"emissions/emission_factors.Rds"))
  
  ## these datasets are all local, saved in local folder.
  local_path <- PATH_TO_LOCAL_DATA
  
  ## DATA FILES FOR CITY
  # GBD file needs to have the following columns: 
  # age (=label, e.g. 15-49)
  # sex (=Male or Female)
  # measure
  # cause (GBD_DATA$cause matches DISEASE_INVENTORY$GBD_name)
  # metric
  # burden
  # min_age (=number, e.g. 15)
  # max_age (=number, e.g. 49)
  filename <- paste0(local_path,"/gbd_",CITY,".csv")
  GBD_DATA <<- read_csv(filename,col_types = cols())
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
  
  filename <- paste0(local_path,"/trips_",CITY,".csv")
  trip_set <- read_csv(filename,col_types = cols())
  trip_set$participant_id <- as.numeric(as.factor(trip_set$participant_id))
  trip_set$trip_mode <- tolower(trip_set$trip_mode)
  trip_set$trip_mode[trip_set$trip_mode=='private car'] <- 'car'
  TRIP_SET <<- trip_set
  
  filename <- paste0(local_path,"/pa_",CITY,".csv")
  PA_SET <<- read_csv(filename,col_types = cols())
  
  ##!! only one injury file is need. 
  # WHW_MAT is the input into injuries_function.
  # set_injury_contingency(injuries) is the input into injuries_function_2.
  # both functions currently have a lot of hard-coded variables, e.g. the modes.
  # we are using injuries_function_2 for Accra.
  #filename <- paste0(local_path,"/who_hit_who_",CITY,".csv")
  #WHW_MAT <<- read_csv(filename)
  
  filename <- paste0(local_path,"/injuries_",CITY,".csv")
  injuries <- read_csv(filename,col_types = cols())
  injuries <- assign_age_groups(injuries,age_label='cas_age')
  injuries$cas_mode <- tolower(injuries$cas_mode)
  injuries$strike_mode <- tolower(injuries$strike_mode)
  injuries$strike_mode[is.na(injuries$strike_mode)] <- 'listed_na'
  set_injury_contingency(injuries)
  
  ## DESCRIPTION OF INJURIES (set_injury_contingency(injuries))
  # has one row per event (fatality)
  # has colnames event_id, year, cas_mode, strike_mode, cas_age, cas_gender
  # classes are character for 'factors' and numeric for age and year
  # levels for cas_mode must match those modes used throughout, defined in TRAVEL_MODES. E.g. for Accra we re-label 'mini' as 'bus'
  # levels for strike_mode that match TRAVEL_MODES will be used in a distance-based regression
  # levels for strike_mode that aren't in TRAVEL_MODES will be used in a distance-independent regression
  # levels in cas_gender must match the sex/gender levels provided elsewhere e.g. in TRIP_SET
  # colnames year, cas_mode, strike_mode, cas_age, cas_gender are used to form a contingency table
  # cas_mode, strike_mode, cas_age, cas_gender are used in the regression model
  # in future, we can add other covariates
  
}
