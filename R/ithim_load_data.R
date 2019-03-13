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
  GBD_DATA <- read_csv(filename,col_types = cols())
  filename <- paste0(local_path,"/population_",CITY,".csv")
  DEMOGRAPHIC <<- read_csv(filename,col_types = cols())
  
  # get age-category details from population data
  AGE_CATEGORY <<- unique(DEMOGRAPHIC$age)
  AGE_LOWER_BOUNDS <<- as.numeric(sapply(AGE_CATEGORY,function(x)strsplit(x,'-')[[1]][1]))
  MAX_AGE <<- max(as.numeric(sapply(AGE_CATEGORY,function(x)strsplit(x,'-')[[1]][2])))
  
  disease_names <- c(as.character(DISEASE_INVENTORY$GBD_name),'Road injuries')
  GBD_DATA <- subset(GBD_DATA,cause_name%in%disease_names)
  GBD_DATA$min_age <- as.numeric(sapply(GBD_DATA$age_name,function(x)str_split(x,' to ')[[1]][1]))
  GBD_DATA$max_age <- as.numeric(sapply(GBD_DATA$age_name,function(x)str_split(x,' to ')[[1]][2]))
  GBD_DATA <- subset(GBD_DATA,min_age>=AGE_LOWER_BOUNDS[1])
  GBD_DATA <- subset(GBD_DATA,max_age<=MAX_AGE)
  names(GBD_DATA)[c(1,3,4,5)] <- c('measure','sex','age','cause')
  
  burden_of_disease <- expand.grid(measure=unique(GBD_DATA$measure),sex=unique(DEMOGRAPHIC$sex),age=unique(DEMOGRAPHIC$age),
                                   cause=disease_names,stringsAsFactors = F)
  burden_of_disease <- left_join(burden_of_disease,DEMOGRAPHIC,by=c('age','sex'))
  burden_of_disease$min_age <- as.numeric(sapply(burden_of_disease$age,function(x)str_split(x,'-')[[1]][1]))
  burden_of_disease$max_age <- as.numeric(sapply(burden_of_disease$age,function(x)str_split(x,'-')[[1]][2]))
  ## when we sum ages, we assume that all age boundaries used coincide with the GBD age boundaries.
  burden_of_disease$rate <- apply(burden_of_disease,1,
                                  function(x){
                                    subtab <- subset(GBD_DATA,measure==as.character(x[1])&sex==as.character(x[2])&cause==as.character(x[4])&
                                                       min_age>=as.numeric(x[6])&max_age<=as.numeric(x[7])); 
                                    sum(subtab$val)/sum(subtab$population)
                                    }
                                  )
  
  burden_of_disease$burden <- burden_of_disease$population*burden_of_disease$rate
  burden_of_disease$burden[is.na(burden_of_disease$burden)] <- 0
  
  ## scale disease burden from country to city using populations
  #burden_of_disease <- left_join(GBD_DATA[,!colnames(GBD_DATA)=='population'],DEMOGRAPHIC,by=c('age','sex'))
  #burden_of_disease$burden <- GBD_DATA$burden*burden_of_disease$population/GBD_DATA$population
  DISEASE_BURDEN <<- burden_of_disease
  
  gbd_injuries <- DISEASE_BURDEN[which(DISEASE_BURDEN$cause == "Road injuries"),]
  gbd_injuries$sex_age <- paste0(gbd_injuries$sex,"_",gbd_injuries$age)
  ## calculating the ratio of YLL to deaths for each age and sex group
  gbd_injuries <- arrange(gbd_injuries, measure)
  gbd_inj_yll <- gbd_injuries[which(gbd_injuries$measure == "YLLs (Years of Life Lost)"),]
  gbd_inj_dth <- gbd_injuries[which(gbd_injuries$measure == "Deaths"),]
  gbd_inj_yll$yll_dth_ratio <- gbd_inj_yll$burden/gbd_inj_dth$burden 
  GBD_INJ_YLL <<- gbd_inj_yll
  
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
