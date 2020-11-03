#' Load data for model and setting
#' 
#' Loads and processes data from file. Local data for the setting and global data for the model.
#' Writes objects to the global environment.
#' 
#' @param setup_call_summary_filename name of file to write to
#' @param speeds named list of mode speeds
#' 
#' 
#' @export
ithim_load_data <- function(setup_call_summary_filename,speeds=list(
  bus=15,
  bus_driver=15,
  minibus=15,
  minibus_driver=15,
  car=21,
  taxi=21,
  pedestrian=4.8,
  walk_to_pt=4.8,
  cycle=14.5,
  motorcycle=25,
  truck=21,
  van=15,
  subway=28,
  rail=35,
  auto_rickshaw=22,
  shared_auto=22,
  cycle_rickshaw=10
)){
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
  cat(paste0('\n  Dose--response read from ',global_path,'dose_response/drpa/extdata/ \n\n'),file=setup_call_summary_filename,append=T)
  
  ## these datasets are all local, saved in local folder.
  local_path <- PATH_TO_LOCAL_DATA
  
  ## DATA FILES FOR CITY
  ## edit trip set.
  ## we need columns: trip_id, trip_mode, stage_mode, stage_duration, trip_distance, stage_distance
  ## trips can be composed of multiple stages
  ## all trip columns are used for scenario generation alone
  ## stage columns are used for downstream calculation
  ## if either trip or stage labels are missing, we copy over from the other.
  filename <- paste0(local_path,"/trips_",CITY,".csv")
  trip_set <- read_csv(filename,col_types = cols())

  # Disable temporary hack   
  # ### TEMPORARY HACK TO REMOVE ALL DIST COLUMNS
  # if ('trip_distance' %in% colnames(trip_set))
  #   trip_set <- trip_set %>% dplyr::select(-c('trip_distance'))
  # if ('stage_distance' %in% colnames(trip_set))
  #   trip_set <- trip_set %>% dplyr::select(-c('stage_distance'))
  
  cat(paste0('\n  Trips read from ',filename,' \n\n'),file=setup_call_summary_filename,append=T)
  trip_set$participant_id <- as.numeric(as.factor(trip_set$participant_id))
  ## copy over as required
  mode_cols <- c('trip_mode','stage_mode')
  if(sum(mode_cols%in%colnames(trip_set))==0) stop(paste0('Please include a column labelled "trip_mode" or "stage_mode" in ', filename))
  if('trip_mode'%in%colnames(trip_set)&&!'stage_mode'%in%colnames(trip_set)) 
    trip_set$stage_mode <- trip_set$trip_mode
  if('stage_mode'%in%colnames(trip_set)&&!'trip_mode'%in%colnames(trip_set)) 
    trip_set$trip_mode <- trip_set$stage_mode
  if('trip_duration'%in%colnames(trip_set)&&!'stage_duration'%in%colnames(trip_set)) 
    trip_set$stage_duration <- trip_set$trip_duration
  if('trip_distance'%in%colnames(trip_set)&&!'stage_distance'%in%colnames(trip_set)) 
    trip_set$stage_distance <- trip_set$trip_distance
  if('stage_distance'%in%colnames(trip_set)&&!'trip_distance'%in%colnames(trip_set)) 
    trip_set$trip_distance <- trip_set$stage_distance
  ## use specified words for key modes
  walk_words <- c('walk','walked','pedestrian')
  cycle_words <- c('bike','cycle','cycling')
  mc_words <- c('motorcycle','mcycle','mc','mtw')
  subway_words <- c('metro','underground')
  rail_words <- c('train')
  for(i in 1:length(mode_cols)){
    ## lower case mode names
    trip_set[[mode_cols[i]]] <- tolower(trip_set[[mode_cols[i]]])
    ## replaces spaces with _
    trip_set[[mode_cols[i]]] <- sapply(trip_set[[mode_cols[i]]],function(x)gsub(' ','_',as.character(x)))
    trip_set[[mode_cols[i]]][trip_set[[mode_cols[i]]]=='private_car'] <- 'car'
    trip_set[[mode_cols[i]]][trip_set[[mode_cols[i]]]%in%walk_words] <- 'pedestrian'
    trip_set[[mode_cols[i]]][trip_set[[mode_cols[i]]]%in%cycle_words] <- 'cycle'
    trip_set[[mode_cols[i]]][trip_set[[mode_cols[i]]]%in%mc_words] <- 'motorcycle'
    trip_set[[mode_cols[i]]][trip_set[[mode_cols[i]]]%in%subway_words] <- 'subway'
    trip_set[[mode_cols[i]]][trip_set[[mode_cols[i]]]%in%rail_words] <- 'rail'
  }
  trip_set <- subset(trip_set,!is.na(age))
  trip_set <- subset(trip_set,!is.na(sex))
  trip_set$sex <- tolower(trip_set$sex)
  trip_set$trip_id[is.na(trip_set$stage_mode)] <- 0
  TRIP_SET <<- trip_set
  
  if(MAX_MODE_SHARE_SCENARIO&&
     (!exists('SCENARIO_PROPORTIONS')||
      exists('SCENARIO_PROPORTIONS')&&!isTRUE(base::all.equal(DIST_CAT,colnames(SCENARIO_PROPORTIONS)))
     )){
    SCENARIO_PROPORTIONS <<- get_scenario_settings(distances=DIST_CAT,speeds=speeds)
  }
  
  # GBD file needs to have the following columns: 
  # age (=label, e.g. 15-49)
  # sex (=male or female)
  # measure
  # cause (GBD_DATA$cause matches DISEASE_INVENTORY$GBD_name)
  # metric
  # burden
  # min_age (=number, e.g. 15)
  # max_age (=number, e.g. 49)
  filename <- paste0(local_path,"/gbd_",CITY,".csv")
  GBD_DATA <- read_csv(filename,col_types = cols())
  cat(paste0('\n  GBD read from ',filename,' \n\n'),file=setup_call_summary_filename,append=T)
  filename <- paste0(local_path,"/population_",CITY,".csv")
  demographic <- read_csv(filename,col_types = cols())
  demographic <- demographic[!apply(demographic,1,anyNA),]
  demographic$sex <- tolower(demographic$sex)
  cat(paste0('\n  Population read from ',filename,' \n\n'),file=setup_call_summary_filename,append=T)
  age_category <- demographic$age
  max_age <- max(as.numeric(sapply(age_category,function(x)strsplit(x,'-')[[1]][2])))
  max_age <- min(max_age,max(trip_set$age),AGE_RANGE[2])
  min_age <- min(as.numeric(sapply(age_category,function(x)strsplit(x,'-')[[1]][1])))
  min_age <- max(min_age,min(trip_set$age),AGE_RANGE[1])
  DEMOGRAPHIC <<- demographic[as.numeric(sapply(age_category,function(x)strsplit(x,'-')[[1]][1]))<=max_age&
                                as.numeric(sapply(age_category,function(x)strsplit(x,'-')[[1]][2]))>=min_age,]
  
  # get age-category details from population data
  AGE_CATEGORY <<- unique(DEMOGRAPHIC$age)
  AGE_LOWER_BOUNDS <<- as.numeric(sapply(AGE_CATEGORY,function(x)strsplit(x,'-')[[1]][1]))
  MAX_AGE <<- max(as.numeric(sapply(AGE_CATEGORY,function(x)strsplit(x,'-')[[1]][2])))
  
  disease_names <- c(as.character(DISEASE_INVENTORY$GBD_name),'Road injuries')
  GBD_DATA <- subset(GBD_DATA,cause_name%in%disease_names)
  GBD_DATA$min_age <- as.numeric(sapply(GBD_DATA$age_name,function(x)str_split(x,' to ')[[1]][1]))
  GBD_DATA$max_age <- as.numeric(sapply(GBD_DATA$age_name,function(x)str_split(x,' to ')[[1]][2]))
  GBD_DATA <- subset(GBD_DATA,max_age>=AGE_LOWER_BOUNDS[1])
  GBD_DATA <- subset(GBD_DATA,min_age<=MAX_AGE)
  names(GBD_DATA)[c(1,3,4,5)] <- c('measure','sex','age','cause')
  GBD_DATA$sex <- tolower(GBD_DATA$sex)
  
  burden_of_disease <- expand.grid(measure=unique(GBD_DATA$measure),sex=unique(DEMOGRAPHIC$sex),age=unique(DEMOGRAPHIC$age),
                                   cause=disease_names,stringsAsFactors = F)
  burden_of_disease <- dplyr::left_join(burden_of_disease,DEMOGRAPHIC,by=c('age','sex'))
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
  DISEASE_BURDEN <<- burden_of_disease
  
  gbd_injuries <- DISEASE_BURDEN[which(DISEASE_BURDEN$cause == "Road injuries"),]
  gbd_injuries$sex_age <- paste0(gbd_injuries$sex,"_",gbd_injuries$age)
  ## calculating the ratio of YLL to deaths for each age and sex group
  gbd_injuries <- arrange(gbd_injuries, measure)
  gbd_inj_yll <- gbd_injuries[which(gbd_injuries$measure == "YLLs (Years of Life Lost)"),]
  gbd_inj_dth <- gbd_injuries[which(gbd_injuries$measure == "Deaths"),]
  gbd_inj_yll$yll_dth_ratio <- gbd_inj_yll$burden/gbd_inj_dth$burden 
  GBD_INJ_YLL <<- gbd_inj_yll
    
  ## pa data
  filename <- paste0(local_path,"/pa_",CITY,".csv")
  pa_set <- read_csv(filename,col_types = cols())
  pa_set$sex <- tolower(pa_set$sex)
  PA_SET <<- pa_set
  cat(paste0('\n  Physical activity survey read from ',filename,' \n\n'),file=setup_call_summary_filename,append=T)
  
  ## injury data
  filename <- paste0(local_path,"/injuries_",CITY,".csv")
  injuries <- read_csv(filename,col_types = cols())
  cat(paste0('\n  Injuries read from ',filename,' \n\n'),file=setup_call_summary_filename,append=T)
  if('cas_age'%in%colnames(injuries)) injuries <- assign_age_groups(injuries,age_label='cas_age')
  injuries$cas_mode <- tolower(injuries$cas_mode)
  injuries$strike_mode <- tolower(injuries$strike_mode)
  if('cas_gender'%in%colnames(injuries)) injuries$cas_gender <- tolower(injuries$cas_gender)
  #injuries$strike_mode[is.na(injuries$strike_mode)] <- 'listed_na'
  nov_words <- c('no.other.fixed.or.stationary.object','no other vehicle','none')
  injuries$strike_mode[injuries$strike_mode%in%nov_words] <- 'nov'
  ## add weight column if missing
  if(!'weight'%in%colnames(injuries)) injuries$weight <- 1
  
  ## AA - Hard-coded
  ## INJURIES - Make all incidents of car, bus, motorcycle and cycle with themselves, as NOV
  ## 25-02-2020
  
  # Get all injuries with same casualty and strike mode for car, bus, motorcycle and cycle
  # Treat bus_driver same as bus for strike mode
  same_cas_str_modes <- injuries %>% filter((cas_mode == 'car' & strike_mode == 'car') | 
                                              (cas_mode == 'bus' & (strike_mode %in% c('bus', 'bus_driver'))) | 
                                              (cas_mode == 'motorcycle' & strike_mode == 'motorcycle') | 
                                              (cas_mode == 'cycle' & strike_mode == 'cycle'))
  
  # Filter all those with similar casualty and strike mode
  injuries <- injuries %>% filter(!( (cas_mode == 'car' & strike_mode == 'car') | 
                                       (cas_mode == 'bus' & (strike_mode %in% c('bus', 'bus_driver'))) | 
                                       (cas_mode == 'motorcycle' & strike_mode == 'motorcycle') |
                                       (cas_mode == 'cycle' & strike_mode == 'cycle')))
  
  # Mutate strike mode as NOV
  same_cas_str_modes <- same_cas_str_modes %>% mutate(strike_mode = 'nov')
  
  # Re-add with NOV
  injuries <- plyr::rbind.fill(injuries, same_cas_str_modes)
  
  # Call function to set tables for WHW and NOV
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

