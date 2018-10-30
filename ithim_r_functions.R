##RJ question for LG/AA/RG
## How many 'age_cat's should there be? How should they be defined for the different parts of the model? 
## E.g., PA has different categories to everything else.

##RJ question for/discussion with AA. 
## Functions ithim_setup_global_values, ithim_setup_parameters, ithim_load_data, set_scenario_specific_variables
## Global variables (i.e. those needed by all functions) are assigned to all environments, using 
## <<- and assign(...,pos=1), and denoted by capitals to make it clear what they are. 
## A better method might be to make an object (list) that contains all inputs, variables, and intermediate objects.

run_ithim_setup <- function(NSAMPLES = 1,
                            CITY = 'Accra',
                            modes = c("Bus", "Private Car", "Taxi", "Walking","Short Walking", "Bicycle", "Motorcycle","Truck","Bus_driver"),
                            speeds = c(15, 21, 21, 4.8, 4.8, 14.5, 25, 21, 15),
                            DIST_CAT = c("0-6 km", "7-9 km", "10+ km"),
                            AGE_CATEGORY = c("15-49", "50-69", "70+"),
                            MAX_AGE=70,
                            #population=1600000,
                            #survey_coverage=1/365,
                            BUS_WALK_TIME= 5,
                            MMET_CYCLING = 4.63,
                            MMET_WALKING = 2.53,
                            PM_CONC_BASE = 50,  
                            PM_TRANS_SHARE = 0.225,
                            PA_DOSE_RESPONSE_QUANTILE = F,
                            AP_DOSE_RESPONSE_QUANTILE = F,
                            BACKGROUND_PA_SCALAR = 1,
                            SAFETY_SCALAR = 1,
                            CHRONIC_DISEASE_SCALAR = 1,
                            RATIO_4W1_TO_4W2 = 10/12,
                            TAXI_TO_CAR_RATIO = 0.04,
                            BUS_TO_CAR_RATIO = 0.12,
                            TRUCK_TO_CAR_RATIO = 0.09,
                            MC_TO_CAR_RATIO = 0.2,
                            LDT_TO_CAR_RATIO = 0.21,
                            OTHER_TO_CAR_RATIO = 0.01 ){
  # Load packages
  library(tidyverse)
  library(haven)
  library(plotly)
  library(ReIns)
  library(dplyr)
  library(tidyverse)
  library(truncnorm)
  library(distr)
  library(pracma)
  library(data.table)
  library(mgcv)
  library(parallel)
  library(splines)
  library(BMS)
  library(MASS)
  
  #################################################
  
  ithim_object <- list()
  
  ## SET GLOBAL VALUES
  ## PROGRAMMING VARIABLES
  NSAMPLES <<- NSAMPLES
  
  ## MODEL VARIABLES
  CITY <<- CITY
  TRAVEL_MODES <<- modes
  MODE_SPEEDS <<- data.frame(trip_mode = modes, speed = speeds, stringsAsFactors = F)
  DIST_CAT <<- DIST_CAT
  DIST_LOWER_BOUNDS <<- as.numeric(sapply(strsplit(DIST_CAT, "[^0-9]+"), function(x) x[1]))
  AGE_CATEGORY <<- AGE_CATEGORY
  AGE_LOWER_BOUNDS <<- as.numeric(sapply(strsplit(AGE_CATEGORY, "[^0-9]+"), function(x) x[1]))
  MAX_AGE <<- MAX_AGE
  
  BASE_LEVEL_INHALATION_RATE <<- 10
  CLOSED_WINDOW_PM_RATIO <<- 0.5
  CLOSED_WINDOW_RATIO <<- 0.5
  ROAD_RATIO_MAX <<- 3.216
  ROAD_RATIO_SLOPE <<- 0.379
  
  RATIO_4W1_TO_4W2 <<-  RATIO_4W1_TO_4W2
  TAXI_TO_CAR_RATIO  <<- TAXI_TO_CAR_RATIO
  BUS_TO_CAR_RATIO  <<- BUS_TO_CAR_RATIO
  TRUCK_TO_CAR_RATIO  <<- TRUCK_TO_CAR_RATIO
  LDT_TO_CAR_RATIO <<- LDT_TO_CAR_RATIO
  OTHER_TO_CAR_RATIO <<- OTHER_TO_CAR_RATIO
  
  ## LOAD DATA
  ithim_load_data()  
  
  ## SET PARAMETERS
  ithim_object$parameters <- ithim_setup_parameters(NSAMPLES,
                                                    BUS_WALK_TIME,
                                                    MMET_CYCLING,
                                                    MMET_WALKING,
                                                    PM_CONC_BASE,  
                                                    PM_TRANS_SHARE,
                                                    MC_TO_CAR_RATIO,
                                                    PA_DOSE_RESPONSE_QUANTILE,
                                                    AP_DOSE_RESPONSE_QUANTILE,
                                                    BACKGROUND_PA_SCALAR,
                                                    SAFETY_SCALAR,
                                                    CHRONIC_DISEASE_SCALAR )
  
  RECALCULATE_TRIPS <<- 'MC_TO_CAR_RATIO'%in%names(ithim_object$parameters)
  ## create inventory and edit trips, if they are not variable dependent
  if(!RECALCULATE_TRIPS){
    set_vehicle_inventory() # sets vehicle inventory
    get_synthetic_from_trips() # sets synthetic trips and synthetic population
  }
  
  RECALCULATE_DISTANCES <<- RECALCULATE_TRIPS||'BUS_WALK_TIME'%in%names(ithim_object$parameters)
  ## calculate distances, if distances are not variable dependent
  if(!RECALCULATE_DISTANCES){
    ithim_object <- get_all_distances(ithim_object) # uses synthetic trips to calculate distances
  }
  ######################
  return(ithim_object)
}

######################################################################
## SET-UP FUNCTIONS
ithim_setup_parameters <- function(NSAMPLES = 1,
                                   BUS_WALK_TIME= 5,
                                   MMET_CYCLING = 4.63,
                                   MMET_WALKING = 2.53,
                                   PM_CONC_BASE = 50,  
                                   PM_TRANS_SHARE = 0.225,
                                   MC_TO_CAR_RATIO = 0.2,
                                   PA_DOSE_RESPONSE_QUANTILE = F,
                                   AP_DOSE_RESPONSE_QUANTILE = F,
                                   BACKGROUND_PA_SCALAR = 1,
                                   SAFETY_SCALAR = 1,
                                   CHRONIC_DISEASE_SCALAR = 1 ){
  ## PARAMETERS
  ##RJ parameters are assigned to the environment and so are set for every function. They are over-written when sample_parameters is called.
  BUS_WALK_TIME <<- BUS_WALK_TIME
  MMET_CYCLING <<- MMET_CYCLING
  MMET_WALKING <<- MMET_WALKING
  PM_CONC_BASE <<- PM_CONC_BASE
  BACKGROUND_PA_SCALAR <<- BACKGROUND_PA_SCALAR
  PM_TRANS_SHARE <<- PM_TRANS_SHARE
  MC_TO_CAR_RATIO <<- MC_TO_CAR_RATIO
  SAFETY_SCALAR <<- SAFETY_SCALAR
  CHRONIC_DISEASE_SCALAR <<- CHRONIC_DISEASE_SCALAR
  PA_DOSE_RESPONSE_QUANTILE <<- PA_DOSE_RESPONSE_QUANTILE
  parameters <- list()
  if(length(BUS_WALK_TIME) > 1 )    parameters$BUS_WALK_TIME <- rlnorm(NSAMPLES,BUS_WALK_TIME[1], BUS_WALK_TIME[2])
  if(length(MMET_CYCLING) > 1 )     parameters$MMET_CYCLING <- rlnorm(NSAMPLES,MMET_CYCLING[1], MMET_CYCLING[2])
  if(length(MMET_WALKING) > 1 )     parameters$MMET_WALKING <- rlnorm(NSAMPLES,MMET_WALKING[1], MMET_WALKING[2])
  if(length(PM_CONC_BASE) > 1 )     parameters$PM_CONC_BASE <- rlnorm(NSAMPLES,PM_CONC_BASE[1],PM_CONC_BASE[2])
  if(length(PM_TRANS_SHARE) > 1 )   parameters$PM_TRANS_SHARE <- rbeta(NSAMPLES,PM_TRANS_SHARE[1],PM_TRANS_SHARE[2])
  if(length(MC_TO_CAR_RATIO) > 1 )  parameters$MC_TO_CAR_RATIO <- rlnorm(NSAMPLES,MC_TO_CAR_RATIO[1],MC_TO_CAR_RATIO[2])
  if(length(BACKGROUND_PA_SCALAR) > 1 )     parameters$BACKGROUND_PA_SCALAR <- rlnorm(NSAMPLES,BACKGROUND_PA_SCALAR[1],BACKGROUND_PA_SCALAR[2])
  if(length(SAFETY_SCALAR) > 1 )    parameters$SAFETY_SCALAR <- rlnorm(NSAMPLES,SAFETY_SCALAR[1],SAFETY_SCALAR[2])
  if(length(CHRONIC_DISEASE_SCALAR) > 1 )   parameters$CHRONIC_DISEASE_SCALAR <- rlnorm(NSAMPLES,CHRONIC_DISEASE_SCALAR[1],CHRONIC_DISEASE_SCALAR[2])
  if(PA_DOSE_RESPONSE_QUANTILE == T ) {
    pa_diseases <- subset(DISEASE_OUTCOMES,physical_activity==1)
    dr_pa_list <- list()
    for(disease in pa_diseases$pa_acronym)
      parameters[[paste0('PA_DOSE_RESPONSE_QUANTILE_',disease)]] <- runif(NSAMPLES,0,1)
  }
  if(AP_DOSE_RESPONSE_QUANTILE == F ) {
    AP_DOSE_RESPONSE_QUANTILE <<- AP_DOSE_RESPONSE_QUANTILE
    dr_ap_list <- list()
    for ( j in 1:nrow(DISEASE_OUTCOMES)) if (DISEASE_OUTCOMES$air_pollution[j] == 1){ 
      cause <- as.character(DISEASE_OUTCOMES$ap_acronym[j])
      dr_ap <- subset(DR_AP,cause_code==cause)
      dr_ap_list[[cause]] <- list()
      for(age in unique(dr_ap$age_code)){
        dr_ap_age <- subset(dr_ap,age_code==age)
        dr_ap_list[[cause]][[as.character(age)]] <- data.frame(alpha=mean(dr_ap_age$alpha),beta=mean(dr_ap_age$beta),gamma=mean(dr_ap_age$gamma),tmrel=mean(dr_ap_age$tmrel))
      }
    }
    DR_AP_LIST <<- dr_ap_list
  }else{
    ap_diseases <- subset(DISEASE_OUTCOMES,air_pollution==1)
    for(disease in ap_diseases$ap_acronym)
      for(letter in c('ALPHA_','BETA_','GAMMA_','TMREL_'))
        parameters[[paste0('AP_DOSE_RESPONSE_QUANTILE_',letter,disease)]] <- runif(NSAMPLES,0,1)
    dr_ap_list <- list()
    for(disease in ap_diseases$ap_acronym){ 
      dr_ap <- subset(DR_AP,cause_code==disease)
      dr_ap_list[[disease]] <- list()
      quant1 <- parameters[[paste0('AP_DOSE_RESPONSE_QUANTILE_GAMMA_',disease)]]
      quant2 <- parameters[[paste0('AP_DOSE_RESPONSE_QUANTILE_BETA_',disease)]]
      quant3 <- parameters[[paste0('AP_DOSE_RESPONSE_QUANTILE_ALPHA_',disease)]]
      quant4 <- parameters[[paste0('AP_DOSE_RESPONSE_QUANTILE_TMREL_',disease)]]
      for(age in unique(dr_ap$age_code)){
        dr_ap_age <- subset(dr_ap,age_code==age)
        #######################################
        ##RJ I recommend the following as a better approximation to the distribution but it is currently  v e r y  slow
        ## so I leave it here commented out until we want to develop it and/or use it
        lbeta <- log(dr_ap_age$beta)
        lgamma <- log(dr_ap_age$gamma)
        gamma_val <- quantile(density(lgamma),quant1)
        beta_val <- c()
        for(i in 1:NSAMPLES){
          den <- kde2d(lgamma,lbeta,n=c(1,100),h=0.2,lims=c(gamma_val[i],gamma_val[i],min(lbeta)-1,max(lbeta)+1))
          beta_val[i] <- approx(x=cumsum(den$z)/sum(den$z),y=den$y,xout=quant2[i])$y
        }
        mod <- gam(log(alpha)~te(log(gamma),log(beta)),data=dr_ap_age)
        pred_val <- predict(mod, newdata=data.frame(beta=exp(beta_val),gamma=exp(gamma_val)),se.fit=T)
        alpha_val <- qnorm(quant3,pred_val$fit,sqrt(mod$sig2))
        #######################################
        
        # generate a value for alpha
        #alpha_val <- quantile(log(dr_ap_age$alpha),parameters[[paste0('AP_DOSE_RESPONSE_QUANTILE_ALPHA_',disease)]])
        # generate a value for beta given alpha
        #mod <- gam(log(beta)~ns(log(alpha),df=8),data=dr_ap_age)
        #pred_val <- predict(mod, newdata=data.frame(alpha=exp(alpha_val)),se.fit=T)
        #beta_val <- qnorm(parameters[[paste0('AP_DOSE_RESPONSE_QUANTILE_BETA_',disease)]],pred_val$fit,sqrt(mod$sig2))
        # generate a value for gamma given beta and alpha
        #mod <- gam(log(gamma)~ns(log(beta),df=8)+ns(log(alpha),df=8),data=dr_ap_age)
        #pred_val <- predict(mod, newdata=data.frame(alpha=exp(alpha_val),beta=exp(beta_val)),se.fit=T)
        #gamma_val <- qnorm(parameters[[paste0('AP_DOSE_RESPONSE_QUANTILE_GAMMA_',disease)]],pred_val$fit,sqrt(mod$sig2))
        
        # generate a value for tmrel given alpha, beta and gamma
        mod <- gam(log(tmrel)~ns(log(gamma),df=8)+ns(log(beta),df=8)+ns(log(alpha),df=8),data=dr_ap_age)
        pred_val <- predict(mod, newdata=data.frame(alpha=exp(alpha_val),beta=exp(beta_val),gamma=exp(gamma_val)),se.fit=T)
        tmrel_val <- qnorm(parameters[[paste0('AP_DOSE_RESPONSE_QUANTILE_TMREL_',disease)]],pred_val$fit,sqrt(mod$sig2))
        dr_ap_list[[disease]][[as.character(age)]] <- data.frame(alpha=exp(alpha_val),beta=exp(beta_val),gamma=exp(gamma_val),tmrel=exp(tmrel_val))
      }
    }
    # turn list inside out, so it's indexed first by sample
    parameters$DR_AP_LIST <- lapply(1:NSAMPLES,function(x)lapply(dr_ap_list,function(y) lapply(y,function(z)z[x,])))
  }
  parameters
}

## this function requires path specification, so that it may differ for different case studies
ithim_load_data <- function(){
  ## DATA FILES FOR MODEL  
  DR_AP <<- read.csv("data/dose_response/AP/dose_response_AP.csv")
  DISEASE_OUTCOMES <<- read.csv("data/dose_response/disease_outcomes_lookup.csv")
  S.I.N <<- read_csv('code/injuries/data/sin_coefficients_pairs.csv')
  list_of_files <- list.files(path = "data/drpa/extdata/", recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)
  for (i in 1:length(list_of_files)){
    assign(stringr::str_sub(basename(list_of_files[[i]]), end = -5),
           read_csv(list_of_files[[i]]),
           pos = 1)
  }
  ##!! Emission factors should depend on the regulatory standards of the setting at the time. This file applies to Accra, Delhi. Would not apply to current HI settings.
  EMISSION_FACTORS <<- readRDS('data/emission calculations accra/emission_factors.Rds')
  
  ## DATA FILES FOR ACCRA
  trip_set <- read_csv("data/synth_pop_data/accra/raw_data/trips/trips_Accra.csv")
  trip_set$participant_id <- as.numeric(as.factor(trip_set$participant_id))
  TRIP_SET <<- trip_set
  PA_SET <<- read_csv("data/synth_pop_data/accra/raw_data/PA/pa_Accra.csv")
  ##!! This item should be replaced by the sum from travel in the synthetic population
  DISTANCE_FOR_EMISSIONS <<- readRDS('data/emission calculations accra/accra_distances_for_emissions.Rds')
  WHW_MAT <<- read_csv('code/injuries/accra/who_hit_who_accra.csv')
  INJURIES <<- readRDS('code/injuries/data/accra_injuries_long.Rds')
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

set_vehicle_inventory <- function(){
  ## the vehicle inventory lists the modes, their speeds, their emission factors, and the distance covered relative to cars.
  ## we use 'distance covered relative to cars' to impute distances for modes not included in the travel survey.
  ## these distances are either added to the synthetic trip set, which means they feature in the emission and injury calculations,
  ## or they are just added ad hoc to the emission calculation.
  ## For Accra, Bus_driver and Truck trips are added to Synthetic trips. LDT and Other are not, so are included in Emission calculation only.
  ## ratios are heuristic values taken from Delhi study. 
  ## They can become set variables, or random variables, but as present are constant as below. To make variable, move VEHICLE_INVENTORY definition to 'dist' calculation.
  ## N.B.: the mode list is the union of trip_modes and EMISSION_FACTORS. To omit an undesired mode, we'd need to set the distance ratio to 0.
  vehicle_inventory <- MODE_SPEEDS
  vehicle_inventory$emission_factor <- 0
  vehicle_inventory$distance_ratio_to_car <- 1
  vehicle_inventory$emission_factor[vehicle_inventory$trip_mode%in%c('Taxi')] <- EMISSION_FACTORS$PM2_5_emiss_fact[EMISSION_FACTORS$vehicle_type=='TAXI']
  vehicle_inventory$distance_ratio_to_car[vehicle_inventory$trip_mode%in%c('Taxi')] <- TAXI_TO_CAR_RATIO
  vehicle_inventory$emission_factor[vehicle_inventory$trip_mode%in%c('Private Car')] <- 
    RATIO_4W1_TO_4W2*EMISSION_FACTORS$PM2_5_emiss_fact[EMISSION_FACTORS$vehicle_type=='4W1']+(1-RATIO_4W1_TO_4W2)*EMISSION_FACTORS$PM2_5_emiss_fact[EMISSION_FACTORS$vehicle_type=='4W1']
  vehicle_inventory$emission_factor[vehicle_inventory$trip_mode%in%c('Bus_driver')] <- EMISSION_FACTORS$PM2_5_emiss_fact[EMISSION_FACTORS$vehicle_type=='BUS']
  vehicle_inventory$distance_ratio_to_car[vehicle_inventory$trip_mode%in%c('Bus_driver')] <- BUS_TO_CAR_RATIO
  vehicle_inventory$emission_factor[vehicle_inventory$trip_mode%in%c('Truck')] <- EMISSION_FACTORS$PM2_5_emiss_fact[EMISSION_FACTORS$vehicle_type=='HDT']
  vehicle_inventory$distance_ratio_to_car[vehicle_inventory$trip_mode%in%c('Truck')] <- TRUCK_TO_CAR_RATIO
  vehicle_inventory$emission_factor[vehicle_inventory$trip_mode%in%c('Motorcycle')] <- EMISSION_FACTORS$PM2_5_emiss_fact[EMISSION_FACTORS$vehicle_type=='2W']
  vehicle_inventory$distance_ratio_to_car[vehicle_inventory$trip_mode%in%c('Motorcycle')] <- MC_TO_CAR_RATIO
  vehicle_inventory <- rbind(vehicle_inventory,data.frame(trip_mode=EMISSION_FACTORS$vehicle_type[7],speed=21,emission_factor=EMISSION_FACTORS$PM2_5_emiss_fact[7],distance_ratio_to_car=LDT_TO_CAR_RATIO))
  vehicle_inventory <- rbind(vehicle_inventory,data.frame(trip_mode=EMISSION_FACTORS$vehicle_type[8],speed=21,emission_factor=EMISSION_FACTORS$PM2_5_emiss_fact[8],distance_ratio_to_car=OTHER_TO_CAR_RATIO))
  VEHICLE_INVENTORY <<- vehicle_inventory
}

add_trips <- function(trip_ids=0,new_mode='Walking',duration=10,participant_id=0,age=20,sex='Male',nTrips=3){
  data.frame(trip_id   = trip_ids, 
             trip_mode = new_mode, 
             trip_duration = sample(duration,nTrips,replace=T), 
             participant_id = participant_id,
             age = sample(age,1,replace=T),
             sex = sample(sex,1,replace=T))
}

edit_accra_trips <- function(raw_trip_set){
  
  total_car_duration <- sum(subset(raw_trip_set,trip_mode=='Private Car')$trip_duration)
  total_car_distance <- total_car_duration/60*VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$trip_mode=='Private Car']
  
  # Redefine motorcycle mode for a select 14 rows
  raw_trip_set$trip_mode[raw_trip_set$trip_mode=='Other'&raw_trip_set$trip_duration<60] <- 'Motorcycle'
  
  # Create new motorbike trips
  # Add 4 new people with 3 trips each
  # Age: 15-59 and gender: male
  new_mode <- 'Motorcycle'
  total_mc_distance <- total_car_distance*VEHICLE_INVENTORY$distance_ratio_to_car[VEHICLE_INVENTORY$trip_mode==new_mode]
  mc_duration <- total_mc_distance/VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$trip_mode==new_mode]*60
  residual_mc_duration <- mc_duration - sum(subset(raw_trip_set,trip_mode==new_mode)$trip_duration)
  duration_range <- 15:100
  nTrips <- 1
  nPeople <- 20#round(residual_mc_duration/nTrips/mean(duration_range))
  duration <- residual_mc_duration/nPeople
  new_gender <- c(rep('Male',20),'Female')
  age_range <- AGE_LOWER_BOUNDS[1]:AGE_LOWER_BOUNDS[3]
  for(i in 1:nPeople){
    new_trips <- add_trips(trip_ids   = max(raw_trip_set$trip_id) + 1: nTrips, 
                           new_mode = new_mode, 
                           duration = duration, 
                           participant_id = max(raw_trip_set$participant_id) + 1,
                           age = age_range,
                           sex = new_gender,
                           nTrips=nTrips)
    # Add new motorbikes trips to baseline
    raw_trip_set <- rbind(raw_trip_set, new_trips)
  }
  
  # Multiply raw_trip_set by 4 to have a bigger number of trips (and raw_trip_set)
  ind1 <- raw_trip_set
  ind1$participant_id <- ind1$participant_id + max(raw_trip_set$participant_id)
  ind1$trip_id <- (max(raw_trip_set$trip_id) + 1): (max(raw_trip_set$trip_id) + nrow(ind1))
  raw_trip_set <- rbind(raw_trip_set, ind1)
  
  ind1 <- raw_trip_set
  ind1$participant_id <- ind1$participant_id + max(raw_trip_set$participant_id)
  ind1$trip_id <- (max(raw_trip_set$trip_id) + 1): (max(raw_trip_set$trip_id) + nrow(ind1))
  raw_trip_set <- rbind(raw_trip_set, ind1)
  
  raw_trip_set
  
}

add_ghost_trips <- function(raw_trip_set){
  
  ## values for new ghost journeys
  age_range <- AGE_LOWER_BOUNDS[1]:(AGE_LOWER_BOUNDS[3]-1)
  nPeople <- 2
  nTrips <- 1
  new_gender <- 'Male'
  total_car_duration <- sum(subset(raw_trip_set,trip_mode=='Private Car')$trip_duration)
  total_car_distance <- total_car_duration/60*VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$trip_mode=='Private Car']
  
  ## add Truck travel
  new_mode <- 'Truck'
  total_truck_distance <- total_car_distance*VEHICLE_INVENTORY$distance_ratio_to_car[VEHICLE_INVENTORY$trip_mode==new_mode]
  truck_duration <- total_truck_distance/VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$trip_mode==new_mode]*60
  duration_range <- c(floor(truck_duration/nPeople),ceiling(truck_duration/nPeople))
  for(i in 1:nPeople){
    new_trips <- add_trips(trip_ids   = max(raw_trip_set$trip_id) + 1: nTrips, 
                           new_mode = new_mode, 
                           duration = duration_range, 
                           participant_id = 0,
                           age = age_range,
                           sex = new_gender,
                           nTrips=nTrips)
    raw_trip_set <- rbind(raw_trip_set, new_trips)
  }
  
  ## add Bus_driver travel
  new_mode <- 'Bus_driver'
  total_bus_distance <- total_car_distance*VEHICLE_INVENTORY$distance_ratio_to_car[VEHICLE_INVENTORY$trip_mode==new_mode]
  bus_duration <- total_bus_distance/VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$trip_mode==new_mode]*60
  duration_range <- c(floor(bus_duration/nPeople),ceiling(bus_duration/nPeople))
  for(i in 1:nPeople){
    new_trips <- add_trips(trip_ids   = max(raw_trip_set$trip_id) + 1: nTrips, 
                           new_mode = new_mode, 
                           duration = duration_range, 
                           participant_id = 0,
                           age = age_range,
                           sex = new_gender,
                           nTrips=nTrips)
    raw_trip_set <- rbind(raw_trip_set, new_trips)
  }
  
  raw_trip_set
  
}

add_walk_trips <- function(bus_trips){
  
  # bus_trips
  # ln_mean = 5
  # ln_sd = 1.2
  
  bus_trips <- arrange(bus_trips, trip_duration)
  walk_trips <- bus_trips
  walk_trips$trip_mode <- 'Short Walking'
  ##RJ all trips have the same BUS_WALK_TIME
  walk_trips$trip_duration <- BUS_WALK_TIME
  
  # Replace walk trips with duration greater than that of bus needs to be set to 0
  if (any(walk_trips$trip_duration - bus_trips$trip_duration  > 0))
    walk_trips$trip_duration[(walk_trips$trip_duration - bus_trips$trip_duration)  > 0] <- 0
  
  bus_trips$trip_duration <- bus_trips$trip_duration - walk_trips$trip_duration
  
  # Corrrect walk trips distance
  walk_trips$trip_distance <- (walk_trips$trip_duration / 60) * VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$trip_mode=='Walking']
  bus_trips$trip_distance <- (bus_trips$trip_duration / 60 ) * VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$trip_mode=='Bus']
  
  # Recategorise trip_distance_cat for both bus and walk trips
  bus_trips$trip_distance_cat[bus_trips$trip_distance > 0 & bus_trips$trip_distance < DIST_LOWER_BOUNDS[2]] <- DIST_CAT[1]
  bus_trips$trip_distance_cat[bus_trips$trip_distance >= DIST_LOWER_BOUNDS[2] & bus_trips$trip_distance < DIST_LOWER_BOUNDS[3]] <- DIST_CAT[2]
  bus_trips$trip_distance_cat[bus_trips$trip_distance >= DIST_LOWER_BOUNDS[3]] <- DIST_CAT[3]
  
  
  walk_trips$trip_distance_cat[walk_trips$trip_distance > 0 & walk_trips$trip_distance < DIST_LOWER_BOUNDS[2]] <- DIST_CAT[1]
  walk_trips$trip_distance_cat[walk_trips$trip_distance >= DIST_LOWER_BOUNDS[2] & walk_trips$trip_distance < DIST_LOWER_BOUNDS[3]] <- DIST_CAT[2]
  walk_trips$trip_distance_cat[walk_trips$trip_distance >= DIST_LOWER_BOUNDS[3]] <- DIST_CAT[3]
  
  return(list(bus_trips, walk_trips))
  
}

######################################################################
## SYNTH FUNCTIONS
get_synthetic_from_trips <- function(){
  raw_trip_set <- TRIP_SET
  
  ## add motorcycle trip to accra, and replicate set four times
  if(CITY=='Accra') raw_trip_set <- edit_accra_trips(raw_trip_set)
  #SURVEY_SCALAR <<- population/length(unique(TRIP_SET$participant_id))/survey_coverage
  ## add bus and truck trips to accra
  if(CITY=='Accra') raw_trip_set <- add_ghost_trips(raw_trip_set)
  
  synth_pop <- create_synth_pop(raw_trip_set)
  SYNTHETIC_POPULATION <<- synth_pop$synthetic_population
  SYNTHETIC_TRIPS <<- synth_pop$trip_set
}

create_synth_pop <- function(raw_trip_set){
  #Add physical activity variables to trip dataset.
  #Leandro Garcia & Ali Abbas.
  #5 July 2018.
  
  # Last Updated by Ali Abbas
  # Added 32 new motorcyle trips 
  # Multiplied baseline dataset by 4
  
  #Notes:
  ##trip_mode = '99': persons who did not travel.
  ##work: job-related physical activity.
  ##ltpa: leisure-time physical activity.
  ##mpa: moderate physical activity (3 MET; 2 marginal MET).
  ##vpa: vigorous physical activity (6 MET; 5 marginal MET).
  ##duration: units are minutes per day.
  ##work_ltpa_marg_met: units are marginal MET-h/week.
  
  trip_set <- subset(raw_trip_set,!trip_mode%in%c("Train", "Other", "Unspecified"))
  # Make age category for trip_set dataset.
  trip_set <- assign_age_groups(trip_set,age_category=AGE_CATEGORY,age_lower_bounds=AGE_LOWER_BOUNDS,max_age=MAX_AGE)
  ##!! assuming more than one age category
  
  pa <- PA_SET
  ##!! RJ question for AA/LG: why the different age categories?
  #Make age category for pa dataset.
  age_category <- c("15-55", "56-69","70+")
  pa <- assign_age_groups(pa,age_category=age_category,age_lower_bounds=c(15,55,70))
  
  #Match persons in the trip (trip_set) e physical activity datasets.
  column_to_keep <- which(colnames(pa)%in%c('work_ltpa_marg_met'))
  unique_ages <- unique(trip_set$age_cat)
  unique_genders <- unique(trip_set$sex)
  
  synthetic_population <- subset(trip_set,!duplicated(participant_id)&participant_id>0)[,names(trip_set)%in%c("participant_id","age","sex","age_cat")]
  synthetic_population$work_ltpa_marg_met <- 0
  ##synth match only for "real" people 
  temp <- c()
  for(age_group in unique_ages){
    for(gender in unique_genders){
      i <- unique(subset(trip_set,age_cat==age_group&sex==gender&participant_id>0)$participant_id)
      pa_age_category <- age_category[which(AGE_CATEGORY==age_group)]
      matching_people <- as.data.frame(filter(pa, age_cat == pa_age_category & sex == gender)[,column_to_keep])
      v <- (matching_people[sample(nrow(matching_people),length(i),replace=T),])
      temp <- rbind( temp, cbind(v,i) )
      i <- which(synthetic_population$age_cat==age_group&synthetic_population$sex==gender)
      synthetic_population$work_ltpa_marg_met[i] <- c(v)
    }
  }
  
  namevector <- c(colnames(pa)[column_to_keep], "participant_id")
  colnames(temp) <- namevector
  temp <- as.data.frame (temp)
  
  trip_and_pa_set <- left_join(trip_set, temp, "participant_id")
  
  # Convert all int columns to numeric
  trip_and_pa_set[, sapply(trip_and_pa_set,class)=='integer'] <- lapply(trip_and_pa_set[, sapply(trip_and_pa_set,class)=='integer'], as.numeric)
  synthetic_population[, sapply(synthetic_population,class)=='integer'] <- lapply(synthetic_population[, sapply(synthetic_population,class)=='integer'], as.numeric)
  trip_set <- subset(trip_set,trip_mode!=99)
  
  trip_and_pa_set$trip_id[trip_and_pa_set$trip_mode == '99'] <- 0
  
  list(trip_and_pa_set=trip_and_pa_set,trip_set=trip_set,synthetic_population=synthetic_population)
  
}

assign_age_groups <- function(dataset,age_category=AGE_CATEGORY,age_lower_bounds=AGE_LOWER_BOUNDS,max_age=MAX_AGE){
  dataset <- filter(dataset,age<max_age)
  dataset$age_cat <- 0
  ##!! assuming more than one age category
  for(i in 2:length(age_lower_bounds)-1){
    dataset$age_cat[dataset$age >= age_lower_bounds[i] & dataset$age < age_lower_bounds[i+1]] <- age_category[i]
  }
  dataset$age_cat[dataset$age >= age_lower_bounds[length(age_lower_bounds)]] <- age_category[length(age_lower_bounds)]
  dataset
}

ithim_setup_baseline_scenario <- function(){
  ## SET UP TRAVEL DATA
  trip_set <- SYNTHETIC_TRIPS
  # Create a row id
  trip_set$rid <- 1:nrow(trip_set)
  
  # Define trip_distances (in km)
  # Based on travel mode and trip duration, calculate distances
  
  mode_indices <- match(trip_set$trip_mode,VEHICLE_INVENTORY$trip_mode)
  trip_speeds <- VEHICLE_INVENTORY$speed[mode_indices]
  trip_speeds[is.na(trip_speeds)] <- 0
  trip_set$trip_distance <- (trip_set$trip_duration / 60) * trip_speeds
  
  # Initialize them
  ## Distance categories are used in scenario generation. They correspond to e.g. ``long trips'' and ``short trips''
  trip_set$trip_distance_cat <- 0
  ##!! assuming more than one distance category
  for(i in 2:length(DIST_LOWER_BOUNDS)-1){
    trip_set$trip_distance_cat[trip_set$trip_distance >= DIST_LOWER_BOUNDS[i] & trip_set$trip_distance < DIST_LOWER_BOUNDS[i+1]] <- DIST_CAT[i]
  }
  trip_set$trip_distance_cat[trip_set$trip_distance >= DIST_LOWER_BOUNDS[length(DIST_LOWER_BOUNDS)]] <- DIST_CAT[length(DIST_LOWER_BOUNDS)]
  
  ##RJ should not need to do ages as age_cat already exists in the synthetic population. 70+ people already filtered out also.
  # Make age category
  #for(i in 2:length(AGE_LOWER_BOUNDS)-1){
  #  trip_set$age_cat[trip_set$age >= AGE_LOWER_BOUNDS[i] & trip_set$age < AGE_LOWER_BOUNDS[i+1]] <- AGE_CATEGORY[i]
  #}
  #trip_set$age_cat[trip_set$age >= AGE_LOWER_BOUNDS[length(AGE_LOWER_BOUNDS)]] <- AGE_CATEGORY[length(AGE_LOWER_BOUNDS)]
  # Remove all participants greater than 70 years of age
  #trip_set <- filter(trip_set, age_cat != AGE_CATEGORY[length(AGE_LOWER_BOUNDS)])
  
  trip_set$scenario <- "Baseline"
  
  ##RJ question for AA: do we want to add walking to train?
  bus_walk_trips <- add_walk_trips(filter(trip_set, trip_mode == "Bus"))
  
  trip_set$trip_duration[trip_set$trip_mode == 'Bus' & trip_set$rid %in% bus_walk_trips[[1]]$rid] <- bus_walk_trips[[1]]$trip_duration
  trip_set$trip_distance[trip_set$trip_mode == 'Bus' & trip_set$rid %in% bus_walk_trips[[1]]$rid] <- bus_walk_trips[[1]]$trip_distance
  trip_set$trip_distance_cat[trip_set$trip_mode == 'Bus' & trip_set$rid %in% bus_walk_trips[[1]]$rid] <- bus_walk_trips[[1]]$trip_distance_cat
  
  trip_set <- rbind(trip_set, bus_walk_trips[[2]])
  
  trip_set$row_id <- 1:nrow(trip_set)

  trip_set
}

create_all_scenarios <- function(trip_set){
  ###############################################################
  rd_list <- list()
  rd_list[[1]] <- trip_set
  # Scenario 1
  
  rdr <- trip_set
  
  source_modes <- c('Bus', 'Walking')
  target_modes <- c('Private Car')
  
  source_percentages <- c(0.16, 0.49)
  
  tt <- nrow(filter(rdr, ! trip_mode %in% c('99', 'Short Walking')))
  
  rdr <- create_scenario(rdr, scen_name = 'Scenario 1', source_modes = source_modes, 
                         target_modes = target_modes, source_distance_cats = DIST_CAT, 
                         source_trips = c(round(source_percentages[1] * tt), 
                                          round(source_percentages[2] * tt)))
  
  rd_list[[2]] <- rdr
  
  ###############################################################
  # Scenario 2
  
  rdr <- rd_list[[2]]#filter(rdfinal, scenario == 'Scenario 1')
  
  # 35 % of all trips are Bus.
  # These come from private car and taxi.
  # All car and taxi trips > 6 km go to Bus. Then 35 car and taxi trips 0--6 km go to bus.
  
  source_modes <- c('Private Car', 'Taxi')
  target_modes <- c('Bus')
  
  tt <- nrow(filter(rdr,! trip_mode %in% c('99', 'Short Walking')))
  
  target_new_trips <- round(0.35 * tt - sum(rdr$trip_mode=='Bus'))
  
  total_car_trips <- filter(rdr, trip_mode %in% source_modes)
  
  t_dc <- total_car_trips %>% group_by(trip_distance_cat) %>% summarise(count = dplyr::n())
  
  long_trips <- sum(t_dc$count[t_dc$trip_distance_cat != DIST_CAT[1]])
  
  long_car_trips_sample <- create_scenario(total_car_trips, scen_name = 'Scenario 2', source_modes = source_modes, combined_modes = T, 
                                           target_modes = target_modes, source_distance_cats = DIST_CAT[2:3], 
                                           source_trips = long_trips)
  
  short_trips <- as.integer(target_new_trips - long_trips)
  if(short_trips>0){
    short_car_trips_sample <- create_scenario(total_car_trips, scen_name = 'Scenario 2', source_modes = source_modes, combined_modes = T, 
                                              target_modes = target_modes, source_distance_cats = DIST_CAT[1], 
                                              source_trips = short_trips) ##!! RJ for some reason short_trips is not working here.
    
    long_car_trips_sample <- rbind(long_car_trips_sample, short_car_trips_sample)
  }
  
  ##  ADDING SHORT WALK TRIPS FOR NEW BUS TRIPS
  
  # Divide bus trips into bus and walk trips
  bus_trips <- long_car_trips_sample
  
  bus_walk_trips <- add_walk_trips(bus_trips)
  
  # Update selected rows for mode and duration
  rdr$trip_mode[rdr$row_id %in% bus_walk_trips[[1]]$row_id] <- bus_walk_trips[[1]]$trip_mode
  rdr$trip_duration[rdr$trip_mode == 'Bus' & rdr$rid %in% bus_walk_trips[[1]]$rid] <- bus_walk_trips[[1]]$trip_duration
  rdr$trip_distance[rdr$trip_mode == 'Bus' & rdr$rid %in% bus_walk_trips[[1]]$rid] <- bus_walk_trips[[1]]$trip_distance
  rdr$trip_distance_cat[rdr$trip_mode == 'Bus' & rdr$rid %in% bus_walk_trips[[1]]$rid] <- bus_walk_trips[[1]]$trip_distance_cat
  
  rdr %>% group_by(trip_mode) %>% summarise(c = dplyr::n(), p = dplyr::n() / nrow(rdr) * 100)
  
  rdr <- rbind(rdr, bus_walk_trips[[2]])
  
  rdr$scenario <- "Scenario 2"
  
  #rdfinal <- rbind(rdfinal, rdr)
  rd_list[[3]] <- rdr
  ###############################################################
  # Scenario 3
  
  rdr <- rd_list[[2]]#filter(rdfinal, scenario == 'Scenario 1')
  
  # 16 % Bus remain as is
  # 10 % Mcycle increase 
  # x decrease private car
  
  tt <- nrow(filter(rdr,! trip_mode %in% c('99', 'Short Walking')))
  source_modes <- c('Private Car')
  target_modes <- c('Motorcycle')
  
  target_new_trips <- c(round(0.1 * tt) - 
                          nrow(filter(rdr, trip_mode == 'Motorcycle')))
  
  mcycle_trips_sample <- create_scenario(rdr, scen_name = 'Scenario 3', source_modes = source_modes, 
                                         combined_modes = T, target_modes = target_modes, 
                                         source_distance_cats = DIST_CAT, source_trips = c(round(0.1 * tt) - 
                                                                                             nrow(filter(rdr, trip_mode == 'Motorcycle'))))
  
  # Update selected rows for mode and duration
  rdr[rdr$row_id %in% mcycle_trips_sample$row_id,]$trip_mode <- mcycle_trips_sample$trip_mode
  rdr[rdr$row_id %in% mcycle_trips_sample$row_id,]$trip_duration <- mcycle_trips_sample$trip_duration
  
  rdr$scenario <- "Scenario 3"
  
  #rdfinal <- rbind(rdfinal, rdr)
  rd_list[[4]] <- rdr
  ###############################################################
  # Scenario 4
  
  rdr <- rd_list[[2]]#filter(rdfinal, scenario == 'Scenario 1')
  
  # 3.5 % Cycle
  
  tt <- nrow(filter(rdr, ! trip_mode %in% c('99', 'Short Walking')))
  
  source_modes <- c('Motorcycle', 'Private Car', 'Taxi')
  target_modes <- c('Bicycle')
  
  target_new_trips <- c(52, round(0.035 * tt) - nrow(filter(rdr, trip_mode == 'Bicycle')) - 52)
  
  mbike_trips <- create_scenario(rdr, scen_name = 'Scenario 4', source_modes = source_modes[1], 
                                 combined_modes = T, target_modes = target_modes, 
                                 source_distance_cats = DIST_CAT, 
                                 source_trips = target_new_trips[1])
  
  car_trips <- create_scenario(rdr, scen_name = 'Scenario 4', source_modes = c(source_modes[2], source_modes[3]), 
                               combined_modes = T, target_modes = target_modes, 
                               source_distance_cats = DIST_CAT[1], 
                               source_trips = target_new_trips[2])
  
  car_mbike_trips <- rbind(mbike_trips, car_trips)
  
  # Update selected rows for mode and duration
  rdr[rdr$row_id %in% car_mbike_trips$row_id,]$trip_mode <- car_mbike_trips$trip_mode
  rdr[rdr$row_id %in% car_mbike_trips$row_id,]$trip_duration <- car_mbike_trips$trip_duration
  
  rdr %>% group_by(trip_mode) %>% summarise(c = dplyr::n(), p = dplyr::n() / nrow(rdr) * 100)
  
  rdr$scenario <- "Scenario 4"
  
  #rdfinal <- rbind(rdfinal, rdr)
  rd_list[[5]] <- rdr
  ###############################################################
  # Scenario 5
  
  rdr <- rd_list[[2]]#filter(rdfinal, scenario == 'Scenario 1')
  
  # 3.5 % Cycle
  
  tt <- nrow(filter(rdr, ! trip_mode %in% c('99', 'Short Walking')))
  
  source_modes <- c('Private Car', 'Taxi')
  target_modes <- c('Walking')
  
  target_new_trips <- c(round(0.54 * tt) - nrow(filter(rdr, trip_mode == 'Walking')))
  
  motorised_trips <- create_scenario(rdr, scen_name = 'Scenario 4', source_modes = c(source_modes[1], source_modes[2]), 
                                     combined_modes = T, target_modes = target_modes, 
                                     source_distance_cats = DIST_CAT[1], 
                                     source_trips = target_new_trips[1])
  
  # Update selected rows for mode and duration
  rdr[rdr$row_id %in% motorised_trips$row_id,]$trip_mode <- motorised_trips$trip_mode
  rdr[rdr$row_id %in% motorised_trips$row_id,]$trip_duration <- motorised_trips$trip_duration
  rdr %>% group_by(trip_mode) %>% summarise(c = dplyr::n(), p = dplyr::n() / nrow(rdr) * 100)
  rdr$scenario <- "Scenario 5"
  
  rd_list[[6]] <- rdr
  
  
  do.call('rbind',rd_list)
}

create_scenario <- function(rdr, scen_name, source_modes, combined_modes = F, target_modes, source_distance_cats, 
                            source_trips, target_trips){
  ##!! RJ target_modes must be length 1
  local_source_trips <- list()
  if (!combined_modes){
    for (i in 1:length(source_modes)){
      local_source_trips[i] <- nrow(filter(rdr, trip_mode == source_modes[i])) - source_trips[i]
    }
    local_source_trips <- purrr::flatten_dbl(local_source_trips)
  }
  
  all_samples <- NULL
  
  if (!combined_modes){
    
    for (i in 1:length(source_modes)){
      
      sample <- filter(rdr,
                       trip_mode == source_modes[i] &
                         trip_distance_cat %in% source_distance_cats) %>% sample_n(local_source_trips[i]) %>%
        mutate(
          trip_mode = target_modes[1],
          trip_duration = (trip_distance * 60) / VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$trip_mode == target_modes[1]]
        )
      # Update selected rows for mode and duration
      rdr[rdr$row_id %in% sample$row_id,]$trip_mode <- sample$trip_mode
      rdr[rdr$row_id %in% sample$row_id,]$trip_duration <- sample$trip_duration
      if (source_modes[i] == 'Bus'){
        # Remove bus associated short walking trips that have been changed to Private Car trips
        rdr <- rdr[!(rdr$trip_mode == 'Short Walking' & rdr$trip_id %in% sample$trip_id),]
      }
    } 
  }  else {
    
    sample <- filter(rdr,
                     trip_mode %in% source_modes &
                       trip_distance_cat %in% source_distance_cats) %>% sample_n(source_trips) %>%
      mutate(
        trip_mode = target_modes[1],
        trip_duration = (trip_distance * 60) / VEHICLE_INVENTORY$speed[VEHICLE_INVENTORY$trip_mode == target_modes[1]]
      )
    
    sample$scenario <- scen_name
    
    return(sample)
  }
  
  
  rdr$scenario <- scen_name
  
  return(rdr)
  
}

set_scenario_specific_variables <- function(trip_set){
  NSCEN <<- length(unique(trip_set$scenario)) - 1
  SCEN <<- unique(trip_set$scenario)
  SCEN_SHORT_NAME <<- c("base",paste0("scen", 1:NSCEN) )
}

######################################################################
## DISTANCE FUNCTIONS
get_all_distances <- function(ithim_object){
  trip_set <- ithim_setup_baseline_scenario()
  ithim_object$trip_scen_sets <- create_all_scenarios(trip_set)
  set_scenario_specific_variables(ithim_object$trip_scen_sets)
  # Generate distance and duration matrices
  dist_and_dir <- dist_dur_tbls(ithim_object$trip_scen_sets)
  ithim_object$dist <- dist_and_dir$dist
  ithim_object$dur <- dist_and_dir$dur
  # distances for injuries calculation
  ithim_object$inj_distances <- distances_for_injury_function(ithim_object$trip_scen_sets)
  ithim_object
}

distances_for_injury_function <- function(trip_scen_sets){
  ##synth need supplementary journeys to have age_cat,sex,trip_mode, scenario
  
  journeys <- trip_scen_sets %>% 
    group_by (age_cat,sex,trip_mode, scenario) %>% 
    summarise(tot_dist = sum(trip_distance))
  distances <- spread(journeys,trip_mode, tot_dist,fill=0) 
  distances$Pedestrian <- distances$Walking + distances$`Short Walking`
  distances <- distances[, -which(names(distances) ==  "Walking")]
  distances <- distances[, -which(names(distances) ==  "Short Walking")]
  distances$Car <- distances$Taxi + distances$`Private Car`
  distances <- distances[, -which(names(distances) ==  "Private Car")]
  distances <- distances[, -which(names(distances) ==  "Taxi")]
  true_distances <- distances
  true_distances$sex_age <-  paste0(true_distances$sex,"_",true_distances$age_cat)
  true_distances$Bus <- true_distances$Bus + true_distances$Bus_driver
  true_distances <- true_distances[,-c(which(names(true_distances) == 'sex'))]
  
  scen_dist <- sapply(1:(NSCEN+1),function(x)c(colSums(subset(distances,scenario == SCEN[x])[,3+1:(length(unique(journeys$trip_mode))-2)])))
  colnames(scen_dist) <- SCEN_SHORT_NAME
  for(i in 2:ncol(scen_dist)) scen_dist[,i] <- scen_dist[,i]/scen_dist[,1] 
  scen_dist <- rbind(scen_dist,Tuktuk=1)
  
  mode_names <- names(distances)[3+1:(length(unique(journeys$trip_mode))-2)]
  for (i in 1: length(mode_names))
    for (n in 1:(NSCEN+1))
      distances[[mode_names[i]]][which(distances$scenario == SCEN[n])] <- 
    distances[[mode_names[i]]][which(distances$scenario == SCEN[n])]/ sum(distances[[mode_names[i]]][which(distances$scenario == SCEN[n])],na.rm=T)
  relative_distances <- distances
  relative_distances$sex_age <-  paste0(relative_distances$sex,"_",relative_distances$age_cat)
  relative_distances <- relative_distances[,-c(which(names(relative_distances) == 'sex'))]
  
  list(relative_distances=relative_distances,scen_dist=scen_dist,true_distances=true_distances)
}

dist_dur_tbls <- function(trip_scen_sets){
  
  bs <- trip_scen_sets
  
  ## calculate all distances & durations
  l_dist <-  l_dur <- list()
  for (i in 1:length(SCEN)){
    local <- group_by(filter(bs,scenario == SCEN[i]), trip_mode)
    
    local_dist <- summarise(local, sum_dist = sum(trip_distance))
    local_dist$sum_dist[local_dist$trip_mode == "Walking"] <- 
      local_dist$sum_dist[local_dist$trip_mode == "Walking"] + 
      local_dist$sum_dist[local_dist$trip_mode == "Short Walking"]
    colnames(local_dist)[2] <- SCEN[i]
    l_dist[[i]] <- local_dist
    
    local_dur <- summarise(local, sum_dur = sum(trip_duration))
    local_dur$sum_dur[local_dur$trip_mode == "Walking"] <- 
      local_dur$sum_dur[local_dur$trip_mode == "Walking"] + 
      local_dur$sum_dur[local_dur$trip_mode == "Short Walking"]
    colnames(local_dur)[2] <- SCEN[i]
    l_dur[[i]] <- local_dur
  }
  
  ## join distances & durations
  for (i in 1:length(l_dist)){
    if (i == 1){
      local_dist <- l_dist[[i]]
      local_dur <- l_dur[[i]]
    }else{
      local_dist <- left_join(local_dist, l_dist[[i]], by = "trip_mode")
      local_dur <- left_join(local_dur, l_dur[[i]], by = "trip_mode")
    }
  }
 
  # Remove short walking
  dist <- filter(local_dist, trip_mode != 'Short Walking')
  dur <- filter(local_dur, trip_mode != 'Short Walking')
  
  list(dist=dist,dur=dur)
}

######################################################################
## CALCULATION FUNCTIONS
total_mmet <- function(trip_scen_sets){
  
  synth_pop <- SYNTHETIC_POPULATION
  rd_pa <- subset(trip_scen_sets,trip_mode%in%c('Walking','Short Walking','Bicycle')&participant_id%in%synth_pop$participant_id) 
  # Convert baseline's trip duration from mins to hours
  rd_pa$trip_duration_hrs <- rd_pa$trip_duration / 60
  # Get total individual level walking and cycling and sport mmets 
  for (i in 1:length(SCEN)){
    synth_pop[[paste0(SCEN_SHORT_NAME[i],'_mmet')]] <- synth_pop$work_ltpa_marg_met * BACKGROUND_PA_SCALAR
    scen_trips <- subset(rd_pa,scenario == SCEN[i]&participant_id%in%synth_pop$participant_id)
    
    individual_data <- setDT(scen_trips)[,.(cycling_mmet_base = sum(trip_duration_hrs[trip_mode == 'Bicycle']) * MMET_CYCLING,
                                 walking_mmet_base = sum(trip_duration_hrs[trip_mode %in%c('Walking','Short Walking')]) * MMET_WALKING ),by='participant_id']
    
    
    synth_pop[[paste0(SCEN_SHORT_NAME[i],'_mmet')]][match(individual_data$participant_id,synth_pop$participant_id)] <- 
      synth_pop[[paste0(SCEN_SHORT_NAME[i],'_mmet')]][match(individual_data$participant_id,synth_pop$participant_id)] + individual_data$cycling_mmet_base + individual_data$walking_mmet_base
  }
  
  name_indices <- which(colnames(synth_pop)%in%c('participant_id', 'sex', 'age', 'age_cat', paste0(SCEN_SHORT_NAME,'_mmet')))
  mmets <- tbl_df(synth_pop)[,name_indices]
  mmets
  
}

scenario_pm_calculations <- function(dist,trip_scen_sets){
  
  # concentration contributed by non-transport share (remains constant across the scenarios)
  non_transport_pm_conc <- PM_CONC_BASE*(1 - PM_TRANS_SHARE)  
  
  ## adding in travel not covered in the synthetic trip set, based on distances travelled relative to car, set in VEHICLE_INVENTORY
  emission_dist <- dist
  for(mode_type in which(!VEHICLE_INVENTORY$trip_mode%in%emission_dist$trip_mode)){
    emission_dist <- rbind(emission_dist,emission_dist[which(emission_dist$trip_mode=='Private Car'),])
    emission_dist[nrow(emission_dist),1] <- VEHICLE_INVENTORY$trip_mode[mode_type]
    emission_dist[nrow(emission_dist),0:NSCEN+2] <- emission_dist[nrow(emission_dist),2]*VEHICLE_INVENTORY$distance_ratio_to_car[mode_type]
  }
  
  ## multiply distance by emission factor. (We don't need to scale to a whole year, as we are just scaling the background concentration.)
  ordered_efs <- VEHICLE_INVENTORY$emission_factor[match(emission_dist$trip_mode,VEHICLE_INVENTORY$trip_mode)]
  trans_emissions <- emission_dist[,0:NSCEN+2]*t(repmat(ordered_efs,NSCEN+1,1))#*SURVEY_SCALAR
  
  baseline_sum <- sum(trans_emissions[[SCEN[1]]])
  conc_pm <- c()
  for(i in 1:length(SCEN_SHORT_NAME))
    conc_pm[i] <- non_transport_pm_conc + PM_TRANS_SHARE*PM_CONC_BASE*sum(trans_emissions[[SCEN[i]]])/baseline_sum
  
  ##RJ rewriting ventilation as a function of MMET_CYCLING and MMET_WALKING, loosely following de Sa's SP model.
  vent_rates <- data.frame(trip_mode=VEHICLE_INVENTORY$trip_mode,stringsAsFactors = F) 
  vent_rates$vent_rate <- BASE_LEVEL_INHALATION_RATE # L / min
  vent_rates$vent_rate[vent_rates$trip_mode=='Bicycle'] <- BASE_LEVEL_INHALATION_RATE + 5.0*MMET_CYCLING
  vent_rates$vent_rate[vent_rates$trip_mode%in%c('Walking','Short Walking')] <- BASE_LEVEL_INHALATION_RATE + 5.0*MMET_WALKING
  
  ##RJ rewriting exposure ratio as function of ambient PM2.5, as in Goel et al 2015
  ##!! five fixed parameters: BASE_LEVEL_INHALATION_RATE (10), CLOSED_WINDOW_PM_RATIO (0.5), CLOSED_WINDOW_RATIO (0.5), ROAD_RATIO_MAX (3.216), ROAD_RATIO_SLOPE (0.379)
  ##RJ question for RG: should this function account for PM_TRANS_SHARE?
  on_road_off_road_ratio <- ROAD_RATIO_MAX - ROAD_RATIO_SLOPE*log(conc_pm)
  ##RJ question for RG: why is 'in car' twice better than 'away from road'?
  in_vehicle_ratio <- (1-CLOSED_WINDOW_RATIO)*on_road_off_road_ratio + CLOSED_WINDOW_RATIO*CLOSED_WINDOW_PM_RATIO # averaging over windows open and windows closed
  ratio_by_mode <- rbind(on_road_off_road_ratio,in_vehicle_ratio)
  
  vent_rates$vehicle_ratio_index <- sapply(vent_rates$trip_mode,function(x) ifelse(x%in%c('Walking','Short Walking','Bicycle'),1,2))
  
  trip_set <- left_join(trip_scen_sets,vent_rates,'trip_mode')
  trip_set$on_road_air <- trip_set$trip_duration*trip_set$vent_rate / 60 # L
  scen_index <- match(trip_set$scenario,SCEN)
  scen_pm <- as.numeric(conc_pm[scen_index])
  scen_ratio <- ratio_by_mode[cbind(trip_set$vehicle_ratio_index,scen_index)]
  trip_set$pm_dose <- trip_set$on_road_air * scen_ratio * scen_pm # mg
  
  synth_pop <- SYNTHETIC_POPULATION
  
  ### following code generates final_data
  for (i in 1:length(SCEN)){
    synth_pop[[paste0('pm_conc_',SCEN_SHORT_NAME[i])]] <- conc_pm[i]
    scen_trips <- subset(trip_set,scenario == SCEN[i]&participant_id%in%synth_pop$participant_id)
    
    individual_data <- setDT(scen_trips)[,.(on_road_dur = sum(trip_duration,na.rm=TRUE), 
                                            on_road_pm = sum(pm_dose,na.rm=TRUE), 
                                            air_inhaled = sum(on_road_air,na.rm=TRUE)),by='participant_id']
    
    ## PM2.5 inhalation = total mg inhaled / total volume inhaled
    non_transport_air_inhaled <- (24-individual_data$on_road_dur/60)*BASE_LEVEL_INHALATION_RATE
    pm_conc <- ((non_transport_air_inhaled * as.numeric(conc_pm[i])) + individual_data$on_road_pm)/(non_transport_air_inhaled+individual_data$air_inhaled)
    
    synth_pop[[paste0('pm_conc_',SCEN_SHORT_NAME[i])]][match(individual_data$participant_id,synth_pop$participant_id)] <- pm_conc
  }
  
  #####PM normalise
  ##RJ question for RG: why normalise?
  mean_conc <- rep(0,length(SCEN_SHORT_NAME))
  
  ## calculating means of individual-level concentrations
  for ( i in 1: length(SCEN_SHORT_NAME))
    mean_conc[i] <- mean(synth_pop[[paste0("pm_conc_", SCEN_SHORT_NAME[i])]])
  
  normalise <- as.numeric(conc_pm[1])/as.numeric(mean_conc[1])
  ###Lines which are normalising the concentrations
  
  for (i in 1: length(SCEN_SHORT_NAME))
    synth_pop[[paste0("pm_conc_", SCEN_SHORT_NAME[i])]] <- normalise*synth_pop[[paste0("pm_conc_", SCEN_SHORT_NAME[i])]]
  
  synth_pop$participant_id <- as.integer(synth_pop$participant_id)

  list(scenario_pm=conc_pm, pm_conc_pp=synth_pop)
  
}

gen_ap_rr <- function(pm_conc_pp){
  
  ### combining PM2.5 concentration data (scenario_pm_calculations.R) and PA data (total_mmet.R) at the individual level (n=732)
  
  pm_rr_pp <- pm_conc_pp ## PM2.5 relative risk per person
  
  ## assigning air pollution age band to the individual_level data
  min_ages <- c(seq(24,94,by=5),200)
  pm_rr_pp$ap_age <-
    sapply(pm_rr_pp$age, function(x)
      if(x > min_ages[1])
        min_ages[which(min_ages > x)[1] - 1] + 1
      else
        0)
  
  pm_indices <- sapply(SCEN_SHORT_NAME,function(x)which(colnames(pm_rr_pp)==paste0("pm_conc_",x)))
  ### iterating over all all disease outcomes
  for ( j in 1:nrow(DISEASE_OUTCOMES)){
    ## checking whether to calculate this health outcome for air pollution
    if (DISEASE_OUTCOMES$air_pollution[j] == 1){ 
      # initialise lists
      for (x in 1:length(SCEN_SHORT_NAME))
        pm_rr_pp[[paste0("RR_ap_", SCEN_SHORT_NAME[x])]] <- 0
      cause <- as.character(DISEASE_OUTCOMES$ap_acronym[j])
      dr_ap_disease <- subset(DR_AP, cause_code == cause)
      # apply by age groups
      ages <- unique(dr_ap_disease$age_code)
      for(age in ages){
        dr_ap_sub <- subset(dr_ap_disease,age_code == age )
        if(age==99){
          i <-1:nrow(pm_rr_pp)
        }else{
          i <- which(pm_rr_pp$ap_age==age)
        }
        # get parameters
        alpha <- DR_AP_LIST[[cause]][[as.character(age)]]$alpha
        beta <- DR_AP_LIST[[cause]][[as.character(age)]]$beta
        gamma <- DR_AP_LIST[[cause]][[as.character(age)]]$gamma
        tmrel <- DR_AP_LIST[[cause]][[as.character(age)]]$tmrel
        # calculate AP and apply to all in age group
        for(x in 1: length(SCEN_SHORT_NAME)) 
          pm_rr_pp[[paste0("RR_ap_", SCEN_SHORT_NAME[x])]][i] <-
          as.numeric(1 + alpha * (1 - exp(-beta * (pm_rr_pp[[pm_indices[x]]][i] - tmrel) ^ gamma )))
      }
      ## change the names of the columns as per the disease
      for (n in 1: length(SCEN_SHORT_NAME)){
        col <- which(names(pm_rr_pp)== paste0("RR_ap_",SCEN_SHORT_NAME[n]))
        names(pm_rr_pp)[col]<- paste0("RR_ap_",SCEN_SHORT_NAME[n],"_",DISEASE_OUTCOMES$acronym[j])
      }
    }
  }
  pm_rr_pp
}

PA_dose_response <- function (cause, outcome_type, dose, confidence_intervals = F){
  
  if (sum(is.na(dose))>0 || class(dose)!= "numeric"){
    stop ('Please provide dose in numeric')
  }
  if (!cause %in% c('all_cause_mortality', 'breast-cancer', 'cardiovascular-disease',
                    'colon-cancer', 'coronary_heart_disease', 'diabetes', 'endometrial-cancer',
                    'heart-failure', 'lung_cancer', 'stroke', 'total_cancer')){
    stop('Unsupported cause/disease. Please select from \n
         all_cause_mortality \n
         breast-cancer\n
         cardiovascular-disease \n
         colon-cancer \n
         coronary_heart_disease \n
         endometrial-cancer \n
         heart-failure \n
         lung_cancer \n
         stroke \n
         total_cancer')
  }
  if (!outcome_type %in% c('mortality', 'incidence')){
    stop('Unsupported outcome_type. Please select from \n
         mortality \n
         incidence')
  }
  if (cause == 'all_cause_mortality' && outcome_type == 'incidence'){
    stop('Incidence does not exist for all_cause_mortality')
  }
  fname <- paste(cause, outcome_type, sep = "_")
  if (cause == 'all_cause_mortality')
    fname <- cause
  lookup_table <- get(paste0(fname))
  lookup_df <- as.data.frame(lookup_table)
  #pert_75 <- stringr::str_sub(basename(list_of_files[[1]]), end = -5)
  ##RJ previously:
  ## cond <- ifelse(use_75_pert, abs(lookup_table$dose - dose), which.min(abs(lookup_table$dose - dose)))
  rr <- approx(x=lookup_df$dose,y=lookup_df$RR,xout=dose,yleft=1,yright=min(lookup_df$RR))$y
  if (confidence_intervals || PA_DOSE_RESPONSE_QUANTILE==T) {
    lb <-
      approx(
        x = lookup_df$dose,
        y = lookup_df$lb,
        xout = dose,
        yleft = 1,
        yright = min(lookup_df$lb)
      )$y
    ub <-
      approx(
        x = lookup_df$dose,
        y = lookup_df$ub,
        xout = dose,
        yleft = 1,
        yright = min(lookup_df$ub)
      )$y
  }
  if (PA_DOSE_RESPONSE_QUANTILE==T){
    ##RJ question for AA: this function has standard deviation = 1. Is that right?
    rr <- truncnorm::qtruncnorm(get(paste0('PA_DOSE_RESPONSE_QUANTILE_',cause)), rr, sd=rr-lb,a=0, b=1)
  }
  if (confidence_intervals) {
    return(data.frame (rr = rr, lb = lb, ub = ub))
  }else{
    return(data.frame(rr = rr))
  }
}

gen_pa_rr <- function(mmets_pp){
  ### iterating over all all disease outcomes
  dose_columns <- match(paste0(SCEN_SHORT_NAME, '_mmet'),colnames(mmets_pp))
  doses_clean <- mmets_pp[,dose_columns]
  for ( j in 1:nrow(DISEASE_OUTCOMES)){
    ## checking whether to calculate this health outcome for PA
    if (DISEASE_OUTCOMES$physical_activity[j] == 1){
      pa_dn <- as.character(DISEASE_OUTCOMES$pa_acronym[j])
      pa_n <- as.character(DISEASE_OUTCOMES$acronym[j])
      outcome_type <- ifelse(pa_dn%in%c('lung_cancer','stroke'), 'incidence' , 'mortality')
      # CHD: 35 mmeth per week use mortality
      # Lung cancer: 10 mmeth per week use incidence
      # stroke 75 pert: 13.37
      # Diabetes no limits
      # total cancer: 35 mmeths per week use mortality
      doses <- doses_clean
      if(pa_dn %in% c('total_cancer','coronary_heart_disease')) doses[doses>35] <- 35
      else if(pa_dn == 'lung_cancer') doses[doses>10] <- 10
      else if(pa_dn == 'stroke') doses[doses>13.37] <- 13.37
      else if(pa_dn == 'all_cause_mortality') doses[doses>16.08] <- 16.08
      ##RJ apply function to all doses as one long vector
      return_vector <- PA_dose_response(cause = pa_dn, outcome_type = outcome_type, 
                                   dose = unlist(data.frame(doses)))
      ##RJ take segments of returned vector corresponding to scenario
      for (i in 1:length(SCEN_SHORT_NAME)){
        scen <- SCEN_SHORT_NAME[i]
        mmets_pp[[paste('RR_pa', scen, pa_n, sep = '_')]] <- return_vector$rr[(1+(i-1)*nrow(doses)):(i*nrow(doses))]
      }
    }
  }
  mmets_pp 
}

combined_rr_pa_pa <- function(ind_pa,ind_ap){
  
  # Replace NaNs with 1
  ind_ap[is.na(ind_ap)] <- 1
  
  # Replace Na with 1
  ind_pa[is.na(ind_pa)] <- 1
  
  # remove common columns from ap
  ind_ap <- dplyr::select(ind_ap, -c(sex, age, age_cat))
  
  # join pa and ap datasets
  ind_ap_pa <- left_join(ind_pa, ind_ap, by = "participant_id")

  ### iterating over all all disease outcomes
  for ( j in 1:nrow(DISEASE_OUTCOMES)){
    ## checking whether to calculate this health outcome for PA
    if (DISEASE_OUTCOMES$physical_activity[j] == 1 & DISEASE_OUTCOMES$air_pollution[j] == 1){
      ac <- as.character(DISEASE_OUTCOMES$acronym[j])
      for (scen in SCEN_SHORT_NAME){
        ind_ap_pa[[paste('RR_pa_ap', scen, ac, sep = '_')]] <- ind_ap_pa[[paste('RR_pa', scen, ac, sep = '_')]] * ind_ap_pa[[paste('RR_ap', scen, ac, sep = '_')]]
        
      }
    }
  }
  
  ind_ap_pa
}

injuries_function_2 <- function(true_distances){
  ##RJ
  mode_names <- names(true_distances)[!names(true_distances)%in%c('age_cat','scenario','sex_age')]
  # divide injuries into those for which we can write a WHW matrix, i.e. we know distances of both striker and casualty, 
  ## and those for which we don't know striker distance: no or other vehicle (noov)
  ## we can only model casualties for which we know distance travelled (i.e. no Truck casualties for Accra)
  injury_list <- list()
  injury_list$whw <- subset(INJURIES,cas_mode%in%mode_names&strike_mode%in%mode_names)
  injury_list$noov <- subset(INJURIES,cas_mode%in%mode_names&!strike_mode%in%mode_names)
  injury_table <- list()
  cas_mode_indices <- list()
  injury_gen_age <- list()
  for(type in c('whw','noov')){
    ##TODO make contingency table without prior knowledge of column names
    injury_table[[type]] <- expand.grid(year=unique(injury_list[[type]]$year),cas_mode=unique(injury_list[[type]]$cas_mode),
                                            strike_mode=unique(injury_list[[type]]$strike_mode),cas_age=AGE_CATEGORY,cas_gender=unique(injury_list[[type]]$cas_gender),stringsAsFactors = F)
    injury_table[[type]]$count <- apply(injury_table[[type]],1,function(x)nrow(subset(injury_list[[type]],year==as.numeric(x[1])&cas_mode==as.character(x[2])&
                                                                                                          strike_mode==as.character(x[3])&cas_gender==as.character(x[5])&
                                                                                                          cas_age>=AGE_LOWER_BOUNDS[which(AGE_CATEGORY==x[4])]&
                                                                                                          cas_age<AGE_LOWER_BOUNDS[which(AGE_CATEGORY==x[4])+1]))) 
    cas_mode_indices[[type]] <- match(injury_table[[type]]$cas_mode,mode_names)
    injury_gen_age[[type]] <- apply(cbind(injury_table[[type]]$cas_age,injury_table[[type]]$cas_gender),1,function(x)paste(x[c(2,1)],collapse='_'))
    injury_table[[type]]$injury_gen_age <- injury_gen_age[[type]]
  }
  strike_mode_indices <- match(injury_table$whw$strike_mode,mode_names)
  
  ## Calculated distances
  ## true distances should be the total for the whole population for a whole year. 
  ##TODO precalculate and save distances (for uncertainty use case)
  injuries_list <- list()
  for(scen in SCEN){
    injuries_list[[scen]] <- list()
    scen_dist <- subset(true_distances,scenario==scen)
    for(type in c('whw','noov')){
      injuries_list[[scen]][[type]] <- injury_table[[type]]
      ##TODO get distances without prior knowledge of column names
      ##TODO differentiate between driver and passenger for casualty and striker distances
      injuries_list[[scen]][[type]]$strike_distance <- 1
      injuries_list[[scen]][[type]]$strike_distance_sum <- 1
      distance_sums <- sapply(mode_names,function(x)sum(scen_dist[[x]]))
      injuries_list[[scen]][[type]]$cas_distance_sum <- distance_sums[cas_mode_indices[[type]]]
      
      cas_demo_indices <- match(injury_gen_age[[type]],scen_dist$sex_age)
      injuries_list[[scen]][[type]]$cas_distance <- as.numeric(as.data.frame(scen_dist)[cbind(cas_demo_indices,cas_mode_indices[[type]]+2)])
      
      if(type=='whw'){
        injuries_list[[scen]][[type]]$strike_distance <- distance_sums[strike_mode_indices]
        injuries_list[[scen]][[type]]$strike_distance_sum <- injuries_list[[scen]][[type]]$strike_distance
      }
      injuries_list[[scen]][[type]] <- subset(injuries_list[[scen]][[type]],strike_distance>0&cas_distance>0)
      
    }
  }
  reg_model <- list()
  ##TODO write formulae without prior knowledge of column names
  ##TODO different formulae for whw and noov
  for(type in c('whw','noov'))
    reg_model[[type]] <- glm(count~cas_mode+strike_mode+cas_age+cas_gender,data=injuries_list[[1]][[type]],family='poisson',
                   offset=log(cas_distance)+log(strike_distance)-0.5*log(strike_distance_sum)-0.5*log(cas_distance_sum))
  for(scen in SCEN)
    for(type in c('whw','noov')){
      injuries_list[[scen]][[type]] <- subset(injuries_list[[scen]][[type]],year==2016)
      injuries_list[[scen]][[type]]$pred <- predict(reg_model[[type]],newdata = injuries_list[[scen]][[type]],type='response')
    }
  injuries <- true_distances
  injuries$Bus_driver <- 0
  for(scen in SCEN)
    for(injured_mode in unique(injury_list$whw$cas_mode)){
      injuries[injuries$scenario==scen,match(injured_mode,colnames(injuries))] <- 
        apply(injuries[injuries$scenario==scen,] , 1, function(y)
            sum(subset(injuries_list[[scen]]$whw,cas_mode==injured_mode&injury_gen_age==as.character(y[10]))$pred) + 
              sum(subset(injuries_list[[scen]]$noov,cas_mode==injured_mode&injury_gen_age==as.character(y[10]))$pred)
            )
    }
  injuries$Deaths <- rowSums(injuries[,match(unique(injury_list$whw$cas_mode),colnames(injuries))])
  injuries
  ##TODO add in upcaptured fatalities as constant
}

injuries_function <- function(relative_distances,scen_dist){
  ### injury code
  ### This is the script for distance-based injury model for Accra using safety-in-numbers
  
  ##RJ match exponents and distances to multiply matrices
  ## TO DO: regression model with reporting rate 1/3 -- 1. Therefore, fix safety scalar to be either 0.5 or 1.
  whw_mat2 <- list()
  whw_mat <- data.frame(WHW_MAT)
  vic_order <- whw_mat[,1]
  strike_order <- colnames(whw_mat)[2:ncol(whw_mat)]
  sin_vic <- S.I.N[1:6,]
  sin_vic_ordered <- sin_vic[match(vic_order,data.frame(sin_vic)[,1]),match(strike_order,colnames(sin_vic))]
  sin_str <- S.I.N[7:12,]
  sin_str_ordered <- sin_str[match(vic_order,data.frame(sin_str)[,1]),match(strike_order,colnames(sin_str))]
  whw_mat_adjusted <- whw_mat[,2:8]*SAFETY_SCALAR
  for (k in 1:(length(SCEN_SHORT_NAME))) {
    victim_dist <- scen_dist[match(vic_order,rownames(scen_dist)),k]
    strk_dist <- scen_dist[match(strike_order,rownames(scen_dist)),k]
    victim_dist_mat <- t(repmat(victim_dist,length(strk_dist),1))
    strk_dist_mat <- repmat(strk_dist,length(victim_dist),1)
    whw_mat2[[k]] <- whw_mat_adjusted*(victim_dist_mat^sin_vic_ordered)*(strk_dist_mat^sin_str_ordered) 
  }
  
  ## get total injuries
  # names of victim types
  victim_deaths <- as.data.frame(WHW_MAT[,1])  
  # number of deaths in baseline by victim type
  victim_deaths <- cbind(victim_deaths, rowSums(whw_mat_adjusted))
  for (k in 2:(length(SCEN_SHORT_NAME))) victim_deaths <- cbind(victim_deaths, as.data.frame(rowSums(whw_mat2[[k]],na.rm=T))) 
  names(victim_deaths)[1] <- c("victim_type")
  names(victim_deaths)[2:(length(SCEN_SHORT_NAME)+1)] <- SCEN_SHORT_NAME
  
  ## distribute injuries to ages
  ##RJ match distances and injuries to multiply matrices
  dist_scen_indices <- match(relative_distances$scenario,SCEN)
  vic_scen_indices <- match(SCEN_SHORT_NAME[dist_scen_indices],colnames(victim_deaths))
  vic_mode_indices <- match(names(relative_distances)[2+1:nrow(victim_deaths)],victim_deaths[,1])
  injuries <- relative_distances
  ##!! hard-coding of indices
  injuries[,2+1:nrow(victim_deaths)] <- injuries[,2+1:nrow(victim_deaths)]*t(victim_deaths[vic_mode_indices,vic_scen_indices])
  
  injuries[,ncol(injuries)+1] <- rowSums(injuries[,2+1:nrow(victim_deaths)],na.rm=T)
  names(injuries)[ncol(injuries)] <-"Deaths"
  
  injuries
}

injury_death_to_yll <- function(injuries){
  
  joined_injury <- left_join(injuries, GBD_INJ_YLL[,c('sex_age','sex','yll_dth_ratio')], by="sex_age")
  
  joined_injury$YLL <- joined_injury$Deaths*joined_injury$yll_dth_ratio
  death_and_yll <- dplyr::select(joined_injury, c('age_cat','sex','scenario','Deaths','YLL'))
  
  x_deaths <- dplyr::select(death_and_yll, -YLL)
  x_deaths <- spread(x_deaths,scenario, Deaths)
  x_yll <- dplyr::select(death_and_yll, -Deaths)
  x_yll <- spread(x_yll,scenario, YLL)
  
  scen1_injuries <- list(deaths=x_deaths[,4],ylls=x_yll[,4])
  deaths <- x_deaths[,-4]
  deaths[,3:7] <- - deaths[,3:7] + t(repmat(unlist(scen1_injuries$deaths),NSCEN,1))
  ylls <- x_yll[,-4]
  ylls[,3:7] <- - ylls[,3:7] + t(repmat(unlist(scen1_injuries$ylls),NSCEN,1))
  
  deaths_yll_injuries <- as.data.frame(cbind(deaths, ylls[,-c(1:2)]))
  names(deaths_yll_injuries)[1:2]<- c("age_cat", "sex")
  
  metric <- c("deaths", "yll")
  k <- 1
  for  (i in 1: 2)
    for (j in c(1:(NSCEN+1))[-2]){
      names(deaths_yll_injuries)[2+k] <- paste0(SCEN_SHORT_NAME[j],"_",metric[i],"_inj")
      k<-k+1
    }
  
  list(deaths_yll_injuries=deaths_yll_injuries,scen1_injuries=scen1_injuries)
}

health_burden <- function(ind_ap_pa,inj){
  # subset gbd data for outcome types
  gbd_data_scaled <- GBD_DATA
  gbd_data_scaled$value_gama[gbd_data_scaled$cause%in%c("Neoplasms","Ischemic heart disease","Tracheal, bronchus, and lung cancer")] <- 
    gbd_data_scaled$value_gama[gbd_data_scaled$cause%in%c("Neoplasms","Ischemic heart disease","Tracheal, bronchus, and lung cancer")]*CHRONIC_DISEASE_SCALAR
  gbd_deaths <- subset(gbd_data_scaled,measure=='Deaths' & metric == "Number")
  gbd_ylls <- subset(gbd_data_scaled,measure=='YLLs (Years of Life Lost)' & metric == "Number")
  ##!! Hard-coded column names to initialise tables.
  sex_index <- which(colnames(ind_ap_pa)=='sex')
  age_index <- which(colnames(ind_ap_pa)=='age_cat')
  unique_category1 <- unique(ind_ap_pa[[sex_index]])
  unique_category2 <- unique(ind_ap_pa[[age_index]])
  pop_details <- expand.grid(unique_category1, unique_category2,stringsAsFactors = F)
  colnames(pop_details) <- colnames(ind_ap_pa)[c(sex_index,age_index)]
  deaths <- deaths_red <- ylls <- ylls_red <- pop_details
  # set up reference (scen1)
  reference_scenario <- 'scen1'
  scen_names <- SCEN_SHORT_NAME[SCEN_SHORT_NAME!=reference_scenario]
  ### iterating over all all disease outcomes
  for ( j in 1:nrow(DISEASE_OUTCOMES)){
    # Disease acronym and full name
    ac <- as.character(DISEASE_OUTCOMES$acronym[j])
    gbd_dn <- as.character(DISEASE_OUTCOMES$GBD_name[j])
    # set up column names
    middle_bit <-
      paste0(
        ifelse(DISEASE_OUTCOMES$physical_activity[j] == 1, 'pa_', ''),
        ifelse(DISEASE_OUTCOMES$air_pollution[j] == 1, 'ap_', '')
      )
    base_var <- paste0('RR_', middle_bit, reference_scenario, '_', ac)
    scen_vars <- paste0('RR_', middle_bit, scen_names, '_', ac)
    # subset gbd data
    gbd_deaths_disease <- subset(gbd_deaths,cause==gbd_dn)
    gbd_ylls_disease <- subset(gbd_ylls,cause==gbd_dn)
    # set up pif tables
    pif_ref <-
      population_attributable_fraction(pop = ind_ap_pa[, colnames(ind_ap_pa) %in% c(base_var, 'sex', 'age_cat')], cn = base_var, mat =
            pop_details)
    yll_ref <-
      combine_health_and_pif(pop = pop_details,
                             pif_values = pif_ref,
                             hc = gbd_ylls_disease)
    death_ref <-
      combine_health_and_pif(pop = pop_details,
                             pif_values = pif_ref,
                             hc = gbd_ylls_disease)
    for (index in 1:length(scen_vars)){
      # set up naming conventions
      scen <- scen_names[index]
      scen_var <- scen_vars[index]
      yll_name <- paste0(scen, '_ylls_',middle_bit,ac)
      deaths_name <- paste0(scen, '_deaths_',middle_bit,ac)
      # Calculate PIFs for selected scenario
      pif_temp <- population_attributable_fraction(pop = ind_ap_pa[,colnames(ind_ap_pa)%in%c(scen_var,'sex', 'age_cat')], cn = scen_var, mat=pop_details)
      pif_scen <- (pif_ref - pif_temp) / pif_ref
      # Calculate ylls 
      yll_dfs <- combine_health_and_pif(pop=pop_details,pif_values=pif_scen, hc = gbd_ylls_disease)
      ylls[[yll_name]] <- yll_dfs
      # Calculate deaths 
      death_dfs <- combine_health_and_pif(pop=pop_details,pif_values=pif_scen,hc=gbd_deaths_disease)
      deaths[[deaths_name]] <- death_dfs
    }
  }
  # Select deaths columns
  inj_deaths <- dplyr::select(inj, c(age_cat, sex, contains("deaths")))
  # Select yll columns
  inj_ylls <- dplyr::select(inj, c(age_cat, sex, contains("yll")))
  # Join injuries data to global datasets
  deaths <- left_join(deaths, inj_deaths, by = c("age_cat", "sex"))
  ylls <- left_join(ylls, inj_ylls, by = c("age_cat", "sex"))
  list(deaths=deaths,ylls=ylls)
}

population_attributable_fraction <- function(pop, cn, mat){
  ##!! hard coding of indices: 1=sex, 2=age or age_cat
  paf <- apply(mat,1,function(x)sum(pop[[cn]][pop[[1]]==x[1]&pop[[2]]==x[2]]))
  paf
}

combine_health_and_pif <- function(pop, pif_values, hc=GBD_DATA, hm_cn = 'value_gama'){
  # pif_values are already ordered as in pop; reorder hc values to match.
  hm_cn_values <- hc[[hm_cn]]
  return_values <- c()
  for (new_row in 1:nrow(pop))
    return_values[new_row] <- hm_cn_values[hc$sex == pop$sex[new_row] & hc$age ==  pop$age_cat[new_row] ]
  return_values <- return_values * pif_values
  round(return_values,5)
}

######################################################################
## RUN FUNCTIONS
ithim_uncertainty <- function(ithim_object,seed=1){ 
  ############################
  for(i in 1:length(ithim_object))
    assign(names(ithim_object)[i],ithim_object[[i]])
  # Get parameters
  for(i in 1:length(parameters))
    assign(names(parameters)[i],parameters[[i]][[seed]],pos=1)
  ## Re-do set up if BUS_WALK_TIME or MC_TO_CAR_RATIO has changed
  if(RECALCULATE_TRIPS){
    set_vehicle_inventory()
    get_synthetic_from_trips()
  }
  
  ## calculate distances, if distances are not variable dependent
  if(RECALCULATE_DISTANCES){
    ithim_object <- get_all_distances(ithim_object)
  }
  ############################
  # Run ITHIM cascade of functions
  run_results <- run_ithim(ithim_object,seed)
  run_results$dist <- ithim_object$dist
  run_results$dur <- ithim_object$dur
  #return(run_results)
  ##!! RJ for now return only hb from uncertain simulations; otherwise the file is too big
  return(list(hb=run_results$hb))
}

run_ithim <- function(ithim_object,seed=1){ 
  ############################
  ## (0) SET UP
  set.seed(seed)
  for(i in 1:length(ithim_object))
    assign(names(ithim_object)[i],ithim_object[[i]])
  ############################
  ## (1) AP PATHWAY
  # Calculated PM2.5 concentrations
  (pm_conc <- scenario_pm_calculations(dist,trip_scen_sets))
  scenario_pm <- pm_conc$scenario_pm
  pm_conc_pp <- pm_conc$pm_conc_pp
  # Air pollution calculation
  (RR_AP_calculations <- gen_ap_rr(pm_conc_pp))
  ############################
  ## (2) PA PATHWAY
  # Calculate total mMETs
  (mmets_pp <- total_mmet(trip_scen_sets))
  # Physical activity calculation
  (RR_PA_calculations <- gen_pa_rr(mmets_pp))
  ############################
  ## (3) COMBINE (1) AND (2)
  # Physical activity and air pollution combined
  (RR_PA_AP_calculations <- combined_rr_pa_pa(RR_PA_calculations,RR_AP_calculations))
  ############################
  ## (4) INJURIES
  # Injuries calculation
  for(i in 1:length(inj_distances))
    assign(names(inj_distances)[i],inj_distances[[i]])
  injuries <- injuries_function(relative_distances,scen_dist)
  #injuries <- injuries_function_2(true_distances)
  (deaths_yll_injuries <- injury_death_to_yll(injuries))
  scen1_injuries <- deaths_yll_injuries$scen1_injuries
  ############################
  ## (5) COMBINE (3) AND (4)
  # Combine health burden from disease and injury
  (hb <- health_burden(RR_PA_AP_calculations,deaths_yll_injuries$deaths_yll_injuries))
  return(list(mmets=mmets_pp,scenario_pm=scenario_pm,pm_conc_pp=pm_conc_pp,injuries=injuries,scen1_injuries=scen1_injuries,hb=hb))
}

parallel_evppi_for_AP <- function(disease,parameter_samples,outcome){
  AP_DOSE_RESPONSE_QUANTILE <- c()
  x1 <- parameter_samples[,which(colnames(parameter_samples)==paste0('AP_DOSE_RESPONSE_QUANTILE_ALPHA_',disease))];
  x2 <- parameter_samples[,which(colnames(parameter_samples)==paste0('AP_DOSE_RESPONSE_QUANTILE_BETA_',disease))];
  x3 <- parameter_samples[,which(colnames(parameter_samples)==paste0('AP_DOSE_RESPONSE_QUANTILE_GAMMA_',disease))];
  x4 <- parameter_samples[,which(colnames(parameter_samples)==paste0('AP_DOSE_RESPONSE_QUANTILE_TMREL_',disease))];
  for(j in 1:(NSCEN)){
    y <- outcome[, j+5] ## +5 means we choose ihd outcome for each scenario
    vary <- var(y)
    model <- gam(y ~ te(x1,x2,x3,x4))
    AP_DOSE_RESPONSE_QUANTILE[j] <- (vary - mean((y - model$fitted) ^ 2)) / vary * 100 
  }
  AP_DOSE_RESPONSE_QUANTILE
}


