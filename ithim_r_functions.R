##RJ question for LG/AA/RG
## How many 'age_cat's should there be? How should they be defined for the different parts of the model? 
## E.g., PA has different categories to everything else.

##RJ question for LG/AA/RG
## in lots of places we write
# Remove short walking, 99, Train, Other and Unspecified modes
## do we want to remove any of these from the raw dataset that gets passed around?

##RJ question for/discussion with AA. 
## Functions ithim_setup_global_values, ithim_setup_parameters, ithim_load_data, set_scenario_specific_variables
## Global variables (i.e. those needed by all functions) are assigned to all environments, using 
## <<- and assign(...,pos=1), and denoted by capitals to make it clear what they are. 
## A better method might be to make an object (list) that contains all inputs, variables, and intermediate objects.

run_ithim_setup <- function(plotFlag = F,
                            NSAMPLES = 1,
                            CITY = 'Accra',
                            modes = c("Bus", "Private Car", "Taxi", "Walking","Short Walking", "Bicycle", "Motorcycle"),
                            speeds = c(15, 21, 21, 4.8, 4.8, 14.5, 25),
                            DIST_CAT = c("0-6 km", "7-9 km", "10+ km"),
                            AGE_CATEGORY = c("15-49", "50-69", "70+"),
                            MEAN_BUS_WALK_TIME= 5,
                            MMET_CYCLING = 4.63,
                            MMET_WALKING = 2.53,
                            PM_CONC_BASE = 50,  
                            PM_TRANS_SHARE = 0.225,
                            PA_DOSE_RESPONSE_QUANTILE = F,
                            AP_DOSE_RESPONSE_QUANTILE = F,
                            BACKGROUND_PA_SCALAR = 1,
                            SAFETY_SCALAR = 1,
                            CHRONIC_DISEASE_SCALAR = 1 ){
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
  
  #################################################
  
  ithim_object <- list()
  
  #ithim_setup_global_values(plotFlag,NSAMPLES,modes,speeds,DIST_CAT,AGE_CATEGORY)
  ## SET GLOBAL VALUES
  ## PROGRAMMING VARIABLES
  plotFlag <<- plotFlag
  NSAMPLES <<- NSAMPLES
  
  ## MODEL VARIABLES
  CITY <<- CITY
  MODE_SPEEDS <<- data.frame(trip_mode = modes, speed = speeds, stringsAsFactors = F)
  DIST_CAT <<- DIST_CAT
  DIST_LOWER_BOUNDS <<- as.numeric(sapply(strsplit(DIST_CAT, "[^0-9]+"), function(x) x[1]))
  AGE_CATEGORY <<- AGE_CATEGORY
  AGE_LOWER_BOUNDS <<- as.numeric(sapply(strsplit(AGE_CATEGORY, "[^0-9]+"), function(x) x[1]))
  
  ithim_load_data()
  
  ithim_object$parameters <- ithim_setup_parameters(NSAMPLES,
                                                    MEAN_BUS_WALK_TIME,
                                                    MMET_CYCLING,
                                                    MMET_WALKING,
                                                    PM_CONC_BASE,  
                                                    PM_TRANS_SHARE,
                                                    PA_DOSE_RESPONSE_QUANTILE,
                                                    AP_DOSE_RESPONSE_QUANTILE,
                                                    BACKGROUND_PA_SCALAR,
                                                    SAFETY_SCALAR,
                                                    CHRONIC_DISEASE_SCALAR )
  
  if(CITY=='Accra') edit_accra_trips()
  RD <<- create_synth_pop()
  
  ##RJ these distance calculations are currently not parameter dependent, which means we can make the calculation outside the function.
  ## We could either integrate them into another external function, or move them to the internal function, so that they can become variable.
  if(!'MEAN_BUS_WALK_TIME'%in%names(ithim_object$parameters)){
    rd <- ithim_setup_baseline_scenario()
    ithim_object$bs <- create_all_scenarios(rd)
    set_scenario_specific_variables(ithim_object$bs)
    # Generate distance and duration matrices
    dist_and_dir <- dist_dur_tbls(ithim_object$bs)
    ithim_object$dist <- dist_and_dir$dist
    ithim_object$dur <- dist_and_dir$dur
    # distances for injuries calculation
    ithim_object$inj_distances <- distances_for_injury_function(ithim_object$bs)
  }
  ######################
  return(ithim_object)
}

##RJ question for AA/RG/LG
## what are all the parameters we might like to change?
## which functions do they affect?
## do we want to pre-specify the distribution, as below?
## if yes, which parameters and what distributions?
ithim_setup_parameters <- function(NSAMPLES = 1,
                                   MEAN_BUS_WALK_TIME= 5,
                                   MMET_CYCLING = 4.63,
                                   MMET_WALKING = 2.53,
                                   PM_CONC_BASE = 50,  
                                   PM_TRANS_SHARE = 0.225,
                                   PA_DOSE_RESPONSE_QUANTILE = F,
                                   AP_DOSE_RESPONSE_QUANTILE = F,
                                   BACKGROUND_PA_SCALAR = 1,
                                   SAFETY_SCALAR = 1,
                                   CHRONIC_DISEASE_SCALAR = 1 ){
  ## PARAMETERS
  ##RJ parameters are assigned to the environment and so are set for every function. They are over-written when sample_parameters is called.
  parameters <- list()
  if(length(MEAN_BUS_WALK_TIME) == 1 ) {
    MEAN_BUS_WALK_TIME <<- MEAN_BUS_WALK_TIME
  }else{
    parameters$MEAN_BUS_WALK_TIME <- rlnorm(NSAMPLES,MEAN_BUS_WALK_TIME[1], MEAN_BUS_WALK_TIME[2])
  }
  if(length(MMET_CYCLING) == 1 ) {
    MMET_CYCLING <<- MMET_CYCLING
  }else{
    parameters$MMET_CYCLING <- rlnorm(NSAMPLES,MMET_CYCLING[1], MMET_CYCLING[2])
  }
  if(length(MMET_WALKING) == 1 ) {
    MMET_WALKING <<- MMET_WALKING
  }else{
    parameters$MMET_WALKING <- rlnorm(NSAMPLES,MMET_WALKING[1], MMET_WALKING[2])
  }
  if(length(PM_CONC_BASE) == 1 ) {
    PM_CONC_BASE <<- PM_CONC_BASE
  }else{
    parameters$PM_CONC_BASE <- rlnorm(NSAMPLES,PM_CONC_BASE[1],PM_CONC_BASE[2])
  }
  if(length(PM_TRANS_SHARE) == 1 ) {
    PM_TRANS_SHARE <<- PM_TRANS_SHARE
  }else{
    parameters$PM_TRANS_SHARE <- rbeta(NSAMPLES,PM_TRANS_SHARE[1],PM_TRANS_SHARE[2])
  }
  if(length(BACKGROUND_PA_SCALAR) == 1 ) {
    BACKGROUND_PA_SCALAR <<- BACKGROUND_PA_SCALAR
  }else{
    parameters$BACKGROUND_PA_SCALAR <- rlnorm(NSAMPLES,BACKGROUND_PA_SCALAR[1],BACKGROUND_PA_SCALAR[2])
  }
  if(length(SAFETY_SCALAR) == 1 ) {
    SAFETY_SCALAR <<- SAFETY_SCALAR
  }else{
    parameters$SAFETY_SCALAR <- rlnorm(NSAMPLES,SAFETY_SCALAR[1],SAFETY_SCALAR[2])
  }
  if(length(CHRONIC_DISEASE_SCALAR) == 1 ) {
    CHRONIC_DISEASE_SCALAR <<- CHRONIC_DISEASE_SCALAR
  }else{
    parameters$CHRONIC_DISEASE_SCALAR <- rlnorm(NSAMPLES,CHRONIC_DISEASE_SCALAR[1],CHRONIC_DISEASE_SCALAR[2])
  }
  if(PA_DOSE_RESPONSE_QUANTILE == F ) {
    PA_DOSE_RESPONSE_QUANTILE <<- PA_DOSE_RESPONSE_QUANTILE
  }else{
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
      for(age in unique(dr_ap$age_code)){
        dr_ap_age <- subset(dr_ap,age_code==age)
        #######################################
        ##RJ I recommend the following as a better approximation to the distribution but it is currently  v e r y  slow
        ## so I leave it here commented out until we want to develop it and/or use it
        #lbeta <- log(dr_ap_age$beta)
        #lgamma <- log(dr_ap_age$gamma)
        #gamma_val <- quantile(density(lgamma),quant1)
        #beta_val <- c()
        #for(i in 1:n){
        #  den <- kde2d(lgamma,lbeta,n=c(1,100),h=0.2,lims=c(gamma_val[i],gamma_val[i],min(lbeta)-1,max(lbeta)+1))
        #  beta_val[i] <- approx(x=cumsum(den$z)/sum(den$z),y=den$y,xout=quant2[i])$y
        #}
        #mod <- gam(log(alpha)~te(log(gamma),log(beta)),data=dr_ap_age)
        #pred_val <- predict(mod, newdata=data.frame(beta=exp(beta_val),gamma=exp(gamma_val)),se.fit=T)
        #alpha_val <- qnorm(quant3,pred_val$fit,sqrt(mod$sig2))
        #mod <- gam(log(tmrel)~ns(log(gamma),df=8)+ns(log(beta),df=8)+ns(log(alpha),df=8),data=dr_ap_age)
        #pred_val <- predict(mod, newdata=data.frame(alpha=exp(alpha_val),beta=exp(beta_val),gamma=exp(gamma_val)),se.fit=T)
        #tmrel_val <- qnorm(quant4,pred_val$fit,sqrt(mod$sig2))
        #dr_ap_list[[cause]][[age]] <- data.frame(alpha=exp(alpha_val),beta=exp(beta_val),gamma=exp(gamma_val),tmrel=exp(tmrel_val))
        #######################################
        
        # generate a value for alpha
        alpha_val <- quantile(log(dr_ap_age$alpha),parameters[[paste0('AP_DOSE_RESPONSE_QUANTILE_ALPHA_',disease)]])
        # generate a value for beta given alpha
        mod <- gam(log(beta)~ns(log(alpha),df=8),data=dr_ap_age)
        pred_val <- predict(mod, newdata=data.frame(alpha=exp(alpha_val)),se.fit=T)
        beta_val <- qnorm(parameters[[paste0('AP_DOSE_RESPONSE_QUANTILE_BETA_',disease)]],pred_val$fit,sqrt(mod$sig2))
        # generate a value for gamma given beta and alpha
        mod <- gam(log(gamma)~ns(log(beta),df=8)+ns(log(alpha),df=8),data=dr_ap_age)
        pred_val <- predict(mod, newdata=data.frame(alpha=exp(alpha_val),beta=exp(beta_val)),se.fit=T)
        gamma_val <- qnorm(parameters[[paste0('AP_DOSE_RESPONSE_QUANTILE_GAMMA_',disease)]],pred_val$fit,sqrt(mod$sig2))
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
  ind <- read_csv("data/synth_pop_data/accra/raw_data/trips/trips_Accra.csv")
  ind$participant_id <- as.numeric(as.factor(ind$participant_id))
  TRIP_SET <<- ind
  PA_SET <<- read_csv("data/synth_pop_data/accra/raw_data/PA/pa_Accra.csv")
  trans_emissions_file <- read_csv("data/emission calculations accra/transport_emission_inventory_accra.csv")
  names(trans_emissions_file) <- c("vehicle_type", "delhi_fleet_2011", "delhi_fleet_perHH", "accra_fleet_2010", "PM2_5_emiss_fact", "base")
  ##!! RJ these emissions are scaled from Delhi. They multiply distance and emission factors. We ought to (a) load emission factors, and (b) calculate total distance by mode to replace this.
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

edit_accra_trips <- function(){
  
  ind <- TRIP_SET
  nPeople <- 4
  nTrips <- 3
  new_mode <- 'Motorcycle'
  new_gender <- 'Male'
  distance_range <- c(15,100)
  
  new_trips <- data.frame(trip_id = c( (max(ind$trip_id) + 1):(max(ind$trip_id) + (nPeople * nTrips) )), 
                          trip_mode = new_mode, 
                          trip_duration = round(runif( (nPeople * nTrips), distance_range[1], distance_range[2])), 
                          participant_id = rep((max(ind$trip_id)+1):(max(ind$trip_id) + nPeople), nTrips),
                          age = rep(floor(runif(nPeople, AGE_LOWER_BOUNDS[1], AGE_LOWER_BOUNDS[2])), nTrips),
                          sex = new_gender)
  
  # Add new motorbikes trips to baseline
  ind <- rbind(ind, new_trips)
  
  # Redefine motorcycle mode for a select 14 rows
  ind$trip_mode[ind$trip_mode=='Other'&ind$trip_duration<60] <- 'Motorcycle'
  
  # Multiply ind by 4 to have a bigger number of trips (and ind)
  ind1 <- ind
  ind1$participant_id <- ind1$participant_id + max(ind$participant_id)
  ind1$trip_id <- (max(ind$trip_id) + 1): (max(ind$trip_id) + nrow(ind1))
  ind <- rbind(ind, ind1)
  
  ind1 <- ind
  ind1$participant_id <- ind1$participant_id + max(ind$participant_id)
  ind1$trip_id <- (max(ind$trip_id) + 1): (max(ind$trip_id) + nrow(ind1))
  ind <- rbind(ind, ind1)
  
  TRIP_SET <<- ind
  
}

create_synth_pop <- function(){
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
  
  # Create new motorbike trips
  # Add 4 new people with 3 trips each
  # Age: 15-59 and gender: male
  
  ind <- TRIP_SET
  
  #Make age category for ind dataset.
  # Make age category
  ##!! assuming more than one age category
  ind$age_cat <- 0
  for(i in 2:length(AGE_LOWER_BOUNDS)-1){
    ind$age_cat[ind$age >= AGE_LOWER_BOUNDS[i] & ind$age < AGE_LOWER_BOUNDS[i+1]] <- AGE_CATEGORY[i]
  }
  ind$age_cat[ind$age >= AGE_LOWER_BOUNDS[length(AGE_LOWER_BOUNDS)]] <- AGE_CATEGORY[length(AGE_LOWER_BOUNDS)]
  ind <- filter(ind, age_cat != AGE_CATEGORY[length(AGE_LOWER_BOUNDS)])
  
  ##!! RJ question for AA/LG: why the different age categories?
  #Make age category for pa dataset.
  pa <- PA_SET
  pa <- filter(pa, age < 70)
  age_category <- c("15-55", "56-69")
  pa$age_cat <- 0
  pa$age_cat[pa$age >= 15 & pa$age <= 55] <- age_category[1]
  pa$age_cat[pa$age > 55 & pa$age < 70] <- age_category[2]
  
  #Match persons in the trip (ind) e physical activity datasets.
  column_to_keep <- which(colnames(pa)%in%c('work_ltpa_marg_met'))
  unique_ages <- unique(ind$age_cat)
  unique_genders <- unique(ind$sex)
  
  temp <- c()
  for(age_group in unique_ages){
    for(gender in unique_genders){
      i <- unique(subset(ind,age_cat==age_group&sex==gender)$participant_id)
      pa_age_category <- age_category[which(AGE_CATEGORY==age_group)]
      matching_people <- filter(pa, age_cat == pa_age_category & sex == gender)[,column_to_keep]
      v <- (matching_people[sample(nrow(matching_people),length(i),replace=T),])
      temp <- rbind( temp, cbind(v,i) )
    }
  }
  
  namevector <- c(colnames(pa)[column_to_keep], "participant_id")
  colnames(temp) <- namevector
  temp <- as.data.frame (temp)
  
  ind <- left_join(ind, temp, "participant_id")
  
  # Convert all int columns to numeric
  ind[, sapply(ind,class)=='integer'] <- lapply(ind[, sapply(ind,class)=='integer'], as.numeric)
  
  ind$trip_id[ind$trip_mode == '99'] <- 0
  
  rd <- ind
  rd
  
}

ithim_setup_baseline_scenario <- function(){
  ## SET UP TRAVEL DATA
  rd <- RD
  # Create a row id
  rd$rid <- 1:nrow(rd)
  
  # Define trip_distances (in km)
  # Based on travel mode and trip duration, calculate distances
  
  rd <- left_join(rd, MODE_SPEEDS, by = "trip_mode")
  rd$speed[is.na(rd$speed)] <- 0
  rd$trip_distance <- (rd$trip_duration / 60) * rd$speed
  
  # Initialize them
  ## Distance categories are used in scenario generation. They correspond to e.g. ``long trips'' and ``short trips''
  rd$trip_distance_cat <- 0
  ##!! assuming more than one distance category
  for(i in 2:length(DIST_LOWER_BOUNDS)-1){
    rd$trip_distance_cat[rd$trip_distance >= DIST_LOWER_BOUNDS[i] & rd$trip_distance < DIST_LOWER_BOUNDS[i+1]] <- DIST_CAT[i]
  }
  rd$trip_distance_cat[rd$trip_distance >= DIST_LOWER_BOUNDS[length(DIST_LOWER_BOUNDS)]] <- DIST_CAT[length(DIST_LOWER_BOUNDS)]
  
  ##RJ should not need to do ages as age_cat already exists in the synthetic population. 70+ people already filtered out also.
  # Make age category
  ##!! assuming more than one age category
  #for(i in 2:length(AGE_LOWER_BOUNDS)-1){
  #  rd$age_cat[rd$age >= AGE_LOWER_BOUNDS[i] & rd$age < AGE_LOWER_BOUNDS[i+1]] <- AGE_CATEGORY[i]
  #}
  #rd$age_cat[rd$age >= AGE_LOWER_BOUNDS[length(AGE_LOWER_BOUNDS)]] <- AGE_CATEGORY[length(AGE_LOWER_BOUNDS)]
  # Remove all participants greater than 70 years of age
  ##!! 
  #rd <- filter(rd, age_cat != AGE_CATEGORY[length(AGE_LOWER_BOUNDS)])
  
  rd$scenario <- "Baseline"
  
  ##RJ question for AA: do we want to add walking to train?
  bus_walk_trips <- add_walk_trips(filter(rd, trip_mode == "Bus"), ln_mean = MEAN_BUS_WALK_TIME, ln_sd = 1.2)
  
  rd[rd$trip_mode == 'Bus' & rd$rid %in% bus_walk_trips[[1]]$rid,]$trip_duration <- bus_walk_trips[[1]]$trip_duration
  rd[rd$trip_mode == 'Bus' & rd$rid %in% bus_walk_trips[[1]]$rid,]$trip_distance <- bus_walk_trips[[1]]$trip_distance
  rd[rd$trip_mode == 'Bus' & rd$rid %in% bus_walk_trips[[1]]$rid,]$trip_distance_cat <- bus_walk_trips[[1]]$trip_distance_cat
  
  rd <- rbind(rd, bus_walk_trips[[2]])
  
  rd$row_id <- 1:nrow(rd)

  rd
}

set_scenario_specific_variables <- function(rd){
  NSCEN <<- length(unique(rd$scenario)) - 1
  SCEN <<- unique(rd$scenario)
  SCEN_SHORT_NAME <<- c("base",paste0("scen", 1:NSCEN) )
}

add_walk_trips <- function(bus_trips, ln_mean, ln_sd){
  
  # bus_trips
  # ln_mean = 5
  # ln_sd = 1.2
  
  bus_trips <- arrange(bus_trips, trip_duration)
  walk_trips <- bus_trips
  walk_trips$trip_mode <- 'Short Walking'
  ##RJ all trips have the same MEAN_BUS_WALK_TIME
  walk_trips$trip_duration <- MEAN_BUS_WALK_TIME#sort(rlnorm(n = nrow(bus_trips), meanlog = log(MEAN_BUS_WALK_TIME), sdlog = log(ln_sd)))
  
  # Replace walk trips with duration greater than that of bus needs to be set to 0
  if (nrow(walk_trips[(walk_trips$trip_duration - bus_trips$trip_duration)  > 0,]) > 0)
    walk_trips[(walk_trips$trip_duration - bus_trips$trip_duration)  > 0,]$trip_duration <- 0
  
  bus_trips$trip_duration <- bus_trips$trip_duration - walk_trips$trip_duration
  
  # Corrrect walk trips distance
  walk_trips$trip_distance <- (walk_trips$trip_duration / 60) * MODE_SPEEDS$speed[MODE_SPEEDS$trip_mode=='Walking']
  bus_trips$trip_distance <- (bus_trips$trip_duration / 60 ) * MODE_SPEEDS$speed[MODE_SPEEDS$trip_mode=='Bus']
  
  # Recategorise trip_distance_cat for both bus and walk trips
  bus_trips$trip_distance_cat[bus_trips$trip_distance > 0 & bus_trips$trip_distance < DIST_LOWER_BOUNDS[2]] <- DIST_CAT[1]
  bus_trips$trip_distance_cat[bus_trips$trip_distance >= DIST_LOWER_BOUNDS[2] & bus_trips$trip_distance < DIST_LOWER_BOUNDS[3]] <- DIST_CAT[2]
  bus_trips$trip_distance_cat[bus_trips$trip_distance >= DIST_LOWER_BOUNDS[3]] <- DIST_CAT[3]
  
  
  walk_trips$trip_distance_cat[walk_trips$trip_distance > 0 & walk_trips$trip_distance < DIST_LOWER_BOUNDS[2]] <- DIST_CAT[1]
  walk_trips$trip_distance_cat[walk_trips$trip_distance >= DIST_LOWER_BOUNDS[2] & walk_trips$trip_distance < DIST_LOWER_BOUNDS[3]] <- DIST_CAT[2]
  walk_trips$trip_distance_cat[walk_trips$trip_distance >= DIST_LOWER_BOUNDS[3]] <- DIST_CAT[3]
  
  return(list(bus_trips, walk_trips))
  
}

create_scenario <- function(rdr, scen_name, source_modes, combined_modes = F, target_modes, source_distance_cats, 
                            source_trips, target_trips){
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
          trip_duration = (trip_distance * 60) / MODE_SPEEDS[MODE_SPEEDS$trip_mode == target_modes[i], ]$speed
        )
      
      
      # Update selected rows for mode and duration
      rdr[rdr$row_id %in% sample$row_id,]$trip_mode <- sample$trip_mode
      rdr[rdr$row_id %in% sample$row_id,]$trip_duration <- sample$trip_duration
      
      
      if (source_modes[i] == 'Bus'){
        # Remove bus associated short walking trips that have been changed to Private Car trips
        rdr <- rdr[!(rdr$trip_mode == 'Short Walking' & rdr$trip_id %in% sample$trip_id),]
      }
      
      
    } 
  }
  
  else {
    
    
    sample <- filter(rdr,
                     trip_mode %in% source_modes &
                       trip_distance_cat %in% source_distance_cats) %>% sample_n(source_trips[1]) %>%
      mutate(
        trip_mode = target_modes[1],
        trip_duration = (trip_distance * 60) / MODE_SPEEDS[MODE_SPEEDS$trip_mode == target_modes[1], ]$speed
      )
    
    sample$scenario <- scen_name
    
    return(sample)
  }
  
  
  rdr$scenario <- scen_name
  
  return(rdr)
  
}

create_all_scenarios <- function(rd){
  rd_list <- list()
  rd_list[[1]] <- rd
  # Scenario 1
  
  rdr <- rd
  
  source_modes <- c('Bus', 'Walking')
  target_modes <- c('Private Car')
  
  source_percentages <- c(0.16, 0.49)
  
  tt <- nrow(filter(rdr, ! trip_mode %in% c('99', 'Short Walking')))
  
  rdr <- create_scenario(rdr, scen_name = 'Scenario 1', source_modes = source_modes, 
                         target_modes = target_modes, source_distance_cats = DIST_CAT, 
                         source_trips = c(round(source_percentages[1] * tt), 
                                          round(source_percentages[2] * tt)))
  
  #rdfinal <- rbind(rd, rdr)
  rd_list[[2]] <- rdr
  
  ###############################################################
  # Scenario 2
  
  rdr <- rd_list[[2]]#filter(rdfinal, scenario == 'Scenario 1')
  
  # 35 % Bus
  
  tt <- nrow(filter(rdr,! trip_mode %in% c('99', 'Short Walking')))
  
  source_modes <- c('Private Car', 'Taxi')
  target_modes <- c('Bus')
  
  target_new_trips <- c(round(0.35 * tt) - nrow(filter(rdr, trip_mode == 'Bus')))
  
  total_car_trips <- filter(rdr, (trip_mode %in% c(source_modes[1], source_modes[2])))
  
  
  t_dc <- total_car_trips %>% group_by(trip_distance_cat) %>% summarise(count = dplyr::n())
  
  long_trips <- sum(t_dc[t_dc$trip_distance_cat != DIST_CAT[1],]$count)
  
  long_car_trips_sample <- create_scenario(total_car_trips, scen_name = 'Scenario 2', source_modes = source_modes, combined_modes = T, 
                                           target_modes = target_modes, source_distance_cats = c(DIST_CAT[2], DIST_CAT[3]), 
                                           source_trips = c(long_trips))
  
  short_car_trips_sample <- create_scenario(total_car_trips, scen_name = 'Scenario 2', source_modes = source_modes, combined_modes = T, 
                                            target_modes = target_modes, source_distance_cats = DIST_CAT[1], 
                                            source_trips = c(target_new_trips[1] - long_trips))
  
  car_trips_sample <- rbind(long_car_trips_sample, short_car_trips_sample)
  
  ##  ADDING SHORT WALK TRIPS FOR NEW BUS TRIPS
  
  # Divide bus trips into bus and walk trips
  bus_trips <- car_trips_sample
  
  bus_walk_trips <- add_walk_trips(bus_trips, ln_mean = MEAN_BUS_WALK_TIME, ln_sd = 1.2)
  
  # Update selected rows for mode and duration
  rdr[rdr$row_id %in% bus_walk_trips[[1]]$row_id,]$trip_mode <- bus_walk_trips[[1]]$trip_mode
  rdr[rdr$trip_mode == 'Bus' & rdr$rid %in% bus_walk_trips[[1]]$rid,]$trip_duration <- bus_walk_trips[[1]]$trip_duration
  rdr[rdr$trip_mode == 'Bus' & rdr$rid %in% bus_walk_trips[[1]]$rid,]$trip_distance <- bus_walk_trips[[1]]$trip_distance
  rdr[rdr$trip_mode == 'Bus' & rdr$rid %in% bus_walk_trips[[1]]$rid,]$trip_distance_cat <- bus_walk_trips[[1]]$trip_distance_cat
  
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
  
  #rdfinal <- rbind(rdfinal, rdr)
  rd_list[[6]] <- rdr
  
  
  do.call('rbind',rd_list)
}

distances_for_injury_function <- function(rd){
  
  journeys <- filter(rd,!is.na(trip_id), !trip_mode%in%c(99,"Train","Unspecified","Other")) %>% 
    group_by (age_cat,sex,trip_mode, scenario) %>% 
    summarise(tot_dist = sum(trip_distance))
  distances <- spread(journeys,trip_mode, tot_dist,fill=0) 
  distances$Pedestrian <- distances$Walking + distances$`Short Walking`
  distances <- distances[, -which(names(distances) ==  "Walking")]
  distances <- distances[, -which(names(distances) ==  "Short Walking")]
  distances$Car <- distances$Taxi + distances$`Private Car`
  distances <- distances[, -which(names(distances) ==  "Private Car")]
  distances <- distances[, -which(names(distances) ==  "Taxi")]
  scen_dist <- sapply(1:(NSCEN+1),function(x)c(colSums(subset(distances,scenario == SCEN[x])[,4:8])))
  colnames(scen_dist) <- SCEN_SHORT_NAME
  for(i in 2:6) scen_dist[,i] <- scen_dist[,i]/scen_dist[,1] 
  scen_dist <- rbind(scen_dist,Truck=1,Tuktuk=1)
  
  mode_names <- names(distances)[4:8]
  for (i in 1: length(mode_names))
    for (n in 1:(NSCEN+1))
      distances[[mode_names[i]]][which(distances$scenario == unique(rd$scenario)[n])] <- 
    distances[[mode_names[i]]][which(distances$scenario == unique(rd$scenario)[n])]/ sum(distances[[mode_names[i]]][which(distances$scenario == unique(rd$scenario)[n])],na.rm=T)
  relative_distances <- distances
  relative_distances$sex_age <-  paste0(relative_distances$sex,"_",relative_distances$age_cat)
  relative_distances <- relative_distances[,-c(which(names(relative_distances) == 'sex'))]
  
  list(relative_distances=relative_distances,scen_dist=scen_dist)
}

dist_dur_tbls <- function(bs){
  
  bs <- filter(bs, !trip_mode %in% c("99", "Train", "Other", "Unspecified")&!is.na(trip_mode))
  
  ##RJ only calculate if plotFlag==T
  if(plotFlag){
    # Remove short walking, 99, Train, Other and Unspecified modes
    dataset <- filter(bs, ! trip_mode %in% c('Short Walking', "99", "Train", "Other", "Unspecified"))
    # Unique number of ind
    total_ind <- length(unique(bs$participant_id))
    
    l <- list()
    for (i in 1:length(unique(dataset$scenario))){
      bd <- filter(dataset, scenario == unique(dataset$scenario)[i])
      bdnr <- nrow(bd)
      bd <- bd %>% group_by(trip_mode) %>%  summarise(pert = dplyr::n())
      bd <- bd %>%  dplyr::select(trip_mode, pert) %>% 
        setNames(c("trip_mode",unique(dataset$scenario)[i])) 
      l[[i]] <- bd
      
    }
    
    bd <- l[[1]]
    if(length(l)>1)
      for (i in 2:length(l))
        bd <- left_join(bd, l[[i]], by = "trip_mode")
    
    l <- list()
    for (i in 1:length(unique(dataset$scenario))){
      bd <- filter(dataset, scenario == unique(dataset$scenario)[i])
      bdnr <- nrow(bd)
      bd <- bd %>% group_by(trip_mode) %>%  summarise(pert = round(dplyr::n()/bdnr * 100, 1))
      bd <- bd %>%  dplyr::select(trip_mode, pert) %>% 
        setNames(c("trip_mode",unique(dataset$scenario)[i])) 
      l[[i]] <- bd
    }
    
    bd <- l[[1]]
    if(length(l)>1)
      for (i in 2:length(l))
        bd <- left_join(bd, l[[i]], by = "trip_mode")
    
    bd <- reshape2::melt(bd)
    
    plotly::ggplotly(ggplot(data = bd, aes(x = trip_mode, y = value, fill = variable)) + 
                       geom_bar(stat = 'identity', position = "dodge", color = "black") + 
                       theme_minimal() + xlab('Mode') + ylab('Percentage (%)') + labs(title = "Mode distribution per week"))
    # Calculate trip distance for baseline and three scenarios
    
  }
  
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
  
  if(plotFlag){
    
    dist_melted <- reshape2::melt(dist, by = trip_mode)
    # Plot
    plotly::ggplotly(ggplot(data = dist_melted, aes(x = trip_mode, y = value / total_ind, fill = variable)) + 
                       geom_bar(stat = 'identity', position = 'dodge', color = "black") + 
                       theme_minimal() + xlab('Mode') + ylab('Distance (km)') + labs(title = "Mode distance  per person per week (km)")
    )
    
    dur_melted <- reshape2::melt(dur, by = trip_mode)
    
    dur_melted$value <- round(dur_melted$value / (60 * total_ind), 2)
    
    # Plot
    plotly::ggplotly(ggplot(data = dur_melted, aes(x = trip_mode, y = value, fill = variable)) + 
                       geom_bar(stat = 'identity', position = 'dodge', color = 'black') + 
                       theme_minimal() + xlab('Mode') + ylab('Duration (hours)') + labs(title = 
                                                                                          "Mode Duration per person per week (hours)")
    )
  }
  
  #list(dist,dur)
  list(dist=dist,dur=dur)
}

total_mmet <- function(rd){
  rd_pa <- subset(rd,trip_mode%in%c('Bicycle','Walking','Short Walking'))
  # Convert baseline's trip duration from mins to hours
  rd_pa$trip_duration_hrs <- rd_pa$trip_duration / 60
  # Get total individual level walking and cycling and sport mmets 
  pa_ind <- setDT(rd_pa)[,.(sex = first(sex), age = first(age),  age_cat = first(age_cat), 
                              cycling_mmet_base = (sum(trip_duration_hrs[trip_mode == 'Bicycle' & scenario == 'Baseline']) ),
                              walking_mmet_base = (sum(trip_duration_hrs[trip_mode %in%c('Walking','Short Walking')  & scenario == 'Baseline']) ),
                              cycling_mmet_scen1 = (sum(trip_duration_hrs[trip_mode == 'Bicycle'  & scenario == 'Scenario 1']) ),
                              walking_mmet_scen1 = (sum(trip_duration_hrs[trip_mode %in%c('Walking','Short Walking')  & scenario == 'Scenario 1']) ),
                              cycling_mmet_scen2 = (sum(trip_duration_hrs[trip_mode == 'Bicycle'   & scenario == 'Scenario 2']) ),
                              walking_mmet_scen2 = (sum(trip_duration_hrs[trip_mode  %in%c('Walking','Short Walking')   & scenario == 'Scenario 2']) ), 
                              cycling_mmet_scen3 = (sum(trip_duration_hrs[trip_mode == 'Bicycle'   & scenario == 'Scenario 3']) ),
                              walking_mmet_scen3 = (sum(trip_duration_hrs[trip_mode  %in%c('Walking','Short Walking')  & scenario == 'Scenario 3']) ),
                              cycling_mmet_scen4 = (sum(trip_duration_hrs[trip_mode == 'Bicycle'   & scenario == 'Scenario 4']) ),
                              walking_mmet_scen4 = (sum(trip_duration_hrs[trip_mode  %in%c('Walking','Short Walking')   & scenario == 'Scenario 4']) ), 
                              cycling_mmet_scen5 = (sum(trip_duration_hrs[trip_mode == 'Bicycle'   & scenario == 'Scenario 5']) ),
                              walking_mmet_scen5 = (sum(trip_duration_hrs[trip_mode  %in%c('Walking','Short Walking')   & scenario == 'Scenario 5']) ),
                              work_ltpa_mmet = first(work_ltpa_marg_met)),by='participant_id']
  
  # Calculate MMETs
  pa_ind$base_mmet <- pa_ind$work_ltpa_mmet * BACKGROUND_PA_SCALAR +  pa_ind$cycling_mmet_base* MMET_CYCLING + pa_ind$walking_mmet_base * MMET_WALKING
  pa_ind$scen1_mmet <- pa_ind$work_ltpa_mmet * BACKGROUND_PA_SCALAR +  pa_ind$cycling_mmet_scen1* MMET_CYCLING + pa_ind$walking_mmet_scen1 * MMET_WALKING
  pa_ind$scen2_mmet <- pa_ind$work_ltpa_mmet * BACKGROUND_PA_SCALAR +  pa_ind$cycling_mmet_scen2* MMET_CYCLING + pa_ind$walking_mmet_scen2 * MMET_WALKING
  pa_ind$scen3_mmet <- pa_ind$work_ltpa_mmet * BACKGROUND_PA_SCALAR +  pa_ind$cycling_mmet_scen3* MMET_CYCLING + pa_ind$walking_mmet_scen3 * MMET_WALKING
  pa_ind$scen4_mmet <- pa_ind$work_ltpa_mmet * BACKGROUND_PA_SCALAR +  pa_ind$cycling_mmet_scen4* MMET_CYCLING + pa_ind$walking_mmet_scen4 * MMET_WALKING
  pa_ind$scen5_mmet <- pa_ind$work_ltpa_mmet * BACKGROUND_PA_SCALAR +  pa_ind$cycling_mmet_scen5* MMET_CYCLING + pa_ind$walking_mmet_scen5 * MMET_WALKING
  name_indices <- which(colnames(pa_ind)%in%c('participant_id', 'sex', 'age', 'age_cat', paste0(SCEN_SHORT_NAME,'_mmet')))
  mmets <- tbl_df(pa_ind)[,name_indices]
  mmets
  
}

scenario_pm_calculations <- function(scen_dist,rd){
  
  # concentration contributed by non-transport share (remains constant across the scenarios)
  non_transport_pm_conc <- PM_CONC_BASE*(1 - PM_TRANS_SHARE)  
  
  ### Calculating number of scenarios besides the baseline
  trans_emissions <- TRANS_EMISSIONS_ORIGINAL
  ## get distance, multiply by accra emission factor
  ##RJ question for RG: looks like this is using bus travel distance to estimate emissions. Is this right?
  #for (i in 2:6)  trans_emissions[[SCEN_SHORT_NAME[i]]] <- trans_emissions$base*c(scen_dist[[SCEN[i]]][c(4,4,3,5,2)]/scen_dist[[SCEN[1]]][c(4,4,3,5,2)],1,1,1)
  for (i in 2:6)  trans_emissions[[SCEN_SHORT_NAME[i]]] <- trans_emissions$base*c(scen_dist[[SCEN[i]]][c(4,4,3,5)]/scen_dist[[SCEN[1]]][c(4,4,3,5)],1,1,1,1)
  #for (i in 1:NSCEN){
  #  trans_emissions[1,p+i] <- trans_emissions$base_emissions[1]*scen_dist[4,n+i]/scen_dist[4,n] ## scenario emissions of 4W1
  #  trans_emissions[2,p+i] <- trans_emissions$base_emissions[2]*scen_dist[4,n+i]/scen_dist[4,n] ## scenario emissions of 4W2 (>2000cc engine size)
  #  trans_emissions[3,p+i] <- trans_emissions$base_emissions[3]*scen_dist[3,n+i]/scen_dist[3,n] ## scenario emissions of 2W
  #  trans_emissions[4,p+i] <- trans_emissions$base_emissions[4]*scen_dist[5,n+i]/scen_dist[5,n] ## scenario emissions of Taxi
  #  trans_emissions[5,p+i] <- trans_emissions$base_emissions[5]*scen_dist[2,n+i]/scen_dist[2,n] ## scenario emissions of bus
  #  trans_emissions[6,p+i] <- trans_emissions$base_emissions[6]*1 ## scenario emissions of trucks
  #  trans_emissions[7,p+i] <- trans_emissions$base_emissions[7]*1 ## scenario emissions of trucks
  #  trans_emissions[8,p+i] <- trans_emissions$base_emissions[8]*1 ## scenario emissions of trucks
  #  names(trans_emissions)[p+i] <-(paste("scen",i,"_emissions", sep=""))
  #}
  
  baseline_sum <- sum(trans_emissions[[SCEN_SHORT_NAME[1]]])
  conc_pm <- c()
  for(i in 1:length(SCEN_SHORT_NAME))
    conc_pm[i] <- non_transport_pm_conc + PM_TRANS_SHARE*PM_CONC_BASE*sum(trans_emissions[[SCEN_SHORT_NAME[i]]])/baseline_sum
  
  ##RJ rewriting ventilation as a function of MMET_CYCLING and MMET_WALKING, loosely following de Sa's SP model.
  vent_rates <- LOOKUP_RATIO_PM # L / min
  ## RG will send RJ equation for ratio, which is the ratio of pm inhalation on road relative to off road (1). This value depends on the total background pm.
  vent_rates$vent_rate[vent_rates$trip_mode=='Bicycle'] <- 10 + 5.0*MMET_CYCLING
  vent_rates$vent_rate[vent_rates$trip_mode%in%c('Walking','Short Walking')] <- 10 + 5.0*MMET_WALKING
  ### following code generates final_data
  for (i in 1:length(SCEN)){
    scen_index <- SCEN[i]
    rd_scen <- filter(rd, scenario == scen_index)
    rd_scen <- left_join(rd_scen,vent_rates, "trip_mode")  ## attaching the file with in-vehicle ratio and ventilation rate
    rd_scen$on_road_air <- rd_scen$trip_duration*rd_scen$vent_rate / 60 # L
    rd_scen$pm_dose <- rd_scen$on_road_air * rd_scen$ratio * as.numeric(conc_pm[i]) # mg
    
    ##RJ need to retain ids
    #rd_scen$participant_id <- as.factor(rd_scen$participant_id)
    
    individual_data <- summarise(group_by(rd_scen,participant_id),on_road_dur = sum(trip_duration,na.rm=TRUE), 
                                 on_road_pm = sum(pm_dose,na.rm=TRUE), 
                                 air_inhaled = sum(on_road_air,na.rm=TRUE))
    ##RJ question for RG: can you write, in words or equations, what this calculation is doing?
    non_transport_air_inhaled <- (24-individual_data$on_road_dur/60)*10
    individual_data$pm_conc <- ((non_transport_air_inhaled * as.numeric(conc_pm[i])) + individual_data$on_road_pm)/(non_transport_air_inhaled+individual_data$air_inhaled)
    individual_data <- subset(individual_data, select=c("participant_id", "pm_conc"))
    names(individual_data)[2] <- paste0('pm_conc_',SCEN_SHORT_NAME[i])
     
    if (i == 1 ){
      final_data <- individual_data 
    }else{
      final_data <- left_join(final_data,individual_data,by="participant_id")
    }
  }
  
  #####PM normalise
  
  mean_conc <- rep(0,length(SCEN_SHORT_NAME))
  
  ## calculating means of individual-level concentrations
  for ( i in 1: length(SCEN_SHORT_NAME))
    mean_conc[i] <- mean(final_data[[paste0("pm_conc_", SCEN_SHORT_NAME[i])]])
  
  normalise <- as.numeric(conc_pm[1])/as.numeric(mean_conc[1])
  ###Lines which are normalising the concentrations
  
  for (i in 1: length(SCEN_SHORT_NAME))
    final_data[[paste0("pm_conc_", SCEN_SHORT_NAME[i])]] <- normalise*final_data[[paste0("pm_conc_", SCEN_SHORT_NAME[i])]]
  
  list(scenario_pm=conc_pm, pm_conc_pp=as.data.frame(final_data))
  
}

gen_ap_rr <- function(rd,ind_pm){
  # Create dummy ind pop
  ind <-  summarise(group_by(rd,participant_id),sex = first(sex),
                    age = first(age),
                    age_cat = first(age_cat))
  
  ### combining PM2.5 concentration data (scenario_pm_calculations.R) and PA data (total_mmet.R) at the individual level (n=732)
  #ind<- read.csv("data/synth_pop_data/accra/processed_data/indiv_mmet/pa_total_mmet_weekly.csv") ### PA 
  ind_pm$participant_id <- as.integer(ind_pm$participant_id)
  
  ind <-  left_join(ind,ind_pm, by = "participant_id")
  ## assigning air pollution age band to the individual_level data
  min_ages <- c(seq(24,94,by=5),200)
  ind$ap_age <-
    sapply(ind$age, function(x)
      if(x > min_ages[1])
        min_ages[which(min_ages > x)[1] - 1] + 1
      else
        0)
  
  ## for every individual average of all parameter draws within the age and disease-outcome
  
  pm_indices <- sapply(SCEN_SHORT_NAME,function(x)which(colnames(ind)==paste0("pm_conc_",x)))
  ### iterating over all all disease outcomes
  for ( j in 1:nrow(DISEASE_OUTCOMES)){
    ## checking whether to calculate this health outcome for air pollution
    if (DISEASE_OUTCOMES$air_pollution[j] == 1){ 
      # initialise lists
      for (x in 1:length(SCEN_SHORT_NAME))
        ind[[paste0("RR_ap_", SCEN_SHORT_NAME[x])]] <- 0
      cause <- as.character(DISEASE_OUTCOMES$ap_acronym[j])
      dr_ap_disease <- subset(DR_AP, cause_code == cause)
      # apply by age groups
      ages <- unique(dr_ap_disease$age_code)
      for(age in ages){
        dr_ap_sub <- subset(dr_ap_disease,age_code == age )
        if(age==99){
          i <-1:nrow(ind)
        }else{
          i <- which(ind$ap_age==age)
        }
        # get parameters
        alpha <- DR_AP_LIST[[cause]][[as.character(age)]]$alpha
        beta <- DR_AP_LIST[[cause]][[as.character(age)]]$beta
        gamma <- DR_AP_LIST[[cause]][[as.character(age)]]$gamma
        tmrel <- DR_AP_LIST[[cause]][[as.character(age)]]$tmrel
        # calculate AP and apply to all in age group
        for(x in 1: length(SCEN_SHORT_NAME)) 
          ind[[paste0("RR_ap_", SCEN_SHORT_NAME[x])]][i] <-
          as.numeric(1 + alpha * (1 - exp(-beta * (ind[[pm_indices[x]]][i] - tmrel) ^ gamma )))
      }
      ## change the names of the columns as per the disease
      for (n in 1: length(SCEN_SHORT_NAME)){
        col <- which(names(ind)== paste0("RR_ap_",SCEN_SHORT_NAME[n]))
        names(ind)[col]<- paste0("RR_ap_",SCEN_SHORT_NAME[n],"_",DISEASE_OUTCOMES$acronym[j])
      }
    }
  }
  ind
}

PA_dose_response <- function (cause, outcome_type, dose, confidence_intervals = F){
  
  if (sum(is.na(dose))>0 || class(dose)!= "numeric"){
    stop ('Please provide dose in numeric')
  }
  if (!cause %in% c('all-cause-mortality', 'breast-cancer', 'cardiovascular-disease',
                    'colon-cancer', 'coronary-heart-disease', 'diabetes', 'endometrial-cancer',
                    'heart-failure', 'lung-cancer', 'stroke', 'total-cancer')){
    stop('Unsupported cause/disease. Please select from \n
         all-cause-mortality \n
         breast-cancer\n
         cardiovascular-disease \n
         colon-cancer \n
         coronary-heart-disease \n
         endometrial-cancer \n
         heart-failure \n
         lung-cancer \n
         stroke \n
         total-cancer')
  }
  if (!outcome_type %in% c('mortality', 'incidence')){
    stop('Unsupported outcome_type. Please select from \n
         mortality \n
         incidence')
  }
  if (cause == 'all-cause-mortality' && outcome_type == 'incidence'){
    stop('Incidence does not exist for all-cause-mortality')
  }
  fname <- paste(cause, outcome_type, sep = "-")
  if (cause == 'all-cause-mortality')
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
    rr <- truncnorm::qtruncnorm(get(paste0('PA_DOSE_RESPONSE_QUANTILE_',cause)), rr, a=lb, b=ub)
  }
  if (confidence_intervals) {
    return(data.frame (rr = rr, lb = lb, ub = ub))
  }else{
    return(data.frame(rr = rr))
  }
}

gen_pa_rr <- function(ind){
  ### iterating over all all disease outcomes
  dose_columns <- match(paste0(SCEN_SHORT_NAME, '_mmet'),colnames(ind))
  doses_clean <- ind[,dose_columns]
  for ( j in 1:nrow(DISEASE_OUTCOMES)){
    ## checking whether to calculate this health outcome for PA
    if (DISEASE_OUTCOMES$physical_activity[j] == 1){
      pa_dn <- as.character(DISEASE_OUTCOMES$pa_acronym[j])
      pa_n <- as.character(DISEASE_OUTCOMES$acronym[j])
      outcome_type <- ifelse(pa_dn%in%c('lung-cancer','stroke'), 'incidence' , 'mortality')
      # CHD: 35 mmeth per week use mortality
      # Lung cancer: 10 mmeth per week use incidence
      # stroke 75 pert: 13.37
      # Diabetes no limits
      # total cancer: 35 mmeths per week use mortality
      doses <- doses_clean
      if(pa_dn %in% c('total-cancer','coronary-heart-disease')) doses[doses>35] <- 35
      else if(pa_dn == 'lung-cancer') doses[doses>10] <- 10
      else if(pa_dn == 'stroke') doses[doses>13.37] <- 13.37
      else if(pa_dn == 'all-cause-mortality') doses[doses>16.08] <- 16.08
      ##RJ apply function to all doses as one long vector
      return_vector <- PA_dose_response(cause = pa_dn, outcome_type = outcome_type, 
                                   dose = unlist(data.frame(doses)))
      ##RJ take segments of returned vector corresponding to scenario
      for (i in 1:length(SCEN_SHORT_NAME)){
        scen <- SCEN_SHORT_NAME[i]
        ind[[paste('RR_pa', scen, pa_n, sep = '_')]] <- return_vector$rr[(1+(i-1)*nrow(doses)):(i*nrow(doses))]
      }
    }
  }
  ind 
}

combined_rr_pa_pa <- function(ind_pa,ind_ap){
  
  # Replace NaNs with 1
  ind_ap[is.na(ind_ap)] <- 1
  
  # Replace Na with 1
  ind_pa[is.na(ind_pa)] <- 1
  
  # remove common columns from ap
  ind_ap <- dplyr::select(ind_ap, -c(sex, age, age_cat))
  
  # join pa and ap ind datasets
  ind <- left_join(ind_pa, ind_ap, by = "participant_id")

  ### iterating over all all disease outcomes
  for ( j in 1:nrow(DISEASE_OUTCOMES)){
    ## checking whether to calculate this health outcome for PA
    if (DISEASE_OUTCOMES$physical_activity[j] == 1 & DISEASE_OUTCOMES$air_pollution[j] == 1){
      for (scen in SCEN_SHORT_NAME){
        ac <- as.character(DISEASE_OUTCOMES$acronym[j])
        ind[[paste('RR_pa_ap', scen, ac, sep = '_')]] <- ind[[paste('RR_pa', scen, ac, sep = '_')]] * ind[[paste('RR_ap', scen, ac, sep = '_')]]
        
      }
    }
  }
  
  ind
}

injuries_function <- function(relative_distances,scen_dist){
  ### injury code
  ### This is the script for distance-based injury model for Accra using safety-in-numbers
  
  ##RJ match exponents and distances to multiply matrices
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
  for (k in 2:(length(SCEN_SHORT_NAME))) victim_deaths <- cbind(victim_deaths, as.data.frame(rowSums(whw_mat2[[k]][,2:7],na.rm=T))) 
  names(victim_deaths)[1] <- c("victim_type")
  names(victim_deaths)[2:(length(SCEN_SHORT_NAME)+1)] <- SCEN_SHORT_NAME
  
  ## distribute injuries to ages
  ##RJ match distances and injuries to multiply matrices
  dist_scen_indices <- match(relative_distances$scenario,SCEN)
  vic_scen_indices <- match(SCEN_SHORT_NAME[dist_scen_indices],colnames(victim_deaths))
  vic_mode_indices <- match(names(relative_distances)[3:7],victim_deaths[,1])
  injuries <- relative_distances
  ##!! hard-coding of indices
  injuries[,3:7] <- injuries[,3:7]*t(victim_deaths[vic_mode_indices,vic_scen_indices])
  
  injuries[,9] <- rowSums(injuries[,3:7],na.rm=T)
  names(injuries)[9] <-"Deaths"
  
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
  
  list(injuries=injuries,deaths_yll_injuries=deaths_yll_injuries,scen1_injuries=scen1_injuries)
}

health_burden <- function(ind,inj){
  # subset gbd data for outcome types
  gbd_data_scaled <- GBD_DATA
  gbd_data_scaled$value_gama[gbd_data_scaled$cause%in%c("Neoplasms","Ischemic heart disease","Tracheal, bronchus, and lung cancer")] <- 
    gbd_data_scaled$value_gama[gbd_data_scaled$cause%in%c("Neoplasms","Ischemic heart disease","Tracheal, bronchus, and lung cancer")]*CHRONIC_DISEASE_SCALAR
  gbd_deaths <- subset(gbd_data_scaled,measure=='Deaths' & metric == "Number")
  gbd_ylls <- subset(gbd_data_scaled,measure=='YLLs (Years of Life Lost)' & metric == "Number")
  ##!! Hard-coded column names to initialise tables.
  unique_category1 <- unique(ind[[2]])
  unique_category2 <- unique(ind[[4]])
  pop_details <- expand.grid(unique_category1, unique_category2,stringsAsFactors = F)
  colnames(pop_details) <- colnames(ind)[c(2,4)]
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
      PAF(pop = ind[, colnames(ind) %in% c(base_var, 'sex', 'age_cat')], cn = base_var, mat =
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
      #yll_red_name <- paste0(scen, '_ylls_red_',middle_bit,ac)
      deaths_name <- paste0(scen, '_deaths_',middle_bit,ac)
      #deaths_red_name <- paste0(scen, '_deaths_red_',middle_bit,ac)
      # Calculate PIFs for selected scenario
      pif_temp <- PAF(pop = ind[,colnames(ind)%in%c(scen_var,'sex', 'age_cat')], cn = scen_var, mat=pop_details)
      pif_scen <- (pif_ref - pif_temp) / pif_ref
      # Calculate ylls (total and red)
      yll_dfs <- combine_health_and_pif(pop=pop_details,pif_values=pif_scen, hc = gbd_ylls_disease)
      ylls[[yll_name]] <- yll_dfs
      #ylls_red[[yll_red_name]] <- yll_dfs / yll_ref
      # Calculate deaths (total and red)
      death_dfs <- combine_health_and_pif(pop=pop_details,pif_values=pif_scen,hc=gbd_deaths_disease)
      deaths[[deaths_name]] <- death_dfs
      #deaths_red[[deaths_red_name]] <- death_dfs / death_ref
    }
  }
  # Select deaths columns
  inj_deaths <- dplyr::select(inj, c(age_cat, sex, contains("deaths")))
  # Select yll columns
  inj_ylls <- dplyr::select(inj, c(age_cat, sex, contains("yll")))
  # Join injuries data to global datasets
  deaths <- left_join(deaths, inj_deaths, by = c("age_cat", "sex"))
  ylls <- left_join(ylls, inj_ylls, by = c("age_cat", "sex"))
  #list(deaths=deaths,deaths_red=deaths_red,ylls=ylls,ylls_red=ylls_red)
  list(deaths=deaths,ylls=ylls)
}

PAF <- function(pop, cn, mat){
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

ithim_uncertainty <- function(ithim_obj,seed=1){ 
  ############################
  for(i in 1:length(ithim_obj))
    assign(names(ithim_obj)[i],ithim_obj[[i]])
  # Get parameters
  for(i in 1:length(parameters))
    assign(names(parameters)[i],parameters[[i]][[seed]],pos=1)
  ## Re-do set up if MEAN_BUS_WALK_TIME has changed
  if('MEAN_BUS_WALK_TIME'%in%names(parameters)){
    rd <- ithim_setup_baseline_scenario()
    ithim_obj$bs <- create_all_scenarios(rd)
    ######################
    # Generate distance and duration matrices
    dist_and_dur <- dist_dur_tbls(ithim_obj$bs)
    ithim_obj$dist <- dist_and_dur$dist
    ithim_obj$dur <- dist_and_dur$dur
    # distances for injuries calculation
    ithim_obj$inj_distances <- distances_for_injury_function(ithim_obj$bs)
  }
  ############################
  # Run ITHIM cascade of functions
  run_results <- run_ithim(ithim_obj,seed)
  run_results$dist <- ithim_obj$dist
  run_results$dur <- ithim_obj$dur
  #return(run_results)
  ##!! RJ for now return only hb from uncertain simulations
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
  (pm_conc <- scenario_pm_calculations(dist,bs))
  scenario_pm <- pm_conc$scenario_pm
  pm_conc_pp <- pm_conc$pm_conc_pp
  # Air pollution calculation
  (RR_AP_calculations <- gen_ap_rr(bs,pm_conc_pp))
  ############################
  ## (2) PA PATHWAY
  # Calculate total mMETs
  (mmets <- total_mmet(bs))
  # Physical activity calculation
  (RR_PA_calculations <- gen_pa_rr(mmets))
  ############################
  ## (3) COMBINE (1) AND (2)
  # Physical activity and air pollution combined
  (RR_PA_AP_calculations <- combined_rr_pa_pa(RR_PA_calculations,RR_AP_calculations))
  ############################
  ## (4) INJURIES
  # Injuries calculation
  (deaths_yll_injuries <- injuries_function(inj_distances[[1]],inj_distances[[2]]))
  injuries <- deaths_yll_injuries$injuries
  scen1_injuries <- deaths_yll_injuries$scen1_injuries
  ############################
  ## (5) COMBINE (3) AND (4)
  # Combine health burden from disease and injury
  (hb <- health_burden(RR_PA_AP_calculations,deaths_yll_injuries$deaths_yll_injuries))
  return(list(mmets=mmets,scenario_pm=scenario_pm,pm_conc_pp=pm_conc_pp,injuries=injuries,scen1_injuries=scen1_injuries,hb=hb))
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


