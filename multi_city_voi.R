library(ithimr)
library(earth)
library(RColorBrewer)
library(plotrix)
library(foreach)
library(future)
plan(multisession)
library(doRNG)
library(future.apply) 
library(voi) #install_github("chjackson/voi")
library(readxl)
library(rlist)

if (!require("drpa",character.only = TRUE)) {
  print('Installing "drpa" package...')
  remotes::install_github("meta-analyses/drpa")
  library(drpa)
  print("")
}

rm(list=ls())

# cities <- c('antofagasta', 'arica', 'belo_horizonte', 'bogota', 'buenos_aires',
#             'cali', 'copiapo', 'coquimbo_laserena', 'gran_valparaiso',
#             'iquique_altohospicio', 'medellin', 'mexico_city', 'montevideo',
#             'osorno', 'puerto_montt', 'san_antonio',
#             'santiago', 'sao_paulo', 'temuco_padrelascasas', 'valdivia')

cities <- c('bogota')

# number of times input values are sampled from each input parameter distribution
nsamples <- 2


voi_analysis <- T # set to T if want to run VoI analysis and to F otherwise

# list of potential values for the outcome_voi_list
# 'pa_ap_all_cause', 'pa_ap_IHD', 'pa_total_cancer', 'pa_ap_lung_cancer', 'ap_COPD', 
# 'pa_ap_stroke', 'pa_ap_T2D', 'ap_LRI', 'pa_breast_cancer', 'pa_colon_cancer', 'pa_endo_cancer',
# 'pa_liver_cancer', 'pa_ap_CVD', 'pa_total_dementia', 'pa_myeloma', 'pa_Parkinson',
# 'pa_head_neck_cancer', 'pa_stomach_cancer', 'inj'


# outcome_voi_list <- c('pa_ap_all_cause', 'pa_ap_IHD', 'pa_total_cancer', 'pa_ap_lung_cancer', 'ap_COPD', 
#                       'pa_ap_stroke', 'pa_ap_T2D', 'ap_LRI', 'pa_breast_cancer', 'pa_colon_cancer', 'pa_endo_cancer',
#                       'pa_liver_cancer', 'pa_ap_CVD', 'pa_total_dementia', 'pa_myeloma', 'pa_Parkinson',
#                       'pa_head_neck_cancer', 'pa_stomach_cancer', 'inj')

outcome_voi_list <- c('pa_ap_all_cause', 'inj')


# flag whether to run VOI analysis split by age and gender as well
voi_age_gender <- F   # set to T if want to include split and to F otherwise

# add total across all outputs in VOI list for each scenario - only makes sense if results are independent of each other
# i.e. combining e.g. "total_cancer" with "lung_cancer" results in double-counting and invalid VOI analysis for the sum
voi_add_sum <- T


input_parameter_file <- "InputParameters_v37.0.xlsx"


## Get the current repo sha
gitArgs <- c("rev-parse", "--short", "HEAD", ">", file.path("repo_sha"))
# Use shell command for Windows as it's failing with system2 for Windows (giving status 128)
if (.Platform$OS.type == "windows"){
  shell(paste(append("git", gitArgs), collapse = " "), wait = T)
} else {
  system2("git", gitArgs, wait = T)
}


repo_sha <-  as.character(readLines(file.path("repo_sha")))
# records the main aspects of an ithim run in the OutputVersionControl.txt document
# text file records timestamp of run, author name, cities the script is run for, 
# the input parameter file version used, the output version, 
# the number of samples (which is 1 in constant mode), the path to any other input files,
# any comments and the runtime of the code
write_output_control = T # whether you want to save the model run specifics or not
#output_version <- paste0(repo_sha, "_test_run") # gives the version number of the output documents, independent of the input parameter file name
output_version <- 'v0.3'
author <- "AKS"
comment <- "Added CO2 emission sampling"

# scenario definition
scenario_name <- "BOGOTA"
reference_scenario <- 'Baseline'
scenario_increase <- 0.05 # increase for each mode in each scenario



compute_mode <- 'sample' # sample from the given input parameter distributions




############################### No need to change the following ##################################
# keep record when code started:
starttime <- Sys.time()

all_inputs <- read_excel(input_parameter_file, sheet = "all_city_parameter_inputs")
all_inputs[is.na(all_inputs)] <- ""
all_inputs <- as.data.frame(all_inputs)

#all_inputs <- read.csv('all_city_parameter_inputs.csv',stringsAsFactors = F) # read in parameter list


# get input parameters into correct format
parameter_names <- all_inputs$parameter
parameter_starts <- which(parameter_names!='')
parameter_stops <- c(parameter_starts[-1] - 1, nrow(all_inputs)) 
parameter_names <- parameter_names[parameter_names!='']
parameter_list <- list()

for(i in 1:length(parameter_names)){
  parameter_list[[parameter_names[i]]] <- list()
  parameter_index <- which(all_inputs$parameter==parameter_names[i]) 
  if(all_inputs[parameter_index,2]=='')  { 
    parameter_list[[parameter_names[i]]] <- lapply(cities,function(x) {
      city_index <- which(colnames(all_inputs)==x)
      val <- all_inputs[parameter_index,city_index]
      ifelse(val%in%c('T','F'),val,ifelse(is.numeric(val), as.numeric(val), as.character(val)))
    })
    names(parameter_list[[parameter_names[i]]]) <- cities
  }else if(all_inputs[parameter_index,2]=='constant'){
    if (compute_mode != 'sample'){
      indices <- 0
      parameter_list[[parameter_names[i]]] <- lapply(cities,function(x) {
        city_index <- which(colnames(all_inputs)==x)
        val <- all_inputs[parameter_index+indices,city_index]
        ifelse(val=='',0,as.numeric(val))
      })
    }
    if(compute_mode=='sample'){ # if sampling from distribution, check that distribution parameters exist
      parameter_list[[parameter_names[i]]] <- lapply(cities,function(x) {
        indices <- 1:2
        city_index <- which(colnames(all_inputs)==x)  
        val <- all_inputs[parameter_index+indices,city_index] 
        if (val[1] == '' & val[2]==''){  # if no distribution parameters given in input file, read in constant value instead
          indices <-0
          city_index <- which(colnames(all_inputs)==x) 
          val <- all_inputs[parameter_index+indices,city_index]} 
        val <- as.numeric(val)
      })
    }
    names(parameter_list[[parameter_names[i]]]) <- cities
  }else{
    parameter_list[[parameter_names[i]]] <- lapply(cities,function(x) {
      city_index <- which(colnames(all_inputs)==x)
      if(any(all_inputs[parameter_starts[i]:parameter_stops[i],city_index]!='')){
        sublist_indices <- which(all_inputs[parameter_starts[i]:parameter_stops[i],city_index]!='')
        thing <- as.list(as.numeric(c(all_inputs[parameter_starts[i]:parameter_stops[i],city_index])[sublist_indices]))
        names(thing) <- c(all_inputs[parameter_starts[i]:parameter_stops[i],2])[sublist_indices]
        thing
      }
    }
    )
    names(parameter_list[[parameter_names[i]]]) <- cities
  }
}

list2env(parameter_list, environment()) 


# read in global parameters


all_global_inputs <- read_excel(input_parameter_file, sheet = "all_global_parameter_inputs")
all_global_inputs[is.na(all_global_inputs)] <- ""
all_global_inputs <- as.data.frame(all_global_inputs)


# get input parameters into correct format
global_parameter_names <- all_global_inputs$parameter
global_parameter_starts <- which(global_parameter_names!='')
global_parameter_stops <- c(global_parameter_starts[-1] - 1, nrow(all_global_inputs)) 
global_parameter_names <- global_parameter_names[global_parameter_names!='']
global_parameter_list <- list()

for(i in 1:length(global_parameter_names)){
  global_parameter_list[[global_parameter_names[i]]] <- list()
  global_parameter_index <- which(all_global_inputs$parameter==global_parameter_names[i]) 
  if(all_global_inputs[global_parameter_index,2]=='')  { 
    
    global_parameter_list[[global_parameter_names[i]]] <- all_global_inputs[global_parameter_index,'global']
    
  }else if(all_global_inputs[global_parameter_index,2]=='constant'){
    if (compute_mode != 'sample'){
      global_parameter_list[[global_parameter_names[i]]] <- ifelse(all_global_inputs[global_parameter_index,'global']=='',
                                                                   0,as.numeric(all_global_inputs[global_parameter_index,'global']))
    }
    else if(compute_mode=='sample'){ # if sampling from distribution, check that distribution parameters exist
      indices <- 1:2
      val <- all_global_inputs[global_parameter_index+indices,'global'] 
      if (val[1] == '' & val[2]==''){  # if no distribution parameters given in input file, read in constant value instead
        val <- all_global_inputs[global_parameter_index,'global']} 
      val <- as.numeric(val)
      global_parameter_list[[global_parameter_names[i]]] <- val
    }
  }
}

list2env(global_parameter_list, environment()) 

dist_cat <- unlist(strsplit(gsub(" ", "", dist_cat, fixed = TRUE), "\\,"))

outcome_age_min <- as.numeric(unlist(strsplit(gsub(" ", "", outcome_age_min, fixed = TRUE), "\\,")))
outcome_age_max <- as.numeric(unlist(strsplit(gsub(" ", "", outcome_age_max, fixed = TRUE), "\\,")))
outcome_age_groups <- unlist(strsplit(gsub(" ", "", outcome_age_groups, fixed = TRUE), "\\,"))

min_age <- as.numeric(min_age)
max_age <- as.numeric(max_age)




################################### Start running the the actual analysis


## with uncertainty
## comparison across cities
setting_parameters <- c("PM_CONC_BASE","BACKGROUND_PA_SCALAR","BACKGROUND_PA_ZEROS","PM_EMISSION_INVENTORY","CO2_EMISSION_INVENTORY",
                        "CHRONIC_DISEASE_SCALAR","PM_TRANS_SHARE","INJURY_REPORTING_RATE","BUS_TO_PASSENGER_RATIO", "CAR_OCCUPANCY_RATIO",
                        "TRUCK_TO_CAR_RATIO", "FLEET_TO_MOTORCYCLE_RATIO","BUS_WALK_TIME", 'RAIL_WALK_TIME',"PROPORTION_MOTORCYCLE_TRIPS" ,
                        "DISTANCE_SCALAR_CAR_TAXI",
                        "DISTANCE_SCALAR_WALKING", "DISTANCE_SCALAR_PT", "DISTANCE_SCALAR_CYCLING", "DISTANCE_SCALAR_MOTORCYCLE")


# logical for PA dose response: set T for city 1, and reuse values in 2 and 3; no need to recompute
pa_dr_quantile <-  c(rep(as.logical(pa_dr_quantile_city1), length(cities)))
# logical for AP dose response: set T for city 1, and reuse values in 2 and 3; no need to recompute
ap_dr_quantile <-  c(rep(as.logical(ap_dr_quantile_city1), length(cities)))


betaVariables <- c("PM_TRANS_SHARE",
                   "INJURY_REPORTING_RATE",
                   "CASUALTY_EXPONENT_FRACTION",
                   "BUS_TO_PASSENGER_RATIO",
                   "CAR_OCCUPANCY_RATIO",
                   "TRUCK_TO_CAR_RATIO",
                   "FLEET_TO_MOTORCYCLE_RATIO",
                   "PROPORTION_MOTORCYCLE_TRIPS",
                   "CHRONIC_DISEASE_SCALAR",
                   "SIN_EXPONENT_SUM",
                   "SIN_EXPONENT_SUM_NOV",
                   "SIN_EXPONENT_SUM_CYCLE",
                   "SIN_EXPONENT_SUM_PED",
                   "SIN_EXPONENT_SUM_VEH")
normVariables <- c('CYCLING_MET',
                   'WALKING_MET', 
                   'PASSENGER_MET',
                   'CAR_DRIVER_MET',
                   'MOTORCYCLIST_MET',
                   'SEDENTARY_ACTIVITY_MET',
                   'LIGHT_ACTIVITY_MET',
                   'MODERATE_PA_MET',
                   'VIGOROUS_PA_MET',
                   "PM_CONC_BASE",
                   "BACKGROUND_PA_SCALAR",
                   "CASUALTY_EXPONENT_FRACTION",
                   "CASUALTY_EXPONENT_FRACTION_CYCLE",
                   "CASUALTY_EXPONENT_FRACTION_PED",
                   "CASUALTY_EXPONENT_FRACTION_VEH",
                   "DISTANCE_SCALAR_CAR_TAXI", 
                   "DISTANCE_SCALAR_WALKING",
                   "DISTANCE_SCALAR_PT",
                   "DISTANCE_SCALAR_CYCLING",
                   "DISTANCE_SCALAR_MOTORCYCLE")


save(cities,setting_parameters,injury_reporting_rate,chronic_disease_scalar,pm_conc_base,pm_trans_share,
     background_pa_scalar,background_pa_confidence,cycling_met,walking_met,passenger_met,
     car_driver_met,motorcyclist_met,sedentary_activity_met,light_activity_met,moderate_pa_met,
     PM_emission_inventories,CO2_emission_inventories,
     sin_exponent_sum,casualty_exponent_fraction, sin_exponent_sum_nov,
     sin_exponent_sum_cycle,casualty_exponent_fraction_cycle, sin_exponent_sum_ped,casualty_exponent_fraction_ped,
     sin_exponent_sum_veh,casualty_exponent_fraction_veh, pa_dr_quantile,ap_dr_quantile,
     bus_to_passenger_ratio,car_occupancy_ratio,truck_to_car_ratio,PM_emission_confidence,CO2_emission_confidence,
     distance_scalar_car_taxi,distance_scalar_motorcycle,
     distance_scalar_pt,distance_scalar_walking,distance_scalar_cycling,add_motorcycle_fleet,add_personal_motorcycle_trips, 
     fleet_to_motorcycle_ratio, proportion_motorcycle_trips,
     betaVariables,normVariables,file='diagnostic/parameter_settings.Rdata')


#parameters_only <- F
#numcores <- parallel::detectCores() - 1

multi_city_ithim <- outcome <- outcome_pp <- yll_per_hundred_thousand <- list()

print(system.time(
  for(ci in 1:length(cities)){
    city <- cities[ci]
    print(city)
    multi_city_ithim[[city]] <- run_ithim_setup(NSAMPLES = nsamples,
                                                seed=ci,
                                                # from multi city script
                                                DIST_CAT = as.character(dist_cat), 
                                                ADD_WALK_TO_PT_TRIPS = as.logical(add_walk_to_pt_trips[[city]]),# originally = F,
                                                CITY=city,
                                                AGE_RANGE =  c(min_age,max_age),
                                                ADD_TRUCK_DRIVERS = as.logical(add_truck_drivers),
                                                ADD_BUS_DRIVERS = as.logical(add_bus_drivers),
                                                ADD_CAR_DRIVERS = as.logical(add_car_drivers),
                                                ADD_MOTORCYCLE_FLEET = as.logical(add_motorcycle_fleet[[city]]), #ADD_MOTORCYCLE_FLEET = add_motorcycle_fleet[[city]],
                                                ADD_PERSONAL_MOTORCYCLE_TRIPS = as.character(add_personal_motorcycle_trips[[city]]),
                                                PM_emission_inventory = PM_emission_inventories[[city]],
                                                CO2_emission_inventory = CO2_emission_inventories[[city]], # added
                                                speeds = speeds[[city]],
                                                
                                                FLEET_TO_MOTORCYCLE_RATIO = fleet_to_motorcycle_ratio[[city]],
                                                PROPORTION_MOTORCYCLE_TRIPS = proportion_motorcycle_trips[[city]],
                                                CYCLING_MET =	cycling_met,
                                                WALKING_MET =	walking_met,
                                                PASSENGER_MET =	passenger_met,
                                                CAR_DRIVER_MET =	car_driver_met,
                                                MOTORCYCLIST_MET =	motorcyclist_met,
                                                SEDENTARY_ACTIVITY_MET =	sedentary_activity_met,
                                                LIGHT_ACTIVITY_MET =	light_activity_met,
                                                MODERATE_PA_MET =	moderate_pa_met,
                                                VIGOROUS_PA_MET	= vigorous_pa_met,
                                                DAY_TO_WEEK_TRAVEL_SCALAR = as.numeric(day_to_week_scalar[[city]]),
                                                SIN_EXPONENT_SUM = sin_exponent_sum,
                                                CASUALTY_EXPONENT_FRACTION = casualty_exponent_fraction,
                                                SIN_EXPONENT_SUM_NOV = sin_exponent_sum_nov,
                                                SIN_EXPONENT_SUM_CYCLE = sin_exponent_sum_cycle,
                                                CASUALTY_EXPONENT_FRACTION_CYCLE = casualty_exponent_fraction_cycle,
                                                SIN_EXPONENT_SUM_PED = sin_exponent_sum_ped,
                                                CASUALTY_EXPONENT_FRACTION_PED = casualty_exponent_fraction_ped,
                                                SIN_EXPONENT_SUM_VEH = sin_exponent_sum_veh,
                                                CASUALTY_EXPONENT_FRACTION_VEH = casualty_exponent_fraction_veh,
                                                CALL_INDIVIDUAL_SIN = as.logical(call_individual_sin),
                                                PA_DOSE_RESPONSE_QUANTILE = pa_dr_quantile[ci],  
                                                AP_DOSE_RESPONSE_QUANTILE = ap_dr_quantile[ci],
                                                INJURY_REPORTING_RATE = injury_reporting_rate[[city]],  
                                                CHRONIC_DISEASE_SCALAR = chronic_disease_scalar[[city]],  
                                                PM_CONC_BASE = pm_conc_base[[city]],  
                                                PM_TRANS_SHARE = pm_trans_share[[city]],  
                                                BACKGROUND_PA_SCALAR = background_pa_scalar[[city]],
                                                BUS_WALK_TIME = bus_walk_time[[city]],
                                                RAIL_WALK_TIME = rail_walk_time[[city]], 
                                                
                                                
                                                #additional in VoI script
                                                REFERENCE_SCENARIO= reference_scenario,
                                                
                                                BACKGROUND_PA_CONFIDENCE = as.numeric(background_pa_confidence[[city]]),
                                                BUS_TO_PASSENGER_RATIO = bus_to_passenger_ratio[[city]],
                                                CAR_OCCUPANCY_RATIO = car_occupancy_ratio[[city]],
                                                TRUCK_TO_CAR_RATIO = truck_to_car_ratio[[city]],
                                                
                                                PM_EMISSION_INVENTORY_CONFIDENCE = as.numeric(PM_emission_confidence[[city]]),
                                                CO2_EMISSION_INVENTORY_CONFIDENCE = as.numeric(CO2_emission_confidence[[city]]),
                                                DISTANCE_SCALAR_CAR_TAXI = distance_scalar_car_taxi[[city]],
                                                DISTANCE_SCALAR_WALKING = distance_scalar_walking[[city]],
                                                DISTANCE_SCALAR_PT = distance_scalar_pt[[city]],
                                                DISTANCE_SCALAR_CYCLING = distance_scalar_cycling[[city]],
                                                DISTANCE_SCALAR_MOTORCYCLE = distance_scalar_motorcycle[[city]],
                                                SCENARIO_NAME = scenario_name,
                                                SCENARIO_INCREASE = scenario_increase,
                                                
                                                BUS_DRIVER_PROP_MALE = as.numeric(bus_driver_prop_male[[city]]),
                                                BUS_DRIVER_MALE_AGERANGE = bus_driver_male_agerange[[city]],
                                                BUS_DRIVER_FEMALE_AGERANGE = bus_driver_female_agerange[[city]],
                                                TRUCK_DRIVER_PROP_MALE = as.numeric(truck_driver_prop_male[[city]]),
                                                TRUCK_DRIVER_MALE_AGERANGE = truck_driver_male_agerange[[city]],
                                                TRUCK_DRIVER_FEMALE_AGERANGE = truck_driver_female_agerange[[city]],
                                                COMMERCIAL_MBIKE_PROP_MALE = as.numeric(commerical_mbike_prop_male[[city]]),
                                                COMMERCIAL_MBIKE_MALE_AGERANGE = commerical_mbike_male_agerange[[city]],
                                                COMMERCIAL_MBIKE_FEMALE_AGERANGE = commerical_mbike_female_agerange[[city]],
                                                MINIMUM_PT_TIME = as.numeric(minimum_pt_time)
    )
    
    
    
    
    # for first city, store model parameters. For subsequent cities, copy parameters over.
    if(ci==1){
      model_parameters <- names(multi_city_ithim[[city]]$parameters)[!names(multi_city_ithim[[city]]$parameters)%in%setting_parameters]
      parameter_names <- model_parameters[model_parameters!="DR_AP_LIST"]
      parameter_samples <- sapply(parameter_names,function(x)multi_city_ithim[[city]]$parameters[[x]])
    }else{
      for(param in model_parameters) multi_city_ithim[[city]]$parameters[[param]] <- multi_city_ithim[[1]]$parameters[[param]]
      if (!is.null(multi_city_ithim[[1]]$parameters$PM_CONC_BASE)){
        background_quantile <- plnorm(multi_city_ithim[[1]]$parameters$PM_CONC_BASE,log(pm_conc_base[[1]][1]),log(pm_conc_base[[1]][2]))
        multi_city_ithim[[city]]$parameters$PM_CONC_BASE <- qlnorm(background_quantile,log(pm_conc_base[[city]][1]),log(pm_conc_base[[city]][2]))
      }
      if (!is.null(multi_city_ithim[[1]]$parameters$PM_TRANS_SHARE)){
        proportion_quantile <- pbeta(multi_city_ithim[[1]]$parameters$PM_TRANS_SHARE,pm_trans_share[[1]][1],pm_trans_share[[1]][2])
        multi_city_ithim[[city]]$parameters$PM_TRANS_SHARE <- qbeta(proportion_quantile,pm_trans_share[[city]][1],pm_trans_share[[city]][2])
      }
    }
    
    
    multi_city_ithim[[city]]$outcomes <- list()
    doFuture::registerDoFuture()
    run_ithm_fn <- function(nsamples, ithim_object,seed, FUN=run_ithim){
      foreach(i=1:nsamples, .export = ls(globalenv()) ) %dorng% {   #%dopar%
        FUN(ithim_object, seed=i)
      }
    }
    multi_city_ithim[[city]]$outcomes <- run_ithm_fn(nsamples,ithim_object = multi_city_ithim[[city]], seed)
    
    #multi_city_ithim[[cities]]$outcomes <- run_ithim(ithim_object=multi_city_ithim[[cities]], seed = 1)
    
    multi_city_ithim[[city]]$DEMOGRAPHIC <- DEMOGRAPHIC
    
    
    ## rename city-specific parameters according to city
    for(i in 1:length(multi_city_ithim[[city]]$parameters$PM_EMISSION_INVENTORY[[1]])){
      extract_vals <- sapply(multi_city_ithim[[city]]$parameters$PM_EMISSION_INVENTORY,function(x)x[[i]])
      if(sum(extract_vals)!=0)
        multi_city_ithim[[city]]$parameters[[paste0('PM_EMISSION_INVENTORY_',names(multi_city_ithim[[city]]$parameters$PM_EMISSION_INVENTORY[[1]])[i],'_',city)]] <- extract_vals
    }
    for(i in 1:length(multi_city_ithim[[city]]$parameters$CO2_EMISSION_INVENTORY[[1]])){
      extract_vals <- sapply(multi_city_ithim[[city]]$parameters$CO2_EMISSION_INVENTORY,function(x)x[[i]])
      if(sum(extract_vals)!=0)
        multi_city_ithim[[city]]$parameters[[paste0('CO2_EMISSION_INVENTORY_',names(multi_city_ithim[[city]]$parameters$CO2_EMISSION_INVENTORY[[1]])[i],'_',city)]] <- extract_vals
    }
    for(param in setting_parameters) names(multi_city_ithim[[city]]$parameters)[which(names(multi_city_ithim[[city]]$parameters)==param)] <- paste0(param,'_',city)
    multi_city_ithim[[city]]$parameters <- multi_city_ithim[[city]]$parameters[-which(names(multi_city_ithim[[city]]$parameters)==paste0('PM_EMISSION_INVENTORY_',city))]
    multi_city_ithim[[city]]$parameters <- multi_city_ithim[[city]]$parameters[-which(names(multi_city_ithim[[city]]$parameters)==paste0('CO2_EMISSION_INVENTORY_',city))]
    
    parameter_names_city <- names(multi_city_ithim[[city]]$parameters)[sapply(names(multi_city_ithim[[city]]$parameters),function(x)grepl(x,pattern=city))]
    ## add to parameter names
    parameter_names <- c(parameter_names,parameter_names_city)
    ## get parameter samples and add to array of parameter samples
    parameter_samples <- cbind(parameter_samples,sapply(parameter_names_city,function(x)multi_city_ithim[[city]]$parameters[[x]]))
    #if(ci>1) multi_city_ithim[[city]]$parameters <- c()
    
    # save scenario names
    scenario_names <- multi_city_ithim[[city]]$outcomes[[1]]$SCEN
    
    # remove scenario definitions
    for (n in 1:nsamples){
      multi_city_ithim[[city]]$outcomes[[n]]<-list.remove(multi_city_ithim[[city]]$outcomes[[n]],'SCEN')
    }
    
    # add relevant model run information to  multi_city_ithim list
    timestamp <- Sys.time()
    multi_city_ithim[[city]]$ithim_run <- list()
    multi_city_ithim[[city]]$ithim_run$input_parameter_file <- input_parameter_file
    multi_city_ithim[[city]]$ithim_run$scenarios_used <- scenario_name
    multi_city_ithim[[city]]$ithim_run$reference_scenario <- reference_scenario
    multi_city_ithim[[city]]$ithim_run$scenario_increase <- scenario_increase
    multi_city_ithim[[city]]$ithim_run$scenario_names <- scenario_names
    multi_city_ithim[[city]]$ithim_run$compute_mode <- compute_mode
    multi_city_ithim[[city]]$ithim_run$timestamp <- timestamp
    multi_city_ithim[[city]]$ithim_run$output_version <- output_version
    multi_city_ithim[[city]]$ithim_run$author <- author
    multi_city_ithim[[city]]$ithim_run$comment <- comment
    
    
    # save results for city and then delete
    saveRDS(multi_city_ithim[[city]],paste0('results/multi_city/',city,'.Rds'))
    
    if(ci>1){
      multi_city_ithim[[ci]] <- 0
    }else{
      multi_city_ithim[[ci]]$outcomes <- 0
    }
  }
)) 


# save scenario names and then delete this sublist from the multi_city_ithim outcomes
# scenario_names <- multi_city_ithim[[city]]$outcomes[[1]]$SCEN
# 
# for (city in cities){
#   for (n in 1:nsamples){
#     multi_city_ithim[[city]]$outcomes[[n]]<-list.remove(multi_city_ithim[[city]]$outcomes[[n]],'SCEN')
#   }
# }






# save the sampled parameters for all model runs and cities
saveRDS(parameter_samples,'diagnostic/parameter_samples.Rds',version=2)

print('finished ithim-run')

########################################### re-read and extract results #########################################

# set parameters
NSCEN <- length(scenario_names) - 1 # number of scenarios not including baseline scenario
SCEN_SHORT_NAME <- scenario_names
NSAMPLES <- nsamples


# get outputs from ithim run into correct formats
ithim_results <- ithimr::extract_data_for_voi(NSCEN, NSAMPLES, SCEN_SHORT_NAME,outcome_age_groups,cities,multi_city_ithim)

# dataframe for all cities with all outcomes for all model runs, age groups and disease and scenario combinations
voi_data_all_df <- ithim_results$voi_data_all_df

# total yll outcome for all outcome age categories per city and scenario and disease combination, also combined city result (sum)
outcome <- ithim_results$outcome

# save yll per 100,000 people for each city, outcome age category, model run and disease and scen combination
saveRDS(ithim_results$yll_per_hundred_thousand,'results/multi_city/yll_per_hundred_thousand.Rds',version=2)

# save total YLLs
saveRDS(outcome,'results/multi_city/outcome.Rds',version=2)

# save total ylls per 100,000 (median, 5th and 95th percentiles) as sum across all 
# disease per outcome age group, scenario and city (plus combined results)
saveRDS(ithim_results$yll_per_hundred_thousand_stats,'results/multi_city/yll_per_hundred_thousand_quantiles.Rds',version=2)

# save dateframe with total ylls (median, 5th and 95th percentiles) per age group and city (plus combined results)
write.csv(ithim_results$summary_ylls_df,
          paste0('results/multi_city/yll_per_hundred_thousand/Summary_ylls_all_cities','_',output_version,'.csv'), row.names = FALSE)



######################################################### plot results #######################################################
print('plot results')

# plots only work if more than one sample was selected
if(nsamples > 1){
  
  # plot total YLL sum across all diseases 
  {pdf(paste0('results/multi_city/city_yll_',output_version,'.pdf'),height=6,width=6)
    
    # one plot for all cities - might be difficult to read
    par <- par(mar=c(5,5,1,1))
    sp_index <- which(cities==city)
    scen_out <- lapply(outcome[-length(outcome)],function(x)sapply(1:NSCEN,function(y)rowSums(x[,seq(y,ncol(x),by=NSCEN)])))
    means <- sapply(scen_out,function(x)apply(x,2,mean))
    ninefive <- lapply(scen_out,function(x) apply(x,2,quantile,c(0.05,0.95)))
    yvals <- rep(1:length(scen_out),each=NSCEN)/10 + rep(1:NSCEN,times=length(scen_out))
    cols <- rainbow(length(outcome)-1)
    
    plot(as.vector(means),yvals,pch=16,cex=1,frame=F,ylab='',xlab='Change in total YLL relative to baseline',
         col=rep(cols,each=NSCEN),yaxt='n',xlim=range(unlist(ninefive)))
    axis(2,las=2,at=(1+0.1):(NSCEN+0.1),labels=SCEN_SHORT_NAME[2:length(SCEN_SHORT_NAME)])
    for(i in 1:length(outcome[-length(outcome)])) for(j in 1:NSCEN) lines(ninefive[[i]][,j],rep(yvals[j+(i-1)*NSCEN],2),lwd=2,col=cols[i])
    abline(v=0,col='grey',lty=2,lwd=2)
    text(y=(NSCEN-1)+0.2,x=ninefive[[sp_index]][1,(NSCEN-1)],'90%',col='navyblue',adj=c(-0,-0.3*sp_index))
    legend(col=rev(cols),lty=1,bty='n',x= mean(means),legend=rev(names(outcome)[-length(outcome)]),y=NSCEN-1,lwd=2)
    par(par)
    
    # one plot per city
    for(city in cities){ 
      sp_index <- which(cities==city)
      scen_out <- lapply(outcome[-length(outcome)],function(x)sapply(1:NSCEN,function(y)rowSums(x[,seq(y,ncol(x),by=NSCEN)])))
      scen_out_city <- scen_out[[city]]
      means <- colMeans(scen_out_city) 
      ninefive <- apply(scen_out_city,2,quantile,probs = c(0.05,0.95))
      yvals <- rep(1,each=NSCEN)/10 + rep(1:NSCEN) 
      cols <- rainbow(length(outcome)-1)
      col_city <- cols[sp_index]
      
      par_city <- par(mar=c(5,5,1,1))
      xlab <- paste0(city,': Change in total YLL relative to baseline')
      plot(as.vector(means),yvals,pch=16,cex=1,frame=F,ylab='',xlab=xlab,col=rep(col_city,each=NSCEN),
           yaxt='n',xlim=range(unlist(ninefive)))
      axis(2,las=2,at=(1+0.1):(NSCEN+0.1),labels=SCEN_SHORT_NAME[2:length(SCEN_SHORT_NAME)])
      for(j in 1:NSCEN) lines(ninefive[,j],rep(yvals[j],2),lwd=2,col=col_city)
      abline(v=0,col='grey',lty=2,lwd=2)
      text(y=(NSCEN-1)+0.2,x=ninefive[1,(NSCEN-1)],'90%',col='navyblue',adj=c(-0,-0.3*sp_index))
      legend(col=col_city, lty=1,bty='n',x= mean(means),legend=city,y=NSCEN-1,lwd=2)
      par(par_city)
    }
    dev.off()
  } 
  
  
  # plotting the output YLL per person as sums across all cities
  comb_out <- sapply(1:NSCEN,function(y)rowSums(outcome[[length(outcome)]][,seq(y,ncol(outcome[[length(outcome)]]),by=NSCEN)]))
  ninefive <- apply(comb_out,2,quantile,c(0.05,0.95))
  means <- apply(comb_out,2,mean)
  {pdf(paste0('results/multi_city/combined_yll_pp','_',output_version,'.pdf'),height=3,width=6); par(mar=c(5,5,1,1))
    plot(as.vector(means),1:NSCEN,pch=16,cex=1,frame=F,ylab='',xlab='Change in total YLL per person relative to baseline summed across all cities',
         col='navyblue',yaxt='n',xlim=range(ninefive))
    axis(2,las=2,at=1:NSCEN,labels=SCEN_SHORT_NAME[2:length(SCEN_SHORT_NAME)])
    for(j in 1:NSCEN) lines(ninefive[,j],c(j,j),lwd=2,col='navyblue')
    abline(v=0,col='grey',lty=2,lwd=2)
    text(y=(NSCEN-1),x=ninefive[1,(NSCEN-1)],'90%',col='navyblue',adj=c(-0,-0.7))
    dev.off()
  }
  
}




################################################ calculate EVPPI ################################################
if (voi_analysis == T & nsamples > 1){ # only run EVPPI part if there is more than one sample
  
  print('start EVPPI analysis')
  
  parameter_samples <- readRDS('diagnostic/parameter_samples.Rds')
  
  # first extract input parameters of interest
  # create list with global parameters that are relevant for all cities
  general_inputs <- sapply(colnames(parameter_samples),function(x)!grepl(paste(cities, collapse = "|"),x))
  general_parsampl <- parameter_samples[,general_inputs]
  
  # remove alpha, beta, gamma and tmrel dose response parameters as they are not independent of each other
  general_noDRpara <- sapply(colnames(general_parsampl), function(x)!grepl(paste(c('ALPHA','BETA','GAMMA','TMREL'),
                                                                                 collapse = "|"),x))
  general_noDRpara_parsampl <- general_parsampl[,general_noDRpara]
  
  
  
  ########### EVPPI for total YLLs (i.e. summed across the entire population considered in the model
  # by disease and scenario outcome)
  
  evppi_df <- data.frame()
  
  # extract city specific input parameters
  for (city in cities){ # loop through cities
    
    # extract city specific input parameters
    city_inputs <- sapply(colnames(parameter_samples),function(x)grepl(city,x))
    city_parsampl <- parameter_samples[,city_inputs]
    
    # remove CO2 parameters
    city_noCO2para <- sapply(colnames(city_parsampl), function(x)!grepl('CO2',x))
    city_parsampl <- city_parsampl[,city_noCO2para]
    
    # extract the required outcomes for each city
    city_out <- as.data.frame(outcome[[city]]) # take total YLLs for each scenario and disease combination
    city_outputs <- sapply(colnames(city_out),function(x)grepl(paste(outcome_voi_list, collapse = "|"),x))
    city_outcomes <- city_out[,city_outputs]
    
    if(voi_add_sum){
      # add total result for each scenario - only makes sense if results are independent of each other
      # i.e. combining e.g. "total_cancer" with "lung_cancer" results in double-counting and an invalid VOI analysis for the sum
      for (n in 1:NSCEN){
        scen_outputs <- sapply(colnames(city_outcomes), function(x)grepl(paste0("scen",n),x))
        if (length(outcome_voi_list) == 1){
          city_outcomes[paste0('scen',n,"_ylls_sum_",city)] <- city_outcomes[,scen_outputs]
        } else{
          city_outcomes[paste0('scen',n,"_ylls_sum_",city)] <- rowSums(city_outcomes[,scen_outputs])
        }
      }
    }
    
    
    
    param_no <- ncol(city_parsampl) + ncol(general_noDRpara_parsampl)
    
    
    # calculate the evppi for each city (still within the city loop)
    evppi_city <- future_lapply(1:param_no, 
                                FUN = ithimr::compute_evppi,
                                global_para = as.data.frame(general_noDRpara_parsampl),
                                city_para = as.data.frame(city_parsampl),
                                city_outcomes = city_outcomes,
                                nsamples = NSAMPLES)
    
    # evppi_city <- future_lapply(1:param_no, # calculate the evppi for each city
    #                             FUN = compute_evppi,
    #                             global_para = as.data.frame(general_noDRpara_parsampl),
    #                             city_para = as.data.frame(city_parsampl),
    #                             city_outcomes = city_outcomes,
    #                             nsamples = NSAMPLES)
    # 
    evppi_city2 <- do.call(rbind,evppi_city) # bind list
    
    evppi_city3 <- as.data.frame(evppi_city2) # turn into dataframe
    
    # Manipulate evppi_city3 df into correct format
    # add column names without city part
    evppi_outcome_names <- strsplit(colnames(city_outcomes),paste("_",city,sep="")) 
    colnames(evppi_city3) <- evppi_outcome_names
    evppi_outcome_names <- colnames(evppi_city3)
    
    evppi_city3$parameters <-  c(colnames(general_noDRpara_parsampl), colnames(city_parsampl)) # add parameter name column
    evppi_city3$city <- city # add city name column
    
    
    
    # look at dose response AP input parameters separately, as alpha, beta, gammy and trmel are dependent on each other
    if(any(ap_dr_quantile)&&NSAMPLES>=300){
      AP_names <- sapply(colnames(parameter_samples),function(x)length(strsplit(x,'AP_DOSE_RESPONSE_QUANTILE_ALPHA')[[1]])>1)
      diseases <- sapply(colnames(parameter_samples)[AP_names],function(x)strsplit(x,'AP_DOSE_RESPONSE_QUANTILE_ALPHA_')[[1]][2])
      sources <- list()
      for(di in diseases){ 
        col_names <- sapply(colnames(parameter_samples),function(x)grepl('AP_DOSE_RESPONSE_QUANTILE',x)&grepl(di,x))
        sources[[di]] <- parameter_samples[,col_names]
      }
      evppi_for_AP_city <- future_lapply(1:length(sources),
                                         FUN = ithimr:::compute_evppi,
                                         global_para = sources,
                                         city_para = data.frame(),
                                         city_outcomes = city_outcomes,
                                         nsamples = NSAMPLES)
      
      evppi_for_AP_city2 <- do.call(rbind,evppi_for_AP_city) # bind list
      evppi_for_AP_city3 <- as.data.frame(evppi_for_AP_city2) # turn into dataframe
      colnames(evppi_for_AP_city3) <- evppi_outcome_names
      
      evppi_for_AP_city3$parameters <-  c(paste0('AP_DOSE_RESPONSE_QUANTILE_',diseases)) # add parameter name column
      evppi_for_AP_city3$city <- city # add city name column
      
      evppi_city3 <- rbind(evppi_city3, evppi_for_AP_city3)
    }
    
    evppi_df <- rbind(evppi_df, evppi_city3) # add to total evppi dataframe
  } # end of city loop
  
  
  evppi_df <- evppi_df %>% relocate(city,parameters) # change order of columns
  
  saveRDS(evppi_df,'results/multi_city/evppi.Rds',version=2) # save evppi dataframe
  
  evppi_csv <- paste0('results/multi_city/evppi_',output_version,".csv")
  #write.csv(evppi_df,'results/multi_city/evppi.csv',row.names = FALSE) # save as csv file
  
  write.csv(evppi_df,evppi_csv,row.names = FALSE) # save as csv file
  
  
  
  
  # create output plots
  
  output_pdf <- paste0('results/multi_city/evppi_',output_version,".pdf")
  #{pdf('results/multi_city/evppi.pdf',height=15,width=4+length(outcome_voi_list))
  {pdf(output_pdf,height=15,width=4+length(outcome_voi_list)+1)
    for ( city_name in cities){
      
      evppi_city_df <- evppi_df %>% filter(city == city_name) 
      
      par_city <- par(mar=c(10,13,4,3.5))
      
      labs <- evppi_city_df$parameters # y axis label
      labs <- str_replace(labs,'DOSE_RESPONSE','DR') # replace DOSE_RESPONSE with DR
      labs <- str_replace(labs,'EMISSION_INVENTORY','EMISSION_INV') # replace EMISSION_INVENTORY with EMISSION_INV
      evppi_dummy <- evppi_city_df[,evppi_outcome_names]
      # for plotting purposes, replace all NaN with 0
      evppi_dummy[is.na(evppi_dummy)] <- 0
      get.pal=colorRampPalette(brewer.pal(9,"Reds"))
      redCol=rev(get.pal(12))
      bkT <- seq(max(evppi_dummy[!is.na(evppi_dummy)])+1e-10, 0,length=13)
      cex.lab <- 1.0
      maxval <- round(bkT[1],digits=1)
      col.labels<- c(0,maxval/2,maxval)
      cellcolors <- vector()
      title <- paste(city_name,  " - No of samples: ", nsamples, 
                     # ': By how much (%) could we\n reduce uncertainty in the outcome\n if we knew this parameter perfectly?')
                     '- By how much (%) could we reduce\n uncertainty in the outcome if we knew this parameter perfectly?')
      for(ii in 1:length(unlist(evppi_dummy))) # determine the cellcolors
        cellcolors[ii] <- redCol[tail(which(unlist(evppi_dummy)[ii]<bkT),n=1)]
      color2D.matplot(evppi_dummy,cellcolors=cellcolors,xlab="",ylab="",axes=F,border='white')
      title(title, adj = 0, cex.main = 0.7 )
      fullaxis(side=1,at=(ncol(evppi_dummy)-1):0+0.5,labels=rev(colnames(evppi_dummy)),
               las = 2, line=NA,pos=NA,outer=FALSE,font=NA,lwd=0,cex.axis=0.65)  # x-axis labels
      fullaxis(side=2,las=1,at=(length(labs)-1):0+0.5,labels=labs,
               line=NA,pos=NA,outer=FALSE,font=NA,lwd=0,cex.axis=0.6) # y-axis labels
      color.legend(ncol(evppi_dummy)+0.5,0,ncol(evppi_dummy)+1.2,length(labs),col.labels,rev(redCol),
                   gradient="y",cex=0.7,align="rb")
      for(i in seq(0,ncol(evppi_dummy),by=NSCEN)) abline(v=i, lwd=2) # add vertical lines
      for(i in c(0,length(labs))) abline(h=i, lwd = 2) # add horizontal lines at top and bottom
      par(par_city)
      
    }
    dev.off()}
  
  
  
  ##### run EVPPI for different age groups and gender
  
  if(voi_age_gender){
    
    print('starting EVPPI analysis by age and sex')
    evppi_agesex_df <- data.frame()
    
    for (city in cities){
      print(city)
      
      # extract city specific input parameters
      city_inputs <- sapply(colnames(parameter_samples),function(x)grepl(city,x))
      city_parsampl <- parameter_samples[,city_inputs]
      
      param_no <- ncol(city_parsampl) + ncol(general_noDRpara_parsampl)
      
      # extract the required outcomes for each city - loop through age and gender categories
      city_name <- city
      city_agesex_out <- voi_data_all_df %>% filter(city == city_name)
      age_gender_cat <- unique(city_agesex_out$age_sex)
      
      k <- 1
      
      for(age_gender in age_gender_cat){
        
        city_agesex_out2 <- city_agesex_out %>% filter(age_sex == age_gender)
        city_agesex_outputs <- sapply(colnames(city_agesex_out2),function(x)grepl(paste(outcome_voi_list, collapse = "|"),x))
        city_agesex_outcomes <- city_agesex_out2[,city_agesex_outputs]
        
        if(voi_add_sum){
          # add total result for each scenario - only makes sense if results are independent of each other
          # i.e. combining e.g. "total_cancer" with "lung_cancer" results in double-counting and invalid VOI analysis for the sum
          for (n in 1:NSCEN){
            scen_outputs <- sapply(colnames(city_agesex_outcomes), function(x)grepl(paste0("scen",n),x))
            if (length(outcome_voi_list) == 1){
              city_agesex_outcomes[paste0('scen',n,"_ylls_sum_",city)] <- sapply(city_agesex_outcomes[,scen_outputs], unlist)
            } else{
              city_agesex_outcomes[paste0('scen',n,"_ylls_sum_",city)] <- rowSums(sapply(city_agesex_outcomes[,scen_outputs], unlist))
            }
          }
        }
        
        # replace NA with 0s, note that the evppi analysis returns NA for all outcomes that are all 0
        city_agesex_outcomes_na_cols <- names(which(colSums(is.na(city_agesex_outcomes))>0)) # record colnames
        city_agesex_outcomes[is.na(city_agesex_outcomes)] <- 0 # replace NAs with 0
        
        # calculate evppi
        evppi_agesex_city <- future_lapply(1:param_no, # calculate the evppi for each city
                                           FUN = ithimr::compute_evppi,
                                           global_para = as.data.frame(general_noDRpara_parsampl),
                                           city_para = as.data.frame(city_parsampl),
                                           city_outcomes = city_agesex_outcomes,
                                           nsamples = NSAMPLES)
        
        
        evppi_agesex_city2 <- do.call(rbind,evppi_agesex_city)
        evppi_agesex_city3 <- as.data.frame(evppi_agesex_city2) # turn into dataframe
        
        evppi_agesex_outcome_names <- strsplit(colnames(city_agesex_outcomes),paste("_",city,sep="")) # add column names without city part
        colnames(evppi_agesex_city3) <- paste(evppi_agesex_outcome_names, age_gender, sep = "_")
        evppi_agesex_outcome_names <- colnames(evppi_agesex_city3)
        
        # replace columns for which the outcomes where originally NA by NA again
        if (length(city_agesex_outcomes_na_cols)>0){
          city_agesex_outcomes_na_cols2 <- strsplit(city_agesex_outcomes_na_cols,paste("_",city,sep=""))
          city_agesex_outcomes_na_cols3 <- paste(city_agesex_outcomes_na_cols2,age_gender, sep = "_")
          
          evppi_agesex_city3[,city_agesex_outcomes_na_cols3] <- NaN
        }
        
        if(k == 1){
          evppi_agesex_city_df <- evppi_agesex_city3
        }else{evppi_agesex_city_df <- cbind(evppi_agesex_city_df, evppi_agesex_city3)}
        k <- k + 1
      }
      
      evppi_agesex_city_df$parameters <-  c(colnames(general_noDRpara_parsampl), colnames(city_parsampl)) # add parameter name column
      evppi_agesex_city_df$city <- city # add city name column
      
      
      # look at dose response input parameters separately, as alpha, beta, gammy and trmel are dependent on each other
      if(any(ap_dr_quantile)&&NSAMPLES>=300){
        AP_names <- sapply(colnames(parameter_samples),function(x)length(strsplit(x,'AP_DOSE_RESPONSE_QUANTILE_ALPHA')[[1]])>1)
        diseases <- sapply(colnames(parameter_samples)[AP_names],function(x)strsplit(x,'AP_DOSE_RESPONSE_QUANTILE_ALPHA_')[[1]][2])
        sources <- list()
        for(di in diseases){
          col_names <- sapply(colnames(parameter_samples),function(x)grepl('AP_DOSE_RESPONSE_QUANTILE',x)&grepl(di,x))
          sources[[di]] <- parameter_samples[,col_names]
        }
        
        k <- 1
        
        for(age_gender in age_gender_cat){
          
          city_agesex_out2 <- city_agesex_out %>% filter(age_sex == age_gender)
          city_agesex_outputs <- sapply(colnames(city_agesex_out2),function(x)grepl(paste(outcome_voi_list, collapse = "|"),x))
          city_agesex_outcomes <- city_agesex_out2[,city_agesex_outputs]
          
          
          if(voi_add_sum){
            # add total result for each scenario - only makes sense if results are independent of each other
            # i.e. combining e.g. "total_cancer" with "lung_cancer" results in double-counting and invalid VOI analysis for the sum
            for (n in 1:NSCEN){
              scen_outputs <- sapply(colnames(city_agesex_outcomes), function(x)grepl(paste0("scen",n),x))
              if (length(outcome_voi_list) == 1){
                city_agesex_outcomes[paste0('scen',n,"_ylls_sum_",city)] <- sapply(city_agesex_outcomes[,scen_outputs], unlist)
              } else{
                city_agesex_outcomes[paste0('scen',n,"_ylls_sum_",city)] <- rowSums(sapply(city_agesex_outcomes[,scen_outputs], unlist))
              }
            }
          }
          
          # replace NA with 0s, note that the evppi analysis returns NA for all outcomes that are all 0
          city_agesex_outcomes_na_cols <- names(which(colSums(is.na(city_agesex_outcomes))>0)) # record colnames
          city_agesex_outcomes[is.na(city_agesex_outcomes)] <- 0 # replace NAs with 0
          
          evppi_agesex_for_AP_city <- future_lapply(1:length(sources),
                                                    FUN = ithimr:::compute_evppi,
                                                    global_para = sources,
                                                    city_para = data.frame(),
                                                    city_outcomes = city_agesex_outcomes,
                                                    nsamples = NSAMPLES)
          
          evppi_agesex_for_AP_city2 <- do.call(rbind,evppi_agesex_for_AP_city)
          
          evppi_agesex_for_AP_city3 <- as.data.frame(evppi_agesex_for_AP_city2) # turn into dataframe
          
          
          evppi_agesex_AP_outcome_names <- strsplit(colnames(city_agesex_outcomes),paste("_",city,sep="")) # add column names without city part
          colnames(evppi_agesex_for_AP_city3) <- paste(evppi_agesex_AP_outcome_names, age_gender, sep = "_")
          #evppi_agesex_AP_outcome_names <- colnames(evppi_agesex_for_AP_city3)
          
          # replace columns for which the outcomes where originally NA by NA again
          if (length(city_agesex_outcomes_na_cols)>0){
            city_agesex_outcomes_na_cols2 <- strsplit(city_agesex_outcomes_na_cols,paste("_",city,sep=""))
            city_agesex_outcomes_na_cols3 <- paste(city_agesex_outcomes_na_cols2,age_gender, sep = "_")
            evppi_agesex_for_AP_city3[,city_agesex_outcomes_na_cols3] <- NaN
          }          
          
          
          if(k == 1){
            evppi_agesex_ap_city_df <- evppi_agesex_for_AP_city3
          } else{
            evppi_agesex_ap_city_df <- cbind(evppi_agesex_ap_city_df, evppi_agesex_for_AP_city3)
          }
          k <- k + 1
        }
        
        
        evppi_agesex_ap_city_df$parameters <-  c(paste0('AP_DOSE_RESPONSE_QUANTILE_',diseases)) # add parameter name column
        evppi_agesex_ap_city_df$city <- city # add city name column
        
        evppi_agesex_city_df <- rbind(evppi_agesex_city_df, evppi_agesex_ap_city_df)
        
      }
      
      # change structure of evppi_agesex_city_df to make it more flexible when different age categories are used for different cities
      # create one df for each city
      assign(paste0("evppi_agesex_",city_name,'_df'), evppi_agesex_city_df)
      evppi_agesex_city_df_rearranged <- data.frame()
      k <- 1
      
      for (ag in age_gender_cat){
        ag_colnames <- sapply(colnames(evppi_agesex_city_df),function(x)grepl(paste0("_",ag),x)) # find column names depending on age and gender
        ag_columns_df <- evppi_agesex_city_df[,ag_colnames] # only keep those columns that are dependent on age and gender
        new_col_names <- sapply(colnames(ag_columns_df),function(x)strsplit(x,paste0('_',ag))[[1]]) # remove age and gender part from those columns
        colnames(ag_columns_df) = new_col_names
        
        ag_columns_df$parameters <- evppi_agesex_city_df$parameters
        ag_columns_df$city <- evppi_agesex_city_df$city
        ag_columns_df$gender <- strsplit(ag, " ")[[1]][1]
        ag_columns_df$age <- strsplit(ag, " ")[[1]][2]
        ag_columns_df$age_gender <- ag
        if (k == 1){
          evppi_agesex_city_df_rearranged <- ag_columns_df
        }else{
          evppi_agesex_city_df_rearranged <- rbind(evppi_agesex_city_df_rearranged, ag_columns_df)
        }
        k <- k +1
      }
      
      evppi_agesex_df <- rbind(evppi_agesex_df, evppi_agesex_city_df_rearranged) # add to total evppi dataframe
    } # end of city loop
    
    
    # merge with evppi_df data
    evppi_df$gender <- 'all'
    evppi_df$age <- 'all'
    evppi_df$age_gender <- 'all'
    evppi_agesex_df <- rbind(evppi_agesex_df,evppi_df)
    
    
    # change order of columns such that ordered by scenario and demographic group
    evppi_agesex_df <- evppi_agesex_df %>% relocate(city,parameters, age, gender, age_gender)
    
    # re-order rows
    evppi_agesex_df <- evppi_agesex_df[order(evppi_agesex_df$city, evppi_agesex_df$age_gender),]
    
    
    saveRDS(evppi_agesex_df,'results/multi_city/evppi_agesex.Rds',version=2) 
    
    evppi_agesex_csv <- paste0('results/multi_city/evppi_agesex_',output_version,".csv")
    #write.csv(evppi_df,'results/multi_city/evppi.csv',row.names = FALSE) # save as csv file
    
    write.csv(evppi_agesex_df,evppi_agesex_csv,row.names = FALSE) # save as csv file
    
    
    
    # create output plots
    output_pdf <- paste0('results/multi_city/evppi_agesex_',output_version,".pdf")
    #{pdf('results/multi_city/evppi.pdf',height=15,width=4+length(outcome_voi_list))
    {pdf(output_pdf,height=15,width=4+length(age_gender_cat)+1)
      for ( city_name in cities){
        
        evppi_agesex_city_df <- get(paste0("evppi_agesex_",city_name,'_df'))
        
        if (voi_add_sum){outcome_list <- c(outcome_voi_list, 'sum') 
        }else{ outcome_list <- outcome_voi_list}
        
        # loop through each outcome in outcome_voi_list separately and create one page per outcome and city
        for (out in outcome_list){
          
          outcome_cols <- sapply(colnames(evppi_agesex_city_df),function(x) grepl(paste(out, collapse = "|"),x))
          evppi_agesex_city_outcome_df <- evppi_agesex_city_df[,outcome_cols]
          par_city <- par(mar=c(14,12.5,4,3.5))
          
          labs <- evppi_agesex_city_df$parameters # y axis label
          labs <- str_replace(labs,'DOSE_RESPONSE_QUANTILE','DR_QUANT') # replace DOSE_RESPONSE with DR
          labs <- str_replace(labs,'EMISSION_INVENTORY','EMISSION_INV') # replace EMISSION_INVENTORY with EMISSION_INV
          evppi_dummy <- evppi_agesex_city_outcome_df
          # for plotting purposes, replace all NaN with 0
          evppi_dummy[is.na(evppi_dummy)] <- 0
          get.pal=colorRampPalette(brewer.pal(9,"Reds"))
          redCol=rev(get.pal(12))
          bkT <- seq(max(evppi_dummy[!is.na(evppi_dummy)])+1e-10, 0,length=13)
          cex.lab <- 1.0
          maxval <- round(bkT[1],digits=1)
          col.labels<- c(0,maxval/2,maxval)
          cellcolors <- vector()
          title <- paste(city_name, "- ", out, ", No of samples: ", nsamples, 
                         # ': By how much (%) could we\n reduce uncertainty in the outcome\n if we knew this parameter perfectly?')
                         '- \nBy how much (%) could we reduce uncertainty in the outcome if we knew this parameter perfectly?')
          for(ii in 1:length(unlist(evppi_dummy))) # determine the cellcolors
            cellcolors[ii] <- redCol[tail(which(unlist(evppi_dummy)[ii]<bkT),n=1)]
          color2D.matplot(evppi_dummy,cellcolors=cellcolors,xlab="",ylab="",axes=F,border='white')
          title(title, adj = 0, cex.main = 0.7 )
          fullaxis(side=1,at=(ncol(evppi_dummy)-1):0+0.5,labels=rev(colnames(evppi_dummy)),
                   las = 2, line=NA,pos=NA,outer=FALSE,font=NA,lwd=0,cex.axis=0.6)  # x-axis labels
          fullaxis(side=2,las=1,at=(length(labs)-1):0+0.5,labels=labs,
                   line=NA,pos=NA,outer=FALSE,font=NA,lwd=0,cex.axis=0.5) # y-axis labels
          color.legend(ncol(evppi_dummy)+0.5,0,ncol(evppi_dummy)+1.2,length(labs),col.labels,rev(redCol),
                       gradient="y",cex=0.7,align="rb")
          for(i in seq(0,ncol(evppi_dummy),by=length(age_gender_cat)+1)) abline(v=i, lwd=2) # add vertical lines
          for(j in 1:NSCEN){
            for(i in seq(0 + (j-1)*(length(age_gender_cat)+1),j*(length(age_gender_cat)+1),by = 2)) abline(v=i, lwd=1, lty=5)} # add vertical dashed
          for(i in c(0,length(labs))) abline(h=i, lwd = 2) # add horizontal lines at top and bottom
          par(par_city)
        }
      }
      dev.off()}
    
  } # end of age / gender VOI analysis
  
  
} # end of nsamples >1 condition  and end of Voi analysis

# record time it took to run code
endtime <- Sys.time()  
runtime <- round(as.numeric(difftime(endtime, starttime, units = "mins")),2)

# add to output control document
if (write_output_control == TRUE){
  
  input_version <- input_parameter_file
  global_path <- paste0(file.path(find.package('ithimr',lib.loc = .libPaths()),
                                  'extdata/global'), "/")
  
  cat("",
      paste(timestamp, "by", author, sep = " "),
      paste("Cities:", cities, sep = " "),
      paste("Input parameter file:", input_version, sep = " "),
      paste("Version number of outputs:", output_version, sep = " "),
      paste("Number of samples:", '1', sep = " "),
      paste("Comments:", comment, sep=" "),
      paste("Path of other input files:", global_path, sep=" "),
      paste("Runtime in minutes:", runtime, sep=" "),
      file="OutputVersionControl.txt",sep="\n",append=TRUE)
  
}

print(paste0("The code took ", runtime, " minutes to run"))  

