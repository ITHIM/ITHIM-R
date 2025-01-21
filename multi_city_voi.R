#' Main script to run ITHIM Global in sampling mode
#' 
#' Script to run ITHIM Global using input parameter distributions. Outputs the health impacts associated with transport in a given city
#' via an air pollution, physical activity and injury pathway. Also performs a Value of Information analysis to calculate the the 
#' expected values of partially perfect information, i.e the reduction in variance in the outcome were we to know 
#' an input parameter or several interdependent input parameters exactly.
#' 
#' The ITHIM Global main sampling script works as follows:
#' 
#' - the following variables need to be defined before running the script:
#'    - the name(s) of the city or cities for which the model is to be run
#'    - the number of samples to be drawn from the input parameters distributions which 
#'      equals the number of model runs 
#'    - whether a VoI analysis is to be performed, if yes, also set the following parameters:
#'      - define the list of outcomes for which the VoI analysis is to be performed
#'      - define whether you want to run the VoI analysis split by sex 
#'      - define whether you want to run the VoI analysis split by sex and age
#'      - define whether you want to calculate the sum of the outcomes of interest and whether
#'        whether to include this sum in the VoI analysis
#'    - The input parameter file name containing the global and local input parameters and their distribution parameters  
#'    - define the output_version
#'    - define whether to write the key aspect of the model run to the OutputVersionControl.txt file which
#'      documents the key aspects of the model run (timestamp, author name, cities for which model was run, 
#'      input parameter file name, output version number, number of samples, and any comments. If yes, also define
#'      - author name
#'      - any comments that are to be written to the file
#'    - The scenarios need defining by:
#'      - updating the character defining which scenario script is to be called
#'      - giving the reference scenario against which all other scenarios are compared,
#'        this reference scenario needs to be the scenario name which corresponds to the current input parameter files
#'      - giving the percentage increase in each mode for the BOGOTA (GLOBAL, LATAM, and AFRICA_INDIA) scenarios
#'      
#' - the remainder of the code does not need to be changed:
#' 
#' - local and global input parameters from the input parameter spreadsheet are read in and put into the correct format needed
#'   for the model run
#'   
#' - The\code{\link{run_ithim_setup()}} script is called which prepares the input data needed for the health impact assessment
#'   and samples for the input parameter distributions
#' 
#' - The \code{\link{run_ithim()}} script is called which performs the health impact assessment NSAMPLE times
#' 
#' - The \code{\link{extract_data_for_voi()}} function is called which gets the data into the correct
#'   format for plotting and the VoI analysis
#' 
#' - Plots are created for all cities and each city individually showing the total YLL outcomes and their 
#'   95% confidence intervals for each scenario ('results/voi/city_yll_',output_version,'.pdf')
#'   
#' - Plots are created for each city individually showing the total YLL outcomes for the entire population and 
#'   both sexes and their 95% confidence intervals for each scenario ('results/voi/city_yll_sex',output_version,'.pdf')
#' 
#' - One plot is created showing the change in total YLL per person relative to the baseline summed 
#'   across all cities ('results/voi/combined_yll_pp','_',output_version,'.pdf')
#'
#' - if required the VoI analysis is started:
#'   - EVPPI values for the different input parameters of the total population outcomes are calculated for
#'     each city ('results/voi/evppi_',output_version,".csv")
#'   - EVPPI values are plotted for each city ('results/voi/evppi_',output_version,".pdf")
#'   - if required the VoI analysis by sex is started:
#'      - EVPPI values for the different input parameters of the total population outcomes by sex are 
#'        calculated for each city ('results/voi/evppi_sex_',output_version,".csv")
#'      - EVPPI values are plotted for each city ('results/voi/evppi_sex_',output_version,".pdf")
#'   - if required the VoI analysis by sex and age group is started:
#'      - EVPPI values for the different input parameters of the total population outcomes by sex and age group are 
#'        calculated for each city (results/voi/evppi_agesex_',output_version,".csv")
#'      - EVPPI values are plotted for each city and outcome ('results/voi/evppi_agesex_',output_version,".pdf")
#'
#' - The OutputVersionControl.txt file is updated if needed
#'
#'
#'
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
library(janitor)

if (!require("drpa",character.only = TRUE)) {
  print('Installing "drpa" package...')
  remotes::install_github("meta-analyses/drpa")
  library(drpa)
  print("")
}

rm(list=ls())
# 
# cities <- c('belo_horizonte', 'bogota', 'buenos_aires',
#             'cali',  'medellin', 'mexico_city', 'montevideo',
#             'santiago', 'sao_paulo', 'accra', 'bangalore', 'cape_town','delhi',
#             'vizag', 'kisumu', 'nairobi', 'port_louis')

# cities <- c('antofagasta', 'arica', 'belo_horizonte', 'bogota', 'buenos_aires',
#             'cali', 'copiapo', 'coquimbo_laserena', 'gran_valparaiso',
#             'iquique_altohospicio', 'medellin', 'mexico_city', 'montevideo',
#             'osorno', 'puerto_montt', 'san_antonio',
#             'santiago', 'sao_paulo', 'temuco_padrelascasas', 'valdivia',
#             'accra', 'bangalore', 'cape_town','delhi', 'vizag', 'kisumu', 'nairobi', 'port_louis')

cities <- c('bogota')

# number of times input values are sampled from each input parameter distribution
nsamples <- 10


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

# flag whether to run VOI analysis split gender
voi_gender <- T # set to T if want to include split and to F otherwise

# flag whether to run VOI analysis split by age and gender 
voi_age_gender <- T # set to T if want to include split and to F otherwise

# add total across all outputs in VOI list for each scenario - only makes sense if results are independent of each other
# i.e. combining e.g. "total_cancer" with "lung_cancer" results in double-counting and invalid VOI analysis for the sum
voi_add_sum <- T


input_parameter_file <- "InputParameters_v41.0.xlsx"


## Get the current repo sha
gitArgs <- c("rev-parse", "--short", "HEAD", ">", file.path("repo_sha"))
# Use shell command for Windows as it's failing with system2 for Windows (giving status 128)
if (.Platform$OS.type == "windows"){
  shell(paste(append("git", gitArgs), collapse = " "), wait = T)
} else {
  system2("git", gitArgs, wait = T)
}
repo_sha <-  as.character(readLines(file.path("repo_sha")))

#output_version <- paste0(repo_sha, "_test_run") # gives the version number of the output documents, independent of the input parameter file name
output_version <- 'bogota_10samples'

# records the main aspects of an ithim run in the OutputVersionControl.txt document
# text file records timestamp of run, author name, cities the script is run for, 
# the input parameter file version used, the output version, 
# the number of samples (which is 1 in constant mode), the path to any other input files,
# any comments and the runtime of the code
write_output_control = T # whether you want to save the model run specifics or not
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
normVariables <- c('CYCLING_MMET',
                   'WALKING_MMET', 
                   'PASSENGER_MMET',
                   'CAR_DRIVER_MMET',
                   'MOTORCYCLIST_MMET',
                   'SEDENTARY_ACTIVITY_MMET',
                   'LIGHT_ACTIVITY_MMET',
                   'MODERATE_PA_MMET',
                   'VIGOROUS_PA_MMET',
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
     background_pa_scalar,background_pa_confidence,cycling_mmet,walking_mmet,passenger_mmet,
     car_driver_mmet,motorcyclist_mmet,sedentary_activity_mmet,light_activity_mmet,moderate_pa_mmet,
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
                                                TREAT_TAXI_AS_CAR = as.logical(treat_taxi_as_car[[city]]),
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
                                                CYCLING_MMET =	cycling_mmet,
                                                WALKING_MMET =	walking_mmet,
                                                PASSENGER_MMET =	passenger_mmet,
                                                CAR_DRIVER_MMET =	car_driver_mmet,
                                                MOTORCYCLIST_MMET =	motorcyclist_mmet,
                                                SEDENTARY_ACTIVITY_MMET =	sedentary_activity_mmet,
                                                LIGHT_ACTIVITY_MMET =	light_activity_mmet,
                                                MODERATE_PA_MMET =	moderate_pa_mmet,
                                                VIGOROUS_PA_MMET	= vigorous_pa_mmet,
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
    
    #multi_city_ithim[[city]]$outcomes <- run_ithim(ithim_object=multi_city_ithim[[city]], seed = 1)
    
    multi_city_ithim[[city]]$DEMOGRAPHIC <- DEMOGRAPHIC
    
    scenario_names <- multi_city_ithim[[city]]$outcome[[1]]$SCEN
    
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
    
   
  
    
    # save results for city and then delete
    saveRDS(multi_city_ithim[[city]],paste0('results/voi/',city,'_',output_version,'.Rds'))
    
    # if(ci>1){
    #   multi_city_ithim[[ci]] <- 0
    # }else{
    #   multi_city_ithim[[ci]]$outcomes <- 0
    # }
  }
)) 




# add relevant model run information to  multi_city_ithim list
timestamp <- Sys.time()
multi_city_ithim$ithim_run <- list()
multi_city_ithim$ithim_run$input_parameter_file <- input_parameter_file
multi_city_ithim$ithim_run$scenarios_used <- scenario_name
multi_city_ithim$ithim_run$reference_scenario <- reference_scenario
multi_city_ithim$ithim_run$scenario_increase <- scenario_increase
multi_city_ithim$ithim_run$scenario_names <- scenario_names
multi_city_ithim$ithim_run$compute_mode <- compute_mode
multi_city_ithim$ithim_run$timestamp <- timestamp
multi_city_ithim$ithim_run$output_version <- output_version
multi_city_ithim$ithim_run$author <- author
multi_city_ithim$ithim_run$comment <- comment


# save the sampled parameters for all model runs and cities
saveRDS(parameter_samples,paste('diagnostic/parameter_samples_',output_version,'.Rds'),version=2)

print('finished ithim-run')

########################################### re-read and extract results #########################################

# set parameters
NSCEN <- length(scenario_names) - 1 # number of scenarios not including baseline scenario
SCEN_SHORT_NAME <- scenario_names
NSAMPLES <- nsamples

# get outputs from ithim run into correct formats
ithim_results <- ithimr::extract_data_for_voi(NSCEN, NSAMPLES, SCEN_SHORT_NAME,outcome_age_groups,cities,multi_city_ithim, output_version)

# dataframe for all cities with all outcomes for all model runs, age groups and disease and scenario combinations
voi_data_all_df <- ithim_results$voi_data_all_df

# dataframe for all cities with all outcomes for all model runs, sexes and disease and scenario combinations
voi_data_all_sex_df <- ithim_results$voi_data_all_sex_df

# total yll outcome for all outcome age categories per city and scenario and disease combination, also combined city result (sum)
outcome <- ithim_results$outcome

# dataframe containing all the population numbers
population_df <- ithim_results$population_df

# save yll per 100,000 people for each city, outcome age category, model run and disease and scen combination
saveRDS(ithim_results$yll_per_hundred_thousand,paste0('results/voi/yll_per_hundred_thousand_',output_version,'.Rds'),version=2)

# save total YLLs
saveRDS(outcome,paste0('results/voi/outcome_',output_version,'.Rds'),version=2)

# save total ylls per 100,000 (median, 5th and 95th percentiles) as sum across all 
# disease per outcome age group, scenario and city (plus combined results)
saveRDS(ithim_results$yll_per_hundred_thousand_stats,paste0('results/voi/yll_per_hundred_thousand_quantiles_',output_version,'.Rds'),version=2)

# save dateframe with total ylls (median, 5th and 95th percentiles) per age group and city (plus combined results)
write.csv(ithim_results$summary_ylls_df,
          paste0('results/voi/Summary_ylls_all_cities','_',output_version,'.csv'), row.names = FALSE)



################# create outcome statistics for different scenarios and health outcomes

# number the different runs
voi_data_all_df$run <- rep(1:nsamples,length(unique(voi_data_all_df$age_sex))*length(cities))
voi_data_all_sex_df$run <- rep(1:nsamples,length(unique(voi_data_all_sex_df$sex))*length(cities))

# add extra columns
voi_data_all_sex_df$age_cat <- 'all'
voi_data_all_sex_df$age_sex <- paste(voi_data_all_sex_df$sex, voi_data_all_sex_df$age_cat, sep = ' ')

# create total for each run for all outcomes
voi_data_total_df <- voi_data_all_sex_df %>% group_by( age_cat, run, city) %>% summarise(across(where(is.numeric), sum), .groups = 'drop')
voi_data_total_df$sex <- 'all'
voi_data_total_df$age_sex <- paste(voi_data_total_df$sex, voi_data_total_df$age_cat, sep = ' ')


# create one dataset
voi_data_all_df$sex <- as.character(voi_data_all_df$sex)
voi_data_all_df$age_cat <- as.character(voi_data_all_df$age_cat)
voi_data_all_df <- voi_data_all_df %>% mutate_if(is.list,as.numeric)
voi_data_complete <- bind_rows(voi_data_all_df, voi_data_all_sex_df , voi_data_total_df)

# add population data
population_df <- population_df %>% rename(age_cat = age)
voi_data_complete2 <- merge(voi_data_complete, population_df, by = c('age_cat','sex','city'))

# calculate data for various levels

# names of all scenarios excluding base
scen_only_names <- scenario_names[2:length(scenario_names)]

# different levels
level1 <-c('pa_ap_all_cause','inj')
level2 <- c('pa_total_cancer','pa_ap_CVD','ap_respiratory','inj')
level3 <- c('pa_ap_IHD','pa_ap_lung_cancer','ap_COPD','pa_ap_stroke','pa_ap_T2D','ap_LRI',
            'pa_breast_cancer','pa_colon_cancer','pa_endo_cancer','pa_liver_cancer','pa_total_dementia',
            'pa_myeloma','pa_Parkinson','pa_head_neck_cancer', 'pa_stomach_cancer',
            'pa_myeloid_leukemia','inj')

level_list <- list(level1, level2, level3)

# loop through levels
all_levels <- data.frame()
l<-1
for (level in list(level1, level2, level3)){
  print(level)
  dummy_level_df <- voi_data_complete2 %>% dplyr::select(matches(level))

  # loop trough scenarios and calculate sum
  for (scen in scen_only_names){
    dummy_level_df <- dummy_level_df %>%
      mutate(sum = rowSums(pick(matches(scen))))  %>% # calcualte sum
      rename_with(~paste0(scen, '_ylls_level',l), .cols= sum) # re-name new column
        
  }
  if (l ==1){
    all_levels <- dummy_level_df %>% dplyr::select(!matches(level))
  } else{
    all_levels <- cbind(all_levels, dummy_level_df%>% dplyr::select(!matches(level)))
  }
  l <- l +1
}

# add levels to dataframe
voi_data_complete3 <- cbind(voi_data_complete2, all_levels)


# calculate per 100k
voi_data_complete3_100k <- voi_data_complete3 %>% mutate(across(where(is.numeric) & !run & !population, ~round(as.numeric(.x)/population*100000,4)))


# add summary statistics
#voi_data_complete3_mean <- voi_data_complete3 %>% group_by(city, age_cat, sex, age_sex, population) %>% summarise(across(where(is.numeric), sum), .groups = 'drop')


# add summary statistics - 100k




######################################################### plot results #######################################################
print('plot results')

# plots only work if more than one sample was selected
if(nsamples > 1){
  
  # plot total YLL sum
  {pdf(paste0('results/voi/city_yll_',output_version,'.pdf'),height=6,width=6)
    
    # one plot for all cities - might be difficult to read
    par <- par(mar=c(5,5,1,1))
    sp_index <- which(cities==city)
    scen_out <- lapply(outcome[-length(outcome)],function(x)sapply(1:NSCEN,function(y)rowSums(x[,seq(y,ncol(x),by=NSCEN)])))
    means <- sapply(scen_out,function(x)apply(x,2,mean))
    ninefive <- lapply(scen_out,function(x) apply(x,2,quantile,c(0.025,0.975)))
    yvals <- rep(1:length(scen_out),each=NSCEN)/10 + rep(1:NSCEN,times=length(scen_out))
    cols <- rainbow(length(outcome)-1)
    
    plot(as.vector(means),yvals,pch=16,cex=1,frame=F,ylab='',xlab='Change in total YLL relative to baseline',
         col=rep(cols,each=NSCEN),yaxt='n',xlim=range(unlist(ninefive)))
    axis(2,las=2,at=(1+0.1):(NSCEN+0.1),labels=SCEN_SHORT_NAME[2:length(SCEN_SHORT_NAME)])
    for(i in 1:length(outcome[-length(outcome)])) for(j in 1:NSCEN) lines(ninefive[[i]][,j],rep(yvals[j+(i-1)*NSCEN],2),lwd=2,col=cols[i])
    abline(v=0,col='grey',lty=2,lwd=2)
    text(y=(NSCEN-1)+0.2,x=ninefive[[sp_index]][1,(NSCEN-1)],'95%',col='navyblue',adj=c(-0,-0.3*sp_index))
    legend(col=rev(cols),lty=1,bty='n',x= mean(means),legend=rev(names(outcome)[-length(outcome)]),y=NSCEN-1,lwd=2)
    par(par)
    
    # one plot per city
    for(city in cities){ 
      sp_index <- which(cities==city)
      scen_out <- lapply(outcome[-length(outcome)],function(x)sapply(1:NSCEN,function(y)rowSums(x[,seq(y,ncol(x),by=NSCEN)])))
      scen_out_city <- scen_out[[city]]
      means <- colMeans(scen_out_city) 
      ninefive <- apply(scen_out_city,2,quantile,probs = c(0.025,0.975))
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
      text(y=(NSCEN-1)+0.2,x=ninefive[1,(NSCEN-1)],'95%',col='navyblue',adj=c(-0,-0.3*sp_index))
      legend(col=col_city, lty=1,bty='n',x= mean(means),legend=city,y=NSCEN-1,lwd=2)
      par(par_city)
    }
    dev.off()
  } 
  

  # plot total YLL sum plus include split by sex (95% CIs)
  {pdf(paste0('results/voi/city_yll_sex_',output_version,'.pdf'),height=6,width=6)
  
  # one plot per city
    for(city in cities){ 
      sp_index <- which(cities==city)
      scen_out <- lapply(outcome[-length(outcome)],function(x)sapply(1:NSCEN,function(y)rowSums(x[,seq(y,ncol(x),by=NSCEN)])))
      scen_out_city <- scen_out[[city]]
      means <- colMeans(scen_out_city) 
      ninefive <- apply(scen_out_city,2,quantile,probs = c(0.025,0.975))
      yvals <- rep(3,each=NSCEN)/10 + rep(1:NSCEN) 
      cols <- rainbow(length(outcome)-1)
      col_city <- cols[sp_index]
      
      # male
      scen_city_male <- voi_data_all_sex_df %>% filter(sex == 'male', city == city) %>% dplyr::select(-c(sex,city,run, age_cat, age_sex))
      scen_out_city_male <- sapply(1:NSCEN,function(y)rowSums(scen_city_male[,seq(y,ncol(scen_city_male),by=NSCEN)]))
      means_male <- colMeans(scen_out_city_male) 
      ninefive_male <- apply(scen_out_city_male,2,quantile,probs = c(0.025,0.975))
      yvals_male <- rep(2,each=NSCEN)/10 + rep(1:NSCEN)
  
      # female
      scen_city_female <- voi_data_all_sex_df %>% filter(sex == 'female', city == city) %>% dplyr::select(-c(sex,city,run, age_cat, age_sex))
      scen_out_city_female <- sapply(1:NSCEN,function(y)rowSums(scen_city_female[,seq(y,ncol(scen_city_female),by=NSCEN)]))
      means_female <- colMeans(scen_out_city_female) 
      ninefive_female <- apply(scen_out_city_female,2,quantile,probs = c(0.025,0.975))
      yvals_female <- rep(1,each=NSCEN)/10 + rep(1:NSCEN)
      
      par_city <- par(mar=c(5,7,1,1))
      xlab <- paste0(city,': Change in total YLL relative to baseline')
      plot(as.vector(means),yvals,pch=16,cex=1,frame=F,ylab='',xlab=xlab,col='black',
           yaxt='n', ylim = range(.9,4.2),xlim=range(unlist(ninefive),unlist(ninefive_male),unlist(ninefive_female)))
      axis(2,las=2,at= seq(1.2, 1.05*NSCEN + 0.2, by = 1.05) #(1+0.1):(NSCEN+.1)
           ,labels=SCEN_SHORT_NAME[2:length(SCEN_SHORT_NAME)])
      
      points(as.vector(means_male),yvals_male,pch=16,cex=1,col='blue')
      points(as.vector(means_female),yvals_female,pch=16,cex=1,col='red')
      
      for(j in 1:NSCEN){
        lines(ninefive[,j],rep(yvals[j],2),lwd=2,col='black')
        lines(ninefive_male[,j],rep(yvals_male[j],2),lwd=2, col='blue')
        lines(ninefive_female[,j],rep(yvals_female[j],2),lwd=2, col='red')
      } 
      abline(v=0,col='grey',lty=2,lwd=2)
      text(y=(NSCEN-1)+0.4,x=ninefive[1,(NSCEN-1)],'95%',col='black',adj=c(-0,-0.3*sp_index))
      legend(col='black', lty=1,bty='n',x= mean(means),legend=paste0(city,': all'),y=NSCEN-1,lwd=2, cex = 0.8)
      legend(col='blue', lty=1,bty='n',x= mean(means),legend=paste0(city,': male'),y=NSCEN-1.1,lwd=2, cex = 0.8)
      legend(col='red', lty=1,bty='n',x= mean(means),legend=paste0(city,': female'),y=NSCEN-1.2,lwd=2, cex = 0.8)
      par(par_city)
    }
    dev.off()
  } 
  
  
  
  
  
################## repeat plots per 100,000  
  
  # plot total YLL sum per 100,000
  {pdf(paste0('results/voi/city_yll_100k_',output_version,'.pdf'),height=6,width=6)
    
    # one plot for all cities - might be difficult to read
    # par <- par(mar=c(5,5,1,1))
    # sp_index <- which(cities==city)
    # scen_out <- lapply(outcome[-length(outcome)],function(x)sapply(1:NSCEN,function(y)rowSums(x[,seq(y,ncol(x),by=NSCEN)])))
    # scen_out_100k <- lapply(scen_out, function(x) x*100000/subset(population_df, sex=='all' & age_cat == 'all' & city == city)$population)
    # means <- sapply(scen_out_100k,function(x)apply(x,2,mean))
    # ninefive <- lapply(scen_out_100k,function(x) apply(x,2,quantile,c(0.025,0.975)))
    # yvals <- rep(1:length(scen_out_100k),each=NSCEN)/10 + rep(1:NSCEN,times=length(scen_out_100k))
    # cols <- rainbow(length(outcome)-1)
    # 
    # plot(as.vector(means),yvals,pch=16,cex=1,frame=F,ylab='',xlab='Change in total YLL per 100k relative to baseline',
    #      col=rep(cols,each=NSCEN),yaxt='n',xlim=range(unlist(ninefive)))
    # axis(2,las=2,at=(1+0.1):(NSCEN+0.1),labels=SCEN_SHORT_NAME[2:length(SCEN_SHORT_NAME)])
    # for(i in 1:length(outcome[-length(outcome)])) for(j in 1:NSCEN) lines(ninefive[[i]][,j],rep(yvals[j+(i-1)*NSCEN],2),lwd=2,col=cols[i])
    # abline(v=0,col='grey',lty=2,lwd=2)
    # text(y=(NSCEN-1)+0.2,x=ninefive[[sp_index]][1,(NSCEN-1)],'95%',col='navyblue',adj=c(-0,-0.3*sp_index))
    # legend(col=rev(cols),lty=1,bty='n',x= mean(means),legend=rev(names(outcome)[-length(outcome)]),y=NSCEN-1,lwd=2)
    # par(par)
    # 
    # one plot per city
    par <- par(mar=c(5,5,1,1))
    for(city in cities){ 
      sp_index <- which(cities==city)
      scen_out <- lapply(outcome[-length(outcome)],function(x)sapply(1:NSCEN,function(y)rowSums(x[,seq(y,ncol(x),by=NSCEN)])))
      scen_out_city <- scen_out[[city]]
      scen_out_city_100k <- scen_out_city*100000/subset(population_df, sex=='all' & age_cat == 'all' & city == city)$population
      means <- colMeans(scen_out_city_100k) 
      ninefive <- apply(scen_out_city_100k,2,quantile,probs = c(0.025,0.975))
      yvals <- rep(1,each=NSCEN)/10 + rep(1:NSCEN) 
      cols <- rainbow(length(outcome)-1)
      col_city <- cols[sp_index]
      
      par_city <- par(mar=c(5,5,1,1))
      xlab <- paste0(city,': Change in total YLL per 100k relative to baseline')
      plot(as.vector(means),yvals,pch=16,cex=1,frame=F,ylab='',xlab=xlab,col=rep(col_city,each=NSCEN),
           yaxt='n',xlim=range(unlist(ninefive)))
      axis(2,las=2,at=(1+0.1):(NSCEN+0.1),labels=SCEN_SHORT_NAME[2:length(SCEN_SHORT_NAME)])
      for(j in 1:NSCEN) lines(ninefive[,j],rep(yvals[j],2),lwd=2,col=col_city)
      abline(v=0,col='grey',lty=2,lwd=2)
      text(y=(NSCEN-1)+0.2,x=ninefive[1,(NSCEN-1)],'95%',col='navyblue',adj=c(-0,-0.3*sp_index))
      legend(col=col_city, lty=1,bty='n',x= mean(means),legend=city,y=NSCEN-1,lwd=2)
      par(par_city)
    }
    dev.off()
  } 
  
  
  # plot total YLL sum plus include split by sex (95% CIs) per 100,000
  {pdf(paste0('results/voi/city_yll_sex_100k_',output_version,'.pdf'),height=6,width=6)
    
    # one plot per city
    for(city in cities){ 
      sp_index <- which(cities==city)
      scen_out <- lapply(outcome[-length(outcome)],function(x)sapply(1:NSCEN,function(y)rowSums(x[,seq(y,ncol(x),by=NSCEN)])))
      scen_out_city_100k <- scen_out[[city]]*100000/subset(population_df, sex=='all' & age_cat == 'all' & city == city)$population
      means <- colMeans(scen_out_city_100k) 
      ninefive <- apply(scen_out_city_100k,2,quantile,probs = c(0.025,0.975))
      yvals <- rep(3,each=NSCEN)/10 + rep(1:NSCEN) 
      cols <- rainbow(length(outcome)-1)
      col_city <- cols[sp_index]
      
      # male
      scen_city_male <- voi_data_all_sex_df %>% filter(sex == 'male', city == city) %>% dplyr::select(-c(sex,city,run, age_cat, age_sex))
      scen_out_city_male <- sapply(1:NSCEN,function(y)rowSums(scen_city_male[,seq(y,ncol(scen_city_male),by=NSCEN)]))
      scen_out_city_male_100k <- scen_out_city_male*100000/subset(population_df, sex=='male' & age_cat == 'all' & city == city)$population
      means_male <- colMeans(scen_out_city_male_100k) 
      ninefive_male <- apply(scen_out_city_male_100k,2,quantile,probs = c(0.025,0.975))
      yvals_male <- rep(2,each=NSCEN)/10 + rep(1:NSCEN)
      
      # female
      scen_city_female <- voi_data_all_sex_df %>% filter(sex == 'female', city == city) %>% dplyr::select(-c(sex,city,run, age_cat, age_sex))
      scen_out_city_female <- sapply(1:NSCEN,function(y)rowSums(scen_city_female[,seq(y,ncol(scen_city_female),by=NSCEN)]))
      scen_out_city_female_100k <- scen_out_city_female*100000/subset(population_df, sex=='female' & age_cat == 'all' & city == city)$population
      means_female <- colMeans(scen_out_city_female_100k) 
      ninefive_female <- apply(scen_out_city_female_100k,2,quantile,probs = c(0.025,0.975))
      yvals_female <- rep(1,each=NSCEN)/10 + rep(1:NSCEN)
      
      par_city <- par(mar=c(5,7,1,1))
      xlab <- paste0(city,': Change in total YLL per 100k relative to baseline')
      plot(as.vector(means),yvals,pch=16,cex=1,frame=F,ylab='',xlab=xlab,col='black',
           yaxt='n', ylim = range(.9,4.2),xlim=range(unlist(ninefive),unlist(ninefive_male),unlist(ninefive_female)))
      axis(2,las=2,at= seq(1.2, 1.05*NSCEN + 0.2, by = 1.05) #(1+0.1):(NSCEN+.1)
           ,labels=SCEN_SHORT_NAME[2:length(SCEN_SHORT_NAME)])
      
      points(as.vector(means_male),yvals_male,pch=16,cex=1,col='blue')
      points(as.vector(means_female),yvals_female,pch=16,cex=1,col='red')
      
      for(j in 1:NSCEN){
        lines(ninefive[,j],rep(yvals[j],2),lwd=2,col='black')
        lines(ninefive_male[,j],rep(yvals_male[j],2),lwd=2, col='blue')
        lines(ninefive_female[,j],rep(yvals_female[j],2),lwd=2, col='red')
      } 
      abline(v=0,col='grey',lty=2,lwd=2)
      text(y=(NSCEN-1)+0.4,x=ninefive[1,(NSCEN-1)],'95%',col='black',adj=c(-0,-0.3*sp_index))
      legend(col='black', lty=1,bty='n',x= mean(means),legend=paste0(city,': all'),y=NSCEN-1,lwd=2, cex = 0.8)
      legend(col='blue', lty=1,bty='n',x= mean(means),legend=paste0(city,': male'),y=NSCEN-1.1,lwd=2, cex = 0.8)
      legend(col='red', lty=1,bty='n',x= mean(means),legend=paste0(city,': female'),y=NSCEN-1.2,lwd=2, cex = 0.8)
      par(par_city)
    }
    dev.off()
  } 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
############## YLL per person  
  
  # plotting the output YLL per person as sums across all cities
  comb_out <- sapply(1:NSCEN,function(y)rowSums(outcome[[length(outcome)]][,seq(y,ncol(outcome[[length(outcome)]]),by=NSCEN)]))
  ninefive <- apply(comb_out,2,quantile,c(0.025,0.975))
  means <- apply(comb_out,2,mean)
  {pdf(paste0('results/voi/combined_yll_pp','_',output_version,'.pdf'),height=3,width=6); par(mar=c(5,5,1,1))
    plot(as.vector(means),1:NSCEN,pch=16,cex=1,frame=F,ylab='',xlab='Change in total YLL per person relative to baseline summed across all cities',
         col='navyblue',yaxt='n',xlim=range(ninefive), cex.lab = 0.8, cex.axis = 0.8)
    axis(2,las=2,at=1:NSCEN,labels=SCEN_SHORT_NAME[2:length(SCEN_SHORT_NAME)], cex = 0.8)
    for(j in 1:NSCEN) lines(ninefive[,j],c(j,j),lwd=2,col='navyblue')
    abline(v=0,col='grey',lty=2,lwd=2)
    text(y=(NSCEN-1),x=ninefive[1,(NSCEN-1)],'95%',col='navyblue',adj=c(-0,-0.7))
    dev.off()
  }
  
}




################################################ calculate EVPPI ################################################
if (voi_analysis == T & nsamples > 1){ # only run EVPPI part if there is more than one sample
  
  print('start EVPPI analysis')
  
  
  parameter_samples <- readRDS(paste('diagnostic/parameter_samples_',output_version,'.Rds'))
  
  # calculate the evppi values for all input parameters for all outcomes
  # defined in the outcome_voi_list for all scenarios
  evppi_list <- call_evppi(parameter_samples, outcome_voi_list, outcome, cities, voi_add_sum, 
                         NSCEN, NSAMPLES, scenario_names)
  
  evppi_df <- evppi_list[[1]]
  evppi_outcome_names <- unlist(evppi_list[[2]])
  
  saveRDS(evppi_df,'results/voi/evppi.Rds',version=2) # save evppi dataframe
  
  evppi_csv <- paste0('results/voi/evppi_',output_version,".csv")
  
  write.csv(evppi_df,evppi_csv,row.names = FALSE) # save as csv file
  
  
  
  
  # create output plots
  
  output_pdf <- paste0('results/voi/evppi_',output_version,".pdf")
  #{pdf('results/voi/evppi.pdf',height=15,width=4+length(outcome_voi_list))
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
  
  
  

  ##### run EVPPI for different sexes
  
  if(voi_gender){
    
    print('starting EVPPI analysis by sex')
    
    evppi_sex_list <- call_evppi_sex(voi_data_all_sex_df,parameter_samples, outcome_voi_list, outcome, cities, voi_add_sum, 
                                            NSCEN, NSAMPLES, scenario_names, evppi_df)
    
    evppi_sex_df <- evppi_sex_list[[1]] 
    sex_cat <- unlist(evppi_sex_list[[2]])
    evppi_city_list_all_sex <- evppi_sex_list[[3]]
    
    saveRDS(evppi_sex_df,paste0('results/voi/evppi_sex_',output_version,".csv"),version=2) 
    
    evppi_sex_csv <- paste0('results/voi/evppi_sex_',output_version,".csv")
    #write.csv(evppi_df,'results/voi/evppi.csv',row.names = FALSE) # save as csv file
    
    write.csv(evppi_sex_df,evppi_sex_csv,row.names = FALSE) # save as csv file
    
    
    
    # create output plots
    output_pdf <- paste0('results/voi/evppi_sex_',output_version,".pdf")
    ci <- 1
    #{pdf('results/voi/evppi.pdf',height=15,width=4+length(outcome_voi_list))
    {pdf(output_pdf,height=15,width=4+length(outcome_voi_list)+1)
      for ( city_name in cities){
        
        
        #evppi_agesex_city_df <- get(paste0("evppi_agesex_",city_name,'_df'))
        evppi_sex_city_df <- evppi_city_list_all_sex[[ci]]
        
        if (voi_add_sum){outcome_list <- c(outcome_voi_list, 'sum') 
        }else{ outcome_list <- outcome_voi_list}

        par_city <- par(mar=c(10,13,4,3.5))
        
        labs <- evppi_sex_city_df$parameters # y axis label
        labs <- str_replace(labs,'DOSE_RESPONSE','DR') # replace DOSE_RESPONSE with DR
        labs <- str_replace(labs,'EMISSION_INVENTORY','EMISSION_INV') # replace EMISSION_INVENTORY with EMISSION_INV
        evppi_sex_dummy <- evppi_sex_city_df %>% dplyr::select(!c(city, parameters))    #[,evppi_outcome_names]
        # for plotting purposes, replace all NaN with 0
        evppi_sex_dummy[is.na(evppi_sex_dummy)] <- 0
        get.pal=colorRampPalette(brewer.pal(9,"Reds"))
        redCol=rev(get.pal(12))
        bkT <- seq(max(evppi_sex_dummy[!is.na(evppi_sex_dummy)])+1e-10, 0,length=13)
        cex.lab <- 1.0
        maxval <- round(bkT[1],digits=1)
        col.labels<- c(0,maxval/2,maxval)
        cellcolors <- vector()
        title <- paste(city_name,  " - No of samples: ", nsamples, 
                       # ': By how much (%) could we\n reduce uncertainty in the outcome\n if we knew this parameter perfectly?')
                       '- By how much (%) could we reduce\n uncertainty in the outcome if we knew this parameter perfectly?')
        for(ii in 1:length(unlist(evppi_sex_dummy))) # determine the cellcolors
          cellcolors[ii] <- redCol[tail(which(unlist(evppi_sex_dummy)[ii]<bkT),n=1)]
        color2D.matplot(evppi_sex_dummy,cellcolors=cellcolors,xlab="",ylab="",axes=F,border='white')
        title(title, adj = 0, cex.main = 0.7 )
        fullaxis(side=1,at=(ncol(evppi_sex_dummy)-1):0+0.5,labels=rev(colnames(evppi_sex_dummy)),
                 las = 2, line=NA,pos=NA,outer=FALSE,font=NA,lwd=0,cex.axis=0.65)  # x-axis labels
        fullaxis(side=2,las=1,at=(length(labs)-1):0+0.5,labels=labs,
                 line=NA,pos=NA,outer=FALSE,font=NA,lwd=0,cex.axis=0.6) # y-axis labels
        color.legend(ncol(evppi_sex_dummy)+0.5,0,ncol(evppi_sex_dummy)+1.2,length(labs),col.labels,rev(redCol),
                     gradient="y",cex=0.7,align="rb")
        for(i in seq(0,ncol(evppi_sex_dummy),by=NSCEN)) abline(v=i, lwd=1) # add vertical lines
        abline(v=(ncol(evppi_sex_dummy))/2, lwd=2) # add vertical line between male and female results
        for(i in c(0,length(labs))) abline(h=i, lwd = 2) # add horizontal lines at top and bottom
        par(par_city)

      }
      dev.off()}
    
  } # end of gender VOI analysis
  
  
  
  
  
    
  
  ##### run EVPPI for different ages and sexes
  
  if(voi_age_gender){
    
    print('starting EVPPI analysis by age and sex')

    
    evppi_agesex_list <- call_evppi_age_sex(voi_data_all_df, parameter_samples, outcome_voi_list, outcome, cities, voi_add_sum, 
                                          NSCEN, NSAMPLES, scenario_names, evppi_df)
    
    evppi_agesex_df <- evppi_agesex_list[[1]] 
    age_gender_cat <- unlist(evppi_agesex_list[[2]])
    evppi_city_list_all <- evppi_agesex_list[[3]]
    
    saveRDS(evppi_agesex_df,paste0('results/voi/evppi_agesex_',output_version,".csv"),version=2) 
    
    evppi_agesex_csv <- paste0('results/voi/evppi_agesex_',output_version,".csv")
    #write.csv(evppi_df,'results/voi/evppi.csv',row.names = FALSE) # save as csv file
    
    write.csv(evppi_agesex_df,evppi_agesex_csv,row.names = FALSE) # save as csv file
    
    
    
    # create output plots
    output_pdf <- paste0('results/voi/evppi_agesex_',output_version,".pdf")
    ci <- 1
    #{pdf('results/voi/evppi.pdf',height=15,width=4+length(outcome_voi_list))
    {pdf(output_pdf,height=15,width=4+length(age_gender_cat)+1)
      for ( city_name in cities){
        
        
        #evppi_agesex_city_df <- get(paste0("evppi_agesex_",city_name,'_df'))
        evppi_agesex_city_df <- evppi_city_list_all[[ci]]
        
        if (voi_add_sum){outcome_list <- c(outcome_voi_list, 'sum') 
        }else{ outcome_list <- outcome_voi_list}
        
        # loop through each outcome in outcome_voi_list separately and create one page per outcome and city
        for (out in outcome_list){
          
          outcome_cols <- sapply(colnames(evppi_agesex_city_df),function(x) grepl(paste(out, collapse = "|"),x))
          evppi_agesex_city_outcome_df <- evppi_agesex_city_df[,outcome_cols]
          par_city <- par(mar=c(14,12.5,4,3.5))
          
          labs <- evppi_agesex_city_df$parameters # y axis label
          # labs <- str_replace(labs,'DOSE_RESPONSE_QUANTILE','DR_QUANT') # replace DOSE_RESPONSE with DR
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
          #for(i in seq(0,ncol(evppi_dummy),by=length(age_gender_cat)+1)) abline(v=i, lwd=2) # add vertical lines
          for(i in seq(0,ncol(evppi_dummy),by=NSCEN)) abline(v=i, lwd=1, lty=5) # add vertical dashed
          for(i in seq(0,ncol(evppi_dummy),by=ncol(evppi_dummy)/2)) abline(v=i, lwd=2) # add vertical lines
          # for(j in 1:NSCEN){
          #   for(i in seq(0 + (j-1)*(length(age_gender_cat)+1),j*(length(age_gender_cat)+1),by = 2)) abline(v=i, lwd=1, lty=5)} # add vertical dashed
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
      paste("Scenario:", SCENARIO_INCREASE * 100, "%", sep = " "),
      paste("Input parameter file:", input_version, sep = " "),
      paste("Version number of outputs:", output_version, sep = " "),
      paste("Number of samples:", '1', sep = " "),
      paste("Comments:", comment, sep=" "),
      paste("Path of other input files:", global_path, sep=" "),
      paste("Runtime in minutes:", runtime, sep=" "),
      file="OutputVersionControl.txt",sep="\n",append=TRUE)
  
}

print(paste0("The code took ", runtime, " minutes to run"))  

