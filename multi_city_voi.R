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


voi_analysis <- F # set to T if want to run VoI analysis and to F otherwise

# list of potential values for the outcome_voi_list
# 'pa_ap_all_cause', 'pa_ap_IHD', 'pa_total_cancer', 'pa_ap_lung_cancer', 'ap_COPD', 
# 'pa_ap_stroke', 'pa_ap_T2D', 'ap_LRI', 'pa_breast_cancer', 'pa_colon_cancer', 'pa_endo_cancer',
# 'pa_liver_cancer', 'pa_ap_CVD', 'pa_total_dementia', 'pa_myeloma', 'pa_Parkinson',
# 'pa_head_neck_cancer', 'pa_stomach_cancer', 'inj'


# outcome_voi_list <- c('pa_ap_all_cause', 'pa_ap_IHD', 'pa_total_cancer', 'pa_ap_lung_cancer', 'ap_COPD', 
#                       'pa_ap_stroke', 'pa_ap_T2D', 'ap_LRI', 'pa_breast_cancer', 'pa_colon_cancer', 'pa_endo_cancer',
#                       'pa_liver_cancer', 'pa_ap_CVD', 'pa_total_dementia', 'pa_myeloma', 'pa_Parkinson',
#                       'pa_head_neck_cancer', 'pa_stomach_cancer', 'inj',
#                       'level1', 'level2', 'level3')

outcome_voi_list <- c('pa_ap_all_cause', 'inj', 'level1')

# flag whether to run VOI analysis split gender
voi_gender <- T # set to T if want to include split and to F otherwise

# flag whether to run VOI analysis split by age and gender 
voi_age_gender <- T # set to T if want to include split and to F otherwise

# add total across all outputs in VOI list for each scenario - only makes sense if results are independent of each other
# i.e. combining e.g. "total_cancer" with "lung_cancer" results in double-counting and invalid VOI analysis for the sum
voi_add_sum <- T


input_parameter_file <- "InputParameters_v42.0_NOTreadyYet.xlsx"


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
output_version <- 'bogota_10samples2_v42.0'

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

# get outputs from ithim run into correct formats and calculate summary statistics
ithim_results <- ithimr::extract_data_for_voi(NSCEN, NSAMPLES, SCEN_SHORT_NAME,cities,multi_city_ithim, output_version)


voi_complete_df <- ithim_results$voi_complete 
voi_complete_summary_df <- ithim_results$voi_complete_summary
voi_complete_100k_df <- ithim_results$voi_complete_100k 
voi_complete_100k_summary_df <- ithim_results$voi_complete_100k_summary

voi_outputs_all_csv <- paste0('results/voi/All_samples_output_',output_version,".csv")
write.csv(voi_complete_df ,voi_outputs_all_csv,row.names = FALSE) # save as csv file

voi_outputs_all_100k_csv <- paste0('results/voi/All_samples_100k_output_',output_version,".csv")
write.csv(voi_complete_100k_df ,voi_outputs_all_100k_csv,row.names = FALSE) # save as csv file

ci_outputstats_csv <- paste0('results/voi/VoI_CI_output_stats_',output_version,".csv")
write.csv(voi_complete_summary_df,ci_outputstats_csv,row.names = FALSE) # save as csv file

ci_100k_outputstats_csv <- paste0('results/voi/VoI_CI_100k_output_stats_',output_version,".csv")
write.csv(voi_complete_100k_summary_df,ci_100k_outputstats_csv,row.names = FALSE) # save as csv file



######################################################### plot results #######################################################
print('plot results')

# plots only work if more than one sample was selected
if(nsamples > 1){

  # plot level 1 results - one plot for all cities plus individual plots for each city
  {pdf(paste0('results/voi/city_yll_level1_',output_version,'.pdf'),height=6,width=6)

    # one plot for all cities - might be difficult to read if too many cities
    par <- par(mar=c(5,5,1,1))
    level1_df <- voi_complete_summary_df %>% filter(age_sex == 'all all') %>% dplyr::select(city, value_type, matches('level1'))
    means <- as.matrix(unlist(transpose(level1_df %>% filter(value_type == 'mean') %>% dplyr::select(matches('level1')) )))
    ninefive <- list()
    for (cityname in cities){
      ninefive[[cityname]] <- as.matrix(level1_df %>% filter(city == cityname & (value_type == '2.5perc' | 
                                                                               value_type == '97.5perc')) %>% dplyr::select(!c(city, value_type)))
    }
    yvals <- rep(1:length(cities),each=NSCEN)/10 + rep(1:NSCEN,times=length(cities))
    cols <- rainbow(length(cities))
    
    plot(as.vector(means),yvals,pch=16,cex=1,frame=F,ylab='',xlab='Change in total YLL relative to baseline - Level 1',
         col=rep(cols,each=NSCEN),yaxt='n',xlim=range(unlist(ninefive)))
    axis(2,las=2,at=(1+0.1):(NSCEN+0.1),labels=SCEN_SHORT_NAME[2:length(SCEN_SHORT_NAME)])
    for(i in 1:length(cities)) for(j in 1:NSCEN) lines(ninefive[[i]][,j],rep(yvals[j+(i-1)*NSCEN],2),lwd=2,col=cols[i])
    abline(v=0,col='grey',lty=2,lwd=2)
    text(y=(NSCEN-1)+0.2,x=ninefive[[1]][1,(NSCEN-1)],'95%',col='navyblue',adj=c(-0,-0.3*1))
    legend(col=rev(cols),lty=1,bty='n',x= mean(means),legend=rev(cities),y=NSCEN-1,lwd=2)
    par(par)

    # one plot per city
    for(cityname in cities){
      sp_index <- which(cities==cityname)
      scen_out_city <- level1_df %>% filter(city == cityname)
      means <- as.matrix(unlist(transpose(scen_out_city %>% filter(value_type == 'mean') %>% dplyr::select(matches('level1')) )))
      ninefive <- as.matrix(scen_out_city %>% filter(value_type == '2.5perc' | value_type == '97.5perc'
                                                     ) %>% dplyr::select(matches('level1')))
      yvals <- rep(1,each=NSCEN)/10 + rep(1:NSCEN)
      cols <- rainbow(length(cities))
      col_city <- cols[sp_index]

      par_city <- par(mar=c(5,5,1,1))
      xlab <- paste0(cityname,': Change in total YLL relative to baseline per - Level 1')
      plot(as.vector(means),yvals,pch=16,cex=1,frame=F,ylab='',xlab=xlab,col=rep(col_city,each=NSCEN),
           yaxt='n',xlim=range(unlist(ninefive)))
      axis(2,las=2,at=(1+0.1):(NSCEN+0.1),labels=SCEN_SHORT_NAME[2:length(SCEN_SHORT_NAME)])
      for(j in 1:NSCEN) lines(ninefive[,j],rep(yvals[j],2),lwd=2,col=col_city)
      abline(v=0,col='grey',lty=2,lwd=2)
      text(y=(NSCEN-1)+0.2,x=ninefive[1,(NSCEN-1)],'95%',col='navyblue',adj=c(-0,-0.3*sp_index))
      legend(col=col_city, lty=1,bty='n',x= mean(means),legend=cityname,y=NSCEN-1,lwd=2)
      par(par_city)
    }
    dev.off()
  }


  # plot level 1 results per 100k - one plot for all cities plus individual plots for each city
  {pdf(paste0('results/voi/city_yll_level1_100k_',output_version,'.pdf'),height=6,width=6)
    
    # one plot for all cities - might be difficult to read if too many cities
    par <- par(mar=c(5,5,1,1))
    level1_df <- voi_complete_100k_summary_df %>% filter(age_sex == 'all all') %>% dplyr::select(city, value_type, matches('level1'))
    means <- as.matrix(unlist(transpose(level1_df %>% filter(value_type == 'mean') %>% dplyr::select(matches('level1')) )))
    ninefive <- list()
    for (cityname in cities){
      ninefive[[cityname]] <- as.matrix(level1_df %>% filter(city == cityname & (value_type == '2.5perc' | value_type == '97.5perc')) %>% dplyr::select(matches('level1')))
    }
    yvals <- rep(1:length(cities),each=NSCEN)/10 + rep(1:NSCEN,times=length(cities))
    cols <- rainbow(length(cities))
    
    plot(as.vector(means),yvals,pch=16,cex=1,frame=F,ylab='',xlab='Change in total YLL relative to baseline per 100k - Level 1',
         col=rep(cols,each=NSCEN),yaxt='n',xlim=range(unlist(ninefive)))
    axis(2,las=2,at=(1+0.1):(NSCEN+0.1),labels=SCEN_SHORT_NAME[2:length(SCEN_SHORT_NAME)])
    for(i in 1:length(cities)) for(j in 1:NSCEN) lines(ninefive[[i]][,j],rep(yvals[j+(i-1)*NSCEN],2),lwd=2,col=cols[i])
    abline(v=0,col='grey',lty=2,lwd=2)
    text(y=(NSCEN-1)+0.2,x=ninefive[[1]][1,(NSCEN-1)],'95%',col='navyblue',adj=c(-0,-0.3*1))
    legend(col=rev(cols),lty=1,bty='n',x= mean(means),legend=rev(cities),y=NSCEN-1,lwd=2)
    par(par)
    
    # one plot per city
    for(cityname in cities){
      sp_index <- which(cities==cityname)
      scen_out_city <- level1_df %>% filter(city == cityname)
      means <- as.matrix(unlist(transpose(scen_out_city %>% filter(value_type == 'mean') %>% dplyr::select(matches('level1')) )))
      ninefive <- as.matrix(scen_out_city %>% filter(city == cityname & (value_type == '2.5perc' | value_type == '97.5perc')
      ) %>% dplyr::select(matches('level1')))
      yvals <- rep(1,each=NSCEN)/10 + rep(1:NSCEN)
      cols <- rainbow(length(cities))
      col_city <- cols[sp_index]
      
      par_city <- par(mar=c(5,5,1,1))
      xlab <- paste0(cityname,': Change in total YLL relative to baseline per 100k - Level 1')
      plot(as.vector(means),yvals,pch=16,cex=1,frame=F,ylab='',xlab=xlab,col=rep(col_city,each=NSCEN),
           yaxt='n',xlim=range(unlist(ninefive)))
      axis(2,las=2,at=(1+0.1):(NSCEN+0.1),labels=SCEN_SHORT_NAME[2:length(SCEN_SHORT_NAME)])
      for(j in 1:NSCEN) lines(ninefive[,j],rep(yvals[j],2),lwd=2,col=col_city)
      abline(v=0,col='grey',lty=2,lwd=2)
      text(y=(NSCEN-1)+0.2,x=ninefive[1,(NSCEN-1)],'95%',col='navyblue',adj=c(-0,-0.3*sp_index))
      legend(col=col_city, lty=1,bty='n',x= mean(means),legend=cityname,y=NSCEN-1,lwd=2)
      par(par_city)
    }
    dev.off()
  }
 
  
  
 
  # plot total YLL per 100k for 3 levels for all scenarios - one plot per city
  {pdf(paste0('results/voi/city_yll_all_levels_100k_',output_version,'.pdf'),height=6,width=6)

  # one plot per city
    for(cityname in cities){
      sp_index <- which(cities==cityname)
      # extract information for all 3 levels
      scen_out_city <- voi_complete_100k_summary_df %>% filter(age_sex == 'all all' & city == cityname
                                                          ) %>% dplyr::select(city, value_type, matches('level'))
      # level 1 means and CI interval values
      means <- as.matrix(unlist(transpose(scen_out_city %>% filter(value_type == 'mean') %>% dplyr::select(matches('level1')) )))
      ninefive <- as.matrix(scen_out_city %>% filter(city == cityname & (value_type == '2.5perc' | value_type == '97.5perc')
                                                     ) %>% dplyr::select(matches('level1')))
      yvals <- rep(3,each=NSCEN)/10 + rep(1:NSCEN)
  
      # level 2
      means_l2 <- as.matrix(unlist(transpose(scen_out_city %>% filter(value_type == 'mean') %>% dplyr::select(matches('level2')) )))
      ninefive_l2  <- as.matrix(scen_out_city %>% filter(city == cityname & (value_type == '2.5perc' | value_type == '97.5perc')
                                                          ) %>% dplyr::select(matches('level2')))
      yvals_l2<- rep(2,each=NSCEN)/10 + rep(1:NSCEN)
  
      # level 3
      means_l3 <- as.matrix(unlist(transpose(scen_out_city %>% filter(value_type == 'mean') %>% dplyr::select(matches('level3')) )))
      ninefive_l3  <- as.matrix(scen_out_city %>% filter(city == cityname & (value_type == '2.5perc' | value_type == '97.5perc')
                                                         ) %>% dplyr::select(matches('level3')))
      yvals_l3 <- rep(1,each=NSCEN)/10 + rep(1:NSCEN)
  
      par_city <- par(mar=c(5,7,1,1))
      xlab <- paste0(cityname,': Change YLL per 100k relative to baseline for each level')
      plot(as.vector(means),yvals,pch=16,cex=1,frame=F,ylab='',xlab=xlab,col='black',
           yaxt='n', ylim = range(.9,4.2),xlim=range(unlist(ninefive),unlist(ninefive_l2),unlist(ninefive_l3)))
      axis(2,las=2,at= seq(1.2, 1.05*NSCEN + 0.2, by = 1.05) 
           ,labels=SCEN_SHORT_NAME[2:length(SCEN_SHORT_NAME)])
  
      points(as.vector(means_l2),yvals_l2,pch=16,cex=1,col='blue')
      points(as.vector(means_l3),yvals_l3,pch=16,cex=1,col='red')
  
      for(j in 1:NSCEN){
        lines(ninefive[,j],rep(yvals[j],2),lwd=2,col='black')
        lines(ninefive_l2[,j],rep(yvals_l2[j],2),lwd=2, col='blue')
        lines(ninefive_l3[,j],rep(yvals_l3[j],2),lwd=2, col='red')
      }
      abline(v=0,col='grey',lty=2,lwd=2)
      text(y=(NSCEN-1)+0.4,x=ninefive[1,(NSCEN-1)],'95%',col='black',adj=c(-0,-0.3*sp_index))
      legend(col='black', lty=1,bty='n',x= mean(means),legend=paste0(cityname,': Level 1'),y=NSCEN-1,lwd=2, cex = 0.8)
      legend(col='blue', lty=1,bty='n',x= mean(means),legend=paste0(cityname,': Level 2'),y=NSCEN-1.1,lwd=2, cex = 0.8)
      legend(col='red', lty=1,bty='n',x= mean(means),legend=paste0(cityname,': Level 3'),y=NSCEN-1.2,lwd=2, cex = 0.8)
      par(par_city)
    }
    dev.off()
  }
  


  

  
  
  
  # plot total YLL per 100k for 3 levels, both sexes and for all scenarios - one plot per city
  {pdf(paste0('results/voi/city_yll_all_levels_sex_100k_',output_version,'.pdf'),height=6,width=6)
    
    # one plot per city
    for(cityname in cities){
      sp_index <- which(cities==cityname)
      # extract information for all 3 levels
      scen_out_city <- voi_complete_100k_summary_df %>% filter(age_cat == 'all' & city == cityname
                                                          ) %>% dplyr::select(city, sex, value_type, matches('level'))
      
      # level 1 means and CI interval values
      means <- as.matrix(unlist(transpose(scen_out_city %>% filter(value_type == 'mean' & sex == 'all') %>% dplyr::select(matches('level1')) )))
      ninefive <- as.matrix(scen_out_city %>% filter(city == cityname & sex == 'all' &  (value_type == '2.5perc' | value_type == '97.5perc')
      ) %>% dplyr::select(matches('level1')))
      yvals <- rep(6,each=NSCEN)/10 + rep(1:NSCEN)
      
      # level 1 means and CI interval values - male
      means_m <- as.matrix(unlist(transpose(scen_out_city %>% filter(value_type == 'mean' & sex == 'male') %>% dplyr::select(matches('level1')) )))
      ninefive_m <- as.matrix(scen_out_city %>% filter(city == cityname & sex == 'male' & (value_type == '2.5perc' | value_type == '97.5perc')
      ) %>% dplyr::select(matches('level1')))
      yvals_m <- rep(5.5,each=NSCEN)/10 + rep(1:NSCEN)
      
      # level 1 means and CI interval values - female
      means_f <- as.matrix(unlist(transpose(scen_out_city %>% filter(value_type == 'mean' & sex == 'female') %>% dplyr::select(matches('level1')) )))
      ninefive_f <- as.matrix(scen_out_city %>% filter(city == cityname & sex == 'female' & (value_type == '2.5perc' | value_type == '97.5perc')
                                                       ) %>% dplyr::select(matches('level1')))
      yvals_f <- rep(5,each=NSCEN)/10 + rep(1:NSCEN)
      
      
      # level 2
      means_l2 <- as.matrix(unlist(transpose(scen_out_city %>% filter(value_type == 'mean' & sex == 'all') %>% dplyr::select(matches('level2')) )))
      ninefive_l2  <- as.matrix(scen_out_city %>% filter(city == cityname & sex == 'all' & (value_type == '2.5perc' | value_type == '97.5perc')
                                                         ) %>% dplyr::select(matches('level2')))
      yvals_l2<- rep(4,each=NSCEN)/10 + rep(1:NSCEN)
      
      # level 2 - male
      means_l2_m <- as.matrix(unlist(transpose(scen_out_city %>% filter(value_type == 'mean' & sex == 'male') %>% dplyr::select(matches('level2')) )))
      ninefive_l2_m  <- as.matrix(scen_out_city %>% filter(city == cityname & sex == 'male' & (value_type == '2.5perc' | value_type == '97.5perc')
                                                            ) %>% dplyr::select(matches('level2')))
      yvals_l2_m<- rep(3.5,each=NSCEN)/10 + rep(1:NSCEN)      
      
      # level 2 - female
      means_l2_f <- as.matrix(unlist(transpose(scen_out_city %>% filter(value_type == 'mean' & sex == 'female') %>% dplyr::select(matches('level2')) )))
      ninefive_l2_f  <- as.matrix(scen_out_city %>% filter(city == cityname & sex == 'female' & (value_type == '2.5perc' | value_type == '97.5perc')
                                                           ) %>% dplyr::select(matches('level2')))
      yvals_l2_f<- rep(3,each=NSCEN)/10 + rep(1:NSCEN)            
      
      
      
      # level 3
      means_l3 <- as.matrix(unlist(transpose(scen_out_city %>% filter(value_type == 'mean' & sex == 'all') %>% dplyr::select(matches('level3')) )))
      ninefive_l3  <- as.matrix(scen_out_city %>% filter(city == cityname & sex == 'all' & (value_type == '2.5perc' | value_type == '97.5perc')
                                                         ) %>% dplyr::select(matches('level3')))
      yvals_l3 <- rep(2,each=NSCEN)/10 + rep(1:NSCEN)
      
      # level 3 - male
      means_l3_m <- as.matrix(unlist(transpose(scen_out_city %>% filter(value_type == 'mean' & sex == 'male') %>% dplyr::select(matches('level3')) )))
      ninefive_l3_m  <- as.matrix(scen_out_city %>% filter(city == cityname & sex == 'male' & (value_type == '2.5perc' | value_type == '97.5perc')
                                                           ) %>% dplyr::select(matches('level3')))
      yvals_l3_m <- rep(1.5,each=NSCEN)/10 + rep(1:NSCEN)      

      # level 3 - female      
      means_l3_f <- as.matrix(unlist(transpose(scen_out_city %>% filter(value_type == 'mean' & sex == 'female') %>% dplyr::select(matches('level3')) )))
      ninefive_l3_f  <- as.matrix(scen_out_city %>% filter(city == cityname & sex == 'female' & (value_type == '2.5perc' | value_type == '97.5perc')
                                                           ) %>% dplyr::select(matches('level3')))
      yvals_l3_f <- rep(1,each=NSCEN)/10 + rep(1:NSCEN)       
      
     
      par_city <- par(mar=c(5,7,1,1))
      xlab <- paste0(cityname,': Change YLL per 100k relative to baseline for each level and by sex')
      plot(as.vector(means),yvals,pch=16,cex=1,frame=F,ylab='',xlab=xlab,col='black',
           yaxt='n', ylim = range(.9,4.2),xlim=range(unlist(ninefive),unlist(ninefive_m),unlist(ninefive_f),
                                                     unlist(ninefive_l2), unlist(ninefive_l2_m), unlist(ninefive_l2_f),
                                                     unlist(ninefive_l3), unlist(ninefive_l3_m), unlist(ninefive_l3_f)),
           cex.lab = 0.8)
      axis(2,las=2,at= seq(1.3, 1.3*NSCEN + 0.2, by = 1.05) 
           ,labels=SCEN_SHORT_NAME[2:length(SCEN_SHORT_NAME)])
      
      # levels
      points(as.vector(means_l2),yvals_l2,pch=16,cex=1,col='blue')
      points(as.vector(means_l3),yvals_l3,pch=16,cex=1,col='red')
      
      # male 
      points(as.vector(means_m),yvals_m,pch=16,cex=1,col='grey39')
      points(as.vector(means_l2_m),yvals_l2_m,pch=16,cex=1,col='cornflowerblue')
      points(as.vector(means_l3_m),yvals_l3_m,pch=16,cex=1,col='coral1')

      # female 
      points(as.vector(means_f),yvals_f,pch=16,cex=1,col='grey')
      points(as.vector(means_l2_f),yvals_l2_f,pch=16,cex=1,col='dodgerblue')
      points(as.vector(means_l3_f),yvals_l3_f,pch=16,cex=1,col='orange')
      
      for(j in 1:NSCEN){ # levels both sexes
        lines(ninefive[,j],rep(yvals[j],2),lwd=2,col='black')
        lines(ninefive_l2[,j],rep(yvals_l2[j],2),lwd=2, col='blue')
        lines(ninefive_l3[,j],rep(yvals_l3[j],2),lwd=2, col='red')
      }
      
      for(j in 1:NSCEN){ # levels for males
        lines(ninefive_m[,j],rep(yvals_m[j],2),lwd=2,col='grey39', lty = 1)
        lines(ninefive_l2_m[,j],rep(yvals_l2_m[j],2),lwd=2, col='cornflowerblue', lty = 1)
        lines(ninefive_l3_m[,j],rep(yvals_l3_m[j],2),lwd=2, col='coral1', lty = 1)
      }      
      
      for(j in 1:NSCEN){ # levels for females
        lines(ninefive_f[,j],rep(yvals_f[j],2),lwd=2,col='grey', lty = 1)
        lines(ninefive_l2_f[,j],rep(yvals_l2_f[j],2),lwd=2, col='dodgerblue', lty = 1)
        lines(ninefive_l3_f[,j],rep(yvals_l3_f[j],2),lwd=2, col='orange', lty = 1)
      }           
      
      abline(v=0,col='grey',lty=2,lwd=2)
      
      text(y=(NSCEN-1)+0.7,x=ninefive[1,(NSCEN-1)],'95%',col='black',adj=c(-0,-0.3*sp_index))
      
      starting_y = 3
      
      legend(col='black', lty=1,bty='n',x= mean(means),legend=paste0(cityname,': Level 1'),y=starting_y,lwd=2, cex = 0.8)
      legend(col='grey39', lty=1,bty='n',x= mean(means),legend=paste0(cityname,': Level 1 - male'),y=starting_y-0.1,lwd=2, cex = 0.8)
      legend(col='grey', lty=1,bty='n',x= mean(means),legend=paste0(cityname,': Level 1 - female'),y=starting_y-0.2,lwd=2, cex = 0.8)
      
      legend(col='blue', lty=1,bty='n',x= mean(means),legend=paste0(cityname,': Level 2'),y=starting_y-0.4,lwd=2, cex = 0.8)
      legend(col='cornflowerblue', lty=1,bty='n',x= mean(means),legend=paste0(cityname,': Level 2 - male'),y=starting_y-0.5,lwd=2, cex = 0.8)
      legend(col='dodgerblue', lty=1,bty='n',x= mean(means),legend=paste0(cityname,': Level 2 - female'),y=starting_y-0.6,lwd=2, cex = 0.8)
      
      legend(col='red', lty=1,bty='n',x= mean(means),legend=paste0(cityname,': Level 3'),y=starting_y-0.8,lwd=2, cex = 0.8)
      legend(col='coral1', lty=1,bty='n',x= mean(means),legend=paste0(cityname,': Level 3 - male'),y=starting_y-0.9,lwd=2, cex = 0.8)
      legend(col='orange', lty=1,bty='n',x= mean(means),legend=paste0(cityname,': Level 3 - female'),y=starting_y-1,lwd=2, cex = 0.8)
      
      
      
      
      par(par_city)
    }
    dev.off()
  }
  

  
  
  
  
  
  
  
  # plot total YLL per 100k for level 1, ap_pa_all_cause and inj, both sexes and for all scenarios - one plot per city
  {pdf(paste0('results/voi/city_yll_level1_inj_allcause_sex_100k_',output_version,'.pdf'),height=6,width=6)
    
    # one plot per city
    for(cityname in cities){
      sp_index <- which(cities==cityname)
      # extract information for all 3 levels
      scen_out_city <- voi_complete_100k_summary_df %>% filter(age_cat == 'all' & city == cityname
                                              ) %>% dplyr::select(city, sex, value_type, matches('level1|inj|pa_ap_all_cause'))
      
      # level 1 means and CI interval values
      means <- as.matrix(unlist(transpose(scen_out_city %>% filter(value_type == 'mean' & sex == 'all') %>% dplyr::select(matches('level1')) )))
      ninefive <- as.matrix(scen_out_city %>% filter(city == cityname & sex == 'all' &  (value_type == '2.5perc' | value_type == '97.5perc')
                                                      ) %>% dplyr::select(matches('level1')))
      yvals <- rep(6,each=NSCEN)/10 + rep(1:NSCEN)
      
      # level 1 means and CI interval values - male
      means_m <- as.matrix(unlist(transpose(scen_out_city %>% filter(value_type == 'mean' & sex == 'male') %>% dplyr::select(matches('level1')) )))
      ninefive_m <- as.matrix(scen_out_city %>% filter(city == cityname & sex == 'male' & (value_type == '2.5perc' | value_type == '97.5perc')
      ) %>% dplyr::select(matches('level1')))
      yvals_m <- rep(5.5,each=NSCEN)/10 + rep(1:NSCEN)
      
      # level 1 means and CI interval values - female
      means_f <- as.matrix(unlist(transpose(scen_out_city %>% filter(value_type == 'mean' & sex == 'female') %>% dplyr::select(matches('level1')) )))
      ninefive_f <- as.matrix(scen_out_city %>% filter(city == cityname & sex == 'female' & (value_type == '2.5perc' | value_type == '97.5perc')
      ) %>% dplyr::select(matches('level1')))
      yvals_f <- rep(5,each=NSCEN)/10 + rep(1:NSCEN)
      
      
      # pa_ap_all_cause
      means_l2 <- as.matrix(unlist(transpose(scen_out_city %>% filter(value_type == 'mean' & sex == 'all') %>% dplyr::select(matches('pa_ap_all_cause')) )))
      ninefive_l2  <- as.matrix(scen_out_city %>% filter(city == cityname & sex == 'all' & (value_type == '2.5perc' | value_type == '97.5perc')
                                                                                    ) %>% dplyr::select(matches('pa_ap_all_cause')))
      yvals_l2<- rep(4,each=NSCEN)/10 + rep(1:NSCEN)
      
      # pa_ap_all_cause - male
      means_l2_m <- as.matrix(unlist(transpose(scen_out_city %>% filter(value_type == 'mean' & sex == 'male') %>% dplyr::select(matches('pa_ap_all_cause')) )))
      ninefive_l2_m  <- as.matrix(scen_out_city %>% filter(city == cityname & sex == 'male' & (value_type == '2.5perc' | value_type == '97.5perc')
                                                            ) %>% dplyr::select(matches('pa_ap_all_cause')))
      yvals_l2_m<- rep(3.5,each=NSCEN)/10 + rep(1:NSCEN)      
      
      # pa_ap_all_cause - female
      means_l2_f <- as.matrix(unlist(transpose(scen_out_city %>% filter(value_type == 'mean' & sex == 'female') %>% dplyr::select(matches('pa_ap_all_cause')) )))
      ninefive_l2_f  <- as.matrix(scen_out_city %>% filter(city == cityname & sex == 'female' & (value_type == '2.5perc' | value_type == '97.5perc')
                                                            ) %>% dplyr::select(matches('pa_ap_all_cause')))
      yvals_l2_f<- rep(3,each=NSCEN)/10 + rep(1:NSCEN)            
      
      
      
      # inj
      means_l3 <- as.matrix(unlist(transpose(scen_out_city %>% filter(value_type == 'mean' & sex == 'all') %>% dplyr::select(matches('inj')) )))
      ninefive_l3  <- as.matrix(scen_out_city %>% filter(city == cityname & sex == 'all' & (value_type == '2.5perc' | value_type == '97.5perc')
                                                          ) %>% dplyr::select(matches('inj')))
      yvals_l3 <- rep(2,each=NSCEN)/10 + rep(1:NSCEN)
      
      # inj - male
      means_l3_m <- as.matrix(unlist(transpose(scen_out_city %>% filter(value_type == 'mean' & sex == 'male') %>% dplyr::select(matches('inj')) )))
      ninefive_l3_m  <- as.matrix(scen_out_city %>% filter(city == cityname & sex == 'male' & (value_type == '2.5perc' | value_type == '97.5perc')
                                                            ) %>% dplyr::select(matches('inj')))
      yvals_l3_m <- rep(1.5,each=NSCEN)/10 + rep(1:NSCEN)      
      
      # inj - female      
      means_l3_f <- as.matrix(unlist(transpose(scen_out_city %>% filter(value_type == 'mean' & sex == 'female') %>% dplyr::select(matches('inj')) )))
      ninefive_l3_f  <- as.matrix(scen_out_city %>% filter(city == cityname & sex == 'female' & (value_type == '2.5perc' | value_type == '97.5perc')
                                                            ) %>% dplyr::select(matches('inj')))
      yvals_l3_f <- rep(1,each=NSCEN)/10 + rep(1:NSCEN)       
      
      
      par_city <- par(mar=c(5,7,1,1))
      xlab <- paste0(cityname,': Change YLL per 100k relative to baseline for level1, all cause mortality \nand injury and by sex')
      plot(as.vector(means),yvals,pch=16,cex=1,frame=F,ylab='',xlab=xlab,col='black',
           yaxt='n', ylim = range(.9,4.2),xlim=range(unlist(ninefive),unlist(ninefive_m),unlist(ninefive_f),
                                                     unlist(ninefive_l2), unlist(ninefive_l2_m), unlist(ninefive_l2_f),
                                                     unlist(ninefive_l3), unlist(ninefive_l3_m), unlist(ninefive_l3_f)),
           cex.lab = 0.8)
      axis(2,las=2,at= seq(1.3, 1.3*NSCEN + 0.2, by = 1.05) 
           ,labels=SCEN_SHORT_NAME[2:length(SCEN_SHORT_NAME)])
      
      # levels
      points(as.vector(means_l2),yvals_l2,pch=16,cex=1,col='blue')
      points(as.vector(means_l3),yvals_l3,pch=16,cex=1,col='red')
      
      # male 
      points(as.vector(means_m),yvals_m,pch=16,cex=1,col='grey39')
      points(as.vector(means_l2_m),yvals_l2_m,pch=16,cex=1,col='cornflowerblue')
      points(as.vector(means_l3_m),yvals_l3_m,pch=16,cex=1,col='coral1')
      
      # female 
      points(as.vector(means_f),yvals_f,pch=16,cex=1,col='grey')
      points(as.vector(means_l2_f),yvals_l2_f,pch=16,cex=1,col='dodgerblue')
      points(as.vector(means_l3_f),yvals_l3_f,pch=16,cex=1,col='orange')
      
      for(j in 1:NSCEN){ # lines both sexes
        lines(ninefive[,j],rep(yvals[j],2),lwd=2,col='black')
        lines(ninefive_l2[,j],rep(yvals_l2[j],2),lwd=2, col='blue')
        lines(ninefive_l3[,j],rep(yvals_l3[j],2),lwd=2, col='red')
      }
      
      for(j in 1:NSCEN){ # lines for males
        lines(ninefive_m[,j],rep(yvals_m[j],2),lwd=2,col='grey39', lty = 1)
        lines(ninefive_l2_m[,j],rep(yvals_l2_m[j],2),lwd=2, col='cornflowerblue', lty = 1)
        lines(ninefive_l3_m[,j],rep(yvals_l3_m[j],2),lwd=2, col='coral1', lty = 1)
      }      
      
      for(j in 1:NSCEN){ # lines for females
        lines(ninefive_f[,j],rep(yvals_f[j],2),lwd=2,col='grey', lty = 1)
        lines(ninefive_l2_f[,j],rep(yvals_l2_f[j],2),lwd=2, col='dodgerblue', lty = 1)
        lines(ninefive_l3_f[,j],rep(yvals_l3_f[j],2),lwd=2, col='orange', lty = 1)
      }           
      
      abline(v=0,col='grey',lty=2,lwd=2)
      
      text(y=(NSCEN-1)+0.7,x=ninefive[1,(NSCEN-1)],'95%',col='black',adj=c(-0,-0.3*sp_index))
      
      starting_y = 3
      
      legend(col='black', lty=1,bty='n',x= mean(means),legend=paste0(cityname,': Level 1'),y=starting_y,lwd=2, cex = 0.8)
      legend(col='grey39', lty=1,bty='n',x= mean(means),legend=paste0(cityname,': Level 1 - male'),y=starting_y-0.1,lwd=2, cex = 0.8)
      legend(col='grey', lty=1,bty='n',x= mean(means),legend=paste0(cityname,': Level 1 - female'),y=starting_y-0.2,lwd=2, cex = 0.8)
      
      legend(col='blue', lty=1,bty='n',x= mean(means),legend=paste0(cityname,': pa_ap_all_cause'),y=starting_y-0.4,lwd=2, cex = 0.8)
      legend(col='cornflowerblue', lty=1,bty='n',x= mean(means),legend=paste0(cityname,': pa_ap_all_cause - male'),y=starting_y-0.5,lwd=2, cex = 0.8)
      legend(col='dodgerblue', lty=1,bty='n',x= mean(means),legend=paste0(cityname,': pa_ap_all_cause - female'),y=starting_y-0.6,lwd=2, cex = 0.8)
      
      legend(col='red', lty=1,bty='n',x= mean(means),legend=paste0(cityname,': inj'),y=starting_y-0.8,lwd=2, cex = 0.8)
      legend(col='coral1', lty=1,bty='n',x= mean(means),legend=paste0(cityname,': inj - male'),y=starting_y-0.9,lwd=2, cex = 0.8)
      legend(col='orange', lty=1,bty='n',x= mean(means),legend=paste0(cityname,': inj - female'),y=starting_y-1,lwd=2, cex = 0.8)
      
      
      
      
      par(par_city)
    }
    dev.off()
  }
  
  
  
  
  
  
  # plot total YLL per 100k for all cause and cvd, both sexes and for all scenarios - one plot per city
  {pdf(paste0('results/voi/city_yll_allcause_cvd_sex_100k_',output_version,'.pdf'),height=6,width=6)
    
    # one plot per city
    for(cityname in cities){
      sp_index <- which(cities==cityname)
      # extract information for all 3 levels
      scen_out_city <- voi_complete_100k_summary_df %>% filter(age_cat == 'all' & city == cityname
                                                                ) %>% dplyr::select(city, sex, value_type, matches('cvd|pa_ap_all_cause'))
      
      
      # pa_ap_all_cause
      means_l2 <- as.matrix(unlist(transpose(scen_out_city %>% filter(value_type == 'mean' & sex == 'all') %>% dplyr::select(matches('pa_ap_all_cause')) )))
      ninefive_l2  <- as.matrix(scen_out_city %>% filter(city == cityname & sex == 'all' & (value_type == '2.5perc' | value_type == '97.5perc')
                                                          ) %>% dplyr::select(matches('pa_ap_all_cause')))
      yvals_l2<- rep(4,each=NSCEN)/10 + rep(1:NSCEN)
      
      # pa_ap_all_cause - male
      means_l2_m <- as.matrix(unlist(transpose(scen_out_city %>% filter(value_type == 'mean' & sex == 'male') %>% dplyr::select(matches('pa_ap_all_cause')) )))
      ninefive_l2_m  <- as.matrix(scen_out_city %>% filter(city == cityname & sex == 'male' & (value_type == '2.5perc' | value_type == '97.5perc')
                                                            ) %>% dplyr::select(matches('pa_ap_all_cause')))
      yvals_l2_m<- rep(3.5,each=NSCEN)/10 + rep(1:NSCEN)      
      
      # pa_ap_all_cause - female
      means_l2_f <- as.matrix(unlist(transpose(scen_out_city %>% filter(value_type == 'mean' & sex == 'female') %>% dplyr::select(matches('pa_ap_all_cause')) )))
      ninefive_l2_f  <- as.matrix(scen_out_city %>% filter(city == cityname & sex == 'female' & (value_type == '2.5perc' | value_type == '97.5perc')
                                                            ) %>% dplyr::select(matches('pa_ap_all_cause')))
      yvals_l2_f<- rep(3,each=NSCEN)/10 + rep(1:NSCEN)            
      
      
      
      # cvd
      means_l3 <- as.matrix(unlist(transpose(scen_out_city %>% filter(value_type == 'mean' & sex == 'all') %>% dplyr::select(matches('cvd')) )))
      ninefive_l3  <- as.matrix(scen_out_city %>% filter(city == cityname & sex == 'all' & (value_type == '2.5perc' | value_type == '97.5perc')
                                                          ) %>% dplyr::select(matches('cvd')))
      yvals_l3 <- rep(2,each=NSCEN)/10 + rep(1:NSCEN)
      
      # cvd - male
      means_l3_m <- as.matrix(unlist(transpose(scen_out_city %>% filter(value_type == 'mean' & sex == 'male') %>% dplyr::select(matches('cvd')) )))
      ninefive_l3_m  <- as.matrix(scen_out_city %>% filter(city == cityname & sex == 'male' & (value_type == '2.5perc' | value_type == '97.5perc')
                                                            ) %>% dplyr::select(matches('cvd')))
      yvals_l3_m <- rep(1.5,each=NSCEN)/10 + rep(1:NSCEN)      
      
      # cvd - female      
      means_l3_f <- as.matrix(unlist(transpose(scen_out_city %>% filter(value_type == 'mean' & sex == 'female') %>% dplyr::select(matches('cvd')) )))
      ninefive_l3_f  <- as.matrix(scen_out_city %>% filter(city == cityname & sex == 'female' & (value_type == '2.5perc' | value_type == '97.5perc')
                                                              ) %>% dplyr::select(matches('cvd')))
      yvals_l3_f <- rep(1,each=NSCEN)/10 + rep(1:NSCEN)       
      
      
      par_city <- par(mar=c(5,7,1,1))
      xlab <- paste0(cityname,': Change YLL per 100k relative to baseline for all cause mortality and CVD and by sex')
      plot(as.vector(means_l2),yvals_l2,pch=16,cex=1,frame=F,ylab='',xlab=xlab,col='blue',
           yaxt='n', ylim = range(.9,4.2),xlim=range(unlist(ninefive_l2), unlist(ninefive_l2_m), unlist(ninefive_l2_f),
                                                     unlist(ninefive_l3), unlist(ninefive_l3_m), unlist(ninefive_l3_f)),
           cex.lab = 0.8)
      axis(2,las=2,at= seq(1.3, 1.3*NSCEN + 0.2, by = 1.05),labels=SCEN_SHORT_NAME[2:length(SCEN_SHORT_NAME)])
      
      # levels
      points(as.vector(means_l3),yvals_l3,pch=16,cex=1,col='red')
      
      # male 
      points(as.vector(means_l2_m),yvals_l2_m,pch=16,cex=1,col='cornflowerblue')
      points(as.vector(means_l3_m),yvals_l3_m,pch=16,cex=1,col='coral1')
      
      # female 
      points(as.vector(means_l2_f),yvals_l2_f,pch=16,cex=1,col='dodgerblue')
      points(as.vector(means_l3_f),yvals_l3_f,pch=16,cex=1,col='orange')
      
      for(j in 1:NSCEN){ # lines both sexes
        lines(ninefive_l2[,j],rep(yvals_l2[j],2),lwd=2, col='blue')
        lines(ninefive_l3[,j],rep(yvals_l3[j],2),lwd=2, col='red')
      }
      
      for(j in 1:NSCEN){ # lines for males
        lines(ninefive_l2_m[,j],rep(yvals_l2_m[j],2),lwd=2, col='cornflowerblue', lty = 1)
        lines(ninefive_l3_m[,j],rep(yvals_l3_m[j],2),lwd=2, col='coral1', lty = 1)
      }      
      
      for(j in 1:NSCEN){ # lines for females
        lines(ninefive_l2_f[,j],rep(yvals_l2_f[j],2),lwd=2, col='dodgerblue', lty = 1)
        lines(ninefive_l3_f[,j],rep(yvals_l3_f[j],2),lwd=2, col='orange', lty = 1)
      }           
      
      abline(v=0,col='grey',lty=2,lwd=2)
      
      text(y=(NSCEN-1)+0.7,x=ninefive_l2[1,(NSCEN-1)],'95%',col='black',adj=c(-0,-0.3*sp_index))
      
      starting_y = 3
      legend(col='blue', lty=1,bty='n',x= mean(means),legend=paste0(cityname,': pa_ap_all_cause'),y=starting_y-0.4,lwd=2, cex = 0.8)
      legend(col='cornflowerblue', lty=1,bty='n',x= mean(means),legend=paste0(cityname,': pa_ap_all_cause - male'),y=starting_y-0.5,lwd=2, cex = 0.8)
      legend(col='dodgerblue', lty=1,bty='n',x= mean(means),legend=paste0(cityname,': pa_ap_all_cause - female'),y=starting_y-0.6,lwd=2, cex = 0.8)
      
      legend(col='red', lty=1,bty='n',x= mean(means),legend=paste0(cityname,': CVD'),y=starting_y-0.8,lwd=2, cex = 0.8)
      legend(col='coral1', lty=1,bty='n',x= mean(means),legend=paste0(cityname,': CVD - male'),y=starting_y-0.9,lwd=2, cex = 0.8)
      legend(col='orange', lty=1,bty='n',x= mean(means),legend=paste0(cityname,': CVD - female'),y=starting_y-1,lwd=2, cex = 0.8)
      
      par(par_city)
    }
    dev.off()
  }
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    
  
  
 
}




################################################ calculate EVPPI ################################################
if (voi_analysis == T & nsamples > 1){ # only run EVPPI part if there is more than one sample
  
  print('start EVPPI analysis')
  
  
  parameter_samples <- readRDS(paste('diagnostic/parameter_samples_',output_version,'.Rds'))
  
  # calculate the evppi values for all input parameters for all outcomes
  # defined in the outcome_voi_list for all scenarios
  evppi_list <- call_evppi(parameter_samples, outcome_voi_list, voi_complete_df, cities, voi_add_sum, 
                         NSCEN, NSAMPLES, scenario_names)
  
  evppi_df <- evppi_list[[1]]
  evppi_outcome_names <- unlist(evppi_list[[2]])
  
  saveRDS(evppi_df,paste0('results/voi/evppi_',output_version,'.Rds'),version=2) # save evppi dataframe
  
  evppi_csv <- paste0('results/voi/evppi_',output_version,".csv")
  
  write.csv(evppi_df,evppi_csv,row.names = FALSE) # save as csv file
  
  
  
  
  # create output plots
  
  output_pdf <- paste0('results/voi/evppi_',output_version,".pdf")
  #{pdf('results/voi/evppi.pdf',height=15,width=4+length(outcome_voi_list))
  {pdf(output_pdf,height=15,width=4+length(outcome_voi_list)+1)
    for ( city_name in cities){
      
      evppi_city_df <- evppi_df %>% filter(city == city_name) 
      
      par_city <- par(mar=c(10,19,4,3.5))
      
      labs <- paste0(evppi_city_df$classification," (", evppi_city_df$parameters_lowerCase,")") # y axis label
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
    
    evppi_sex_list <- call_evppi_sex(voi_complete_df,parameter_samples, outcome_voi_list, cities, voi_add_sum, 
                                            NSCEN, NSAMPLES, scenario_names)
    
    evppi_sex_df <- evppi_sex_list[[1]] 
    sex_cat <- unlist(evppi_sex_list[[2]])

    
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
        evppi_sex_city_df <- evppi_sex_df %>% filter(city == city_name)
        
        # create dataframe where both sexes are in the same row
        evppi_sex_city_df2 <- evppi_sex_df%>% filter(gender == sex_cat[1]) %>% dplyr::select(city, classification, parameters, parameters_lowerCase)
        for (s in sex_cat){
          df_sex <- evppi_sex_city_df %>% filter(gender == s) %>% dplyr::select(-gender)
          # extend scenario names with sex
          colnames(df_sex)[grepl(paste(c('sc', 'sum'), collapse = "|") ,colnames(df_sex))] <- paste0(
                                              colnames(df_sex)[grepl(paste(c('sc', 'sum'), collapse = "|") ,colnames(df_sex))], '_',s)
          # merge with other data
          evppi_sex_city_df2 <- inner_join(evppi_sex_city_df2, df_sex, by = c('city','classification', 'parameters', 'parameters_lowerCase'))
          
        }

        if (voi_add_sum){outcome_list <- c(outcome_voi_list, 'sum') 
        }else{ outcome_list <- outcome_voi_list}

        par_city <- par(mar=c(10,19,4,3.5))
        
        labs <- paste0(evppi_sex_city_df2$classification," (", evppi_sex_city_df2$parameters_lowerCase,")") # y axis label
        labs <- str_replace(labs,'DOSE_RESPONSE','DR') # replace DOSE_RESPONSE with DR
        labs <- str_replace(labs,'EMISSION_INVENTORY','EMISSION_INV') # replace EMISSION_INVENTORY with EMISSION_INV
        evppi_sex_dummy <- evppi_sex_city_df2 %>% dplyr::select(!c(city, parameters,classification, parameters_lowerCase))    #[,evppi_outcome_names]
        # for plotting purposes, replace all NaN with 0
        evppi_sex_dummy[is.na(evppi_sex_dummy)] <- 0
        get.pal=colorRampPalette(brewer.pal(9,"Reds"))
        redCol=rev(get.pal(12))
        bkT <- seq(max(evppi_sex_dummy)+1e-10, 0,length=13)
        cex.lab <- 1.0
        maxval <- round(bkT[1],digits=1)
        col.labels<- c(0,maxval/2,maxval)
        cellcolors <- vector()
        title <- paste(city_name,  " - No of samples: ", nsamples, 
                       # ': By how much (%) could we\n reduce uncertainty in the outcome\n if we knew this parameter perfectly?')
                       '- By how much (%) could we reduce\n uncertainty in the outcome if we knew this parameter perfectly?')
        for(ii in 1:length(unlist(evppi_sex_dummy ))) # determine the cellcolors
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

    
    evppi_agesex_list <- call_evppi_age_sex(voi_complete_df, parameter_samples, outcome_voi_list, cities, voi_add_sum, 
                                          NSCEN, NSAMPLES, scenario_names)
    
    evppi_agesex_df <- evppi_agesex_list[[1]] 
    age_gender_cat <- unlist(evppi_agesex_list[[2]])

    
    
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
        
        evppi_agesex_city_df <- evppi_agesex_df %>% filter(city == city_name)
  
        # create dataframe where all age and sex categories are in the same row
        evppi_agesex_city_df2 <- evppi_agesex_df%>% filter(age_gender == age_gender_cat[1]) %>% dplyr::select(city, classification, parameters, parameters_lowerCase)
        for (s in age_gender_cat){
          df_agesex <- evppi_agesex_city_df %>% filter(age_gender == s) %>% dplyr::select(-c(gender, age, age_gender))
          # extend scenario names with age and sex category
          colnames(df_agesex)[grepl(paste(c('sc', 'sum'), collapse = "|") ,colnames(df_agesex))] <- paste0(
                                colnames(df_agesex)[grepl(paste(c('sc', 'sum'), collapse = "|") ,colnames(df_agesex))], '_',s)
          # merge with other data
          evppi_agesex_city_df2 <- inner_join(evppi_agesex_city_df2, df_agesex, by = c('city','classification', 'parameters', 'parameters_lowerCase'))
        }
      
        
        if (voi_add_sum){outcome_list <- c(outcome_voi_list, 'sum') 
        }else{ outcome_list <- outcome_voi_list}
        
        # loop through each outcome in outcome_voi_list separately and create one page per outcome and city
        for (out in outcome_list){
          
          outcome_cols <- sapply(colnames(evppi_agesex_city_df2),function(x) grepl(paste(out, collapse = "|"),x))
          evppi_agesex_city_outcome_df <- evppi_agesex_city_df2[,outcome_cols]
          par_city <- par(mar=c(14,19,4,3.5))
          
          
          labs <- unique(paste0(evppi_agesex_city_df2$classification," (", evppi_agesex_city_df$parameters_lowerCase,")") ) # y axis label
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
  
  # combine all evppi datasets
  
  
  evppi_df$gender <- 'all'
  evppi_df$age <- 'all'
  evppi_df$age_gender <- 'all all'
  evppi_df <- evppi_df %>% relocate(city,parameters, age, gender, age_gender)
  
  if (voi_gender){
    evppi_sex_df$age <- 'all'
    evppi_sex_df$age_gender <- paste0(evppi_sex_df$gender, ' all ')
    evppi_sex_df <- evppi_sex_df %>% relocate(city,parameters, age, gender, age_gender)
    
    evppi_df <- evppi_df %>% relocate(city,parameters, age, gender, age_gender)
    if (voi_age_gender){
      evppi_df_all <- rbind(evppi_df, evppi_sex_df, evppi_agesex_df)
    } else {
      evppi_df_all <- rbind(evppi_df, evppi_sex_df)
    }
  } else if (voi_age_gender & !voi_gender){
    evppi_df_all <- rbind(evppi_df, evppi_agesex_df)
  } else {
    evppi_df_all <- evppi_df
  }
  
  saveRDS(evppi_df_all,paste0('results/voi/evppi_all_',output_version,".csv"),version=2) 
  
  evppi_all_csv <- paste0('results/voi/evppi_all_',output_version,".csv")
  #write.csv(evppi_df,'results/voi/evppi.csv',row.names = FALSE) # save as csv file
  
  write.csv(evppi_df_all,evppi_all_csv,row.names = FALSE) # save as csv file

  
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

