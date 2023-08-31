#' Main script to run ITHIM Global in constant mode
#' 
#' Script to run ITHIM Global using constant input parameters. Outputs the health impacts associated with transport in a given city
#' via an air pollution, physical activity and injury pathway.
#' 
#' The ITHIM Global main script works as follows:
#' 
#' - the following variables need to be defined before running the script:
#'    - the name(s) of the city or cities for which the model is to be run
#'    - The input parameter file name containing the global and local input parameters
#'    - author name, output version number and any comments that are to be written to the OutputVersionControl.txt file
#'      documenting the key aspects of the model run (timestamp, author name, cities
#'      for which model was run, input parameter file name, output version number,
#'      number of samples which equals to 1 here as the model is run in constant mode, 
#'      and any comments)
#'    - The scenarios need defining by:
#'      - updating the character defining which scenario script is to be called
#'      - giving the reference scenario against which all other scenarios are compared,
#'        this reference scenario needs to be the scenario name which corresponds to the current input parameter files
#'      - giving the percentage increase in each mode for the BOGOTA (GLOBAL, LATAM, and AFRICA_INDIA) scenarios
#'    - the name of the diseases considered in the output plot need defining. Note that no more than 6 diseases
#'      can be plotted at the same time and if the script is run for many cities at once (roughly > 10), 
#'      the plot gets overcrowded
#'      
#' - the remainder of the code does not need to be changed:
#' 
#' - local and global input parameters from the input parameter spreadsheet are read in and put into the correct format needed
#'   for the model run
#'   
#' - The run_ithim_setup.R script is called which prepares the input data needed for the health impact assessment
#' 
#' - The run_ithim.R script is called which performs the health impact assessment
#' 
#' - output results are stored for plotting of the results
#' 
#' - an output plot is created 
#'
#' - the following output files are saved:
#'   - OutputVersionControl.txt documenting the key aspects of the model run (main folder)
#'   - the ithim_objects list containing the key input files and the health burden results
#'     is stored in results/multi_city/io.rds
#'
#'
#'
#'


rm(list=ls())
library(ithimr)
library(readxl)
library(truncnorm)
library(tidyverse)

if (!require("drpa",character.only = TRUE)) {
  print('Installing "drpa" package...')
  remotes::install_github("meta-analyses/drpa")
  library(drpa)
  print("")
}

# All cities
# cities <- c('antofagasta', 'arica', 'belo_horizonte', 'bogota', 'buenos_aires',
#             'cali', 'copiapo', 'coquimbo_laserena', 'gran_valparaiso',
#             'iquique_altohospicio', 'medellin', 'mexico_city', 'montevideo',
#             'osorno', 'puerto_montt', 'san_antonio',
#             'santiago', 'sao_paulo', 'temuco_padrelascasas', 'valdivia',
#             'accra', 'bangalore', 'cape_town','delhi', 'vizag', 'kisumu', 'nairobi', 'port_louis')

# # latam
# cities = c('antofagasta', 'arica', 'belo_horizonte', 'bogota', 'buenos_aires',
#            'cali', 'copiapo', 'coquimbo_laserena', 'gran_valparaiso',
#            'iquique_altohospicio', 'medellin', 'mexico_city', 'montevideo',
#            'osorno', 'puerto_montt', 'san_antonio',
#            'santiago', 'sao_paulo', 'temuco_padrelascasas', 'valdivia')
# 
# #African & Indian cities
#cities <- c('accra','cape_town','kisumu', 'nairobi', 'port_louis', 'bangalore', 'delhi', 'vizag')

cities <- 'bogota'


input_parameter_file <- "InputParameters_v35.0.xlsx" # file containing the local and global input parameters


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
output_version <- paste0(repo_sha, "_test_run") # gives the version number of the output documents, independent of the input parameter file name
author <- "DGS"
comment <- "Testing after changing order in names in injury_death_to_yll function"

# scenario definition
scenario_name <- "BOGOTA" # name of scenario to be called
# scenario the other scenarios are compared to, the reference scenario name should always
# be the name of the scenario corresponding to the actual baseline burden of disease and 
# other input data for the city 
reference_scenario <- 'Baseline' 
scenario_increase <- 0.05 # increase for each mode in each scenario (used in GLOBAL, BOGOTA, LATAM and AFRICA_INDIA scenarios)


# define which output results to plot, more than 6 cannot be plotted
# potential outputs (in yll for all scenarios):  c('pa_ap_all_cause', 'pa_ap_IHD', 'pa_total_cancer', 'pa_ap_lung_cancer', 'ap_COPD', 
#                       'pa_ap_stroke', 'pa_ap_T2D', 'ap_LRI', 'pa_breast_cancer', 'pa_colon_cancer', 'pa_endo_cancer',
#                       'pa_liver_cancer', 'pa_ap_CVD', 'pa_total_dementia', 'pa_myeloma', 'pa_Parkinson',
#                       'pa_head_neck_cancer', 'pa_stomach_cancer', 'inj')
outputs_to_plot <- c('pa_ap_all_cause', 'ap_COPD', 'pa_total_dementia', 'pa_myeloma', 'ap_LRI','pa_stomach_cancer' )





############################### No need to change the following ##################################
compute_mode <- 'constant' # constant parameters from the given parameters

# keep record when code started:
starttime <- Sys.time()

# read in local input parameters
all_inputs <- read_excel(input_parameter_file, sheet = "all_city_parameter_inputs")
all_inputs[is.na(all_inputs)] <- ""
all_inputs <- as.data.frame(all_inputs)

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

# write input parameters to global environment
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

list2env(global_parameter_list, environment()) # write input parameters to global environment

# update the format of some of the global parameters
dist_cat <- unlist(strsplit(gsub(" ", "", dist_cat, fixed = TRUE), "\\,"))

outcome_age_min <- as.numeric(unlist(strsplit(gsub(" ", "", outcome_age_min, fixed = TRUE), "\\,")))
outcome_age_max <- as.numeric(unlist(strsplit(gsub(" ", "", outcome_age_max, fixed = TRUE), "\\,")))
outcome_age_groups <- unlist(strsplit(gsub(" ", "", outcome_age_groups, fixed = TRUE), "\\,"))

min_age <- as.numeric(min_age)
max_age <- as.numeric(max_age)



################################### Start running the the actual analysis

# logical for PA dose response: set F - use quantile 0.5
pa_dr_quantile <-  F
# logical for AP dose response: set F - use quantile 0.5
ap_dr_quantile <-  F

ithim_objects <- outcome <- outcome_pp <- yll_per_hundred_thousand <- list()


print(system.time(for(city in cities){
  cat('\n')
  print(city)
  # run code to prepare the input data for the actual ITHIM Global health impact assessment
  ithim_objects[[city]] <- run_ithim_setup(
    DIST_CAT = as.character(dist_cat),
    ADD_WALK_TO_PT_TRIPS = as.logical(add_walk_to_pt_trips[[city]]),
    CITY = city,
    AGE_RANGE = c(min_age,max_age),
    ADD_TRUCK_DRIVERS = as.logical(add_truck_drivers),
    ADD_BUS_DRIVERS = as.logical(add_bus_drivers),
    ADD_CAR_DRIVERS = as.logical(add_car_drivers),
    ADD_MOTORCYCLE_FLEET = as.logical(add_motorcycle_fleet[[city]]),
    ADD_PERSONAL_MOTORCYCLE_TRIPS = as.character(add_personal_motorcycle_trips[[city]]),
    PM_emission_inventory = PM_emission_inventories[[city]],
    CO2_emission_inventory = CO2_emission_inventories[[city]],
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
    PA_DOSE_RESPONSE_QUANTILE = pa_dr_quantile,  
    AP_DOSE_RESPONSE_QUANTILE = ap_dr_quantile,
    INJURY_REPORTING_RATE = injury_reporting_rate[[city]],  
    CHRONIC_DISEASE_SCALAR = chronic_disease_scalar[[city]],  
    PM_CONC_BASE = pm_conc_base[[city]],  
    PM_TRANS_SHARE = pm_trans_share[[city]],  
    BACKGROUND_PA_SCALAR = background_pa_scalar[[city]],
    BUS_WALK_TIME = bus_walk_time[[city]],
    RAIL_WALK_TIME = rail_walk_time[[city]],
    
    BUS_TO_PASSENGER_RATIO = bus_to_passenger_ratio[[city]],
    TRUCK_TO_CAR_RATIO = truck_to_car_ratio[[city]],
    CAR_OCCUPANCY_RATIO = car_occupancy_ratio[[city]],
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
    MINIMUM_PT_TIME = as.numeric(minimum_pt_time),
    MODERATE_PA_CONTRIBUTION = as.numeric(moderate_pa_contribution)
  )
  
  # add additional information to the ithim_objects list storing the key input and output data
  ithim_objects$scen_prop <- SCENARIO_PROPORTIONS
  ithim_objects[[city]]$demographic <- DEMOGRAPHIC
  ithim_objects[[city]]$synth_pop <- SYNTHETIC_POPULATION
  
  # run the ITHIM-Global health impact assessment
  ithim_objects[[city]]$outcomes <- run_ithim(ithim_object=ithim_objects[[city]], seed = 1)
  
  # add further information to the ithim_objects list
  ithim_objects[[city]]$disease_burden <- DISEASE_BURDEN
  ithim_objects[[city]]$PM_emission_inventory <- PM_EMISSION_INVENTORY
  ithim_objects[[city]]$injury_table <- INJURY_TABLE
  ithim_objects[[city]]$vehicle_inventory <- VEHICLE_INVENTORY
  ithim_objects[[city]]$location$country <- country[[CITY]]
  ithim_objects[[city]]$location$continent <- continent[[CITY]]
  ithim_objects[[city]]$new_walk_trips_count <- list()
  ithim_objects[[city]]$new_walk_trips_count$all <- count_new_walk_trips
  ithim_objects[[city]]$new_walk_trips_count$bus <- count_new_walk_trips_bus
  ithim_objects[[city]]$new_walk_trips_count$rail <- count_new_walk_trips_rail
 
  # store results to plot
  min_ages <- sapply(ithim_objects[[city]]$outcome$hb$ylls$age_cat,function(x)as.numeric(strsplit(x,'-')[[1]][1]))
  max_ages <- sapply(ithim_objects[[city]]$outcome$hb$ylls$age_cat,function(x)as.numeric(strsplit(x,'-')[[1]][2]))
  sub_outcome <- subset(ithim_objects[[city]]$outcome$hb$ylls,
                        min_ages >= min_age & max_ages <= max_age)

  
  # all results without upper and lower confidence interval limit values
  sub_outcome_noLimits <- sub_outcome %>% dplyr::select(-contains(c('lb','ub')))
  
  # results for plotting without upper and lower confidence interval limit values
  sub_outcomes_plot <- sub_outcome_noLimits %>% dplyr::select(contains(outputs_to_plot))
  # replace column names with 'yll_' with 'ylls_'
  colnames(sub_outcomes_plot) <- sub("yll_", "ylls_", colnames(sub_outcomes_plot))
  result_mat_plot <- colSums(sub_outcomes_plot)
  
  # find number of disease to plot and create a list with all the different disease outcomes for the different scenarios
  columns <- length(result_mat_plot)
  nDiseases <- columns/NSCEN
  if (city == cities[1]) {
    disease_list <- list()
    for (i in 1:nDiseases) disease_list[[i]] <- matrix(0, NSCEN, ncol = length(cities))
  }
  min_pop_ages <- sapply(DEMOGRAPHIC$age,function(x)as.numeric(strsplit(x,'-')[[1]][1]))
  max_pop_ages <- sapply(DEMOGRAPHIC$age,function(x)as.numeric(strsplit(x,'-')[[1]][2]))
  for (i in 1:nDiseases)
    disease_list[[i]][,which(cities == city)] <- result_mat_plot[1:NSCEN + (i - 1) * NSCEN]/sum(subset(DEMOGRAPHIC,min_pop_ages >= min_age & max_pop_ages <= max_age)$population)
}))


# add run relevant information to ithim_objects list
timestamp <- Sys.time()
ithim_objects$ithim_run <- list()
ithim_objects$ithim_run$input_parameter_file <- input_parameter_file
ithim_objects$ithim_run$scenarios_used <- scenario_name
ithim_objects$ithim_run$reference_scenario <- reference_scenario
ithim_objects$ithim_run$scenario_increase <- scenario_increase
ithim_objects$ithim_run$scenario_names <- SCEN
ithim_objects$ithim_run$compute_mode <- compute_mode
ithim_objects$ithim_run$timestamp <- timestamp
ithim_objects$ithim_run$output_version <- output_version
ithim_objects$ithim_run$author <- author
ithim_objects$ithim_run$comment <- comment

# Create the output plots
{x11(width = 10, height = 8);
  #layout.matrix <- matrix(c(2:6,1,7:12), nrow = 2, ncol = 6,byrow = T)
  layout.matrix <- matrix(c(1:6), nrow = 2, ncol = 3,byrow = T)
  graphics::layout(mat = layout.matrix, heights = c(2,3),
                   #widths = c(2.8,2,2,2,2,2.5))
                   widths = c(2,2,2))
  ylim <- range(result_mat_plot) # define y limit
  cols <- rainbow(length(cities)) # define colours
  mar1 <- rep(7, nDiseases); mar1[1:6] <- 2.5 # define bottom margin
  mar2 <- rep(1, nDiseases); mar2[c(2,5)] <- 2; mar2[c(1,4)] <- 4.5 # define margin left of plots
  
  for (i in 1:nDiseases) {
    #ylim <- if (i %in% c(1,4)) range(disease_list[[i]]) else c(-11,4)*1e-4
    ylim <- range(disease_list[[i]])
    par(mar = c(mar1[i], mar2[i], 6, 1)) # define margins
    barplot(t(disease_list[[i]]), ylim = ylim, las = 2, beside = T,
            col = cols,
            main = paste0(last(strsplit(names(result_mat_plot)[i * NSCEN], 'ylls_')[[1]])),
            yaxt='n') # create boxplot
 
    # add y-axis label
    axis(2,cex.axis=1.5); 
    if(i%in%c(1,4)) mtext(side=2,'YLL gain per person',line=3)
    
    # add city legend
    if (i == nDiseases - 1) legend(legend = cities, fill = cols, bty = 'n',
                                   y = -1e-5, x = 5, cex = 0.9)
    # add scenario names
    scen_names_only <- c()
    for (i in 1:NSCEN)
      scen_names_only <- paste0(scen_names_only,"    ", SCEN_SHORT_NAME[i+1], "    ")
    mtext(side = 1, scen_names_only, line = 1, cex = 0.8)
  }
}


saveRDS(ithim_objects, paste0("results/multi_city/io_",output_version,".rds"), version = 2)



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
      file="OutputVersionControl.txt",sep="\n",append=TRUE)
  
  # record time it took to run code
  endtime <- Sys.time()  
  runtime <- round(as.numeric(difftime(endtime, starttime, units = "mins")),2)
  
  cat(paste("Runtime in minutes:", runtime, sep=" "),
      file="OutputVersionControl.txt",sep="\n",append=TRUE)

}