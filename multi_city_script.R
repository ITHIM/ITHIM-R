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

# Disable scientific notation
options(scipen = 999)
options(datatable.fread.datatable=FALSE)

if (!require("drpa",character.only = TRUE)) {
  print('Installing "drpa" package...')
  remotes::install_github("meta-analyses/drpa")
  library(drpa)
  print("")
}

cities <- 'bogota'

input_parameter_file <- "Bogota_InputParameters_v6.0.xlsx" # file containing the local and global input parameters
# 

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
output_version <- repo_sha # gives the version number of the output documents, independent of the input parameter file name
author <- "AA"
comment <- "Simplified ithim by stripping off parameters"

# scenario definition
scenario_name <- "BOGOTA" # name of scenario to be called
# scenario the other scenarios are compared to, the reference scenario name should always
# be the name of the scenario corresponding to the actual baseline burden of disease and 
# other input data for the city 
reference_scenario <- 'Baseline' 
scenario_increase <- 0.05 # increase for each mode in each scenario (used in GLOBAL, BOGOTA, LATAM and AFRICA_INDIA scenarios)


# define which output results to plot
outputs_to_plot <- c('pa_ap_all_cause', 'pa_ap_IHD', 'pa_total_cancer', 'pa_ap_lung_cancer', 'ap_COPD', 
                     'pa_ap_stroke', 'pa_ap_T2D', 'ap_LRI', 'pa_breast_cancer', 'pa_colon_cancer', 'pa_endo_cancer',
                     'pa_liver_cancer', 'pa_ap_CVD', 'pa_total_dementia', 'pa_myeloma', 'pa_Parkinson',
                     'pa_head_neck_cancer', 'pa_stomach_cancer', 'inj')



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

min_age <- as.numeric(min_age)
max_age <- as.numeric(max_age)

################################### Start running the the actual analysis

# logical for PA dose response: set F - use quantile 0.5
pa_dr_quantile <-  F
# logical for AP dose response: set F - use quantile 0.5
ap_dr_quantile <-  F

ithim_objects <- outcome <- outcome_pp <- yll_per_hundred_thousand <- list()


#print(system.time(for(city in cities){
  city <- 'bogota'
  cat('\n')
  print(city)
  # run code to prepare the input data for the actual ITHIM Global health impact assessment
  ithim_objects[[city]] <- run_ithim_setup(
    DIST_CAT = as.character(dist_cat),
    CITY = city,
    AGE_RANGE = c(min_age,max_age),
    PM_emission_inventory = PM_emission_inventories[[city]],
    CO2_emission_inventory = CO2_emission_inventories[[city]],
    speeds = speeds[[city]],
    ADD_WALK_TO_PT_TRIPS = T,
    CYCLING_MMET =	cycling_mmet,
    WALKING_MMET =	walking_mmet,
    PASSENGER_MMET =	passenger_mmet,
    CAR_DRIVER_MMET =	car_driver_mmet,
    MOTORCYCLIST_MMET =	motorcyclist_mmet,
    SEDENTARY_ACTIVITY_MMET =	sedentary_activity_mmet,
    LIGHT_ACTIVITY_MMET =	light_activity_mmet,
    MODERATE_PA_MMET =	moderate_pa_mmet,
    VIGOROUS_PA_MMET	= vigorous_pa_mmet,
    PA_DOSE_RESPONSE_QUANTILE = pa_dr_quantile,  
    AP_DOSE_RESPONSE_QUANTILE = ap_dr_quantile,
    PM_CONC_BASE = pm_conc_base[[city]],  
    PM_TRANS_SHARE = pm_trans_share[[city]],  
    ADD_BUS_DRIVERS = F,
    ADD_CAR_DRIVERS = F,
    SCENARIO_NAME = scenario_name,
    SCENARIO_INCREASE = scenario_increase
  )
  
  # add additional information to the ithim_objects list storing the key input and output data
  ithim_objects$scen_prop <- SCENARIO_PROPORTIONS
  ithim_objects[[city]]$demographic <- DEMOGRAPHIC
  ithim_objects[[city]]$base_pop <- BASELINE_POPULATION
  
  # # run the ITHIM-Global health impact assessment
  ithim_objects[[city]]$outcomes <- run_ithim(ithim_object=ithim_objects[[city]], seed = 1)
  # 
  # # add further information to the ithim_objects list
  ithim_objects[[city]]$disease_burden <- DISEASE_BURDEN
  ithim_objects[[city]]$PM_emission_inventory <- PM_EMISSION_INVENTORY
  ithim_objects[[city]]$vehicle_inventory <- VEHICLE_INVENTORY
  ithim_objects[[city]]$location$country <- country[[CITY]]
  ithim_objects[[city]]$location$continent <- continent[[CITY]]
  
  # # store results to plot
  # min_ages <- sapply(ithim_objects[[city]]$outcome$hb$ylls$age_cat,function(x)as.numeric(strsplit(x,'-')[[1]][1]))
  # max_ages <- sapply(ithim_objects[[city]]$outcome$hb$ylls$age_cat,function(x)as.numeric(strsplit(x,'-')[[1]][2]))
  # sub_outcome <- subset(ithim_objects[[city]]$outcome$hb$ylls,
  #                       min_ages >= min_age & max_ages <= max_age)
  # 
  # 
  # # all results without upper and lower confidence interval limit values
  # sub_outcome_noLimits <- sub_outcome %>% dplyr::select(-contains(c('lb','ub')))
  # 
  # # results for plotting without upper and lower confidence interval limit values
  # sub_outcomes_plot <- sub_outcome_noLimits %>% dplyr::select(contains(outputs_to_plot))
  # # replace column names with 'yll_' with 'ylls_'
  # colnames(sub_outcomes_plot) <- sub("yll_", "ylls_", colnames(sub_outcomes_plot))
  # result_mat_plot <- colSums(sub_outcomes_plot)
  # 
  # # find number of disease to plot and create a list with all the different disease outcomes for the different scenarios
  # columns <- length(result_mat_plot)
  # nDiseases <- columns/NSCEN
  # if (city == cities[1]) {
  #   disease_list <- list()
  #   for (i in 1:nDiseases) disease_list[[i]] <- matrix(0, NSCEN, ncol = length(cities))
  # }
  # min_pop_ages <- sapply(DEMOGRAPHIC$age,function(x)as.numeric(strsplit(x,'-')[[1]][1]))
  # max_pop_ages <- sapply(DEMOGRAPHIC$age,function(x)as.numeric(strsplit(x,'-')[[1]][2]))
  # for (i in 1:nDiseases)
  #   disease_list[[i]][,which(cities == city)] <- result_mat_plot[1:NSCEN + (i - 1) * NSCEN]/sum(subset(DEMOGRAPHIC,min_pop_ages >= min_age & max_pop_ages <= max_age)$population)
# }))


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

get_qualified_scen_name <- function(cs){
  qualified_scen_name <- ""
  if (cs == 'base' | cs == 'baseline' | cs == 'Baseline')
    qualified_scen_name <- 'Baseline'
  else if(cs == "sc_walk")
    qualified_scen_name <- 'Walking'
  else if(cs == "sc_cycle")
    qualified_scen_name <- 'Cycling'
  else if(cs == "sc_car")
    qualified_scen_name <- 'Car'
  else if(cs == "sc_motorcycle")
    qualified_scen_name <- 'Motorcycling'
  else if(cs == "sc_bus")
    qualified_scen_name <- 'Bus'
  
  return(qualified_scen_name)
}

# initialise variables
health <- list()
measure <- c("ylls", "deaths")
pathway <- c("hb", "pathway_hb")

for (k in measure) { # loop for ylls or deaths
  for (j in pathway) { # loop for hb or pathway_hb
    for (i in names(ithim_objects)[!names(ithim_objects) %in% c('scen_prop','ithim_run' )]) { # loop for cities
      
      # Temporal dataset for health outcomes
      temp <- ithim_objects[[i]]$outcomes[[j]][[k]]
      
      # Total population
      #overall_pop <- sum(io[[i]]$demographic$population)
      
      # Population by age and sex
      pop_by_age_sex <- ithim_objects[[i]]$demographic %>% group_by(age, sex) %>%
        summarize(pop_age_sex = sum(population))
      
      # get the health burden into the correct format by creating a long format and adding additional columns
      # create long format
      health[[k]][[j]][[i]] <- gather(temp, key = "cause", "measure",
                                      -sex, -age_cat) %>%
        mutate(city = i, # create city column
               scenario = case_when( # create scenario column
                 grepl("sc_cycle", cause) ~ get_qualified_scen_name('sc_cycle'),
                 grepl("sc_car", cause) ~ get_qualified_scen_name('sc_car'),
                 grepl("sc_bus", cause) ~ get_qualified_scen_name('sc_bus'),
                 grepl("sc_motorcycle", cause) ~ get_qualified_scen_name('sc_motorcycle')
               ),
               level1 = case_when( # create level 1 column
                 grepl("all_cause", cause) ~ "L1: All Cause",
                 grepl("inj", cause) ~ "L1: RTI"
               ),
               level2 = case_when( # create level 2 column
                 grepl("total_cancer", cause) ~ "L2: Cancer",
                 grepl("CVD", cause) ~ "L2: CVD",
                 grepl("respiratory", cause) ~ "L2: Respiratory",
                 grepl("T2D", cause) ~ "L2: Other",
                 grepl("total_dementia", cause) ~ "L2: Other",
                 grepl("Parkinson", cause) ~ "L2: Other",
                 grepl("inj", cause) ~ "L2: RTI"
               ),
               level3 = case_when( # create level 3 column
                 grepl("IHD", cause) ~ "L3: IHD",
                 grepl("lung_cancer", cause) ~ "L3: lung_cancer",
                 grepl("COPD", cause) ~ "L3: COPD",
                 grepl("stroke", cause) ~ "L3: stroke",
                 grepl("T2D", cause) ~ "L3: T2D",
                 grepl("LRI", cause) ~ "L3: LRI",
                 grepl("breast_cancer", cause) ~ "L3: breast_cancer",
                 grepl("colon_cancer", cause) ~ "L3: colon_cancer",
                 grepl("endo_cancer", cause) ~ "L3: endo_cancer",
                 grepl("liver_cancer", cause) ~ "L3: liver_cancer",
                 grepl("total_dementia", cause) ~ "L3: total_dementia",
                 grepl("myeloma", cause) ~ "L3: myeloma",
                 grepl("myeloid_leukemia", cause) ~ "L3: Myeloid Leukemia",
                 grepl("Parkinson", cause) ~ "L3: Parkinson",
                 grepl("head_neck_cancer", cause) ~ "L3: head_neck_cancer",
                 grepl("stomach_cancer", cause) ~ "L3: stomach_cancer",
                 grepl("inj", cause) ~ "L3: RTI"
               ),
               dose = case_when( # create dose column
                 grepl("_pa_ap_", cause) ~ "PA and AP",
                 grepl("_pa_", cause) ~ "PA",
                 grepl("_ap_", cause) ~ "AP",
                 grepl("_inj", cause) ~ "RTI"
               )
        ) %>% # End mutate
        # join with demographic information
        left_join(pop_by_age_sex, by = c("age_cat" = "age", "sex" = "sex"))
      
      # %>%
      # create new columns showing burden per 100000 people by age or by age and sex
      # mutate(metric_100k = measure / overall_pop * 100000,
      #        metric_100k_sex = measure / overall_pop * 100000)
      
    } # Loop for each city
  } # Loop for each pathway
} # Loop for each measure


# for each health burden and pathway join all cities into one dataframe
ylls <-  rbindlist(health$ylls$hb, use.names = T) #|> filter(!str_detect(cause, "_lb") & !str_detect(cause, "_ub"))
ylls_pathway <- rbindlist(health$ylls$pathway_hb, use.names = T) #|> filter(!str_detect(cause, "_lb") & !str_detect(cause, "_ub"))
deaths <- rbindlist(health$deaths$hb, use.names = T) #|> filter(!str_detect(cause, "_lb") & !str_detect(cause, "_ub"))
deaths_pathway <- rbindlist(health$deaths$pathway_hb, use.names = T) #|> filter(!str_detect(cause, "_lb") & !str_detect(cause, "_ub"))

# write csv without output version number
write.csv(ylls,
          paste0('results/multi_city/health_impacts/ylls.csv'),
          row.names = F)
# Export ylls PATHWAY_HB
write.csv(ylls_pathway,
          paste0('results/multi_city/health_impacts/ylls_pathway.csv'),
          row.names = F)
# Export deaths HB
write.csv(deaths,
          paste0('results/multi_city/health_impacts/deaths.csv'),
          row.names = F)
# Export deaths PATHWAY_HB
write.csv(deaths_pathway,
          paste0('results/multi_city/health_impacts/deaths_pathway.csv'),
          row.names = F)


saveRDS(ithim_objects, "results/multi_city/io.rds", version = 2)
saveRDS(ithim_objects, "exercises/data/io.rds", version = 2)

# add to output control document

if (write_output_control == TRUE){

  input_version <- input_parameter_file
  global_path <- paste0(file.path(find.package('ithimr',lib.loc = .libPaths()),
                                  'extdata/global'), "/")
  cat("",
      paste(timestamp, "by", author, sep = " "),
      paste("Scenario:", SCENARIO_INCREASE * 100, "%", sep = " "),
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