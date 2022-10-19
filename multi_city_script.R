rm(list=ls())
library(ithimr)
library(readxl)
library(truncnorm)

if (!require("drpa",character.only = TRUE)) {
  print('Installing "drpa" package...')
  remotes::install_github("meta-analyses/drpa")
  library(drpa)
  print("")
}

# All cities
cities <- c('antofagasta', 'arica', 'belo_horizonte', 'bogota', 'buenos_aires',
            'cali', 'copiapo', 'coquimbo_laserena', 'gran_valparaiso',
            'iquique_altohospicio', 'medellin', 'mexico_city', 'montevideo',
            'osorno', 'puerto_montt', 'san_antonio',
            'santiago', 'sao_paulo', 'temuco_padrelascasas', 'valdivia',
            'accra', 'bangalore', 'cape_town','delhi', 'vizag', 'kisumu', 'nairobi', 'port_louis')

# # latam
# cities = c('antofagasta', 'arica', 'belo_horizonte', 'bogota', 'buenos_aires',
#            'cali', 'copiapo', 'coquimbo_laserena', 'gran_valparaiso',
#            'iquique_altohospicio', 'medellin', 'mexico_city', 'montevideo',
#            'osorno', 'puerto_montt', 'san_antonio',
#            'santiago', 'sao_paulo', 'temuco_padrelascasas', 'valdivia')
# 
# #African & Indian cities
 cities <- c('accra','cape_town','kisumu', 'nairobi', 'port_louis', 'bangalore', 'delhi', 'vizag')
 
# number of times input values are sampled from each input parameter distribution

input_parameter_file <- "InputParameters_v15.0.xlsx"

output_version <- "v0.3" # gives the version number of the output documents, independent of the input parameter file name
author <- "AA"
comment <- "Added CO2 emission sampling"

# scenario definition
scenario_name <- "GLOBAL"
reference_scenario <- 'Baseline'

compute_mode <- 'constant' # constant parameters from the given parameters
############################### No need to change the following ##################################
# keep record when code started:
starttime <- Sys.time()

# define min and max age to be considered
# min_age <- 15
# max_age <- 69
# # set age ranges for outcome statistics
# outcome_age_min <- c(15,50)
# outcome_age_max <- c(49,69)
# outcome_age_groups <- c('15-49','50-69')
# 

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

day_to_week_scalar <- as.numeric(day_to_week_scalar)

################################### Start running the the actual analysis

# logical for PA dose response: set F - use quantile 0.5
pa_dr_quantile <-  F
# logical for AP dose response: set F - use quantile 0.5
ap_dr_quantile <-  F

ithim_objects <- outcome <- outcome_pp <- yll_per_hundred_thousand <- list()

#toplot <- matrix(0, nrow = 3, ncol = length(cities)) #3 scenarios, 20 cities
#ithim_objects <- list()
print(system.time(for(city in cities){
  print(city)
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
    MMET_CYCLING = mmet_cycling, 
    MMET_WALKING = mmet_walking, 
    DAY_TO_WEEK_TRAVEL_SCALAR = day_to_week_scalar,
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
    SCENARIO_NAME = scenario_name
  )
  
  ithim_objects$scen_prop <- SCENARIO_PROPORTIONS
  ithim_objects[[city]]$demographic <- DEMOGRAPHIC
  ithim_objects[[city]]$synth_pop <- SYNTHETIC_POPULATION
  ithim_objects[[city]]$outcomes <- run_ithim(ithim_object=ithim_objects[[city]], seed = 1)
  ithim_objects[[city]]$disease_burden <- DISEASE_BURDEN
  ithim_objects[[city]]$PM_emission_inventory <- PM_EMISSION_INVENTORY
  ithim_objects[[city]]$injury_table <- INJURY_TABLE
  ithim_objects[[city]]$vehicle_inventory <- VEHICLE_INVENTORY
  
  ## store results to plot
  # min_ages <- sapply(ithim_objects[[city]]$outcome$hb$ylls$age_cat,function(x)as.numeric(strsplit(x,'-')[[1]][1]))
  # max_ages <- sapply(ithim_objects[[city]]$outcome$hb$ylls$age_cat,function(x)as.numeric(strsplit(x,'-')[[1]][2]))
  # sub_outcome <- subset(ithim_objects[[city]]$outcome$hb$ylls,
  #                       min_ages >= min_age & max_ages <= max_age)
  # result_mat <- colSums(sub_outcome[,3:ncol(sub_outcome)])
  # columns <- length(result_mat)
  # nDiseases <- columns/NSCEN
  # if (city == cities[1]) {
  #   disease_list <- list()
  #   for (i in 1:nDiseases) disease_list[[i]] <- toplot
  # }
  # min_pop_ages <- sapply(DEMOGRAPHIC$age,function(x)as.numeric(strsplit(x,'-')[[1]][1]))
  # max_pop_ages <- sapply(DEMOGRAPHIC$age,function(x)as.numeric(strsplit(x,'-')[[1]][2]))
  # for (i in 1:nDiseases)
  #   disease_list[[i]][,which(cities == city)] <- result_mat[1:NSCEN + (i - 1) * NSCEN]/sum(subset(DEMOGRAPHIC,min_pop_ages >= min_age & max_pop_ages <= max_age)$population)
}))


# {x11(width = 10, height = 5);
#   layout.matrix <- matrix(c(2:6,1,7:12), nrow = 2, ncol = 6,byrow = T)
#   graphics::layout(mat = layout.matrix, heights = c(2,3), 
#                    widths = c(2.8,2,2,2,2,2.5))
#   ylim <- range(result_mat)
#   cols <- rainbow(length(cities))
#   mar1 <- rep(7, nDiseases); mar1[1:6] <- 1
#   mar2 <- rep(1, nDiseases); mar2[c(2,7)] <- 6; mar2[c(1,12)] <- 3
#   for (i in 1:nDiseases) {
#     ylim <- if (i %in% c(1,12)) range(disease_list[[i]]) else c(-11,4)*1e-4
#     par(mar = c(mar1[i], mar2[i], 4, 1))
#     barplot(t(disease_list[[i]]), ylim = ylim, las = 2, beside = T, 
#             col = cols, 
#             names.arg = if (i < 7) NULL else rownames(SCENARIO_PROPORTIONS), 
#             main = paste0(last(strsplit(names(result_mat)[i * NSCEN], '_')[[1]])),
#             yaxt='n')
#     if (i %in% c(2,1,7,12)) {
#       axis(2,cex.axis=1.5); if(i%in%c(2,7)) mtext(side=2,'YLL gain per person',line=3)}
#     if (i == nDiseases - 1) legend(legend = cities, fill = cols, bty = 'n',
#                                    y = -1e-5, x = 5, cex = 0.9)
#   }
# }


saveRDS(ithim_objects, "results/multi_city/io.rds", version = 2)
