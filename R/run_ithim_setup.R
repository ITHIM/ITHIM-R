#' Run the set up script for ITHIM
#' 
#' Sets up the basic ITHIM object for onward calculation. Data loading, processing and harmonisation. Setting global values.
#' 
#' Parameters have two options: to be set to a constant, and to be sampled from a prespecified distribution.
#' Each parameter is given as an argument of length 1 or 2. 
#' If length 1, it's constant, and set to the global environment. 
#' If length 2, a distribution is defined and sampled from NSAMPLE times.
#' There are some exceptions, listed above
#' 
#' @param seed random seed
#' @param CITY name of the city, and name of the directory containing city data files
#' @param speeds named list of mode speeds
#' @param PM_emission_inventory named list of mode emissions
#' @param setup_call_summary_filename name to write setup call summary to
#' @param DIST_CAT vector string of distance categories in the form '0-6'. (The unit is assumed to be the same as in the trip set.)
#' @param AGE_RANGE vector of minimum and maximum ages to include
#' @param ADD_WALK_TO_BUS_TRIPS logic: whether or not to add short walks to all PT trips
#' @param ADD_BUS_DRIVERS logic: whether or not to add bus drivers
#' @param ADD_TRUCK_DRIVERS logic: whether or not to add truck drivers
#' @param ADD_MOTORCYCLE_FLEET logic: whether or not to add motorcycle fleet
#' @param TEST_WALK_SCENARIO logic: whether or not to run the walk scenario
#' @param TEST_CYCLE_SCENARIO logic: whether or not to run the cycle scenario
#' @param MAX_MODE_SHARE_SCENARIO logic: whether or not to run the max mode share scenario
#' @param REFERENCE_SCENARIO which scenario forms the reference for the health comparison
#' @param PATH_TO_LOCAL_DATA path to CITY directory, if not using package
#' @param NSAMPLES constant integer: number of samples to take
#' @param BUS_WALK_TIME lognormal parameter: duration of walk to PT
#' @param MMET_CYCLING lognormal parameter: mMETs when cycling
#' @param MMET_WALKING lognormal parameter: mMETs when walking
#' @param PM_CONC_BASE lognormal parameter: background PM2.5 concentration
#' @param PM_TRANS_SHARE beta parameter: fraction of background PM2.5 attributable to transport
#' @param PA_DOSE_RESPONSE_QUANTILE logic: whether or not to sample from PA RR DR functions
#' @param AP_DOSE_RESPONSE_QUANTILE logic: whether or not to sample from AP RR DR functions
#' @param BACKGROUND_PA_SCALAR lognormal parameter: reporting scalar for PA
#' @param BACKGROUND_PA_CONFIDENCE beta parameter: confidence in accuracy of PA survey
#' @param INJURY_REPORTING_RATE lognormal parameter: rate of injury reporting
#' @param CHRONIC_DISEASE_SCALAR lognormal parameter: scalar for background disease rates
#' @param DAY_TO_WEEK_TRAVEL_SCALAR beta parameter: rate of scaling travel from one day to one week
#' @param SIN_EXPONENT_SUM lognormal parameter: linearity of injuries with respect to two modes. SIN_EXPONENT_SUM=2 means no safety in numbers.
#' @param CASUALTY_EXPONENT_FRACTION beta parameter: casualty contribution to SIN_EXPONENT_SUM
#' @param BUS_TO_PASSENGER_RATIO beta parameter: number of buses per passenger
#' @param TRUCK_TO_CAR_RATIO beta parameter: number of trucks per car
#' @param FLEET_TO_MOTORCYCLE_RATIO beta parameter: fraction of total motorcycles that's fleet
#' @param PM_EMISSION_INVENTORY_CONFIDENCE beta parameter: confidence in accuracy of emission inventory
#' @param DISTANCE_SCALAR_CAR_TAXI lognormal parameter: scalar for car distance travelled
#' @param DISTANCE_SCALAR_WALKING lognormal parameter: scalar for walking distance travelled
#' @param DISTANCE_SCALAR_PT lognormal parameter: scalar for PT distance travelled
#' @param DISTANCE_SCALAR_CYCLING lognormal parameter: scalar for cycling distance travelled
#' @param DISTANCE_SCALAR_MOTORCYCLE lognormal parameter: scalar for motorcycle distance travelled
#' 
#' @return ithim_object list of objects for onward use.
#' 
#' @export
run_ithim_setup <- function(seed = 1,
                            CITY = 'accra',
                            speeds = NULL,
                            PM_emission_inventory = NULL,
                            CO2_emission_inventory = NULL,
                            setup_call_summary_filename = 'setup_call_summary.txt',
                            DIST_CAT = c("0-6 km", "7-9 km", "10+ km"),
                            AGE_RANGE = c(0,150),
                            ADD_WALK_TO_BUS_TRIPS = T,
                            ADD_BUS_DRIVERS = T,
                            ADD_TRUCK_DRIVERS = T,
                            ADD_MOTORCYCLE_FLEET = F,
                            TEST_WALK_SCENARIO = F,
                            TEST_CYCLE_SCENARIO = F,
                            MAX_MODE_SHARE_SCENARIO = F,
                            REFERENCE_SCENARIO = 'Baseline',
                            PATH_TO_LOCAL_DATA = NULL,
                            NSAMPLES = 1,
                            BUS_WALK_TIME= 5,
                            MMET_CYCLING = 4.63,
                            MMET_WALKING = 2.53,
                            PM_CONC_BASE = 50,  
                            PM_TRANS_SHARE = 0.225,
                            PA_DOSE_RESPONSE_QUANTILE = F,
                            AP_DOSE_RESPONSE_QUANTILE = F,
                            BACKGROUND_PA_SCALAR = 1,
                            BACKGROUND_PA_CONFIDENCE = 1,
                            INJURY_REPORTING_RATE = 1,
                            CHRONIC_DISEASE_SCALAR = 1,
                            DAY_TO_WEEK_TRAVEL_SCALAR = 7,
                            SIN_EXPONENT_SUM = 2,
                            CASUALTY_EXPONENT_FRACTION = 0.5,
                            BUS_TO_PASSENGER_RATIO = 0.022,
                            TRUCK_TO_CAR_RATIO = 0.21,
                            FLEET_TO_MOTORCYCLE_RATIO = 0.01,
                            PM_EMISSION_INVENTORY_CONFIDENCE = 1,
                            DISTANCE_SCALAR_CAR_TAXI = 1,
                            DISTANCE_SCALAR_WALKING = 1,
                            DISTANCE_SCALAR_PT = 1,
                            DISTANCE_SCALAR_CYCLING = 1,
                            DISTANCE_SCALAR_MOTORCYCLE = 1){
  
  ## SUMMARY OF INPUTS
  # seed = double. sets seed to allow some reproducibility.
  # CITY = string. used to identify input files.
  
  # speeds = named list of doubles. average mode speeds.
  # PM_emission_inventory = named list of doubles. vehicle emission factors.
  # CO2_emission_inventory = named list of doubles. CO2 emission factors
  # setup_call_summary_filename = string. Where to write input call summary.
  # DIST_CAT = vector of strings. defines distance categories for scenario generation (5 accra scenarios)
  
  # AGE_RANGE = vector of length 2, specifying the minimum and maximum ages to be used in the model. Note that the actual 
  # maximum and minimum will coincide with boundaries in the population and GBD files.
  
  # ADD_WALK_TO_BUS_TRIPS = logic. T: adds walk trips to all bus trips whose duration exceeds BUS_WALK_TIME. F: no trips added
  # ADD_BUS_DRIVERS = logic. T: adds `ghost trips', i.e. trips not taken by any participant. F: no trips added
  # ADD_TRUCK_DRIVERS = logic. T: adds `ghost trips', i.e. trips not taken by any participant. F: no trips added
  # ADD_MOTORCYCLE_FLEET = logic. T: adds `ghost trips', i.e. trips not taken by any participant. F: no trips added
  
  # TEST_WALK_SCENARIO = logic. T: run `scenario 0', one simple scenario where everyone takes one (extra) ten-minute walk trip. F: 5 Accra scenarios.
  # TEST_CYCLE_SCENARIO = logic. F: 5 Accra scenarios.
  # MAX_MODE_SHARE_SCENARIO = logic. T: run scenarios where we take the maximum mode share across cities and distance categories. F: 5 Accra scenarios.
  
  # REFERENCE_SCENARIO = string: at present, one of 'Baseline' or 'Scenario N' where N is an integer
  # PATH_TO_LOCAL_DATA = string: path to input files, if not one of the default case studies 
  
  # NSAMPLES = integer: number of samples to take for each parameter to be sampled
  
  # BUS_WALK_TIME = parameter. double: time taken to walk to bus. vector: samples from distribution.
  # MMET_CYCLING = parameter. double: sets cycling (M)METs. vector: samples from distribution.
  # MMET_WALKING = parameter. double: sets walking (M)METs. vector: samples from distribution.
  # PM_CONC_BASE = parameter. double: sets background PM. vector: samples from distribution.
  # PM_TRANS_SHARE = parameter. double: sets PM proportion that comes from transport. vector: samples from distribution.
  
  # PA_DOSE_RESPONSE_QUANTILE = logic. T: PA dose--response relationship is sampled. F: relationship is fixed.
  # AP_DOSE_RESPONSE_QUANTILE = logic. T: AP dose--response relationship is sampled. F: relationship is fixed.
  # CHRONIC_DISEASE_SCALAR = parameter. double: sets scalar for chronic disease background burden. vector: samples from distribution.
  
  # BACKGROUND_PA_SCALAR = parameter. double: sets scalar for background PA. vector: samples from distribution.
  # BACKGROUND_PA_CONFIDENCE = parameter. double between 0 and 1. 1 = use PA data as they are.
  # INJURY_REPORTING_RATE = parameter. double: sets scalar for injury counts (inverse). vector: samples from distribution.
  # SIN_EXPONENT_SUM = parameter. double: sets scalar. vector: samples from distribution.
  # CASUALTY_EXPONENT_FRACTION = parameter. double: sets scalar. vector: samples from distribution.
  
  # DAY_TO_WEEK_TRAVEL_SCALAR = parameter. double: sets scalar for extrapolation from day to week. vector: samples from distribution.
  # BUS_TO_PASSENGER_RATIO = parameter. double: sets bus distance relative to bus passenger distance. vector: samples from distribution.
  # TRUCK_TO_CAR_RATIO = parameter. double: sets truck distance relative to car. vector: samples from distribution.
  # FLEET_TO_MOTORCYCLE_RATIO = parameter. double: sets fleet distance relative to motorcycle. vector: samples from distribution.
  # PM_EMISSION_INVENTORY_CONFIDENCE = parameter. double between 0 and 1. 1 = use emission data as they are.
  # DISTANCE_SCALAR_CAR_TAXI = double: sets scalar. vector: samples from distribution.
  # DISTANCE_SCALAR_WALKING = double: sets scalar. vector: samples from distribution.
  # DISTANCE_SCALAR_PT = double: sets scalar. vector: samples from distribution.
  # DISTANCE_SCALAR_CYCLING = double: sets scalar. vector: samples from distribution.
  # DISTANCE_SCALAR_MOTORCYCLE = double: sets scalar. vector: samples from distribution.
  
  #################################################
  set.seed(seed)
  
  ithim_object <- list()
  
  ## SET GLOBAL VALUES
  ## PROGRAMMING VARIABLES
  NSAMPLES <<- NSAMPLES
  
  ## MODEL FLAGS
  ADD_WALK_TO_BUS_TRIPS <<- ADD_WALK_TO_BUS_TRIPS
  ADD_BUS_DRIVERS <<- ADD_BUS_DRIVERS
  ADD_TRUCK_DRIVERS <<- ADD_TRUCK_DRIVERS
  ADD_MOTORCYCLE_FLEET <<- ADD_MOTORCYCLE_FLEET
  TEST_WALK_SCENARIO <<- TEST_WALK_SCENARIO
  TEST_CYCLE_SCENARIO <<- TEST_CYCLE_SCENARIO
  MAX_MODE_SHARE_SCENARIO <<- MAX_MODE_SHARE_SCENARIO
  
  ## MODEL VARIABLES
  CITY <<- CITY
  if(is.null(PATH_TO_LOCAL_DATA)){
    PATH_TO_LOCAL_DATA <<- file.path(find.package('ithimr',lib.loc=.libPaths()), 'extdata/local/',CITY,'/') 
  }else{
    PATH_TO_LOCAL_DATA <<- PATH_TO_LOCAL_DATA
  }
  REFERENCE_SCENARIO <<- REFERENCE_SCENARIO
  AGE_RANGE <<- AGE_RANGE
  
  #BUS_TO_PASSENGER_RATIO <<- BUS_TO_PASSENGER_RATIO
  #TRUCK_TO_CAR_RATIO <<- TRUCK_TO_CAR_RATIO
  DIST_CAT <<- DIST_CAT
  DIST_LOWER_BOUNDS <<- as.numeric(sapply(strsplit(DIST_CAT, "[^0-9]+"), function(x) x[1]))
  
  ## fixed parameters for AP inhalation
  BASE_LEVEL_INHALATION_RATE <<- 1
  CLOSED_WINDOW_PM_RATIO <<- 0.5
  CLOSED_WINDOW_RATIO <<- 0.5
  ROAD_RATIO_MAX <<- 3.216
  ROAD_RATIO_SLOPE <<- 0.379
  SUBWAY_PM_RATIO <<- 0.8
  
  ## default speeds that can be edited by input. 
  default_speeds <- list(
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
    shared_taxi=21,
    cycle_rickshaw=10
  )
  if(!is.null(speeds)){
    for(m in names(speeds))
      default_speeds[[m]] <- speeds[[m]]
  }
  
  TRAVEL_MODES <<- tolower(names(default_speeds))
  MODE_SPEEDS <<- data.frame(stage_mode = TRAVEL_MODES, speed = unlist(default_speeds), stringsAsFactors = F)
  cat('\n  SPEEDS \n\n',file=setup_call_summary_filename,append=F)
  #print(MODE_SPEEDS)
  for(i in 1:nrow(MODE_SPEEDS)) {
    cat(paste0(MODE_SPEEDS[i,]),file=setup_call_summary_filename,append=T); 
    cat('\n',file=setup_call_summary_filename,append=T)
  }
  
  ## default PM2.5 emission contributions that can be edited by input. 
  default_PM_emission_inventory <- list(
    bus=0,
    bus_driver=0.82,
    car=0.228,
    taxi=0.011,
    pedestrian=0,
    cycle=0,
    motorcycle=0.011,
    truck=0.859,
    big_truck=0.711,
    other=0.082
  )
  if(!is.null(PM_emission_inventory)){
    for(m in names(PM_emission_inventory))
      if(grepl('bus$',m,ignore.case=T)&&!paste0(m,'_driver')%in%names(PM_emission_inventory)){
        default_PM_emission_inventory[[paste0(m,'_driver')]] <- PM_emission_inventory[[m]]
      }else{
        default_PM_emission_inventory[[m]] <- PM_emission_inventory[[m]]
      }
  }
  names(default_PM_emission_inventory) <- tolower(names(default_PM_emission_inventory))
    
  PM_EMISSION_INVENTORY <<- default_PM_emission_inventory
  cat('\n  PM 2.5 EMISSION INVENTORY \n\n',file=setup_call_summary_filename,append=T)
  for(i in 1:length(default_PM_emission_inventory)) {
    cat(paste(names(PM_EMISSION_INVENTORY)[i],PM_EMISSION_INVENTORY[[i]]),file=setup_call_summary_filename,append=T); 
    cat('\n',file=setup_call_summary_filename,append=T)
  }
  
  ## default C02 emission contributions that can be edited by input. 
  default_CO2_emission_inventory <- list(
    motorcycle = 15.95,
    car = 33.13,
    bus_driver = 9.76,
    big_truck = 3.68,
    truck	= 6.19,
    other	= 31.28
  )
  
  
  if(!is.null(CO2_emission_inventory)){
    for(m in names(CO2_emission_inventory))
      if(grepl('bus$',m,ignore.case=T)&&!paste0(m,'_driver')%in%names(CO2_emission_inventory)){
        default_CO2_emission_inventory[[paste0(m,'_driver')]] <- CO2_emission_inventory[[m]]
      }else{
        default_CO2_emission_inventory[[m]] <- CO2_emission_inventory[[m]]
      }
  }
  names(default_CO2_emission_inventory) <- tolower(names(default_CO2_emission_inventory))
  
  CO2_EMISSION_INVENTORY <<- default_CO2_emission_inventory
  cat('\n  CO2 EMISSION INVENTORY \n\n',file=setup_call_summary_filename,append=T)
  for(i in 1:length(default_CO2_emission_inventory)) {
    cat(paste(names(CO2_EMISSION_INVENTORY)[i],CO2_EMISSION_INVENTORY[[i]]),file=setup_call_summary_filename,append=T); 
    cat('\n',file=setup_call_summary_filename,append=T)
  }
  
  ## LOAD DATA
  ithim_load_data(setup_call_summary_filename,speeds=default_speeds)  
  
  if(any(!unique(TRIP_SET$stage_mode)%in%MODE_SPEEDS$stage_mode)){
    cat("\n  The following modes do not have speeds, and won't be included in the model:\n",file=setup_call_summary_filename,append=T)
    cat(unique(TRIP_SET$stage_mode)[!unique(TRIP_SET$stage_mode)%in%MODE_SPEEDS$stage_mode],file=setup_call_summary_filename,append=T)
    cat("\n\n  To include a mode, or change a speed, supply e.g. 'speeds=list(car=15,hoverboard=30)' in the call to 'run_ithim_setup'.\n\n",file=setup_call_summary_filename,append=T)
  }
  ## SET PARAMETERS
  ithim_object$parameters <- ithim_setup_parameters(NSAMPLES,
                                                    BUS_WALK_TIME,
                                                    MMET_CYCLING,
                                                    MMET_WALKING,
                                                    PM_CONC_BASE,  
                                                    PM_TRANS_SHARE,
                                                    PA_DOSE_RESPONSE_QUANTILE,
                                                    AP_DOSE_RESPONSE_QUANTILE,
                                                    BACKGROUND_PA_SCALAR,
                                                    BACKGROUND_PA_CONFIDENCE,
                                                    INJURY_REPORTING_RATE,
                                                    CHRONIC_DISEASE_SCALAR,
                                                    DAY_TO_WEEK_TRAVEL_SCALAR,
                                                    SIN_EXPONENT_SUM,
                                                    CASUALTY_EXPONENT_FRACTION,
                                                    BUS_TO_PASSENGER_RATIO,
                                                    TRUCK_TO_CAR_RATIO,
                                                    FLEET_TO_MOTORCYCLE_RATIO,
                                                    PM_EMISSION_INVENTORY_CONFIDENCE,
                                                    DISTANCE_SCALAR_CAR_TAXI,
                                                    DISTANCE_SCALAR_WALKING,
                                                    DISTANCE_SCALAR_PT,
                                                    DISTANCE_SCALAR_CYCLING,
                                                    DISTANCE_SCALAR_MOTORCYCLE)
  
  # programming flags: do we need to recompute elements given uncertain variables?
  RECALCULATE_PM_EMISSION_INVENTORY <<- any(c('PM_EMISSION_INVENTORY')%in%names(ithim_object$parameters))
  RECALCULATE_TRIPS <<- any(c('BUS_WALK_TIME',"DISTANCE_SCALAR_PT",
                              "DISTANCE_SCALAR_CAR_TAXI",
                              "DISTANCE_SCALAR_MOTORCYCLE",
                              "DISTANCE_SCALAR_WALKING",
                              "DISTANCE_SCALAR_CYCLING",
                              'BUS_TO_PASSENGER_RATIO',
                              'TRUCK_TO_CAR_RATIO',
                              'BACKGROUND_PA_ZEROS')%in%names(ithim_object$parameters))
  RECALCULATE_DISTANCES <<- RECALCULATE_TRIPS||any(c('SIN_EXPONENT_SUM',
                                                     'CASUALTY_EXPONENT_FRACTION')%in%names(ithim_object$parameters))
  
  ## complete TRIP_SET to contain distances and durations for trips and stages
  complete_trip_distance_duration() 
  if(!RECALCULATE_PM_EMISSION_INVENTORY) set_vehicle_inventory() # sets vehicle inventory
  
  ## create inventory and edit trips, if they are not variable dependent
  if(!RECALCULATE_TRIPS){
    ithim_object$trip_scen_sets <- get_synthetic_from_trips() # sets synthetic trips and synthetic population
  }
  
  ## calculate distances, if distances are not variable dependent
  if(!RECALCULATE_DISTANCES){
    ithim_object <- get_all_distances(ithim_object) # uses synthetic trips to calculate distances
  }
  ######################
  
  casualty_modes <- unique(INJURY_TABLE[[1]]$cas_mode)
  match_modes <- c(TRIP_SET$stage_mode,'pedestrian')
  if(ADD_TRUCK_DRIVERS) match_modes <- c(match_modes,'truck')
  if(!all(casualty_modes%in%match_modes)){
    cat('\n  The following casualty modes do not have distance data and will not be included in injury module:\n',file=setup_call_summary_filename,append=T)
    cat(casualty_modes[!casualty_modes%in%match_modes],file=setup_call_summary_filename,append=T)
    cat('\n\n',file=setup_call_summary_filename,append=T)
  }
  
  cat('\n  Emissions will be calculated for the following modes:\n',file=setup_call_summary_filename,append=T)
  cat(names(PM_EMISSION_INVENTORY)[unlist(PM_EMISSION_INVENTORY)>0],file=setup_call_summary_filename,append=T)
  cat("\n  To edit an emission contribution, supply e.g. 'PM_emission_inventory=list(car=4)' in the call to 'run_ithim_setup'.\n\n",file=setup_call_summary_filename,append=T)
  cat("  To exclude a mode from the emission inventory, supply e.g. 'PM_emission_inventory=list(other=0)' in the call to 'run_ithim_setup'.\n\n",file=setup_call_summary_filename,append=T)
  cat('\n\n',file=setup_call_summary_filename,append=T)
  
  return(ithim_object)
}
