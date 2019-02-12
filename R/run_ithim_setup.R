#' @export
run_ithim_setup <- function(seed=1,
                            CITY = 'accra',
                            speeds = NULL,
                            DIST_CAT = c("0-6 km", "7-9 km", "10+ km"),
                            ADD_WALK_TO_BUS_TRIPS = T,
                            ADD_BUS_DRIVERS = T,
                            ADD_TRUCK_DRIVERS = T,
                            TEST_WALK_SCENARIO = F,
                            REFERENCE_SCENARIO = 'Baseline',
                            PATH_TO_LOCAL_DATA = NULL,
                            #population=1600000,
                            #survey_coverage=1/365,
                            NSAMPLES = 1,
                            BUS_WALK_TIME= 5,
                            MMET_CYCLING = 4.63,
                            MMET_WALKING = 2.53,
                            PM_CONC_BASE = 50,  
                            PM_TRANS_SHARE = 0.225,
                            PA_DOSE_RESPONSE_QUANTILE = F,
                            AP_DOSE_RESPONSE_QUANTILE = F,
                            BACKGROUND_PA_SCALAR = 1,
                            INJURY_REPORTING_RATE = 1,
                            CHRONIC_DISEASE_SCALAR = 1,
                            DAY_TO_WEEK_TRAVEL_SCALAR = 7,
                            TAXI_TO_CAR_RATIO = 0.04,
                            BUS_TO_CAR_RATIO = 0.12,
                            BIG_TRUCK_TO_CAR_RATIO = 0.09,
                            MOTORCYCLE_TO_CAR_RATIO = 0.2,
                            TRUCK_TO_CAR_RATIO = 0.21,
                            OTHER_TO_CAR_RATIO = 0.01 ){
  
  ## SUMMARY OF INPUTS
  # seed = double. sets seed to allow some reproducibility.
  # CITY = string. used to identify input files.
  
  # speeds = named list of doubles. average mode speeds.
  # DIST_CAT = vector of strings. defines distance categories for scenario generation (5 accra scenarios)
  
  # ADD_WALK_TO_BUS_TRIPS = logic. T: adds walk trips to all bus trips whose duration exceeds BUS_WALK_TIME. F: no trips added
  # ADD_BUS_DRIVERS = logic. T: adds `ghost trips', i.e. trips not taken by any participant. F: no trips added
  # ADD_TRUCK_DRIVERS = logic. T: adds `ghost trips', i.e. trips not taken by any participant. F: no trips added
  # TEST_WALK_SCENARIO = logic. T: run `scenario 0', one simple scenario where everyone takes one (extra) ten-minute walk trip. F: 5 Accra scenarios.
  
  # REFERENCE_SCENARIO = string: at present, one of 'Baseline' or 'Scenario N' where N is an integer
  # PATH_TO_LOCAL_DATA = string: path to input files, if not one of the default case studies 
  
  # #population = integer, but this information should be covered in GBD
  # #survey_coverage = double, for when we have travel surveys covering different durations?
  
  # NSAMPLES = integer: number of samples to take for each parameter to be sampled
  
  # BUS_WALK_TIME = parameter. double: time taken to walk to bus. vector: samples from distribution.
  # MMET_CYCLING = parameter. double: sets cycling (M)METs. vector: samples from distribution.
  # MMET_WALKING = parameter. double: sets walking (M)METs. vector: samples from distribution.
  # PM_CONC_BASE = parameter. double: sets background PM. vector: samples from distribution.
  # PM_TRANS_SHARE = parameter. double: sets PM proportion that comes from transport. vector: samples from distribution.
  
  # PA_DOSE_RESPONSE_QUANTILE = logic. T: PA dose--response relationship is sampled. F: relationship is fixed.
  # AP_DOSE_RESPONSE_QUANTILE = logic. T: AP dose--response relationship is sampled. F: relationship is fixed.
  
  # BACKGROUND_PA_SCALAR = parameter. double: sets scalar for background PA. vector: samples from distribution.
  # INJURY_REPORTING_RATE = parameter. double: sets scalar for injury counts (inverse). vector: samples from distribution.
  # CHRONIC_DISEASE_SCALAR = parameter. double: sets scalar for chronic disease background burden. vector: samples from distribution.
  
  # MOTORCYCLE_TO_CAR_RATIO = parameter. double: sets motorcycle distance relative to car. vector: samples from distribution.
  
  # TAXI_TO_CAR_RATIO = double. Sets taxi distance relative to car.
  # BUS_TO_CAR_RATIO = double. Sets bus distance relative to car.
  # TRUCK_TO_CAR_RATIO = double. Sets truck distance relative to car.
  # BIG_TRUCK_TO_CAR_RATIO = double. Sets `big truck' contribution to emission relative to car.
  # OTHER_TO_CAR_RATIO = double. Sets `other' contribution to emission relative to car.
  
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
  TEST_WALK_SCENARIO <<- TEST_WALK_SCENARIO
  
  ## MODEL VARIABLES
  CITY <<- CITY
  if(is.null(PATH_TO_LOCAL_DATA)){
    PATH_TO_LOCAL_DATA <<- file.path(find.package('ithimr',lib.loc=.libPaths()), 'extdata/local/',CITY,'/') 
  }else{
    PATH_TO_LOCAL_DATA <<- PATH_TO_LOCAL_DATA
  }
  REFERENCE_SCENARIO <<- REFERENCE_SCENARIO
  
  ## default speeds that can be edited by input. We could do the same with emissions?
  default_speeds <- list(
    bus=15,
    bus_driver=15,
    minibus=15,
    minibus_driver=15,
    car=21,
    taxi=21,
    walking=4.8,
    walk_to_bus=4.8,
    bicycle=14.5,
    motorcycle=25,
    truck=25,
    van=15,
    subway=28
  )
  if(!is.null(speeds)){
    for(m in names(speeds))
      default_speeds[[m]] <- speeds[[m]]
  }
  
  TRAVEL_MODES <<- tolower(names(default_speeds))
  MODE_SPEEDS <<- data.frame(trip_mode = TRAVEL_MODES, speed = unlist(default_speeds), stringsAsFactors = F)
  cat('\n  Using speeds: \n')
  print(MODE_SPEEDS)
  DIST_CAT <<- DIST_CAT
  DIST_LOWER_BOUNDS <<- as.numeric(sapply(strsplit(DIST_CAT, "[^0-9]+"), function(x) x[1]))
  
  ## fixed parameters for AP inhalation
  BASE_LEVEL_INHALATION_RATE <<- 10
  CLOSED_WINDOW_PM_RATIO <<- 0.5
  CLOSED_WINDOW_RATIO <<- 0.5
  ROAD_RATIO_MAX <<- 3.216
  ROAD_RATIO_SLOPE <<- 0.379
  
  # transport mode ratios, for imputation where missing
  TAXI_TO_CAR_RATIO  <<- TAXI_TO_CAR_RATIO
  BUS_TO_CAR_RATIO  <<- BUS_TO_CAR_RATIO
  TRUCK_TO_CAR_RATIO  <<- TRUCK_TO_CAR_RATIO
  BIG_TRUCK_TO_CAR_RATIO <<- BIG_TRUCK_TO_CAR_RATIO
  OTHER_TO_CAR_RATIO <<- OTHER_TO_CAR_RATIO
  
  ## LOAD DATA
  ithim_load_data()  
  
  if(any(!unique(TRIP_SET$trip_mode)%in%MODE_SPEEDS$trip_mode)){
    cat("\n  The following modes do not have speeds, and won't be included in the model:\n")
    cat(unique(TRIP_SET$trip_mode)[!unique(TRIP_SET$trip_mode)%in%MODE_SPEEDS$trip_mode])
    cat("\n  To include a mode, supply e.g. 'speeds=list(car=15,bus=10)' in the call to 'run_ithim_setup'.\n\n")
  }
  ## SET PARAMETERS
  ithim_object$parameters <- ithim_setup_parameters(NSAMPLES,
                                                    BUS_WALK_TIME,
                                                    MMET_CYCLING,
                                                    MMET_WALKING,
                                                    PM_CONC_BASE,  
                                                    PM_TRANS_SHARE,
                                                    MOTORCYCLE_TO_CAR_RATIO,
                                                    PA_DOSE_RESPONSE_QUANTILE,
                                                    AP_DOSE_RESPONSE_QUANTILE,
                                                    BACKGROUND_PA_SCALAR,
                                                    INJURY_REPORTING_RATE,
                                                    CHRONIC_DISEASE_SCALAR,
                                                    DAY_TO_WEEK_TRAVEL_SCALAR)
  
  # programming flags: do we need to recompute elements given uncertain variables?
  RECALCULATE_TRIPS <<- 'MOTORCYCLE_TO_CAR_RATIO'%in%names(ithim_object$parameters)
  RECALCULATE_DISTANCES <<- RECALCULATE_TRIPS||'BUS_WALK_TIME'%in%names(ithim_object$parameters)
  
  ## create inventory and edit trips, if they are not variable dependent
  if(!RECALCULATE_TRIPS){
    set_vehicle_inventory() # sets vehicle inventory
    get_synthetic_from_trips() # sets synthetic trips and synthetic population
  }
  
  ## calculate distances, if distances are not variable dependent
  if(!RECALCULATE_DISTANCES){
    ithim_object <- get_all_distances(ithim_object) # uses synthetic trips to calculate distances
  }
  ######################
  
  casualty_modes <- unique(INJURY_TABLE[[1]]$cas_mode)
  match_modes <- c(TRIP_SET$trip_mode,'pedestrian')
  if(ADD_TRUCK_DRIVERS) match_modes <- c(match_modes,'truck')
  if(!all(casualty_modes%in%match_modes)){
    cat('\n  The following casualty modes do not have distance data and will not be included in injury module:\n')
    cat(casualty_modes[!casualty_modes%in%match_modes])
    cat('\n\n')
  }
  
  cat('  Emissions will be calculated for the following modes:\n')
  cat(VEHICLE_INVENTORY$trip_mode[VEHICLE_INVENTORY$emission_factor*VEHICLE_INVENTORY$distance_ratio_to_car>0])
  cat('\n\n')
  
  return(ithim_object)
}
