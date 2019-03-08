#' @export
run_ithim_setup <- function(seed=1,
                            CITY = 'accra',
                            speeds = NULL,
                            #car_ratios = NULL,
                            emission_inventory = NULL,
                            setup_call_summary_filename = 'setup_call_summary.txt',
                            DIST_CAT = c("0-6 km", "7-9 km", "10+ km"),
                            ADD_WALK_TO_BUS_TRIPS = T,
                            ADD_BUS_DRIVERS = T,
                            ADD_TRUCK_DRIVERS = T,
                            TEST_WALK_SCENARIO = F,
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
                            INJURY_REPORTING_RATE = 1,
                            CHRONIC_DISEASE_SCALAR = 1,
                            DAY_TO_WEEK_TRAVEL_SCALAR = 7,
                            INJURY_LINEARITY= 1,
                            CASUALTY_EXPONENT_FRACTION = 0.5,
                            MOTORCYCLE_TO_CAR_RATIO = 0.2,
                            BUS_TO_PASSENGER_RATIO = 0.022,
                            TRUCK_TO_CAR_RATIO = 0.21){
  
  ## SUMMARY OF INPUTS
  # seed = double. sets seed to allow some reproducibility.
  # CITY = string. used to identify input files.
  
  # speeds = named list of doubles. average mode speeds.
  # car_ratios = named list of doubles. distances travelled by modes relative to car, for imputation if they are missing from trip set.
  # emission_inventory = named list of doubles. vehicle emission factors.
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
  
  ## default speeds that can be edited by input. 
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
    truck=21,
    van=15,
    subway=28
  )
  if(!is.null(speeds)){
    for(m in names(speeds))
      default_speeds[[m]] <- speeds[[m]]
  }
  
  TRAVEL_MODES <<- tolower(names(default_speeds))
  MODE_SPEEDS <<- data.frame(trip_mode = TRAVEL_MODES, speed = unlist(default_speeds), stringsAsFactors = F)
  cat('\n  SPEEDS \n\n',file=setup_call_summary_filename,append=F)
  #print(MODE_SPEEDS)
  for(i in 1:nrow(MODE_SPEEDS)) {
    cat(paste0(MODE_SPEEDS[i,]),file=setup_call_summary_filename,append=T); 
    cat('\n',file=setup_call_summary_filename,append=T)
  }
  
  ## default distances relative to car that can be edited by input. 
  #default_car_ratio <- list(
  #  bus=1,
  #  bus_driver=0.12,
  #  car=1,
  #  taxi=0.04,
  #  walking=1,
  #  bicycle=1,
  #  motorcycle=0.2,
  #  truck=0.21,
  #  big_truck=0.09,
  #  other=0.01
  #)
  #if(!is.null(car_ratios)){
  #  for(m in names(car_ratios))
  #    default_car_ratio[[m]] <- car_ratios[[m]]
  #}
  
  #names(default_car_ratio) <- tolower(names(default_car_ratio))
  #DISTANCE_RATIOS <<- default_car_ratio
  #cat('\n  DISTANCE RATIOS \n\n',file=setup_call_summary_filename,append=T)
  #for(i in 1:length(default_car_ratio)) {
  #  cat(paste(names(DISTANCE_RATIOS)[i],DISTANCE_RATIOS[[i]]),file=setup_call_summary_filename,append=T); 
  #  cat('\n',file=setup_call_summary_filename,append=T)
  #}
  
  ## default emission contributions that can be edited by input. 
  default_emission_inventory <- list(
    bus=0,
    bus_driver=0.82,
    car=0.228,
    taxi=0.011,
    walking=0,
    bicycle=0,
    motorcycle=0.011,
    truck=0.859,
    big_truck=0.711,
    other=0.082
  )
  if(!is.null(emission_inventory)){
    for(m in names(emission_inventory))
      if(grepl('bus',m,ignore.case=T)){
        default_emission_inventory[[paste0(m,'_driver')]] <- emission_inventory[[m]]
      }else{
        default_emission_inventory[[m]] <- emission_inventory[[m]]
      }
  }
  names(default_emission_inventory) <- tolower(names(default_emission_inventory))
    
  EMISSION_INVENTORY <<- default_emission_inventory
  cat('\n  EMISSION INVENTORY \n\n',file=setup_call_summary_filename,append=T)
  for(i in 1:length(default_emission_inventory)) {
    cat(paste(names(EMISSION_INVENTORY)[i],EMISSION_INVENTORY[[i]]),file=setup_call_summary_filename,append=T); 
    cat('\n',file=setup_call_summary_filename,append=T)
  }
  
  BUS_TO_PASSENGER_RATIO <<- BUS_TO_PASSENGER_RATIO
  TRUCK_TO_CAR_RATIO <<- TRUCK_TO_CAR_RATIO
  DIST_CAT <<- DIST_CAT
  DIST_LOWER_BOUNDS <<- as.numeric(sapply(strsplit(DIST_CAT, "[^0-9]+"), function(x) x[1]))
  
  ## fixed parameters for AP inhalation
  BASE_LEVEL_INHALATION_RATE <<- 10
  CLOSED_WINDOW_PM_RATIO <<- 0.5
  CLOSED_WINDOW_RATIO <<- 0.5
  ROAD_RATIO_MAX <<- 3.216
  ROAD_RATIO_SLOPE <<- 0.379
  
  ## LOAD DATA
  ithim_load_data()  
  
  if(any(!unique(TRIP_SET$trip_mode)%in%MODE_SPEEDS$trip_mode)){
    cat("\n  The following modes do not have speeds, and won't be included in the model:\n",file=setup_call_summary_filename,append=T)
    cat(unique(TRIP_SET$trip_mode)[!unique(TRIP_SET$trip_mode)%in%MODE_SPEEDS$trip_mode],file=setup_call_summary_filename,append=T)
    cat("\n\n  To include a mode, or change a speed, supply e.g. 'speeds=list(car=15,hoverboard=30)' in the call to 'run_ithim_setup'.\n\n",file=setup_call_summary_filename,append=T)
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
                                                    DAY_TO_WEEK_TRAVEL_SCALAR,
                                                    INJURY_LINEARITY,
                                                    CASUALTY_EXPONENT_FRACTION)
  
  # programming flags: do we need to recompute elements given uncertain variables?
  RECALCULATE_TRIPS <<- 'MOTORCYCLE_TO_CAR_RATIO'%in%names(ithim_object$parameters)
  RECALCULATE_DISTANCES <<- RECALCULATE_TRIPS||any(c('BUS_WALK_TIME','INJURY_LINEARITY','CASUALTY_EXPONENT_FRACTION')%in%names(ithim_object$parameters))
  
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
    cat('\n  The following casualty modes do not have distance data and will not be included in injury module:\n',file=setup_call_summary_filename,append=T)
    cat(casualty_modes[!casualty_modes%in%match_modes],file=setup_call_summary_filename,append=T)
    cat('\n\n',file=setup_call_summary_filename,append=T)
  }
  
  cat('\n  Emissions will be calculated for the following modes:\n',file=setup_call_summary_filename,append=T)
  cat(VEHICLE_INVENTORY$trip_mode[VEHICLE_INVENTORY$emission_inventory*VEHICLE_INVENTORY$distance_ratio_to_car>0],file=setup_call_summary_filename,append=T)
  cat("\n  To edit an emission contribution, supply e.g. 'emission_inventory=list(car=4)' in the call to 'run_ithim_setup'.\n\n",file=setup_call_summary_filename,append=T)
  cat("  To exclude a mode from the emission inventory, supply e.g. 'emission_inventory=list(other=0)' in the call to 'run_ithim_setup'.\n\n",file=setup_call_summary_filename,append=T)
  cat('\n\n',file=setup_call_summary_filename,append=T)
  
  return(ithim_object)
}
