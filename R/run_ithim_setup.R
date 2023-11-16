#' Run the set up scripts for ITHIM
#' 
#' Sets up the basic ITHIM object for onward calculation. Data loading, processing and harmonisation. Setting of global values.
#' 
#' This function is used to read in the various input files and parameters and to 
#' process and harmonise the data ready for the health impact assessment. 
#' Input Parameters have two options: to be set to a constant or to be sampled 
#' from a pre-specified distribution. Most of these parameters are given as an 
#' argument of length 1 or 2. If of length 1, the parameter is usually used as 
#' a constant. If the parameter is of length 2, a distribution is defined 
#' and sampled from NSAMPLE times.
#' 
#' 
#' This function performs the following steps:
#' 
#' \itemize{
#' \item check whether a valid scenario name is called, get an error message if not
#' 
#' \item set various input parameters as global parameters
#' 
#' \item find the path to the local data
#' 
#' \item define fixed parameters for air pollution inhalation
#' 
#' 
#' \item define the mode speeds:
#'    \itemize{
#'    \item set default speeds for the various modes
#'    
#'    \item update the default speeds with city specific mode speeds if these are 
#'      given as input parameters
#'    
#'    \item ensure similar modes have the same speed assigned
#'    
#'    \item set-up dataframe with modes and speeds
#'    }
#'    
#' \item define PM emissions inventory
#'    \itemize{
#'    \item define default emission values
#'    
#'    \item update default values if city specific values are given as input parameters
#'    }
#'
#' \item define CO2 emissions inventory 
#'    \itemize{
#'    \item set default emission values
#'    
#'    \item update default values if city specific values are given as input parameters
#'    }
#'    
#' \item load and processe data from files by calling ithim_load_data.R
#' 
#' \item call ithim_setup_parameters.R to set the given input parameters to the global
#'   environment if running in constant mode or to obtain NSAMPLE samples from the
#'   given distributions for each of the input parameters if running in sample mode
#'    
#' \item set flags which cause certain parts of the model to be called at a later stage
#'  (ithim_uncertainty.R) IF certain input parameters were sampled from a distribution   
#'
#'\item call complete_trip_distance_duration.R to add any missing stage or distance
#'   information to the trip data  
#'
#' \item if none of the corresponding input parameters were sampled from a distribution, 
#'   call set_vehicle_inventory.R to create a dataframe with mode specific speed, 
#'   distance and emission information    
#'
#' \item if none of the corresponding input parameters were sampled from a distribution,
#'   call get_synthetic_from_trips to set synthetic trips and synthetic population   
#'
#' \item if none of the corresponding input parameters were sampled from a distribution,
#'   call get_all_distances.R to calculate trip distances
#' }
#' 
#' @param seed set seed to get the same results when sampling from a distribution
#' @param CITY name of the city, and name of the directory containing city data files
#' @param speeds named list of mode speeds
#' @param PM_emission_inventory named list of mode PM emissions
#' @param CO2_emission_inventory named list of CO2 mode emissions
#' @param DIST_CAT vector string of distance categories in the form '0-6'. (The unit is assumed to be the same as in the trip set and is related to speed values, usually in km)
#' @param AGE_RANGE vector of minimum and maximum ages to include
#' @param ADD_WALK_TO_PT_TRIPS logic: whether or not to add short walks to all PT trips
#' @param ADD_BUS_DRIVERS logic: whether or not to add bus drivers
#' @param ADD_CAR_DRIVERS logic: whether or not to find and add distance travelled by individual cars, denoted by car drivers
#' @param ADD_TRUCK_DRIVERS logic: whether or not to add truck drivers
#' @param ADD_MOTORCYCLE_FLEET logic: whether or not to add additional commercial motorcycle fleet as ghost trips
#' @param ADD_PERSONAL_MOTORCYCLE_TRIPS character: if 'no' does not add any personal motorcycle trips otherwise set to geographic region which defines the set-up of the motorcycle trips to be added 
#' @param REFERENCE_SCENARIO which scenario forms the reference for the health comparison
#' @param PATH_TO_LOCAL_DATA path to CITY directory, if not using package
#' @param NSAMPLES constant integer: number of samples to take
#' @param BUS_WALK_TIME lognormal parameter: duration of walk to bus stage
#' @param RAIL_WALK_TIME lognormal parameter: duration of walk to rail stage
#' @param CYCLING_MET lognormal parameter: METs when cycling
#' @param WALKING_MET lognormal parameter: METs when walking
#' @param PASSENGER_MET lognormal parameter: MET value associated with being a passenger on public transport
#' @param CAR_DRIVER_MET lognormal parameter: MET value associated with being a car driver 
#' @param MOTORCYCLIST_MET lognormal parameter: MET value associated with being a motorcyclist
#' @param SEDENTARY_ACTIVITY_MET lognormal parameter: MET value associated with sedentary activity
#' @param LIGHT_ACTIVITY_MET lognormal parameter: MET value associated with light activity
#' @param MODERATE_PA_MET lognormal parameter: MET value associated with moderate activity
#' @param VIGOROUS_PA_MET lognormal parameter: MET value associated with vigorous activity 
#' @param PM_CONC_BASE lognormal parameter: background PM2.5 concentration
#' @param PM_TRANS_SHARE beta parameter: fraction of background PM2.5 attributable to transport
#' @param PA_DOSE_RESPONSE_QUANTILE logic: whether or not to sample from physical activity relative risk dose response functions
#' @param AP_DOSE_RESPONSE_QUANTILE logic: whether or not to sample from air pollution relative risk dose response functions
#' @param BACKGROUND_PA_SCALAR lognormal parameter: reporting scalar for physical activity to correct bias in data
#' @param BACKGROUND_PA_CONFIDENCE beta parameter: confidence in accuracy of zero non-travel physical activity levels
#' @param INJURY_REPORTING_RATE lognormal parameter: rate of injury fatality reporting
#' @param CHRONIC_DISEASE_SCALAR lognormal parameter: scalar for background disease rates to adjust for bias in GBD data
#' @param DAY_TO_WEEK_TRAVEL_SCALAR beta parameter: rate of scaling travel from one day to one week - CURRENTLY used as constant only (using as beta parameter would need some further considerations)
#' @param SIN_EXPONENT_SUM lognormal parameter: linearity of injuries with respect to two modes. SIN_EXPONENT_SUM=2 means no safety in numbers
#' @param CASUALTY_EXPONENT_FRACTION beta parameter: casualty exponent contribution to SIN_EXPONENT_SUM
#' @param SIN_EXPONENT_SUM_NOV lognormal parameter: linearity of injuries with respect to two modes where strike mode = NOV. SIN_EXPONENT_SUM=2 means no safety in numbers
#' @param SIN_EXPONENT_SUM_CYCLE lognormal parameter: linearity of injuries with respect to two modes where victim mode = cycle. SIN_EXPONENT_SUM=2 means no safety in numbers
#' @param CASUALTY_EXPONENT_FRACTION_CYCLE beta parameter: casualty exponent contribution to SIN_EXPONENT_SUM_CYCLE  where victim mode = cycle
#' @param SIN_EXPONENT_SUM_PED lognormal parameter: linearity of injuries with respect to two modes  where victim mode = pedestrian. SIN_EXPONENT_SUM=2 means no safety in numbers
#' @param CASUALTY_EXPONENT_FRACTION_PED beta parameter: casualty exponent contribution to SIN_EXPONENT_SUM_PED where victim mode = pedestrian
#' @param SIN_EXPONENT_SUM_VEH lognormal parameter: linearity of injuries with respect to two modes where victim mode = a vehicle. SIN_EXPONENT_SUM=2 means no safety in numbers
#' @param CASUALTY_EXPONENT_FRACTION_VEH beta parameter: casualty exponent contribution to SIN_EXPONENT_SUM_VEH where victim mode = a vehicle
#' @param CALL_INDIVIDUAL_SIN logic: whether or not to call the safety in number coefficients for individual vehicles or use the same coefficients for all modes
#' @param BUS_TO_PASSENGER_RATIO beta parameter: number of buses per passenger
#' @param CAR_OCCUPANCY_RATIO beta parameter: number of people per car (including driver)
#' @param TRUCK_TO_CAR_RATIO beta parameter: proportion of truck to car vehicle km travelled
#' @param FLEET_TO_MOTORCYCLE_RATIO beta parameter: amount of motorcycle trips that are to be added as commercial trips
#' @param PM_EMISSION_INVENTORY_CONFIDENCE beta parameter: confidence in accuracy of PM emission inventory
#' @param PROPORTION_MOTORCYCLE_TRIPS beta parameter: proportion of trips that are to be added as personal motorcycle trips
#' @param CO2_EMISSION_INVENTORY_CONFIDENCE beta parameter: confidence in accuracy of CO2 emission inventory
#' @param DISTANCE_SCALAR_CAR_TAXI lognormal parameter: scalar to adjust for bias in car distance travelled
#' @param DISTANCE_SCALAR_WALKING lognormal parameter: scalar to adjust for bias in walking distance travelled
#' @param DISTANCE_SCALAR_PT lognormal parameter: scalar to adjust for bias in PT distance travelled
#' @param DISTANCE_SCALAR_CYCLING lognormal parameter: scalar to adjust for bias in cycling distance travelled
#' @param DISTANCE_SCALAR_MOTORCYCLE lognormal parameter: scalar to adjust for biase in motorcycle distance travelled
#' @param BUS_DRIVER_PROP_MALE scalar: proportion of bus drivers that are male
#' @param BUS_DRIVER_MALE_AGERANGE character: age range of male bus drivers
#' @param BUS_DRIVER_FEMALE_AGERANGE character: age range of female bus drivers
#' @param TRUCK_DRIVER_PROP_MALE scalar: proportion of truck drivers that are male
#' @param TRUCK_DRIVER_MALE_AGERANGE character: age range of male truck drivers
#' @param TRUCK_DRIVER_FEMALE_AGERANGE character: age range of female truck drivers
#' @param COMMERCIAL_MBIKE_PROP_MALE scalar: proportion of commercial motorcycle drivers that are male
#' @param COMMERCIAL_MBIKE_MALE_AGERANGE character: age range of male commercial motorcycle drivers
#' @param COMMERCIAL_MBIKE_FEMALE_AGERANGE character: age range of female commercial motorcycle drivers
#' @param MINIMUM_PT_TIME scalar: minimum time that person spends on public transport
#' @param MODERATE_PA_CONTRIBUTION scalar: proportion contribution of moderate PA in Leisure MVPA
#' @param SCENARIO_NAME name of the scenarios (currently supports: TEST_WALK_SCENARIO, TEST_CYCLE_SCENARIO, 
#'                                              MAX_MODE_SHARE_SCENARIO, LATAM, GLOBAL, AFRICA_INDIA, BOGOTA)
#' @param SCENARIO_INCREASE increase of given mode in each scenario (currently used in GLOBAL, BOGOTA, LATAM and AFRICA_INDIA scenarios)
#' 
#' @return ithim_object list of objects for onward use.
#' 
#' @export


run_ithim_setup <- function(seed = 1,
                            CITY = 'bogota',
                            speeds = NULL,
                            PM_emission_inventory = NULL,
                            CO2_emission_inventory = NULL,
                            DIST_CAT = c("0-2 km", "2-6 km", "6+ km"),
                            AGE_RANGE = c(15,69),
                            ADD_WALK_TO_PT_TRIPS = T,
                            ADD_BUS_DRIVERS = T,
                            ADD_CAR_DRIVERS = T,
                            ADD_TRUCK_DRIVERS = T,
                            ADD_MOTORCYCLE_FLEET = T,
                            ADD_PERSONAL_MOTORCYCLE_TRIPS = 'no',
                            REFERENCE_SCENARIO = 'baseline',
                            PATH_TO_LOCAL_DATA = NULL,
                            NSAMPLES = 1,
                            BUS_WALK_TIME = 16,
                            RAIL_WALK_TIME = 12.5,
                            CYCLING_MET =	6.8,
                            WALKING_MET =	3.5,
                            PASSENGER_MET	= 1.3,
                            CAR_DRIVER_MET = 2.5,
                            MOTORCYCLIST_MET =	2.8,
                            SEDENTARY_ACTIVITY_MET =	1.3,
                            LIGHT_ACTIVITY_MET =	1.3,
                            MODERATE_PA_MET =	4,
                            VIGOROUS_PA_MET =	8,
                            PM_CONC_BASE = 12.69,  
                            PM_TRANS_SHARE = 0.42,
                            PA_DOSE_RESPONSE_QUANTILE = F,
                            AP_DOSE_RESPONSE_QUANTILE = F,
                            BACKGROUND_PA_SCALAR = 1,
                            BACKGROUND_PA_CONFIDENCE = 1,
                            INJURY_REPORTING_RATE = 1,
                            CHRONIC_DISEASE_SCALAR = 1,
                            DAY_TO_WEEK_TRAVEL_SCALAR = 7,
                            SIN_EXPONENT_SUM= 2,
                            CASUALTY_EXPONENT_FRACTION = 0.5,
                            SIN_EXPONENT_SUM_NOV= 1,
                            SIN_EXPONENT_SUM_CYCLE= 2,
                            CASUALTY_EXPONENT_FRACTION_CYCLE = 0.5,
                            SIN_EXPONENT_SUM_PED= 2,
                            CASUALTY_EXPONENT_FRACTION_PED = 0.5,
                            SIN_EXPONENT_SUM_VEH= 2,
                            CASUALTY_EXPONENT_FRACTION_VEH = 0.5,
                            BUS_TO_PASSENGER_RATIO = 0.0389,
                            CAR_OCCUPANCY_RATIO = 0.625,
                            TRUCK_TO_CAR_RATIO = 0.3,
                            FLEET_TO_MOTORCYCLE_RATIO = 0.441,
                            PROPORTION_MOTORCYCLE_TRIPS = 0,
                            PM_EMISSION_INVENTORY_CONFIDENCE = 1,
                            CO2_EMISSION_INVENTORY_CONFIDENCE = 1,
                            DISTANCE_SCALAR_CAR_TAXI = 1,
                            DISTANCE_SCALAR_WALKING = 1,
                            DISTANCE_SCALAR_PT = 1,
                            DISTANCE_SCALAR_CYCLING = 1,
                            DISTANCE_SCALAR_MOTORCYCLE = 1,
                            BUS_DRIVER_PROP_MALE = 0.98,
                            BUS_DRIVER_MALE_AGERANGE = "19, 65", 
                            BUS_DRIVER_FEMALE_AGERANGE = "19, 65",
                            TRUCK_DRIVER_PROP_MALE = 0.98,
                            TRUCK_DRIVER_MALE_AGERANGE = "18, 65",
                            TRUCK_DRIVER_FEMALE_AGERANGE = "18, 65",
                            COMMERCIAL_MBIKE_PROP_MALE = 0.99,
                            COMMERCIAL_MBIKE_MALE_AGERANGE ="18, 65",
                            COMMERCIAL_MBIKE_FEMALE_AGERANGE ="18, 65",
                            MINIMUM_PT_TIME = 3,
                            MODERATE_PA_CONTRIBUTION = 0.5,
                            CALL_INDIVIDUAL_SIN = F, 
                            SCENARIO_NAME = 'GLOBAL', 
                            SCENARIO_INCREASE = 0.05 
                            ){
  

  #################################################
  
  set.seed(seed) # to obtain the same results every time when sampling from a distribution
  
  ithim_object <- list()
  
  # check if valid scenario name
  if (!SCENARIO_NAME %in% c("TEST_WALK_SCENARIO",
                            "TEST_CYCLE_SCENARIO",
                            "MAX_MODE_SHARE_SCENARIO",
                            "LATAM",
                            "GLOBAL",
                            "AFRICA_INDIA", 
                            'BOGOTA')){
    stop("Unsupported scenario. Please select one from \n
        TEST_WALK_SCENARIO \n
        TEST_CYCLE_SCENARIO \n
         MAX_MODE_SHARE_SCENARIO \n
         LATAM \n
         GLOBAL \n
         AFRICA_INDIA \n
         BOGOTA"
         )
  }
  
  # Set global values
  SCENARIO_NAME <<- SCENARIO_NAME
  SCENARIO_INCREASE <<- SCENARIO_INCREASE
  
  NSAMPLES <<- NSAMPLES
  
  # MODEL FLAGS, set to global
  ADD_WALK_TO_PT_TRIPS <<- ADD_WALK_TO_PT_TRIPS
  ADD_BUS_DRIVERS <<- ADD_BUS_DRIVERS
  ADD_CAR_DRIVERS <<- ADD_CAR_DRIVERS
  ADD_TRUCK_DRIVERS <<- ADD_TRUCK_DRIVERS
  ADD_MOTORCYCLE_FLEET <<- ADD_MOTORCYCLE_FLEET
  ADD_PERSONAL_MOTORCYCLE_TRIPS <<- ADD_PERSONAL_MOTORCYCLE_TRIPS
  CALL_INDIVIDUAL_SIN <<- CALL_INDIVIDUAL_SIN
  
  DAY_TO_WEEK_TRAVEL_SCALAR <<- DAY_TO_WEEK_TRAVEL_SCALAR
  
  REFERENCE_SCENARIO <<- REFERENCE_SCENARIO
  AGE_RANGE <<- AGE_RANGE
  DIST_CAT <<- DIST_CAT
  DIST_LOWER_BOUNDS <<- as.numeric(sapply(strsplit(DIST_CAT, "[^0-9]+"), function(x) x[1]))
  
  CITY <<- CITY

  # find path where local city specific data is stored
  if(is.null(PATH_TO_LOCAL_DATA)){
    PATH_TO_LOCAL_DATA <<- file.path(find.package('ithimr',lib.loc=.libPaths()), 'extdata/local',CITY) 
  }else{
    PATH_TO_LOCAL_DATA <<- PATH_TO_LOCAL_DATA
  }
  
  
  # fixed parameters for AP inhalation
  BASE_LEVEL_INHALATION_RATE <<- 1
  CLOSED_WINDOW_PM_RATIO <<- 0.5
  CLOSED_WINDOW_RATIO <<- 0.5
  ROAD_RATIO_MAX <<- 3.216
  ROAD_RATIO_SLOPE <<- 0.379
  SUBWAY_PM_RATIO <<- 0.8
  
  ## Mode speeds
  # set default speeds that are overwritten if city specific mode speeds are given as input parameters 
  default_speeds <- list( bus = 8.1, 
                          bus_driver = 8.1, 
                          minibus = 8.1, 
                          minibus_driver = 8.1, 
                          car = 13.8, 
                          car_driver = 13.8,
                          taxi = 13.8, 
                          pedestrian = 2.5, 
                          walk_to_pt = 2.5, 
                          cycle = 7.2, 
                          motorcycle = 15.2, 
                          truck = 8.1, 
                          van = 13.8, 
                          subway = 18.1, 
                          rail = 21.9, 
                          auto_rickshaw = 4, 
                          shared_auto = 13.8, 
                          shared_taxi = 13.8, 
                          cycle_rickshaw = 4,
                          other = 9.1)
  if(!is.null(speeds)){
    for(m in names(speeds))
      default_speeds[[m]] <- speeds[[m]]
  }
  
  ## ensure similar modes have the same speed assigned
  # ensure bus, bus_driver, minibus and minibus_driver have the same speed values
  default_speeds[['bus_driver']] <- default_speeds[['minibus']] <- default_speeds[['minibus_driver']] <- default_speeds[['bus']]
  
  # ensure car, car_driver and shared_auto have the same speed
  default_speeds[['car_driver']] <- default_speeds[['shared_auto']] <- default_speeds[['car']]
  
  # walk to pt has the same speed as pedestrians
  default_speeds[['walk_to_pt']] <- default_speeds[['pedestrian']]
  
  # shared_taxi has the same speed as taxi
  default_speeds[['shared_taxi']] <- default_speeds[['taxi']]
  

  # define dataframe with modes and speeds
  TRAVEL_MODES <<- tolower(names(default_speeds))
  MODE_SPEEDS <<- data.frame(stage_mode = TRAVEL_MODES, speed = unlist(default_speeds), stringsAsFactors = F)
 
  
  ## define PM emission inventory
  # set default PM2.5 emission contributions that are overwritten if city specific input parameters are given
  default_PM_emission_inventory <- list(
    bus=0,
    bus_driver=0.122466774,
    car=0.081980353,
    taxi=0,
    pedestrian=0,
    cycle=0,
    motorcycle=0.002303165,
    truck=0.33879567,
    big_truck=0.454454038,
    other=0
  )
  if(!is.null(PM_emission_inventory)){ # overwrite default inventory if city specific input values are given 
    for(m in names(PM_emission_inventory))
      if(grepl('bus$',m,ignore.case=T)&&!paste0(m,'_driver')%in%names(PM_emission_inventory)){
        default_PM_emission_inventory[[paste0(m,'_driver')]] <- PM_emission_inventory[[m]]
      }else{
        default_PM_emission_inventory[[m]] <- PM_emission_inventory[[m]]
      }
  }
  names(default_PM_emission_inventory) <- tolower(names(default_PM_emission_inventory))
  
  PM_EMISSION_INVENTORY <<- default_PM_emission_inventory

  
  ## define CO2 emission inventory
  # set default C02 emission contributions that are overwritten if city specific input parameters are given
  default_CO2_emission_inventory <- list(
    bus=0,
    taxi=0,
    pedestrian=0,
    cycle=0,
    motorcycle = 30033.57,
    car = 1033377.34,
    bus_driver = 100429.72,
    big_truck = 	259655.32,
    truck	= 163516.11,
    van = 0,
    auto_rickshaw = 0,
    other	= 1906.89
  )
  if(!is.null(CO2_emission_inventory)){  # overwrite default inventory if city specific input values are given 
    for(m in names(CO2_emission_inventory))
      if(grepl('bus$',m,ignore.case=T)&&!paste0(m,'_driver')%in%names(CO2_emission_inventory)){
        default_CO2_emission_inventory[[paste0(m,'_driver')]] <- CO2_emission_inventory[[m]]
      }else{
        default_CO2_emission_inventory[[m]] <- CO2_emission_inventory[[m]]
      }
  }
  names(default_CO2_emission_inventory) <- tolower(names(default_CO2_emission_inventory))
  
  CO2_EMISSION_INVENTORY <<- default_CO2_emission_inventory

  
  # load data
  ithim_load_data(speeds=default_speeds)  
  

  ## set parameters - obtain NSAMPLES samples from the given distributions for each of the input parameters
  ithim_object$parameters <- ithim_setup_parameters(NSAMPLES = NSAMPLES,
                                                    BUS_WALK_TIME = BUS_WALK_TIME,
                                                    RAIL_WALK_TIME = RAIL_WALK_TIME,
                                                    CYCLING_MET = CYCLING_MET,
                                                    WALKING_MET = WALKING_MET, 
                                                    PASSENGER_MET = PASSENGER_MET,
                                                    CAR_DRIVER_MET = CAR_DRIVER_MET,
                                                    MOTORCYCLIST_MET = MOTORCYCLIST_MET,
                                                    SEDENTARY_ACTIVITY_MET = SEDENTARY_ACTIVITY_MET,
                                                    LIGHT_ACTIVITY_MET = LIGHT_ACTIVITY_MET, 
                                                    MODERATE_PA_MET = MODERATE_PA_MET,
                                                    VIGOROUS_PA_MET = VIGOROUS_PA_MET,
                                                    PM_CONC_BASE = PM_CONC_BASE, 
                                                    PM_TRANS_SHARE = PM_TRANS_SHARE,
                                                    PA_DOSE_RESPONSE_QUANTILE = PA_DOSE_RESPONSE_QUANTILE,
                                                    AP_DOSE_RESPONSE_QUANTILE = AP_DOSE_RESPONSE_QUANTILE,
                                                    BACKGROUND_PA_SCALAR = BACKGROUND_PA_SCALAR,
                                                    BACKGROUND_PA_CONFIDENCE = BACKGROUND_PA_CONFIDENCE,
                                                    INJURY_REPORTING_RATE = INJURY_REPORTING_RATE,
                                                    CHRONIC_DISEASE_SCALAR = CHRONIC_DISEASE_SCALAR,
                                                    DAY_TO_WEEK_TRAVEL_SCALAR = DAY_TO_WEEK_TRAVEL_SCALAR,
                                                    SIN_EXPONENT_SUM = SIN_EXPONENT_SUM,
                                                    CASUALTY_EXPONENT_FRACTION = CASUALTY_EXPONENT_FRACTION,
                                                    SIN_EXPONENT_SUM_NOV = SIN_EXPONENT_SUM_NOV,
                                                    SIN_EXPONENT_SUM_CYCLE = SIN_EXPONENT_SUM_CYCLE,
                                                    CASUALTY_EXPONENT_FRACTION_CYCLE = CASUALTY_EXPONENT_FRACTION_CYCLE,
                                                    SIN_EXPONENT_SUM_PED = SIN_EXPONENT_SUM_PED,
                                                    CASUALTY_EXPONENT_FRACTION_PED = CASUALTY_EXPONENT_FRACTION_PED,
                                                    SIN_EXPONENT_SUM_VEH = SIN_EXPONENT_SUM_VEH,
                                                    CASUALTY_EXPONENT_FRACTION_VEH = CASUALTY_EXPONENT_FRACTION_VEH,
                                                    BUS_TO_PASSENGER_RATIO = BUS_TO_PASSENGER_RATIO,
                                                    CAR_OCCUPANCY_RATIO = CAR_OCCUPANCY_RATIO,
                                                    TRUCK_TO_CAR_RATIO = TRUCK_TO_CAR_RATIO,
                                                    FLEET_TO_MOTORCYCLE_RATIO = FLEET_TO_MOTORCYCLE_RATIO,
                                                    PROPORTION_MOTORCYCLE_TRIPS = PROPORTION_MOTORCYCLE_TRIPS,
                                                    PM_EMISSION_INVENTORY_CONFIDENCE = PM_EMISSION_INVENTORY_CONFIDENCE,
                                                    CO2_EMISSION_INVENTORY_CONFIDENCE = CO2_EMISSION_INVENTORY_CONFIDENCE,
                                                    DISTANCE_SCALAR_CAR_TAXI = DISTANCE_SCALAR_CAR_TAXI,
                                                    DISTANCE_SCALAR_WALKING = DISTANCE_SCALAR_WALKING,
                                                    DISTANCE_SCALAR_PT = DISTANCE_SCALAR_PT,
                                                    DISTANCE_SCALAR_CYCLING = DISTANCE_SCALAR_CYCLING,
                                                    DISTANCE_SCALAR_MOTORCYCLE = DISTANCE_SCALAR_MOTORCYCLE,
                                                    BUS_DRIVER_PROP_MALE = BUS_DRIVER_PROP_MALE,
                                                    BUS_DRIVER_MALE_AGERANGE = BUS_DRIVER_MALE_AGERANGE, 
                                                    BUS_DRIVER_FEMALE_AGERANGE = BUS_DRIVER_FEMALE_AGERANGE,
                                                    TRUCK_DRIVER_PROP_MALE = TRUCK_DRIVER_PROP_MALE,
                                                    TRUCK_DRIVER_MALE_AGERANGE = TRUCK_DRIVER_MALE_AGERANGE,
                                                    TRUCK_DRIVER_FEMALE_AGERANGE = TRUCK_DRIVER_FEMALE_AGERANGE,
                                                    COMMERCIAL_MBIKE_PROP_MALE = COMMERCIAL_MBIKE_PROP_MALE,
                                                    COMMERCIAL_MBIKE_MALE_AGERANGE = COMMERCIAL_MBIKE_MALE_AGERANGE,
                                                    COMMERCIAL_MBIKE_FEMALE_AGERANGE = COMMERCIAL_MBIKE_FEMALE_AGERANGE,
                                                    MINIMUM_PT_TIME = MINIMUM_PT_TIME,
                                                    MODERATE_PA_CONTRIBUTION = MODERATE_PA_CONTRIBUTION)
  
  
  ## Set flags which cause certain parts of the model to be called again IF certain 
  #  input parameters were sampled from a distribution

  # flag causes set_vehicle_inventory.R function to be called at a later stage (ithim_uncertainty.R)
  RECALCULATE_PM_EMISSION_INVENTORY <<- any(c('PM_EMISSION_INVENTORY')%in%names(ithim_object$parameters))
  
  # flag causes set_vehicle_inventory.R function to be called at a later stage (ithim_uncertainty.R)
  RECALCULATE_CO2_EMISSION_INVENTORY <<- any(c('CO2_EMISSION_INVENTORY')%in%names(ithim_object$parameters))
  
  # flag causes get_synthetic_from_trips.R which sets synthetic trips and synthetic 
  # population to be called at a later stage (ithim_uncertainty.R)
  RECALCULATE_TRIPS <<- any(c('BUS_WALK_TIME','RAIL_WALK_TIME',
                              "DISTANCE_SCALAR_PT",
                              "DISTANCE_SCALAR_CAR_TAXI",
                              "DISTANCE_SCALAR_MOTORCYCLE",
                              "DISTANCE_SCALAR_WALKING",
                              "DISTANCE_SCALAR_CYCLING",
                              'BUS_TO_PASSENGER_RATIO',
                              'CAR_OCCUPANCY_RATIO',
                              'TRUCK_TO_CAR_RATIO',
                              'BACKGROUND_PA_ZEROS')%in%names(ithim_object$parameters)) 
  
  # flag causes get_all_distances.R which uses synthetic trips to calculate distances
  # to be called at a later stage (ithim_uncertainty.R)
  RECALCULATE_DISTANCES <<- RECALCULATE_TRIPS||any(c('SIN_EXPONENT_SUM',
                                                     'CASUALTY_EXPONENT_FRACTION',
                                                     'SIN_EXPONENT_SUM_NOV',
                                                     'SIN_EXPONENT_SUM_CYCLE',
                                                     'CASUALTY_EXPONENT_FRACTION_CYCLE',
                                                     'SIN_EXPONENT_SUM_PED',
                                                     'CASUALTY_EXPONENT_FRACTION_PED',
                                                     'SIN_EXPONENT_SUM_VEH',
                                                     'CASUALTY_EXPONENT_FRACTION_VEH')%in%names(ithim_object$parameters))
  
  # complete TRIP_SET to contain distances and durations for trips and stages
  complete_trip_distance_duration() 
  
  # call set_vehicle_inventory if none of the PM and CO2 input parameters are samples from a distribution 
  if(!RECALCULATE_PM_EMISSION_INVENTORY & !RECALCULATE_CO2_EMISSION_INVENTORY) set_vehicle_inventory() # sets vehicle inventory
  
  # create inventory and edit trips, if they are not variable dependent
  if(!RECALCULATE_TRIPS){
    ithim_object$trip_scen_sets <- get_synthetic_from_trips() # sets synthetic trips and synthetic population
  }
  
  # calculate distances, if distances are not variable dependent
  if(!RECALCULATE_DISTANCES){
    ithim_object <- get_all_distances(ithim_object) # uses synthetic trips to calculate distances
  }
 
  return(ithim_object)
}
