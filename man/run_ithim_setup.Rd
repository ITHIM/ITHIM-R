% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_ithim_setup.R
\name{run_ithim_setup}
\alias{run_ithim_setup}
\title{Run the set up scripts for ITHIM}
\usage{
run_ithim_setup(
  seed = 1,
  CITY = "bogota",
  speeds = NULL,
  PM_emission_inventory = NULL,
  CO2_emission_inventory = NULL,
  DIST_CAT = c("0-2 km", "2-6 km", "6+ km"),
  AGE_RANGE = c(15, 69),
  ADD_WALK_TO_PT_TRIPS = T,
  ADD_BUS_DRIVERS = T,
  ADD_CAR_DRIVERS = T,
  ADD_TRUCK_DRIVERS = T,
  ADD_MOTORCYCLE_FLEET = T,
  ADD_PERSONAL_MOTORCYCLE_TRIPS = "no",
  REFERENCE_SCENARIO = "baseline",
  PATH_TO_LOCAL_DATA = NULL,
  NSAMPLES = 1,
  BUS_WALK_TIME = 16,
  RAIL_WALK_TIME = 12.5,
  CYCLING_MET = 6.8,
  WALKING_MET = 3.5,
  PASSENGER_MET = 1.3,
  CAR_DRIVER_MET = 2.5,
  MOTORCYCLIST_MET = 2.8,
  SEDENTARY_ACTIVITY_MET = 1.3,
  LIGHT_ACTIVITY_MET = 1.3,
  MODERATE_PA_MET = 4,
  VIGOROUS_PA_MET = 8,
  PM_CONC_BASE = 12.69,
  PM_TRANS_SHARE = 0.42,
  PA_DOSE_RESPONSE_QUANTILE = F,
  AP_DOSE_RESPONSE_QUANTILE = F,
  BACKGROUND_PA_SCALAR = 1,
  BACKGROUND_PA_CONFIDENCE = 1,
  INJURY_REPORTING_RATE = 1,
  CHRONIC_DISEASE_SCALAR = 1,
  DAY_TO_WEEK_TRAVEL_SCALAR = 7,
  SIN_EXPONENT_SUM = 2,
  CASUALTY_EXPONENT_FRACTION = 0.5,
  SIN_EXPONENT_SUM_NOV = 1,
  SIN_EXPONENT_SUM_CYCLE = 2,
  CASUALTY_EXPONENT_FRACTION_CYCLE = 0.5,
  SIN_EXPONENT_SUM_PED = 2,
  CASUALTY_EXPONENT_FRACTION_PED = 0.5,
  SIN_EXPONENT_SUM_VEH = 2,
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
  COMMERCIAL_MBIKE_MALE_AGERANGE = "18, 65",
  COMMERCIAL_MBIKE_FEMALE_AGERANGE = "18, 65",
  MINIMUM_PT_TIME = 3,
  MODERATE_PA_CONTRIBUTION = 0.5,
  CALL_INDIVIDUAL_SIN = F,
  SCENARIO_NAME = "GLOBAL",
  SCENARIO_INCREASE = 0.05
)
}
\arguments{
\item{seed}{set seed to get the same results when sampling from a distribution}

\item{CITY}{name of the city, and name of the directory containing city data files}

\item{speeds}{named list of mode speeds}

\item{PM_emission_inventory}{named list of mode PM emissions}

\item{CO2_emission_inventory}{named list of CO2 mode emissions}

\item{DIST_CAT}{vector string of distance categories in the form '0-6'. (The unit is assumed to be the same as in the trip set and is related to speed values, usually in km)}

\item{AGE_RANGE}{vector of minimum and maximum ages to include}

\item{ADD_WALK_TO_PT_TRIPS}{logic: whether or not to add short walks to all PT trips}

\item{ADD_BUS_DRIVERS}{logic: whether or not to add bus drivers}

\item{ADD_CAR_DRIVERS}{logic: whether or not to find and add distance travelled by individual cars, denoted by car drivers}

\item{ADD_TRUCK_DRIVERS}{logic: whether or not to add truck drivers}

\item{ADD_MOTORCYCLE_FLEET}{logic: whether or not to add additional commercial motorcycle fleet as ghost trips}

\item{ADD_PERSONAL_MOTORCYCLE_TRIPS}{character: if 'no' does not add any personal motorcycle trips otherwise set to geographic region which defines the set-up of the motorcycle trips to be added}

\item{REFERENCE_SCENARIO}{which scenario forms the reference for the health comparison}

\item{PATH_TO_LOCAL_DATA}{path to CITY directory, if not using package}

\item{NSAMPLES}{constant integer: number of samples to take}

\item{BUS_WALK_TIME}{lognormal parameter: duration of walk to bus stage}

\item{RAIL_WALK_TIME}{lognormal parameter: duration of walk to rail stage}

\item{CYCLING_MET}{lognormal parameter: METs when cycling}

\item{WALKING_MET}{lognormal parameter: METs when walking}

\item{PASSENGER_MET}{lognormal parameter: MET value associated with being a passenger on public transport}

\item{CAR_DRIVER_MET}{lognormal parameter: MET value associated with being a car driver}

\item{MOTORCYCLIST_MET}{lognormal parameter: MET value associated with being a motorcyclist}

\item{SEDENTARY_ACTIVITY_MET}{lognormal parameter: MET value associated with sedentary activity}

\item{LIGHT_ACTIVITY_MET}{lognormal parameter: MET value associated with light activity}

\item{MODERATE_PA_MET}{lognormal parameter: MET value associated with moderate activity}

\item{VIGOROUS_PA_MET}{lognormal parameter: MET value associated with vigorous activity}

\item{PM_CONC_BASE}{lognormal parameter: background PM2.5 concentration}

\item{PM_TRANS_SHARE}{beta parameter: fraction of background PM2.5 attributable to transport}

\item{PA_DOSE_RESPONSE_QUANTILE}{logic: whether or not to sample from physical activity relative risk dose response functions}

\item{AP_DOSE_RESPONSE_QUANTILE}{logic: whether or not to sample from air pollution relative risk dose response functions}

\item{BACKGROUND_PA_SCALAR}{lognormal parameter: reporting scalar for physical activity to correct bias in data}

\item{BACKGROUND_PA_CONFIDENCE}{beta parameter: confidence in accuracy of zero non-travel physical activity levels}

\item{INJURY_REPORTING_RATE}{lognormal parameter: rate of injury fatality reporting}

\item{CHRONIC_DISEASE_SCALAR}{lognormal parameter: scalar for background disease rates to adjust for bias in GBD data}

\item{DAY_TO_WEEK_TRAVEL_SCALAR}{beta parameter: rate of scaling travel from one day to one week - CURRENTLY used as constant only (using as beta parameter would need some further considerations)}

\item{SIN_EXPONENT_SUM}{lognormal parameter: linearity of injuries with respect to two modes. SIN_EXPONENT_SUM=2 means no safety in numbers}

\item{CASUALTY_EXPONENT_FRACTION}{beta parameter: casualty exponent contribution to SIN_EXPONENT_SUM}

\item{SIN_EXPONENT_SUM_NOV}{lognormal parameter: linearity of injuries with respect to two modes where strike mode = NOV. SIN_EXPONENT_SUM=2 means no safety in numbers}

\item{SIN_EXPONENT_SUM_CYCLE}{lognormal parameter: linearity of injuries with respect to two modes where victim mode = cycle. SIN_EXPONENT_SUM=2 means no safety in numbers}

\item{CASUALTY_EXPONENT_FRACTION_CYCLE}{beta parameter: casualty exponent contribution to SIN_EXPONENT_SUM_CYCLE  where victim mode = cycle}

\item{SIN_EXPONENT_SUM_PED}{lognormal parameter: linearity of injuries with respect to two modes  where victim mode = pedestrian. SIN_EXPONENT_SUM=2 means no safety in numbers}

\item{CASUALTY_EXPONENT_FRACTION_PED}{beta parameter: casualty exponent contribution to SIN_EXPONENT_SUM_PED where victim mode = pedestrian}

\item{SIN_EXPONENT_SUM_VEH}{lognormal parameter: linearity of injuries with respect to two modes where victim mode = a vehicle. SIN_EXPONENT_SUM=2 means no safety in numbers}

\item{CASUALTY_EXPONENT_FRACTION_VEH}{beta parameter: casualty exponent contribution to SIN_EXPONENT_SUM_VEH where victim mode = a vehicle}

\item{BUS_TO_PASSENGER_RATIO}{beta parameter: number of buses per passenger}

\item{CAR_OCCUPANCY_RATIO}{beta parameter: number of people per car (including driver)}

\item{TRUCK_TO_CAR_RATIO}{beta parameter: proportion of truck to car vehicle km travelled}

\item{FLEET_TO_MOTORCYCLE_RATIO}{beta parameter: amount of motorcycle trips that are to be added as commercial trips}

\item{PROPORTION_MOTORCYCLE_TRIPS}{beta parameter: proportion of trips that are to be added as personal motorcycle trips}

\item{PM_EMISSION_INVENTORY_CONFIDENCE}{beta parameter: confidence in accuracy of PM emission inventory}

\item{CO2_EMISSION_INVENTORY_CONFIDENCE}{beta parameter: confidence in accuracy of CO2 emission inventory}

\item{DISTANCE_SCALAR_CAR_TAXI}{lognormal parameter: scalar to adjust for bias in car distance travelled}

\item{DISTANCE_SCALAR_WALKING}{lognormal parameter: scalar to adjust for bias in walking distance travelled}

\item{DISTANCE_SCALAR_PT}{lognormal parameter: scalar to adjust for bias in PT distance travelled}

\item{DISTANCE_SCALAR_CYCLING}{lognormal parameter: scalar to adjust for bias in cycling distance travelled}

\item{DISTANCE_SCALAR_MOTORCYCLE}{lognormal parameter: scalar to adjust for biase in motorcycle distance travelled}

\item{BUS_DRIVER_PROP_MALE}{scalar: proportion of bus drivers that are male}

\item{BUS_DRIVER_MALE_AGERANGE}{character: age range of male bus drivers}

\item{BUS_DRIVER_FEMALE_AGERANGE}{character: age range of female bus drivers}

\item{TRUCK_DRIVER_PROP_MALE}{scalar: proportion of truck drivers that are male}

\item{TRUCK_DRIVER_MALE_AGERANGE}{character: age range of male truck drivers}

\item{TRUCK_DRIVER_FEMALE_AGERANGE}{character: age range of female truck drivers}

\item{COMMERCIAL_MBIKE_PROP_MALE}{scalar: proportion of commercial motorcycle drivers that are male}

\item{COMMERCIAL_MBIKE_MALE_AGERANGE}{character: age range of male commercial motorcycle drivers}

\item{COMMERCIAL_MBIKE_FEMALE_AGERANGE}{character: age range of female commercial motorcycle drivers}

\item{MINIMUM_PT_TIME}{scalar: minimum time that person spends on public transport}

\item{MODERATE_PA_CONTRIBUTION}{scalar: proportion contribution of moderate PA in Leisure MVPA}

\item{CALL_INDIVIDUAL_SIN}{logic: whether or not to call the safety in number coefficients for individual vehicles or use the same coefficients for all modes}

\item{SCENARIO_NAME}{name of the scenarios (currently supports: TEST_WALK_SCENARIO, TEST_CYCLE_SCENARIO,
MAX_MODE_SHARE_SCENARIO, LATAM, GLOBAL, AFRICA_INDIA, BOGOTA)}

\item{SCENARIO_INCREASE}{increase of given mode in each scenario (currently used in GLOBAL, BOGOTA, LATAM and AFRICA_INDIA scenarios)}
}
\value{
ithim_object list of objects for onward use.
}
\description{
Sets up the basic ITHIM object for onward calculation. Data loading, processing and harmonisation. Setting of global values.
}
\details{
This function is used to read in the various input files and parameters and to
process and harmonise the data ready for the health impact assessment.
Input Parameters have two options: to be set to a constant or to be sampled
from a pre-specified distribution. Most of these parameters are given as an
argument of length 1 or 2. If of length 1, the parameter is usually used as
a constant. If the parameter is of length 2, a distribution is defined
and sampled from NSAMPLE times.


This function performs the following steps:

\itemize{
\item check whether a valid scenario name is called, get an error message if not

\item set various input parameters as global parameters

\item find the path to the local data

\item define fixed parameters for air pollution inhalation


\item define the mode speeds:
   \itemize{
   \item set default speeds for the various modes

   \item update the default speeds with city specific mode speeds if these are
     given as input parameters

   \item ensure similar modes have the same speed assigned

   \item set-up dataframe with modes and speeds
   }

\item define PM emissions inventory
   \itemize{
   \item define default emission values

   \item update default values if city specific values are given as input parameters
   }

\item define CO2 emissions inventory
   \itemize{
   \item set default emission values

   \item update default values if city specific values are given as input parameters
   }

\item load and process data from files by calling \code{\link{ithim_load_data()}}

\item call \code{\link{ithim_setup_parameters()}} to set the given input parameters to the global
  environment if running in constant mode or to obtain NSAMPLE samples from the
  given distributions for each of the input parameters if running in sample mode

\item set flags which cause certain parts of the model to be called at a later stage
 (\code{\link{ithim_uncertainty()}}) IF certain input parameters were sampled from a distribution

\item call \code{\link{complete_trip_distance_duration()}} to add any missing stage or distance
  information to the trip data

\item if none of the corresponding input parameters were sampled from a distribution,
  call \code{\link{set_vehicle_inventory()}} to create a dataframe with mode specific speed,
  distance and emission information

\item if none of the corresponding input parameters were sampled from a distribution,
  call \code{\link{get_synthetic_from_trips()}} to set synthetic trips and synthetic population

\item if none of the corresponding input parameters were sampled from a distribution,
  call \code{\link{get_all_distances()}} to calculate trip distances
}
}
