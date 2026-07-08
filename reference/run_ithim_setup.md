# Run the set up scripts for ITHIM

Sets up the basic ITHIM object for onward calculation. Data loading,
processing and harmonisation. Setting of global values.

## Usage

``` r
run_ithim_setup(
  seed = 1,
  CITY = "bogota",
  speeds = NULL,
  PM_emission_inventory = NULL,
  CO2_emission_inventory = NULL,
  DIST_CAT = c("0-2 km", "2-6 km", "6+ km"),
  AGE_RANGE = c(15, 69),
  TREAT_TAXI_AS_CAR = T,
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
  CYCLING_MMET = 5.8,
  WALKING_MMET = 2.5,
  PASSENGER_MMET = 0.3,
  CAR_DRIVER_MMET = 1.5,
  MOTORCYCLIST_MMET = 1.8,
  SEDENTARY_ACTIVITY_MMET = 0.3,
  LIGHT_ACTIVITY_MMET = 1,
  MODERATE_PA_MMET = 3,
  VIGOROUS_PA_MMET = 7,
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
```

## Arguments

- seed:

  set seed to get the same results when sampling from a distribution

- CITY:

  name of the city, and name of the directory containing city data files

- speeds:

  named list of mode speeds

- PM_emission_inventory:

  named list of mode PM emissions

- CO2_emission_inventory:

  named list of CO2 mode emissions

- DIST_CAT:

  vector string of distance categories in the form '0-6'. (The unit is
  assumed to be the same as in the trip set and is related to speed
  values, usually in km)

- AGE_RANGE:

  vector of minimum and maximum ages to include

- TREAT_TAXI_AS_CAR:

  logic: whether to treat taxi as car/car_driver

- ADD_WALK_TO_PT_TRIPS:

  logic: whether or not to add short walks to all PT trips

- ADD_BUS_DRIVERS:

  logic: whether or not to add bus drivers

- ADD_CAR_DRIVERS:

  logic: whether or not to find and add distance travelled by individual
  cars, denoted by car drivers

- ADD_TRUCK_DRIVERS:

  logic: whether or not to add truck drivers

- ADD_MOTORCYCLE_FLEET:

  logic: whether or not to add additional commercial motorcycle fleet as
  ghost trips

- ADD_PERSONAL_MOTORCYCLE_TRIPS:

  character: if 'no' does not add any personal motorcycle trips
  otherwise set to geographic region which defines the set-up of the
  motorcycle trips to be added

- REFERENCE_SCENARIO:

  which scenario forms the reference for the health comparison

- PATH_TO_LOCAL_DATA:

  path to CITY directory, if not using package

- NSAMPLES:

  constant integer: number of samples to take

- BUS_WALK_TIME:

  lognormal parameter: duration of walk to bus stage

- RAIL_WALK_TIME:

  lognormal parameter: duration of walk to rail stage

- CYCLING_MMET:

  lognormal parameter: MMETs when cycling

- WALKING_MMET:

  lognormal parameter: MMETs when walking

- PASSENGER_MMET:

  lognormal parameter: MMET value associated with being a passenger on
  public transport

- CAR_DRIVER_MMET:

  lognormal parameter: MMET value associated with being a car driver

- MOTORCYCLIST_MMET:

  lognormal parameter: MMET value associated with being a motorcyclist

- SEDENTARY_ACTIVITY_MMET:

  lognormal parameter: MMET value associated with sedentary activity

- LIGHT_ACTIVITY_MMET:

  lognormal parameter: MMET value associated with light activity

- MODERATE_PA_MMET:

  lognormal parameter: MMET value associated with moderate activity

- VIGOROUS_PA_MMET:

  lognormal parameter: MMET value associated with vigorous activity

- PM_CONC_BASE:

  lognormal parameter: background PM2.5 concentration

- PM_TRANS_SHARE:

  beta parameter: fraction of background PM2.5 attributable to transport

- PA_DOSE_RESPONSE_QUANTILE:

  logic: whether or not to sample from physical activity relative risk
  dose response functions

- AP_DOSE_RESPONSE_QUANTILE:

  logic: whether or not to sample from air pollution relative risk dose
  response functions

- BACKGROUND_PA_SCALAR:

  lognormal parameter: reporting scalar for physical activity to correct
  bias in data

- BACKGROUND_PA_CONFIDENCE:

  beta parameter: confidence in accuracy of zero non-travel physical
  activity levels

- INJURY_REPORTING_RATE:

  lognormal parameter: rate of injury fatality reporting

- CHRONIC_DISEASE_SCALAR:

  lognormal parameter: scalar for background disease rates to adjust for
  bias in GBD data

- DAY_TO_WEEK_TRAVEL_SCALAR:

  beta parameter: rate of scaling travel from one day to one week -
  CURRENTLY used as constant only (using as beta parameter would need
  some further considerations)

- SIN_EXPONENT_SUM:

  lognormal parameter: linearity of injuries with respect to two modes.
  SIN_EXPONENT_SUM=2 means no safety in numbers

- CASUALTY_EXPONENT_FRACTION:

  beta parameter: casualty exponent contribution to SIN_EXPONENT_SUM

- SIN_EXPONENT_SUM_NOV:

  lognormal parameter: linearity of injuries with respect to two modes
  where strike mode = NOV. SIN_EXPONENT_SUM=2 means no safety in numbers

- SIN_EXPONENT_SUM_CYCLE:

  lognormal parameter: linearity of injuries with respect to two modes
  where victim mode = cycle. SIN_EXPONENT_SUM=2 means no safety in
  numbers

- CASUALTY_EXPONENT_FRACTION_CYCLE:

  beta parameter: casualty exponent contribution to
  SIN_EXPONENT_SUM_CYCLE where victim mode = cycle

- SIN_EXPONENT_SUM_PED:

  lognormal parameter: linearity of injuries with respect to two modes
  where victim mode = pedestrian. SIN_EXPONENT_SUM=2 means no safety in
  numbers

- CASUALTY_EXPONENT_FRACTION_PED:

  beta parameter: casualty exponent contribution to SIN_EXPONENT_SUM_PED
  where victim mode = pedestrian

- SIN_EXPONENT_SUM_VEH:

  lognormal parameter: linearity of injuries with respect to two modes
  where victim mode = a vehicle. SIN_EXPONENT_SUM=2 means no safety in
  numbers

- CASUALTY_EXPONENT_FRACTION_VEH:

  beta parameter: casualty exponent contribution to SIN_EXPONENT_SUM_VEH
  where victim mode = a vehicle

- BUS_TO_PASSENGER_RATIO:

  beta parameter: number of buses per passenger

- CAR_OCCUPANCY_RATIO:

  beta parameter: number of people per car (including driver)

- TRUCK_TO_CAR_RATIO:

  beta parameter: proportion of truck to car vehicle km travelled

- FLEET_TO_MOTORCYCLE_RATIO:

  beta parameter: amount of motorcycle trips that are to be added as
  commercial trips

- PROPORTION_MOTORCYCLE_TRIPS:

  beta parameter: proportion of trips that are to be added as personal
  motorcycle trips

- PM_EMISSION_INVENTORY_CONFIDENCE:

  beta parameter: confidence in accuracy of PM emission inventory

- CO2_EMISSION_INVENTORY_CONFIDENCE:

  beta parameter: confidence in accuracy of CO2 emission inventory

- DISTANCE_SCALAR_CAR_TAXI:

  lognormal parameter: scalar to adjust for bias in car distance
  travelled

- DISTANCE_SCALAR_WALKING:

  lognormal parameter: scalar to adjust for bias in walking distance
  travelled

- DISTANCE_SCALAR_PT:

  lognormal parameter: scalar to adjust for bias in PT distance
  travelled

- DISTANCE_SCALAR_CYCLING:

  lognormal parameter: scalar to adjust for bias in cycling distance
  travelled

- DISTANCE_SCALAR_MOTORCYCLE:

  lognormal parameter: scalar to adjust for biase in motorcycle distance
  travelled

- BUS_DRIVER_PROP_MALE:

  scalar: proportion of bus drivers that are male

- BUS_DRIVER_MALE_AGERANGE:

  character: age range of male bus drivers

- BUS_DRIVER_FEMALE_AGERANGE:

  character: age range of female bus drivers

- TRUCK_DRIVER_PROP_MALE:

  scalar: proportion of truck drivers that are male

- TRUCK_DRIVER_MALE_AGERANGE:

  character: age range of male truck drivers

- TRUCK_DRIVER_FEMALE_AGERANGE:

  character: age range of female truck drivers

- COMMERCIAL_MBIKE_PROP_MALE:

  scalar: proportion of commercial motorcycle drivers that are male

- COMMERCIAL_MBIKE_MALE_AGERANGE:

  character: age range of male commercial motorcycle drivers

- COMMERCIAL_MBIKE_FEMALE_AGERANGE:

  character: age range of female commercial motorcycle drivers

- MINIMUM_PT_TIME:

  scalar: minimum time that person spends on public transport

- MODERATE_PA_CONTRIBUTION:

  scalar: proportion contribution of moderate PA in Leisure MVPA

- CALL_INDIVIDUAL_SIN:

  logic: whether or not to call the safety in number coefficients for
  individual vehicles or use the same coefficients for all modes

- SCENARIO_NAME:

  name of the scenarios (currently supports: TEST_WALK_SCENARIO,
  TEST_CYCLE_SCENARIO, MAX_MODE_SHARE_SCENARIO, LATAM, GLOBAL,
  AFRICA_INDIA, BOGOTA)

- SCENARIO_INCREASE:

  increase of given mode in each scenario (currently used in GLOBAL,
  BOGOTA, LATAM and AFRICA_INDIA scenarios)

## Value

ithim_object list of objects for onward use.

## Details

This function is used to read in the various input files and parameters
and to process and harmonise the data ready for the health impact
assessment. Input Parameters have two options: to be set to a constant
or to be sampled from a pre-specified distribution. Most of these
parameters are given as an argument of length 1 or 2. If of length 1,
the parameter is usually used as a constant. If the parameter is of
length 2, a distribution is defined and sampled from NSAMPLE times.

This function performs the following steps:

- check whether a valid scenario name is called, get an error message if
  not

- set various input parameters as global parameters

- find the path to the local data

- define fixed parameters for air pollution inhalation

- define the mode speeds:

  - set default speeds for the various modes

  - update the default speeds with city specific mode speeds if these
    are given as input parameters

  - ensure similar modes have the same speed assigned

  - set-up dataframe with modes and speeds

- define PM emissions inventory

  - define default emission values

  - update default values if city specific values are given as input
    parameters

- define CO2 emissions inventory

  - set default emission values

  - update default values if city specific values are given as input
    parameters

- load and process data from files by calling
  [`ithim_load_data()`](https://usr110.github.io/ITHIM-R/reference/ithim_load_data.md)

- call
  [`ithim_setup_parameters()`](https://usr110.github.io/ITHIM-R/reference/ithim_setup_parameters.md)
  to set the given input parameters to the global environment if running
  in constant mode or to obtain NSAMPLE samples from the given
  distributions for each of the input parameters if running in sample
  mode

- set flags which cause certain parts of the model to be called at a
  later stage
  ([`ithim_uncertainty()`](https://usr110.github.io/ITHIM-R/reference/ithim_uncertainty.md))
  IF certain input parameters were sampled from a distribution

- call
  [`complete_trip_distance_duration()`](https://usr110.github.io/ITHIM-R/reference/complete_trip_distance_duration.md)
  to add any missing stage or distance information to the trip data

- if none of the corresponding input parameters were sampled from a
  distribution, call
  [`set_vehicle_inventory()`](https://usr110.github.io/ITHIM-R/reference/set_vehicle_inventory.md)
  to create a dataframe with mode specific speed, distance and emission
  information

- if none of the corresponding input parameters were sampled from a
  distribution, call
  [`get_synthetic_from_trips()`](https://usr110.github.io/ITHIM-R/reference/get_synthetic_from_trips.md)
  to set synthetic trips and baseline population

- if none of the corresponding input parameters were sampled from a
  distribution, call
  [`get_all_distances()`](https://usr110.github.io/ITHIM-R/reference/get_all_distances.md)
  to calculate trip distances
