# Set parameters for ITHIM-Global run

Function to set parameters by either using the constant value or
sampling from a pre-defined function

## Usage

``` r
ithim_setup_parameters(
  NSAMPLES = 1,
  BUS_WALK_TIME = 16,
  RAIL_WALK_TIME = 12.5,
  CYCLING_MMET = 5.8,
  WALKING_MMET = 2.5,
  PASSENGER_MMET = 0.3,
  CAR_DRIVER_MMET = 1.5,
  MOTORCYCLIST_MMET = 1.8,
  SEDENTARY_ACTIVITY_MMET = 0.3,
  LIGHT_ACTIVITY_MMET = 2,
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

## Value

list of samples of uncertain parameters

## Details

For each input parameters there are two options: to be set to a
constant, or to be sampled from a specified distribution. Each parameter
is given as an argument of length 1 or 2. If length 1, it's constant,
and set to the global environment. If length 2, a distribution is
defined and sampled from NSAMPLE times. There are some exceptions,
listed below.

The function performs the following steps:

- set all input parameters to the global environment (if sampling
  function is called, they are overwritten)

- loop through all potential variables with a lognormal distribution and
  sample from this distribution if required

- loop through all potential variables with a beta distribution and
  sample from this distribution if required

- if BACKGROUND_PA_CONFIDENCE\<1 then add BACKGROUND_PA_ZEROS parameters

- if PM_EMISSION_INVENTORY_CONFIDENCE\<1, then sample those PM inventory
  values by using a Dirichlet distribution which is parameterised by
  gamma random variables

- if CO2_EMISSION_INVENTORY_CONFIDENCE\<1, then sample those CO2
  inventory values by using a Dirichlet distribution which is
  parameterised by gamma random variables

- if PA_DOSE_RESPONSE_QUANTILE == T, find all diseases that are related
  to physical activity levels and assign a quantile to them by sampling
  from a uniform distribution between 0 and 1

- if AP_DOSE_RESPONSE_QUANTILE == T, find all diseases that are related
  to air pollution levels and assign a quantile to them by sampling from
  a uniform distribution between 0 and 1

At the bottom of this function, the
[`dirichlet_pointiness()`](https://ithim.github.io/ITHIM-R/reference/dirichlet_pointiness.md)
function is defined which parameterises the Dirichlet distributions for
the PM and CO2 emission inventories.
