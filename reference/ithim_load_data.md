# Load data for model and prepare input data for model

Loads and processes data from files using both city specific local data
and global data. Processes the input data ready for the ITHIM-Global
model run. Writes objects to the global environment.

## Usage

``` r
ithim_load_data(
  speeds = list(bus = 8.1, bus_driver = 8.1, minibus = 8.1, minibus_driver = 8.1, car =
    13.8, car_driver = 13.8, taxi = 13.8, pedestrian = 2.5, walk_to_pt = 2.5, cycle =
    7.2, motorcycle = 15.2, truck = 8.1, van = 13.8, subway = 18.1, rail = 21.9,
    auto_rickshaw = 4, shared_auto = 13.8, shared_taxi = 13.8, cycle_rickshaw = 4, other
    = 9.1)
)
```

## Arguments

- speeds:

  named list of mode speeds

## Details

This function performs the following steps to load and process the input
data:

- find path of ithimr package where global and local data can be found

- check whether drpa package is installed

- read in Global Burden of Disease data for country

- read in city specific trip data:

  - set missing stage or trip information to known stage or trip
    information, e.g. if stage duration is missing but trip duration is
    known then set stage duration to trip duration

  - ensure that stage modes and trip modes only consist of set keywords,
    replace all other mode names by those key words, e.g. replace
    'train' by 'rail'

  - remove trips where age or sex are 'NA'

  - Rename pedestrian stage modes of non-pedestrian trips from
    'pedestrian' to 'walk_to_pt'

  - (call
    [`get_scenario_settings()`](https://usr110.github.io/ITHIM-R/reference/get_scenario_settings.md)
    if using the max_mode_share_scenario)

- read in the local Global Burden of Disease (GBD) data:

  - combine various head and neck cancers, combine myeloid leukemia
    diseases, combine respiratory diseases at level 2

  - adjust for rectum cancer in the combined colon and rectum cancer
    burden

- read in local demographic data:

  - find / re-define max and min ages based on the max and min ages in
    the trip data, the demographic data and the max and min ages
    considered in the model

  - remove any population data outside these max and min ages

  - find the proportion of the total population that is considered in
    the model to the total population

  - get age-category details from population data, after any ages above
    and below the max and min ages have been removed

- extract all diseases plus road injures from GBD data, update format of
  max and min ages for each entry

- compute proportion of injuries in the age range considered in the
  model from the GBD data, this proportion is applied to those injury
  datasets without age and sex information

- remove ages outside age ranges considered in model from GBD_data

- create burden_of_disease dataframe from the GBD_data by changing the
  layout of the GBD_data:

  - add city specific population data

  - add the country specific disease rate from the GBD data, ie. the
    proportion of the number of people in the country with that disease
    over the population of the country for each age and sex category

  - using the country disease rate calculate the city population
    affected by the disease

- using the burden_of_disease data (now called DISEASE_BURDEN),
  calculate the ratio of YLL to death for each age and sex category for
  the road_injuries data

- read in the city specific road injury data:

  - Set a 'weight' column to the unique number of years for which injury
    data exists (if such column does not already exist)

  - calculate average yearly injury fatalities for each strike and
    casualty mode pair from original injury data

  - adjusted those yearly counts by the injury reporting rate

  - where strike mode equals casualty mode, set the strike mode to 'nov'
    (no other vehicle)

  - call set_injury_contingency.R function to set tables for WHW (who
    hit whom) and NOV (no other vehicle) fatalities
